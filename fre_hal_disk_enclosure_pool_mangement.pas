unit fre_hal_disk_enclosure_pool_mangement;

{
(§LIC)
  (c) Autor,Copyright
      Dipl.Ing.- Helmut Hartl, Dipl.Ing.- Franz Schober, Dipl.Ing.- Christian Koch
      FirmOS Business Solutions GmbH
      www.openfirmos.org
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2013, FirmOS Business Solutions GmbH
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

      * Redistributions of source code must retain the above copyright notice,
        this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright notice,
        this list of conditions and the following disclaimer in the documentation
        and/or other materials provided with the distribution.
      * Neither the name of the <FirmOS Business Solutions GmbH> nor the names
        of its contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(§LIC_END)
}

{$mode objfpc}{$H+}
{$codepage UTF8}
{$modeswitch nestedprocvars}
{$interfaces corba}

interface

uses
  Classes, SysUtils,FRE_DB_INTERFACE, FRE_DB_COMMON, FRE_PROCESS, FOS_BASIS_TOOLS,
  FOS_TOOL_INTERFACES,FRE_ZFS,fre_scsi,fre_base_parser,FRE_SYSTEM,fre_hal_schemes,fre_monitoring,
  fre_diff_transport;


type

  { TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT }

  TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT = class (TFRE_DB_Base)
  private
    finitialized_from_db               : boolean;

    hdata_lock                         : IFOS_LOCK;
    hdata                              : IFRE_DB_Object;
    snap_data                          : IFRE_DB_Object;
    update_data                        : IFRE_DB_Object;

  public
    constructor Create                 ; override;
    destructor  Destroy                ; override;

    function  GetLockedDataForMachine  (const machinename: TFRE_DB_NameType) : IFRE_DB_Object;
    procedure ReleaseLockedData;

    procedure ReceivedDBO                           (const dbo:IFRE_DB_Object);
    procedure UpdateDiskAndEnclosure                (const dbo:IFRE_DB_Object;const machinename : TFRE_DB_NameType);
    procedure UpdateIostat                          (const dbo:IFRE_DB_Object;const machinename : TFRE_DB_NameType);
    procedure UpdateZpoolStatus                     (const dbo:IFRE_DB_Object;const machinename : TFRE_DB_NameType);
    procedure UpdateMpath                           (const dbo:IFRE_DB_Object;const machinename : TFRE_DB_NameType);

    function  GetUpdateDataAndTakeStatusSnaphot     (const machine: string): IFRE_DB_Object;
    procedure ClearStatusSnapshotAndUpdates         ;
    procedure ResetToUnInitialized                  ;
    procedure ServerDiskEncPoolDataAnswer           (const data:IFRE_DB_Object);

    function  GetPools                  (const remoteuser:string='';const remotehost:string='';const remotekey:string=''): IFRE_DB_OBJECT;
    function  CreateDiskpool            (const input:IFRE_DB_Object; const remoteuser:string='';const remotehost:string='';const remotekey:string=''): IFRE_DB_OBJECT;

  published
  end;

function  Common_Disk_DataFeed          (const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;


implementation

function Common_Disk_DataFeed(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;

var
    unassigned_disks     : TFRE_DB_ZFS_UNASSIGNED;
    ua_obj               : IFRE_DB_Object;
    machinecollection    : IFRE_DB_COLLECTION;
    poolcollection       : IFRE_DB_COLLECTION;
    blockdevicecollection: IFRE_DB_COLLECTION;
    enclosurecollection  : IFRE_DB_COLLECTION;
    expandercollection   : IFRE_DB_COLLECTION;
    driveslotcollection  : IFRE_DB_COLLECTION;
    machine_uid          : TGUID;
    i                    : NativeInt;
    enclosure_refs       : TFRE_DB_ObjectReferences;
    disk_refs            : TFRE_DB_ObjectReferences;
    dbo_uid              : TGUID;

//    unassigned_uid       : TGUID;

    procedure _UpdatePools(const obj:IFRE_DB_Object);
    var pool : TFRE_DB_ZFS_POOL;
    begin
      pool := (obj.Implementor_HC as TFRE_DB_ZFS_POOL);
      pool.MachineID := machine_uid;
      pool.Field('mosparentIds').AddObjectLink(machine_uid);
      pool.FlatEmbeddedAndStoreInCollections(conn);
    end;

    procedure _InsertDisksIntoSlots(const obj:IFRE_DB_Object);
    var disk           : TFRE_DB_PHYS_DISK;
        targetports    : TFRE_DB_StringArray;
        i              : integer;
        guida          : TFRE_DB_GUIDArray;
        slotguid       : TGUID;
        index_name     : string;
        dbo            : IFRE_DB_Object;
        db_disk        : TFRE_DB_PHYS_DISK;
        sdbo           : IFRE_DB_Object;
        db_slot        : TFRE_DB_DRIVESLOT;
        refs           : TFRE_DB_GUIDArray;
        old_dbo        : IFRE_DB_Object;
        old_disk       : TFRE_DB_PHYS_DISK;
        already_in_slot: boolean;

        procedure _CheckSlot;
        begin
          if slotguid=CFRE_DB_NullGUID then
            begin
              if length(guida)<>1 then
                raise EFRE_DB_Exception.Create(edb_ERROR,'index for driveslot delivered more than one driveslot for targetport '+targetports[i])
              else
                begin
                  slotguid := guida[0];
                end;
            end
          else
            begin
              if length(guida)<>1 then
                raise EFRE_DB_Exception.Create(edb_ERROR,'index for driveslot delivered more than one driveslot for targetport '+targetports[i]);
              if slotguid <> guida[0] then
                raise EFRE_DB_Exception.Create(edb_ERROR,'index for driveslot delivered different driveslots for targetport '+targetports[i]);
            end;
        end;

    begin
      if (obj.Implementor_HC is TFRE_DB_PHYS_DISK) then
        begin
          disk            := (obj.Implementor_HC as TFRE_DB_PHYS_DISK);

          if disk.MachineID<>machine_uid then  // only for actual machine
            exit;

          targetports     := disk.GetTargetPorts;
          slotguid        := CFRE_DB_NullGUID;
          already_in_slot := false;

          for i:=low(targetports) to high(targetports) do
            begin
              if driveslotcollection.GetIndexedUID(targetports[i],guida,CFRE_DB_DRIVESLOT_TP1_INDEX) then
                _CheckSlot
              else
                if driveslotcollection.GetIndexedUID(targetports[i],guida,CFRE_DB_DRIVESLOT_TP2_INDEX) then
                  _CheckSlot;
            end;
          if slotguid<>CFRE_DB_NullGUID then
            begin
              if not blockdevicecollection.Fetch(disk.UID,dbo) then
                 raise EFRE_DB_Exception.Create(edb_ERROR,'could not fetch disk '+disk.UID_String);
              if not driveslotcollection.Fetch(slotguid,sdbo) then
                 raise EFRE_DB_Exception.Create(edb_ERROR,'could not fetch driveslot '+GFRE_BT.GUID_2_HexString(slotguid));
              db_slot := sdbo.Implementor_HC as TFRE_DB_DRIVESLOT;

              refs:=conn.GetReferences(db_slot.UID,false,'','parent_in_enclosure_uid');
              if length(refs)>0 then
                begin
                  if length(refs)>1 then
                    raise EFRE_DB_Exception.Create(edb_ERROR,'more than one drive  driveslot '+GFRE_BT.GUID_2_HexString(slotguid));
                  if disk.UID <> refs[0] then  // disk changed in slot
                    begin
                      if not blockdevicecollection.Fetch(refs[0],old_dbo) then
                         raise EFRE_DB_Exception.Create(edb_ERROR,'could not fetch disk '+FREDB_G2H(old_dbo.UID));
                       old_disk := old_dbo.Implementor_HC as TFRE_DB_PHYS_DISK;
                       old_disk.ClearSlotandEnclosureInformation;
                       CheckDbResult(blockdevicecollection.Update(old_dbo),'update blockdevice clearslot information');
                    end
                  else
                    begin
                      already_in_slot := true;
                    end;
                end;
              if already_in_slot then
                begin
                  dbo.Finalize;
                end
              else
                begin
                  db_disk := dbo.Implementor_HC as TFRE_DB_PHYS_DISK;
                  db_disk.ParentInEnclosureUID:= slotguid;
                  db_disk.EnclosureUID        := db_slot.ParentInEnclosureUID;
                  db_disk.EnclosureNr         := db_slot.EnclosureNr;
                  db_disk.SlotNr              := db_slot.SlotNr;
                  db_disk.Field('mosparentIds').AddObjectLink(db_slot.ParentInEnclosureUID);

                  CheckDbResult(blockdevicecollection.Update(dbo),'update blockdevice with slot information');
                end;
              sdbo.Finalize;
            end;
        end;
    end;

    procedure            _updateDisks(const obj:IFRE_DB_Object);
    var  disk               : TFRE_DB_ZFS_BLOCKDEVICE;
         db_disk            : TFRE_DB_ZFS_BLOCKDEVICE;
         dbo                : IFRE_DB_Object;

      procedure __updatedisk;
      var i : NativeInt;
          domID : TGUID;
      begin
//        writeln('SWL: UPDATEABLE DISK ', dbo.DumpToString());


        domID    := dbo.DomainID;
        db_disk  := dbo.Implementor_HC as TFRE_DB_ZFS_BLOCKDEVICE;
        db_disk.SetAllSimpleObjectFieldsFromObject(disk);
        db_disk.SetDomainID(domID);

        db_disk.MachineID :=  machine_uid;
        if db_disk.FieldExists('target_port') then
          db_disk.Field('target_port').AsStringArr := disk.Field('target_port').asstringArr;
        if disk.FieldExists('iostat') then
          begin
            if db_disk.FieldExists('iostat') then
              begin
                db_disk.Field('iostat').asobject.SetAllSimpleObjectFieldsFromObject(disk.Field('iostat').AsObject);
              end
            else
              begin
                db_disk.Field('iostat').asobject := disk.Field('iostat').AsObject.CloneToNewObject;
              end;
          end;
        if disk.FieldExists('log') then
          begin
            if db_disk.FieldExists('log') then
              begin
                db_disk.Field('log').asobject.SetAllSimpleObjectFieldsFromObject(disk.Field('log').AsObject);
              end
            else
              begin
                db_disk.Field('log').asobject := disk.Field('log').AsObject.CloneToNewObject;
              end;
          end;
        for i:=0 to high(disk_refs) do
          begin
            if disk.UID=disk_refs[i].linked_uid then
              begin
                disk_refs[i].linked_uid := CFRE_DB_NullGUID;
                break;
              end;
          end;
      end;

      procedure __insertdisk;
      begin
        dbo      := disk.CloneToNewObject;
        db_disk  := dbo.Implementor_HC as TFRE_DB_ZFS_BLOCKDEVICE;
        db_disk.MachineID :=  machine_uid;
//        db_disk.caption   :=  disk.Devicename; // device.WWN+' ('+device.Manufacturer+' '+device.Model_number+' '+device.Serial_number+')';
        unassigned_disks.addBlockdevice(db_disk);
      end;

      procedure __deletedisk;
      begin
        CheckDbResult(conn.Delete(dbo.UID),'store blockdevice in disk machine ['+FREDB_G2H(machine_uid)+'] deviceidentifier ['+disk.DeviceIdentifier+'] devicename ['+disk.DeviceName+']');
        dbo.Finalize;
      end;

    begin
      disk := (obj.Implementor_HC as TFRE_DB_ZFS_BLOCKDEVICE);
      if blockdevicecollection.GetIndexedObj(TFRE_DB_ZFS_BLOCKDEVICE.GetMachineDeviceIdentifier(machine_uid,disk.DeviceIdentifier),dbo,CFRE_DB_DEVICE_DEV_ID_INDEX) then
        begin
          if dbo.UID<>disk.UID then
            begin
              __deletedisk;
              __insertdisk;
              CheckDbResult(blockdevicecollection.Store(db_disk),'failed store blockdevice in disk machine ['+FREDB_G2H(machine_uid)+'] deviceidentifier ['+disk.DeviceIdentifier+'] devicename ['+disk.DeviceName+']'); ;
            end
          else
            begin
              assert(dbo.DomainID<>CFRE_DB_NullGUID,'fetched disk with Null GUiD');  //SWL
              __updatedisk;
              assert(db_disk.DomainID<>CFRE_DB_NullGUID,'updating disk with Null GUiD');      //SWL
              CheckDbResult(blockdevicecollection.Update(db_disk),'failed update blockdevice in disk ['+FREDB_G2H(machine_uid)+'] deviceidentifier ['+disk.DeviceIdentifier+'] devicename ['+disk.DeviceName+']');
            end;
        end
      else
        begin
          __insertdisk;
          dbo_UID :=dbo.UID;
          CheckDbResult(blockdevicecollection.Store(db_disk),'failed store blockdevice in disk machine ['+FREDB_G2H(machine_uid)+'] deviceidentifier ['+disk.DeviceIdentifier+'] devicename ['+disk.DeviceName+']');
//          writeln('SWL: INSERTDISK DONE ',disk.Devicename);
//          blockdevicecollection.Fetch(dbo_UID,dbo);  //test fetch                 //SWL
//          writeln('SWL: DISK FROM DB ',dbo.DumpToString());
//          assert(dbo.DomainID<>CFRE_DB_NullGUID,'updating disk with Null GUiD');  //SWL
        end;
    end;

begin
  FREDIFF_ApplyTransportObjectToDB(input,conn);

  result := GFRE_DB_NIL_DESC;
end;

{ TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT }

constructor TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.Create;
var indbo:IFRE_DB_Object;
begin
  inherited;
  GFRE_TF.Get_Lock(hdata_lock);
  finitialized_from_db := false;
  hdata       :=GFRE_DBI.NewObject;
  update_data :=GFRE_DBI.NewObject;
  snap_data   :=hdata.CloneToNewObject();

end;

destructor TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.Destroy;
begin

  hdata_lock.Finalize;
  hdata.Finalize;
  update_data.Finalize;
  snap_data.Finalize;

  inherited Destroy;
end;

function TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.GetLockedDataForMachine(const machinename: TFRE_DB_NameType): IFRE_DB_Object;
begin
  assert(length(machinename)>0,'TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.GetLockeDataforMachine no machiname provided!');
  hdata_lock.Acquire;
  if hdata.FieldExists(machinename) then
    result := hdata.Field(machinename).AsObject
  else
    begin
      result := GFRE_DBI.NewObjectScheme(TFRE_DB_MACHINE);
      result.Field('objname').AsString  := machinename;
      hdata.Field(machinename).AsObject := result;
    end;
end;

procedure TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.ReleaseLockedData;
begin
  hdata_lock.Release;
end;

procedure TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.ReceivedDBO(const dbo: IFRE_DB_Object);
var subfeedmodule : string;
    machinename   : TFRE_DB_NameType;
begin
//  writeln('SWL: RECEIVED',dbo.DumpToString());
  subfeedmodule := dbo.Field('SUBFEED').asstring;
  machinename   := dbo.Field('MACHINENAME').asstring;
  if subfeedmodule='DISKENCLOSURE' then
    UpdateDiskAndEnclosure(dbo.Field('data').asobject,machinename)
  else
    if subfeedmodule='IOSTAT' then
      UpdateIostat(dbo.Field('data').asobject,machinename)
    else
      if subfeedmodule='ZPOOLSTATUS' then
        UpdateZpoolStatus(dbo.Field('data').asobject,machinename)
      else
        if subfeedmodule='MPATH' then
          UpdateMpath(dbo.Field('data').asobject,machinename)
        else
          writeln('UNHANDLED SUBFEEDMODULE ',subfeedmodule);
end;

procedure TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.UpdateDiskAndEnclosure(const dbo: IFRE_DB_Object; const machinename: TFRE_DB_NameType);
var
  last_fdata: IFRE_DB_Object;
  mdata     : IFRE_DB_Object;
  pools     : IFRE_DB_Object;
  ua_obj    : IFRE_DB_Object;
  ua        : TFRE_DB_ZFS_UNASSIGNED;


  procedure _UpdateDisks(const obj:IFRE_DB_Object);
  var feed_disk        : TFRE_DB_ZFS_BLOCKDEVICE;
      mdata_obj        : IFRE_DB_OBject;
      struct_disk      : TFRE_DB_OS_BLOCKDEVICE;
      phys_struct_disk : TFRE_DB_PHYS_DISK;
      zfs_obj          : IFRE_DB_Object;
      targetports      : TFRE_DB_StringArray;
      slotguid         : TGUID;
      slot_obj         : IFRE_DB_Object;
      slot             : TFRE_DB_DRIVESLOT;
      i                : NativeInt;

  begin
    feed_disk := obj.Implementor_HC as TFRE_DB_OS_BLOCKDEVICE;
    feed_disk.MachineID := mdata.UID;
    if mdata.FetchObjWithStringFieldValue('DEVICEIDENTIFIER',feed_disk.DeviceIdentifier,mdata_obj,'') then
      begin
        if mdata_obj.IsA(TFRE_DB_OS_BLOCKDEVICE,struct_disk) then
          begin
            struct_disk.DeleteField('zfs_guid');
            if struct_disk.hasParentInZFS then
              struct_disk.RemoveMosParentID(struct_disk.parentInZFSId);
            struct_disk.SetAllSimpleObjectFieldsFromObject(feed_disk);
            if feed_disk.FieldExists('log') then
              begin
                if (struct_disk.FieldExists('log')) then
                    struct_disk.Field('log').AsObject.SetAllSimpleObjectFieldsFromObject(feed_disk.Field('log').AsObject)
                else
                    struct_disk.Field('log').AsObject := feed_disk.Field('log').AsObject.CloneToNewObject;
              end;
            // assign zfs guid
            if assigned(pools) then
              begin
                writeln('SWL: FIND ZFS GUID FOR ',feed_disk.DeviceName);
                if pools.FetchObjWithStringFieldValue('devicename',feed_disk.DeviceName,zfs_obj,uppercase(TFRE_DB_ZFS_BLOCKDEVICE.ClassName)) then
                  begin
                    writeln('SWL: FOUND ZFS GUID ',(zfs_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).getZFSGuid);
                    struct_disk.setZFSGuid((zfs_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).getZFSGuid);
                    struct_disk.parentInZFSId := zfs_obj.UID;
                  end
                else
                  begin
                    writeln('SWL: NO ZFS GUID ');
                    if pools.FetchObjWithStringFieldValue('zfs_guid',mdata.UID_String+'_'+GFRE_BT.HashString_MD5_HEX('UNASSIGNED'),zfs_obj,uppercase(TFRE_DB_ZFS_UNASSIGNED.ClassName)) then
                      begin
                        writeln('SWL: SET TO UNASSIGNED ');
                        struct_disk.parentInZFSId := zfs_obj.UID;
                      end
                    else
                      raise EFRE_DB_Exception.Create('NO UNASSIGNED DISK OBJECT FOR MACHINE FOUND!');
                  end;
              end;
          end
        else
          raise EFRE_DB_Exception.Create('invalid class in UpdateDiskAndEnclosure _updatedisks'+mdata_obj.SchemeClass);
      end
    else
      begin
        mdata_obj := feed_disk.CloneToNewObject;
        mdata.Field('disks').asObject.Field(feed_disk.Field('DEVICEIDENTIFIER').asstring).asobject:=mdata_obj;
      end;
    //assign disks to diskslots
    if mdata_obj.IsA(TFRE_DB_PHYS_DISK,phys_struct_disk) then
      begin
        if phys_struct_disk.hasParentinEnclosure then
          phys_struct_disk.RemoveMosParentID(phys_struct_disk.ParentInEnclosureUID);
        targetports     := phys_struct_disk.GetTargetPorts;
        slotguid        := CFRE_DB_NullGUID;
        for i:=low(targetports) to high(targetports) do
          begin
            writeln ('SWL: SEARCHING FOR TARGETPORT_'+inttostr(i+1)+ ' '+targetports[i]);
            if mdata.Field('enclosures').AsObject.FetchObjWithStringFieldValue('TARGETPORT_'+inttostr(i+1),targetports[i],slot_obj,TFRE_DB_DRIVESLOT.ClassName) then
              begin
                if slotguid=CFRE_DB_NullGUID then
                  begin
                    writeln ('SWL: FOUND FOR TARGETPORT_'+inttostr(i+1)+ ' '+targetports[i]);
                    slotguid :=slot_obj.UID;
                  end
                else
                  begin
                    writeln ('SWL: FOUND AGAIN FOR TARGETPORT_'+inttostr(i+1)+ ' '+targetports[i]);
                    if slot_obj.UID<>slotguid then
                      begin
                        writeln ('SWL: FOUND DIFFERENT SLOT FOR TARGETPORT_'+inttostr(i+1)+ ' '+targetports[i]);
                        raise EFRE_DB_Exception.Create(edb_ERROR,'FetchObjWithStringFieldValue for driveslot delivered different driveslots for targetport '+targetports[i]+'slot uids:'+FREDB_G2H(slotguid)+' '+FREDB_G2H(slot_obj.UID));
                      end;
                  end;
              end;
          end;
        if slotguid <> CFRE_DB_NullGUID then
          begin
            if slot_obj.IsA(TFRE_DB_DRIVESLOT,slot) then
              begin
                writeln ('SWL: ASSIGNING SLOT TO DISK '+FREDB_G2H(slotguid));
                phys_struct_disk.ParentInEnclosureUID:= slotguid;
                phys_struct_disk.EnclosureUID        := slot.ParentInEnclosureUID;
                phys_struct_disk.EnclosureNr         := slot.EnclosureNr;
                phys_struct_disk.SlotNr              := slot.SlotNr;
              end
            else
              raise EFRE_DB_Exception.Create(edb_ERROR,'Class of slot_obj is invalid '+slot_obj.SchemeClass+' '+FREDB_G2H(slot_obj.UID));
          end;
      end;
  end;

  procedure _UpdateEnclosures(const obj:IFRE_DB_Object);
  var feed_enclosure : TFRE_DB_ENCLOSURE;
      stat_enclosure  : IFRE_DB_OBject;

    procedure _UpdateSlots(const slotobj:IFRE_DB_Object);
    var feed_slot    : TFRE_DB_DRIVESLOT;
        stat_slot     : IFRE_DB_Object;
    begin
      feed_slot                       := slotobj.Implementor_HC as TFRE_DB_DRIVESLOT;
      feed_slot.ParentInEnclosureUID  := stat_enclosure.UID;
      if mdata.FetchObjWithStringFieldValue('DEVICEIDENTIFIER',feed_slot.DeviceIdentifier,stat_slot,'') then
        begin
          stat_slot.SetAllSimpleObjectFieldsFromObject(feed_slot);
        end
      else
        begin
          (stat_enclosure.Implementor_HC as TFRE_DB_ENCLOSURE).AddDriveSlotEmbedded(feed_slot.SlotNr,(feed_slot.CloneToNewObject.Implementor_HC as TFRE_DB_DRIVESLOT));
        end;
    end;

    procedure _UpdateExpanders(const expanderobj:IFRE_DB_Object);
    var feed_expander     : TFRE_DB_SAS_EXPANDER;
        stat_expander     : IFRE_DB_Object;
    begin
      feed_expander      := expanderobj.Implementor_HC as TFRE_DB_SAS_EXPANDER;
      feed_expander.ParentInEnclosureUID  := stat_enclosure.UID;
      if mdata.FetchObjWithStringFieldValue('DEVICEIDENTIFIER',feed_expander.DeviceIdentifier,stat_expander,'') then
        begin
          stat_expander.SetAllSimpleObjectFieldsFromObject(feed_expander);
        end
      else
        begin
          (stat_enclosure.Implementor_HC as TFRE_DB_ENCLOSURE).AddExpanderEmbedded((feed_expander.CloneToNewObject.Implementor_HC as TFRE_DB_SAS_EXPANDER));
        end;
    end;

  begin
    feed_enclosure           := obj.Implementor_HC as TFRE_DB_ENCLOSURE;
    feed_enclosure.MachineID := mdata.UID;
    feed_enclosure.AddMosParentID(mdata.UID);
    if mdata.FetchObjWithStringFieldValue('DEVICEIDENTIFIER',feed_enclosure.DeviceIdentifier,stat_enclosure,'') then
      begin
        stat_enclosure.SetAllSimpleObjectFieldsFromObject(feed_enclosure);
      end
    else
      begin
        stat_enclosure := feed_enclosure.CloneToNewObject;
        mdata.Field('enclosures').asObject.Field(feed_enclosure.Field('DEVICEIDENTIFIER').asstring).asobject:=stat_enclosure;
      end;
    feed_enclosure.Field('slots').AsObject.ForAllObjects(@_updateslots);
    feed_enclosure.Field('expanders').AsObject.ForAllObjects(@_updateexpanders);
  end;

begin
  mdata := GetLockedDataForMachine(machinename);
  try
    last_fdata := mdata.CloneToNewObject;

    if mdata.FieldExists('pools') then
      pools := mdata.Field('pools').AsObject
    else
      begin
        pools := GFRE_DBI.NewObject;
        mdata.Field('pools').AsObject:=pools;
      end;

    if pools.FetchObjWithStringFieldValue('zfs_guid',mdata.UID_String+'_'+GFRE_BT.HashString_MD5_HEX('UNASSIGNED'),ua_obj,uppercase(TFRE_DB_ZFS_UNASSIGNED.ClassName))=false then
      begin
        ua := TFRE_DB_ZFS_UNASSIGNED.CreateForDB;
        ua.InitforMachine(mdata.UID);
        pools.Field(ua.GetName).AsObject:=ua;
      end;

    if not mdata.FieldExists('disks') then
      mdata.Field('disks').AsObject:=GFRE_DBI.NewObject;

    if not mdata.FieldExists('enclosures') then
      mdata.Field('enclosures').AsObject:=GFRE_DBI.NewObject;

    dbo.Field('enclosures').AsObject.ForAllObjects(@_updateenclosures);

//    writeln('SWL ENCLOSURE STRUCTURE', mdata.Field('enclosures').AsObject.DumpToString());

    dbo.Field('disks').AsObject.ForAllObjects(@_updatedisks);

//    writeln('SWL DISK STRUCTURE', mdata.Field('disks').AsObject.DumpToString());


//    writeln('SWL TOTAL STRUCTURE', mdata.DumpToString());

    last_fdata.Finalize;
  finally
    ReleaseLockedData;
  end;
end;

procedure TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.UpdateIostat(const dbo: IFRE_DB_Object; const machinename: TFRE_DB_NameType);
var mdata:IFRE_DB_Object;

  procedure _UpdateIostat(const obj:IFRE_DB_Object);
  var feed_io   : TFRE_DB_IOSTAT;
      old_obj   : IFRE_DB_Object;
      disk_obj  : IFRE_DB_Object;
      new_io    : IFRE_DB_Object;
      io        : TFRE_DB_IOSTAT;
  begin
    feed_io     := obj.Implementor_HC as TFRE_DB_IOSTAT;
    if mdata.FetchObjWithStringFieldValue('IODEVICENAME',feed_io.Field('iodevicename').asstring,old_obj,'TFRE_DB_IOSTAT') then
      begin
        old_obj.SetAllSimpleObjectFieldsFromObject(feed_io);
      end
    else
      begin
        if mdata.FetchObjWithStringFieldValue('DEVICENAME',feed_io.Field('iodevicename').asstring,old_obj,'') then
          begin
            new_io := feed_io.CloneToNewObject;
            io     := (new_io.Implementor_HC as TFRE_DB_IOSTAT);
            (old_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).IoStat := io;
            io.SetBlockdevice(old_obj.UID);
          end
        else
          begin
            writeln('update iostat for unknown devicename:',feed_io.Field('iodevicename').asstring);
          end;
      end;
  end;

begin
  mdata := GetLockedDataForMachine(machinename);
  try
    dbo.ForAllObjects(@_updateiostat);
  finally
    ReleaseLockedData;
  end;
end;

procedure TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.UpdateZpoolStatus(const dbo: IFRE_DB_Object; const machinename: TFRE_DB_NameType);
var mdata: IFRE_DB_Object;

  procedure _UpdatePools(const obj:IFRE_DB_Object);
  var feed_pool : TFRE_DB_ZFS_POOL;
      new_pool  : TFRE_DB_ZFS_POOL;
      old_pool  : IFRE_DB_Object;


    procedure _UpdateHierarchic(const new_obj:IFRE_DB_Object; var halt:boolean);
    var zfs_guid     : string;
        zfs_obj      : IFRE_DB_Object;
        zpool_iostat : TFRE_DB_ZPOOL_IOSTAT;


      procedure _UpdateObjectLinks(const fld:IFRE_DB_Field);
      var feed_link : IFRE_DB_Object;
          lzfs_guid : string;
          old_link  : IFRE_DB_Object;
          feed_mach : TFRE_DB_MACHINE;
          i         : NativeInt;
      begin
        if (fld.FieldType=fdbft_ObjLink) then
          begin
//            writeln('SWL: CHECK FIELD',fld.FieldName);
            for i:=0 to fld.ValueCount-1 do begin
              if feed_pool.FetchObjByUID(fld.AsObjectLinkItem[i],feed_link)=False then
                begin
                  GFRE_DBI.LogError(dblc_APPLICATION,'OBJECT HAS INVALID OBJLINK OBJ [%s %s]',[fld.fieldname,FREDB_G2H(fld.AsObjectLinkItem[i])]);
  //                writeln('SWL:INVO ',feed_pool.DumpToString());
  //                writeln('SWL:INVO OLD ',old_pool.DumpToString());
                  exit;
                end;
              lzfs_guid:=feed_link.Field('zfs_guid').asstring;
              if old_pool.FetchObjWithStringFieldValue('ZFS_GUID',lzfs_guid,old_link,'') then
                begin
  //                writeln('SWL: FOUND OLD LINK ',fld.FieldName,' ',old_link.UID_String);
                  fld.AsObjectLinkItem[i] := old_link.UID;
                end
              else
                begin
                  GFRE_DBI.LogError(dblc_APPLICATION,'COULD NOT FIND OLD LINK IN LAST POOL WITH ZFS_GUID [%s]',[lzfs_guid]);
                end;
            end;
          end;
      end;

    begin
      halt :=false;
      if not (new_obj.Implementor_HC is TFRE_DB_ZFS_OBJ) then
        exit;
//      writeln('SWL: CLASS ',new_obj.SchemeClass);
      if new_obj.FieldExists('TIMESTAMP') then
        new_obj.DeleteField('TIMESTAMP');
      if new_obj.FieldExists('CONFIG_TS') then
        new_obj.DeleteField('CONFIG_TS');
      zfs_guid := new_obj.Field('zfs_guid').asstring;
      if zfs_guid<>'' then
        begin
          if old_pool.FetchObjWithStringFieldValue('ZFS_GUID',zfs_guid,zfs_obj,'') then
            begin
              new_obj.Field('UID').AsGUID := zfs_obj.UID;
              new_obj.ForAllFields(@_UpdateObjectLinks);
              if (zfs_obj.Implementor_HC is TFRE_DB_ZFS_OBJ) then
                begin
                  zpool_iostat :=(zfs_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).ZPoolIostat;
                  if assigned(zpool_iostat) then
                    begin
                      (new_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).ZPoolIostat.Field('UID').AsGUID := zpool_iostat.Field('UID').AsGUID;
                      (new_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).ZPoolIostat.Field('PARENT_IN_ZFS_UID').AsObjectLink := new_obj.UID;
                    end;
                end;
            end
          else
            begin
              new_obj.ForAllFields(@_UpdateObjectLinks);
//              GFRE_DBI.LogInfo(dblc_APPLICATION,'OBJECT WITH ZFS GUID [%s] NOT IN LAST POOL STATUS [%s]',[zfs_guid,feed_pool.Field('pool').asstring]);
            end;
        end
      else
        GFRE_DBI.LogError(dblc_APPLICATION,'OBJECT HAS NO ZFS GUID [%s]',[new_obj.DumpToString()]);
    end;

    procedure _Delete(const delete_obj : IFRE_DB_Object);

      procedure _checkforreferencedobjects(const obj:IFRE_DB_Object; var halt:boolean);
      var del_fields: IFRE_DB_Object;

        procedure _checkLinks (const fld:IFRE_DB_Field);
        var i:NativeInt;
        begin
          if (fld.FieldType=fdbft_ObjLink) then
            begin
              for i:=fld.ValueCount-1 downto 0 do
                begin
                  if fld.AsObjectLinkItem[i]=delete_obj.UID then
                    begin
                      GFRE_DBI.LogInfo(dblc_APPLICATION,'REMOVE OBJECT LINK FROM %s %s',[obj.SchemeClass,fld.FieldName]);
                      fld.RemoveObjectLink(i);
                      if fld.IsEmptyArray then
                        del_fields.Field(fld.FieldName).AsBoolean := true;
                    end;
                end;
            end;
        end;

        procedure _delete_emptyarrays (const fld:IFRE_DB_Field);
        begin
          if (fld.FieldType=fdbft_Boolean) then
            obj.DeleteField(fld.FieldName);
        end;

      begin
        halt := false;
        del_fields := GFRE_DBI.NewObject;
        obj.ForAllFields(@_checkLinks);
        del_fields.ForAllFields(@_delete_emptyarrays);
        del_fields.Finalize;
      end;

    begin
      GFRE_DBI.LogInfo(dblc_APPLICATION,'DELETED OBJECTS FROM POOL: %s',[delete_obj.DumpToString()]);
      mdata.ForAllObjectsBreakHierarchic(@_checkforreferencedobjects);
    end;

    procedure _Update(const is_child_update : boolean ; const update_obj : IFRE_DB_Object ; const update_type :TFRE_DB_ObjCompareEventType  ;const new_field, old_field: IFRE_DB_Field);
    begin
      // not used
    end;

    procedure _Insert(const o : IFRE_DB_Object);
    begin
      // not used
    end;


  begin
    feed_pool   := obj.Implementor_HC as TFRE_DB_ZFS_POOL;
    new_pool    := (feed_pool.CloneToNewObject.Implementor_HC as TFRE_DB_ZFS_POOL);
    writeln('SWL: NEW POOL ',new_pool.DumpToString());
//    abort;
    if mdata.FetchObjWithStringFieldValue('objname',feed_pool.Field('objname').asstring,old_pool,'TFRE_DB_ZFS_POOL') then
      begin
        new_pool.ForAllObjectsBreakHierarchic(@_updateHierarchic);

        // check for deleted objects
        GFRE_DBI.GenerateAnObjChangeList(new_pool,old_pool,@_Insert,@_Delete,@_Update);

      end;
    new_pool.MachineID := mdata.UID;
    new_pool.AddMosParentID(mdata.UID);
    mdata.Field('pools').AsObject.Field(feed_pool.Field('objname').asstring).AsObject:= new_pool;
  end;

begin
  mdata := GetLockedDataForMachine(machinename);
  try

    if not mdata.FieldExists('pools') then
      mdata.Field('pools').AsObject:=GFRE_DBI.NewObject;

//    writeln('ZPOOLSTATUS',dbo.DumpToString());

    dbo.ForAllObjects(@_updatepools);
//    writeln('SWL: MDATA POOLS',mdata.Field('pools').AsObject.DumpToString());

  finally
    ReleaseLockedData;
  end;
end;

procedure TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.UpdateMpath(const dbo: IFRE_DB_Object; const machinename: TFRE_DB_NameType);
var mdata: IFRE_DB_Object;

  procedure _UpdateMPath(const obj:IFRE_DB_Object);
  var old_obj   : IFRE_DB_OBject;
  begin
    if mdata.FetchObjWithStringFieldValue('DEVICENAME',obj.Field('DEVICE').asstring,old_obj,'') then
      begin
        if (old_obj.Implementor_HC is TFRE_DB_PHYS_DISK) then
          begin
            (old_obj.Implementor_HC as TFRE_DB_PHYS_DISK).OperationalPathCount := obj.Field('OPATHCOUNT').AsUInt16;
            (old_obj.Implementor_HC as TFRE_DB_PHYS_DISK).TotalPathCount       := obj.Field('TPATHCOUNT').AsUInt16;
          end
        else
          writeln('device is not a phys diskin mpath update:',obj.Field('DEVICE').asstring);
      end
    else
      writeln('undefined device in mpath update:',obj.Field('DEVICE').asstring);
  end;


begin
  mdata := GetLockedDataForMachine(machinename);
  try
    if not mdata.FieldExists('disks') then
      mdata.Field('disks').AsObject:=GFRE_DBI.NewObject;
    dbo.ForAllObjects(@_updatempath);
  finally
    ReleaseLockedData;
  end;
end;


function TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.GetUpdateDataAndTakeStatusSnaphot (const machine:string) : IFRE_DB_Object;
begin
  if finitialized_from_db then
    begin
      hdata_lock.Acquire;
      try

        FREDIFF_GenerateDiffContainersandAddToObject(hdata,snap_data,update_data);

        writeln('SWL: UPDATES:',update_data.DumpToString());
//        writeln('SWL: STRUCTURE:',hdata.DumpToString());

        snap_data.Finalize;

        snap_data := hdata.CloneToNewObject;

        result := update_data.CloneToNewObject;
        update_data.ClearAllFields;
      finally
        hdata_lock.Release;
      end;
    end
  else
    result := GFRE_DBI.NewObject;   //DEBUG TODO
end;

procedure TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.ClearStatusSnapshotAndUpdates;
begin
  hdata_lock.Acquire;
  try
    hdata.ClearAllFields;
    snap_data.ClearAllFields;
    update_data.ClearAllFields;
  finally
    hdata_lock.Release;
  end;

end;

procedure TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.ResetToUnInitialized;
begin
  finitialized_from_db := false;
  ClearStatusSnapshotAndUpdates;
end;

procedure TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.ServerDiskEncPoolDataAnswer(const data: IFRE_DB_Object);
var machine     : TFRE_DB_MACHINE;

  procedure _forallObjects(const obj:IFRE_DB_Object);
  var machinename : TFRE_DB_String;
  begin
    if obj.IsA(TFRE_DB_MACHINE,machine) then
      begin
        machinename := machine.GetName;
        assert(length(machinename)>0,'TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.ServerDiskEncPoolDataAnswer no machineame provided!');
        hdata.Field(machinename).AsObject := machine.CloneToNewObject;
      end;
  end;

begin
  writeln('SWL SERVERDISKENC ',data.DumpToString());

  hdata_lock.Acquire;
  try
    data.ForAllObjects(@_forallObjects);

    snap_data.Finalize;
    snap_data := hdata.CloneToNewObject;
  finally
    hdata_lock.Release;
  end;
  finitialized_from_db:=true;
end;

function TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.GetPools(const remoteuser: string; const remotehost: string; const remotekey: string): IFRE_DB_OBJECT;
var zo    : TFRE_DB_ZFS;
    obj   : IFRE_DB_Object;
    error : string;
    res   : integer;
begin
  zo     := TFRE_DB_ZFS.create;
  try
    zo.SetRemoteSSH(remoteuser, remotehost, remotekey);
    res    := zo.GetPools(error,obj);
    result := GFRE_DBI.NewObject;
    result.Field('resultcode').AsInt32 := res;
    result.Field('error').asstring     := error;
    result.Field('data').AsObject      := obj;
  finally
    zo.Free;
  end;
end;

function TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.CreateDiskpool(const input: IFRE_DB_Object; const remoteuser: string; const remotehost: string; const remotekey: string): IFRE_DB_OBJECT;
var zo    : TFRE_DB_ZFS;
    obj   : IFRE_DB_Object;
    error : string;
    res   : integer;
begin
  zo     := TFRE_DB_ZFS.create;
  try
    zo.SetRemoteSSH(remoteuser, remotehost, remotekey);
    res    := zo.CreateDiskpool(input,error,obj);
    result := GFRE_DBI.NewObject;
    result.Field('resultcode').AsInt32 := res;
    result.Field('error').asstring     := error;
    result.Field('data').AsObject      := obj;
  finally
    zo.Free;
  end;
end;

end.

