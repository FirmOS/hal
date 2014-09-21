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
  newenclosures : IFRE_DB_Object;
  newdisks      : IFRE_DB_Object;


  procedure _UpdateDisks(const obj:IFRE_DB_Object);
  var feed_disk        : TFRE_DB_ZFS_BLOCKDEVICE;
      new_obj          : IFRE_DB_OBject;
      struct_obj       : IFRE_DB_Object;
      struct_disk      : TFRE_DB_OS_BLOCKDEVICE;
      new_disk         : TFRE_DB_OS_BLOCKDEVICE;
      phys_new_disk : TFRE_DB_PHYS_DISK;
      zfs_obj          : IFRE_DB_Object;
      targetports      : TFRE_DB_StringArray;
      slotguid         : TFRE_DB_GUID;
      slot_obj         : IFRE_DB_Object;
      slot             : TFRE_DB_DRIVESLOT;
      i                : NativeInt;
      ua               : TFRE_DB_ZFS_UNASSIGNED;

  begin
    feed_disk := obj.Implementor_HC as TFRE_DB_OS_BLOCKDEVICE;
    feed_disk.MachineID := mdata.UID;
    if mdata.FetchObjWithStringFieldValue('DEVICEIDENTIFIER',feed_disk.DeviceIdentifier,struct_obj,'') then
      begin
        if struct_obj.isA(TFRE_DB_UNDEFINED_BLOCKDEVICE.ClassName) then
          exit; // skip undefined blockdevices for ZFS Assignment
        if struct_obj.IsA(TFRE_DB_OS_BLOCKDEVICE,struct_disk) then
          begin
            struct_disk.DeleteField('zfs_guid');
            if struct_disk.hasParentInZFS then
              struct_disk.RemoveMosParentID(struct_disk.parentInZFSId);
            new_obj  := struct_disk.CloneToNewObject;
            new_disk := (new_obj.Implementor_HC as TFRE_DB_OS_BLOCKDEVICE);
            new_disk.SetAllSimpleObjectFieldsFromObject(feed_disk);
            if feed_disk.FieldExists('log') then
              begin
                if (new_disk.FieldExists('log')) then
                    new_disk.Field('log').AsObject.SetAllSimpleObjectFieldsFromObject(feed_disk.Field('log').AsObject)
                else
                    new_disk.Field('log').AsObject := feed_disk.Field('log').AsObject.CloneToNewObject;
              end;
            // assign zfs guid
            if assigned(pools) then
              begin
//                writeln('SWL: FIND ZFS GUID FOR ',feed_disk.DeviceName);
                if pools.FetchObjWithStringFieldValue('devicename',feed_disk.DeviceName,zfs_obj,uppercase(TFRE_DB_ZFS_BLOCKDEVICE.ClassName)) then
                  begin
//                    writeln('SWL: FOUND ZFS GUID ',(zfs_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).getZFSGuid);
                    new_disk.setZFSGuid((zfs_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).getZFSGuid);
                    new_disk.parentInZFSId := zfs_obj.UID;
                  end
                else
                  begin
//                    writeln('SWL: NO ZFS GUID ');
                    if pools.FetchObjWithStringFieldValue('zfs_guid',mdata.UID_String+'_'+GFRE_BT.HashString_MD5_HEX('UNASSIGNED'),zfs_obj,uppercase(TFRE_DB_ZFS_UNASSIGNED.ClassName)) then
                      begin
//                        writeln('SWL: SET TO UNASSIGNED ');
                        if zfs_obj.IsA(TFRE_DB_ZFS_UNASSIGNED,ua) then
                          begin
                            ua.addBlockdevice(new_disk);
                          end
                        else
                          raise EFRE_DB_Exception.Create('UNASSIGNED DISK OBJECT FOR MACHINE IS NOT A TFRE_DB_ZFS_UNASSIGNED');
                      end
                    else
                      raise EFRE_DB_Exception.Create('NO UNASSIGNED DISK OBJECT FOR MACHINE FOUND!');
                  end;
              end;
          end
        else
          raise EFRE_DB_Exception.Create('invalid class in UpdateDiskAndEnclosure _updatedisks'+struct_obj.SchemeClass);
      end
    else
      begin
        new_obj := feed_disk.CloneToNewObject;
      end;

    newdisks.Field(feed_disk.Field('DEVICEIDENTIFIER').asstring).asobject:=new_obj;

    //assign disks to diskslots
    if new_obj.IsA(TFRE_DB_PHYS_DISK,phys_new_disk) then
      begin
        if phys_new_disk.hasParentinEnclosure then
          phys_new_disk.RemoveMosParentID(phys_new_disk.ParentInEnclosureUID);
        targetports     := phys_new_disk.GetTargetPorts;
        slotguid        := CFRE_DB_NullGUID;
//        writeln('SWL: CLEAR SLOTGUID');
        for i:=low(targetports) to high(targetports) do
          begin
//            writeln ('SWL: SEARCHING FOR TARGETPORT_'+inttostr(i+1)+ ' '+targetports[i]);
            if mdata.Field('enclosures').AsObject.FetchObjWithStringFieldValue('TARGETPORT_'+inttostr(i+1),targetports[i],slot_obj,TFRE_DB_DRIVESLOT.ClassName) then
              begin
                if slotguid=CFRE_DB_NullGUID then
                  begin
//                    writeln ('SWL: FOUND FOR TARGETPORT_'+inttostr(i+1)+ ' '+targetports[i]);
                    slotguid :=slot_obj.UID;
//                    writeln('SWL: SET SLOTGUID '+FREDB_G2H(slotguid));
                  end
                else
                  begin
//                    writeln ('SWL: FOUND AGAIN FOR TARGETPORT_'+inttostr(i+1)+ ' '+targetports[i]);
                    if slot_obj.UID<>slotguid then
                      begin
//                        writeln ('SWL: FOUND DIFFERENT SLOT FOR TARGETPORT_'+inttostr(i+1)+ ' '+targetports[i]);
                        raise EFRE_DB_Exception.Create(edb_ERROR,'FetchObjWithStringFieldValue for driveslot delivered different driveslots for targetport '+targetports[i]+'slot uids:'+FREDB_G2H(slotguid)+' '+FREDB_G2H(slot_obj.UID));
                      end;
                  end;
              end;
          end;
        if slotguid <> CFRE_DB_NullGUID then
          begin
            if mdata.Field('enclosures').AsObject.FetchObjByUID(slotguid,slot_obj)=false then
              raise EFRE_DB_Exception.Create(edb_ERROR,'Slot_obj not found on fetching for assignment for disk !'+FREDB_G2H(slotguid));
            if not Assigned(slot_obj) then
              raise EFRE_DB_Exception.Create(edb_ERROR,'Slot_obj must be assigned here !'+FREDB_G2H(slotguid));
            if slot_obj.IsA(TFRE_DB_DRIVESLOT,slot) then
              begin
//                writeln ('SWL: ASSIGNING SLOT ',slot.slotnr,' ',FREDB_G2H(slotguid),' TO DISK ',phys_new_disk.UID_String);
                phys_new_disk.ParentInEnclosureUID:= slotguid;
                phys_new_disk.EnclosureUID        := slot.ParentInEnclosureUID;
                phys_new_disk.EnclosureNr         := slot.EnclosureNr;
                phys_new_disk.SlotNr              := slot.SlotNr;
              end
            else
              raise EFRE_DB_Exception.Create(edb_ERROR,'Class of slot_obj is invalid '+slot_obj.SchemeClass+' '+FREDB_G2H(slot_obj.UID));
          end;
      end;
  end;

  procedure _UpdateEnclosures(const obj:IFRE_DB_Object);
  var feed_enclosure  : TFRE_DB_ENCLOSURE;
      stat_enclosure  : IFRE_DB_OBject;
      new_enclosure   : IFRE_DB_Object;

    procedure _UpdateSlots(const slotobj:IFRE_DB_Object);
    var feed_slot     : TFRE_DB_DRIVESLOT;
        stat_slot     : IFRE_DB_Object;
        new_slot      : IFRE_DB_Object;
    begin
      feed_slot                       := slotobj.Implementor_HC as TFRE_DB_DRIVESLOT;
      if mdata.FetchObjWithStringFieldValue('DEVICEIDENTIFIER',feed_slot.DeviceIdentifier,stat_slot,'') then
        begin
          new_slot:=stat_slot.CloneToNewObject;
          new_slot.SetAllSimpleObjectFieldsFromObject(feed_slot);
        end
      else
        begin
          new_slot := feed_slot.CloneToNewObject;
        end;
      (new_enclosure.Implementor_HC as TFRE_DB_ENCLOSURE).AddDriveSlotEmbedded(feed_slot.SlotNr,new_slot.Implementor_HC as TFRE_DB_DRIVESLOT);
      (new_slot.Implementor_HC as TFRE_DB_DRIVESLOT).ParentInEnclosureUID  := new_enclosure.UID;
    end;

    procedure _UpdateExpanders(const expanderobj:IFRE_DB_Object);
    var feed_expander     : TFRE_DB_SAS_EXPANDER;
        stat_expander     : IFRE_DB_Object;
        new_expander      : IFRE_DB_Object;
    begin
      feed_expander      := expanderobj.Implementor_HC as TFRE_DB_SAS_EXPANDER;
      if mdata.FetchObjWithStringFieldValue('DEVICEIDENTIFIER',feed_expander.DeviceIdentifier,stat_expander,'') then
        begin
          new_expander := stat_expander.CloneToNewObject;
          new_expander.SetAllSimpleObjectFieldsFromObject(feed_expander);
        end
      else
        begin
          new_expander := feed_expander.CloneToNewObject;
        end;
      (new_enclosure.Implementor_HC as TFRE_DB_ENCLOSURE).AddExpanderEmbedded(new_expander.Implementor_HC as TFRE_DB_SAS_EXPANDER);
      (new_expander.Implementor_HC as TFRE_DB_SAS_EXPANDER).ParentInEnclosureUID  := new_enclosure.UID;
    end;

  begin
    feed_enclosure           := obj.Implementor_HC as TFRE_DB_ENCLOSURE;
    feed_enclosure.MachineID := mdata.UID;
    feed_enclosure.ParentInEnclosureUID := mdata.UID;

    feed_enclosure.AddMosParentID(mdata.UID);
    if mdata.FetchObjWithStringFieldValue('DEVICEIDENTIFIER',feed_enclosure.DeviceIdentifier,stat_enclosure,'') then
      begin
        new_enclosure:=stat_enclosure.CloneToNewObject;
        new_enclosure.SetAllSimpleObjectFieldsFromObject(feed_enclosure);
      end
    else
      begin
        new_enclosure := feed_enclosure.CloneToNewObject;
      end;
    feed_enclosure.Field('slots').AsObject.ForAllObjects(@_updateslots);
    feed_enclosure.Field('expanders').AsObject.ForAllObjects(@_updateexpanders);

    newenclosures.Field(feed_enclosure.Field('DEVICEIDENTIFIER').asstring).asobject:=new_enclosure;
  end;

begin

//  writeln('SWL DBO DISKENC', dbo.DumpToString());
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


    newenclosures := GFRE_DBI.NewObject;
    newenclosures.Field('UID').AsGUID := mdata.Field('enclosures').AsObject.UID;
    newenclosures.SetDomainID(mdata.Field('enclosures').AsObject.DomainID);

    dbo.Field('enclosures').AsObject.ForAllObjects(@_updateenclosures);
    mdata.Field('enclosures').AsObject:=newenclosures;

//    writeln('SWL DBOENCLOSURE STRUCTURE', dbo.Field('enclosures').AsObject.DumpToString());
//    writeln('SWL UPDATED ENCLOSURE STRUCTURE', mdata.Field('enclosures').AsObject.DumpToString());
    newdisks := GFRE_DBI.NewObject;
    newdisks.Field('UID').AsGUID := mdata.Field('disks').AsObject.UID;
    newdisks.SetDomainID(mdata.Field('disks').AsObject.DomainID);



    dbo.Field('disks').AsObject.ForAllObjects(@_updatedisks);
    mdata.Field('disks').AsObject:=newdisks;

//    writeln('SWL UPDATED DISK STRUCTURE', mdata.Field('disks').AsObject.DumpToString());


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
//  writeln('SWL: IOSTAT',dbo.DumpToString());
  mdata := GetLockedDataForMachine(machinename);
  try
    dbo.ForAllObjects(@_updateiostat);
  finally
    ReleaseLockedData;
  end;
end;

procedure TFRE_HAL_DISK_ENCLOSURE_POOL_MANAGEMENT.UpdateZpoolStatus(const dbo: IFRE_DB_Object; const machinename: TFRE_DB_NameType);
var mdata    : IFRE_DB_Object;
    newpools : IFRE_DB_Object;

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
//              writeln('SWL: FOUND ZFS OBJECT');
              new_obj.Field('UID').AsGUID := zfs_obj.UID;
              new_obj.ForAllFields(@_UpdateObjectLinks);
              if (zfs_obj.Implementor_HC is TFRE_DB_ZFS_OBJ) then
                begin
                  zpool_iostat :=(zfs_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).getZpoolIoStatEmbedded;
                  if assigned(zpool_iostat) then
                    begin
//                      writeln('SWL: UPDATING ZPOOL IOSTAT ',(new_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).getZpoolIoStatEmbedded.UID_String);
                      (new_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).getZpoolIoStatEmbedded.Field('UID').AsGUID := zpool_iostat.Field('UID').AsGUID;
                      (new_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).getZpoolIoStatEmbedded.Field('zfs_obj_id').AsObjectLink := new_obj.UID;
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




  begin
    feed_pool   := obj.Implementor_HC as TFRE_DB_ZFS_POOL;
    new_pool    := (feed_pool.CloneToNewObject.Implementor_HC as TFRE_DB_ZFS_POOL);
//    writeln('SWL: INCOMING POOL ',new_pool.DumpToString());
    if mdata.FetchObjWithStringFieldValue('objname',feed_pool.Field('objname').asstring,old_pool,'TFRE_DB_ZFS_POOL') then
      begin
        new_pool.ForAllObjectsBreakHierarchic(@_updateHierarchic);
      end;
    new_pool.MachineID := mdata.UID;
    new_pool.parentInZFSId := mdata.UID;
    new_pool.AddMosParentID(mdata.UID);
    newpools.Field(feed_pool.Field('objname').asstring).AsObject:= new_pool;
  end;

  procedure _cloneUnassigned(const obj:IFRE_DB_Object);
  var ua: TFRE_DB_ZFS_UNASSIGNED;
  begin
    if obj.IsA(TFRE_DB_ZFS_UNASSIGNED,ua) then
      newpools.Field(ua.GetName).AsObject:=ua.CloneToNewObject;
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
  mdata := GetLockedDataForMachine(machinename);
  try

    if not mdata.FieldExists('pools') then
      mdata.Field('pools').AsObject:=GFRE_DBI.NewObject;

    newpools := GFRE_DBI.NewObject;
    newpools.Field('UID').AsGUID := mdata.Field('pools').AsObject.UID;
    newpools.SetDomainID(mdata.Field('pools').AsObject.DomainID);


    dbo.ForAllObjects(@_updatepools);
    mdata.Field('pools').AsObject.ForAllObjects(@_cloneUnassigned);


    // check for deleted objects and remove objlinks
    GFRE_DBI.GenerateAnObjChangeList(newpools,mdata.Field('pools').asObject,@_Insert,@_Delete,@_Update);

    mdata.Field('pools').asObject := newpools;

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
  writeln('SWL: SERVERDISKENC ',data.DumpToString());

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
    res    := zo.CreateDiskpool(input,obj);
    result := GFRE_DBI.NewObject;
    result.Field('resultcode').AsInt32 := res;
//    result.Field('error').asstring     := error;
    result.Field('data').AsObject      := obj;
  finally
    zo.Free;
  end;
end;

end.

