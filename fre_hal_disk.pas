unit fre_hal_disk;

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
  FOS_TOOL_INTERFACES,FRE_ZFS,fre_scsi,fre_base_parser,FRE_SYSTEM,fre_hal_schemes;


type

  { TFRE_HAL_DISK }

  TFRE_HAL_DISK = class (TFRE_DB_Base)
  private

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
    procedure UpdateZpoolIostat                     (const dbo:IFRE_DB_Object;const machinename : TFRE_DB_NameType);
    procedure UpdateMpath                           (const dbo:IFRE_DB_Object;const machinename : TFRE_DB_NameType);

    function  GetUpdateDataAndTakeSnaphot           : IFRE_DB_Object;
    procedure ClearSnapshotAndUpdates               ;

//    function  FetchDiskAndEnclosureInformation      (const remoteuser:string='';const remotehost:string='';const remotekey:string=''): IFRE_DB_OBJECT;
    function  FetchPoolConfiguration                (const zfs_pool_name:string; const remoteuser:string='';const remotehost:string='';const remotekey:string=''): IFRE_DB_OBJECT;

    function  GetPools                  (const remoteuser:string='';const remotehost:string='';const remotekey:string=''): IFRE_DB_OBJECT;
    function  CreateDiskpool            (const input:IFRE_DB_Object; const remoteuser:string='';const remotehost:string='';const remotekey:string=''): IFRE_DB_OBJECT;

  published
    procedure REM_GetDiskInformation    (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
    procedure REM_GetPools              (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
    procedure REM_GetPoolConfiguration  (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
    procedure REM_CreateDiskpool        (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
  end;

function  Common_Disk_DataFeed          (const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;


implementation

function Common_Disk_DataFeed(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;

var unassigned_disks     : TFRE_DB_ZFS_UNASSIGNED;
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
              if driveslotcollection.GetIndexedUIDs(targetports[i],guida,CFRE_DB_DRIVESLOT_TP1_INDEX) then
                _CheckSlot
              else
                if driveslotcollection.GetIndexedUIDs(targetports[i],guida,CFRE_DB_DRIVESLOT_TP2_INDEX) then
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
        db_disk.caption   :=  disk.Devicename; // device.WWN+' ('+device.Manufacturer+' '+device.Model_number+' '+device.Serial_number+')';
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

    procedure            _UpdateEnclosures(const obj:IFRE_DB_Object);
    var enclosure        : TFRE_DB_ENCLOSURE;
        db_enclosure     : TFRE_DB_ENCLOSURE;
        dbo              : IFRE_DB_Object;
        dummy            : IFRE_DB_Object;

      procedure __UpdateDriveSlots(const obj:IFRE_DB_Object);
      var driveslot      : TFRE_DB_DRIVESLOT;
          db_driveslot   : TFRE_DB_DRIVESLOT;
      begin
        driveslot        := (obj.Implementor_HC as TFRE_DB_DRIVESLOT);
        if driveslotcollection.GetIndexedObj(driveslot.DeviceIdentifier,dbo,CFRE_DB_DRIVESLOT_ID_INDEX) then
          begin
            if dbo.UID<>driveslot.UID then
              begin
                abort;
              end
            else
              begin
                db_driveslot := dbo.Implementor_HC as TFRE_DB_DRIVESLOT;
                db_driveslot.SetAllSimpleObjectFieldsFromObject(driveslot);
                db_driveslot.ParentInEnclosureUID  := enclosure.UID;
                CheckDbResult(driveslotcollection.Update(db_driveslot),'could not update driveslot');
              end;
          end
        else
          begin
            db_driveslot                      := driveslot.CloneToNewObject(false).Implementor_HC as TFRE_DB_DRIVESLOT;
            db_driveslot.ParentInEnclosureUID := enclosure.UID;
            CheckDbResult(driveslotcollection.Store(db_driveslot),'could not store driveslot');
          end;
      end;

      procedure __UpdateExpanders(const obj:IFRE_DB_Object);
      var expander       : TFRE_DB_SAS_EXPANDER;
          db_expander    : TFRE_DB_SAS_EXPANDER;
      begin
        expander         := (obj.Implementor_HC as TFRE_DB_SAS_EXPANDER);
        if expandercollection.GetIndexedObj(expander.DeviceIdentifier,dbo,CFRE_DB_EXPANDER_ID_INDEX) then
          begin
            if dbo.UID<>expander.UID then
              begin
                abort;
              end
            else
              begin
                db_expander := dbo.Implementor_HC as TFRE_DB_SAS_EXPANDER;
                db_expander.SetAllSimpleObjectFieldsFromObject(expander);
                db_expander.ParentInEnclosureUID  := enclosure.UID;
                CheckDbResult(expandercollection.Update(db_expander),'could not update expander');
              end;
          end
        else
          begin
            db_expander                       := expander.CloneToNewObject(false).Implementor_HC as TFRE_DB_SAS_EXPANDER;
            db_expander.ParentInEnclosureUID  := enclosure.UID;
            CheckDbResult(expandercollection.Store(db_expander),'could not store expander');
          end;
      end;

      procedure __updateenclosure;
      var i : NativeInt;
      begin
        db_enclosure.SetAllSimpleObjectFieldsFromObject(enclosure);
        db_enclosure.MachineID := machine_uid;
        for i:=0 to high(enclosure_refs) do
          begin
            if db_enclosure.UID=enclosure_refs[i].linked_uid then
              begin
                enclosure_refs[i].linked_uid := CFRE_DB_NullGUID;
              end;
          end;
      end;

      procedure __deleteenclosure;
      begin
        (dbo.Implementor_HC as TFRE_DB_ENCLOSURE).DeleteReferencingToMe(conn);
        CheckDbResult(conn.Delete(dbo.UID),'could not delete enclosure uid ['+dbo.UID_String+']');
        dbo.Finalize;
      end;

      procedure __insertenclosure;
      begin
        db_enclosure := TFRE_DB_ENCLOSURE.CreateForDB;
        db_enclosure.Field('UID').asGuid := enclosure.UID;
      end;

    begin
      enclosure      := (obj.Implementor_HC as TFRE_DB_ENCLOSURE);
      if enclosurecollection.GetIndexedObj(enclosure.DeviceIdentifier,dbo,CFRE_DB_ENCLOSURE_ID_INDEX) then
        begin
          if dbo.UID<>enclosure.UID then
            begin
              __deleteenclosure;
              __insertenclosure;
              __updateenclosure;
              CheckDbResult(enclosurecollection.Store(db_enclosure),'could not store enclosure');
            end
          else
            begin
              db_enclosure := dbo.Implementor_HC as TFRE_DB_ENCLOSURE;
              __updateenclosure;
              CheckDbResult(enclosurecollection.Update(db_enclosure),'could not update enclosure');
            end;
        end
      else
        begin
          __insertenclosure;
          __updateenclosure;
          CheckDbResult(enclosurecollection.Store(db_enclosure),'could not store enclosure');
        end;
      enclosure.Field('slots').AsObject.ForAllObjects(@__UpdateDriveSlots);
      enclosure.Field('expanders').AsObject.ForAllObjects(@__UpdateExpanders);
    end;

    procedure _deleteNoMoreExistingEnclosuresFromDB;
    var i   : NativeInt;
        obj : IFRE_DB_Object;
    begin
      for i:=0 to high(enclosure_refs) do
       begin
         if enclosure_refs[i].linked_uid<>CFRE_DB_NullGUID then
           begin
             CheckDbResult(conn.Fetch(enclosure_refs[i].linked_uid,obj),'could not fetch enclosure uid ['+FREDB_G2H(enclosure_refs[i].linked_uid)+']');
             (obj.Implementor_HC as TFRE_DB_ENCLOSURE).DeleteReferencingToMe(conn);
             obj.Finalize;
             CheckDbResult(conn.Delete(enclosure_refs[i].linked_uid),'could not delete enclosure refs uid ['+FREDB_G2H(enclosure_refs[i].linked_uid)+'] scheme ['+enclosure_refs[i].schemename+']');
           end;
       end;
    end;

    procedure _deleteNoMoreExistingDisksFromDB;
    var i   : NativeInt;
        obj : IFRE_DB_Object;
    begin
      for i:=0 to high(disk_refs) do
       begin
         if disk_refs[i].linked_uid<>CFRE_DB_NullGUID then
           begin
//             writeln('      SWL:',i,' ',disk_refs[i].schemename,' ',FREDB_G2H(disk_refs[i].linked_uid),' ',disk_refs[i].fieldname);
             CheckDbResult(conn.Fetch(disk_refs[i].linked_uid,obj),'could not fetch disk uid ['+FREDB_G2H(disk_refs[i].linked_uid)+']');
             if (obj.Implementor_HC is TFRE_DB_OS_BLOCKDEVICE) then
               CheckDbResult(conn.Delete(disk_refs[i].linked_uid),'could not delete enclosure refs uid ['+FREDB_G2H(disk_refs[i].linked_uid)+'] scheme ['+disk_refs[i].schemename+']');
             obj.Finalize;
           end;
       end;
    end;

    procedure _updateMachine(const machine:TFRE_DB_MACHINE);
    begin
      GFRE_DBI.LogInfo(dblc_APPLICATION,'Updated machine [%s] uid [%s] to db', [machine.Field('objname').asstring,machine.UID_String]);
      machine_uid := machine.UID;
      enclosure_refs := conn.GetReferencesDetailed(machine_uid,false,TFRE_DB_ENCLOSURE.ClassName);
      machine.Field('enclosures').AsObject.ForAllObjects(@_updateEnclosures);
      _deleteNoMoreExistingEnclosuresFromDB;
      disk_refs := conn.GetReferencesDetailed(machine_uid,false);
//      writeln('SWL DISKS ',machine.Field('disks').AsObject.DumpToString());
      machine.Field('disks').AsObject.ForAllObjects(@_updateDisks);
      _deleteNoMoreExistingDisksFromDB;
      blockdevicecollection.ForAll(@_InsertDisksIntoSlots);
      machine.Field('pools').AsObject.ForAllObjects(@_UpdatePools);
    end;

    procedure _updateAddMachine(const machine:TFRE_DB_MACHINE);
    var machinename  : TFRE_DB_NameType;
        db_machine   : IFRE_DB_Object;
        dbo          : IFRE_DB_Object;
        mosparentids : TFRE_DB_GUIDArray;


       procedure __insertmachine;
       begin
         dbo := TFRE_DB_MACHINE.CreateForDB;
         db_machine  := dbo.Implementor_HC as TFRE_DB_MACHINE;
         db_machine.Field('UID').asGuid := machine.UID;
         db_machine.SetAllSimpleObjectFieldsFromObject(machine);
         db_machine.Field('mosparentIds').AsObjectLinkArray := mosparentids;
         db_machine.Field('caption_mos').AsString:=machinename;

         CheckDbResult(machinecollection.Store(db_machine),'store machine in machinecollection');
         GFRE_DBI.LogInfo(dblc_APPLICATION,'Added Machine [%s] uid [%s] to db', [machinename,machine.UID_String]);
       end;

       procedure __deletemachine;
       begin
         mosparentids := dbo.Field('mosparentIds').AsObjectLinkArray;
         (dbo.Implementor_HC as TFRE_DB_MACHINE).DeleteReferencingToMe(conn);
         CheckDbResult(conn.Delete(dbo.UID),'could not delete machine uid ['+dbo.UID_String+']');
       end;

    begin
      machinename    :=  machine.field('objname').asstring;
      if machinecollection.GetIndexedObj(machinename,dbo) then
        begin
          if dbo.UID<>machine.UID then          // delete machine if UID is not matching
            begin
              __deletemachine;
              __insertmachine;
              _updateMachine(machine);
            end
          else
            begin
              _updatemachine(machine);
            end;
        end
      else
        begin
          __insertmachine;
          _updatemachine(machine);
        end;
    end;

    procedure _processUpdates(const updatestep:TFRE_DB_UPDATE_TRANSPORT);
    var obj_id      : TGUID;
        target_obj  : IFRE_DB_Object;
        update_type : TFRE_DB_ObjCompareEventType;

        //procedure _UpdateObjInMatchingCollection;
        //begin
        //  if target_obj.SchemeClass=TFOS_ARTEMES_DEVICE.ClassName then
        //    CheckDbResult(dev_coll.Update(target_obj),'could not update target obj in collection DEVICES'+FREDB_G2H(obj_id))
        //  else
        //    if target_obj.SchemeClass=TFOS_ARTEMES_MEASUREMENT.ClassName then
        //      CheckDbResult(mes_coll.Update(target_obj),'could not update target obj in collection MEASUREMENTS'+FREDB_G2H(obj_id))
        //    else
        //      CheckDbResult(conn.Update(target_obj),'could not update target obj '+FREDB_G2H(obj_id));
        //end;

    begin
      update_type   := updatestep.GetType;
      if updatestep.GetIsChild=false then        // root object with machines
        begin
          case update_type of
           cev_FieldDeleted:
             begin
               raise EFRE_DB_Exception.Create(edb_ERROR,'Unsupported field delete for subobjects in root object [%s] ',[updatestep.DumpToString()]);
             end;
           cev_FieldAdded:
             begin
               if updatestep.GetNewField.FieldType<>fdbft_Object then
                 begin
                   raise EFRE_DB_Exception.Create(edb_ERROR,'Unsupported field add for simple fields in root object [%s] ',[updatestep.DumpToString()]);
                 end
               else
                 begin
                   if (updatestep.GetNewField.AsObject.Implementor_HC is TFRE_DB_MACHINE) then
                     begin
                       _updateAddMachine ((updatestep.GetNewField.AsObject.Implementor_HC as TFRE_DB_MACHINE));
                     end
                   else
                     raise EFRE_DB_Exception.Create(edb_ERROR,'Unsupported field add for other subobjects than TFRE_DB_MACHINE [%s] ',[updatestep.DumpToString()]);
                 end;
             end;
           cev_FieldChanged:
             begin
               raise EFRE_DB_Exception.Create(edb_ERROR,'Unsupported field change for subobjects in root class [%s] ',[updatestep.DumpToString()]);
             end;
          else
            raise EFRE_DB_Exception.Create(edb_ERROR,'Invalid Update Type [%s] for id [%s]',[Inttostr(Ord(updatestep.GetType)),FREDB_G2H(obj_id)]);
          end;
        end
      else
        begin
          obj_id        := updatestep.GetTargetID;

        end;
      //CheckDbResult(conn.Fetch(obj_id,target_obj),'could not fetch object for change');
      //case update_type of
      // cev_FieldDeleted:
      //   begin
      //     if updatestep.GetOldField.FieldType<>fdbft_Object then
      //       begin
      //         target_obj.DeleteField(updatestep.GetOldFieldName);
      //         _UpdateObjInMatchingCollection;
      //       end
      //     else
      //       if target_obj.SchemeClass=TFOS_ARTEMES_DEVICE.ClassName then
      //         begin
      //           if (updatestep.GetOldField.AsObject.Implementor_HC is TFOS_ARTEMES_MEASUREMENT) then
      //             begin
      //               CheckDbResult(mes_coll.Remove(updatestep.GetOldField.asObject.UID),'could not remove measurement in update');
      //             end
      //           else
      //             raise EFRE_DB_Exception.Create(edb_ERROR,'Unsupported field delete for other subobjects than TFOS_ARTEMES_MEASUREMENT [%s] ',[updatestep.DumpToString()]);
      //         end
      //       else
      //         raise EFRE_DB_Exception.Create(edb_ERROR,'Unsupported field delete for subobjects not in TFOS_ARTEMES_DEVICE [%s] ',[updatestep.DumpToString()]);
      //   end;
      // cev_FieldAdded:
      //   begin
      //     if updatestep.GetNewField.FieldType<>fdbft_Object then
      //       begin
      //         target_obj.Field(updatestep.GetNewFieldName).CloneFromField(updatestep.GetNewField);
      //         _UpdateObjInMatchingCollection;
      //       end
      //     else
      //       if target_obj.SchemeClass=TFOS_ARTEMES_DEVICE.ClassName then
      //         begin
      //           if (updatestep.GetNewField.AsObject.Implementor_HC is TFOS_ARTEMES_MEASUREMENT) then
      //             begin
      //               mesId:=_getMesiD(obj_id,updatestep.GetNewField.AsObject);
      //               _addMeasurement(updatestep.GetNewField.AsObject,obj_id);
      //             end
      //           else
      //             raise EFRE_DB_Exception.Create(edb_ERROR,'Unsupported field add for other subobjects than TFOS_ARTEMES_MEASUREMENT [%s] ',[updatestep.DumpToString()]);
      //         end
      //       else
      //         raise EFRE_DB_Exception.Create(edb_ERROR,'Unsupported field add for subobjects not in TFOS_ARTEMES_DEVICE [%s] ',[updatestep.DumpToString()]);
      //   end;
      // cev_FieldChanged:
      //   begin
      //     if updatestep.GetNewField.FieldType<>fdbft_Object then
      //       begin
      //         target_obj.Field(updatestep.GetNewFieldName).CloneFromField(updatestep.GetNewField);
      //         _UpdateObjInMatchingCollection;
      //       end
      //     else
      //       raise EFRE_DB_Exception.Create(edb_ERROR,'Unsupported fiel update for object fields [%s] ',[updatestep.DumpToString()]);
      //   end;
      //else
      //  raise EFRE_DB_Exception.Create(edb_ERROR,'Invalid Update Type [%s] for id [%s]',[Inttostr(Ord(updatestep.GetType)),FREDB_G2H(obj_id)]);
      //end;

    end;


begin
  machinecollection      := conn.GetCollection(CFRE_DB_MACHINE_COLLECTION);
  poolcollection         := conn.GetCollection(CFRE_DB_ZFS_POOL_COLLECTION);
  blockdevicecollection  := conn.GetCollection(CFRE_DB_DEVICE_COLLECTION);
  enclosurecollection    := conn.GetCollection(CFRE_DB_ENCLOSURE_COLLECTION);
  expandercollection     := conn.GetCollection(CFRE_DB_SAS_EXPANDER_COLLECTION);
  driveslotcollection    := conn.GetCollection(CFRE_DB_DRIVESLOT_COLLECTION);

//  writeln('SWL: DISKDATA',input.DumpToString());

  unassigned_disks       := TFRE_DB_ZFS_UNASSIGNED.FetchUnassigned(conn);

  if input.FieldExists('UPDATE') then
    begin
      for i := 0 to input.Field('UPDATE').ValueCount-1 do
        begin
          _processUpdates((input.Field('UPDATE').AsObjectItem[i].Implementor_HC as TFRE_DB_UPDATE_TRANSPORT));
        end;
    end;

  result := GFRE_DB_NIL_DESC;
end;

{ TFRE_HAL_DISK }

constructor TFRE_HAL_DISK.Create;
var indbo:IFRE_DB_Object;
begin
  inherited;
  GFRE_TF.Get_Lock(hdata_lock);
  hdata       :=GFRE_DBI.NewObject;
  update_data :=GFRE_DBI.NewObject;
  snap_data   :=hdata.CloneToNewObject();
end;

destructor TFRE_HAL_DISK.Destroy;
begin

  hdata_lock.Finalize;
  hdata.Finalize;
  update_data.Finalize;
  snap_data.Finalize;

  inherited Destroy;
end;

function TFRE_HAL_DISK.GetLockedDataForMachine(const machinename: TFRE_DB_NameType): IFRE_DB_Object;
begin
  assert(length(machinename)>0,'TFRE_HAL_DISK.GetLockeDataforMachine no machiname provided!');
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

procedure TFRE_HAL_DISK.ReleaseLockedData;
begin
  hdata_lock.Release;
end;

procedure TFRE_HAL_DISK.ReceivedDBO(const dbo: IFRE_DB_Object);
var subfeedmodule : string;
    machinename   : TFRE_DB_NameType;
begin
//  writeln('SWL:',dbo.DumpToString());
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
        if subfeedmodule='ZPOOLIOSTAT' then
          UpdateZpoolIostat(dbo.Field('data').asobject,machinename)
        else
          if subfeedmodule='MPATH' then
            UpdateMpath(dbo.Field('data').asobject,machinename)
          else
            writeln('UNHANDLED SUBFEEDMODULE ',subfeedmodule);

end;

procedure TFRE_HAL_DISK.UpdateDiskAndEnclosure(const dbo: IFRE_DB_Object; const machinename: TFRE_DB_NameType);
var
  last_fdata:IFRE_DB_Object;
  mdata     :IFRE_DB_Object;
  pools     : IFRE_DB_Object;

  procedure _UpdateDisks(const obj:IFRE_DB_Object);
  var feed_disk : TFRE_DB_ZFS_BLOCKDEVICE;
      old_obj   : IFRE_DB_OBject;
      zfs_obj   : IFRE_DB_Object;

  begin
    feed_disk := obj.Implementor_HC as TFRE_DB_ZFS_BLOCKDEVICE;
    if mdata.FetchObjWithStringFieldValue('DEVICEIDENTIFIER',feed_disk.DeviceIdentifier,old_obj,'') then
      begin
        old_obj.DeleteField('zfs_guid');
        old_obj.SetAllSimpleObjectFieldsFromObject(feed_disk);
        if feed_disk.FieldExists('log') then
          begin
            if (old_obj.FieldExists('log')) then
                old_obj.Field('log').AsObject.SetAllSimpleObjectFieldsFromObject(feed_disk.Field('log').AsObject)
            else
                old_obj.Field('log').AsObject := feed_disk.Field('log').AsObject.CloneToNewObject;
          end;
        // assign zfs guid
        if assigned(pools) then
          begin
//            writeln('SWL: FIND ZFS GUID FOR ',feed_disk.DeviceName);
            if pools.FetchObjWithStringFieldValue('devicename',feed_disk.DeviceName,zfs_obj,uppercase(TFRE_DB_ZFS_BLOCKDEVICE.ClassName)) then
              begin
//                writeln('SWL: FOUND ZFS GUID ',(zfs_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).getZFSGuid);
                (old_obj.Implementor_HC as TFRE_DB_ZFS_BLOCKDEVICE).setZFSGuid((zfs_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).getZFSGuid);
              end
            else
              begin
//                writeln('SWL: NO ZFS GUID ');
              end;
          end;
      end
    else
      begin
        mdata.Field('disks').asObject.Field(feed_disk.Field('DEVICEIDENTIFIER').asstring).asobject:=feed_disk.CloneToNewObject;
      end;
  end;

  procedure _UpdateEnclosures(const obj:IFRE_DB_Object);
  var feed_enclosure : TFRE_DB_ENCLOSURE;
      old_enclosure  : IFRE_DB_OBject;

    procedure _UpdateSlots(const slotobj:IFRE_DB_Object);
    var feed_slot    : TFRE_DB_DRIVESLOT;
        old_slot     : IFRE_DB_Object;
    begin
      feed_slot      := slotobj.Implementor_HC as TFRE_DB_DRIVESLOT;
      if mdata.FetchObjWithStringFieldValue('DEVICEIDENTIFIER',feed_slot.DeviceIdentifier,old_slot,'') then
        begin
          old_slot.SetAllSimpleObjectFieldsFromObject(feed_slot);
        end
      else
        begin
          (old_enclosure.Implementor_HC as TFRE_DB_ENCLOSURE).AddDriveSlotEmbedded(feed_slot.SlotNr,(feed_slot.CloneToNewObject.Implementor_HC as TFRE_DB_DRIVESLOT));
        end;
    end;

    procedure _UpdateExpanders(const expanderobj:IFRE_DB_Object);
    var feed_expander    : TFRE_DB_SAS_EXPANDER;
        old_expander     : IFRE_DB_Object;
    begin
      feed_expander      := expanderobj.Implementor_HC as TFRE_DB_SAS_EXPANDER;
      if mdata.FetchObjWithStringFieldValue('DEVICEIDENTIFIER',feed_expander.DeviceIdentifier,old_expander,'') then
        begin
          old_expander.SetAllSimpleObjectFieldsFromObject(feed_expander);
        end
      else
        begin
          (old_enclosure.Implementor_HC as TFRE_DB_ENCLOSURE).AddExpanderEmbedded((feed_expander.CloneToNewObject.Implementor_HC as TFRE_DB_SAS_EXPANDER));
        end;
    end;

  begin
    feed_enclosure   := obj.Implementor_HC as TFRE_DB_ENCLOSURE;
    if mdata.FetchObjWithStringFieldValue('DEVICEIDENTIFIER',feed_enclosure.DeviceIdentifier,old_enclosure,'') then
      begin
        old_enclosure.SetAllSimpleObjectFieldsFromObject(feed_enclosure);
        feed_enclosure.Field('slots').AsObject.ForAllObjects(@_updateslots);
        feed_enclosure.Field('expanders').AsObject.ForAllObjects(@_updateexpanders);
      end
    else
      begin
        mdata.Field('enclosures').asObject.Field(feed_enclosure.Field('DEVICEIDENTIFIER').asstring).asobject:=feed_enclosure.CloneToNewObject;
      end;

  end;

begin
  mdata := GetLockedDataForMachine(machinename);
  try
    last_fdata := mdata.CloneToNewObject;

    if mdata.FieldExists('pools') then
      pools := mdata.Field('pools').AsObject;

    if not mdata.FieldExists('disks') then
      mdata.Field('disks').AsObject:=GFRE_DBI.NewObject;
    dbo.Field('disks').AsObject.ForAllObjects(@_updatedisks);

    if not mdata.FieldExists('enclosures') then
      mdata.Field('enclosures').AsObject:=GFRE_DBI.NewObject;

    dbo.Field('enclosures').AsObject.ForAllObjects(@_updateenclosures);

//    writeln('OLDFDATA:',last_fdata.DumpToString());

//    CheckDifferences(last_fdata);

    last_fdata.Finalize;
  finally
    ReleaseLockedData;
  end;
end;

procedure TFRE_HAL_DISK.UpdateIostat(const dbo: IFRE_DB_Object; const machinename: TFRE_DB_NameType);
var mdata:IFRE_DB_Object;

  procedure _UpdateIostat(const obj:IFRE_DB_Object);
  var feed_io   : TFRE_DB_IOSTAT;
      old_obj   : IFRE_DB_Object;
      disk_obj  : IFRE_DB_Object;
      new_io    : IFRE_DB_Object;
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
            (old_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).IoStat:=(new_io.Implementor_HC as TFRE_DB_IOSTAT);
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

procedure TFRE_HAL_DISK.UpdateZpoolStatus(const dbo: IFRE_DB_Object; const machinename: TFRE_DB_NameType);
var mdata: IFRE_DB_Object;

  procedure _UpdatePools(const obj:IFRE_DB_Object);
  var feed_pool : TFRE_DB_ZFS_POOL;
      new_pool  : TFRE_DB_ZFS_POOL;
      old_pool  : IFRE_DB_Object;


    procedure _UpdateHierarchic(const new_obj:IFRE_DB_Object; var halt:boolean);
    var zfs_guid     : string;
        zfs_obj      : IFRE_DB_Object;
        zpool_iostat : TFRE_DB_ZPOOL_IOSTAT;
    begin
      halt :=false;
      if not (new_obj.Implementor_HC is TFRE_DB_ZFS_OBJ) then
        exit;
      zfs_guid := new_obj.Field('zfs_guid').asstring;
      if zfs_guid<>'' then
        begin
          if old_pool.FetchObjWithStringFieldValue('ZFS_GUID',zfs_guid,zfs_obj,'') then
            begin
              new_obj.Field('UID').AsGUID := zfs_obj.UID;
              if (zfs_obj.Implementor_HC is TFRE_DB_ZFS_OBJ) then
                begin
                  zpool_iostat :=(zfs_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).ZPoolIostat;
                  if assigned(zpool_iostat) then
                    begin
                      (new_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).ZPoolIostat:= (zpool_iostat.CloneToNewObject.Implementor_HC as TFRE_DB_ZPOOL_IOSTAT);
                    end;
                end;
            end
          else
            GFRE_DBI.LogInfo(dblc_APPLICATION,'OBJECT WITH ZFS GUID [%s] NOT IN LAST POOL STATUS [%s]',[zfs_guid,feed_pool.Field('pool').asstring]);
        end
      else
        GFRE_DBI.LogError(dblc_APPLICATION,'OBJECT HAS NO ZFS GUID [%s]',[new_obj.DumpToString()]);
    end;

  begin
    feed_pool   := obj.Implementor_HC as TFRE_DB_ZFS_POOL;
    new_pool    := (feed_pool.CloneToNewObject.Implementor_HC as TFRE_DB_ZFS_POOL);
//    writeln('SWL: NEW POOL ',new_pool.DumpToString());
    if mdata.FetchObjWithStringFieldValue('POOL',feed_pool.Field('pool').asstring,old_pool,'TFRE_DB_ZFS_POOL') then
      begin
        new_pool.ForAllObjectsBreakHierarchic(@_updateHierarchic);
        mdata.Field('pools').AsObject.Field(feed_pool.Field('pool').asstring).AsObject := new_pool;
      end
    else
      begin
        mdata.Field('pools').AsObject.Field(feed_pool.Field('pool').asstring).AsObject:=feed_pool.CloneToNewObject;
      end;
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

procedure TFRE_HAL_DISK.UpdateZpoolIostat(const dbo: IFRE_DB_Object; const machinename: TFRE_DB_NameType);
var mdata: IFRE_DB_Object;

  procedure _UpdatezpoolIostat(const obj:IFRE_DB_Object);
  var feed_zpool_io   : TFRE_DB_ZPOOL_IOSTAT;
      old_obj         : IFRE_DB_Object;
      new_io          : IFRE_DB_Object;
  begin
    feed_zpool_io     := obj.Implementor_HC as TFRE_DB_ZPOOL_IOSTAT;
//    writeln(feed_zpool_io.DumpToString());
//    writeln(feed_zpool_io.Field('zfs_guid').asstring);
    if mdata.FetchObjWithStringFieldValue('ZFS_GUID',feed_zpool_io.Field('ZFS_GUID').asstring,old_obj,'TFRE_DB_ZPOOL_IOSTAT') then
      begin
//        writeln('update zpool iostat direct');
        old_obj.SetAllSimpleObjectFieldsFromObject(feed_zpool_io);
      end
    else
      begin
        if mdata.FetchObjWithStringFieldValue('ZFS_GUID',feed_zpool_io.Field('ZFS_GUID').asstring,old_obj,'') then
          begin
//            writeln('new zpool iostat');
            new_io := feed_zpool_io.CloneToNewObject;
            (old_obj.Implementor_HC as TFRE_DB_ZFS_OBJ).ZPoolIostat:=(new_io.Implementor_HC as TFRE_DB_ZPOOL_IOSTAT);
          end
        else
          begin
            writeln('update zpool iostat for unknown zfs guid:',feed_zpool_io.Field('zfs_guid').asstring);
          end;
      end;
  end;

  procedure _UpdatePool(const zobj:IFRE_DB_Object);
  begin
    zobj.ForAllObjects(@_UpdatezpoolIostat);
  end;

begin
  mdata := GetLockedDataForMachine(machinename);
  try

    if not mdata.FieldExists('pools') then
      mdata.Field('pools').AsObject:=GFRE_DBI.NewObject;

//    writeln('ZPOOLIOSTAT',dbo.DumpToString());

    dbo.ForAllObjects(@_UpdatePool);


  finally
    ReleaseLockedData;
  end;

end;

procedure TFRE_HAL_DISK.UpdateMpath(const dbo: IFRE_DB_Object; const machinename: TFRE_DB_NameType);
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


function TFRE_HAL_DISK.GetUpdateDataAndTakeSnaphot: IFRE_DB_Object;

    procedure _Update(const is_child_update : boolean ; const update_obj : IFRE_DB_Object ; const update_type :TFRE_DB_ObjCompareEventType  ;const new_field, old_field: IFRE_DB_Field);
    var update_step                  : IFRE_DB_Object;
    begin
      update_step := TFRE_DB_UPDATE_TRANSPORT.CreateUpdateObject(is_child_update,update_obj,update_type,new_field,old_field);
      update_data.Field('UPDATE').AddObject(update_step);
    end;

    procedure _Insert(const o : IFRE_DB_Object);
    begin
    end;

    procedure _Delete(const o : IFRE_DB_Object);
    begin
    end;

begin
  hdata_lock.Acquire;
  try

    GFRE_DBI.GenerateAnObjChangeList(hdata,snap_data,@_Insert,@_Delete,@_Update);
    writeln('SWL: UPDATES:',update_data.DumpToString());
    snap_data.Finalize;

    snap_data := hdata.CloneToNewObject;

    result := update_data.CloneToNewObject;
    update_data.ClearAllFields;
  finally
    hdata_lock.Release;
  end;
end;

procedure TFRE_HAL_DISK.ClearSnapshotAndUpdates;
begin
  hdata_lock.Acquire;
  try
    snap_data.ClearAllFields;
    update_data.ClearAllFields;
  finally
    hdata_lock.Release;
  end;

end;

function TFRE_HAL_DISK.GetPools(const remoteuser: string; const remotehost: string; const remotekey: string): IFRE_DB_OBJECT;
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

function TFRE_HAL_DISK.FetchPoolConfiguration(const zfs_pool_name: string; const remoteuser: string; const remotehost: string; const remotekey: string): IFRE_DB_OBJECT;
var zo    : TFRE_DB_ZFS;
    obj   : IFRE_DB_Object;
    error : string;
    res   : integer;
begin
  zo     := TFRE_DB_ZFS.create;
  try
    zo.SetRemoteSSH(remoteuser, remotehost, remotekey);
    res    := zo.GetPoolStatus(zfs_pool_name,error,obj);
    result := GFRE_DBI.NewObject;
    result.Field('resultcode').AsInt32 := res;
    result.Field('error').asstring     := error;
    result.Field('data').AsObject      := obj;
  finally
    zo.Free;
  end;
end;

function TFRE_HAL_DISK.CreateDiskpool(const input: IFRE_DB_Object; const remoteuser: string; const remotehost: string; const remotekey: string): IFRE_DB_OBJECT;
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

procedure TFRE_HAL_DISK.REM_GetDiskInformation(const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
begin
  // AnswerSyncCommand(command_id,GetDiskInformation(input.Field('remoteuser').asstring,input.Field('remotehost').asstring,input.Field('remotekey').asstring));
  input.Finalize;
end;

procedure TFRE_HAL_DISK.REM_GetPools(const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
begin
  // AnswerSyncCommand(command_id,GetPools(input.Field('remoteuser').asstring,input.Field('remotehost').asstring,input.Field('remotekey').asstring));
  input.Finalize;
end;

procedure TFRE_HAL_DISK.REM_GetPoolConfiguration(const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
begin
  // AnswerSyncCommand(command_id,GetPoolConfiguration(input.Field('poolname').asstring,input.Field('remoteuser').asstring,input.Field('remotehost').asstring,input.Field('remotekey').asstring));
  input.Finalize;
end;

procedure TFRE_HAL_DISK.REM_CreateDiskpool(const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
begin
  // AnswerSyncCommand(command_id,CreateDiskpool(input,input.Field('remoteuser').asstring,input.Field('remotehost').asstring,input.Field('remotekey').asstring));
  input.Finalize;
end;

//function TFOS_STATS_CONTROL.Get_Disk_Data: IFRE_DB_Object;
//var
//  DISKAGGR: IFRE_DB_Object;
//
//  procedure _addDisk(const field: IFRE_DB_Field);
//    begin
//    if pos('C',field.FieldName)<>1 then exit; //fixxme - hack to check for disks
//    DISKAGGR.Field('rps').AsInt64:=DISKAGGR.Field('rps').AsInt64 + field.AsObject.Field('rps').AsInt64;
//    DISKAGGR.Field('wps').AsInt64:=DISKAGGR.Field('wps').AsInt64 + field.AsObject.Field('wps').AsInt64;
//  end;
//
//begin
//  result := FDiskMon.Get_Data_Object;
//  DISKAGGR:=GFRE_DBI.NewObject;
//  DISKAGGR.Field('rps').AsInt64:=0;
//  DISKAGGR.Field('wps').AsInt64:=0;
//  result.ForAllFields(@_addDisk);
//
//  result.Field('disk_aggr').AsObject := DISKAGGR;
//end;


end.

