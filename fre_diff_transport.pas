unit fre_diff_transport;

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
{$modeswitch nestedprocvars}
{$codepage utf-8}

interface


uses
  Classes, SysUtils,FOS_TOOL_INTERFACES,
  FRE_DB_COMMON,
  FRE_DB_INTERFACE;

const
   CDIFF_INSERT_LIST = 'insert';
   CDIFF_UPDATE_LIST = 'update';
   CDIFF_DELETE_LIST = 'delete';

type

  TFREDIFF_TransportType      = (frediffInsertObject,frediffUpdateObject,frediffDeleteObject);


  function FREDIFF_TRANSPORT_CreateInsertObject(const insert_obj : IFRE_DB_Object; const collection_assign:IFRE_DB_OBject)  : IFRE_DB_Object;
  function FREDIFF_TRANSPORT_CreateDeleteObject(const delete_obj : IFRE_DB_Object)  : IFRE_DB_Object;
  function FREDIFF_TRANSPORT_CreateUpdateObject(const is_child_update : boolean ; const update_obj : IFRE_DB_Object ; const update_type :TFRE_DB_ObjCompareEventType  ;const new_field, old_field: IFRE_DB_Field; const last_diff_update_obj:IFRE_DB_Object) : IFRE_DB_Object;
  function FREDIFF_TRANSPORT_CreateSubUpdateObject (const is_child_update : boolean ; const update_obj : IFRE_DB_Object ; const update_type :TFRE_DB_ObjCompareEventType  ;const new_field, old_field: IFRE_DB_Field; const last_diff_update_obj:IFRE_DB_Object; const parent_guid:TFRE_DB_GUID; const collection_assign:IFRE_DB_OBject; out transport_type:TFREDIFF_TransportType) : IFRE_DB_Object;

  procedure FREDIFF_GenerateRelationalDiffContainersandAddToBulkObject  (const first_obj:IFRE_DB_Object;const second_obj:IFRE_DB_Object;const collection_assign:IFRE_DB_Object;const transport_list_obj:IFRE_DB_Object);
  procedure FREDIFF_GenerateSubobjectDiffContainersandAddToBulkObject   (const first_obj:IFRE_DB_Object;const second_obj:IFRE_DB_Object;const collection_assign:IFRE_DB_Object;const transport_list_obj:IFRE_DB_Object);

  procedure FREDIFF_ApplyTransportObjectToDB                  (const transport_object: IFRE_DB_Object; const conn: IFRE_DB_CONNECTION);      // GetDefaultCollection must be defined on every object to insert in db


implementation

function FREDIFF_TRANSPORT_CreateInsertObject(const insert_obj: IFRE_DB_Object; const collection_assign: IFRE_DB_OBject): IFRE_DB_Object;
var new_obj   : IFRE_DB_Object;
    coll_name : string;
begin
  if collection_assign.FieldExists(insert_obj.SchemeClass) then
    begin
      coll_name := collection_assign.Field(insert_obj.SchemeClass).asstring;
      if coll_name='' then
        begin
          writeln('SWL: NO COLLECTION FOR ',insert_obj.SchemeClass,' SKIP');
        end;
    end
  else
    begin
      writeln('SWL: SCHEME CLASS NOT IN COLLECTION ASSIGNMENT:',insert_obj.DumpToString);
      raise EFRE_DB_Exception.Create(edb_ERROR,'SCHEME CLASS NOT IN COLLECTION ASSIGNMENT [%s]',[insert_obj.SchemeClass]);
    end;

  result := GFRE_DBI.NewObject;
  new_obj := insert_obj.CloneToNewObjectWithoutSubobjects;
  result.Field('UID').AsGUID:=new_obj.UID;
  result.Field('N').AsObject:=new_obj;
  result.Field('COLL').asstring := coll_name;
end;

//function FREDIFF_TRANSPORT_GetInsertObject(const self: IFRE_DB_Object): IFRE_DB_Object;
//begin
//  result := self.Field('N').AsObject;
//end;

function FREDIFF_TRANSPORT_CreateDeleteObject(const delete_obj: IFRE_DB_Object): IFRE_DB_Object;
begin
 result := GFRE_DBI.NewObject;
 result.Field('UID').AsGUID:=delete_obj.UID;
end;

function FREDIFF_TRANSPORT_CreateUpdateObject(const is_child_update: boolean; const update_obj: IFRE_DB_Object; const update_type: TFRE_DB_ObjCompareEventType; const new_field, old_field: IFRE_DB_Field; const last_diff_update_obj: IFRE_DB_Object): IFRE_DB_Object;
var diff_update_obj:IFRE_DB_Object;
    c              :NativeInt;
begin
  if (update_type=cev_FieldAdded) and (new_field.FieldType=fdbft_Object) then
    begin
      exit(nil);  { do not send insert for fields with type object, handled in insert object  }
    end;

  if ((update_type=cev_FieldDeleted) or (update_type=cev_FieldChanged)) and (old_field.FieldType=fdbft_Object) then
    begin
      exit(nil);  { do not send update oder delete for fields with type object, handled in insert and delete object  }
    end;

  if assigned(last_diff_update_obj) and (last_diff_update_obj.UID=update_obj.UID) then
    begin
      result          := nil;   // no new update
      diff_update_obj := last_diff_update_obj;   // use the same object
    end
  else
    begin
      result          := GFRE_DBI.NewObject;
      diff_update_obj := result;
      diff_update_obj.Field('UID').AsGUID := update_obj.UID;
    end;

  //  diff_update_obj.Field('C').AsBoolean    := is_child_update;
  //  diff_update_obj.Field('T').AsByte       := Ord(update_type);
  //  diff_update_obj.Field('S').asstring      := update_obj.SchemeClass;
  //  if assigned(update_obj.Parent) then
  //    diff_update_obj.Field('P').asGUID        := update_obj.Parent.UID;
  case update_type of
    cev_FieldAdded:
      begin
        c:=diff_update_obj.Field('I_FN').ValueCount;
        diff_update_obj.Field('I_FN').addstring(new_field.FieldName);
        diff_update_obj.Field('I_F_'+inttostr(c)).CloneFromField(new_field);
      end;
    cev_FieldChanged:
      begin
        c:=diff_update_obj.Field('U_FN').ValueCount;
        diff_update_obj.Field('U_FN').addstring(new_field.FieldName);
        diff_update_obj.Field('U_F_'+inttostr(c)).CloneFromField(new_field);
      end;
    cev_FieldDeleted:
      begin
        diff_update_obj.Field('D_FN').addstring(old_field.FieldName);
      end;
    else
      raise EFRE_DB_Exception.Create('INVALID UPDATE TYPE FOR DIFF TRANSPORT');
  end;
end;

function FREDIFF_TRANSPORT_CreateSubUpdateObject(const is_child_update: boolean; const update_obj: IFRE_DB_Object; const update_type: TFRE_DB_ObjCompareEventType; const new_field, old_field: IFRE_DB_Field; const last_diff_update_obj: IFRE_DB_Object; const parent_guid: TFRE_DB_GUID; const collection_assign: IFRE_DB_OBject;  out transport_type:TFREDIFF_TransportType): IFRE_DB_Object;
var diff_update_obj:IFRE_DB_Object;
    c              :NativeInt;
    diff_step      :IFRE_DB_Object;
    pobj           :IFRE_DB_Object;

begin
  if (update_obj.UID=parent_guid) and (update_type=cev_FieldAdded) and (new_field.FieldType=fdbft_Object) then
    begin
      // handle insert parent level
//      writeln('SWL: HANDLE PARENT INSERT');
      result := FREDIFF_TRANSPORT_CreateInsertObject(new_field.AsObject,collection_assign);
      transport_type :=frediffInsertObject;
//      writeln('SWL HANDLED');
      exit(result);
    end;

  if (update_obj.UID=parent_guid) and ((update_type=cev_FieldDeleted) or (update_type=cev_FieldChanged)) and (old_field.FieldType=fdbft_Object) then
    begin
//      writeln('SWL: HANDLE PARENT DELETE');
      result := FREDIFF_TRANSPORT_CreateDeleteObject(old_field.AsObject);
      transport_type :=frediffDeleteObject;
      exit(result);
    end;

  if assigned(last_diff_update_obj) and (last_diff_update_obj.UID=update_obj.UID) then
    begin
      result          := nil;   // no new update
      diff_update_obj := last_diff_update_obj;   // use the same object
    end
  else
    begin
      result          := GFRE_DBI.NewObject;
      diff_update_obj := result;
      diff_update_obj.Field('UID').AsGUID := update_obj.UID;
      pobj := update_obj.Parent;
      while assigned(pobj) do
        begin
//          writeln('SWL: TOP GUID ',parent_guid.AsHexString);
          if pobj.UID<>parent_guid then
            diff_update_obj.Field('P_REV').AddGuid(pobj.UID);
          pobj := pobj.Parent;
        end;
      for c :=diff_update_obj.Field('P_REV').ValueCount-1 downto 0 do
        diff_update_obj.Field('P').AddGuid(diff_update_obj.Field('P_REV').AsGUIDItem[c]);
      diff_update_obj.DeleteField('P_REV');
      diff_update_obj.Field('P').AddGUID(update_obj.UID);
    end;

  transport_type :=frediffUpdateObject;

  //  diff_update_obj.Field('C').AsBoolean    := is_child_update;
  //  diff_update_obj.Field('S').asstring      := update_obj.SchemeClass;

  case update_type of
    cev_FieldAdded:
      begin
        c:=diff_update_obj.Field('I_FN').ValueCount;
        diff_update_obj.Field('I_FN').addstring(new_field.FieldName);
        diff_update_obj.Field('I_F_'+inttostr(c)).CloneFromField(new_field);
      end;
    cev_FieldChanged:
      begin
        c:=diff_update_obj.Field('U_FN').ValueCount;
        diff_update_obj.Field('U_FN').addstring(new_field.FieldName);
        diff_update_obj.Field('U_F_'+inttostr(c)).CloneFromField(new_field);
      end;
    cev_FieldDeleted:
      begin
        diff_update_obj.Field('D_FN').addstring(old_field.FieldName);
      end;
    else
      raise EFRE_DB_Exception.Create('INVALID UPDATE TYPE FOR DIFF TRANSPORT');
  end;
end;

//function FREDIFF_TRANSPORT_GetIsChild(const self: IFRE_DB_Object): boolean;
//begin
//  result :=  self.Field('C').AsBoolean;
//end;
//
//function FREDIFF_TRANSPORT_GetCompareType(const self: IFRE_DB_Object): TFRE_DB_ObjCompareEventType;
//begin
//  result := TFRE_DB_ObjCompareEventType(self.Field('T').AsByte);
//end;
//
//function FREDIFF_TRANSPORT_GetNewField(const self: IFRE_DB_Object): IFRE_DB_Field;
//begin
//  result := self.Field('N');
//end;
//
//function FREDIFF_TRANSPORT_GetOldField(const self: IFRE_DB_Object): IFRE_DB_Field;
//begin
//  result := self.Field('O');
//end;
//
//function FREDIFF_TRANSPORT_GetNewFieldName(const self: IFRE_DB_Object): TFRE_DB_NameType;
//begin
// result := self.Field('NFN').asstring;
//end;
//
//function FREDIFF_TRANSPORT_GetOldFieldName(const self: IFRE_DB_Object): TFRE_DB_NameType;
//begin
//  result := self.Field('OFN').asstring;
//end;
//
//function FREDIFF_TRANSPORT_GetUpdateScheme(const self: IFRE_DB_Object): TFRE_DB_NameType;
//begin
//  result := self.Field('S').asstring;
//end;
//
//function FREDIFF_TRANSPORT_GetParentUID(const self: IFRE_DB_Object): TFRE_DB_GUID;
//begin
//  result := self.Field('P').AsGUID;
//end;
//
//function FREDIFF_TRANSPORT_GetTransportType(const self: IFRE_DB_OBject): TFREDIFF_TransportType;
//begin
//  result := TFREDIFF_TransportType(self.Field('TT').asbyte);
//end;


procedure FREDIFF_GenerateRelationalDiffContainersandAddToBulkObject(const first_obj: IFRE_DB_Object; const second_obj: IFRE_DB_Object; const collection_assign: IFRE_DB_Object; const transport_list_obj: IFRE_DB_Object);
var
  last_update_object:IFRE_DB_Object;
  inserted_list     :IFRE_DB_Object;
  waiting_list      :IFRE_DB_Object;
  backlog_list      :IFRE_DB_Object;

  procedure _Update(const is_child_update : boolean ; const update_obj : IFRE_DB_Object ; const update_type :TFRE_DB_ObjCompareEventType  ;const new_field, old_field: IFRE_DB_Field);
  var diff_step : IFRE_DB_Object;
  begin
    diff_step :=  FREDIFF_TRANSPORT_CreateUpdateObject(is_child_update,update_obj,update_type,new_field,old_field,last_update_object);       //TODO: Combine fields in one update object with only changed fields
    if assigned(diff_step) then
      begin
        transport_list_obj.Field(CDIFF_UPDATE_LIST).AddObject(diff_step);
        last_update_object := diff_step;
      end;
  end;

  procedure addtowaitinglist(const fieldname:string;const obj_uid:TFRE_DB_GUID);
  var  fi         : NativeInt;
       can_add    : boolean;
  begin
    can_add :=true;
    if waiting_list.FieldExists(fieldname) then
      begin
        for fi:=0 to waiting_list.Field(fieldname).ValueCount-1 do
          begin
            if waiting_list.Field(fieldname).AsGUIDItem[fi]=obj_uid then
              begin
//                writeln('SWL CAN NOT INSERT, ALREADY WAITING '+fieldname);
                can_add:=false;
              end;
          end;
      end;
    if can_add then
      begin
//        writeln('SWL CAN NOT INSERT, WAITING '+fieldname);
        waiting_list.Field(fieldname).AddGuid(obj_uid);
      end;
  end;

  function _canInsert (const o:IFRE_DB_Object; const add_to_waiting:boolean):boolean;
  var can_insert:boolean;

    procedure checklinks(const fld:IFRE_DB_Field);
    begin
      if fld.FieldType=fdbft_ObjLink then
        begin
//          writeln ('SWL: CHECK ',fld.AsGUID.AsHexString);
          if not inserted_list.FieldExists(fld.AsGUID.AsHexString) then
            begin
//              writeln('SWL: CAN NOT INSERT');
              if add_to_waiting then
                addtowaitinglist(fld.AsGUID.AsHexString,o.UID);
              can_insert := false;
            end;
        end;
    end;

  begin
    can_insert :=true;
    o.ForAllFields(@checklinks,true,true);
    result := can_insert;
  end;

  procedure _insertChecked(const diff_step:IFRE_DB_Object);
  begin
    transport_list_obj.Field(CDIFF_INSERT_LIST).AddObject(diff_step);
    inserted_list.Field(diff_step.UID.AsHexString).AsBoolean:=true;
  end;

  procedure checkwaiting(const inserted_uid:TFRE_DB_GUID);
  var insert_obj  : IFRE_DB_Object;
      ci          : Nativeint;
      waiting_fld : string;
  begin
    if waiting_list.FieldExists(inserted_uid.AsHexString) then
      begin
        for ci:=0 to waiting_list.Field(inserted_uid.AsHexString).ValueCount-1 do
          begin
            waiting_fld := waiting_list.Field(inserted_uid.AsHexString).AsGUIDItem[ci].AsHexString;
            if not backlog_list.FieldExists(waiting_fld) then
              raise EFRE_DB_Exception.Create('INTERNAL ERROR ON FINDING IN BACKLOG '+waiting_fld);
            insert_obj := backlog_list.Field(waiting_fld).AsObject;
            if _caninsert(insert_obj.Field('N').AsObject,false) then
              begin
//                writeln('SWL INSERT FROM BACKLOG ',waiting_fld);
                insert_obj := backlog_list.Field(waiting_fld).CheckOutObject;
                _insertChecked(insert_obj);
                checkWaiting(insert_obj.UID);
              end;
          end;
        waiting_list.DeleteField(inserted_uid.AsHexString);
      end;
  end;

  procedure _Insert(const o : IFRE_DB_Object);
  var diff_step  : IFRE_DB_Object;

  begin
    diff_step := FREDIFF_TRANSPORT_CreateInsertObject(o,collection_assign);
    if assigned(diff_step) then
      begin
        if _caninsert(o,true)=true then
          begin
//            writeln('SWL INSERTED ',o.UID_String);
            _insertchecked(diff_step);
            checkWaiting(diff_step.UID);
          end
        else
          begin
//            writeln('SWL BACKLOGGED ',o.UID_String);
            backlog_list.Field(o.UID.AsHexString).AsObject:=diff_step;
          end;
      end;

  end;

  procedure _Delete(const o : IFRE_DB_Object);
  var diff_step : IFRE_DB_Object;
  begin
    diff_step := FREDIFF_TRANSPORT_CreateDeleteObject(o);
    transport_list_obj.Field(CDIFF_DELETE_LIST).AddObject(diff_step);
  end;

  procedure CleanupBacklog(const fld:IFRE_DB_Field);
  var co:IFRE_DB_Object;
  begin
    if fld.FieldType=fdbft_Object then
      begin
        if _canInsert(fld.AsObject.Field('N').AsObject,false) then
          begin
//            writeln('SWL CLEAN UP BACK LOG ',fld.FieldName);
            waiting_list.DeleteField(fld.Fieldname);
            co := backlog_list.Field(fld.FieldName).CheckOutObject;
            _insertChecked(co);
          end;
      end;
  end;

  procedure CleanupWaitingList(const fld:IFRE_DB_Field);
  var co:IFRE_DB_Object;
  begin
    if fld.FieldType=fdbft_GUID then
      begin
        if not backlog_list.FieldExists(fld.FieldName) then
          begin
//            writeln('SWL CLEAN UP WAITING LIST ',fld.FieldName);
            inserted_list.Field(fld.FieldName).asboolean:=true;
            waiting_list.DeleteField(fld.Fieldname);
          end;
      end;
  end;


begin
  last_update_object :=nil;
  inserted_list      := GFRE_DBI.NewObject;
  waiting_list       := GFRE_DBI.NewObject;
  backlog_list       := GFRE_DBI.NewObject;

  GFRE_DBI.GenerateAnObjChangeList(first_obj,second_obj,@_Insert,@_Delete,@_Update);

//  writeln('SWL: ------------- WAITING',waiting_list.DumpToString);
//  writeln('SWL: ------------- BACKLOG',backlog_list.DumpToString);

//  writeln('SWL CLEANING WL');

  // mark all in waiting list as already inserted, because they have not been in the bulk insert list, so they must be in the db

  waiting_list.ForAllFields(@CleanupWaitingList,true,true);;

//  writeln('SWL: ------------- WAITING',waiting_list.DumpToString);
//  writeln('SWL: ------------- BACKLOG',backlog_list.DumpToString);


  // try to insert all in backlog, run till all remaining objects are inserted (needed if they are referring to another object in the backlog)

  while backlog_list.FieldCount(true,true)>0 do
    begin
//      writeln('SWL: BACKLOG FOR ALL FIELDS');
      backlog_list.ForAllFields(@CleanupBacklog);
    end;

//  writeln('SWL: ------------- WAITING',waiting_list.DumpToString);
//  writeln('SWL: ------------- BACKLOG',backlog_list.DumpToString);

  inserted_list.Finalize;
  waiting_list.Finalize;
  backlog_list.Finalize;
end;

procedure FREDIFF_GenerateSubobjectDiffContainersandAddToBulkObject(const first_obj: IFRE_DB_Object; const second_obj: IFRE_DB_Object; const collection_assign: IFRE_DB_Object; const transport_list_obj: IFRE_DB_Object);
var
  last_update_object : IFRE_DB_Object;
  parent_guid        : TFRE_DB_GUID;

  procedure _Update(const is_child_update : boolean ; const update_obj : IFRE_DB_Object ; const update_type :TFRE_DB_ObjCompareEventType  ;const new_field, old_field: IFRE_DB_Field);
  var diff_step      : IFRE_DB_Object;
      transport_type : TFREDIFF_TransportType;
  begin
    diff_step :=  FREDIFF_TRANSPORT_CreateSubUpdateObject(is_child_update,update_obj,update_type,new_field,old_field,last_update_object,parent_guid,collection_assign,transport_type);       //TODO: Combine fields in one update object with only changed fields
    if assigned(diff_step) then
      begin
        case transport_type of
            frediffInsertObject : transport_list_obj.Field(CDIFF_INSERT_LIST).AddObject(diff_step);
            frediffUpdateObject :
              begin
                transport_list_obj.Field(CDIFF_UPDATE_LIST).AddObject(diff_step);
                last_update_object := diff_step;
              end;
            frediffDeleteObject : transport_list_obj.Field(CDIFF_DELETE_LIST).AddObject(diff_step);
          else
            raise EFRE_DB_Exception.Create('INVALID FREDIFF TRANSPORT TYPE!');
        end;
      end;
  end;

begin
  parent_guid:=first_obj.UID;
  last_update_object :=nil;

  GFRE_DBI.GenerateAnObjChangeList(first_obj,second_obj,nil,nil,@_Update);
end;

procedure FREDIFF_ApplyTransportObjectToDB(const transport_object: IFRE_DB_Object; const conn: IFRE_DB_CONNECTION);
begin

end;
//procedure FREDIFF_ApplyTransportObjectToDB(const transport_object: IFRE_DB_Object; const conn: IFRE_DB_CONNECTION);
//var update_obj   : IFRE_DB_Object;
//    i            : NativeInt;
//    insert_array : TFRE_DB_GUIDArray;
//    insert_loops : NativeInt;
//    insert_count : NativeInt;
//    to_insert    : NativeInt;
//
//  procedure _processDiff(const diffstep:IFRE_DB_Object);
//  var insert_obj      : IFRE_DB_Object;
//      collection_name : string;
//      collection      : IFRE_DB_COLLECTION;
//      res             : TFRE_DB_Errortype;
//
//      procedure _DumpDeleteReferences(const uid:TFRE_DB_GUID);
//      var    i           : NativeInt;
//             ex_del_obj  : IFRE_DB_Object;
//             ex_refs     : TFRE_DB_ObjectReferences;
//      begin
//        ex_refs := conn.GetReferencesDetailed(uid,false);
//        for i:=0 to high(ex_refs) do
//          begin
//            GFRE_DBI.LogError(dblc_APPLICATION,'Object still linked by %s from %s uid %s', [ex_refs[i].fieldname, ex_refs[i].schemename, FREDB_G2H(ex_refs[i].linked_uid)]);
//          end;
//      end;
//
//      function GetDefaultCollectionName(const obj:IFRE_DB_Object) : string;
//      var  res_obj         : IFRE_DB_Object;
//      begin
//        if obj.MethodExists('GetDefaultCollection') then
//          begin
//            res_obj   := obj.Invoke('GetDefaultCollection',nil,nil,nil,conn);
//            result    := res_obj.Field('collection').asstring;
//            res_obj.Finalize;
//  //            writeln('SWL: COLLECTION NAME:',collection_name);
//          end
//        else
//          begin
//            raise EFRE_DB_Exception.Create('No GetDefaultCollection Method for Diff Insert '+obj.DumpToString);
//            result     := '';
//            exit;
//          end;
//      end;
//
//  begin
//    if  FREDIFF_TRANSPORT_GetTransportType(diffstep)=frediffInsertObject then
//      begin
//        if FREDIFF_TRANSPORT_GetInsertObject(diffstep).IsA('TFRE_DB_OBJECT') then
//          begin
//            GFRE_DBI.LogInfo(dblc_APPLICATION,'Skipping %s uid [%s]', [FREDIFF_TRANSPORT_GetInsertObject(diffstep).SchemeClass,diffstep.UID_String]);
//            exit;
//          end;
//        collection_name := GetDefaultCollectionName(FREDIFF_TRANSPORT_GetInsertObject(diffstep));
//        if collection_name='' then
//          exit;
//
//        insert_obj  := FREDIFF_TRANSPORT_GetInsertObject(diffstep).CloneToNewObject;
//        collection := conn.GetCollection(collection_name);
//        CheckDbResult(collection.Store(insert_obj),'store '+FREDIFF_TRANSPORT_GetInsertObject(diffstep).SchemeClass+' in '+ collection_name);
//        GFRE_DBI.LogInfo(dblc_APPLICATION,'Added %s uid [%s] to db', [FREDIFF_TRANSPORT_GetInsertObject(diffstep).SchemeClass,diffstep.UID_String]);
//      end
//    else if FREDIFF_TRANSPORT_GetTransportType(diffstep)=frediffDeleteObject  then
//      begin
//        try
//          res := conn.Delete(diffstep.UID);
//          CheckDbResult(res,'delete '+diffstep.UID_String);
//        except on E:Exception do
//          begin
////            CheckDbResult(conn.Fetch(delete_step.UID,ex_del_obj));
//            _DumpDeleteReferences(diffstep.UID);
//            GFRE_DBI.LogError(dblc_APPLICATION,'Exception in delete uid [%s] from db %s', [diffstep.UID_String,E.Message]);
//            raise E;
//          end;
//        end;
//        GFRE_DBI.LogInfo(dblc_APPLICATION,'Deleted uid [%s] from db', [diffstep.UID_String]);
//      end
//    else if FREDIFF_TRANSPORT_GetTransportType(diffstep)=frediffUpdateObject then
//      begin
//        case FREDIFF_TRANSPORT_GetCompareType(diffstep) of
//          //cev_UpdateBlockStart: begin
//          //  if Assigned(update_obj) then
//          //    raise EFRE_DB_Exception.Create('UpdateBlockStart for already assigned UpdateObject in Diff Update '+diffstep.DumpToString);
//          //  CheckDBResult(conn.Fetch(diffstep.UID,update_obj),'Could not fetch Update Object in Diff Update '+diffstep.UID_String);
//          //  GFRE_DBI.LogDebug(dblc_APPLICATION,'Fetched %s uid [%s] for UpdateBlockStart', [update_obj.SchemeClass,update_obj.UID_String]);
//          //end;
//          //cev_UpdateBlockEnd: begin
//          //  if not Assigned(update_obj) then
//          //    raise EFRE_DB_Exception.Create('UpdateBlockEnd for not assigned UpdateObject in Diff Update '+diffstep.DumpToString);
//          //  collection_name:=GetDefaultCollectionName(update_obj);
//          //  if collection_name='' then
//          //    exit;
//          //  collection := conn.GetCollection(collection_name);
//          //  CheckDBResult(collection.Update(update_obj),'Could not Update Object in Diff Update '+diffstep.UID_String);
//          //  update_obj := nil;
//          //  GFRE_DBI.LogDebug(dblc_APPLICATION,'Updated uid [%s] for UpdateBlockEnd', [diffstep.UID_String]);
//          //end;
//          cev_FieldAdded: begin
//            if not Assigned(update_obj) then
//              raise EFRE_DB_Exception.Create('FieldAdded for not assigned UpdateObject in Diff Update '+diffstep.DumpToString);
//            update_obj.Field(FREDIFF_TRANSPORT_GetNewFieldName(diffstep)).CloneFromField(FREDIFF_TRANSPORT_GetNewField(diffstep));
//            GFRE_DBI.LogDebug(dblc_APPLICATION,'FieldAdded [%s]', [FREDIFF_TRANSPORT_GetNewFieldName(diffstep)]);
//          end;
//          cev_FieldChanged: begin
//            if not Assigned(update_obj) then
//              raise EFRE_DB_Exception.Create('FieldChanged for not assigned UpdateObject in Diff Update '+diffstep.DumpToString);
//             update_obj.Field(FREDIFF_TRANSPORT_GetNewFieldName(diffstep)).CloneFromField(FREDIFF_TRANSPORT_GetNewField(diffstep));
//             GFRE_DBI.LogDebug(dblc_APPLICATION,'FieldChanged [%s]', [FREDIFF_TRANSPORT_GetNewFieldName(diffstep)]);
//          end;
//          cev_FieldDeleted: begin
//            if not Assigned(update_obj) then
//              raise EFRE_DB_Exception.Create('FieldDeleted for not assigned UpdateObject in Diff Update '+diffstep.DumpToString);
//            update_obj.DeleteField(FREDIFF_TRANSPORT_GetOldFieldName(diffstep));
//            GFRE_DBI.LogDebug(dblc_APPLICATION,'FieldDeleted [%s]', [FREDIFF_TRANSPORT_GetOldFieldName(diffstep)]);
//          end;
//        else
//          raise EFRE_DB_Exception.Create('Undefined UpdateType in Diff Update '+diffstep.DumpToString);
//        end;
//      end
//    else
//      raise EFRE_DB_Exception.Create('INVALID DIFF STEP IN DIFF SYNC');
//  end;
//
//  function _canInsert(const diffstep:IFRE_DB_Object):boolean;
//
//  procedure _checkobjectlink(const fld:IFRE_DB_Field);
//    var i: NativeInt;
//    begin
////      writeln('SWL: FIELD ',fld.FieldName, CFRE_DB_FIELDTYPE[fld.FieldType]);
//      if fld.FieldType=fdbft_ObjLink then
//        begin
//          for i:=0 to high(insert_array) do
//            begin
//              if insert_array[i]=fld.AsObjectLink then
//                begin
//                  GFRE_DBI.LogDebug(dblc_APPLICATION,'Delaying Insert of [%s] because of Field %s referencing [%s]', [diffstep.UID_String,fld.FieldName,FREDB_G2H(fld.AsObjectLink)]);
//                  result :=false;
//                end;
//            end;
//        end;
//    end;
//
//  begin
//    if diffstep.UID=CFRE_DB_NullGUID then    // allready inserted
//      exit(false);
//
//    result := true;
//    if FREDIFF_TRANSPORT_GetTransportType(diffstep)=frediffInsertObject then
//      begin
//        FREDIFF_TRANSPORT_GetInsertObject(diffstep).ForAllFields(@_checkobjectlink);
//      end
//    else
//      raise EFRE_DB_Exception.Create('the entry in the insert_list is not an insert transport');
//  end;
//
//begin
//  update_obj := nil;
////  writeln('SWL DIFF:',transport_object.DumpToString);
//try
//  if transport_object.FieldExists(CDIFF_INSERT_LIST) then
//    begin
//      // setup insert array
//      insert_count := transport_object.Field(CDIFF_INSERT_LIST).ValueCount;
//      Setlength(insert_array,insert_count);
//      for i := 0 to insert_count-1 do
//        begin
//          insert_array[i] := transport_object.Field(CDIFF_INSERT_LIST).AsObjectItem[i].UID;
//        end;
//      // check and insert
//      insert_loops := 0;
//      to_insert    := insert_count;
//      repeat
//        for i := 0 to insert_count-1 do
//          begin
//            if _canInsert(transport_object.Field(CDIFF_INSERT_LIST).AsObjectItem[i]) then
//              begin
//                _processDiff(transport_object.Field(CDIFF_INSERT_LIST).AsObjectItem[i]);
//                transport_object.Field(CDIFF_INSERT_LIST).AsObjectItem[i].Field('UID').asGuid := CFRE_DB_NullGUID;
//                insert_array[i] := CFRE_DB_NullGUID;
//                dec(to_insert);
//              end
//          end;
//        inc(insert_loops);
//      until (insert_loops>insert_count) or (to_insert=0);
////      writeln('SWL: INSERT LOOPS ',insert_loops);
//      if (to_insert>0) then
//        raise EFRE_DB_Exception.Create('Could not insert all insert_steps of insert_list, remaining '+inttostr(to_insert));
//    end;
//  if transport_object.FieldExists(CDIFF_UPDATE_LIST) then
//    begin
//      for i := 0 to transport_object.Field(CDIFF_UPDATE_LIST).ValueCount-1 do
//        begin
//          _processDiff(transport_object.Field(CDIFF_UPDATE_LIST).AsObjectItem[i]);
//        end;
//    end;
//  if transport_object.FieldExists(CDIFF_DELETE_LIST) then
//    begin
//      for i := transport_object.Field(CDIFF_DELETE_LIST).ValueCount-1 downto 0 do  // reverse order
//        begin
//          _processDiff(transport_object.Field(CDIFF_DELETE_LIST).AsObjectItem[i]);
//        end;
//    end;
//except
//  on E:Exception do begin
//    writeln('SWL EXCEPTION IN FREDIFF_Applytransport :',E.Message);  //FIXXME
//  end;
// end;
//end;


end.

