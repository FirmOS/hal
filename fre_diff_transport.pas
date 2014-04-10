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


type

   { TFRE_DB_UPDATE_TRANSPORT }

   TFRE_DB_UPDATE_TRANSPORT = class (TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     class function CreateUpdateObject(const is_child_update : boolean ; const update_obj : IFRE_DB_Object ; const update_type :TFRE_DB_ObjCompareEventType  ;const new_field, old_field: IFRE_DB_Field) : TFRE_DB_UPDATE_TRANSPORT;

     function GetIsChild  : boolean;
     function GetType     : TFRE_DB_ObjCompareEventType;
     function GetNewField : IFRE_DB_Field;
     function GetOldField : IFRE_DB_Field;
     function GetNewFieldName : TFRE_DB_NameType;
     function GetOldFieldName : TFRE_DB_NameType;
     function GetUpdateScheme : TFRE_DB_NameType;
     function GetParentUID    : TGUID;
   end;

   { TFRE_DB_INSERT_TRANSPORT }

   TFRE_DB_INSERT_TRANSPORT = class (TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     class function CreateInsertObject(const insert_obj : IFRE_DB_Object) : TFRE_DB_INSERT_TRANSPORT;

     function GetInsertObject : IFRE_DB_Object;

   end;

   { TFRE_DB_DELETE_TRANSPORT }

   TFRE_DB_DELETE_TRANSPORT = class (TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     class function CreateDeleteObject(const delete_obj : IFRE_DB_Object) : TFRE_DB_DELETE_TRANSPORT;
   end;



procedure FREDIFF_GenerateDiffContainersandAddToObject  (const first_obj:IFRE_DB_Object;const second_obj:IFRE_DB_Object;const transport_list_obj:IFRE_DB_Object);
procedure FREDIFF_ApplyTransportObjectToDB              (const transport_object: IFRE_DB_Object; const conn: IFRE_DB_CONNECTION);      // GetDefaultCollection must be defined on every object to insert in db

procedure Register_DB_Extensions;


implementation

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_UPDATE_TRANSPORT);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_INSERT_TRANSPORT);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DELETE_TRANSPORT);
  GFRE_DBI.Initialize_Extension_Objects;
end;



procedure FREDIFF_GenerateDiffContainersandAddToObject(const first_obj: IFRE_DB_Object; const second_obj: IFRE_DB_Object; const transport_list_obj: IFRE_DB_Object);
var
  update_start:IFRE_DB_Object;

  procedure _Update(const is_child_update : boolean ; const update_obj : IFRE_DB_Object ; const update_type :TFRE_DB_ObjCompareEventType  ;const new_field, old_field: IFRE_DB_Field);
  var diff_step : IFRE_DB_Object;
  begin
    diff_step := TFRE_DB_UPDATE_TRANSPORT.CreateUpdateObject(is_child_update,update_obj,update_type,new_field,old_field);
    if assigned(diff_step) then
      begin
        if update_type=cev_UpdateBlockStart then
          update_start := diff_step
        else
          begin
            if update_type=cev_UpdateBlockEnd then
              begin
                if Assigned(update_start) then begin
                  // no updates in between, drop start and end
                  update_start.Finalize;
                  update_start:=nil;
                  diff_step.Finalize;
                  exit;
                end;
              end;
            if assigned(update_start) then
              begin
                transport_list_obj.Field('diff').AddObject(update_start);
                update_start:=nil;
              end;
            transport_list_obj.Field('diff').AddObject(diff_step);
          end;
      end;
  end;

  procedure _Insert(const o : IFRE_DB_Object);
  var diff_step : IFRE_DB_Object;
  begin
    diff_step := TFRE_DB_INSERT_TRANSPORT.CreateInsertObject(o);
    transport_list_obj.Field('diff').AddObject(diff_step);
  end;

  procedure _Delete(const o : IFRE_DB_Object);
  var diff_step : IFRE_DB_Object;
  begin
    diff_step := TFRE_DB_DELETE_TRANSPORT.CreateDeleteObject(o);
    transport_list_obj.Field('diff').AddObject(diff_step);
  end;

begin
  GFRE_DBI.GenerateAnObjChangeList(first_obj,second_obj,@_Insert,@_Delete,@_Update);
end;

procedure FREDIFF_ApplyTransportObjectToDB(const transport_object: IFRE_DB_Object; const conn: IFRE_DB_CONNECTION);
var update_obj : IFRE_DB_Object;
    i          : NativeInt;

  procedure _processDiff(const diffstep:IFRE_DB_Object);
  var insert_step     : TFRE_DB_INSERT_TRANSPORT;
      update_step     : TFRE_DB_UPDATE_TRANSPORT;
      delete_step     : TFRE_DB_DELETE_TRANSPORT;

      insert_obj      : IFRE_DB_Object;
      collection_name : string;
      collection      : IFRE_DB_COLLECTION;

      function GetDefaultCollectionName(const obj:IFRE_DB_Object) : string;
      var  res_obj         : IFRE_DB_Object;
      begin
        if obj.MethodExists('GetDefaultCollection') then
          begin
            res_obj   := obj.Invoke('GetDefaultCollection',nil,nil,nil,conn);
            result    := res_obj.Field('collection').asstring;
            res_obj.Finalize;
  //            writeln('SWL: COLLECTION NAME:',collection_name);
          end
        else
          begin
            raise EFRE_DB_Exception.Create('No GetDefaultCollection Method for Diff Insert '+obj.DumpToString);
            result     := '';
            exit;
          end;
      end;

  begin
    if (diffstep.Implementor_HC is TFRE_DB_INSERT_TRANSPORT) then
      begin
        insert_step := (diffstep.Implementor_HC as TFRE_DB_INSERT_TRANSPORT);
        if insert_step.GetInsertObject.IsA('TFRE_DB_OBJECT') then
          begin
            GFRE_DBI.LogInfo(dblc_APPLICATION,'Skipping %s uid [%s]', [insert_step.GetInsertObject.SchemeClass,insert_step.UID_String]);
            exit;
          end;
        collection_name := GetDefaultCollectionName(insert_step.GetInsertObject);
        if collection_name='' then
          exit;

        insert_obj  := insert_step.GetInsertObject.CloneToNewObject;
        collection := conn.GetCollection(collection_name);
        CheckDbResult(collection.Store(insert_obj),'store '+insert_step.GetInsertObject.SchemeClass+' in '+ collection_name);
        GFRE_DBI.LogInfo(dblc_APPLICATION,'Added %s uid [%s] to db', [insert_step.GetInsertObject.SchemeClass,insert_step.UID_String]);
      end
    else if diffstep.isA(TFRE_DB_DELETE_TRANSPORT,delete_step) then
      begin
        CheckDbResult(conn.Delete(delete_step.UID),'delete '+delete_step.UID_String);
        GFRE_DBI.LogInfo(dblc_APPLICATION,'Deleted uid [%s] from db', [delete_step.UID_String]);
      end
    else if diffstep.isA(TFRE_DB_UPDATE_TRANSPORT,update_step) then
      begin
        case update_step.GetType of
          cev_UpdateBlockStart: begin
            if Assigned(update_obj) then
              raise EFRE_DB_Exception.Create('UpdateBlockStart for already assigned UpdateObject in Diff Update '+update_step.DumpToString);
            CheckDBResult(conn.Fetch(update_step.UID,update_obj),'Could not fetch Update Object in Diff Update '+update_step.UID_String);
            GFRE_DBI.LogDebug(dblc_APPLICATION,'Fetched %s uid [%s] for UpdateBlockStart', [update_obj.SchemeClass,update_obj.UID_String]);
          end;
          cev_UpdateBlockEnd: begin
            if not Assigned(update_obj) then
              raise EFRE_DB_Exception.Create('UpdateBlockEnd for not assigned UpdateObject in Diff Update '+update_step.DumpToString);
            collection_name:=GetDefaultCollectionName(update_obj);
            if collection_name='' then
              exit;
            collection := conn.GetCollection(collection_name);
            CheckDBResult(collection.Update(update_obj),'Could not Update Object in Diff Update '+update_step.UID_String);
            update_obj := nil;
            GFRE_DBI.LogDebug(dblc_APPLICATION,'Updated uid [%s] for UpdateBlockEnd', [update_step.UID_String]);
          end;
          cev_FieldAdded: begin
            if not Assigned(update_obj) then
              raise EFRE_DB_Exception.Create('FieldAdded for not assigned UpdateObject in Diff Update '+update_step.DumpToString);
            update_obj.Field(update_step.GetNewFieldName).CloneFromField(update_step.GetNewField);
            GFRE_DBI.LogDebug(dblc_APPLICATION,'FieldAdded [%s]', [update_step.GetNewFieldName]);
          end;
          cev_FieldChanged: begin
            if not Assigned(update_obj) then
              raise EFRE_DB_Exception.Create('FieldChanged for not assigned UpdateObject in Diff Update '+update_step.DumpToString);
             update_obj.Field(update_step.GetNewFieldName).CloneFromField(update_step.GetNewField);
             GFRE_DBI.LogDebug(dblc_APPLICATION,'FieldChanged [%s]', [update_step.GetNewFieldName]);
          end;
          cev_FieldDeleted: begin
            if not Assigned(update_obj) then
              raise EFRE_DB_Exception.Create('FieldDeleted for not assigned UpdateObject in Diff Update '+update_step.DumpToString);
            update_obj.DeleteField(update_step.GetOldFieldName);
            GFRE_DBI.LogDebug(dblc_APPLICATION,'FieldDeleted [%s]', [update_step.GetOldFieldName]);
          end;
        else
          raise EFRE_DB_Exception.Create('Undefined UpdateType in Diff Update '+update_step.DumpToString);
        end;
      end
    else
      raise EFRE_DB_Exception.Create('INVALID DIFF STEP IN DIFF SYNC');
  end;

begin
  update_obj := nil;
//  writeln('SWL DIFF:',transport_object.DumpToString);
  if transport_object.FieldExists('diff') then
    begin
      for i := 0 to transport_object.Field('diff').ValueCount-1 do
        begin
          _processDiff(transport_object.Field('diff').AsObjectItem[i]);
        end;
    end;
end;

{ TFRE_DB_DELETE_TRANSPORT }

class procedure TFRE_DB_DELETE_TRANSPORT.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  ('TFRE_DB_OBJECTEX');
end;

class procedure TFRE_DB_DELETE_TRANSPORT.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionID:='1.0';
end;

class function TFRE_DB_DELETE_TRANSPORT.CreateDeleteObject(const delete_obj: IFRE_DB_Object): TFRE_DB_DELETE_TRANSPORT;
begin
  result := TFRE_DB_DELETE_TRANSPORT.CreateForDB;
  result.Field('UID').AsGUID:=delete_obj.UID;
end;

{ TFRE_DB_INSERT_TRANSPORT }

class procedure TFRE_DB_INSERT_TRANSPORT.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  ('TFRE_DB_OBJECTEX');
end;

class procedure TFRE_DB_INSERT_TRANSPORT.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionID:='1.0';
end;

class function TFRE_DB_INSERT_TRANSPORT.CreateInsertObject(const insert_obj: IFRE_DB_Object): TFRE_DB_INSERT_TRANSPORT;
var new_obj : IFRE_DB_Object;
begin
  result := TFRE_DB_INSERT_TRANSPORT.CreateForDB;
  new_obj := GFRE_DBI.NewObjectSchemeByName(insert_obj.SchemeClass);
  new_obj.Field('uid').asguid:=insert_obj.UID;
  new_obj.SetAllSimpleObjectFieldsFromObject(insert_obj);
  result.Field('UID').AsGUID:=new_obj.UID;
  result.Field('N').AsObject:=new_obj;
end;

function TFRE_DB_INSERT_TRANSPORT.GetInsertObject: IFRE_DB_Object;
begin
  result := Field('N').AsObject;
end;

{ TFRE_DB_UPDATE_TRANSPORT }

class procedure TFRE_DB_UPDATE_TRANSPORT.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  ('TFRE_DB_OBJECTEX');
end;

class procedure TFRE_DB_UPDATE_TRANSPORT.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionID:='1.0';
end;

class function TFRE_DB_UPDATE_TRANSPORT.CreateUpdateObject(const is_child_update: boolean; const update_obj: IFRE_DB_Object; const update_type: TFRE_DB_ObjCompareEventType; const new_field, old_field: IFRE_DB_Field): TFRE_DB_UPDATE_TRANSPORT;
begin

  if (update_type=cev_FieldAdded) and (new_field.FieldType=fdbft_Object) then
    begin
      exit(nil);  { do not send insert for fields with type object, handled in insert object  }
    end;

  if ((update_type=cev_FieldDeleted) or (update_type=cev_FieldChanged)) and (old_field.FieldType=fdbft_Object) then
    begin
      exit(nil);  { do not send update oder delete for fields with type object, handled in insert and delete object  }
    end;

  result := TFRE_DB_UPDATE_TRANSPORT.CreateForDB;
  result.Field('UID').AsGUID       := update_obj.UID;
//  result.Field('C').AsBoolean     := is_child_update;
  result.Field('T').AsByte        := Ord(update_type);
  if (update_type=cev_UpdateBlockStart) or (update_type=cev_UpdateBlockEnd) then
    exit;
  result.Field('S').asstring      := update_obj.SchemeClass;
  if assigned(update_obj.Parent) then
    result.Field('P').asGUID        := update_obj.Parent.UID;

  if assigned(new_field) then
    begin
      result.Field('NFN').asstring := new_field.FieldName;
      result.Field('N').CloneFromField(new_field);
    end;
  if assigned(old_field) then
    begin
      if update_type=cev_FieldDeleted then
        result.Field('OFN').asstring := old_field.FieldName;
      if (old_field.FieldType=fdbft_Object) then
        result.Field('O').CloneFromField(old_field);
    end;
end;

function TFRE_DB_UPDATE_TRANSPORT.GetIsChild: boolean;
begin
  result :=  Field('C').AsBoolean;
end;

function TFRE_DB_UPDATE_TRANSPORT.GetType: TFRE_DB_ObjCompareEventType;
begin
  result := TFRE_DB_ObjCompareEventType(Field('T').AsByte);
end;

function TFRE_DB_UPDATE_TRANSPORT.GetNewField: IFRE_DB_Field;
begin
  result := Field('N');
end;

function TFRE_DB_UPDATE_TRANSPORT.GetOldField: IFRE_DB_Field;
begin
  result := Field('O');
end;

function TFRE_DB_UPDATE_TRANSPORT.GetNewFieldName: TFRE_DB_NameType;
begin
  result := Field('NFN').asstring;
end;

function TFRE_DB_UPDATE_TRANSPORT.GetOldFieldName: TFRE_DB_NameType;
begin
  result := Field('OFN').asstring;
end;

function TFRE_DB_UPDATE_TRANSPORT.GetUpdateScheme: TFRE_DB_NameType;
begin
  result := Field('S').asstring;
end;

function TFRE_DB_UPDATE_TRANSPORT.GetParentUID: TGUID;
begin
  result := Field('P').AsGUID;
end;


end.
