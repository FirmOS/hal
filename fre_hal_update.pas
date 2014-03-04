unit fre_hal_update;

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
  Process,   FRE_DB_COMMON,
  FRE_DBBUSINESS,
  FRE_DB_INTERFACE;


type

   { TFRE_DB_UPDATE_TRANSPORT }

   TFRE_DB_UPDATE_TRANSPORT = class (TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     class function CreateUpdateObject(const is_child_update : boolean ; const update_obj : IFRE_DB_Object ; const update_type :TFRE_DB_ObjCompareEventType  ;const new_field, old_field: IFRE_DB_Field) : TFRE_DB_UPDATE_TRANSPORT;

     function GetTargetID : TGUID;
     function GetIsChild  : boolean;
     function GetType     : TFRE_DB_ObjCompareEventType;
     function GetNewField : IFRE_DB_Field;
     function GetOldField : IFRE_DB_Field;
     function GetNewFieldName : TFRE_DB_NameType;
     function GetOldFieldName : TFRE_DB_NameType;
     function GetUpdateScheme : TFRE_DB_NameType;
     function GetParentUID    : TGUID;
   end;


procedure Register_DB_Extensions;


implementation

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_UPDATE_TRANSPORT);
  GFRE_DBI.Initialize_Extension_Objects;
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
  result := TFRE_DB_UPDATE_TRANSPORT.CreateForDB;
  result.Field('ID').AsGUID       := update_obj.UID;
  result.Field('C').AsBoolean     := is_child_update;
  result.Field('T').AsByte        := Ord(update_type);
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

function TFRE_DB_UPDATE_TRANSPORT.GetTargetID: TGUID;
begin
  result := Field('ID').AsGUID;
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

