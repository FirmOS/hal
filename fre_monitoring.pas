unit fre_monitoring;

{
(§LIC)
  (c) Autor,Copyright
      Dipl.Ing.- Helmut Hartl, Dipl.Ing.- Franz Schober, Dipl.Ing.- Christian Koch
      FirmOS Business Solutions GmbH

  Licence conditions     
(§LIC_END)
}

{$codepage UTF8}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils,
  FRE_DB_INTERFACE,
  FRE_DB_COMMON,fos_tool_interfaces,fre_system,fre_diff_transport;

type

  TFRE_DB_MOS_STATUS_TYPE      = (fdbstat_ok, fdbstat_warning, fdbstat_error, fdbstat_unknown);

const
  CFRE_DB_MOS_STATUS           : array [TFRE_DB_MOS_STATUS_TYPE] of string          = ('stat_ok','stat_warning','stat_error','stat_unknown');
  CFRE_DB_MOS_COLLECTION       = 'monitoring';

type

  { TFRE_DB_VIRTUALMOSOBJECT }

 TFRE_DB_VIRTUALMOSOBJECT = class(TFRE_DB_ObjectEx)
 private
   procedure SetCaption                 (const AValue: TFRE_DB_String);
   function  GetCaption                 : TFRE_DB_String;
 protected
   class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
   class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
 public
   property  caption                    : TFRE_DB_String      read GetCaption       write SetCaption;
   procedure SetMOSStatus               (const status: TFRE_DB_MOS_STATUS_TYPE; const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION);
   function  GetMOSStatus               : TFRE_DB_MOS_STATUS_TYPE;
   procedure SetMOSKey                  (const avalue: TFRE_DB_String);
   function  GetMOSKey                  : TFRE_DB_String;
 published
   procedure _getStatusIcon             (const calc: IFRE_DB_CALCFIELD_SETTER);
   function  WEB_MOSChildStatusChanged  (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
   function  WEB_MOSStatus              (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
   function  WEB_MOSContent             (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
 end;


  procedure Register_DB_Extensions;


  procedure GFRE_MOS_SetMOSStatusandUpdate          (const mosobject:IFRE_DB_Object;const status: TFRE_DB_MOS_STATUS_TYPE; const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION);
  function  GFRE_MOS_MOSChildStatusChanged          (const mos_UID: TFRE_DB_GUID;const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):TFRE_DB_MOS_STATUS_TYPE;
  procedure GFRE_MOS_GetStatusIcon                  (const status:TFRE_DB_MOS_STATUS_TYPE;const calc: IFRE_DB_CALCFIELD_SETTER);
  function  String2DBMOSStatus                  (const fts: string): TFRE_DB_MOS_STATUS_TYPE;
  procedure CreateMonitoringCollections         (const conn: IFRE_DB_COnnection);

implementation

procedure GFRE_MOS_SetMOSStatusandUpdate(const mosobject: IFRE_DB_Object; const status: TFRE_DB_MOS_STATUS_TYPE; const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION);
var
  i         : Integer;
  mosParent : IFRE_DB_Object;
  mosParents: TFRE_DB_ObjLinkArray;
begin
  if String2DBMOSStatus(mosobject.Field('status_mos').AsString)<>status then begin
    mosobject.Field('status_mos').AsString:=CFRE_DB_MOS_STATUS[status];
    mosParents:=mosobject.Field('mosparentIds').AsObjectLinkArray;
    CheckDbResult(conn.Update(mosobject));
    for i := 0 to Length(mosParents) - 1 do begin
      CheckDbResult(conn.Fetch(mosParents[i],mosParent));
      if mosParent.MethodExists('MOSChildStatusChanged') then begin
        mosParent.Invoke('MOSChildStatusChanged',input,ses,app,conn);
      end;
    end;
  end;
end;


function GFRE_MOS_MOSChildStatusChanged(const mos_UID: TFRE_DB_GUID; const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): TFRE_DB_MOS_STATUS_TYPE;
var
  refs      : TFRE_DB_GUIDArray;
  i         : Integer;
  refObj    : IFRE_DB_Object;
  newStatus : TFRE_DB_MOS_STATUS_TYPE;
  child_stat: IFRE_DB_Object;
begin
  refs:=conn.GetReferences(mos_UID,false,'','MOSPARENTIDS');
  newStatus:=fdbstat_ok;
  for i := 0 to Length(refs) - 1 do begin
    CheckDbResult(conn.Fetch(refs[i],refObj));
    writeln('SWL: GET MOSSTATUS',refobj.SchemeClass);
    if refObj.MethodExists('MOSStatus') then begin
      child_stat:=refObj.Invoke('MOSStatus',input,ses,app,conn);
      case String2DBMOSStatus(child_stat.Field('status_mos').AsString) of
        fdbstat_ok     : ;  //do nothing
        fdbstat_warning: if newStatus=fdbstat_ok then newStatus:=fdbstat_warning;
        fdbstat_error  : newStatus:=fdbstat_error;
        fdbstat_unknown: begin
                           exit(fdbstat_unknown);
                         end;
      end;
    end else begin
      exit(fdbstat_unknown);
    end;
  end;
  result := newStatus;
end;

procedure GFRE_MOS_GetStatusIcon(const status: TFRE_DB_MOS_STATUS_TYPE; const calc: IFRE_DB_CALCFIELD_SETTER);
begin
  case status of
    fdbstat_ok     : calc.SetAsString('images_apps/citycom_monitoring/status_ok.png');
    fdbstat_warning: calc.SetAsString('images_apps/citycom_monitoring/status_warning.png');
    fdbstat_error  : calc.SetAsString('images_apps/citycom_monitoring/status_error.png');
    fdbstat_unknown: calc.SetAsString('images_apps/citycom_monitoring/status_unknown.png');
  else begin
     calc.SetAsString('images_apps/citycom_monitoring/status_unknown.png');
  end; end;
end;


function String2DBMOSStatus(const fts: string): TFRE_DB_MOS_STATUS_TYPE;
begin
  for Result in TFRE_DB_MOS_STATUS_TYPE do begin
    if CFRE_DB_MOS_STATUS[Result]=fts then exit;
  end;
end;

procedure CreateMonitoringCollections(const conn: IFRE_DB_COnnection);
var
  collection: IFRE_DB_COLLECTION;
begin
  if not conn.CollectionExists(CFRE_DB_MOS_COLLECTION) then begin
    collection  := conn.CreateCollection(CFRE_DB_MOS_COLLECTION);
    collection.DefineIndexOnField('key_mos',fdbft_String,true,true,'def',true);
  end;
end;

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_VIRTUALMOSOBJECT);
  GFRE_DBI.Initialize_Extension_Objects;
end;





{ TFRE_DB_VIRTUALMOSOBJECT }

function TFRE_DB_VIRTUALMOSOBJECT.GetCaption: TFRE_DB_String;
begin
  Result:=Field('caption_mos').AsString;
end;

function TFRE_DB_VIRTUALMOSOBJECT.GetMOSStatus: TFRE_DB_MOS_STATUS_TYPE;
begin
  Result:=String2DBMOSStatus(Field('status_mos').AsString);
end;

procedure TFRE_DB_VIRTUALMOSOBJECT.SetMOSKey(const avalue: TFRE_DB_String);
begin
  Field('key_mos').AsString:=AValue;
end;

function TFRE_DB_VIRTUALMOSOBJECT.GetMOSKey: TFRE_DB_String;
begin
  result :=Field('key_mos').AsString;
end;

procedure TFRE_DB_VIRTUALMOSOBJECT.SetCaption(const AValue: TFRE_DB_String);
begin
  Field('caption_mos').AsString:=AValue;
end;

procedure TFRE_DB_VIRTUALMOSOBJECT.SetMOSStatus(const status: TFRE_DB_MOS_STATUS_TYPE; const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION);
begin
  GFRE_MOS_SetMOSStatusandUpdate(self,status,input,ses,app,conn);
end;

procedure TFRE_DB_VIRTUALMOSOBJECT._getStatusIcon(const calc: IFRE_DB_CALCFIELD_SETTER);
begin
  GFRE_MOS_GetStatusIcon(GetMOSStatus,calc);
end;

class procedure TFRE_DB_VIRTUALMOSOBJECT.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_OBJECTEX');

  scheme.AddSchemeField('caption_mos',fdbft_String);
  scheme.AddSchemeField('status_mos',fdbft_String);
  scheme.AddSchemeField('key_mos',fdbft_String);
  scheme.AddCalcSchemeField('status_icon_mos',fdbft_String,@_getStatusIcon);
end;

class procedure TFRE_DB_VIRTUALMOSOBJECT.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
   
end;

function TFRE_DB_VIRTUALMOSOBJECT.WEB_MOSChildStatusChanged(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  SetMOSStatus(GFRE_MOS_MOSChildStatusChanged(UID,input,ses,app,conn),input,ses,app,conn);
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_VIRTUALMOSOBJECT.WEB_MOSStatus(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=GFRE_DBI.NewObject;
  Result.Field('status_mos').AsString:=Field('status_mos').AsString;
end;

function TFRE_DB_VIRTUALMOSOBJECT.WEB_MOSContent(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=TFRE_DB_HTML_DESC.create.Describe('');
end;

end.

