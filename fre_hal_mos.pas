unit fre_hal_mos;

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

{$codepage UTF8}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils,
  FRE_DB_INTERFACE,
  FRE_DB_COMMON,snmpsend,fos_tool_interfaces,fre_system,fre_diff_transport,fre_monitoring,fre_hal_schemes;

type

  { TFRE_HAL_MOS }

  TFRE_HAL_MOS = class (TFRE_DB_Base)
  private
    config_dbo                         : IFRE_DB_Object;

    hdata_lock                         : IFOS_LOCK;
    hdata                              : IFRE_DB_Object;
    snap_data                          : IFRE_DB_Object;
    update_data                        : IFRE_DB_Object;

  private

  public
    constructor Create                 ; override;
    destructor  Destroy                ; override;

    function    GetUpdateDataAndTakeSnaphot           : IFRE_DB_Object;
    procedure   ClearSnapshotAndUpdates               ;


    procedure   LoadConfiguration      ;
    procedure   StartSNMPRequests      ;
    procedure   SetResponse            (const mos_object:IFRE_DB_Object);

  end;


  TFRE_SNMPThread = class;

  TFRE_SNMP_CB = procedure(const sender: TFRE_SNMPThread; const res_boolean : boolean; const res_string: string) is nested;

  { TFRE_SNMPThread }

  TFRE_SNMPThread = class (TThread)
  private
    foid          : string;
    fhost         : string;
    fversion      : Byte;
    fcommunity    : string;
    fcallback     : TFRE_SNMP_CB;
    fobject       : IFRE_DB_Object;
  public
    constructor   Create(const mos_snmp:IFRE_DB_Object; const cb:TFRE_SNMP_CB);
    procedure     Execute; override;
  end;

  { TFRE_DB_MOS_SNMP }

  TFRE_DB_MOS_SNMP = class(TFRE_DB_VIRTUALMOSOBJECT)
  private
    snmp_thread     : TFRE_SNMPThread;
    fhal_mos        : TFRE_HAL_MOS;
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    procedure       Configure            (const oid: string; const host: string; const version: integer; const community: string);
    procedure       SendRequest          (const hal_mos:TFRE_HAL_MOS);
    procedure       SetResponse          (const res_boolean:boolean; const res_string: string);
  published
    function        WEB_MOSContent       (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;


procedure Register_DB_Extensions;


implementation

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MOS_SNMP);
  //GFRE_DBI.Initialize_Extension_Objects;
end;


{ TFRE_HAL_MOS }

procedure TFRE_HAL_MOS.LoadConfiguration;
var snmp_mos :  TFRE_DB_MOS_SNMP;

  procedure __addSwitch(const key,caption,ip:string);
  begin
    snmp_mos :=TFRE_DB_MOS_SNMP.CreateForDB;
    snmp_mos.SetMOSKey(key);
    snmp_mos.caption  :=caption;
    snmp_mos.Configure('1.3.6.1.2.1.1.1.0',ip,2,'srnemapdx8');
    config_dbo.Field(snmp_mos.UID_String).AsObject:=snmp_mos;
  end;

begin
  config_dbo := GFRE_DBI.NewObject;

  __addSwitch('MGMTSWITCHNORD01','Mgmt Switch Nord01','10.54.0.1');
  __addSwitch('MGMTSWITCHNORD02','Mgmt Switch Nord02','10.54.0.2');
  __addSwitch('MGMTSWITCHSUED01','Mgmt Switch Sued01','10.54.0.3');
  __addSwitch('MGMTSWITCHSUED02','Mgmt Switch Sued02','10.54.0.4');
  __addSwitch('10GSWITCHNORD01','10GbE Switch Nord01','10.54.0.5');
  __addSwitch('10GSWITCHNORD02','10GbE Switch Nord02','10.54.0.6');
  __addSwitch('10GSWITCHSUED01','10GbE Switch Sued01','10.54.0.7');
  __addSwitch('10GSWITCHSUED02','10GbE Switch Sued02','10.54.0.8');
  __addSwitch('FCSWITCHNORD01','FC Switch Nord01','10.54.0.9');
  __addSwitch('FCSWITCHNORD02','FC Switch Nord02','10.54.0.10');
  __addSwitch('FCSWITCHSUED01','FC Switch Sued01','10.54.0.11');
  __addSwitch('FCSWITCHSUED02','FC Switch Sued02','10.54.0.12');

end;

constructor TFRE_HAL_MOS.Create;
begin
  inherited Create;
  GFRE_TF.Get_Lock(hdata_lock);
  hdata       :=GFRE_DBI.NewObject;
  snap_data   :=GFRE_DBI.NewObject;
  update_data :=GFRE_DBI.NewObject;
end;

destructor TFRE_HAL_MOS.Destroy;
begin
  update_data.Finalize;
  snap_data.Finalize;
  hdata.Finalize;
  hdata_lock.Finalize;
  inherited Destroy;
end;

function TFRE_HAL_MOS.GetUpdateDataAndTakeSnaphot: IFRE_DB_Object;

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


procedure TFRE_HAL_MOS.ClearSnapshotAndUpdates;
begin
  hdata_lock.Acquire;
  try
    snap_data.ClearAllFields;
    update_data.ClearAllFields;
  finally
    hdata_lock.Release;
  end;
end;


procedure TFRE_HAL_MOS.StartSNMPRequests;

  procedure _startsnmp(const obj:IFRE_DB_Object);
  var snmp_mos : TFRE_DB_MOS_SNMP;
  begin
    if (obj.Implementor_HC is TFRE_DB_MOS_SNMP) then
      begin
        snmp_mos := (obj.Implementor_HC as TFRE_DB_MOS_SNMP);
        snmp_mos.SendRequest(self);
      end;
  end;

begin
  hdata_lock.Acquire;
  try
    config_dbo.ForAllObjects(@_startsnmp);
  finally
    hdata_lock.Release;
  end;
end;

procedure TFRE_HAL_MOS.SetResponse(const mos_object: IFRE_DB_Object);
begin
  hdata_lock.Acquire;
  try
    if hdata.FieldExists(mos_object.UID_String) then
      begin
        hdata.Field(mos_object.UID_String).AsObject.SetAllSimpleObjectFieldsFromObject(mos_object);
      end
    else
      begin
        hdata.Field(mos_object.UID_String).AsObject:= mos_object.CloneToNewObject();
      end;
  finally
    hdata_lock.Release;
  end;
end;

{ TFRE_SNMPThread }

constructor TFRE_SNMPThread.Create(const mos_snmp: IFRE_DB_Object; const cb: TFRE_SNMP_CB);
begin
  foid       := mos_snmp.Field('oid').asstring;
  fhost      := mos_snmp.Field('host').asstring;
  fversion   := mos_snmp.Field('version').AsByte;
  fcommunity := mos_snmp.Field('community').Asstring;
  fobject    := mos_snmp;
  fcallback  := cb;
  FreeOnTerminate:=true;
  inherited Create(false);
end;

procedure TFRE_SNMPThread.Execute;
var res_boolean: boolean;
    res_string : string;
begin
  res_boolean := SNMPGet(foid,fcommunity,fhost,res_string);
  fcallback(self,res_boolean,res_string);
end;

{ TFRE_DB_MOS_SNMP }

class procedure TFRE_DB_MOS_SNMP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_VIRTUALMOSOBJECT.ClassName);
  scheme.AddSchemeField('res_boolean',fdbft_Boolean);
  scheme.AddSchemeField('res_string',fdbft_String);

  group:=scheme.AddInputGroup('snmp').Setup('$scheme_TFRE_DB_MOS_SNMP_snmp');
  group.AddInput('res_string','$scheme_TFRE_DB_MOS_SNMP_result');
end;

class procedure TFRE_DB_MOS_SNMP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if (currentVersionId='') then begin
    currentVersionId:='1.0';
    StoreTranslateableText(conn,'snmp_content_header','SNMP Information');
  end;
end;

procedure TFRE_DB_MOS_SNMP.Configure(const oid: string; const host: string; const version: integer; const community: string);
begin
  Field('oid').asstring        := oid;
  Field('host').asstring       := host;
  Field('version').AsByte      := version;
  Field('community').Asstring  := community;
end;

procedure TFRE_DB_MOS_SNMP.SendRequest(const hal_mos: TFRE_HAL_MOS);

  procedure SNMPResponse (const sender: TFRE_SNMPThread; const res_boolean : boolean; const res_string: string);
  begin
    (sender.fobject.Implementor_HC as TFRE_DB_MOS_SNMP).SetResponse(res_boolean,res_string);
  end;

begin
  fhal_mos    := hal_mos;
  writeln('SWL: START REQUEST ',Field('host').asstring);
  snmp_thread := TFRE_SNMPThread.Create(self,@SNMPResponse);
//      TFRE_SIMPLE_HTTP_CONTENT_CB = procedure(const sender : TFRE_SIMPLE_HTTP_CLIENT ; const http_status,content_len : NativeInt ;  const contenttyp : string ; content : PByte) is nested;

end;

procedure TFRE_DB_MOS_SNMP.SetResponse(const res_boolean: boolean; const res_string: string);
begin
  Field('res_boolean').asboolean :=res_boolean;
  Field('res_string').asstring   :=res_string;
  fhal_mos.SetResponse(self);
end;

function TFRE_DB_MOS_SNMP.WEB_MOSContent(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  panel         : TFRE_DB_FORM_PANEL_DESC;
  scheme        : IFRE_DB_SchemeObject;
begin
  GFRE_DBI.GetSystemSchemeByName(SchemeClass,scheme);
  panel :=TFRE_DB_FORM_PANEL_DESC.Create.Describe(GetTranslateableTextShort(conn,'snmp_content_header'),true,false);
  panel.AddSchemeFormGroup(scheme.GetInputGroup('snmp'),GetSession(input));
  panel.FillWithObjectValues(self,GetSession(input));
  Result:=panel;
end;

end.

