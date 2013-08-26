unit fre_alert;

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

interface

uses
  Classes, SysUtils,FOS_TOOL_INTERFACES,
  Process,
  FRE_HAL_UTILS,
  FRE_DB_INTERFACE,
  FRE_SYSTEM,
  fre_testcase,
  fre_mail;



type
  EFRE_ALERTEXCEPTION = class (Exception);

  TFRE_AlertMode = (almo_NoReaction, almo_EveryWarningAndError, almo_EveryError, almo_Change,almo_ChangeTimeout);
  TFRE_Alerttype = (alty_NoAlert, alty_Mail, alty_SMS, alty_SNMPTrap);

const
  CFRE_AlertMode   : Array[TFRE_AlertMode] of string  = ('NOREACTION','EVERYWARNINGANDERROR','EVERYERROR','CHANGE','CHANGETIMEOUT');
  CFRE_AlertType   : Array[TFRE_AlertType] of string  = ('NOALERT','MAIL','SMS','SNMPTRAP');
  cfield_alertmode = 'alertmode';
  cfield_alerttype = 'alerttype';
  cfield_common    = 'common';
  cfield_timeout   = 'timeout';

  cdefault_timeout = 300;

type
  { TFRE_Alert }

  TFRE_Alert = class
  private
    alerting_config_dbo      : IFRE_DB_Object;
    alerting_status_dbo      : IFRE_DB_Object;
    mail_alerts              : IFRE_DB_Object;
    sms_alerts               : IFRE_DB_Object;
    snmp_alerts              : IFRE_DB_Object;
    default_alertmode        : TFRE_AlertMode;
    default_alerttype        : TFRE_Alerttype;

    function        Config                      (const alertkey:string='common') : IFRE_DB_Object;
    procedure       _AddAlertingEmails          (const email  : TFRE_DB_StringArray; const alertkey : string);
    procedure       _RemoveAlertingEmails       (const email  : TFRE_DB_StringArray; const alertkey : string);
    procedure       _AddAlertType               (const alty   : TFRE_Alerttype; const alertkey : string);
    procedure       _CheckAlert                 (const alertkey: string; const alertstatus:IFRE_DB_Object);

    procedure       AddSMS                      (const alertkey: string; const status:string; const statusdetail: string);
    procedure       AddMail                     (const email: string;const alertkey: string; const status:string; const statusdetail: string);
    procedure       AddSNMP                     (const alertkey: string; const status:string; const statusdetail: string);

  public

    constructor     Create; virtual;

    procedure       CheckAlerting               (const actmon : IFRE_DB_OBJECT);

    procedure       UpdateStatus                (const alertkey : string; const status : TFRE_SignalStatus; const statusdetail: string);

    procedure       SendAlerts                  ;
    procedure       ClearAlerts                 ;

    procedure       ClearStatus                 ;
    procedure       ClearConfig                 ;

    procedure       AddAlertKeys                (const alertkeys : TFRE_DB_StringArray);
    procedure       RemoveAlertKeys             (const alertkeys : TFRE_DB_StringArray);

    procedure       AddAlertingEMailCommon      (const email  : TFRE_DB_StringArray);
    procedure       AddAlertingEMailForKeys     (const email  : TFRE_DB_StringArray; const alertkeys : TFRE_DB_StringArray);
    procedure       RemoveAlertingEMailCommon   (const email  : TFRE_DB_StringArray);
    procedure       RemoveAlertingEMailForKeys  (const email  : TFRE_DB_StringArray; const alertkeys : TFRE_DB_StringArray);
    procedure       ClearAlertingEMailCommon    ;
    procedure       ClearAlertingEMailForKeys   (const alertkeys : TFRE_DB_StringArray);


    procedure       SetAlertModeCommon          (const almo   : TFRE_AlertMode);
    procedure       SetAlertModeForKeys         (const almo   : TFRE_AlertMode; const alertkeys: TFRE_DB_StringArray);
    procedure       ClearAlertModeForKeys       (const alertkeys: TFRE_DB_StringArray);

    procedure       AddAlertTypeCommon          (const alty   : TFRE_Alerttype);
    procedure       AddAlertTypeForKeys         (const alty   : TFRE_Alerttype; const alertkeys: TFRE_DB_StringArray);
    procedure       SetAlertTypeCommon          (const alty   : TFRE_Alerttype);
    procedure       ClearAlertTypeForKeys       (const alertkeys: TFRE_DB_StringArray);

    procedure       SetChangeTimeoutCommon      (const timeout: integer);
    procedure       SetChangeTimeoutForKeys     (const timeout: integer; const alertkeys: TFRE_DB_StringArray);

    procedure       SetSMTPHost                 (const host: string; const port : string; const user: string; const passwd: string);
    procedure       SetMailFrom                 (const from: string);
    procedure       SetMailSubject              (const subject : string);

    procedure       LoadAlertConfig;
    procedure       SaveAlertConfig;
    procedure       LoadAlertStatus;
    procedure       SaveAlertStatus;
  end;

  function GetAlertMode(const s: string) : TFRE_AlertMode;
  function GetAlertType(const s: string) : TFRE_Alerttype;


implementation

function GetAlertMode(const s: string): TFRE_AlertMode;
begin
  case Uppercase(s) of
   'NOREACTION'           : result := almo_NoReaction;
   'EVERYWARNINGANDERROR' : result := almo_EveryWarningAndError;
   'EVERYERROR'           : result := almo_EveryError;
   'CHANGE'               : result := almo_Change;
   'CHANGETIMEOUT'        : result := almo_ChangeTimeout;
  else
    raise EFRE_ALERTEXCEPTION.CREATE('INVALID ALERTMODE '+s);
  end;
end;

function GetAlertType(const s: string): TFRE_Alerttype;
begin
  case Uppercase(s) of
   'NOALERT'           : result := alty_NoAlert;
   'MAIL'              : result := alty_Mail;
   'SMS'               : result := alty_SMS;
   'SNMPTRAP'          : result := alty_SNMPTrap;
  else
    raise EFRE_ALERTEXCEPTION.CREATE('INVALID ALERTTYPE '+s);
  end;
end;

{ TFRE_Alert }

function TFRE_Alert.Config(const alertkey: string): IFRE_DB_Object;
begin
  if alerting_config_dbo.FieldExists(alertkey) then begin
    result := alerting_config_dbo.Field(alertkey).AsObject;
  end else begin
    result := GFRE_DBI.NewObject;
    alerting_config_dbo.Field(alertkey).AsObject := result;
  end;
end;

procedure TFRE_Alert._RemoveAlertingEmails(const email: TFRE_DB_StringArray; const alertkey: string);
var i: integer;
    j: integer;
begin
  if Config(alertkey).FieldExists('email') then begin
    for i := 0 to high(email) do begin
      for j := Config(alertkey).Field('email').ValueCount-1 downto 0 do begin
        if email[i] = Config(alertkey).Field('email').AsStringItem[j] then begin
          Config(alertkey).Field('email').RemoveString(j);
        end;
      end;
    end;
  end;
end;

procedure TFRE_Alert._AddAlertType(const alty: TFRE_Alerttype; const alertkey: string);
var i :integer;
begin
  for i:= 0 to Config(alertkey).Field('alerttype').ValueCount-1 do begin
    if Config(alertkey).Field('alerttype').AsStringItem[i]=CFRE_AlertType[alty] then begin
      exit;
    end;
  end;
  Config(alertkey).Field('alerttype').AddString(CFRE_AlertType[alty]);
end;


procedure TFRE_Alert.AddSMS(const alertkey: string; const status: string; const statusdetail: string);
begin
  sms_alerts.Field('sms').AddString('ALERT: '+alertkey+' STATUS: '+status+' '+statusdetail);
end;

procedure TFRE_Alert.AddMail(const email: string; const alertkey: string; const status: string; const statusdetail: string);
begin
  mail_alerts.Field(GFRE_BT.Str2HexStr(email)).AddString('ALERT : '+alertkey+#13#10+ 'STATUS: '+status+#13#10+statusdetail);
end;

procedure TFRE_Alert.AddSNMP(const alertkey: string; const status: string; const statusdetail: string);
begin
  snmp_alerts.Field('snmp').AddString('ALERT: '+alertkey+' STATUS: '+status+' '+statusdetail);
end;

procedure TFRE_Alert._AddAlertingEmails(const email: TFRE_DB_StringArray; const alertkey: string);
var i: integer;
    j: integer;
    found : boolean;
begin
  for  i := low(email) to high(email) do begin
    found  := false;
    for  j := 0 to Config(alertkey).Field('email').ValueCount-1 do begin
      if Config(alertkey).Field('email').AsStringItem[j]=email[i] then begin
        found := true;
      end;
    end;
    if found=false then begin
      Config(alertkey).Field('email').AddString(email[i]);
    end;
  end;
end;

constructor TFRE_Alert.Create;
begin
  default_alertmode    := almo_Change;
  default_alerttype    := alty_NoAlert;

  mail_alerts    := GFRE_DBI.NewObject;
  sms_alerts     := GFRE_DBI.NewObject;
  snmp_alerts    := GFRE_DBI.NewObject;

  LoadAlertConfig;
end;

procedure TFRE_Alert.CheckAlerting(const actmon: IFRE_DB_OBJECT);

 procedure SetStatusUnknown(const fld:IFRE_DB_Field);
 begin
   if (fld.FieldType=fdbft_Object) and (fld.FieldName<>uppercase(cfield_common)) then begin
//     writeln('setStatus:',fld.FieldName);
     if not alerting_status_dbo.FieldExists(fld.FieldName) then begin
       UpdateStatus(fld.FieldName,statusUnknown,'');
     end;
   end;
 end;

 procedure UpdateJobs(const fld: IFRE_DB_Field);

   function msg : string;
   var i : integer;

   begin
     if fld.AsObject.fieldexists('alertmessage') then begin
       for i := 0 to fld.AsObject.Field('alertmessage').ValueCount-1 do begin
         result := result + fld.AsObject.Field('alertmessage').AsStringItem[i]+#13#10;
       end;
     end;
   end;

 begin
   if fld.FieldType=fdbft_Object then begin
     UpdateStatus(fld.FieldName,GetSignalStatus(fld.AsObject.Field('status').asstring),msg);
   end;
 end;

 procedure CheckAlerts(const fld: IFRE_DB_Field);
 begin
   if fld.FieldType=fdbft_Object then begin
     _CheckAlert(fld.FieldName,fld.AsObject);
   end;
 end;

begin
 LoadAlertStatus;
 alerting_config_dbo.ForAllFields(@setstatusunknown);
 actmon.ForAllFields(@updatejobs);
 alerting_status_dbo.ForAllFields(@checkalerts);

// writeln(alerting_status_dbo.DumpToString);
// writeln(mail_alerts.DumpToString);
 SaveAlertStatus;
end;

procedure TFRE_Alert.UpdateStatus(const alertkey: string; const status: TFRE_SignalStatus; const statusdetail: string);
var  statusobj : IFRE_DB_Object;

  procedure _setchange;
  begin
    if not statusobj.FieldExists('change_status') then begin
//      writeln('set changestatus '+statusobj.Field('status').AsString);
      statusobj.Field('change_ts').AsDateTimeUTC := GFRE_DT.Now_UTC;
      statusobj.Field('change_status').asstring  := statusobj.Field('status').AsString;
    end;
  end;

begin
  if not alerting_status_dbo.FieldExists(alertkey) then begin
    statusobj := GFRE_DBI.NewObject;
    alerting_status_dbo.Field(alertkey).AsObject := statusobj;
  end else begin
    statusobj := alerting_status_dbo.Field(alertkey).AsObject;
  end;
  if statusobj.FieldExists('status') then begin
    if statusobj.Field('status').AsString<>CFRE_SignalStatus[status] then begin
      _setchange;
    end;
  end else begin
    statusobj.Field('change_ts').AsDateTimeUTC := GFRE_DT.Now_UTC;
    statusobj.Field('change_status').asstring  := CFRE_SignalStatus[statusUnknown];
  end;
//  writeln('update '+alertkey+' '+ CFRE_SignalStatus[status]);
  statusobj.Field('status').asstring         := CFRE_SignalStatus[status];
  statusobj.Field('actual_ts').AsDateTimeUTC := GFRE_DT.Now_UTC;
  statusobj.Field('statusdetail').asstring   := statusdetail;
end;

procedure TFRE_Alert.SendAlerts;
var mail  : TFRE_Mail_Send;
    imail : string;

  procedure _MailSend(const fld:IFRE_DB_Field);
  var i  : integer;
      txt: string;
  begin
    if fld.IsUIDField=false then begin
      txt  := 'Alert sent at: '+GFRE_DT.ToStrUTC(gfre_dt.UTCToLocalTime(gfre_dt.Now_UTC,cFRE_SERVER_DEFAULT_TIMEZONE))+#13#10;
      for i:= 0 to fld.ValueCount-1 do begin
       txt := txt + fld.AsStringItem[i]+#13#10;
      end;
      mail.SendText(Config.Field('mailfrom').asstring, GFRE_BT.HexStr2Str(fld.FieldName), Config.Field('mailsubject').asstring,txt);
    end;
  end;

begin
  mail := TFRE_Mail_Send.Create;
  mail.SetSMTPHost(Config.Field('mailhost').asstring,Config.Field('mailport').asstring,Config.Field('mailuser').asstring,Config.Field('mailpassword').asstring);
  mail_alerts.ForAllFields(@_MailSend);

//  writeln(mail_alerts.DumpToString);
//  writeln(sms_alerts.DumpToString);
//  writeln(snmp_alerts.DumpToString);
  ClearAlerts;
end;

procedure TFRE_Alert.ClearAlerts;
begin
  mail_alerts.ClearAllFields;
  sms_alerts.ClearAllFields;
  snmp_alerts.ClearAllFields;
end;

procedure TFRE_Alert.ClearStatus;
begin
  LoadAlertStatus;
  alerting_status_dbo.ClearAllFields;
  SaveAlertStatus;
end;

procedure TFRE_Alert.ClearConfig;
begin
  alerting_config_dbo.ClearAllFields;
  SaveAlertConfig;
end;

procedure TFRE_Alert.AddAlertKeys(const alertkeys: TFRE_DB_StringArray);
var i: integer;
begin
  for i := 0 to high(alertkeys) do begin
    Config(alertkeys[i]);  // generates config for key
  end;
end;

procedure TFRE_Alert.RemoveAlertKeys(const alertkeys: TFRE_DB_StringArray);
var i: integer;
begin
  for i := 0 to high(alertkeys) do begin
    alerting_config_dbo.Field(alertkeys[i]).Clear;
  end;
end;

procedure TFRE_Alert.AddAlertingEMailCommon(const email: TFRE_DB_StringArray);
begin
  _AddAlertingEmails(email,'common');
end;

procedure TFRE_Alert.AddAlertingEMailForKeys(const email: TFRE_DB_StringArray; const alertkeys: TFRE_DB_StringArray);
var i: integer;
begin
  for i := 0 to high(alertkeys) do begin
    _AddAlertingEmails(email,alertkeys[i]);
  end;
end;

procedure TFRE_Alert.RemoveAlertingEMailCommon(const email: TFRE_DB_StringArray);
begin
  _RemoveAlertingEmails(email,'common');
end;

procedure TFRE_Alert.RemoveAlertingEMailForKeys(const email: TFRE_DB_StringArray; const alertkeys: TFRE_DB_StringArray);
var i : integer;
begin
  for i := 0 to high(alertkeys) do begin
    _RemoveAlertingEmails(email,alertkeys[i]);
  end;
end;

procedure TFRE_Alert.ClearAlertingEMailCommon;
begin
  Config.Field('email').Clear;
end;

procedure TFRE_Alert.ClearAlertingEMailForKeys(const alertkeys: TFRE_DB_StringArray);
var i : integer;
begin
  for i := 0 to high(alertkeys) do begin
    Config(alertkeys[i]).Field('email').Clear;
  end;
end;

procedure TFRE_Alert.SetAlertModeCommon(const almo: TFRE_AlertMode);
begin
  Config.Field(cfield_alertmode).asstring := CFRE_AlertMode[almo];
end;

procedure TFRE_Alert.SetAlertModeForKeys(const almo: TFRE_AlertMode; const alertkeys: TFRE_DB_StringArray);
var i : integer;
begin
  for i := 0 to high(alertkeys) do begin
    Config(alertkeys[i]).Field(cfield_alertmode).asstring := CFRE_AlertMode[almo];
  end;
end;

procedure TFRE_Alert.ClearAlertModeForKeys(const alertkeys: TFRE_DB_StringArray);
var i : integer;
begin
  for i := 0 to high(alertkeys) do begin
    Config(alertkeys[i]).Field(cfield_alertmode).Clear;
  end;
end;

procedure TFRE_Alert.AddAlertTypeCommon(const alty: TFRE_Alerttype);
begin
  _AddAlertType(alty,cfield_common);
end;

procedure TFRE_Alert.AddAlertTypeForKeys(const alty: TFRE_Alerttype; const alertkeys: TFRE_DB_StringArray);
var i : integer;
begin
  for i := 0 to high(alertkeys) do begin
    _AddAlertType(alty,alertkeys[i]);
  end;
end;

procedure TFRE_Alert.SetAlertTypeCommon(const alty: TFRE_Alerttype);
begin
  Config.Field('alerttype').asString := CFRE_AlertType[alty];
end;

procedure TFRE_Alert.ClearAlertTypeForKeys(const alertkeys: TFRE_DB_StringArray);
var i : integer;
begin
  for i := 0 to high(alertkeys) do begin
    Config(alertkeys[i]).Field(cfield_alerttype).Clear;
  end;
end;

procedure TFRE_Alert.SetChangeTimeoutCommon(const timeout: integer);
begin
  Config.Field(cfield_timeout).asint32 := timeout;
end;

procedure TFRE_Alert.SetChangeTimeoutForKeys(const timeout: integer; const alertkeys: TFRE_DB_StringArray);
var i : integer;
begin
  for i := 0 to high(alertkeys) do begin
    Config(alertkeys[i]).Field(cfield_timeout).asint32 := timeout;
  end;
end;

procedure TFRE_Alert.SetSMTPHost(const host: string; const port: string; const user: string; const passwd: string);
begin
  Config.Field('mailhost').asstring := host;
  Config.Field('mailport').asstring := port;
  Config.Field('mailuser').asstring := user;
  Config.Field('mailpassword').asstring := passwd;
end;

procedure TFRE_Alert.SetMailFrom(const from: string);
begin
  Config.Field('mailfrom').asstring := from;
end;

procedure TFRE_Alert.SetMailSubject(const subject: string);
begin
  Config.Field('mailsubject').asstring := subject;
end;

procedure TFRE_Alert._CheckAlert(const alertkey: string; const alertstatus: IFRE_DB_Object);
var
  keyconfig    : IFRE_DB_Object;
  commonconfig : IFRE_DB_Object;
  mode         : TFRE_AlertMode;
  alerttypes   : Array of TFRE_Alerttype;
  status       : TFRE_SignalStatus;
  change_status: TFRE_SignalStatus;
  statusdetail : String;
  timeout      : integer;
  diff_sec     : int64;

  procedure _Alert;

    procedure _checkAlertTypes(const config:IFRE_DB_Object);
    var i         : integer;
        imail     : integer;
        alerttype : TFRE_Alerttype;
    begin
      for i:= 0 to config.Field(cfield_alerttype).ValueCount-1 do begin
        alerttype := GetAlertType(config.Field(cfield_alerttype).AsStringItem[i]);
        case alerttype of
          alty_NoAlert: ; // don't do it
          alty_SMS    : AddSMS(alertkey,CFRE_SignalStatus[status],statusdetail);
          alty_Mail   : begin
            if config.FieldExists('email') then begin
              for imail := 0 to config.Field('email').ValueCount-1 do begin
                writeln('ADDING MAIL');
                AddMail(config.Field('email').AsStringItem[imail],alertkey,CFRE_SignalStatus[status],statusdetail);
              end;
            end;
          end;
          alty_SNMPTrap: AddSNMP(alertkey,CFRE_SignalStatus[status],statusdetail);
        end;
      end;
    end;

  begin
    writeln('ALERT ' +alertkey);
    if assigned(keyconfig) and (keyconfig.FieldExists(cfield_alerttype)) then begin
      _checkAlertTypes(keyconfig);
    end else begin
      _checkAlertTypes(commonconfig);
    end;
  end;

  procedure _clearchange;
  begin
//    writeln('clear changestatus');
    alertstatus.Field('change_status').Clear;
    alertstatus.Field('change_ts').Clear;
  end;

begin
  status       := GetSignalStatus(alertstatus.Field('status').asstring);

  if alertstatus.FieldExists('change_status') then begin
    change_status   := GetSignalStatus(alertstatus.Field('change_status').asstring);
  end else begin
    change_status   := statusUnknown;
  end;

  if alertstatus.FieldExists('statusdetail') then begin
    statusdetail := alertstatus.Field('statusdetail').asstring;
  end else begin
    statusdetail := '';
  end;

  keyconfig   := nil;
  if alerting_config_dbo.FieldExists(alertkey) then begin
    keyconfig := alerting_config_dbo.Field(alertkey).AsObject;
  end;

  commonconfig := Config;
  if assigned(keyconfig) and keyconfig.FieldExists(cfield_alertmode) then begin
    mode := GetAlertMode(keyconfig.Field(cfield_alertmode).AsString);
  end else begin
    mode := GetAlertMode(commonconfig.Field(cfield_alertmode).AsString);
  end;
  if assigned(keyconfig) and keyconfig.FieldExists(cfield_timeout) then begin
    timeout := keyconfig.Field(cfield_timeout).asint32;
  end else begin
    timeout := commonconfig.Field(cfield_timeout).asint32;
  end;

  case mode of
    almo_NoReaction: ;//do it :-)
    almo_EveryError: begin
      if (status=statusFailure) then begin
        _Alert;
      end;
    end;
    almo_EveryWarningAndError: begin
      if (status=statusFailure) or (status=statusWarning) then begin
        _Alert;
      end;
    end;
    almo_Change: begin
      if status <> change_status then begin
        _Alert;
      end;
    end;
    almo_ChangeTimeout: begin
//      writeln('check change timeout');
      if alertstatus.FieldExists('change_ts') then begin
        if (status <> change_status) and (change_status=statusUnknown) then begin
//          writeln('old status unknown');
          _Alert;
          _clearchange;
        end else begin
          diff_sec := (alertstatus.Field('actual_ts').AsDateTimeUTC-alertstatus.Field('change_ts').AsDateTimeUTC) div 1000;
          if diff_sec > timeout then begin
            if status <> change_status then begin
              writeln('change timeout alert');
              _alert;
              _clearchange;
            end else begin
              writeln('change timeout clear');
              _clearchange;
            end;
          end;
        end;
      end;
    end;
  end;
  SaveAlertStatus;
end;

procedure TFRE_Alert.LoadAlertConfig;
begin
  if FileExists(cFRE_ALERTING_CONFIG_FILE) then begin
    alerting_config_dbo := GFRE_DBI.CreateFromFile(cFRE_ALERTING_CONFIG_FILE);
  end else begin
    alerting_config_dbo := GFRE_DBI.NewObject;
  end;
  if not Config.FieldExists(cfield_timeout) then begin
    Config.Field(cfield_timeout).asint32 := cdefault_timeout;
  end;
end;

procedure TFRE_Alert.SaveAlertConfig;
begin
  writeln (alerting_config_dbo.DumpToString);
  alerting_config_dbo.SaveToFile(cFRE_ALERTING_CONFIG_FILE);
end;

procedure TFRE_Alert.LoadAlertStatus;
begin
  if not Assigned(alerting_status_dbo) then begin
    if FileExists(cFRE_ALERTING_STATUS_FILE) then begin
      alerting_status_dbo := GFRE_DBI.CreateFromFile(cFRE_ALERTING_STATUS_FILE);
    end else begin
      alerting_status_dbo := GFRE_DBI.NewObject;
    end;
  end;
end;

procedure TFRE_Alert.SaveAlertStatus;
begin
//  writeln (alerting_status_dbo.DumpToString);
  alerting_status_dbo.SaveToFile(cFRE_ALERTING_STATUS_FILE);
end;

end.

