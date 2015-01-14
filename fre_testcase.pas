unit fre_testcase;

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

interface

uses
  Classes, SysUtils,FRE_DB_INTERFACE, FRE_PROCESS, FOS_BASIS_TOOLS,
  FOS_TOOL_INTERFACES, fre_testmethod, FRE_DB_COMMON,fre_system;


type

  TFRE_TestPeriodic = (everyDay, everyHour, everyMinute);

const
  CFRE_TestPeriodic  : Array[TFRE_TestPeriodic]         of string  = ('EVERYDAY', 'EVERYHOUR', 'EVERYMINUTE');

  cWinRMDiskKey      = 'FOS_RMI_WIN_DISKS';
  cWinRMServiceKey   = 'FOS_RMI_WIN_SERVICES';
  cWinRMIdleKey      = 'FOS_RMI_WIN_IDLE_PROCESS';
  cWinRMStateRunning = 'RUNNING';
  cWinRMStateStopped = 'STOPPED';
  cWinRMSMDisabled   = 'DISABLED';


type

  EFOS_TESTCASE_Exception=class(Exception);

  { TFRE_DB_JobProgress }

  TFRE_DB_JobProgress = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    function        GetJobID                    : TFRE_DB_String;
    procedure       SetOutbytes                 (const value : int64);
    procedure       SetInbytes                  (const value : int64);
    procedure       SetErrorbytes               (const value : int64);
    procedure       SetTotalOutbytes            (const value : int64);
    procedure       SetJobID                    (const value : TFRE_DB_String);
  end;

  { TFRE_DB_TestcaseStatus }

  TFRE_DB_TestcaseStatus = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects            (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function        IMI_ClearStatus             (const input:IFRE_DB_Object):IFRE_DB_Object;
    function        WEB_UpdateActualStatus      (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
    function        IMI_CheckActuality          (const input:IFRE_DB_Object):IFRE_DB_Object;
    procedure       CALC_StatusIcon             (const setter: IFRE_DB_CALCFIELD_SETTER);
  end;


  { TFRE_DB_InternetTestcase }

  TFRE_DB_InternetTestcase = class (TFRE_DB_JOB)
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    function        GetAllHosts                 : TFRE_DB_StringArray;
    procedure       AddReport                   (const reportname   : string   ; const proc: TFRE_DB_Process; const rtt: Single);
  public
    procedure       PrepareTest                 (const localgateway : TFRE_DB_String; const providergateway : TFRE_DB_String; const remotehosts : TFRE_DB_StringArray);
    procedure       ExecuteJob                  ; override;
  end;

  { TFRE_DB_WinRMTarget }

  TFRE_DB_WinRMTarget = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    procedure       AnalyzeDisks                (const resultdbo: IFRE_DB_Object);
    procedure       AnalyzeServices             (const resultdbo: IFRE_DB_Object);
    procedure       AnalyzeCPUUsage             (const resultdbo: IFRE_DB_Object);
    //procedure       SetStatus                   (const status : TFRE_SignalStatus; const statussummary : string);
    //procedure       SetDetailStatus             (const detail : IFRE_DB_Object; const status : TFRE_SignalStatus; const statussummary : string);
  public
    procedure       AddTestDiskSpace            (const diskserial: string; const warnlevel_percent: byte; const errorlevel_percent: byte);
    procedure       AddTestServiceRunning       (const servicename: string);
    procedure       SetTestCPUUsage             (const warnlevel_percent: byte);
    procedure       Analyze                     (const resultdbo: IFRE_DB_Object;const report : IFRE_DB_Object);
  end;

  { TFRE_DB_WinRMTestcase }

  TFRE_DB_WinRMTestcase = class (TFRE_DB_JOB)
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    procedure       InternalSetup               ; override;
  public
    function        AddWinRMTarget              (const url:string; const user: string; const password : string) : TFRE_DB_WinRMTarget;
    procedure       ExecuteJob                  ; override;
  end;

  { TFRE_DB_MailSendTestcase }

  TFRE_DB_MailSendTestcase = class (TFRE_DB_JOB)
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    procedure       Analyze                     (const proc : TFRE_DB_Process);
  public
    procedure       AddMailserver               (const host : string; const user : string; const password: string; const mailfrom:string; const mailto:string);
    procedure       ExecuteJob                 ; override;
  end;

  { TFRE_DB_MailCheckTestcase }

  TFRE_DB_MailCheckTestcase = class (TFRE_DB_JOB)
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    procedure       Analyze                     (const testmethod : IFRE_DB_Object);
  public
    procedure       SetMailserver               (const host : string; const sendjobkey:string; const warning_seconds: integer; const error_seconds: integer);
    procedure       ExecuteJob                  ; override;
  end;

  { TFRE_DB_SMBTestcase }

  TFRE_DB_SMBTestcase = class (TFRE_DB_JOB)
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    procedure       SetFileshare                (const host : string; const user : string; const password: string; const fileshare: string; const mountpoint : string);
    procedure       ExecuteJob                 ; override;
  end;

  { TFRE_DB_MultiPingTestcase }

  TFRE_DB_MultiPingTestcase = class (TFRE_DB_JOB)
  protected
    procedure       InternalSetup               ; override;
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    procedure       AddReport                   (const proc: TFRE_DB_Process; const rtt: Single; const information : string);
    function        GetAllHosts                 : TFRE_DB_StringArray;
  public
    procedure       SetInformation              (const information: string);
    procedure       SetRTTTimeout_ms            (const rtt_timeout: integer);
    procedure       SetPingCount                (const count: integer);
    procedure       AddPingTest                 (const host : string; const information : string);
    procedure       ExecuteJob                 ; override;
  end;

  { TFRE_DB_ProcessTestcase }

  TFRE_DB_ProcessTestcase = class (TFRE_DB_JOB)
  protected
    procedure       InternalSetup               ; override;
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    procedure       AddReport                   (const proc: TFRE_DB_Process; const warning_count:integer; const error_count:integer; const information : string);
    function        GetAllProcessnames          : TFRE_DB_StringArray;
  public
    procedure       SetInformation              (const information : string);
    procedure       AddProcessTest              (const processname : string; const warning_count:integer; const error_count:integer; const information : string);
    procedure       ExecuteJob                 ; override;
  end;

  { TFRE_DB_DiskspaceTestcase }

  TFRE_DB_DiskspaceTestcase = class (TFRE_DB_JOB)
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    procedure       SetMountpoint               (const mountpoint : string; const warning_percent: integer; const error_percent: integer);
    procedure       ExecuteJob                 ; override;
  end;

  { TFRE_DB_HTTPTestcase }

  TFRE_DB_HTTPTestcase = class (TFRE_DB_JOB)
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    procedure       SetURL                      (const url : string; const header: string; const responsematch: string);
    procedure       ExecuteJob                 ; override;
  end;

  { TFRE_DB_CPULoadTestcase }

  TFRE_DB_CPULoadTestcase = class (TFRE_DB_JOB)
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    procedure       SetLimits                   (const warning_load:integer; const error_load:integer);
    procedure       ExecuteJob                 ; override;
  end;


procedure Register_DB_Extensions;
function  GetStatusIconURI(const status:string) : string;
function  GetSignalStatus(const status:string): TFRE_SignalStatus;

implementation

{ TFRE_DB_JobProgress }

class procedure TFRE_DB_JobProgress.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;


function TFRE_DB_JobProgress.GetJobID: TFRE_DB_String;
begin
  result := Field('jobid').asstring;
end;

procedure TFRE_DB_JobProgress.SetOutbytes(const value: int64);
begin
  Field('outb').AsInt64:=value;
end;

procedure TFRE_DB_JobProgress.SetInbytes(const value: int64);
begin
  Field('inb').AsInt64:=value;
end;

procedure TFRE_DB_JobProgress.SetErrorbytes(const value: int64);
begin
  Field('errb').AsInt64:=value;
end;

procedure TFRE_DB_JobProgress.SetTotalOutbytes(const value: int64);
begin
  Field('totaloutb').AsInt64:=value;
end;

procedure TFRE_DB_JobProgress.SetJobID(const value: TFRE_DB_String);
begin
  Field('jobid').AsString:=value;
end;

{ TFRE_DB_CPULoadTestcase }

class procedure TFRE_DB_CPULoadTestcase.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
 inherited RegisterSystemScheme(scheme);
 scheme.SetParentSchemeByName  (TFRE_DB_JOB.ClassName);
end;

procedure TFRE_DB_CPULoadTestcase.SetLimits(const warning_load: integer; const error_load: integer);
begin
 config.Field('warning_load').AsUInt16  := warning_load;
 config.Field('error_load').AsUInt16    := error_load;
end;

procedure TFRE_DB_CPULoadTestcase.ExecuteJob;
var
  test  : TFRE_DB_Testmethod_CPULoad;
  proc  : TFRE_DB_Process;
  res   : integer;
  error : string;
  outstring: string;
  cpuloado : IFRE_DB_Object;
begin
  test          := TFRE_DB_Testmethod_CPULoad.create;
  test.SetRemoteSSH (Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);
  try
    res      := test.CPULoad(error,cpuloado);
    if res<>0 then begin
      SetStatus(statusFailure,'FAILURE ON CPU LOAD REQUEST ');
      report.Field('STATUSDETAIL').asstring := error;
    end else begin
      if cpuloado.Field('load15').AsReal32>config.Field('error_load').AsUInt16 then begin
        SetStatus(statusFailure,'LOAD TOO HIGH !');
      end else if cpuloado.Field('load15').AsReal32>config.Field('warning_load').AsUInt16 then begin
        SetStatus(statusWarning,'LOAD IS HIGH !');
      end else begin
        SetStatus(statusOK,'LOAD OK!');
      end;
      report.Field('STATUSDETAIL').asstring:= Format(' Load 1min/5min/15min %6.3f/%6.3f/%6.3f', [cpuloado.Field('load1').AsReal32,cpuloado.Field('load5').AsReal32,cpuloado.Field('load15').AsReal32]);
    end;
  except on E: Exception do begin
    SetStatus(StatusFailure,'Exception:'+E.Message);
  end; end;
end;

{ TFRE_DB_HTTPTestcase }

class procedure TFRE_DB_HTTPTestcase.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  (TFRE_DB_JOB.ClassName);
end;

procedure TFRE_DB_HTTPTestcase.SetURL(const url: string; const header: string; const responsematch: string);
begin
 config.Field('url').AsString           := url;
 config.Field('header').AsString        := header;
 config.Field('responsematch').AsString := responsematch;
end;

procedure TFRE_DB_HTTPTestcase.ExecuteJob;
var
  test  : TFRE_DB_Testmethod_HTTP;
  proc  : TFRE_DB_Process;
  res   : integer;
  error : string;
  outstring: string;
  match : string;
begin
  test          := TFRE_DB_Testmethod_HTTP.create;
  test.SetRemoteSSH (Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);
  try
    res      := test.HTTP(config.Field('url').AsString,config.Field('header').AsString,error,outstring);
    if res<>0 then begin
      SetStatus(statusFailure,'FAILURE ON HTTP REQUEST '+config.Field('url').AsString);
      report.Field('STATUSDETAIL').asstring := error;
    end else begin
      match  := config.Field('responsematch').AsString;
      if (match='') or (Pos(match,outstring)>0) then begin
        SetStatus(statusOK,'HTTP RESPONSE OK');
      end else begin
        SetStatus(statusWarning,'HTTP RESPONSE IS NOT AS EXPECTED');
        report.Field('STATUSDETAIL').asstring:=outstring;
      end;
    end;
  except on E: Exception do begin
    SetStatus(StatusFailure,'Exception:'+E.Message);
  end; end;
end;

{ TFRE_DB_DiskspaceTestcase }

class procedure TFRE_DB_DiskspaceTestcase.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  (TFRE_DB_JOB.ClassName);
end;

procedure TFRE_DB_DiskspaceTestcase.SetMountpoint(const mountpoint: string; const warning_percent: integer; const error_percent: integer);
begin
  config.Field('mountpoint').AsString      := mountpoint;
  config.Field('warning_percent').AsByte   := warning_percent;
  config.Field('error_percent').AsByte     := error_percent;
end;

procedure TFRE_DB_DiskspaceTestcase.ExecuteJob;
var
  test  : TFRE_DB_Testmethod_Diskspace;
  proc  : TFRE_DB_Process;
  res   : integer;
  error : string;
  diskspaceobject : IFRE_DB_OBject;
  total      : UInt64;
  percent    : double;
  percent_b  : byte;
begin
  test          := TFRE_DB_Testmethod_Diskspace.create;
  test.SetRemoteSSH (Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);
  try
    res      := test.Diskspace(config.Field('mountpoint').AsString,error,diskspaceobject);
    if res<>0 then begin
      SetStatus(statusFailure,'FAILURE ON GETTING MOUNTPOINT SPACE '+config.Field('mountpoint').AsString);
      report.Field('STATUSDETAIL').asstring := error;
    end else begin
      total     := diskspaceobject.Field('avail').AsUInt64+diskspaceobject.Field('used').AsUInt64;
      percent   := (diskspaceobject.Field('used').AsUInt64 / total);
      percent_b := round(percent*100);
//      writeln(percent_b);
      if percent_b>config.Field('error_percent').AsByte then begin
        SetStatus(statusFailure,'DISKSPACE LIMIT '+inttostr(config.Field('error_percent').AsByte)+'% EXCEEDED :'+inttostr(percent_b)+'%');
      end else if percent_b>config.Field('warning_percent').AsByte then begin
        SetStatus(statusWarning,'DISKSPACE LIMIT '+inttostr(config.Field('warning_percent').AsByte)+'% WARNING :'+inttostr(percent_b)+'%');
      end else begin
        SetStatus(statusOK,'DISKSPACE LIMIT OK :'+inttostr(percent_b)+'%');
      end;
      report.Field('STATUSDETAIL').asstring := 'TOTAL:'+inttostr((total div (1024*1024*1024)))+'GB AVAIL:'+inttostr((diskspaceobject.Field('avail').AsUInt64 div (1024*1024*1024)))+'GB USED:'+inttostr((diskspaceobject.Field('used').AsUInt64 div (1024*1024*1024)))+'GB';
    end;
    report.Field('information').AsString := config.Field('information').asstring;
//    writeln(report.DumpToString());
  except on E: Exception do begin
    SetStatus(StatusFailure,'Exception:'+E.Message);
  end; end;
end;

{ TFRE_DB_ProcessTestcase }

procedure TFRE_DB_ProcessTestcase.InternalSetup;
begin
  inherited InternalSetup;
end;

class procedure TFRE_DB_ProcessTestcase.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  (TFRE_DB_JOB.ClassName);
end;

procedure TFRE_DB_ProcessTestcase.AddReport(const proc: TFRE_DB_Process; const warning_count: integer; const error_count: integer; const information: string);
var detailreport  : IFRE_DB_Object;
    detailsummary : string;
    instances     : UInt32;
begin
  detailreport := GFRE_DBI.NewObject;
  detailreport.CopyField(proc,'exitstatus');
  detailreport.CopyField(proc,'processname');
  detailreport.Field('information').AsString := information;
  instances     := proc.Field('instances').ValueCount;
  detailreport.Field('instances').AsUInt32   := instances;
  detailsummary := Format('Processname:%0:-15s Running: %2.d', [proc.Field('processname').AsString,proc.Field('instances').ValueCount]);
  detailreport.Field('detailsummary').AsString := detailsummary;
  if instances<error_count then begin
    SetDetailStatus(detailreport,StatusFailure,'TOO LESS RUNNING PROCESSESS');
  end else if instances<warning_count then begin
    SetDetailStatus(detailreport,StatusWarning,'LOW NUMBER OF RUNNING PROCESSES');
  end else begin
    SetDetailStatus(detailreport,StatusOK,'PROCESS OK');
  end;
  report.field('processname').AddObject(detailreport);
  report.Field('alertmessage').AddString(Format('%0:-10s %15s %s',[detailreport.Field('status').asstring,information,detailsummary]));
end;

function TFRE_DB_ProcessTestcase.GetAllProcessnames: TFRE_DB_StringArray;
var
  i         : integer;
begin
  SetLength(result, Config.Field('processname').ValueCount);
  for i:= 0 to Config.Field('processname').ValueCount-1 do begin
    result [i] := Config.Field('processname').AsStringItem[i];
  end;
end;

procedure TFRE_DB_ProcessTestcase.SetInformation(const information: string);
begin

end;

procedure TFRE_DB_ProcessTestcase.AddProcessTest(const processname: string; const warning_count: integer; const error_count: integer; const information: string);
begin
  config.Field('processname').AddString(processname);
  config.Field('warning_count').AddUInt32(warning_count);
  config.Field('error_count').AddUInt32(error_count);
  config.Field('information_process').AddString(information);
end;

procedure TFRE_DB_ProcessTestcase.ExecuteJob;
var
   proctest  : TFRE_DB_Testmethod_Process;
   i         : integer;
   proc      : TFRE_DB_Process;
begin
  proctest          := TFRE_DB_Testmethod_Process.create;
  proctest.SetRemoteSSH (Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);
  try
    proctest.CheckProcess(GetAllProcessnames);
    report.Field('alertmessage').addstring(config.Field('information').asstring);
     for i:= 0 to proctest.ProcessCount-1 do begin
       AddReport (proctest.GetProcess(i),config.Field('warning_count').AsUInt32Item[i],config.Field('error_count').AsUInt32Item[i],config.Field('information_process').AsStringItem[i]);
     end;
     report.Field('information').AsString := config.Field('information').asstring;
//     writeln(report.DumpToString());
   except on E: Exception do begin
     SetStatus(StatusFailure,'Exception:'+E.Message);
   end; end;
end;

{ TFRE_DB_MultiPingTestcase }

procedure TFRE_DB_MultiPingTestcase.InternalSetup;
begin
  inherited InternalSetup;
  Field('MAX_ALLOWED_TIME').AsInt32 := 50;
end;

class procedure TFRE_DB_MultiPingTestcase.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  (TFRE_DB_JOB.ClassName);
end;

procedure TFRE_DB_MultiPingTestcase.AddReport(const proc: TFRE_DB_Process; const rtt: Single; const information: string);
var detailreport  : IFRE_DB_Object;
    detailsummary : string;
begin
  detailreport := GFRE_DBI.NewObject;
  detailreport.CopyField(proc,'rtt_avg');
  detailreport.CopyField(proc,'rtt_max');
  detailreport.CopyField(proc,'rtt_min');
  detailreport.CopyField(proc,'rtt_stddev');
  detailreport.CopyField(proc,'received');
  detailreport.CopyField(proc,'transmitted');
  detailreport.CopyField(proc,'percentlost');
  detailreport.CopyField(proc,'exitstatus');
  detailreport.CopyField(proc,'host');
  detailreport.Field('rtt_limit').AsReal32   := rtt;
  detailreport.Field('information').AsString := information;
  detailsummary := Format('IP:%0:-20s Transmitted: %5.d Received: %5.d LostPercentage: %5.2f', [proc.Field('host').AsString,proc.Field('transmitted').AsInt32,proc.Field('received').AsInt32,proc.Field('percentlost').AsReal32]);
  detailsummary := detailsummary + ' '+ Format(' RTT [ms] min/avg/max/stddev %6.3f/%6.3f/%6.3f/%6.3f', [proc.Field('rtt_min').AsReal32,proc.Field('rtt_avg').AsReal32,proc.Field('rtt_max').AsReal32, proc.Field('rtt_stddev').AsReal32]);
  detailreport.Field('detailsummary').AsString := detailsummary;
  if (detailreport.Field('RECEIVED').Asint32>0) then begin
    if (detailreport.Field('RECEIVED').Asint32=detailreport.Field('TRANSMITTED').asint32) then begin
      if (detailreport.Field('rtt_avg').asreal32<rtt) then begin
        SetDetailStatus(detailreport,statusOK,'PING OK');
      end else begin
        SetDetailStatus(detailreport,StatusWarning,'PING RTT TIME LIMIT EXCEEDED');
      end;
    end else begin
      SetDetailStatus(detailreport,StatusWarning,'PING PACKETLOSS');
    end;
  end else begin
    SetDetailStatus(detailreport,StatusFailure,'HOST NOT PINGABLE');
  end;
  report.field('hosts').AddObject(detailreport);
  report.Field('alertmessage').AddString(Format('%0:-10s %15s %s',[detailreport.Field('status').asstring,information,detailsummary]));
end;

function TFRE_DB_MultiPingTestcase.GetAllHosts: TFRE_DB_StringArray;
var
  i         : integer;
begin
  SetLength(result, Config.Field('ping').ValueCount);
  for i:= 0 to Config.Field('ping').ValueCount-1 do begin
    result [i] := Config.Field('ping').AsStringItem[i];
  end;
end;

procedure TFRE_DB_MultiPingTestcase.SetInformation(const information: string);
begin
  config.Field('information').asstring := information;
end;

procedure TFRE_DB_MultiPingTestcase.SetRTTTimeout_ms(const rtt_timeout: integer);
begin
 config.Field('rtt_timeout').AsInt32  := rtt_timeout;
end;

procedure TFRE_DB_MultiPingTestcase.SetPingCount(const count: integer);
begin
 config.Field('pingcount').AsInt32    := count;
end;

procedure TFRE_DB_MultiPingTestcase.AddPingTest(const host: string; const information: string);
begin
  config.Field('ping').AddString(host);
  config.Field('information_hosts').AddString(information);
end;

procedure TFRE_DB_MultiPingTestcase.ExecuteJob;
var
  ping      : TFRE_DB_Testmethod_Ping;
  i         : integer;
  proc      : TFRE_DB_Process;
begin
  if not config.FieldExists('pingcount') then begin
    config.Field('pingcount').AsInt32 := 10;
  end;
  if not config.FieldExists('rtt_timeout') then begin
    config.Field('rtt_timeout').AsInt32 := 10;
  end;
  ping      := TFRE_DB_Testmethod_ping.create;
  ping.SetRemoteSSH (Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);
  try
    ping.Ping(GetAllHosts, config.Field('pingcount').AsInt32);
    report.Field('alertmessage').addstring(config.Field('information').asstring);
    for i:= 0 to ping.ProcessCount-1 do begin
      AddReport (ping.GetProcess(i),config.Field('rtt_timeout').AsInt32,config.Field('information_hosts').AsStringItem[i]);
    end;
    report.Field('information').AsString := config.Field('information').asstring;
//    writeln(report.DumpToString());
  except on E: Exception do begin
    SetStatus(StatusFailure,'Exception:'+E.Message);
  end; end;
end;

{ TFRE_DB_SMBTestcase }

class procedure TFRE_DB_SMBTestcase.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  (TFRE_DB_JOB.ClassName);
end;

procedure TFRE_DB_SMBTestcase.SetFileshare(const host: string; const user: string; const password: string; const fileshare: string; const mountpoint: string);
begin
  config.Field('host').AsString            := host;
  config.Field('user').AsString            := user;
  config.Field('password').AsString        := password;
  config.Field('fileshare').AsString       := fileshare;
  config.Field('mountpoint').AsString      := mountpoint;
end;

procedure TFRE_DB_SMBTestcase.ExecuteJob;
var
  smb           : TFRE_DB_Testmethod_SMB;
  res           : integer;
  error         : string;
begin
   smb         := TFRE_DB_Testmethod_SMB.create;
   smb.SetRemoteSSH(Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);
   smb.MountSMBandTest(Config.Field('host').asstring, Config.Field('user').asstring, Config.Field('password').asstring, Config.Field('fileshare').asstring,Config.Field('mountpoint').asstring,error);
   if res=0 then begin
     SetStatus(statusOK,'FILESHARE MOUNT AND WRITE OK');
   end else begin
     report.Field('STATUSDETAIL').asstring := error;
     SetStatus(statusFailure,'FILESHARE ERROR');
   end;
end;

{ TFRE_DB_TestcaseStatus }

class procedure TFRE_DB_TestcaseStatus.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var
  enum: IFRE_DB_Enum;
begin
  inherited RegisterSystemScheme(scheme);

  enum:=GFRE_DBI.NewEnum('tcs_signal_status').Setup(GFRE_DBI.CreateText('$enum_tcs_signal_status','signal status Enum'));
  enum.addEntry('ok',GetTranslateableTextKey('enum_tcs_signal_status_ok'));
  enum.addEntry('warning',GetTranslateableTextKey('enum_tcs_signal_status_warning'));
  enum.addEntry('failure',GetTranslateableTextKey('enum_tcs_signal_status_failure'));
  enum.addEntry('unknown',GetTranslateableTextKey('enum_tcs_signal_status_unknown'));
  GFRE_DBI.RegisterSysEnum(enum);

  scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.ClassName);
  scheme.AddSchemeField('status',fdbft_String).SetupFieldDef(true,false,'tcs_signal_status');
  scheme.AddSchemeField('statusupdatetime',fdbft_DateTimeUTC);
  scheme.AddSchemeField('statussummary',fdbft_String);
  scheme.AddSchemeField('testcase',fdbft_Objlink).required:=true;
  scheme.AddSchemeField('actual',fdbft_Boolean);
  scheme.AddCalcSchemeField('status_icon',fdbft_String,@CALC_StatusIcon);
end;

class procedure TFRE_DB_TestcaseStatus.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
 newVersionId:='1.0';

 if (currentVersionId='') then begin
   currentVersionId:='1.0';
   StoreTranslateableText(conn,'enum_tcs_signal_status_ok','Ok');
   StoreTranslateableText(conn,'enum_tcs_signal_status_warning','Warning');
   StoreTranslateableText(conn,'enum_tcs_signal_status_failure','Failure');
   StoreTranslateableText(conn,'enum_tcs_signal_status_unknown','Unknown');
 end;
 if (currentVersionId='1.0') then begin
 //next update code
 end;

  
end;

function TFRE_DB_TestcaseStatus.IMI_ClearStatus(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
 Field('status').AsString          := 'UNKNOWN';
 Field('actual').AsBoolean         := true;
 Field('statusupdatetime').AsDateTimeUTC := GFRE_DT.Now_UTC;
 Field('statussummary').Clear;
end;

function TFRE_DB_TestcaseStatus.WEB_UpdateActualStatus(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var curr_status : string;
begin
// writeln ('UPDATE NOW');

 input.Field('jobkey').Clear;
 Field('actual_result').AsObject  := Input.CloneToNewObject;

 curr_status                      := Field('status').AsString;
// writeln ('CURR:',curr_status);
 if curr_status<>CFRE_SignalStatus[statusFailure] then begin      // status locked on Failure
   Field('statusupdatetime').AsDateTimeUTC := GFRE_DT.Now_UTC;
   Field('status').AsString                := input.Field('status').asstring;
   Field('statussummary').AsString         := input.Field('statussummary').asstring;
 end;
// writeln(DumpToString);
  CheckDbResult(conn.Update(self),'failure on cloned/update');
end;

function TFRE_DB_TestcaseStatus.IMI_CheckActuality(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  periodic    : TFRE_TestPeriodic;
  validtime   : TFRE_DB_DateTime64;
  curr_status : string;

begin
 curr_status                      := Field('status').AsString;
 if curr_status=CFRE_SignalStatus[statusOK] then begin
   writeln('check periodic');
   periodic := TFRE_TestPeriodic(Field('periodic_ord').asint16);
   case periodic of
    everyMinute : validtime := Field('statusupdatetime').AsDateTimeUTC+2*cFRE_DBT_1_MIN;
    everyHour   : validtime := Field('statusupdatetime').AsDateTimeUTC+2*cFRE_DBT_1_HOUR;
    everyDay    : validtime := Field('statusupdatetime').AsDateTimeUTC+2*cFRE_DBT_1_DAY;
   else
     writeln('UNKNOWN PERIODIC IN CHECK ACTUALITY!');
   end;
   if validtime<GFRE_DT.Now_UTC then begin
     Field('status').AsString          := 'UNKNOWN';
   end;
 end;
end;

procedure TFRE_DB_TestcaseStatus.CALC_StatusIcon(const setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString(GetStatusIconURI(Field('status').asstring));
end;

{ TFRE_DB_MailCheckTestcase }

class procedure TFRE_DB_MailCheckTestcase.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  (TFRE_DB_JOB.ClassName);
end;

procedure TFRE_DB_MailCheckTestcase.Analyze(const testmethod: IFRE_DB_Object);
var imails : integer;
    mail   : IFRE_DB_Object;
    nowt   : TFRE_DB_DateTime64;
    mailt  : int64;
    warning_seconds : integer;
    error_seconds   : integer;
    diffs           : integer;
    mindiffs        : integer;
begin
  writeln('CHECK MAIL ANALYZE');
  nowt       := GFRE_DT.Now_UTC;
  warning_seconds  := config.Field('mailserver').asobject.Field('warning_seconds').asint32;
  error_seconds    := config.Field('mailserver').asobject.Field('error_seconds').asint32;
  mindiffs   := MaxInt;
//  writeln(testmethod.Field('result').AsObject.DumpToString());
  for imails := 0 to testmethod.Field('result').ValueCount-1 do begin
    mail     := testmethod.Field('result').AsObjectItem[imails];
    mailt    := mail.Field('ts').AsDateTimeUTC;
//    writeln ('MAILT:',mailt);
//    writeln ('NEWT:',nowt);
    diffs    := (nowt - mailt) div 1000;
    writeln ('MAIL DIVS ',diffs);
    if diffs<mindiffs then mindiffs := diffs;
  end;

 if mindiffs < warning_seconds then begin
   SetStatus(statusOK,'MAIL CHECK OK SECONDS:'+inttostr(mindiffs));
 end else begin
   if mindiffs < error_seconds then begin
     SetStatus(statusWarning,'MAIL CHECK SLOW SECONDS:'+inttostr(mindiffs));
   end else begin
     if mindiffs < Maxint then begin
       SetStatus(statusFailure,'MAIL CHECK FAILED SECONDS:'+inttostr(mindiffs));
     end else begin
       SetStatus(statusFailure,'MAIL CHECK FAILED, NO INCOMING TESTMAIL');
     end;
   end;
 end;

end;

procedure TFRE_DB_MailCheckTestcase.SetMailserver(const host: string; const sendjobkey: string; const warning_seconds: integer; const error_seconds: integer);
var mailserver   : IFRE_DB_Object;
begin
 mailserver                                   := GFRE_DBI.NewObject;
 mailserver.Field('host').AsString            := host;
 mailserver.Field('jobkey').AsString          := sendjobkey;
 mailserver.Field('warning_seconds').AsInt32  := warning_seconds;
 mailserver.Field('error_seconds').AsInt32    := error_seconds;
 config.Field('mailserver').asObject      := mailserver;
end;

procedure TFRE_DB_MailCheckTestcase.ExecuteJob;
var
  fm           : TFRE_DB_Testmethod_FetchMailDir;
  ms           : IFRE_DB_Object;
begin
  fm          := TFRE_DB_Testmethod_FetchMailDir.create;
  fm.SetRemoteSSH (Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);
  ms          := config.Field('mailserver').AsObject;
  fm.FetchMailDirectory(ms.Field('host').AsString,ms.Field('jobkey').AsString);
  Analyze(fm);
end;

{ TFRE_DB_MailSend }

class procedure TFRE_DB_MailSendTestcase.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  (TFRE_DB_JOB.ClassName);
end;

procedure TFRE_DB_MailSendTestcase.Analyze(const proc: TFRE_DB_Process);
var detail : IFRE_DB_Object;
begin
  detail   := GFRE_DBI.NewObject;
  detail.CopyField(proc,'host');
  detail.CopyField(proc,'exitstatus');
  if proc.ExitStatus=0 then begin
    SetDetailStatus(detail,statusOK,'MAIL SEND OK');
  end else begin
    SetDetailStatus(detail,statusFailure,'MAIL SEND FAILED');
    detail.Field('OUTPUT').AsString := proc.OutputToString;
    detail.Field('ERROR').AsString  := proc.ErrorToString;
  end;
  Field('RESULT').AddObject(detail);
end;

procedure TFRE_DB_MailSendTestcase.AddMailserver(const host : string; const user : string; const password: string; const mailfrom:string; const mailto:string);
var mailserver   : IFRE_DB_Object;
begin
 mailserver                               := GFRE_DBI.NewObject;
 mailserver.Field('host').AsString        := host;
 mailserver.Field('user').AsString        := user;
 mailserver.Field('password').AsString    := password;
 mailserver.Field('from').AsString        := mailfrom;
 mailserver.Field('to').AsString          := mailto;
 mailserver.Field('jobkey').AsString      := jobkey;
 config.Field('mailserver').AddObject(mailserver);
end;

procedure TFRE_DB_MailSendTestcase.ExecuteJob;
var
  sm           : TFRE_DB_Testmethod_SMTPSend;
  ms           : IFRE_DB_Object;
  itarget      : integer;
  subject      : string;

begin
  sm          := TFRE_DB_Testmethod_SMTPSend.create;
  sm.SetRemoteSSH (Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);
  for itarget := 0 to config.Field('mailserver').ValueCount-1 do begin
    ms        := config.Field('mailserver').AsObjectItem[itarget];
    subject   := GFRE_BT.HashString_MD5_HEX(uppercase(ms.Field('host').AsString)+' '+uppercase(ms.Field('jobkey').AsString))+' '+IntToStr(GFRE_DT.Now_UTC);
    sm.AddSMTPSend(ms.Field('host').AsString,ms.Field('user').AsString,ms.Field('password').asstring,ms.Field('from').asstring,ms.Field('to').asstring,subject,'');
  end;
  sm.Send;
  for itarget := 0 to sm.ProcessCount-1 do begin
    Analyze (sm.GetProcess(itarget));
  end;
//  writeln(sm.DumpToString);
end;

{ TFRE_DB_WinRMTarget }

class procedure TFRE_DB_WinRMTarget.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

procedure TFRE_DB_WinRMTarget.AnalyzeDisks(const resultdbo: IFRE_DB_Object);
var
  disk          : IFRE_DB_Object;
  idisk         : integer;
  diskserial    : string;
  ientry        : integer;
  entry         : IFRE_DB_Object;
  status        : TFRE_SignalStatus;
  totalsize     : int64;
  freespace     : int64;
  used_percent  : Single;
  statussummary : string;

begin
  for idisk := 0 to Field('disks').ValueCount-1 do begin
    disk          := Field('disks').AsObjectItem[idisk];
    diskserial    := disk.Field('volume_serial_number').AsString;
    status        := statusFailure;
    statussummary := 'DISK NOT FOUND';
    writeln('DISK ',diskserial);
    for ientry := 0 to resultdbo.Field(cWinRMDiskKey).ValueCount-1 do begin
      entry    := resultdbo.Field(cWinRMDiskKey).AsObjectItem[ientry];
      writeln('ENTRY ');
      if entry.Field('volume_serial_number').AsString=diskserial then begin
        writeln('ENTRY FOUND');
        if entry.FieldExists('size') then begin
          totalsize                   := StrToInt64(entry.Field('size').AsString);
          disk.Field('size').AsUInt64 := totalsize;
        end else begin
          totalsize   :=  0;
        end;
        if entry.FieldExists('free_space') then begin
          freespace                         := StrToInt64(entry.Field('free_space').AsString);
          disk.Field('free_space').AsUInt64 := freespace;
        end else begin
          freespace   :=  0;
        end;
        if ((totalsize>0) and (freespace>0)) then begin
          used_percent      :=  ((totalsize - freespace) / totalsize)*100;
          disk.Field('diskused_percent').AsReal32:=used_percent;
          if used_percent < disk.Field('errorlevel_percent').AsByte then begin
            if used_percent < disk.Field('warnlevel_percent').AsByte then begin
              status        := statusOK;
              statussummary := 'DISK CAPACITY OK';
            end else begin
              status        := StatusWarning;
              statussummary := 'DISK CAPACITY WARNING';
            end;
          end else begin
            status        := StatusFailure;
            statussummary := 'DISK CAPACITY EXCEEDED';
          end;
        end else begin
          status        := StatusFailure;
          statussummary := 'NO DISKSPACE INFORMATION';
        end;
        break;
      end;
    end;
    //SetDetailStatus(disk,status,statussummary);
  end;
end;

procedure TFRE_DB_WinRMTarget.AnalyzeServices(const resultdbo: IFRE_DB_Object);
var
  service       : IFRE_DB_Object;
  iservice      : integer;
  servicename   : string;
  ientry        : integer;
  entry         : IFRE_DB_Object;
  status        : TFRE_SignalStatus;
  statussummary : string;
begin
  for iservice := 0 to Field('services').ValueCount-1 do begin
    service       := Field('services').AsObjectItem[iservice];
    servicename   := uppercase(service.Field('servicename').AsString);
    writeln ('SERVICENAME:',servicename);
    status        := StatusFailure;
    statussummary := 'SERVICE NOT FOUND';
    for ientry := 0 to resultdbo.Field(cWinRMServiceKey).ValueCount-1 do begin
      entry    := resultdbo.Field(cWinRMServiceKey).AsObjectItem[ientry];
      if uppercase(entry.Field('name').AsString)=servicename then begin
        writeln('ENTRY FOUND');
        service.CopyField(entry,'state');
        service.CopyField(entry,'started');
        service.CopyField(entry,'caption');
        if Uppercase(entry.Field('state').AsString)=cWinRMStateRunning then begin
          status        := StatusOK;
          statussummary := 'SERVICE RUNNING';
        end else begin
          if Uppercase(entry.Field('start_mode').AsString)=cWinRMSMDisabled then begin
            status        := StatusWarning;
            statussummary := 'SERVICE DISABLED';
          end else begin
            status        := StatusFailure;
            statussummary := 'SERVICE NO RUNNING';
          end;
        end;
      end;
    end;
    //SetDetailStatus(service,status,statussummary);
  end;
end;

procedure TFRE_DB_WinRMTarget.AnalyzeCPUUsage(const resultdbo: IFRE_DB_Object);
var
  cpu       : IFRE_DB_Object;
  entry         : IFRE_DB_Object;
  status        : TFRE_SignalStatus;
  statussummary : string;
  cpuused       : byte;
begin
  cpu           := Field('cpu').AsObject;
  status        := StatusFailure;
  statussummary := 'NO CPU USAGE INFORMATION';
  if resultdbo.Field(cWinRMIdleKey).ValueCount=1 then begin
    entry    := resultdbo.Field(cWinRMIdleKey).AsObject;
    writeln('ENTRY FOUND');
    cpu.CopyField(entry,'PERCENT_PROCESSOR_TIME');
    if entry.FieldExists('PERCENT_PROCESSOR_TIME') then begin
      cpuused                        := 100-(Strtoint(entry.Field('PERCENT_PROCESSOR_TIME').asstring));
      cpu.Field('cpu_used').Asbyte   := cpuused;
      writeln (cpuused);
      if cpuused < cpu.Field('warnlevel_percent').AsByte then begin
        status        := StatusOK;
        statussummary := 'CPU USAGE OK';
      end else begin
        status        := StatusWarning;
        statussummary := 'CPU USAGE HIGH';
      end;
    end else begin
      status        := StatusFailure;
      statussummary := 'NO PROCESSOR TIME INFORMATION';
    end;
  end;
  //SetDetailStatus(cpu,status,statussummary);
end;

//procedure TFRE_DB_WinRMTarget.SetStatus(const status: TFRE_SignalStatus; const statussummary: string);
//begin
//  if FieldExists('status_ord') then begin
//   if Ord(status) > Field('status_ord').AsInt16 then begin
//    Field('status_ord').AsInt16     := Ord(status);
//    Field('status').AsString        := CFRE_SignalStatus [status];
//    Field('statussummary').AsString := statussummary;
//   end;
//  end else begin
//    Field('status_ord').AsInt16     := Ord(status);
//    Field('status').AsString        := CFRE_SignalStatus [status];
//    Field('statussummary').AsString := statussummary;
//  end;
//  if Assigned(parent.Parent) then begin
//    TFRE_DB_JOB(parent.parent.Implementor_HC).SetStatus(status,statussummary);
//  end;
//end;

//procedure TFRE_DB_WinRMTarget.SetDetailStatus(const detail: IFRE_DB_Object; const status: TFRE_SignalStatus; const statussummary: string);
//begin
//  detail.Field('status_ord').AsInt16     := Ord(status);
//  detail.Field('status').AsString        := CFRE_SignalStatus [status];
//  detail.Field('statussummary').AsString := statussummary;
//  SetStatus(status,statussummary);
//end;


procedure TFRE_DB_WinRMTarget.AddTestDiskSpace(const diskserial: string; const warnlevel_percent: byte; const errorlevel_percent: byte);
var
  disk        : IFRE_DB_Object;
begin
  disk         := GFRE_DBI.NewObject;
  disk.Field('volume_serial_number').AsString  := diskserial;
  disk.Field('warnlevel_percent').AsByte       := warnlevel_percent;
  disk.Field('errorlevel_percent').AsByte      := errorlevel_percent;
  if errorlevel_percent < warnlevel_percent then begin
    disk.Field('warnlevel_percent').AsByte    := errorlevel_percent;
  end;
  Field('disks').addObject(disk);
end;

procedure TFRE_DB_WinRMTarget.AddTestServiceRunning(const servicename: string);
var
  service     : IFRE_DB_Object;
begin
  service     := GFRE_DBI.NewObject;
  service.Field('servicename').AsString  := servicename;
  Field('services').addObject(service);
end;

procedure TFRE_DB_WinRMTarget.SetTestCPUUsage(const warnlevel_percent: byte);
var
  cpu        : IFRE_DB_Object;
begin
  cpu         := GFRE_DBI.NewObject;
  cpu.Field('warnlevel_percent').AsByte       := warnlevel_percent;
  Field('cpu').asObject:=cpu;
end;

procedure TFRE_DB_WinRMTarget.Analyze(const resultdbo: IFRE_DB_Object; const report: IFRE_DB_Object);

begin
  writeln ('ANALYZE TARGET');
  AnalyzeDisks(resultdbo);
  AnalyzeServices(resultdbo);
  AnalyzeCPUUsage(resultdbo);
  report.Field('target').AddObject(self.CloneToNewObject);
end;

{ TFRE_DB_WinRMTestcase }


class procedure TFRE_DB_WinRMTestcase.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  (TFRE_DB_JOB.ClassName);
end;

procedure TFRE_DB_WinRMTestcase.InternalSetup;
begin
  inherited InternalSetup;
  Field('MAX_ALLOWED_TIME').AsInt32 := 300;
end;

function TFRE_DB_WinRMTestcase.AddWinRMTarget(const url: string; const user: string; const password: string): TFRE_DB_WinRMTarget;
begin
  result                                   := TFRE_DB_WinRMTarget.Create;
  result.Field('targeturl').AsString       := url;
  result.Field('targetuser').AsString      := user;
  result.Field('targetpassword').AsString  := password;
  config.Field('target').AddObject(result);
end;

procedure TFRE_DB_WinRMTestcase.ExecuteJob;
var
  winrm        : TFRE_DB_Testmethod_WinRM;
  winrmtarget  : IFRE_DB_Object;
  itarget      : integer;
begin
  winrm       := TFRE_DB_Testmethod_WinRM.create;
  winrm.SetRemoteSSH (Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);
  for itarget := 0 to config.Field('target').ValueCount-1 do begin
    winrmtarget             := config.Field('target').AsObjectItem[itarget];
    winrm.AddWinRMTarget(winrmtarget.Field('targeturl').AsString,winrmtarget.Field('targetuser').AsString,winrmtarget.Field('targetpassword').asstring);
  end;
  winrm.GetWinRM;
  for itarget := 0 to config.Field('target').ValueCount-1 do begin
    winrmtarget             := config.Field('target').AsObjectItem[itarget];
    TFRE_DB_WinRMTarget(winrmtarget.Implementor_HC).Analyze(winrm.GetProcess(itarget).Field('RESULT').AsObject,report);
  end;
end;

{ TFRE_DB_InternetTestcase }

class procedure TFRE_DB_InternetTestcase.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  (TFRE_DB_JOB.ClassName);
end;

function TFRE_DB_InternetTestcase.GetAllHosts : TFRE_DB_StringArray;
var
  i         : integer;
begin
  SetLength(result, Config.Field('remotehosts').ValueCount+2);
  result [0] := Config.Field('localgateway').AsString;
  result [1] := Config.Field('providergateway').AsString;
  for i:= 0 to Config.Field('remotehosts').ValueCount-1 do begin
    result [i+2] := Config.Field('remotehosts').AsStringItem[i];
  end;
end;

procedure TFRE_DB_InternetTestcase.AddReport(const reportname: string; const proc: TFRE_DB_Process; const rtt: Single);
var detailreport  : IFRE_DB_Object;
    detailsummary : string;


begin
  detailreport := GFRE_DBI.NewObject;
  detailreport.CopyField(proc,'rtt_avg');
  detailreport.CopyField(proc,'rtt_max');
  detailreport.CopyField(proc,'rtt_min');
  detailreport.CopyField(proc,'rtt_stddev');
  detailreport.CopyField(proc,'received');
  detailreport.CopyField(proc,'transmitted');
  detailreport.CopyField(proc,'percentlost');
  detailreport.CopyField(proc,'exitstatus');
  detailreport.CopyField(proc,'host');
  detailreport.Field('rtt_limit').AsReal32 :=rtt;

  detailsummary := Format('Host:%0:-20s Transmitted: %5.d Received: %5.d LostPercentage: %5.2f', [proc.Field('host').AsString,proc.Field('transmitted').AsInt32,proc.Field('received').AsInt32,proc.Field('percentlost').AsReal32]);
  detailsummary := detailsummary + ' '+ Format(' RTT [ms] min/avg/max/stddev %6.3f/%6.3f/%6.3f/%6.3f', [proc.Field('rtt_min').AsReal32,proc.Field('rtt_avg').AsReal32,proc.Field('rtt_max').AsReal32, proc.Field('rtt_stddev').AsReal32]);
  detailreport.Field('detailsummary').AsString := detailsummary;
//  writeln (detailreport.Field('RECEIVED').Asint32);

  if (detailreport.Field('RECEIVED').Asint32>0) then begin
    if (detailreport.Field('RECEIVED').Asint32=detailreport.Field('TRANSMITTED').asint32) then begin
      if (detailreport.Field('rtt_avg').asreal32<rtt) then begin
        SetDetailStatus(detailreport,statusOK,'PING OK');
      end else begin
        SetDetailStatus(detailreport,StatusWarning,'PING RTT TIME LIMIT EXCEEDED');
      end;
    end else begin
      SetDetailStatus(detailreport,StatusWarning,'PING PACKETLOSS');
    end;
  end else begin
    SetDetailStatus(detailreport,StatusFailure,'HOST NOT PINGABLE');
  end;
  report.field(reportname).AddObject(detailreport);
end;


procedure TFRE_DB_InternetTestcase.PrepareTest(const localgateway: TFRE_DB_String; const providergateway: TFRE_DB_String; const remotehosts: TFRE_DB_StringArray);
begin
  config.Field('localgateway').AsString        := localgateway;
  config.Field('localrtt').AsReal32            := 10;
  config.Field('providergateway').AsString     := providergateway;
  config.Field('providerrtt').AsReal32         := 10;
  config.Field('remotehosts').AsStringArr      := remotehosts;
  config.Field('hostsrtt').AsReal32            := 10;
  config.Field('pingcount').Asint32            := 4;
end;

procedure TFRE_DB_InternetTestcase.ExecuteJob;
var
  ping      : TFRE_DB_Testmethod_Ping;
  i         : integer;
  proc      : TFRE_DB_Process;
  reportname: string;
  rtt       : single;
  fok       : boolean;

begin
  ping      := TFRE_DB_Testmethod_ping.create;
  ping.SetRemoteSSH (Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);

  try
    ping.Ping(GetAllHosts, config.Field('pingcount').AsInt32);
//    #(ping.DumpToString());
    for i:= 0 to ping.ProcessCount-1 do begin
      case i of
        0 : begin
              reportname := 'localgateway';
              rtt        := config.Field('localrtt').AsReal32;
            end;
        1 : begin
              reportname := 'providergateway';
              rtt        := config.Field('providerrtt').AsReal32;
            end;
      else
        reportname := 'remotehosts';
        rtt        := config.Field('hostsrtt').AsReal32;
      end;
      AddReport (reportname,ping.GetProcess(i),rtt);
    end;

    fok := false;
    // check if any remote hosts pingable
    for i:= 0 to report.Field('remotehosts').ValueCount-1 do begin
      if report.Field('remotehosts').AsObjectItem[i].Field('RECEIVED').Asint32>0 then begin
       fok := true;
      end;
    end;
    if fok=false then begin    // no remotehosts pingable
      SetStatus(StatusFailure,'NO REMOTE HOSTS PINGABLE');
      exit;
    end;

    fok  := false;
    if (report.Field('localgateway').AsObject.Field('STATUS').asstring='OK') then begin
      if (report.Field('providergateway').AsObject.Field('STATUS').asstring='OK')  then begin
        for i:= 0 to report.Field('remotehosts').ValueCount-1 do begin
          if (report.Field('remotehosts').AsObject.Field('STATUS').asstring='OK')  then begin
            fok := true;
            break;
          end;
        end;
      end;
    end;

    if fok then begin
      SetStatus(StatusOK,'INTERNET CONNECTION OK');
    end else begin
      SetStatus(StatusWarning,'INTERNET CONNECTION IN BAD CONDITION');
    end;
  except on E: Exception do begin
    SetStatus(StatusFailure,'Exception:'+E.Message);
  end; end;
end;



procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_JOB);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TIMERTEST_JOB);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_JobReport);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TestcaseStatus);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_InternetTestcase);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_WinRMTarget);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_WinRMTestcase);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MailSendTestcase);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MailCheckTestcase);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SMBTestcase);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MultiPingTestcase);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ProcessTestcase);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DiskspaceTestcase);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_HTTPTestcase);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CPULoadTestcase);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_JobProgress);
  //GFRE_DBI.Initialize_Extension_Objects;
end;

function GetStatusIconURI(const status: string): string;
var    lstatus_icon : TFRE_DB_String;
begin
  case status of
    'OK'      : lstatus_icon := FREDB_getThemedResource('images_apps/test/signal_ok.png');
    'WARNING' : lstatus_icon := FREDB_getThemedResource('images_apps/test/signal_warning.png');
    'FAILURE' : lstatus_icon := FREDB_getThemedResource('images_apps/test/signal_failure.png');
    'UNKNOWN' : lstatus_icon := FREDB_getThemedResource('images_apps/test/signal_unknown.png');
   else raise EFRE_DB_Exception.Create(edb_ERROR,'UNKNOWN ENUM FIELD VALUE SIGNAL Status');
  end;
  result := lstatus_icon;
end;

function GetSignalStatus(const status: string): TFRE_SignalStatus;
begin
  case status of
    'OK'      : result := statusOK;
    'WARNING' : result := statusWarning;
    'FAILURE' : result := statusFailure;
    'UNKNOWN' : result := statusUnknown;
  else raise EFRE_DB_Exception.Create(edb_ERROR,'UNKNOWN ENUM FIELD VALUE SIGNAL Status');
  end;
end;


initialization
end.

