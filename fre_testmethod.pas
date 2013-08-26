unit fre_testmethod;

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
  Classes, SysUtils,FRE_DB_INTERFACE, FRE_DB_COMMON, FRE_PROCESS,  FOS_BASIS_TOOLS,
  FOS_TOOL_INTERFACES;


const
  cWinRM         =  '/fre/monitoring/winrm.rb';
  cSMTPcli       =  '/fre/monitoring/smtp-cli';
  cSNMPget       =  'snmpget';
  cMailDirectory =  '/fre/monitoring/mails';

type

  { TFRE_DB_Testmethod }

  TFRE_DB_Testmethod = class (TFRE_DB_Multiprocess)
  protected
    class procedure RegisterSystemScheme  (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  end;

  { TFRE_DB_Testmethod_Ping }

  TFRE_DB_Testmethod_Ping =class (TFRE_DB_Testmethod)
  private
    procedure       AnalyzeResult         (const proc   : TFRE_DB_Process);                 // Solaris
  protected
    class procedure RegisterSystemScheme  (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    procedure       Ping                  (const hosts  : TFRE_DB_StringArray; const count : integer = 10);
  end;

  { TFRE_DB_Testmethod_VirtualMachines }

  TFRE_DB_Testmethod_VirtualMachines =class (TFRE_DB_Testmethod)
  private
    procedure       AnalyzeResult         (const proc   : TFRE_DB_Process);
  protected
    class procedure RegisterSystemScheme  (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    procedure       GetMachineStates      ;
  end;

  { TFRE_DB_Testmethod_Process }

  TFRE_DB_Testmethod_Process =class (TFRE_DB_Testmethod)
  private
    procedure       AnalyzeResult         (const proc   : TFRE_DB_Process);
  protected
    class procedure RegisterSystemScheme  (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    procedure       CheckProcess          (const processname : TFRE_DB_StringArray);
  end;

  { TFRE_DB_Testmethod_Diskspace }

  TFRE_DB_Testmethod_Diskspace =class (TFRE_DB_Testmethod)
  private
    procedure       AnalyzeResult         (const proc   : TFRE_DB_Process);
  public
    function        Diskspace             (const mountpoint : string; out error : string;out diskspaceo: IFRE_DB_Object) : integer;
  end;

  { TFRE_DB_Testmethod_HTTP }

  TFRE_DB_Testmethod_HTTP =class (TFRE_DB_Testmethod)
  public
    function        HTTP                  (const url : string; const header : string ; out error : string;out response:string) : integer;
  end;

  { TFRE_DB_Testmethod_CPULoad }

  TFRE_DB_Testmethod_CPULoad =class (TFRE_DB_Testmethod)
  private
    procedure       AnalyzeResult         (const proc   : TFRE_DB_Process);
  public
    function        CPULoad               (out error : string;out cpuloado: IFRE_DB_Object) : integer;
  end;

  { TFRE_DB_Testmethod_WinRM }

  TFRE_DB_Testmethod_WinRM =class (TFRE_DB_Testmethod)
  private
    procedure       AnalyzeResult         (const proc   : TFRE_DB_Process);
  protected
    class procedure RegisterSystemScheme  (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    procedure       AddWinRMTarget        (const url : string; const user : string; const password: string);
    procedure       GetWinRM;
  end;

  { TFRE_DB_Testmethod_SMTPSend }

  TFRE_DB_Testmethod_SMTPSend =class (TFRE_DB_Testmethod)
  private
    procedure       AnalyzeResult         (const proc   : TFRE_DB_Process);
  protected
    class procedure RegisterSystemScheme  (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    procedure       AddSMTPSend           (const host : string; const user : string; const password: string; const mailfrom:string; const mailto:string; const subject: string; const body:string);
    procedure       Send;
  end;

  { TFRE_DB_Testmethod_FetchMailDir }

  TFRE_DB_Testmethod_FetchMailDir =class (TFRE_DB_Testmethod)
  private
    procedure       AnalyzeResult         (const proc   : TFRE_DB_Process; const key: string);
  protected
    class procedure RegisterSystemScheme  (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    procedure       FetchMailDirectory    (const host : string; const jobkey : string);
  end;

  { TFRE_DB_Testmethod_SMB }

  TFRE_DB_Testmethod_SMB =class (TFRE_DB_Testmethod)
  protected
    class procedure RegisterSystemScheme  (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    function        MountSMB              (const host : string; const user : string; const password: string; const fileshare: string; const mountpoint: string; out error:string) : integer;
    function        UnmountSMB            (const mountpoint: string; out error:string) : integer;
    function        WriteTestFile         (const filename: string; out error:string) : integer;
    function        CheckTestFileExists   (const filename: string; out error:string) : integer;
    function        MountSMBandTest       (const host : string; const user : string; const password: string; const fileshare: string; const mountpoint: string; out error:string) : integer;
  end;

  { TFRE_DB_Testmethod_SNMP }

  TFRE_DB_Testmethod_SNMP =class (TFRE_DB_Testmethod)
  private
    procedure       AnalyzeResult         (const proc   : TFRE_DB_Process);
  protected
    class procedure RegisterSystemScheme  (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    procedure       AddSNMPRequest        (const oid : string; const host : string; const version: integer=1; const community: string='public');
    procedure       SendRequests;
  end;

implementation

{ TFRE_DB_Testmethod_CPULoad }

procedure TFRE_DB_Testmethod_CPULoad.AnalyzeResult(const proc: TFRE_DB_Process);
var   i               : integer;
      line            : string;
      min1,min5,min15 : string;
      cpuloado        : IFRE_DB_Object;
begin
  line :=TFRE_DB_Process (proc.Implementor_HC).OutputToString;
  line := Copy(line,Pos('average:',line)+9,maxint);
  Line := StringReplace(line,',',DecimalSeparator,[rfReplaceAll]);
  Line := StringReplace(line,'.',DecimalSeparator,[rfReplaceAll]);
  min1 := GFRE_BT.SplitString(line,' ');
  min1 := Copy(min1,1,Length(min1)-1);
  min5 := GFRE_BT.SplitString(line,' ');
  min5 := Copy(min5,1,Length(min5)-1);
  min15:= line;
  cpuloado := GFRE_DBI.NewObject;
  cpuloado.Field('load1').AsReal32  := StrToFloat(min1);
  cpuloado.Field('load5').AsReal32  := StrToFloat(min5);
  cpuloado.Field('load15').AsReal32 := StrToFloat(min15);
  proc.Field('cpuload').AsObject    := cpuloado;
end;

function TFRE_DB_Testmethod_CPULoad.CPULoad(out error: string; out cpuloado: IFRE_DB_Object): integer;
var proc  : TFRE_DB_Process;
    cmd   : string;
begin
    proc := TFRE_DB_Process.create;
    proc.SetupInput('uptime');
    AddProcess(proc);
    ExecuteMulti;
    AnalyzeResult(GetProcess(0));
    result        := proc.Field('exitstatus').AsInt32;
    error         := proc.Field('errorstring').AsString;
    cpuloado      := proc.Field('cpuload').AsObject;
end;

{ TFRE_DB_Testmethod_HTTP }

function TFRE_DB_Testmethod_HTTP.HTTP(const url: string; const header: string; out error: string; out response: string): integer;
var proc  : TFRE_DB_Process;
    cmd   : string;
begin
  proc := TFRE_DB_Process.create;
  //cmd  := 'wget ';
  //if header<>'' then cmd := cmd +'--header="'+header+'" ';
  //cmd  := cmd +'-O - '+url;
  //proc.SetupInput(cmd);
  proc.SetupInput('wget',TFRE_DB_StringArray.Create('-O','-','--header='+header,url));
  AddProcess(proc);
  ExecuteMulti;
  result        := proc.Field('exitstatus').AsInt32;
  error         := proc.Field('errorstring').AsString;
  response      := proc.OutputToString;
//  writeln(proc.DumpToString());
end;

{ TFRE_DB_Testmethod_Diskspace }

procedure TFRE_DB_Testmethod_Diskspace.AnalyzeResult(const proc: TFRE_DB_Process);
var   slist       : TStringList;
      llist       : TStringList;

  procedure ParseLine(line : string);
  var diskspace   : IFRE_DB_Object;
  begin
    llist.CommaText:= line;
    diskspace                        := GFRE_DBI.NewObject;
    diskspace.Field('used').AsUInt64 := StrToInt64(trim(llist[2]))*1024*1024;
    diskspace.Field('avail').AsUInt64:= StrToInt64(trim(llist[3]))*1024*1024;
    proc.Field('diskspace').asobject := diskspace;
  end;

begin
  slist := TStringList.Create;
  llist := TStringList.Create;
  try
     slist.text                          := TFRE_DB_Process (proc.Implementor_HC).OutputToString;
     ParseLine (slist[1]);
  finally
    llist.Free;
    slist.Free;
  end;
//  writeln(TFRE_DB_Process (proc.Implementor_HC).DumpToString());
end;


function TFRE_DB_Testmethod_Diskspace.Diskspace(const mountpoint: string; out error: string; out diskspaceo: IFRE_DB_Object): integer;
var iproc : integer;
    proc  : TFRE_DB_Process;
begin
  proc := TFRE_DB_Process.create;
  proc.SetupInput('df',TFRE_DB_StringArray.Create('-m',mountpoint));
  proc.Field('mountpoint').AsString    := mountpoint;
  AddProcess(proc);
  ExecuteMulti;
  for iproc := 0 to Field('process').ValueCount-1 do begin
    AnalyzeResult(GetProcess(iproc));
  end;
  result        := proc.Field('exitstatus').AsInt32;
  error         := proc.Field('errorstring').AsString;
  diskspaceo    := proc.Field('diskspace').asobject;
end;

{ TFRE_DB_Testmethod_Process }

procedure TFRE_DB_Testmethod_Process.AnalyzeResult(const proc: TFRE_DB_Process);
var   slist       : TStringList;
      i           : integer;
      instance    : IFRE_DB_Object;

  procedure ParseLine(line : string);
  begin
     instance       := GFRE_DBI.NewObject;
     line := trim(line);
     instance.Field('pid').asint32    := strtoint(GFRE_BT.SplitString(line,' '));
     if Pos(' ',line)>0 then begin
       instance.Field('cmd').asstring   := GFRE_BT.SplitString(line,' ');
       instance.Field('args').asstring  := line;
     end else begin
       instance.Field('cmd').asstring  := line;
     end;
     proc.Field('instances').AddObject(instance);
  end;

begin
  slist := TStringList.Create;
  try
     slist.text                          := TFRE_DB_Process (proc.Implementor_HC).OutputToString;
     for i := 0 to slist.Count -1 do begin
       ParseLine (slist[i]);
     end;
  finally
    slist.Free;
  end;
//  writeln(TFRE_DB_Process (proc.Implementor_HC).DumpToString());
end;

class procedure TFRE_DB_Testmethod_Process.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

procedure TFRE_DB_Testmethod_Process.CheckProcess(const processname: TFRE_DB_StringArray);
var iproc : integer;
    proc  : TFRE_DB_Process;
begin
  for iproc := low (processname) to high (processname) do begin
   writeln ('Processname:'+processname[iproc]);
   proc := TFRE_DB_Process.create;
   proc.SetupInput('pgrep',TFRE_DB_StringArray.Create('-l','-f',processname[iproc]));
   proc.Field('processname').AsString    := processname[iproc];
   AddProcess(proc);
  end;
  ExecuteMulti;
  for iproc := 0 to Field('process').ValueCount-1 do begin
   AnalyzeResult(GetProcess(iproc));
  end;
end;

{ TFRE_DB_Testmethod_SNMP }

procedure TFRE_DB_Testmethod_SNMP.AnalyzeResult(const proc: TFRE_DB_Process);
var s : string;
    t : string;
    v : string;
begin
  if proc.ExitStatus=0 then begin
    s := proc.OutputToString;
    s := GFRE_BT.SepRight(s,'=');
    t := trim(GFRE_BT.SepLeft(s,':'));
    v := trim(GFRE_BT.SepRight(s,':'));
    if t='STRING' then begin
      proc.Field('value').asstring := v;
    end else if t='INTEGER' then begin
      proc.Field('value').asint32 := strtoint(v);
    end else if t='OIS' then begin
      proc.Field('value').asstring := v;
    end else begin
      proc.Field('value').AsString := 'UNKNOWN TYPE '+t;
    end;
  end;
end;

class procedure TFRE_DB_Testmethod_SNMP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

procedure TFRE_DB_Testmethod_SNMP.AddSNMPRequest(const oid: string; const host: string; const version: integer; const community: string);
var
  proc  : TFRE_DB_Process;
//  snmpget -v1 -cpublic 10.4.0.234 HOST-RESOURCES-MIB::hrProcessorLoad.2
begin
  proc := TFRE_DB_Process.Create;
  proc.SetupInput(cSNMPget,TFRE_DB_StringArray.Create('-v'+inttostr(version),'-c'+community,host,oid));
  proc.Field('host').AsString   := host;
  proc.Field('oid').AsString    := oid;
  AddProcess(proc);
end;

procedure TFRE_DB_Testmethod_SNMP.SendRequests;
var
  iproc : integer;
begin
  ExecuteMulti;
  for iproc := 0 to ProcessCount-1 do begin
    AnalyzeResult(GetProcess(iproc));
  end;
end;


class procedure TFRE_DB_Testmethod_SMB.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

function TFRE_DB_Testmethod_SMB.MountSMB(const host: string; const user: string; const password: string; const fileshare: string; const mountpoint: string; out error: string): integer;
var
  proc  : TFRE_DB_Process;
  //    mount -F smbfs //schramml:xxxxx@10.1.0.132/Library /usr/local/testdata
begin
  ClearProcess;
  proc := TFRE_DB_Process.Create;
  proc.SetupInput('mount',TFRE_DB_StringArray.Create('-F','smbfs','//'+user+':'+password+'@'+host+'/'+fileshare,mountpoint));
  proc.Field('host').AsString:=host;
  AddProcess(proc);
  ExecuteMulti;
  result := proc.ExitStatus;
  error  := proc.ErrorToString;
end;

function TFRE_DB_Testmethod_SMB.UnmountSMB(const mountpoint: string; out error: string): integer;
var
  proc  : TFRE_DB_Process;
  //    mount -F smbfs //schramml:xxxxx@10.1.0.132/Library /usr/local/testdata
begin
  ClearProcess;
  proc := TFRE_DB_Process.Create;
  proc.SetupInput('umount',TFRE_DB_StringArray.Create(mountpoint));
  AddProcess(proc);
  ExecuteMulti;
  result := proc.ExitStatus;
  error  := proc.ErrorToString;
end;

function TFRE_DB_Testmethod_SMB.WriteTestFile(const filename: string; out error: string): integer;
var
  proc  : TFRE_DB_Process;
begin
  ClearProcess;
  proc := TFRE_DB_Process.Create;
  proc.SetupInput('echo "SMBTest" > '+filename,nil);
  AddProcess(proc);
  ExecuteMulti;

  result := proc.ExitStatus;
  error  := proc.ErrorToString;
end;

function TFRE_DB_Testmethod_SMB.CheckTestFileExists(const filename: string; out error: string): integer;
var
  proc  : TFRE_DB_Process;
begin
  ClearProcess;
  proc := TFRE_DB_Process.Create;
  proc.SetupInput('cat',TFRE_DB_StringArray.Create(filename));
  AddProcess(proc);
  ExecuteMulti;

  result := proc.ExitStatus;
  error  := proc.ErrorToString;
end;

function TFRE_DB_Testmethod_SMB.MountSMBandTest(const host: string; const user: string; const password: string; const fileshare: string; const mountpoint: string; out error: string): integer;
var
  testfilename : string;
begin
  result  := MountSMB(host, user, password, fileshare, mountpoint, error);
  writeln(GetProcess(0).DumpToString);
  if result <>0 then begin
    exit;
  end;

  testfilename := mountpoint + DirectorySeparator + 'smbtest.txt';

  result  := WriteTestFile(testfilename, error);
  writeln(GetProcess(0).DumpToString);
  if result <>0 then begin
    exit;
  end;

  result  := UnmountSMB(mountpoint,error);
  writeln(GetProcess(0).DumpToString);
  if result <>0 then begin
    exit;
  end;

  result := CheckTestFileExists(testfilename, error);
  if result=0 then begin      // file should not exist here
    result := -1;
    error  := 'Testfile still exists in filesystem after unmount !';
    exit;
  end;

  result := 0;
  error  := '';
end;

{ TFRE_DB_Testmethod_FetchMailDir }

procedure TFRE_DB_Testmethod_FetchMailDir.AnalyzeResult(const proc: TFRE_DB_Process; const key: string);
var
    mlist  : TStringList;
    i      : integer;
    line   : string;
    subject: string;
    mail   : IFRE_DB_OBject;
begin
  if proc.ExitStatus=0 then begin
    mlist  := TStringList.Create;
    try
      mlist.Text := proc.OutputToString;
      for i      := 0 to mlist.Count-1 do begin
        line      := mlist[i];
        if Pos('Subject:',line)=1 then begin
          subject := trim(GFRE_BT.SepRight(line,':'));
          if Pos(key,subject)>0 then begin
            writeln('Subject:',subject);
            mail := GFRE_DBI.NewObject;
            mail.Field('subject').asstring   := subject;
            try
              mail.Field('ts').AsDateTimeUTC   := StrToInt64(GFRE_BT.SepRight(subject,' '));
              //writeln('MAILTS ',subject, inttostr(mail.Field('ts').AsDateTimeUTC));
              mail.CopyField(proc,'filename');
              field('RESULT').AddObject(mail);
            except
              // Ignore E-Mail, delete it
            end;
            proc.Field('delete').AsBoolean:=true;
          end;
        end;
      end;
    finally
      mlist.Free;
    end;
  end;
end;

class procedure TFRE_DB_Testmethod_FetchMailDir.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

procedure TFRE_DB_Testmethod_FetchMailDir.FetchMailDirectory(const host: string; const jobkey: string);
var
  proc           : TFRE_DB_Process;
  mails          : TFOSStringArray;
  imails         : integer;
  filename       : string;
  iproc          : integer;
  md5hash        : string;
begin
  proc  := TFRE_DB_Process.Create;
  proc.SetupInput('ls',TFRE_DB_StringArray.Create('-m',cMailDirectory));
  AddProcess(proc);
  ExecuteMulti;
  if proc.ExitStatus= 0 then begin
    GFRE_BT.SeperateString(proc.OutputToString,',',mails);
  end;
//  writeln(proc.OutputToString);
  Field('process').Clear;
  writeln('list:');
  if length(mails)=0 then begin
    exit;
  end;
  for imails := low (mails) to high (mails) do begin
    filename := trim (mails[imails]);
 //   writeln(filename);
    proc     := TFRE_DB_Process.Create;
    proc.SetupInput('cat',TFRE_DB_StringArray.Create(cMailDirectory+DirectorySeparator+filename));
    proc.Field('filename').AsString := filename;
    AddProcess(proc);
    ExecuteMulti;
    md5hash   := GFRE_BT.HashString_MD5_HEX(uppercase(host)+' '+uppercase(jobkey));
 //   writeln(md5hash);
    AnalyzeResult(proc,md5hash);
    if proc.FieldExists('delete') then begin
      Field('process').Clear;
      proc     := TFRE_DB_Process.Create;
      proc.SetupInput('rm',TFRE_DB_StringArray.Create(cMailDirectory+DirectorySeparator+filename));
      AddProcess(proc);
      ExecuteMulti;
    end;
    Field('process').Clear;
  end;

  writeln(DumpToString);
end;

{ TFRE_DB_Testmethod_SMTPSend }

procedure TFRE_DB_Testmethod_SMTPSend.AnalyzeResult(const proc: TFRE_DB_Process);
begin
 proc.Field('OUTPUT').AsString:=proc.OutputToString;
end;

class procedure TFRE_DB_Testmethod_SMTPSend.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

procedure TFRE_DB_Testmethod_SMTPSend.AddSMTPSend(const host: string; const user: string; const password: string; const mailfrom: string; const mailto: string; const subject: string; const body: string);
var
  proc  : TFRE_DB_Process;
  //      ./smtp-cli --verbose --missing-modules-ok --server=10.4.0.234 --enable-auth --auth-login --user winrmi --password xxx --from tester@ksm.at --to franz.schober@firmos.at --subject "mailserver Test"
begin
  writeln('SUBJECT:',subject);
  proc := TFRE_DB_Process.Create;
  proc.SetupInput(cSMTPcli,TFRE_DB_StringArray.Create('--missing-modules-ok','--server='+host,'--enable-auth', '--auth-login', '--user',user,'--password',password,'--from',mailfrom,'--to',mailto,'--subject','"'+subject+'"')); //,'--body-plain="'+body+'"'));
  proc.Field('host').AsString:=host;
  writeln ('BODY:',body);
  AddProcess(proc);

end;

procedure TFRE_DB_Testmethod_SMTPSend.Send;
var
  iproc : integer;
begin
  ExecuteMulti;
  for iproc := 0 to ProcessCount-1 do begin
    AnalyzeResult(GetProcess(iproc));
  end;
end;

{ TFRE_DB_Testmethod_WinRM }

procedure TFRE_DB_Testmethod_WinRM.AnalyzeResult(const proc: TFRE_DB_Process);
var
  slist    : TStringList;
  elist    : TStringList;
  i        : integer;
  key      : string;
  keycount : integer;
  resdbo   : IFRE_DB_Object;


  procedure CreateEntry(line : string);
  var
    entries : TFOSStringArray;
    i       : integer;
    entry   : IFRE_DB_Object;
    name    : string;
    value   : string;
  begin
    GFRE_BT.SeperateString(line,',',entries);
    entry                       := GFRE_DBI.NewObject;
    for i:= low(entries) to high(entries) do begin
     name                       := GFRE_BT.SepLeft(entries[i],'=');
     value                      := GFRE_BT.SepRight(entries[i],'=');
     entry.Field(name).AsString := value;
    end;
    resdbo.Field(key).AddObject   (entry);
  end;

  procedure ParseLine(line : string);
  begin
   if key='' then begin
    if Pos('FOS',line)=1  then begin
      key                 := GFRE_BT.SplitString( line, ',');
      keycount            := strtoint (line);                                 //      FOS_RMI_WIN_IDLE_PROCESS,1
    end;
   end else begin
    if keycount>0         then begin
     CreateEntry (line);
     dec(keycount);
     if keycount=0        then begin
       key := '';
     end;
    end else begin
     key:='';
     keycount := 0;
    end;
   end;
  end;

begin
 slist := TStringList.Create;
 try
    slist.text                          := TFRE_DB_Process (proc.Implementor_HC).OutputToString;
    key                                 := '';
    keycount                            := 0;
    resdbo                              := GFRE_DBI.NewObject;
    proc.Field('RESULT').AddObject(resdbo);
    for i := 0 to slist.Count-1 do begin
      ParseLine(slist[i]);
    end;
 finally
   slist.Free;
 end;
 //writeln(proc.DumpToString());
end;

class procedure TFRE_DB_Testmethod_WinRM.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

procedure TFRE_DB_Testmethod_WinRM.AddWinRMTarget(const url: string; const user: string; const password: string);
var
  proc  : TFRE_DB_Process;
begin
  proc := TFRE_DB_Process.create;
  proc.SetupInput(cWinRM,TFRE_DB_StringArray.Create (url,user,password));
  AddProcess(proc);
end;

procedure TFRE_DB_Testmethod_WinRM.GetWinRM;
var
  iproc : integer;
begin
// writeln(DumpToString);
 ExecuteMulti;
 for iproc := 0 to ProcessCount-1 do begin
   AnalyzeResult(GetProcess(iproc));
 end;
end;



{ TFRE_DB_Testmethod_VirtualMachines }

procedure TFRE_DB_Testmethod_VirtualMachines.AnalyzeResult(const proc: TFRE_DB_Process);
var   slist       : TStringList;
      foundheader : boolean;
      i           : integer;
      machine     : IFRE_DB_Object;

  procedure ParseLine(line : string);
  begin
   if foundheader then begin
     writeln (line);
     machine       := GFRE_DBI.NewObject;
     machine.Field('uuid').asstring   := trim(copy(line, 1, 38));
     machine.Field('type').asstring   := trim(copy(line, 39, 6));
     machine.Field('ram').asint32     := strtoint(trim(copy(line, 45, 9)));
     machine.Field('state').asstring  := trim(copy(line, 54, 16));
     machine.Field('alias').asstring  := trim(copy(line, 72, maxint));
     proc.Field('vm').AddObject       (machine);
   end else begin
     if Pos('UUID',line) > 0 then begin
       foundheader   := true;
     end;
   end;
  end;

begin
  slist := TStringList.Create;
  try
     slist.text                          := TFRE_DB_Process (proc.Implementor_HC).OutputToString;
     foundheader := false;
     for i := 0 to slist.Count -1 do begin
       ParseLine (slist[i]);
     end;
  finally
    slist.Free;
  end;

end;

class procedure TFRE_DB_Testmethod_VirtualMachines.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

procedure TFRE_DB_Testmethod_VirtualMachines.GetMachineStates;
var proc  : TFRE_DB_Process;
begin
  proc := TFRE_DB_Process.Create;
  proc.SetupInput('vmadm',TFRE_DB_StringArray.Create('list'));
  AddProcess(proc);
  ExecuteMulti;
  AnalyzeResult(proc);
end;

class procedure TFRE_DB_Testmethod.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

{ TFRE_DB_Testmethod_Ping }


procedure TFRE_DB_Testmethod_Ping.AnalyzeResult(const proc: TFRE_DB_Process);
var slist   : TStringList;
    i       : integer;

  procedure ParseCountLine (line : string);
  var s: string;

  begin
    proc.Field('transmitted').AsInt32   := strtoint(trim(GFRE_BT.SplitString(line,'packets transmitted,')));
    proc.Field('received').AsInt32      := strtoint(trim(GFRE_BT.SplitString(line,'packets received,')));
    proc.Field('percentlost').AsReal32  := strtoFloat(trim(GFRE_BT.SplitString(line,'%')));
  end;

  procedure ParseRTTLine (line : string);
  begin
    // RTT:round-trip (ms)  min/avg/max/stddev = 25.406/25.442/25.479/0.052
//    writeln ('RTT:',line);
    line  := GFRE_BT.SepRight (line,'=');
    proc.Field('rtt_min').AsReal32     := strtoFloat(trim(GFRE_BT.SplitString(line,'/')));
    proc.Field('rtt_avg').AsReal32     := strtoFloat(trim(GFRE_BT.SplitString(line,'/')));
    proc.Field('rtt_max').AsReal32     := strtoFloat(trim(GFRE_BT.SplitString(line,'/')));
    line := trim(line);
    if Pos(' ',line)>0 then line := GFRE_BT.SepLeft(line,' ');
    proc.Field('rtt_stddev').AsReal32  := strtoFloat(trim(line));
  end;

  procedure ParseLine (line : string);
  begin
    if Pos ('PING',uppercase (line))=1 then begin
     // writeln ('HEADER:',line);
    end else if Pos ('BYTES FROM',uppercase (line))>0 then begin
     //  writeln ('PACKET:',line);
    end else if Pos ('TRANSMITTED',uppercase (line))>0 then begin
     ParseCountLine (line);
    end else if Pos ('ROUND-TRIP',uppercase (line))>0 then begin
     ParseRTTLine   (line);
    end else begin
     // writeln ('OTHER:',line);
    end;
  end;

begin
  slist := TStringList.Create;
  try
     slist.text                          := TFRE_DB_Process (proc.Implementor_HC).OutputToString;
     proc.Field('rtt_min').AsReal32     := 0;
     proc.Field('rtt_avg').AsReal32     := 0;
     proc.Field('rtt_max').AsReal32     := 0;
     proc.Field('rtt_stddev').AsReal32  := 0;
     proc.Field('transmitted').AsInt32   := 0;
     proc.Field('received').AsInt32      := 0;
     proc.Field('percentlost').AsReal32  := 100;
//     writeln (sstream.DataString);
     for i := 0 to slist.Count -1 do begin
       ParseLine (slist[i]);
     end;
//     writeln (proc.DumpToString());
  finally
    slist.Free;
  end;
end;

class procedure TFRE_DB_Testmethod_Ping.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

procedure TFRE_DB_Testmethod_Ping.Ping(const hosts: TFRE_DB_StringArray; const count: integer);
var iping : integer;
    proc  : TFRE_DB_Process;
begin
  for iping := low (hosts) to high (hosts) do begin
   writeln ('Ping:'+hosts[iping]);
   proc := TFRE_DB_Process.create;
   {$ifdef solaris}
    proc.SetupInput('ping',TFRE_DB_StringArray.Create('-n','-s',hosts[iping],'56',inttostr(count)));
   {$else}
    proc.SetupInput('ping',TFRE_DB_StringArray.Create('-c',inttostr(count),hosts[iping]));
   {$endif}
   proc.Field('host').AsString    := hosts[iping];
   proc.Field('count').AsInt32    := count;
   AddProcess(proc);
  end;
  ExecuteMulti;
  for iping := 0 to Field('process').ValueCount-1 do begin
   AnalyzeResult(GetProcess(iping));
  end;
end;

end.

