program fre_solariskernel;

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


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  BaseUnix,FOS_DEFAULT_IMPLEMENTATION,FOS_BASIS_TOOLS,FOS_TOOL_INTERFACES,FRE_PROCESS,FRE_DB_INTERFACE,
  fre_db_core,FRE_SYSTEM,fre_configuration,fre_dbbase;

  {$I fos_version_helper.inc}



type

  { TFRE_SolarisKernel }

  TFRE_SolarisKernel = class(TCustomApplication)
  protected
    sd_structure               : IFRE_DB_Object;
    fremote_key                : string;
    procedure DoRun            ; override;
    procedure GetSDStructure   ;
    procedure GetSDParameters  (const sd: IFRE_DB_Object);
    procedure GetSDVendor      (const sd: IFRE_DB_Object);
    procedure CheckAndSetRemote(const proc : TFRE_Process);
    procedure SetParam        ;
    procedure PrintSDStructure ;
    function  ExecWithRemote   (const cmd: string; const procparams: TFRE_DB_StringArray;  const inputstring: string; out outputstring, errorstring:string): integer;
  public
    constructor Create         (TheOwner: TComponent); override;
    destructor  Destroy        ; override;
    procedure   WriteHelp      ; virtual;
  end;

{ TFRE_SolarisKernel }

procedure TFRE_SolarisKernel.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hlocr:t:R:',['help','listsd','other','comstar','remotehost:','timeout:','retry:']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  InitMinimal(false);
  GFRE_DBI.Initialize_Extension_Objects;
  fre_dbbase.Register_DB_Extensions;
  Initialize_Read_FRE_CFG_Parameter;

  if HasOption('r','remotehost') then begin
    cFRE_REMOTE_HOST := GetOptionValue('r','remotehost');
    cFRE_REMOTE_USER := 'root';
    fremote_key      := SetDirSeparators(cFRE_SERVER_DEFAULT_DIR+'/ssl/user/id_rsa');
  end;

  if HasOption('l','listsd') then begin
    GetSDStructure;
    PrintSDStructure;
    Terminate;
    Exit;
  end;

  if HasOption('o','other') and HasOption('c','comstar') then begin
    writeln('Option other and option comstar can not be used together!');
    Terminate;
    Exit;
  end;

  if HasOption('t','timeout') or HasOption('R','retry') then begin
    GetSDStructure;
    PrintSDStructure;
    SetParam;
    GetSDStructure;
    PrintSDStructure;
  end;

  { add your program here }

  // stop program loop
  WriteHelp;
  Terminate;
  Exit;
end;

procedure TFRE_SolarisKernel.GetSDStructure;
var res  : integer;
    outstring   : string;
    errorstring : string;
    sl          : TStringList;
    i           : NativeInt;
    sd          : IFRE_DB_Object;
begin
  sd_structure := GFRE_DBI.NewObject;
  res := ExecWithRemote('mdb -k',nil,'::walk sd_state | ::grep ''.!=0'''+LineEnding,outstring,errorstring);
  if res=0 then
    begin
      sl := TStringList.Create;
      try
        sl.text := outstring;
        for i := 0 to sl.count-1 do
          begin
            sd := GFRE_DBI.NewObject;
            sd.Field('addr').AsString:=sl[i];
            GetSDParameters(sd);
            GetSDVendor(sd);
            sd_structure.Field(sd.Field('addr').asstring).AsObject:=sd;
          end;
      finally
        sl.Free;
      end;
    end
  else
    GFRE_LOG.LogConsole('Error getting sd_lun structures!'+outstring+' '+errorstring);
  //writeln(sd_structure.DumpToString());
end;

procedure TFRE_SolarisKernel.GetSDParameters(const sd: IFRE_DB_Object);
var res  : integer;
    outstring   : string;
    errorstring : string;
    sl          : TStringList;
    i           : NativeInt;
    s           : string;
begin
  res := ExecWithRemote('mdb -k',nil,sd.Field('addr').asstring+'::print -a struct sd_lun'+LineEnding,outstring,errorstring);
  if res=0 then
    begin
      sl := TStringList.Create;
      try
        sl.text := outstring;
        for i := 0 to sl.count-1 do
          begin
            s := sl[i];
            if Pos('un_sd =',s)>0 then
              sd.Field('un_sd').asstring:= trim(GFRE_BT.SepRight(s,'='));
            if Pos('un_cmd_timeout =',s)>0 then
              begin
                sd.Field('un_cmd_timeout_addr').asstring:= trim(GFRE_BT.SepLeft(s,'un_cmd'));
                sd.Field('un_cmd_timeout').asstring:= trim(GFRE_BT.SepRight(s,'='));
                sd.Field('un_cmd_timeout_int16').asint16 := StrToInt(sd.Field('un_cmd_timeout').asstring);
              end;
            if Pos('un_retry_count =',s)>0 then
              begin
                sd.Field('un_retry_count_addr').asstring:= trim(GFRE_BT.SepLeft(s,'un_retry'));
                sd.Field('un_retry_count').asstring:= trim(GFRE_BT.SepRight(s,'='));
                sd.Field('un_retry_count_int16').asint16 := StrToInt(sd.Field('un_retry_count').asstring);
              end;
          end;
      finally
        sl.Free;
      end;
    end
  else
    GFRE_LOG.LogConsole('Error getting sd_lun structure!'+outstring+' '+errorstring);
end;

procedure TFRE_SolarisKernel.GetSDVendor(const sd: IFRE_DB_Object);
var res  : integer;
    outstring   : string;
    errorstring : string;
    sl          : TStringList;
    i           : NativeInt;
    s           : string;
begin
  res := ExecWithRemote('mdb -k',nil,sd.Field('un_sd').asstring+'::print struct scsi_device sd_inq | ::print struct scsi_inquiry inq_vid inq_pid'+LineEnding,outstring,errorstring);
  if res=0 then
    begin
      sl := TStringList.Create;
      try
        sl.text := outstring;
        for i := 0 to sl.count-1 do
          begin
            s := sl[i];
            if Pos('inq_vid =',s)>0 then
              sd.Field('inq_vid').asstring:= GFRE_BT.SepLeft(trim(GFRE_BT.SepRight(s,'[ "')),'" ]');
            if Pos('inq_pid =',s)>0 then
              sd.Field('inq_pid').asstring:= GFRE_BT.SepLeft(trim(GFRE_BT.SepRight(s,'[ "')),'" ]');
          end;
      finally
        sl.Free;
      end;
    end
  else
    GFRE_LOG.LogConsole('Error getting sd_inq structure!'+outstring+' '+errorstring);
end;

procedure TFRE_SolarisKernel.CheckAndSetRemote(const proc: TFRE_Process);
begin
  if cFRE_REMOTE_HOST<>'' then
    begin
      proc.ConfigureRemote_SSH_Mode(cFRE_REMOTE_USER,cFRE_REMOTE_HOST,fremote_key);
    end;
end;

procedure TFRE_SolarisKernel.SetParam;
var cs_to,cs_rtr: Word;

  procedure _SetComstar(const obj:IFRE_DB_Object);
  var res : integer;
      outstring,errorstring:string;
      param : string;
  begin
    if HasOption('o','other') then
      if Pos('COMSTAR',obj.Field('inq_pid').asstring)>0 then
        exit;
    if HasOption('c','comstar') then
      if Pos('COMSTAR',obj.Field('inq_pid').asstring)=0 then
        exit;
    if cs_to>0 then begin
      param := obj.Field('un_cmd_timeout_addr').asstring+'/W '+lowercase('0x'+IntToHex(cs_to,2))+LineEnding;
      res := ExecWithRemote('mdb -kw',nil,param,outstring,errorstring);
      if res=0 then
        writeln(obj.Field('addr').asstring+':',outstring)
      else
        begin
          writeln('error setting un_cmd_timeout for ',obj.Field('addr').asstring,' ',errorstring);
          abort;
        end;
    end;
    if cs_rtr>0 then begin
      param := obj.Field('un_retry_count_addr').asstring+'/W '+lowercase('0x'+IntToHex(cs_rtr,2))+LineEnding;
      res := ExecWithRemote('mdb -kw',nil,param,outstring,errorstring);
      if res=0 then
        writeln(obj.Field('addr').asstring+':',outstring)
      else
        begin
          writeln('error setting un_retry_count for ',obj.Field('addr').asstring,' ',errorstring);
          abort;
        end;
    end;
  end;

begin
  cs_to  := 0;
  cs_rtr := 0;
  if HasOption('t','timeout') then
    cs_to := StrToIntDef(GetOptionValue('t','timeout'),0);
  if HasOption('R','retry') then
    cs_rtr := StrToIntDef(GetOptionValue('R','retry'),0);
  sd_structure.ForAllObjects(@_SetComstar);
end;

procedure TFRE_SolarisKernel.PrintSDStructure;
var fmts: string;

  procedure _PrintSD(const obj:IFRE_DB_Object);
  var s: string;
  begin
    s:=Format(fmts,[obj.Field('addr').asstring,obj.Field('inq_vid').asstring,obj.Field('inq_pid').asstring,inttostr(obj.Field('un_cmd_timeout_int16').asint16),inttostr(obj.Field('un_retry_count_int16').asint16)]);
    writeln(s);
  end;

begin
 fmts :='%-16s %-16s %-16s  %-12s %-12s';
 writeln(Format(fmts,['sd_state_addr','vid','pid','cmd_timeout','cmd_retry']));
 sd_structure.ForAllObjects(@_PrintSD);
// writeln(sd_structure.DumpToString());
end;


function TFRE_SolarisKernel.ExecWithRemote(const cmd: string; const procparams: TFRE_DB_StringArray; const inputstring: string; out outputstring, errorstring: string): integer;
var proc : TFRE_Process;
begin
  proc :=  TFRE_Process.Create(nil);
  try
    CheckAndSetRemote(proc);
    result := proc.ExecutePiped(cmd,procparams,inputstring,outputstring,errorstring);
  finally
    proc.free;
  end;
end;

constructor TFRE_SolarisKernel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TFRE_SolarisKernel.Destroy;
begin
  inherited Destroy;
end;

procedure TFRE_SolarisKernel.WriteHelp;
begin
  { add your help code here }
  writeln(GFOS_VHELP_GET_VERSION_STRING);
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TFRE_SolarisKernel;
begin
  Application:=TFRE_SolarisKernel.Create(nil);
  Application.Title:='SolarisKernel';
  Application.Run;
  Application.Free;
end.

