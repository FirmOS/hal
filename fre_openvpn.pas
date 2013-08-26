unit fre_openvpn;

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
{$codepage UTF8}

interface

uses
  Classes, SysUtils,FRE_DB_INTERFACE, FRE_DB_COMMON, FRE_PROCESS, FOS_BASIS_TOOLS,
  FOS_TOOL_INTERFACES,fre_testcase;

const

  cvpncommand  = 'openvpn';

type

  EFOS_OPENVPN_Exception=class(Exception);

  { TFRE_DB_OPENVPN }

  TFRE_DB_OPENVPN = class (TFRE_DB_Multiprocess)
  private
    openvpn_pid    :   integer;
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    function Connect(const config: IFRE_DB_Object; out error : string; out pid : integer) : integer;
    function Disconnect(out error : string; const pid: integer=-1) : integer;
  end;

implementation

{ TFRE_DB_OPENVPN }

class procedure TFRE_DB_OPENVPN.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

function TFRE_DB_OPENVPN.Connect(const config: IFRE_DB_Object; out error: string; out pid: integer): integer;
var
  proc     : TFRE_DB_Process;
  pidfile  : string;
  cmdline  : string;
  hash     : string;

  procedure ConstructCmdLine;

   procedure AddParams(const fld : IFRE_DB_Field);
   begin
    if fld.FieldType=fdbft_String then begin
      cmdline := cmdline +' --'+lowercase(fld.FieldName)+' '+fld.AsString;
    end else if fld.FieldType=fdbft_Boolean then begin
      cmdline := cmdline +' --'+lowercase(fld.FieldName);
    end;
   end;

  begin
   cmdline := cvpncommand+' --daemon --writepid '+pidfile;
   cmdline := cmdline+' --client';
   config.ForAllFields(@AddParams);
  end;

begin
  openvpn_pid := -1; pid := -1;
  ClearProcess;
  hash        := GFRE_BT.HashString_MD5_HEX(config.DumpToString);
  pidfile     := '/fre/pidlocks/'+hash+'.pid';
  ConstructCmdLine;
  proc := TFRE_DB_Process.create;
  writeln(cmdline);
  proc.SetupInput(cmdline);
  AddProcess(proc);
  ExecuteMulti;
  result := proc.Field('exitstatus').AsInt32;
  error  := proc.Field('errorstring').AsString;
  if result <>0 then begin
    exit;
  end else begin
    ClearProcess;
    proc := TFRE_DB_Process.create;
    proc.SetupInput('cat',TFRE_DB_StringArray.Create(pidfile));
    AddProcess(proc);
    ExecuteMulti;
    result := proc.Field('exitstatus').AsInt32;
    error  := proc.Field('errorstring').AsString;
    if result = 0 then begin
      writeln('PID:',proc.OutputToString);
      openvpn_pid := strtoint(trim(proc.OutputToString));
      pid         := openvpn_pid;
    end;
  end;
end;

function TFRE_DB_OPENVPN.Disconnect(out error: string; const pid: integer): integer;
var vpid  : integer;
    proc  : TFRE_DB_Process;
begin
  ClearProcess;
  if pid=-1 then begin
   vpid := openvpn_pid;
  end else begin
   vpid := pid;
  end;
  if pid = -1 then begin
    raise EFOS_OPENVPN_Exception.Create('NO PID TO DISCONNECT OPENVPN');
  end;
  proc := TFRE_DB_Process.create;
  proc.SetupInput('kill',TFRE_DB_StringArray.Create(inttostr(vpid)));
  AddProcess(proc);
  ExecuteMulti;
  result := proc.Field('exitstatus').AsInt32;
  error  := proc.Field('errorstring').AsString;
end;

end.

