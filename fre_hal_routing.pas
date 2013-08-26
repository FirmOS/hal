unit fre_hal_routing;

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
  Classes, SysUtils,FOS_TOOL_INTERFACES,FRE_DB_INTERFACE,FRE_HAL_UTILS,process;

  type

   { TFRE_HAL_ROUTING }

   TFRE_HAL_ROUTING = class (TFRE_HAL_Service)
   private
   protected
    function  ServiceSchemeName : string; override;
   public
    function  ConfigureService(const hal_object:IFRE_DB_Object) :IFRE_DB_Object; override;
    procedure GetServiceHAL   (const conn: IFRE_DB_Connection;const serviceobj: IFRE_DB_Object; const result_obj: IFRE_DB_Object); override;
   end;


implementation

{ TFRE_HAL_ROUTING }

function TFRE_HAL_ROUTING.ServiceSchemeName: string;
begin
  Result:='TFRE_DB_ROUTING';
end;

function TFRE_HAL_ROUTING.ConfigureService(const hal_object: IFRE_DB_Object): IFRE_DB_Object;
var lsl     : TStringList;
    lso     : IFRE_DB_Object;
    i       : integer;
    lsn     : string;
    lip     : string;
    lmask   : string;
    AProcess: TProcess;

begin
 writeln('ROUTING CONFIGURE');
 lsl:=TStringList.Create;
 try
  lsl.Add('route -n flush');
  lsl.Add('route add default '+hal_object.Field('default').AsString);
  for i:=0 to hal_object.Field('static').ValueCount-1 do begin
   lso:= hal_object.Field('static').AsObjectItem[i];
   lsn:= lso.Field('subnet').AsString;
   lsl.Add('route add -net '+lsn+' '+lso.Field('gateway').AsString);
  end;
  writeln(lsl.Text);
  {$IFDEF FREEBSD}
  AProcess := TProcess.Create(nil);
  try
   for i:=0 to lsl.count-1 do begin
    AProcess.CommandLine := lsl[i];
    AProcess.Options := AProcess.Options + [poWaitOnExit];
    AProcess.Execute;
    writeln(AProcess.ExitStatus);
   end;
  finally
   AProcess.Free;
  end;
  {$ENDIF}
 finally
  lsl.Free;
 end;
end;

procedure TFRE_HAL_ROUTING.GetServiceHAL(const conn: IFRE_DB_Connection; const serviceobj: IFRE_DB_Object; const result_obj: IFRE_DB_Object);
var i:integer;
begin
 result_obj.Field('default').AsString:=serviceobj.Field('Default').AsString;
 for i:=0 to serviceobj.Field('static').ValueCount-1 do begin
  result_obj.Field('static').AddObject(GFRE_DBI.CreateFromString(serviceobj.Field('static').AsObjectItem[i].AsString,conn));
 end;
end;

end.

