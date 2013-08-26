unit fre_hal_cmd_utils;

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

uses Classes, SysUtils,FRE_DB_INTERFACE,FOS_FCOM_INTERFACES,FOS_FCOM_TYPES,FOS_INTERLOCKED;

function Send_HAL_CMD(const addr:string;const port:integer;const DBO:IFRE_DB_COMMAND;do_repeat:integer=1):IFRE_DB_Object;
function Send_HAL_CMD_DIRECT(const addr:string;const port:integer;const schemename,method:string;const in_data:IFRE_DB_Object):IFRE_DB_Object;

implementation

function Send_HAL_CMD(const addr:string;const port:integer;const DBO:IFRE_DB_COMMAND;do_repeat:integer):IFRE_DB_Object;
var err  : EFOS_OS_ERROR;
    sock : IFCOM_SOCK;
    ai   : IFCOM_AI;
    size : QWord;
    sw,sr: integer;
    data : string;
    readable : boolean;
    mem  : Pointer;

  procedure _error(const error:string);
  begin
    raise Exception.Create('SENDHALCMD FAIL:> '+error);
  end;

begin
  //result := nil;
  //sock := GFRE_FF.New_FCOM_NETSOCK(fil_IPV4,fsp_TCP,err);
  //if err=EFOS_OS_OK then begin
  //  ai := GFRE_FF.New_FCOM_AI;
  //  ai.ResolveandSet(addr,port);
  //  err:=sock.Connect(ai);
  //  if err<>EFOS_OS_OK then _error('CONNECT : '+CFOS_OS_ERROR[err]);
  //  while do_repeat>0 do begin
  //    dec(do_repeat);
  //    size := dbo.neededsize;
  //    err  := sock.Send(@size,sizeof(size),sw);
  //    Getmem(mem,size);
  //    dbo.CopyToMemory(mem);
  //    dbo.CommandID := dbo.CommandID+1;
  //    if err<>EFOS_OS_OK then _error('SEND : '+CFOS_OS_ERROR[err]);
  //    err  := sock.Send(mem,size,sw);
  //    Freemem(mem);
  //    err:=sock.Readable(5000,readable);
  //    if err<>EFOS_OS_OK then _error('READABLE : '+CFOS_OS_ERROR[err]);
  //    if not readable then _error('TIMEOUT');
  //    err := sock.Receive(@size,sizeof(size),sr);
  //    if err<>EFOS_OS_OK then _error('RECV SIZE : '+CFOS_OS_ERROR[err]);
  //    SetLength(data,size);
  //    err := sock.Receive(@data[1],size,sr);
  //    if err<>EFOS_OS_OK then _error('RECV DATA : '+CFOS_OS_ERROR[err]);
  //    result := GFRE_DBI.CreateFromMemory(@data[1],nil,false);
  //    if assigned(Result) then begin
  //      if (dbo.CommandID-1 mod 1000)=0  then begin
  //        writeln(Result.DumpToString());
  //      end;
  //    end;
  //    result.Finalize;
  //    result:=nil;
  //  end;
  //  dbo.Finalize;
  //  sock.Finalize;
  //end else _error(' NO SOCKET');
end;


var fClientCMDS : int64=1;

function Send_HAL_CMD_DIRECT(const addr: string; const port: integer;const schemename, method: string; const in_data: IFRE_DB_Object): IFRE_DB_Object;
var err  : EFOS_OS_ERROR;
    sock : IFCOM_SOCK;
    ai   : IFCOM_AI;
    size : QWord;
    sw,sr: integer;
    data : string;
    readable : boolean;
    mem  : Pointer;
    dbo  : IFRE_DB_COMMAND;

  procedure _error(const error:string);
  begin
    raise Exception.Create('SENDHALCMD FAIL:> '+error);
  end;

begin
  //result := nil;
  //sock := GFRE_FF.New_FCOM_NETSOCK(fil_IPV4,fsp_TCP,err);
  //if err=EFOS_OS_OK then begin
  //ai := GFRE_FF.New_FCOM_AI;
  //ai.ResolveandSet(addr,port);
  //err:=sock.Connect(ai);
  //if err<>EFOS_OS_OK then _error('CONNECT : '+CFOS_OS_ERROR[err]);
  //  dbo := GFRE_DBI.NewDBCommand;
  //  try
  //    dbo.InvokeClass  := schemename;
  //    dbo.InvokeMethod := method;
  //    dbo.Data         := in_data;
  //    dbo.CommandID    := fClientCMDS;
  //    FOS_IL_Increment64(fClientCMDS);
  //    dbo.ClientCommand:=true;
  //    size := dbo.neededsize;
  //    err  := sock.Send(@size,sizeof(size),sw);
  //    Getmem(mem,size);
  //    dbo.CopyToMemory(mem);
  //    dbo.CommandID := dbo.CommandID+1;
  //    if err<>EFOS_OS_OK then _error('SEND : '+CFOS_OS_ERROR[err]);
  //    err  := sock.Send(mem,size,sw);
  //    Freemem(mem);
  //    err:=sock.Readable(5000,readable);
  //    if err<>EFOS_OS_OK then _error('READABLE : '+CFOS_OS_ERROR[err]);
  //    if not readable then _error('TIMEOUT');
  //    err := sock.Receive(@size,sizeof(size),sr);
  //    if err<>EFOS_OS_OK then _error('RECV SIZE : '+CFOS_OS_ERROR[err]);
  //    SetLength(data,size);
  //    err := sock.Receive(@data[1],size,sr);
  //    if err<>EFOS_OS_OK then _error('RECV DATA : '+CFOS_OS_ERROR[err]);
  //    result := GFRE_DBI.CreateFromMemory(@data[1],nil,false);
  //  finally
  //    dbo.Finalize;
  //    sock.Finalize;
  //  end;
  //end;
end;

end.

