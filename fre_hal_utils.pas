unit fre_hal_utils;

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
{$interfaces corba}

interface

uses
  Classes, SysUtils,FOS_TOOL_INTERFACES,FRE_SYSTEM,FRE_DB_INTERFACE,Sockets,math,process;

const

  cfre_hal_dbo ='hal.dbo';
  cfre_redirect_dbo ='redirect.dbo';
  chalversion='0.01';
  cfre_hal_service_dhcp = 'DHCP';
  cfre_hal_service_ps   = 'PS';
  cfre_hal_service_vpn  = 'VPN';
  cfre_hal_service_routing = 'ROUTING';
  cfre_hal_service_radius  = 'RADIUS';
  cfre_hal_service_ca   = 'CA';
  cfre_hal_service_redirect = 'REDIRECT';



type

  TFRE_HAL_IP4 = packed record
    case byte of
      0: (_bytes: packed array [0..3] of byte);
      1: (_long:  Longword);
  end;

type


  { TFRE_HAL_BaseService }

  TFRE_HAL_BaseService = class (TFRE_DB_ObjectEx)
  protected
    procedure LogInfo           (const msg:string);
    procedure LogError          (const msg:string);
    procedure GetServiceHAL     (const conn: IFRE_DB_Connection;const serviceobj:IFRE_DB_Object;const result_obj:IFRE_DB_Object); virtual;
    function  ServiceSchemeName : string; virtual; abstract;
  public
    function  GetHALObject      (const conn:IFRE_DB_Connection;const service_guid:TGUID;const machine_id:TGUID) : IFRE_DB_Object;
    procedure StartService      (const name:string); virtual;
    procedure StopService       (const name:string); virtual;
    procedure RestartService    (const name:string); virtual;

  end;

  { TFRE_HAL_Service }

  TFRE_HAL_Service = class (TFRE_HAL_BaseService)
  public
    function  ConfigureService(const hal_object:IFRE_DB_Object) :IFRE_DB_Object; virtual; abstract;

  end;

  { TFRE_HAL_DirectService }

  TFRE_HAL_DirectService = class (TFRE_HAL_BaseService)
  public
    procedure ConfigureServiceDirect(const conn:IFRE_DB_Connection;const service_guid:TGUID); virtual; abstract;
  end;


  procedure SaveHAL(const halo:IFRE_DB_Object);
  function  LoadHal:IFRE_DB_Object;

  procedure SaveRedirect(const halo:IFRE_DB_Object);
  function  LoadRedirect:IFRE_DB_Object;

  function DummyGetServiceGroupID(const conn:IFRE_DB_Connection):TGUID;
  procedure SplitCIDR(const cidr:string;out ip,mask:string);
  procedure SplitCIDRNet(const cidr:string;out net,mask:string);
  function BitsToMask(bits:integer):string;
  function GetNetworkID(const nw:IFRE_DB_Object):string;
  function GetNetworkIDfromString(const ipr:string):string;
  function GetIPDots(const ip:string; co:integer):string;
  function IP4toString(const ip:TFRE_HAL_IP4):string;
  function StringtoIP4(const ips:string): TFRE_HAL_IP4;

implementation


function GetIPDots(const ip:string; co:integer):string;
var o:string;
    p:integer;
begin
 result:='';
 o:=ip;
 while co>0 do begin
  p:=Pos('.',o);
  if p=0 then break;
  result:=result+Copy(o,1,p);
  o:=Copy(o,p+1,maxint);
  dec(co);
 end;
end;



function NMask(bits:integer):TFRE_HAL_IP4;
var m:Longword;
    ibits:integer;
begin
 if (bits>32) then begin
  bits:=32;
 end else if (bits<0) then begin
  bits:=0;
 end;
 ibits:=32-bits;
 m:=trunc(intpower(2,bits))-1;
 m:=m shl ibits;
 result._long:=htonl(m);
end;

function IP4toString(const ip:TFRE_HAL_IP4):string;
begin
 result:=inttostr(ip._bytes[0])+'.'+inttostr(ip._bytes[1])+'.'+inttostr(ip._bytes[2])+'.'+inttostr(ip._bytes[3]);
end;

function StringtoIP4(const ips:string): TFRE_HAL_IP4;
var ip,s  : string;
begin
 try
  ip:= ips;
  s := Copy(ip,1,Pos('.',ip)-1);
  ip:= Copy(ip,Pos('.',ip)+1,maxint);
  result._bytes[0]:=strtoint(s);
  s := Copy(ip,1,Pos('.',ip)-1);
  ip:= Copy(ip,Pos('.',ip)+1,maxint);
  result._bytes[1]:=strtoint(s);
  s := Copy(ip,1,Pos('.',ip)-1);
  ip:= Copy(ip,Pos('.',ip)+1,maxint);
  result._bytes[2]:=strtoint(s);
  result._bytes[3]:=strtoint(ip);
 except
   raise EConvertError.Create('Error on converting '+ips+' to HAL_IP4');
 end;
end;

function BitsToMask(bits:integer):string;
var ip:TFRE_HAL_IP4;
begin
 ip:=Nmask(bits);
 result:=IP4toString(ip);
end;

procedure SaveHAL(const halo: IFRE_DB_Object);
begin
   halo.SaveToFile(cFRE_HAL_CFG_DIR+'/'+cfre_hal_dbo);
end;

function LoadHal: IFRE_DB_Object;
begin
  result:= GFRE_DBI.CreateFromFile(cFRE_HAL_CFG_DIR+'/'+cfre_hal_dbo);
end;

procedure SaveRedirect(const halo: IFRE_DB_Object);
begin
 halo.SaveToFile(cFRE_HAL_CFG_DIR+'/'+cfre_redirect_dbo);
end;

function LoadRedirect: IFRE_DB_Object;
var fn : string;
begin
 fn := cFRE_HAL_CFG_DIR+'/'+cfre_redirect_dbo;
 try
   result:= GFRE_DBI.CreateFromFile(fn);
 except on E:Exception do begin
   writeln('Could not load redirection file '+fn+' : '+E.Message);
 end; end;
end;

function GetNetworkIDfromString(const ipr:string):string;
var ip,mask:string;
    maskbits:integer;


begin
 mask:= Copy(ipr,Pos('/',ipr)+1,maxint);
 ip:=   Copy(ipr,1,Pos('/',ipr)-1);
 case mask of
  '24': ip:=GetIPDots(ip,3);
  '16': ip:=GetIPDots(ip,2);
  '8' : ip:=GetIPDots(ip,1);
 else
  raise Exception.Create('Unsupported Network Mask for '+ipr);
 end;
 result:=uppercase(StringReplace(ip,'.','_',[rfReplaceAll]));
end;

function GetNetworkID(const nw:IFRE_DB_Object):string;
begin
 result:=GetNetworkIDfromString(nw.Field('ip_net').asstring);
end;


function DummyGetServiceGroupID(const conn: IFRE_DB_Connection): TGUID;
var coll:IFRE_DB_COLLECTION;
    found:boolean;

  procedure _find(const obj:IFRE_DB_Object);
  begin
   if found=false then begin
    if obj.Field('objname').asstring='MAINSG' then begin
     result:=obj.UID;
     found :=true;
    end;
   end;
  end;

begin
 found:=false;
 coll:=conn.Collection('servicegroup');
 coll.ForAll(@_find);
end;

procedure SplitCIDR(const cidr:string;out ip,mask:string);
begin
  mask:= Copy(cidr,Pos('/',cidr)+1,maxint);
  ip:=   Copy(cidr,1,Pos('/',cidr)-1);
  mask:=   BitsToMask (strtoint(mask));
end;

procedure SplitCIDRNet(const cidr: string; out net, mask: string);
var maskip:TFRE_HAL_IP4;
    netip :TFRE_HAL_IP4;
    ip    :string;
    s     :string;
begin
 mask  := Copy(cidr,Pos('/',cidr)+1,maxint);
 ip    := Copy(cidr,1,Pos('/',cidr)-1);
 maskip:= NMask(strtoint(mask));
 netip := StringtoIP4(ip);

 netip._long:=netip._long and maskip._long;
 net:=IP4toString(netip);
 mask:=IP4toString(maskip);
end;


{ TFRE_HAL_Service }

procedure TFRE_HAL_BaseService.LogInfo(const msg: string);
begin
 GFRE_LOG.Log(msg,'',fll_Info,'HAL',false);
end;

procedure TFRE_HAL_BaseService.LogError(const msg: string);
begin
 GFRE_LOG.Log(msg,'',fll_Error,'HAL',false);
end;

procedure TFRE_HAL_BaseService.GetServiceHAL(const conn: IFRE_DB_Connection;const serviceobj: IFRE_DB_Object; const result_obj: IFRE_DB_Object);
begin
 // do nothing
end;

function TFRE_HAL_BaseService.GetHALObject(const conn: IFRE_DB_Connection; const service_guid: TGUID; const machine_id: TGUID): IFRE_DB_Object;
var coll:IFRE_DB_Collection;
      ro:IFRE_DB_Object;

  procedure _find(const obj:IFRE_DB_object);
  begin
   if FREDB_Guids_Same(obj.Field('servicegroup').AsObjectLink,service_guid) then begin
    if FREDB_Guids_Same(obj.Field('machineid').AsObjectLink,machine_id) then begin
     if obj.SchemeClass=ServiceSchemeName then begin
      GetServiceHAL(conn,obj,ro);
     end;
    end;
   end;
  end;

begin
 ro  :=GFRE_DBI.NewObject;
 coll:=Conn.Collection('service');
 coll.ForAll(@_find);
 result:=ro;
end;

procedure TFRE_HAL_BaseService.StartService(const name: string);
var aprocess:TProcess;
begin
 {$IFDEF FREEBSD}
 AProcess := TProcess.Create(nil);
 try
  AProcess.CommandLine := '/usr/local/etc/rc.d/'+name+' start';
  writeln(aprocess.CommandLine);
  AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
 finally
  AProcess.Free;
 end;
 {$ENDIF}
end;

procedure TFRE_HAL_BaseService.StopService(const name: string);
var aprocess:TProcess;
begin
 {$IFDEF FREEBSD}
 AProcess := TProcess.Create(nil);
 try
  AProcess.CommandLine := '/usr/local/etc/rc.d/'+name+' stop';
  writeln(aprocess.CommandLine);
  AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
 finally
  AProcess.Free;
 end;
 {$ENDIF}
end;

procedure TFRE_HAL_BaseService.RestartService(const name: string);
begin
 StopService(name);
 StartService(name);
end;


end.

