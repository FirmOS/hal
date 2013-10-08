unit fre_hal_control;

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
  Classes, SysUtils,FOS_TOOL_INTERFACES,Sockets,math,FRE_HAL_UTILS,FRE_DB_INTERFACE,
  FRE_HAL_DHCP,FRE_HAL_ROUTING,FRE_HAL_RADIUS,FRE_HAL_VPN,FRE_HAL_REDIRECT,
  fpjson;

type

  { TFRE_HAL_Control }

  TFRE_HAL_Control = class (TObject)
  public
   function  CreateHal  (const conn:IFRE_DB_Connection;const servicegroup_guid:TGUID;const machine_id:TGUID):IFRE_DB_Object;
   procedure DirectHal  (const conn:IFRE_DB_Connection;const servicegroup_guid:TGUID);
  end;


  procedure CreateHAL(const dbofile:string='/fre/hal/redirect.dbo'; const psfile:string='/fre/hal/ps.dbo');


implementation

{ TFRE_HAL_Control }

function TFRE_HAL_Control.CreateHal(const conn: IFRE_DB_Connection; const servicegroup_guid: TGUID; const machine_id:TGUID): IFRE_DB_Object;
var halo:IFRE_DB_Object;

    procedure _addhal(const servicename:string);
    var obj: IFRE_DB_Object;
        hal: TFRE_HAL_BaseService;
    begin
     hal:=nil;
     writeln(servicename);
     case servicename of
      cfre_hal_service_redirect:  hal:=TFRE_HAL_REDIRECT.Create;
      cfre_hal_service_dhcp    :  hal:=TFRE_HAL_DHCP.Create;
      cfre_hal_service_routing :  hal:=TFRE_HAL_ROUTING.Create;
      cfre_hal_service_vpn     :  hal:=TFRE_HAL_VPN.Create;
      cfre_hal_service_radius  :  hal:=TFRE_HAL_RADIUS.Create;
     end;
     if assigned(hal) then begin
      obj:=hal.GetHALObject(conn,servicegroup_guid,machine_id);
      if obj.FieldCount(true)>1 then begin
       halo.Field(servicename).AsObject:=obj;
      end else begin
       obj.Finalize;
      end;
      hal.Free;
     end;
    end;



begin
   halo:=GFRE_DBI.NewObject;
   _addhal(cfre_hal_service_redirect);
//   _addhal(cfre_hal_service_dhcp);
//   _addhal(cfre_hal_service_routing);
//   _addhal(cfre_hal_service_ps);
   _addhal(cfre_hal_service_vpn);
//   _addhal(cfre_hal_service_radius);
   result:=halo;
end;

procedure TFRE_HAL_Control.DirectHal(const conn: IFRE_DB_Connection; const servicegroup_guid: TGUID);

 procedure _directhal(const servicename:string);
 var hal: TFRE_HAL_DirectService;
 begin
  hal:=nil;
  if assigned(hal) then begin
   writeln('assigned hal '+servicename);
   hal.ConfigureServiceDirect(conn,servicegroup_guid);
   hal.Free;
  end;
 end;
begin
 writeln('directhal');
 _directhal(cfre_hal_service_ca);
end;

procedure CreateHAL(const dbofile:string; const psfile:string);
var conn : IFRE_DB_Connection;
    coll : IFRE_DB_COLLECTION;
    servicegroup_guid:TGUID;
    hal  : TFRE_HAL_Control;
    rd   : IFRE_DB_Object;
    sm   : TStringList;
    js   : TJSONData;

  procedure _machinehal(const obj:IFRE_DB_Object);
  var halo : IFRE_DB_Object;
         s : string;
  begin
   if obj.FieldExists('servicegroup') then begin
     if FREDB_Guids_Same(obj.Field('servicegroup').AsObjectLink,servicegroup_guid) then begin
      writeln('machine ',obj.Field('ip').AsString);
      halo:=hal.CreateHal(conn,servicegroup_guid,obj.uid);
//      writeln(halo.DumpToString());
      if halo.FieldExists(cfre_hal_service_redirect) then begin
       rd:=halo.Field(cfre_hal_service_redirect).AsObject;
  //     rd.SaveToFile('/fre/tmp/rd_'+obj.Field('ip').asstring+'.dbo',true);
       rd.SaveToFile(dbofile,true);
      end;
      if halo.FieldExists(cfre_hal_service_ps) then begin
        rd:=halo.Field(cfre_hal_service_ps).AsObject;
        rd.SaveToFile(psfile,true);
      end;
//      halo.SaveToFile('/fre/tmp/hal_'+obj.Field('ip').asstring+'.dbo',true);
      halo.Finalize;
     end;
    end;
  end;


begin

  CONN := GFRE_DBI.NewConnection;
  CONN.Connect('KACKDB','admin','admin');
  servicegroup_guid:=DummyGetServiceGroupID(conn);
  writeln('createhal');
  hal:=TFRE_HAL_Control.Create;
  try
//   hal.DirectHal(conn,servicegroup_guid);
   coll:=Conn.Collection('machine');
   writeln(coll.Count);
   coll.ForAll(@_machinehal);
  finally
   hal.free;
  end;
end;

end.

