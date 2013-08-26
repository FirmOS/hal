unit fre_hal_dhcp;

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

  const

    {$IFDEF Darwin}
     cdhcppath='/fre/dhcp';
    {$ENDIF}
    {$IFDEF SOLARIS}
     cdhcppath='/fre/dhcp';
    {$ENDIF}
    {$IFDEF Linux}
     cdhcppath='/fre/dhcp';
    {$ENDIF}
    {$IFDEF FREEBSD}
     cdhcppath='/usr/local/etc/';
    {$ENDIF}
    cdhcp_file='dhcpd.conf';

  type

   { TFRE_HAL_DHCP }

   TFRE_HAL_DHCP = class (TFRE_HAL_Service)
   private
   protected
    function  ServiceSchemeName : string; override;
   public
    function  ConfigureService(const hal_object:IFRE_DB_Object) :IFRE_DB_Object; override;
    procedure GetServiceHAL   (const conn: IFRE_DB_Connection;const serviceobj: IFRE_DB_Object; const result_obj: IFRE_DB_Object); override;
   end;

implementation


{ TFRE_HAL_DHCP }

function TFRE_HAL_DHCP.ServiceSchemeName: string;
begin
  Result:='TFRE_DB_DHCP';
end;

function TFRE_HAL_DHCP.ConfigureService(const hal_object: IFRE_DB_Object  ): IFRE_DB_Object;
var lsl     : TStringList;
    lso     : IFRE_DB_Object;
    i       : integer;
    lsn     : string;
    lip     : string;
    lmask   : string;
    AProcess: TProcess;

begin
 ForceDirectories(cdhcppath);
 lsl:=TStringList.Create;
 try
  lsl.Add('# FirmOS DHCP HAL '+chalversion);
  lsl.add('option domain-name "'+hal_object.Field('default_domainname').AsString+'";');
  lsl.add('option domain-name-servers '+hal_object.Field('default_dns').AsString+';');
  lsl.Add('default-lease-time '+hal_object.Field('default_leasetime').AsString+';');
  lsl.Add('max-lease-time 7200;');
  lsl.Add('authoritative;');
  lsl.Add('log-facility local7;');

  for i:=0 to hal_object.Field('subnet').ValueCount-1 do begin
   lso:=   hal_object.Field('subnet').AsObjectItem[i];
   lsn:=   lso.Field('subnet').AsString;
   SplitCIDR(lsn,lip,lmask);
   lsl.Add('subnet '+lip+' netmask '+lmask+' {');
   lsl.Add(' range '+lso.Field('range_start').asstring+' '+lso.Field('range_end').asstring+';');
   if lso.FieldExists('router') then begin
    lsl.add(' option routers '+lso.Field('router').AsString+';');
   end;
   if lso.FieldExists('dns') then begin
    lsl.add(' option domain-name-servers '+lso.Field('dns').AsString+';');
   end;
   lsl.Add(' authoritative;');
   lsl.Add('}');
  end;

  for i:=0 to hal_object.Field('fixed').ValueCount-1 do begin
   lso:=   hal_object.Field('fixed').AsObjectItem[i];
   lsl.Add('host '+lso.Field('name').asstring+' {');
   lsl.Add(' fixed-address '+lso.Field('ip').asstring+';');
   lsl.Add(' hardware ethernet '+lso.Field('mac').asstring+';');
   if lso.FieldExists('router') then begin
    lsl.add(' option routers '+lso.Field('router').AsString+';');
   end;
   if lso.FieldExists('dns') then begin
    lsl.add(' option domain-name-servers '+lso.Field('dns').AsString+';');
   end;
   lsl.Add('}');
  end;
  lsl.SaveToFile(cdhcppath+'/'+cdhcp_file);
  RestartService('isc-dhcpd');
 finally
  lsl.Free;
 end;
end;

procedure TFRE_HAL_DHCP.GetServiceHAL(const conn: IFRE_DB_Connection; const serviceobj: IFRE_DB_Object; const result_obj: IFRE_DB_Object);
var co       : IFRE_DB_Object;
    cop      : IFRE_DB_Object;
    i        : integer;
    referred : TFRE_DB_GUIDArray;
begin
  result_obj.Field('DEFAULT_DNS').AsString:= serviceobj.Field('DEFAULT_DNS').AsString;
  result_obj.Field('DEFAULT_LEASETIME').AsInt16 := serviceobj.Field('DEFAULT_LEASETIME').AsInt16;
  result_obj.Field('DEFAULT_DOMAINNAME').AsString := serviceobj.Field('DEFAULT_DOMAINNAME').AsString;

  referred := serviceobj.ReferencedByList;
  for i:=0 to high(referred) do begin
   if conn.Fetch(referred[i],co) then begin
    if co.IsA('TFRE_DB_DHCP_FIXED') then begin
     cop:=GFRE_DBI.CreateFromString(co.AsString,conn);
     result_obj.Field('fixed').AddObject(cop);
    end;
    if co.IsA('TFRE_DB_DHCP_SUBNET') then begin
     cop:=GFRE_DBI.CreateFromString(co.AsString,conn);
     result_obj.Field('subnet').AddObject(cop);
    end;
   end else begin
    GFRE_BT.CriticalAbort('Error creating HAL Object for DHCP, referenced by DHCP Entry not found [%s] ',[GFRE_BT.GUID_2_HexString(referred[i])]);
   end;
  end;
  ConfigureService(result_obj);
end;




end.

