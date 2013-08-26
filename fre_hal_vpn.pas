unit fre_hal_vpn;

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
  Classes, SysUtils,FOS_TOOL_INTERFACES,FRE_HAL_UTILS,FRE_DB_INTERFACE,process,FRE_DBCLOUDCONTROL,FRE_SYSTEM;

  const
    cvpnpath=cFRE_GLOBAL_DIRECTORY+'/openvpn';
    copenvpn='/usr/local/sbin/openvpn';

  type

    { TFRE_HAL_VPN }

    TFRE_HAL_VPN = class (TFRE_HAL_Service)
    private
    protected
     function  ServiceSchemeName : string; override;
    public
     function  ConfigureService(const hal_object:IFRE_DB_Object) :IFRE_DB_Object; override;
     procedure GetServiceHAL   (const conn: IFRE_DB_Connection;const serviceobj: IFRE_DB_Object; const result_obj: IFRE_DB_Object); override;
     procedure RestartService  (const name:string); override;
     procedure StartService    (const name:string); override;
     procedure StopService     (const name:string); override;
    end;


implementation

{ TFRE_HAL_VPN }

function TFRE_HAL_VPN.ServiceSchemeName: string;
begin
  Result:='TFRE_DB_VPN';
end;

function TFRE_HAL_VPN.ConfigureService(const hal_object: IFRE_DB_Object): IFRE_DB_Object;
var ob:IFRE_DB_Object;
     i:integer;

  procedure _Configure(const vpn:IFRE_DB_Object);
  var vpndir   : string;
          sl   : TStringList;
       ip,mask : string;
          i    : integer;
      vpnname  : string;
      client_nr   : integer;
      client_ip   : string;


   procedure _writeCCD;
   var cl:TStringList;
        j:integer;
        k:integer;
      ccd:IFRE_DB_Object;
      net:string;
     mask:string;

   begin
    ForceDirectories(vpndir+'/ccd');
    if not DirectoryExists(vpndir+'/ccd') then begin
     GFRE_BT.CriticalAbort('Could not create VPN CCD Dir [%s] ',[vpndir+'/ccd']);
    end;
    for j:=0 to vpn.Field('CCD').ValueCount-1 do begin
     ccd :=vpn.Field('CCD').AsObjectItem[j];
     cl  :=TstringList.Create;
     try
      for k:=0 to ccd.Field('IP_NET').ValueCount-1 do begin
       SplitCIDRNet(ccd.Field('IP_NET').AsStringItem[k],net,mask);
       cl.Add('iroute '+net+' '+mask);
      end;
      cl.add('ifconfig-push '+client_ip+inttostr(client_nr)+' 192.168.99.1');
      inc(client_nr);
      cl.SaveToFile(vpndir+'/ccd/'+ccd.Field('CN').AsString);
     finally
      cl.Free;
     end;
    end;
   end;

  begin
   vpnname:=GFRE_BT.Str2HexStr(vpn.Field('name').AsString);
   vpndir:=cvpnpath+'/'+vpnname;
   ForceDirectories(vpndir);
   if not DirectoryExists(vpndir) then begin
    GFRE_BT.CriticalAbort('Could not create VPN Dir [%s] ',[vpndir]);
   end;
   GFRE_BT.StringToFile(vpndir+'ca.pem',vpn.Field('CA').AsString);
   GFRE_BT.StringToFile(vpndir+'dh1024.pem',vpn.Field('DH').AsString);
   GFRE_BT.StringToFile(vpndir+'crt.pem',vpn.Field('CRT').AsString);
   GFRE_BT.StringToFile(vpndir+'key.pem',vpn.Field('KEY').AsString);
   GFRE_BT.StringToFile(vpndir+'crl.pem',vpn.Field('CRL').AsString);

   sl:=TStringList.Create;
   try
     sl.Add('local '+vpn.Field('EXTERNAL_ADDRESS').AsString);
     sl.Add('port ' +vpn.Field('PORT').AsString);
     sl.Add('proto ' +vpn.Field('PROTOCOL').AsString);
     sl.Add('dev tun');
     sl.Add('ca '+vpndir+'ca.pem');
     sl.Add('cert '+vpndir+'crt.pem');
     sl.Add('key '+vpndir+'key.pem');
     sl.add('crl-verify '+vpndir+'crl.pem');
     sl.Add('dh '+vpndir+'dh1024.pem');

     SplitCIDR(vpn.Field('SERVER_IP').AsString,ip,mask);
     sl.Add('server '+ip+' '+mask);
//     ;ifconfig-pool-persist ipp.txt
     for i:=0 to vpn.field('PUSH_ROUTES').ValueCount-1 do begin
      SplitCIDR(vpn.Field('PUSH_ROUTES').AsStringItem[i],ip,mask);
      sl.add('push "route '+ip+' '+mask+'"');
     end;
     sl.Add('client-config-dir ccd');
     for i:=0 to vpn.field('ROUTES').ValueCount-1 do begin
      SplitCIDR(vpn.Field('ROUTES').AsStringItem[i],ip,mask);
      sl.add('route '+ip+' '+mask);
     end;
     if vpn.FieldExists('REDIRECT_GATEWAY') then begin
      if vpn.Field('REDIRECT_GATEWAY').AsBoolean then begin
       sl.add('push "redirect-gateway"');
      end;
     end;
//     ;push "dhcp-option DNS 10.8.0.1"
//     ;push "dhcp-option WINS 10.8.0.1"
//     ;client-to-client
     sl.add('keepalive 10 120');
     sl.Add('cipher '+vpn.Field('CIPHER').asstring);
     if vpn.FieldExists('COMPRESSION') then begin
      if vpn.Field('COMPRESSION').AsBoolean then begin
       sl.add('comp-lzo');
      end;
     end;
     if vpn.FieldExists('MTU') then begin
      sl.Add('tun-mtu '+vpn.Field('MTU').AsString);
     end;
     if vpn.FieldExists('MAX_CLIENTS') then begin
      sl.Add('max-clients '+vpn.Field('MAX_CLIENTS').AsString);
     end;
     sl.Add('user nobody');
     sl.Add('group nobody');
     sl.Add('persist-key');
     sl.Add('persist-tun');
     sl.Add('status /var/log/openvpn_'+vpnname+'.status');
     sl.Add('log /var/log/openvpn_'+vpnname+'.log');
     sl.Add('verb 3');
//     ;mute 20
     sl.SaveToFile(vpndir+'/server.conf');
   finally
    sl.Free;
   end;
   SplitCIDR(vpn.Field('SERVER_IP').AsString,ip,mask);
   client_ip := GetIPDots(ip,3);
   client_nr := 6;
   _writeCCD;
  end;

begin
 for i:=0 to hal_object.Field('VPN').ValueCount-1 do begin
  ob:=hal_object.Field('VPN').AsObjectItem[i];
  _Configure(ob);
  ReStartService(ob.Field('objname').AsString);
 end;
end;

procedure TFRE_HAL_VPN.GetServiceHAL(const conn: IFRE_DB_Connection; const serviceobj: IFRE_DB_Object; const result_obj: IFRE_DB_Object);
var ob    :IFRE_DB_Object;
    co    :IFRE_DB_Object;
    cop   :IFRE_DB_Object;
    ca    :IFRE_DB_Object;
    crt   :IFRE_DB_Object;

 procedure _getsubnets(const vpn:IFRE_DB_Object);
 var colle:IFRE_DB_Collection;

  procedure _loopendpoints(const eob:IFRE_DB_Object);
  var j  :integer;
      crt:IFRE_DB_Object;
      nw :IFRE_DB_Object;
      sub:IFRE_DB_Object;
      linksys   : TFRE_DB_GUIDArray;

  begin
   if eob.FieldExists('vpn_crtid') then begin
    if eob.Field('vpn_crtid').ValueCount=1 then begin
     if conn.Fetch(eob.Field('vpn_crtid').AsObjectLink,crt)=false then begin
      LogError('Crt '+GFRE_BT.GUID_2_HexString(eob.Field('vpn_crtid').AsObjectLink)+' not found in endpoint '+eob.UID_String);
     end else begin
      if FREDB_Guids_Same(crt.Field('ca').AsObjectLink,ca.UID) then begin     // endpoint uses the same ca as configured for vpn
       sub:=GFRE_DBI.NewObject;
       sub.Field('cn').AsString:=crt.Field('cn').asstring;
       linksys   := eob.ReferencedByList(TFRE_DB_Network.ClassName);
       for j:=low(linksys) to high (linksys) do begin
        if Conn.Fetch(linksys[j],nw)=false then begin
         LogError('Network '+GFRE_BT.GUID_2_HexString(linksys[j])+' not found in endpoint '+eob.UID_String);
        end else begin
         sub.Field('ip_net').AddString(nw.Field('ip_net').AsString);
        end;
       end;
       vpn.Field('ccd').AddObject(sub);
      end;
     end;
    end;
   end;
  end;

 begin
  colle:= CONN.Collection('endpoint');
  colle.ForAll(@_loopendpoints);
 end;

begin
 ob:=GFRE_DBI.CreateFromString(serviceobj.AsString,conn);
 if conn.Fetch(ob.Field('CAID').AsObjectLink,ca)=false then begin
  LogError('CA '+GFRE_BT.GUID_2_HexString(ob.Field('CAID').AsObjectLink)+' invalid in VPN '+ob.UID_String);
  exit;
 end;
 if conn.Fetch(ob.Field('CRTID').AsObjectLink,crt)=false then begin
  LogError('CRT '+GFRE_BT.GUID_2_HexString(ob.Field('CRTID').AsObjectLink)+' invalid in VPN '+ob.UID_String);
  exit;
 end;
 ob.Field('CA').AsString:=ca.Field('CRT').asstring;
 ob.Field('DH').AsString:=ca.Field('DH').asstring;
 ob.Field('CRL').AsString:=ca.Field('CRL').asstring;
 ob.Field('CRT').AsString:=crt.Field('crt').AsString;
 ob.Field('KEY').AsString:=crt.Field('KEY').AsString;
 _getsubnets(ob);
 result_obj.Field('VPN').AddObject(ob);

 ConfigureService(result_obj);
end;

procedure TFRE_HAL_VPN.RestartService(const name: string);
begin
 StopService(name);
 StartService(name);
end;



procedure TFRE_HAL_VPN.StartService(const name: string);
var cmd:string;
    vpndir:string;
    vpnname:string;
    AProcess: TProcess;
    i: integer;
begin
 exit;       //deactivated
 vpnname:=GFRE_BT.Str2HexStr(name);
 vpndir:=cvpnpath+'/'+vpnname;

 cmd:=copenvpn;
 cmd:=cmd +' --cd '+vpndir+' --daemon openvpn'+vpnname+' --config '+vpndir+'/server.conf --writepid '+vpndir+'/pid';
 writeln(cmd);
 {$IFDEF FREEBSD}
 AProcess := TProcess.Create(nil);
 try
  AProcess.CommandLine := cmd;
  AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
  writeln(AProcess.ExitStatus);
 finally
  AProcess.Free;
 end;
 {$ENDIF}
end;

procedure TFRE_HAL_VPN.StopService(const name: string);
var cmd     :string;
    vpndir  :string;
    vpnname :string;
    AProcess: TProcess;
    i       : integer;
    sl      : TStringlist;
begin
 exit;       //deactivated
 vpnname:=GFRE_BT.Str2HexStr(name);
 vpndir:=cvpnpath+'/'+vpnname;

 sl:=TStringList.Create;
 try
  sl.loadfromfile(vpndir+'/pid');
  cmd:='kill '+sl.text;
 finally
  sl.free;
 end;
 writeln(cmd);
 {$IFDEF FREEBSD}
 AProcess := TProcess.Create(nil);
 try
  AProcess.CommandLine := cmd;
  AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
  writeln(AProcess.ExitStatus);
 finally
  AProcess.Free;
 end;
 {$ENDIF}

end;

end.

