unit fre_hal_radius;

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


   cradius_dir='/fre/radius';
   cradius_users_file='users';
   cradius_eap_file='eap.conf';
   cradius_clients='clients.conf';

  type

   { TFRE_HAL_RADIUS }

   TFRE_HAL_RADIUS = class (TFRE_HAL_Service)
   private
    procedure WriteUsers   (const hal_object:IFRE_DB_Object);
    procedure WriteClients (const hal_object:IFRE_DB_Object);
    procedure WriteEAP     (const hal_object:IFRE_DB_Object);
   protected
    function  ServiceSchemeName : string; override;
   public
    function  ConfigureService(const hal_object:IFRE_DB_Object) :IFRE_DB_Object; override;
    procedure GetServiceHAL   (const conn: IFRE_DB_Connection;const serviceobj: IFRE_DB_Object; const result_obj: IFRE_DB_Object); override;
   end;



implementation

{ TFRE_HAL_RADIUS }

procedure TFRE_HAL_RADIUS.WriteUsers(const hal_object: IFRE_DB_Object);
var sl:TStringList;
     i:integer;
begin
 sl:=TStringList.Create;
 try
   for i:=0 to hal_object.Field('USER').ValueCount-1  do begin
    sl.Add(hal_object.Field('USER').AsStringItem[i] +#9+' Auth-type := EAP');
   end;
   sl.Add('DEFAULT    Auth-type := Reject');
   sl.Add('           Reply-Message := "You are not allowed here !"');
   sl.SaveToFile(cradius_dir+'/'+cradius_users_file);
 finally
  sl.Free;
 end;
end;

procedure TFRE_HAL_RADIUS.WriteClients(const hal_object: IFRE_DB_Object);
var sl:TStringList;
     i:integer;
    cl:IFRE_DB_Object;
begin
 sl:=TStringList.Create;
 try
  sl.add('client localhost {');
  sl.add('      ipaddr = 127.0.0.1');
 sl.Add('       secret	= testing123');
 sl.add('       require_message_authenticator = no');
 sl.add('	shortname = localhost');
 sl.Add('       nastype = other');
 sl.Add('}');
 for i:=0 to hal_object.Field('clients').ValueCount-1 do begin
  cl:=hal_object.Field('clients').AsObjectItem[i];
  sl.Add('client '+cl.Field('IP_NET').AsString+' {');
  sl.Add(#9+'secret ='+cl.Field('secret').AsString);
  sl.add(#9+'shortname ='+cl.Field('shortname').AsString);
  sl.add(#9+'nastype ='+cl.Field('nastype').AsString);
  sl.Add('}');
 end;
  sl.SaveToFile(cradius_dir+'/'+cradius_clients);
 finally
  sl.Free;
 end;
end;

procedure TFRE_HAL_RADIUS.WriteEAP(const hal_object: IFRE_DB_Object);
var sl:TStringList;
begin
// GFRE_BT.StringToFile(cradius_dir+'/ca.pem',hal_object.Field('CA').AsString);
 GFRE_BT.StringToFile(cradius_dir+'/dh1024.pem',hal_object.Field('DH').AsString);
 GFRE_BT.StringToFile(cradius_dir+'/crt.pem',hal_object.Field('CRT').AsString);
 GFRE_BT.StringToFile(cradius_dir+'/key.pem',hal_object.Field('KEY').AsString);
// GFRE_BT.StringToFile(cradius_dir+'/crl.pem',hal_object.Field('CRL').AsString);
 GFRE_BT.StringToFile(cradius_dir+'/random',hal_object.Field('RANDOM').AsString);
 GFRE_BT.StringToFile(cradius_dir+'/ca_crl.pem',hal_object.Field('CA').AsString+#13#10+hal_object.Field('CRL').AsString);
 sl:=TStringList.Create;
 try
  sl.add('eap {');
  sl.add('	default_eap_type = tls');
  sl.add('	timer_expire     = 60');
  sl.add('	ignore_unknown_eap_types = no');
  sl.add('	cisco_accounting_username_bug = no');
  sl.add('	max_sessions = 4096');
  sl.add('	md5 {');
  sl.add('	}');
  sl.add('	leap {');
  sl.Add('	}');
  sl.Add('	gtc {');
//  sl.add('      	auth_type = PAP');
  sl.add('	}');
  sl.add('	tls {');
  sl.add('		certdir = '+cradius_dir);
  sl.add('		cadir = ${certdir}');
  sl.add('		private_key_password = '+hal_object.Field('Pass').asstring);
  sl.add('		private_key_file = ${certdir}/key.pem');
  sl.add('		certificate_file = ${certdir}/crt.pem');
  sl.add('		CA_file = ${cadir}/ca_crl.pem');
  sl.add('		dh_file = ${certdir}/dh1024.pem');
  sl.add('		random_file = ${certdir}/random');
		//#	fragment_size = 1024
		//#	include_length = yes
  sl.add('              check_crl = yes');
  sl.Add('		CA_path = ${cadir}');
  //		#	check_cert_cn = %{User-Name}
		//#
  sl.add('		cipher_list = "DEFAULT"');
  sl.add('		cache {');
  sl.add('                    enable = no');
  sl.add('		      lifetime = 24 # hours');
  sl.add('		      max_entries = 255');
  sl.add('		}');

//  sl.add('		verify {');
//  sl.add('	#     		tmpdir = /tmp/radiusd');
//		#    		client = "/path/to/openssl verify -CApath ${..CA_path} %{TLS-Client-Cert-Filename}"
//  sl.add('		}');
  sl.add('	}');
  sl.add('}');
  sl.SaveToFile(cradius_dir+'/'+cradius_eap_file);
 finally
  sl.Free;
 end;
end;

function TFRE_HAL_RADIUS.ServiceSchemeName: string;
begin
  Result:='TFRE_DB_RADIUS';
end;

function TFRE_HAL_RADIUS.ConfigureService(const hal_object: IFRE_DB_Object): IFRE_DB_Object;
begin
  ForceDirectories(cradius_dir);
  if not DirectoryExists(cradius_dir) then begin
   GFRE_BT.CriticalAbort('Could not create Radis Dir [%s] ',[cradius_dir]);
  end;
  WriteUsers(hal_object);
  WriteClients(hal_object);
  WriteEAP(hal_object);
  RestartService('radiusd');
end;

procedure TFRE_HAL_RADIUS.GetServiceHAL(const conn: IFRE_DB_Connection; const serviceobj: IFRE_DB_Object; const result_obj: IFRE_DB_Object);
var ca    :IFRE_DB_Object;
    crt   :IFRE_DB_Object;
      i   :integer;
      obj :IFRE_DB_Object;

 procedure _addUser;
 var coll  :IFRE_DB_Collection;

   procedure _findUser(const obj:IFRE_DB_object);
   begin
    if FREDB_Guids_Same(obj.Field('ca').AsObjectLink,ca.uid) then begin
     if not FREDB_Guids_Same(obj.UID,crt.UID) then begin // not the server certificate
      if obj.FieldExists('revoked')=false then begin
       result_obj.Field('user').AddString(obj.field('cn').asstring);
      end;
     end;
    end;
   end;

 begin
  coll:=Conn.Collection('certificate');
  coll.ForAll(@_findUser);
 end;

begin
  for i:=0 to serviceobj.Field('clients').ValueCount-1 do begin
   obj:=GFRE_DBI.CreateFromString(serviceobj.Field('clients').AsObjectItem[i].AsString);
   result_obj.FIeld('clients').AddObject(obj);
  end;

  if conn.Fetch(serviceobj.Field('CAID').AsObjectLink,ca)<>edb_OK then begin
   LogError('CA '+GFRE_BT.GUID_2_HexString(serviceobj.Field('CAID').AsObjectLink)+' invalid in RADIUS '+serviceobj.UID_String);
   exit;
  end;
  if conn.Fetch(serviceobj.Field('CRTID').AsObjectLink,crt)<>edb_OK then begin
   LogError('CRT '+GFRE_BT.GUID_2_HexString(serviceobj.Field('CRTID').AsObjectLink)+' invalid in RADIUS '+serviceobj.UID_String);
   exit;
  end;

  result_obj.Field('CA').AsString:=ca.Field('CRT').asstring;
  result_obj.Field('DH').AsString:=ca.Field('DH').asstring;
  result_obj.Field('CRL').AsString:=ca.Field('CRL').asstring;
  result_obj.Field('RANDOM').AsString:=ca.Field('RANDOM').asstring;
  result_obj.Field('CRT').AsString:=crt.Field('crt').AsString;
  result_obj.Field('PASS').AsString:=crt.Field('pass').AsString;
  result_obj.Field('KEY').AsString:=crt.Field('KEY').AsString;
  _addUser;
end;

end.






