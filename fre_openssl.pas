unit fre_openssl;

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
  Classes, SysUtils,fre_process,fos_tool_interfaces;


const
  csslcnf    ='openssl.cnf';

  procedure FRE_OpenSSL_PrepareCADirectory (const dir:string);
  procedure FRE_OpenSSL_CreateCA           (const dir:string; const cn,c,st,l,o,ou,email:string; const ca_pass:string);
  procedure FRE_OpenSSL_CreateCrt          (const dir:string; const cn,c,st,l,o,ou,email:string; const ca_pass:string; const server:boolean);
  procedure FRE_OpenSSL_WriteConf          (const dir:string; const cn,c,st,l,o,ou,email:string);
  procedure FRE_OpenSSL_RevokeCrt          (const dir:string; const cn:string;const ca_pass:string);


implementation

procedure LogError(const msg:string);
begin
 raise Exception.Create('Exception raised: ERROR :'+msg);
end;

procedure LogInfo(const msg:string);
begin
 writeln('INFO:',msg);
end;

procedure CheckError(const resultcode: integer; const msg:string='');
begin
  if resultcode<>0 then begin
    raise Exception.Create('Exception raised: Resultcode:'+inttostr(resultcode)+' '+msg);
  end;
end;

procedure FRE_OpenSSL_PrepareCADirectory(const dir: string);
var
  dir_signed  : string;
  dir_private : string;

  procedure _Checkdir(const cdir:string);
  begin
    ForceDirectories(cdir);
    if not DirectoryExists(cdir) then begin
      raise Exception.Create('Could not create CA Dir '+cdir);
     end;
  end;

begin
 _CheckDir(dir);
 dir_signed :=dir+'/'+'signed_certs';
 dir_private:=dir+'/'+'private';
 _CheckDir(dir_signed);
 _CheckDir(dir_private);
 _CheckDir(dir+'/crl');
 CheckError(FRE_ProcessCMD('touch '+dir+'/'+'index.txt'));
end;

procedure FRE_OpenSSL_CreateCA(const dir: string; const cn, c, st, l, o, ou, email: string; const ca_pass: string);
var cadir : string;
    sl    : TStringList;
begin
  FRE_OpenSSL_WriteConf(dir,cn,c,st,l,o,ou,email);
  CheckError(FRE_ProcessCMD('openssl req -passout pass:'+ca_pass+' -new -keyout '+dir+'/private/ca.key -out '+dir+'/careq.pem -config '+dir+'/'+csslcnf));
  LogInfo('Sign CA Req ');
  CheckError(FRE_ProcessCMD('openssl ca -batch -passin pass:'+ca_pass+' -create_serial -out '+dir+'/ca.crt -keyfile '+dir+'/private/ca.key -selfsign -extensions v3_ca -in '+dir+'/careq.pem -config '+dir+'/'+csslcnf));
  CheckError(FRE_ProcessCMD('openssl x509 -inform PEM -outform DER -in '+dir+'/ca.crt -out '+dir+'/ca.der'));
  LogInfo('Create DH ');
  CheckError(FRE_ProcessCMD('openssl dhparam -out '+dir+'/dh1024.pem 1024'));
  CheckError(FRE_ProcessCMD('dd if=/dev/urandom of='+dir+'/random count=2'));
  GFRE_BT.StringToFile(dir+'/ca.pass',ca_pass);
  //Create CRL
  sl:=TStringList.Create;
  try
   sl.Add('00');
   sl.SaveToFile(dir+'/crlnumber');
  finally
   sl.Free;
  end;
  LogInfo('Create CA Done ');
  LogInfo('Write XP Extensions ');
  sl:=TStringList.Create;
  try
   sl.Add('[ xpclient_ext]');
   sl.Add('extendedKeyUsage = 1.3.6.1.5.5.7.3.2');
   sl.Add('[ xpserver_ext ]');
   sl.Add('extendedKeyUsage = 1.3.6.1.5.5.7.3.1');
   sl.SaveToFile(dir+'/xpxtensions');
  finally
   sl.Free;
  end;
end;

procedure FRE_OpenSSL_CreateCrt(const dir: string; const cn, c, st, l, o, ou, email: string; const ca_pass: string; const server: boolean);
begin
  LogInfo('Creating Crt '+cn);
  LogInfo('Creating SSL Conf '+cn);
  FRE_OpenSSL_WriteConf(dir,cn,c,st,l,o,ou,email);
  LogInfo('Creating Crt Req '+cn);

  CheckError(FRE_ProcessCMD('openssl req -nodes -new -keyout '+dir+'/private/'+cn+'.key -out '+dir+'/'+cn+'_req.pem -config '+dir+'/'+csslcnf));
  LogInfo('Sign Crt Req '+cn);
  if server=true then begin
   CheckError(FRE_ProcessCMD('openssl ca -verbose -batch -passin pass:'+ca_pass+' -out '+dir+'/signed_certs/'+cn+'.crt -extensions xpserver_ext -extfile '+dir+'/xpxtensions -in '+dir+'/'+cn+'_req.pem -config '+dir+'/'+csslcnf));
  end else begin
   CheckError(FRE_ProcessCMD('openssl ca -verbose -batch -passin pass:'+ca_pass+' -out '+dir+'/signed_certs/'+cn+'.crt -extensions xpclient_ext -extfile '+dir+'/xpxtensions -in '+dir+'/'+cn+'_req.pem -config '+dir+'/'+csslcnf));
  end;
//  CheckError(Command('openssl pkcs12 -export -passout pass:'+ob.Field('pass').AsString+' -clcerts -in '+cadir_signed+'/'+ob.Field('cn').asstring+'.crt -inkey '+cadir+'/private/'+ob.Field('cn').asstring+'.key -out '+cadir+'/private/'+ob.Field('cn').asstring+'.p12'));
  LogInfo('Create Crt Done '+cn);
end;

procedure FRE_OpenSSL_WriteConf (const dir:string; const cn,c,st,l,o,ou,email:string);
var lsl:TStringList;
begin
 lsl:=TStringList.Create;
 try

  lsl.Add('RANDFILE                = $ENV::HOME/.rnd');
  lsl.Add('oid_section             = new_oids');
  lsl.Add('[ new_oids ]');
  lsl.Add('[ ca ]');
  lsl.Add('default_ca      = CA_default            # The default ca section');
  lsl.Add('[ CA_default ]');
  lsl.Add('dir             = '+dir+'               # Where everything is kept');
  lsl.Add('certs           = $dir/                 # Where the issued certs are kept');
  lsl.Add('crl_dir         = $dir/crl              # Where the issued crl are kept');
  lsl.Add('database        = $dir/index.txt        # database index file.');
  lsl.Add('#unique_subject = no                    # Set to no to allow creation of');
  lsl.Add('new_certs_dir   = $dir/signed_certs     # default place for new certs.');
  lsl.Add('certificate     = $dir/ca.crt           # The CA certificate');
  lsl.Add('serial          = $dir/serial           # The current serial number');
  lsl.Add('crlnumber       = $dir/crlnumber        # the current crl number');
  lsl.Add('crl             = $dir/crl.pem          # The current CRL');
  lsl.Add('private_key     = $dir/private/ca.key   # The private key');
  lsl.Add('RANDFILE        = $dir/private/.rand    # private random number file');
  lsl.Add('x509_extensions = v3_ca                 # The extentions to add to the cert');
  lsl.Add('name_opt        = ca_default            # Subject Name options');
  lsl.Add('cert_opt        = ca_default            # Certificate field options');
  lsl.Add('default_days    = 3650                  # how long to certify for');
  lsl.Add('default_crl_days= 30                    # how long before next CRL');
  lsl.Add('default_md      = sha1                  # which md to use.');
  lsl.Add('preserve        = no                    # keep passed DN ordering');
  lsl.Add('policy          = policy_match');
  lsl.Add('[ policy_match ]');
  lsl.Add('countryName             = match');
  lsl.Add('stateOrProvinceName     = match');
  lsl.Add('organizationName        = match');
  lsl.Add('organizationalUnitName  = optional');
  lsl.Add('commonName              = supplied');
  lsl.Add('emailAddress            = optional');
  lsl.Add('[ policy_anything ]');
  lsl.Add('countryName             = optional');
  lsl.Add('stateOrProvinceName     = optional');
  lsl.Add('localityName            = optional');
  lsl.Add('organizationName        = optional');
  lsl.Add('organizationalUnitName  = optional');
  lsl.Add('commonName              = supplied');
  lsl.Add('emailAddress            = optional');
  lsl.Add('[ req ]');
  lsl.Add('default_bits            = 1024');
  lsl.Add('default_keyfile         = privkey.pem');
  lsl.Add('distinguished_name      = req_distinguished_name');
  lsl.Add('attributes              = req_attributes');
  lsl.Add('prompt = no');
  lsl.Add('x509_extensions = v3_ca # The extentions to add to the self signed cert');
  lsl.Add('string_mask = nombstr');
  lsl.Add('# req_extensions = v3_req # The extensions to add to a certificate request');
  lsl.Add('[ req_distinguished_name ]');
  if c<>''  then lsl.Add('C  = '+c);
  if st<>'' then lsl.Add('ST = '+st);
  if l<>''  then lsl.Add('L  = '+l);
  if o<>''  then lsl.Add('O  = '+o);
  if ou<>'' then lsl.Add('OU = '+ou);
  if cn<>'' then lsl.Add('CN = '+cn);
  if email<>''  then lsl.Add('emailAddress = '+email);
  lsl.Add('[ req_attributes ]');
  lsl.Add('challengePassword               = A challenge password');
  lsl.Add('[ usr_cert ]');
  lsl.Add('basicConstraints=CA:FALSE');
  lsl.Add('nsComment                       = "OpenSSL Generated Certificate"');
  lsl.Add('subjectKeyIdentifier=hash');
  lsl.Add('authorityKeyIdentifier=keyid,issuer');
  lsl.Add('[ v3_req ]');
  lsl.Add('basicConstraints = CA:FALSE');
  lsl.Add('keyUsage = nonRepudiation, digitalSignature, keyEncipherment');
  lsl.Add('[ v3_ca ]');
  lsl.Add('subjectKeyIdentifier=hash');
  lsl.Add('authorityKeyIdentifier=keyid:always,issuer:always');
  lsl.Add('basicConstraints = CA:true');
  lsl.Add('[ crl_ext ]');
  lsl.Add('authorityKeyIdentifier=keyid:always,issuer:always');

  lsl.SaveToFile(dir+'/'+csslcnf);
 finally
  lsl.Free;
 end;
end;

procedure FRE_OpenSSL_RevokeCrt(const dir: string; const cn: string; const ca_pass: string);
begin
 LogInfo('Revoke Crt '+cn);
 LogInfo('Creating SSL Conf '+cn);
 FRE_OpenSSL_WriteConf(dir,cn,'','','','','','');
 CheckError(FRE_ProcessCMD('openssl ca -passin pass:'+ca_pass+' -revoke '+dir+'/signed_certs/'+cn+'.crt -config '+dir+'/'+csslcnf));
 CheckError(FRE_ProcessCMD('openssl ca -passin pass:'+ca_pass+' -config '+dir+'/'+csslcnf+' -gencrl -crldays 365 -out '+dir+'/crl/ca.crl'));
 LogInfo('Revoke Crt Done '+cn);
end;


end.

