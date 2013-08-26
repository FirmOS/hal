unit fre_hal_ca;

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
  Classes, SysUtils,FOS_TOOL_INTERFACES,FRE_HAL_UTILS,FRE_DB_INTERFACE,process,fre_openssl;


const

   ccabasedir ='/fre/CA';
   csslcnf    ='openssl.cnf';

type

   { TFRE_HAL_CA }

   TFRE_HAL_CA = class (TFRE_HAL_DirectService)
   private
    function  Command          (const cmd:string):integer;
    procedure CheckError       (const resultcode:integer;const msg:string='');
    procedure ConfigureCA      (const caob:IFRE_DB_Object;const conn:IFRE_DB_Connection);
   public
    procedure ConfigureServiceDirect (const conn:IFRE_DB_Connection;const service_guid:TGUID); override;
    procedure ImportCA               (const conn:IFRE_DB_Connection;const service_guid:TGUID;const name:string);
   end;


implementation

{ TFRE_HAL_CA }

function TFRE_HAL_CA.Command(const cmd: string): integer;
var AProcess:TProcess;
begin
 writeln('CMD:'+cmd);
 AProcess := TProcess.Create(nil);
 try
  AProcess.CommandLine := cmd;
  AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
  result:=Aprocess.ExitStatus;
  if result<>0 then begin
   LogError('Error on cmd:'+cmd+' Result:'+inttostr(result));
  end;
 finally
  AProcess.Free;
 end;
end;

procedure TFRE_HAL_CA.CheckError(const resultcode: integer; const msg: string);
begin
 if resultcode<>0 then begin
  raise Exception.Create('Exception raised: Resultcode:'+inttostr(resultcode)+' '+msg);
 end;
end;


procedure TFRE_HAL_CA.ConfigureCA(const caob: IFRE_DB_Object; const conn: IFRE_DB_Connection);
var cadir         :string;
    caname        :string;
    cadir_signed  :string;
    cadir_private :string;
    lsl           :string;
    i             :integer;
    cob           :IFRE_DB_Object;
    fupdatecrl    :boolean;
    referenced    :TFRE_DB_GUIDArray;

    procedure _Checkdir(const dir:string);
    begin
     ForceDirectories(dir);
     if not DirectoryExists(dir) then begin
      GFRE_BT.CriticalAbort('Could not create CA Dir [%s] for CA [%s]',[dir,caname]);
     end;
    end;

    procedure _UpdateCADB;

     procedure __updateFile(const pn,fn:string);
     begin
      if fileexists(cadir+'/'+fn) then begin
       caob.Field(pn).AsString:=GFRE_BT.StringFromFile(cadir+'/'+fn);
      end;
     end;

    begin
     __updateFile('index','index.txt');
     __updateFile('index_attr','index.txt.attr');
     __updateFile('serial','serial');
     __updateFile('crlnumber','crlnumber');
     __updateFile('crl','crl/ca.crl');
     conn.Update(caob);
    end;

    procedure _updateCrl;
    begin
     CheckError(Command('openssl ca -passin pass:'+caob.Field('pass').asstring+' -config '+cadir+'/'+csslcnf+' -gencrl -crldays 365 -out '+cadir+'/crl/ca.crl'));
    end;

    procedure _CreateCA;
    var sl:TStringList;
    begin
     LogInfo('Creating CA '+caname);
     if FileExists(cadir+'/'+'index.txt') then begin
  //    writeln(caob.DumpToString());
        GFRE_BT.CriticalAbort('Could not create CA, index.txt exists CA [%s]',[caname]);

     end;
     CheckError(Command('touch '+cadir+'/'+'index.txt'));
//     writeln('Pass:'+caob.Field('pass').asstring);
     LogInfo('Creating SSL Conf '+caname);
     FRE_OpenSSL_WriteConf(cadir,caob.Field('CN').AsString, caob.Field('C').AsString, caob.Field('ST').AsString,caob.Field('L').AsString,caob.Field('O').AsString,caob.Field('OU').AsString,caob.Field('EMAIL').AsString);
     LogInfo('Creating CA Req '+caname);
     CheckError(Command('openssl req -passout pass:'+caob.Field('pass').asstring+' -new -keyout '+cadir+'/private/cakey.pem -out '+cadir+'/careq.pem -config '+cadir+'/'+csslcnf));
     LogInfo('Sign CA Req '+caname);
     CheckError(Command('openssl ca -batch -passin pass:'+caob.Field('pass').asstring+' -create_serial -out '+cadir+'/cacert.pem -keyfile '+cadir+'/private/cakey.pem -selfsign -extensions v3_ca -in '+cadir+'/careq.pem -config '+cadir+'/'+csslcnf));
     CheckError(Command('openssl x509 -inform PEM -outform DER -in '+cadir+'/cacert.pem -out '+cadir+'/cacert.der'));
     LogInfo('Create DH '+caname);
     CheckError(Command('openssl dhparam -out '+cadir+'/dh1024.pem 1024'));
     caob.Field('crt').AsString:=GFRE_BT.StringFromFile(cadir+'/cacert.pem');
     caob.Field('key').AsString:=GFRE_BT.StringFromFile(cadir+'/private/cakey.pem');
     caob.Field('dh').AsString:=GFRE_BT.StringFromFile(cadir+'/dh1024.pem');
     CheckError(Command('dd if=/dev/urandom of='+cadir+'/random count=2'));
     caob.Field('random').AsString:=GFRE_BT.StringFromFile(cadir+'/random');
     caob.Field('issued').AsDateTimeUTC:=GFRE_DT.Now_UTC;
     GFRE_BT.StringToFile(cadir+'/ca.pass',caob.Field('pass').asstring);
     //Create CRL
     sl:=TStringList.Create;
     try
      sl.Add('00');
      sl.SaveToFile(cadir+'/crlnumber');
     finally
      sl.Free;
     end;
     fupdatecrl:=true;
     LogInfo('Create CA Done '+caname);
//     writeln(caob.DumpToString());
     LogInfo('Write XP Extensions '+caname);
     sl:=TStringList.Create;
     try
      sl.Add('[ xpclient_ext]');
      sl.Add('extendedKeyUsage = 1.3.6.1.5.5.7.3.2');
      sl.Add('[ xpserver_ext ]');
      sl.Add('extendedKeyUsage = 1.3.6.1.5.5.7.3.1');
      sl.SaveToFile(cadir+'/xpxtensions');
     finally
      sl.Free;
     end;
    end;

    procedure _CreateCrt(const ob:IFRE_DB_Object);
    begin
     LogInfo('Creating Crt '+caname+' '+ob.Field('cn').AsString);
     LogInfo('Creating SSL Conf '+caname+' '+ob.Field('cn').AsString);
     FRE_OpenSSL_WriteConf(cadir,ob.Field('CN').AsString, ob.Field('C').AsString, ob.Field('ST').AsString,ob.Field('L').AsString,ob.Field('O').AsString,ob.Field('OU').AsString,ob.Field('EMAIL').AsString);
     LogInfo('Creating Crt Req '+caname);
     if ob.FieldExists('pass')=false then begin
      ob.Field('pass').AsString:=ob.Field('uid').AsString;
     end;
      //     CheckError(Command('openssl req -passout pass:'+ob.Field('pass').asstring+' -new -keyout '+cadir+'/private/'+GFRE_BT.Str2HexStr(ob.Field('cn').asstring)+'.pem -out '+cadir+'/'+GFRE_BT.Str2HexStr(ob.Field('cn').asstring)+'_req.pem -config '+cadir+'/'+csslcnf));
     CheckError(Command('openssl req -nodes -new -keyout '+cadir+'/private/'+ob.Field('cn').asstring+'.key -out '+cadir+'/'+GFRE_BT.Str2HexStr(ob.Field('cn').asstring)+'_req.pem -config '+cadir+'/'+csslcnf));
     LogInfo('Sign Crt Req '+caname);
     if ob.Field('server').AsBoolean=true then begin
      CheckError(Command('openssl ca -verbose -batch -passin pass:'+caob.Field('pass').asstring+' -out '+cadir_signed+'/'+ob.Field('cn').asstring+'.crt -extensions xpserver_ext -extfile '+cadir+'/xpxtensions -in '+cadir+'/'+GFRE_BT.Str2HexStr(ob.Field('cn').asstring)+'_req.pem -config '+cadir+'/'+csslcnf));
     end else begin
      CheckError(Command('openssl ca -verbose -batch -passin pass:'+caob.Field('pass').asstring+' -out '+cadir_signed+'/'+ob.Field('cn').asstring+'.crt -extensions xpclient_ext -extfile '+cadir+'/xpxtensions -in '+cadir+'/'+GFRE_BT.Str2HexStr(ob.Field('cn').asstring)+'_req.pem -config '+cadir+'/'+csslcnf));
     end;
//     CheckError(Command('openssl pkcs12 -export -passout pass:pk1234 -certfile '+cadir+'/cacert.pem -in '+cadir+'/'+GFRE_BT.Str2HexStr(ob.Field('cn').asstring)+'_cert.pem -inkey '+cadir+'/private/'+GFRE_BT.Str2HexStr(ob.Field('cn').asstring)+'.pem -out '+cadir+'/'+GFRE_BT.Str2HexStr(ob.Field('cn').asstring)+'_ca.p12'));
     CheckError(Command('openssl pkcs12 -export -passout pass:'+ob.Field('pass').AsString+' -clcerts -in '+cadir_signed+'/'+ob.Field('cn').asstring+'.crt -inkey '+cadir+'/private/'+ob.Field('cn').asstring+'.key -out '+cadir+'/private/'+ob.Field('cn').asstring+'.p12'));
     ob.Field('crt').AsString:=GFRE_BT.StringFromFile(cadir_signed+'/'+ob.Field('cn').asstring+'.crt');
     ob.Field('key').AsString:=GFRE_BT.StringFromFile(cadir+'/private'+'/'+ob.Field('cn').asstring+'.key');
     ob.Field('p12').AsString:=GFRE_BT.StringFromFile(cadir+'/private'+'/'+ob.Field('cn').asstring+'.p12');
     ob.Field('issued').AsDateTimeUTC:=GFRE_DT.Now_UTC;
     GFRE_BT.StringToFile(cadir+'/'+GFRE_BT.Str2HexStr(ob.Field('cn').asstring)+'.pass',ob.Field('pass').asstring);
     LogInfo('Create Crt Done '+caname);
 //    writeln(ob.DumpToString());
    end;

    procedure _RevokeCrt(const ob:IFRE_DB_Object);
    begin
     LogInfo('Creating Crt '+caname+' '+ob.Field('cn').AsString);
     LogInfo('Creating SSL Conf '+caname+' '+ob.Field('cn').AsString);
     FRE_OpenSSL_WriteConf(cadir,ob.Field('CN').AsString, ob.Field('C').AsString, ob.Field('ST').AsString,ob.Field('L').AsString,ob.Field('O').AsString,ob.Field('OU').AsString,ob.Field('EMAIL').AsString);
     GFRE_BT.StringToFile(cadir+'/revokecert.pem',ob.Field('crt').AsString);
     CheckError(Command('openssl ca -passin pass:'+caob.Field('pass').asstring+' -revoke '+cadir+'/revokecert.pem -config '+cadir+'/'+csslcnf));
     ob.Field('revoked').AsDateTimeUTC:=GFRE_DT.Now_UTC;
     LogInfo('Revoke Crt Done '+caname);
     fupdatecrl:=true;
 //    writeln(ob.DumpToString());
    end;


begin
//  writeln(caob.DumpToString());
  fupdatecrl:=false;
  caname:=GFRE_BT.Str2HexStr(caob.Field('objname').asstring);
  cadir:=ccabasedir+'/'+caname;
  _CheckDir(cadir);
  writeln('CADIR:',cadir);
  cadir_signed :=cadir+'/'+'signed_certs';
  cadir_private:=cadir+'/'+'private';
  _CheckDir(cadir_signed);
  _CheckDir(cadir_private);
  _CheckDir(cadir+'/crl');
  if caob.FieldExists('crt')=false then begin
   _CreateCA;
  end;
  referenced:=caob.ReferencedByList;
  writeln('REFERENCED ',length(referenced));
  for i:=0 to length(referenced)-1 do begin
   if conn.Fetch(referenced[i],cob)=false then begin
    LogError('Certificate '+ GFRE_BT.GUID_2_HexString(referenced[i])+' in ca '+GFRE_BT.GUID_2_HexString(caob.UID)+' missing !');
   end else begin
    if cob.SchemeClass='TFRE_DB_CERTIFICATE' then begin
     if cob.FieldExists('crt')=false then begin
      _CreateCrt(cob);
      if cob.FieldExists('revoke') then begin
       if cob.FieldExists('revoked')=false then begin
        _RevokeCrt(cob);
       end;
      end;
//     cob.SaveToFile(cadir+'/'+GFRE_BT.Str2HexStr(cob.Field('cn').asstring)+'.dbo');
      conn.Update(cob);
     end;
    end;
   end;
  end;
  if fupdatecrl then begin
   _updateCrl;
  end;
  _UpdateCADB;
  setlength(referenced,0);
//  caob.SaveToFile(cadir+'/ca.dbo');
end;

procedure TFRE_HAL_CA.ConfigureServiceDirect(const conn: IFRE_DB_Connection; const service_guid: TGUID);
var coll  :IFRE_DB_Collection;

  procedure _find(const obj:IFRE_DB_object);
  var i:integer;
  begin
//    writeln('service ',obj.SchemeClass,' ',GFRE_BT.GUID_2_HexString(service_guid),' ',GFRE_BT.GUID_2_HexString(obj.Field('servicegroup').asobjectlink));
    if FREDB_Guids_Same(obj.Field('servicegroup').AsObjectLink,service_guid) then begin
     if obj.SchemeClass='TFRE_DB_CA' then begin
      if obj.FieldExists('pass')=false then begin
       obj.Field('pass').asstring:=obj.Field('UID').AsString;
      end;
      ConfigureCA(obj,conn);
     end;
    end;
  end;

begin
 coll:=Conn.Collection('service');
 coll.ForAll(@_find);
end;

procedure TFRE_HAL_CA.ImportCA(const conn: IFRE_DB_Connection; const service_guid: TGUID; const name: string);
var caname:string;
    cadir :string;
    coll  :IFRE_DB_Collection;
    ca    :IFRE_DB_Object;

   function _find(const obj:IFRE_DB_object):boolean;
   begin
    if FREDB_Guids_Same(obj.Field('servicegroup').AsObjectLink,service_guid) then begin
      if obj.IsA('TFRE_DB_CA') then begin
      end;
    end;
  end;


begin
 exit; // not implemented now
 caname:=GFRE_BT.Str2HexStr(name);
 cadir:=ccabasedir+'/'+caname;

 coll:=Conn.Collection('service');
 coll.ForAllBreak(@_find);


end;


end.

