unit fre_hal_transport;

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
  Classes, SysUtils,FOS_TOOL_INTERFACES,
  Process,

  FRE_HAL_UTILS,
  FRE_DB_COMMON,
  FRE_DB_INTERFACE;

  procedure Register_DB_Extensions;

type

  TFRE_DB_TRANSPORT = class (TFRE_DB_ObjectEx)
  end;

  TFRE_DB_HAL_TRANSPORT = class (TFRE_DB_TRANSPORT)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
  end;

  { TFRE_DB_HAL_TRANSPORT_RESPONCE_V10 }

  TFRE_DB_HAL_TRANSPORT_RESPONSE_V10 = class (TFRE_DB_TRANSPORT)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published

  end;

  { TFRE_DB_HAL_CA_TRANSPORT_RESPONSE_V10 }

  TFRE_DB_HAL_CA_TRANSPORT_RESPONSE_V10 = class (TFRE_DB_HAL_TRANSPORT_RESPONSE_V10)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published

  end;

  { TFRE_DB_HAL_CA_TRANSPORT_RESPONSE_CA_CREATION_V10 }

  TFRE_DB_HAL_CA_TRANSPORT_RESPONSE_CA_CREATION_V10 = class (TFRE_DB_HAL_TRANSPORT_RESPONSE_V10)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published

  end;

  { TFRE_DB_HAL_CA_TRANSPORT_V10 }

  TFRE_DB_HAL_CA_TRANSPORT_V10 = class (TFRE_DB_HAL_TRANSPORT)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
    function DB_GetDataForCACreation           (const input:IFRE_DB_Object):IFRE_DB_Object;
    function HAL_CreateCA                      (const input:IFRE_DB_Object):IFRE_DB_Object;
    //function DB_ResponseCACreation             (const input:IFRE_DB_Object):IFRE_DB_Object;

    function DB_GetDataforCertificatesCreation (const input:IFRE_DB_Object):IFRE_DB_Object;
    function HAL_CreateCertificates            (const input:IFRE_DB_Object):IFRE_DB_Object;
//    function DB_ResponseCertificateCreation    (const input:IFRE_DB_Object):IFRE_DB_Object;

    function DB_GetDataforCADeletion           (const input:IFRE_DB_Object):IFRE_DB_Object;
    function HAL_DeleteCA                      (const input:IFRE_DB_Object):IFRE_DB_Object;
//    function DB_ResponseCADeletion             (const input:IFRE_DB_Object):IFRE_DB_Object;

    function DB_GetDataforCertficateRevokation (const input:IFRE_DB_Object):IFRE_DB_Object;
    function HAL_RevokeCertificate             (const input:IFRE_DB_Object):IFRE_DB_Object;
//    function DB_ResponseCertificateRevokation  (const input:IFRE_DB_Object):IFRE_DB_Object;

  end;


  TFRE_DB_HAL_CERTIFICATE_TRANSPORT_V10 = class (TFRE_DB_TRANSPORT)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published

  end;


implementation

procedure Register_DB_Extensions;
var   enum      : IFRE_DB_Enum;
begin
  enum:=GFRE_DBI.NewEnum('hal_TRANSPORT_response_v10').Setup(GFRE_DBI.CreateText('$enum_hal_TRANSPORT_response_v10','HAL TRANSPORT Response'));
  enum.addEntry('failed',GFRE_DBI.CreateText('$enum_hal_TRANSPORT_response_v10_failed','failed'));
  enum.addEntry('ok',GFRE_DBI.CreateText('$enum_hal_TRANSPORT_response_v10_ok','ok'));
  enum.addEntry('warning',GFRE_DBI.CreateText('$enum_hal_TRANSPORT_response_v10_warning','warning'));
  GFRE_DBI.RegisterSysEnum(enum);

  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TRANSPORT);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_HAL_TRANSPORT_RESPONSE_V10);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_HAL_CA_TRANSPORT_RESPONSE_V10);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_HAL_CA_TRANSPORT_RESPONSE_CA_CREATION_V10);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_HAL_TRANSPORT);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_HAL_CERTIFICATE_TRANSPORT_V10);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_HAL_CA_TRANSPORT_V10);
  GFRE_DBI.Initialize_Extension_Objects;
end;

{ TFRE_DB_HAL_CA_TRANSPORT_RESPONSE_CA_CREATION_V10 }

class procedure TFRE_DB_HAL_CA_TRANSPORT_RESPONSE_CA_CREATION_V10.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
    scheme.SetParentSchemeByName('TFRE_DB_HAL_TRANSPORT_RESPONSE_V10');
end;

{ TFRE_DB_HAL_CA_TRANSPORT_RESPONSE_V10 }

class procedure TFRE_DB_HAL_CA_TRANSPORT_RESPONSE_V10.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_HAL_TRANSPORT_RESPONSE_V10');
  scheme.AddSchemeField('index',fdbft_String);
  scheme.AddSchemeField('index_attr',fdbft_String);
  scheme.AddSchemeField('crlnumber',fdbft_String);
  scheme.AddSchemeField('crl',fdbft_String);
end;


class procedure TFRE_DB_HAL_TRANSPORT_RESPONSE_V10.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);

  scheme.AddSchemeField('response',fdbft_String).SetupFieldDef(true,false,'hal_TRANSPORT_response_v10');
  scheme.AddSchemeField('responsetext',fdbft_String).multiValues:=true;

end;


class procedure TFRE_DB_HAL_TRANSPORT.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_TRANSPORT');
end;

{ TFRE_DB_HAL_CA_TRANSPORT_V10 }

class procedure TFRE_DB_HAL_CA_TRANSPORT_V10.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_HAL_TRANSPORT');
  scheme.AddSchemeField('commonname',fdbft_String).required:=true;
  scheme.AddSchemeField('country',fdbft_String);
  scheme.AddSchemeField('email',fdbft_String);
  scheme.AddSchemeField('state',fdbft_String);
  scheme.AddSchemeField('city',fdbft_String);
  scheme.AddSchemeField('location',fdbft_String);
  scheme.AddSchemeField('organization',fdbft_String);
  scheme.AddSchemeField('organizationunit',fdbft_String);
  scheme.AddSchemeField('password',fdbft_String).required:=true;

  scheme.AddSchemeField('certificate',fdbft_String);
  scheme.AddSchemeField('key',fdbft_String);
  scheme.AddSchemeField('dh',fdbft_String);
  scheme.AddSchemeField('random',fdbft_String);
  scheme.AddSchemeField('issued',fdbft_DateTimeUTC);
  scheme.AddSchemeField('revoked',fdbft_DateTimeUTC);
  scheme.AddSchemeField('valid',fdbft_DateTimeUTC);

  scheme.AddSchemeFieldSubscheme('certificates','TFRE_DB_HAL_CERTIFICATE_TRANSPORT_V10').multiValues:=true;
  scheme.AddSchemeFieldSubscheme('response','TFRE_DB_HAL_TRANSPORT_RESPONSE_V10');
  scheme.AddSchemeFieldSubscheme('responseCA','TFRE_DB_HAL_CA_TRANSPORT_RESPONSE_V10');
  scheme.AddSchemeFieldSubscheme('responseCACreation','TFRE_DB_HAL_CA_TRANSPORT_RESPONSE_CA_CREATION_V10');



end;

function TFRE_DB_HAL_CA_TRANSPORT_V10.DB_GetDataForCACreation(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

function TFRE_DB_HAL_CA_TRANSPORT_V10.HAL_CreateCA(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

function TFRE_DB_HAL_CA_TRANSPORT_V10.DB_GetDataforCertificatesCreation(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

function TFRE_DB_HAL_CA_TRANSPORT_V10.HAL_CreateCertificates(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;




function TFRE_DB_HAL_CA_TRANSPORT_V10.DB_GetDataforCADeletion(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

function TFRE_DB_HAL_CA_TRANSPORT_V10.HAL_DeleteCA(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;


function TFRE_DB_HAL_CA_TRANSPORT_V10.DB_GetDataforCertficateRevokation(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

function TFRE_DB_HAL_CA_TRANSPORT_V10.HAL_RevokeCertificate(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;


{ TFRE_DB_HAL_CERTIFICATE_TRANSPORT_V10 }

class procedure TFRE_DB_HAL_CERTIFICATE_TRANSPORT_V10.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_HAL_TRANSPORT');

  scheme.AddSchemeField('commonname',fdbft_String).required:=true;
  scheme.AddSchemeField('country',fdbft_String);
  scheme.AddSchemeField('email',fdbft_String);
  scheme.AddSchemeField('state',fdbft_String);
  scheme.AddSchemeField('city',fdbft_String);
  scheme.AddSchemeField('location',fdbft_String);
  scheme.AddSchemeField('organization',fdbft_String);
  scheme.AddSchemeField('organizationunit',fdbft_String);
  scheme.AddSchemeField('password',fdbft_String).required:=true;

  scheme.AddSchemeField('certificate',fdbft_String);
  scheme.AddSchemeField('key',fdbft_String);
  scheme.AddSchemeField('revoke',fdbft_Boolean);

  scheme.AddSchemeField('issued',fdbft_DateTimeUTC);
  scheme.AddSchemeField('revoked',fdbft_DateTimeUTC);
  scheme.AddSchemeField('valid',fdbft_DateTimeUTC);

end;


end.

