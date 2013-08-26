unit fre_hal_ca_interface;

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
  Classes, SysUtils, FRE_DB_INTERFACE;

type

  IFRE_HAL_CA_INTERFACE = interface (IFRE_DB_BASE)
   procedure CreateCertificateAuthority           (const  commonname,  country,  email,  state, organization, organizationunit: TFRE_DB_String;               const capassword : TFRE_DB_String; out certificate, key, dh, random : TFRE_DB_String; out camaintenanceobject: IFRE_DB_Object);
   procedure DeleteCertificateAuthority           (const  capassword : TFRE_DB_String ;  out certificaterevokationlist: TFRE_DB_String;                       const camaintenanceobject: IFRE_DB_Object);
   procedure CreateAndSignCertificate             (const  commonname,  country,  email,  state, organization, organizationunit, password: TFRE_DB_String;     const capassword : TFRE_DB_String; out certificate, key : TFRE_DB_String; const  camaintenanceobject: IFRE_DB_Object);
//   procedure RevokeCertificate;                   (const  certificate : TFRE_DB_String;  out certificaterevokationlist: TFRE_DB_String;                       const camaintenanceobject: IFRE_DB_Object);
  end;

implementation




end.

