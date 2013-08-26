unit fos_captiveportal_interface;

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
{$interfaces corba}

interface

uses
  Classes;

const
  cversioncaptiveportal = 'V 2.01';
  ccaptivelogtarget     = 'CAPTIVE';

type

  TFOSCaptiveportalResult =(cprOK,cprDENIED,cprINTERNALERROR,cprNOTDEFINED);

  RFOSCaptiveAccess = record
   requesturl   : string;
   redirecturl  : string;            // redirect to the following url
  end;

  { IFOS_CAPTIVEPORTAL_CONTROL }

  IFOS_CAPTIVEPORTAL_CONTROL=interface
   function EnableInternetAccessForIP         (const ip:string)                                            :TFOSCaptiveportalResult;
   function EnableInternetAccessForIPbyUser   (const ip:string;const username:string;const password:string):TFOSCaptiveportalResult;
   function EnableInternetAccessForIPbyOTP    (const ip:string;const otp:string)                           :TFOSCaptiveportalResult;
   function EnableInternetAccessForIPRedirect (const ip:string;const state:string;out url:string)          :TFOSCaptiveportalResult;
   function DisableInternetAccessForIP        (const ip:string)                                            :TFOSCaptiveportalResult;
   function DisableInternetAccessForUser      (const username:string)                                      :TFOSCaptiveportalResult;
   function DisableInternetAccessForIPRedirect(const ip:string;out url:string)                             :TFOSCaptiveportalResult;
   function GetLoginStateForIP                (const ip:string;const stateurl:string;out state:integer;out statetext:string)     :TFOSCaptiveportalResult;
  end;

  IFOS_CAPTIVEPORTAL_REDIREKTOR=interface
   function IsInternetAccessAllowedforIP    (const ip:string;var access:RFOSCaptiveAccess)             :TFOSCaptiveportalResult;    // internally checks timeout
  end;


var
   GFOS_CAPTIVEPORTAL_REDIRECTOR  : IFOS_CAPTIVEPORTAL_REDIREKTOR;
   GFOS_CAPTIVEPORTAL_CONTROL     : IFOS_CAPTIVEPORTAL_CONTROL;

implementation

end.

