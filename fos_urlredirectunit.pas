unit fos_urlredirectunit;

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
  Classes, SysUtils,FOS_CAPTIVEPORTALHAL,FOS_CAPTIVEPORTAL_INTERFACE,FOS_TOOL_INTERFACES;

type

  { TFOS_REdirector }

  TFOS_Redirector=class(TObject)
  private
   sl:TStringList;
  public
   constructor   Create;
   function      Process(const inputs:string):string;
   destructor    Destroy; override;
  end;

implementation

{ TFOS_REdirector }

constructor TFOS_REdirector.Create;
begin
 sl:=TStringList.Create;
 sl.Delimiter:=' ';
end;


function TFOS_Redirector.Process(const inputs: string): string;
var ip:string;
    fdn:string;
    url:string;
    i:integer;
    j:integer;
    ar  : RFOSCaptiveAccess;
    res : TFOSCaptiveportalResult;
begin
 sl.DelimitedText:=inputs;
 result:='302:http://wrong.redirection.parameter.org';
 if sl.count=0 then exit;
 url:=sl[0];
 if sl.count=7 then begin
  ip:=Copy(sl[1],1,Pos('/',sl[1])-1);
  ar.requesturl:=url;
  res:=GFOS_CAPTIVEPORTAL_REDIRECTOR.IsInternetAccessAllowedforIP(ip,ar);
  case res of
   cprOK: begin
    result:=url;
    GFRE_LOG.Log ('REDIRECTOR: IP:'+ip+' OK:'+url,'',fll_Debug,ccaptivelogtarget);
   end;
   cprDENIED: begin
    result:='302:'+ar.redirecturl;
    GFRE_LOG.Log ('REDIRECTOR: IP:'+ip+' DENIED:'+url+' => '+ ar.redirecturl,'',fll_Debug,ccaptivelogtarget);
   end;
   cprNOTDEFINED: begin
    result:='302:http://your.ip.is.not.allowed.org';
    GFRE_LOG.Log ('REDIRECTOR: IP:'+ip+' NOT DEFINED:'+url,'',fll_Warning,ccaptivelogtarget);
   end;
  end;
 end else begin
  GFRE_LOG.Log ('REDIRECTOR: WRONG PARAMETERS '+sl.DelimitedText,'',fll_Warning,ccaptivelogtarget);
 end;
end;

destructor TFOS_REdirector.Destroy;
begin
 sl.free;
 inherited Destroy;
end;

end.

