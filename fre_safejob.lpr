program fre_safejob;

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

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, sysutils,
  FRE_DB_INTERFACE,FRE_DB_EMBEDDED_IMPL,
  FOS_TOOL_INTERFACES, FRE_DB_CORE,
  FOS_DEFAULT_IMPLEMENTATION,fre_configuration,FRE_SYSTEM,
  fre_hal_schemes,
  FRE_ZFS,FRE_DBBUSINESS,
  fre_do_safejob, FRE_PROCESS,fre_testcase,
  FRE_DBBASE;

{$I fos_version_helper.inc}


begin
  Initialize_Read_FRE_CFG_Parameter;
  InitEmbedded;
  InitMinimal(false);

  FRE_DBBASE.Register_DB_Extensions;
  FRE_DBBUSINESS.Register_DB_Extensions;
  fre_testcase.Register_DB_Extensions;
  FRE_ZFS.Register_DB_Extensions;
  //FRE_DBCLOUDCONTROL.Register_DB_Extensions;

  //FRE_DB_Startup_Initialization_Complete;
  GFRE_DBI.LocalZone := 'Europe/Vienna';

  if ParamCount<>1 then begin
    writeln(GFOS_VHELP_GET_VERSION_STRING);
    writeln('Usage: fre_safejob <jobkey>');
  end else begin
    DO_SaveJob(uppercase(ParamStr(1)),cFRE_HAL_CFG_DIR+'jobs.dbo');
  end;
//  DO_SaveJob(TestIt.Field('TEST_JOB').AsObject);
  GFRE_LOG.Sync_Logger;
end.

