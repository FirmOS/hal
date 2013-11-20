program test_hal;

{$mode objfpc}
{$H+}
{$codepage utf8}
{$LIBRARYPATH ../../fre_external/fre_ext_libs}


uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  cmem,
  FRE_SYSTEM,FRE_CONFIGURATION,math,
  consoletestrunner,
  FOS_DEFAULT_IMPLEMENTATION,
  FRE_DB_CORE,FRE_DB_INTERFACE,FRE_dbbase,
  FOS_TOOL_INTERFACES,
  sysutils,fre_hal_testsuite,
  fre_db_embedded_impl,
  FRE_ZFS;

var App: TTestRunner;

begin
  Initialize_Read_FRE_CFG_Parameter;

  InitEmbedded;
  Init4Server;
  GFRE_DBI.SetLocalZone('Europe/Vienna');
  fre_dbbase.Register_DB_Extensions;
  fre_zfs.Register_DB_Extensions;


  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FirmOS FRE HAL Testsuite';
  App.Run;
  App.Free;
end.

