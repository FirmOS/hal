unit fre_certificate_common;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$codepage utf-8}

interface

uses
  Classes, SysUtils,
  FRE_DB_INTERFACE,
  FRE_DBBASE,
  fre_hal_schemes,
  fre_testcase,
  fre_certificate_app
  ;

implementation

procedure InitializeCertificateExtension(const dbname: string; const user, pass: string);
begin
end;

procedure CERTIFICATE_MetaRegister;
begin
  FRE_DBBASE.Register_DB_Extensions;
  fre_testcase.Register_DB_Extensions;
  fre_hal_schemes.Register_DB_Extensions;
  fre_certificate_app.Register_DB_Extensions;
end;

procedure CERTIFICATE_MetaInitializeDatabase(const dbname: string; const user, pass: string);
begin
end;

procedure CERTIFICATE_MetaRemove(const dbname: string; const user, pass: string);
begin
end;

initialization

GFRE_DBI_REG_EXTMGR.RegisterNewExtension('CERTIFICATE',@CERTIFICATE_MetaRegister,@CERTIFICATE_MetaInitializeDatabase,@CERTIFICATE_MetaRemove);

end.

