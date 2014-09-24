unit fre_certificate_app;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$codepage utf-8}

interface

uses
  Classes, SysUtils,
  FOS_TOOL_INTERFACES,
  FRE_DB_INTERFACE,
  FRE_DB_COMMON,
  FRE_DBBASE,
  fre_hal_schemes,
  fre_testcase,
  fre_openssl_interface
  ;

type

  { TFRE_CERTIFICATE_APP }

  TFRE_CERTIFICATE_APP=class(TFRE_DB_APPLICATION)
  private
    procedure       SetupApplicationStructure     ; override;
    procedure       _UpdateSitemap                (const session: TFRE_DB_UserSession);
  protected
    procedure       MySessionInitialize           (const session: TFRE_DB_UserSession); override;
    procedure       MySessionPromotion            (const session: TFRE_DB_UserSession); override;
    procedure       MyServerInitialize            (const admin_dbc: IFRE_DB_CONNECTION); override;
  public
    class procedure RegisterSystemScheme          (const scheme:IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects              (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure InstallDBObjects4Domain       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TFRE_DB_GUID); override;
    class procedure InstallUserDBObjects4Domain   (const conn: IFRE_DB_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TFRE_DB_GUID); override;
  end;

  { TFRE_CERTIFICATE_CA_MOD }

  TFRE_CERTIFICATE_CA_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme        (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure     ; override;
    procedure       MySessionInitializeModule   (const session: TFRE_DB_UserSession); override;
  public
    class procedure InstallDBObjects                   (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function        WEB_Content                        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_CertificatesContent            (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_CAContent                      (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_CAMenu                         (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_CrtContent                     (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_CrtMenu                        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_Menu                           (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_addCertificateAuthority        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_newCertificateAuthority        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_addImportCertificateAuthority  (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_importCertificateAuthority     (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_delCertificateAuthority        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_delCertificateAuthorityConfirmed(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_addCertificate                 (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_newCertificate                 (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_revokeCertificate              (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_backupCertificateAuthority     (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_restoreCertificateAuthority    (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;

  end;



procedure Register_DB_Extensions;

implementation

{ TFRE_CERTIFICATE_CA_MOD }

class procedure TFRE_CERTIFICATE_CA_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_CERTIFICATE_CA_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('cert_description');
end;

procedure TFRE_CERTIFICATE_CA_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var  dc_ca     : IFRE_DB_DERIVED_COLLECTION;
     dc_crt    : IFRE_DB_DERIVED_COLLECTION;
     ca_grid   : IFRE_DB_SIMPLE_TRANSFORM;
     crt_grid  : IFRE_DB_SIMPLE_TRANSFORM;
     app       : TFRE_DB_APPLICATION;
     conn      : IFRE_DB_CONNECTION;
begin
  inherited;
  if session.IsInteractiveSession then begin
    app  := GetEmbeddingApp;
    conn := session.GetDBConnection;
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,ca_Grid);
     with ca_grid do begin
       AddOneToOnescheme('objname','objname',app.FetchAppTextShort(session,'ca_name'));
     end;
    DC_CA := session.NewDerivedCollection('ca_grid');
    with DC_CA do begin
      SetDeriveParent           (conn.GetCollection(CFRE_DB_CA_COLLECTION));
      SetDeriveTransformation   (ca_Grid);
      SetDisplayType            (cdt_Listview,[],'',nil,'',CWSF(@WEB_CAMenu),nil,CWSF(@WEB_CAContent));
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,crt_Grid);
    with crt_Grid do begin
      AddOneToOnescheme('objname','objname',app.FetchAppTextShort(session,'crt_cn'));
      AddOneToOnescheme('email','email',app.FetchAppTextShort(session,'crt_email'));
      AddOneToOnescheme('issued','issued',app.FetchAppTextShort(session,'crt_issued'),dt_date);
      AddOneToOnescheme('revoked','revoked',app.FetchAppTextShort(session,'crt_revoked'),dt_date);
    end;
    dc_crt := session.NewDerivedCollection('crt_grid');
    with dc_crt do begin
      SetDeriveParent(conn.GetCollection(CFRE_DB_CERTIFICATE_COLLECTION));
      SetUseDependencyAsRefLinkFilter(['TFRE_DB_CERTIFICATE<CA'],false);
      SetDeriveTransformation(crt_Grid);
      SetDisplayType(cdt_Listview,[],'',nil,'',CWSF(@WEB_CrtMenu),nil,CWSF(@WEB_CrtContent));
    end;
  end;
end;

class procedure TFRE_CERTIFICATE_CA_MOD.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  dc_ca               : IFRE_DB_DERIVED_COLLECTION;
  dc_crt              : IFRE_DB_DERIVED_COLLECTION;
  grid_ca             : TFRE_DB_VIEW_LIST_DESC;
  ca                  : TFRE_DB_LAYOUT_DESC;
  sub_sec_ca          : TFRE_DB_SUBSECTIONS_DESC;
  txt                 : IFRE_DB_TEXT;

begin
  CheckClassVisibility4MyDomain(ses);

  dc_ca        := ses.FetchDerivedCollection('ca_grid');
  grid_ca      := dc_ca.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  if conn.sys.CheckClassRight4MyDomain(sr_STORE,TFRE_DB_CA) then begin
    txt:=app.FetchAppTextFull(ses,'create_ca');
    grid_ca.AddButton.Describe(CWSF(@WEB_addCertificateAuthority),'images_apps/certificate/create_ca.png',txt.Getshort,txt.GetHint);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4MyDomain(sr_STORE,TFRE_DB_CA) then begin
    txt:=app.FetchAppTextFull(ses,'import_ca');
    grid_ca.AddButton.Describe(CWSF(@WEB_addimportCertificateAuthority),'images_apps/certificate/import_ca.png',txt.Getshort,txt.GetHint);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4MyDomain(sr_DELETE,TFRE_DB_CA) then begin
    txt:=app.FetchAppTextFull(ses,'delete_ca');
    grid_ca.AddButton.Describe(CWSF(@WEB_DelCertificateAuthority),'images_apps/certificate/delete_ca.png',txt.Getshort,txt.GetHint);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4MyDomain(sr_STORE,TFRE_DB_CA) then begin
    txt:=app.FetchAppTextFull(ses,'backup_ca');
    grid_ca.AddButton.Describe(CWSF(@WEB_backupCertificateAuthority),'images_apps/certificate/backup_ca.png',txt.Getshort,txt.GetHint);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4MyDomain(sr_STORE,TFRE_DB_CA) then begin
    txt:=app.FetchAppTextFull(ses,'restore_ca');
    grid_ca.AddButton.Describe(CWSF(@WEB_restoreCertificateAuthority),'images_apps/certificate/restore_ca.png',txt.Getshort,txt.GetHint);
    txt.Finalize;
  end;

  dc_crt       := ses.FetchDerivedCollection('crt_grid');
  grid_ca.AddFilterEvent(dc_crt.getDescriptionStoreId(),'uids');


  sub_sec_ca   := TFRE_DB_SUBSECTIONS_DESC.Create.Describe(sec_dt_tab);

  sub_sec_ca.AddSection.Describe(CWSF(@Web_CAContent),app.FetchAppTextShort(ses,'certificate_ca'),1,'ca');
  sub_sec_ca.AddSection.Describe(CWSF(@Web_CertificatesContent),app.FetchAppTextShort(ses,'certificate_certificates'),2,'certificates');

  ca            := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(grid_ca,sub_sec_ca,nil,nil,nil,true);
  Result        := ca;
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_CertificatesContent(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  dc_crt              : IFRE_DB_DERIVED_COLLECTION;
  grid_crt            : TFRE_DB_VIEW_LIST_DESC;
  crt                 : TFRE_DB_LAYOUT_DESC;
  txt                 : IFRE_DB_TEXT;

begin
  CheckClassVisibility4MyDomain(ses);

  dc_crt       := ses.FetchDerivedCollection('crt_grid');
  grid_crt     := dc_crt.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  if conn.sys.CheckClassRight4MyDomain(sr_STORE,TFRE_DB_Certificate) then begin
    txt:=app.FetchAppTextFull(ses,'create_crt');
    grid_crt.AddButton.Describe(CWSF(@WEB_addCertificate),'images_apps/certificate/create_crt.png',txt.Getshort,txt.GetHint);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4MyDomain(sr_DELETE,TFRE_DB_Certificate) then begin
    txt:=app.FetchAppTextFull(ses,'revoke_crt');
    grid_crt.AddButton.Describe(CWSF(@WEB_revokeCertificate),'images_apps/certificate/revoke_crt.png',txt.Getshort,txt.GetHint);
    txt.Finalize;
  end;
  crt           := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(grid_crt,WEB_CrtContent(input,ses,app,conn).Implementor_HC as TFRE_DB_CONTENT_DESC,nil,nil,nil,true,2,2);
  Result        := crt;
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_CAContent(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  panel         : TFRE_DB_FORM_PANEL_DESC;
  scheme        : IFRE_DB_SchemeObject;
  ca            : IFRE_DB_Object;
  sel_guid      : TFRE_DB_GUID;
begin

  if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
    sel_guid := input.Field('SELECTED').AsGUID;

    CheckDbResult(conn.Fetch(sel_guid,ca),'could not fetch ca from database');

    GFRE_DBI.GetSystemSchemeByName(ca.SchemeClass,scheme);
    panel :=TFRE_DB_FORM_PANEL_DESC.Create.Describe(app.FetchAppTextShort(ses,'ca_content_header'));
    panel.AddSchemeFormGroup(scheme.GetInputGroup('main_edit'),GetSession(input));
    panel.FillWithObjectValues(ca,GetSession(input));
    panel.AddButton.DescribeDownload(app.FetchAppTextShort(ses,'crt_download_crt'),ses.GetDownLoadLink4StreamField(sel_guid,'crt_stream',true,'application/octet-stream','ca.crt'),false);
    panel.AddButton.DescribeDownload(app.FetchAppTextShort(ses,'crt_download_key'),ses.GetDownLoadLink4StreamField(sel_guid,'key_stream',true,'application/octet-stream','ca.key'),false);
    panel.AddButton.DescribeDownload(app.FetchAppTextShort(ses,'crl_download'),ses.GetDownLoadLink4StreamField(sel_guid,'crl_stream',true,'application/octet-stream','crl.pem'),false);
    panel.contentId:='CA_CONTENT';
    Result:=panel;

  end else begin
    panel :=TFRE_DB_FORM_PANEL_DESC.Create.Describe(app.FetchAppTextShort(ses,'ca_content_header'));
    panel.contentId:='CA_CONTENT';
    Result:=panel;
  end;
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_CAMenu(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  result := GFRE_DB_NIL_DESC;
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_CrtContent(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  panel         : TFRE_DB_FORM_PANEL_DESC;
  scheme        : IFRE_DB_SchemeObject;
  crt           : IFRE_DB_Object;
  sel_guid      : TFRE_DB_GUID;
begin
  if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
    sel_guid := input.Field('SELECTED').AsGUID;
    CheckDBResult(conn.Fetch(sel_guid,crt),'could not fetch certificate from db');
    GFRE_DBI.GetSystemSchemeByName(crt.SchemeClass,scheme);
    panel :=TFRE_DB_FORM_PANEL_DESC.Create.Describe(app.FetchAppTextShort(ses,'crt_content_header'));
    panel.AddSchemeFormGroup(scheme.GetInputGroup('main_edit'),GetSession(input));
    panel.AddButton.DescribeDownload(app.FetchAppTextShort(ses,'crt_download_crt'),ses.GetDownLoadLink4StreamField(sel_guid,'crt_Stream',true,'application/octet-stream','certificate.crt'),false);
    panel.AddButton.DescribeDownload(app.FetchAppTextShort(ses,'crt_download_key'),ses.GetDownLoadLink4StreamField(sel_guid,'key_Stream',true,'application/octet-stream','certificate.key'),false);
    panel.FillWithObjectValues(crt,GetSession(input));
    panel.contentId:='CRT_CONTENT';
    Result:=panel;
  end else begin
    panel :=TFRE_DB_FORM_PANEL_DESC.Create.Describe(app.FetchAppTextShort(ses,'crt_content_header'));
    panel.contentId:='CRT_CONTENT';
    Result:=panel;
  end;
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_CrtMenu(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  result :=GFRE_DB_NIL_DESC;
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_Menu(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res            : TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Add Certificate Authority','images_apps/certificate/add_ca.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'addCertificateAuthority'));
  Result:=res;
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_addCertificateAuthority(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  scheme     : IFRE_DB_SchemeObject;
  res        : TFRE_DB_FORM_DIALOG_DESC;
begin
  if not conn.sys.CheckClassRight4MyDomain(sr_STORE,TFRE_DB_CA) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  GFRE_DBI.GetSystemScheme(TFRE_DB_CA,scheme);
  res:=TFRE_DB_FORM_DIALOG_DESC.create.Describe(app.FetchAppTextShort(ses,'ca_add_diag_cap'),600,true,true,false);
  res.AddSchemeFormGroup(scheme.GetInputGroup('main_create'),ses,false,false);
  res.AddButton.Describe(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('button_save')),CWSF(@WEB_newCertificateAuthority),fdbbt_submit);
  Result:=res;
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_newCertificateAuthority(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var caob_id          : TFRE_DB_GUID;
    caob             : IFRE_DB_Object;
    data             : IFRE_DB_Object;
    lSchemeObject    : IFRE_DB_SchemeObject;
    res              : TFRE_DB_Errortype;

begin
  if not conn.sys.CheckClassRight4MyDomain(sr_STORE,TFRE_DB_CA) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  data             := input.Field('DATA').asobject;

  if not GFRE_DBI.GetSystemScheme(TFRE_DB_CA,lSchemeObject) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'the scheme [%s] is unknown!',[TFRE_DB_CA]);
  caob             := GFRE_DBI.NewObjectScheme(TFRE_DB_CA);
  lSchemeObject.SetObjectFieldsWithScheme(data,caob,true,conn);

  if (caob.Implementor_HC as TFRE_DB_CA).Create_SSL_CA then
    begin
      if conn.GetCollection(CFRE_DB_CA_COLLECTION).Store(caob)=edb_OK then
        Result:=TFRE_DB_CLOSE_DIALOG_DESC.create.Describe()
      else
        raise EFRE_DB_Exception.Create('could not store created ca object');
    end
  else
    result := TFRE_DB_MESSAGE_DESC.create.Describe(app.FetchAppTextShort(ses,'ca_add_diag_cap'),app.FetchAppTextShort(ses,'ca_add_error_msg'),fdbmt_error,nil);
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_addimportCertificateAuthority(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  scheme     : IFRE_DB_SchemeObject;
  res        : TFRE_DB_FORM_DIALOG_DESC;
begin
  if not conn.sys.CheckClassRight4MyDomain(sr_STORE,TFRE_DB_CA) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  GFRE_DBI.GetSystemScheme(TFRE_DB_CA,scheme);
  res:=TFRE_DB_FORM_DIALOG_DESC.create.Describe(app.FetchAppTextShort(ses,'ca_import_diag_cap'),600,true,true,false);
  res.AddSchemeFormGroup(scheme.GetInputGroup('main_import'),ses,false,false);
  res.AddButton.Describe(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('button_save')),CWSF(@WEB_importCertificateAuthority),fdbbt_submit);
  Result:=res;
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_importCertificateAuthority(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var caob_id          : TFRE_DB_GUID;
    caob             : IFRE_DB_Object;
    data             : IFRE_DB_Object;
    lSchemeObject    : IFRE_DB_SchemeObject;
    res              : TFRE_DB_Errortype;
    base_dir         : TFRE_DB_String;
    ca_crt_file,serial_file,ca_key_file,random_file,index_file,crt_dir,key_dir,crl_number_file:TFRE_DB_String;
    import_status    : TFRE_DB_String;

begin
  if not conn.sys.CheckClassRight4MyDomain(sr_STORE,TFRE_DB_CA) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  data             := input.Field('DATA').asobject;
  base_dir         := data.Field('directory').asstring;

  if not GFRE_DBI.GetSystemScheme(TFRE_DB_CA,lSchemeObject) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'the scheme [%s] is unknown!',[TFRE_DB_CA]);
  caob             := GFRE_DBI.NewObjectScheme(TFRE_DB_CA);
  lSchemeObject.SetObjectFieldsWithScheme(data,caob,true,conn);

  ca_crt_file      := base_dir+DirectorySeparator+'ca.crt';
  ca_key_file      := base_dir+DirectorySeparator+'private/ca.key';
  serial_file      := base_dir+DirectorySeparator+'serial';
  random_file      := base_dir+DirectorySeparator+'random';
  index_file       := base_dir+DirectorySeparator+'index.txt';
  crl_number_file  := base_dir+DirectorySeparator+'crlnumber';
  crt_dir          := base_dir+DirectorySeparator+'signed_certs';
  key_dir          := base_dir+DirectorySeparator+'private';

  if (caob.Implementor_HC as TFRE_DB_CA).Import_SSL_CA(ca_crt_file,serial_file,ca_key_file,random_file,index_file,crl_number_file,import_status) then
    begin
      if conn.GetCollection(CFRE_DB_CA_COLLECTION).Store(caob)=edb_OK then
        Result:=TFRE_DB_CLOSE_DIALOG_DESC.create.Describe()
      else
        raise EFRE_DB_Exception.Create('could not store created ca object');
    end
  else
    exit(TFRE_DB_MESSAGE_DESC.create.Describe(app.FetchAppTextShort(ses,'ca_add_import_cap'),import_status,fdbmt_error,nil));

  if (caob.Implementor_HC as TFRE_DB_CA).Import_SSL_Certificates(conn,crt_dir,key_dir,import_status) then
    Result:=TFRE_DB_CLOSE_DIALOG_DESC.create.Describe()
  else
    result := TFRE_DB_MESSAGE_DESC.create.Describe(app.FetchAppTextShort(ses,'ca_add_import_cap'),import_status,fdbmt_error,nil);


end;

function TFRE_CERTIFICATE_CA_MOD.WEB_delCertificateAuthority(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  sf     : TFRE_DB_SERVER_FUNC_DESC;
  cap,msg: String;
begin
  if not conn.sys.CheckClassRight4MyDomain(sr_DELETE,TFRE_DB_CA) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));
  sf:=CWSF(@WEB_delCertificateAuthorityConfirmed);
  sf.AddParam.Describe('selected',input.Field('selected').AsStringArr);
  if input.Field('selected').ValueCount=1 then begin
    cap:=app.FetchAppTextShort(ses,'ca_delete_diag_cap');
    msg:=app.FetchAppTextShort(ses,'ca_delete_diag_msg');
    Result:=TFRE_DB_MESSAGE_DESC.create.Describe(cap,msg,fdbmt_confirm,sf);
  end else begin
    result :=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_delCertificateAuthorityConfirmed(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  coll       : IFRE_DB_COLLECTION;
  ca         : IFRE_DB_Object;
  refs_array : TFRE_DB_GUIDArray;
  i          : NativeInt;
begin
  if not conn.sys.CheckClassRight4MyDomain(sr_DELETE,TFRE_DB_CA) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  if input.field('confirmed').AsBoolean then begin
    refs_array  := conn.GetReferences(input.Field('selected').AsGUID,false,TFRE_DB_CERTIFICATE.ClassName);
    for i:= 0 to High(refs_array) do
      begin
        CheckDbResult(conn.Delete(refs_array[i]),'could not delete crt id '+FREDB_G2H(refs_array[i]));
      end;
    CheckDbResult(conn.Delete(input.Field('selected').AsGUID),'could not delete ca id '+FREDB_G2H(input.Field('selected').AsGUID));
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_addCertificate(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  scheme     : IFRE_DB_SchemeObject;
  res        : TFRE_DB_FORM_DIALOG_DESC;
  ca         : TFRE_DB_String;
  dependend  : TFRE_DB_StringArray;
begin

  if not conn.sys.CheckClassRight4MyDomain(sr_STORE,TFRE_DB_Certificate) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  dependend  := GetDependencyFiltervalues(input,'uids_ref');
  if length(dependend)=0 then begin
     Result:=TFRE_DB_MESSAGE_DESC.create.Describe(app.FetchAppTextShort(ses,'crt_add_diag_cap'),app.FetchAppTextShort(ses,'crt_add_no_ca_msg'),fdbmt_warning,nil);
     exit;
  end;
  ca := dependend[0];

  GFRE_DBI.GetSystemScheme(TFRE_DB_Certificate,scheme);
  res:=TFRE_DB_FORM_DIALOG_DESC.create.Describe(app.FetchAppTextShort(ses,'crt_add_diag_cap'),600,true,true,false);
  res.AddSchemeFormGroup(scheme.GetInputGroup('main_create'),GetSession(input),false,false);
  res.SetElementValue('ca',ca);
  res.AddButton.Describe(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('button_save')),CWSF(@WEB_newCertificate),fdbbt_submit);
  Result:=res;
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_newCertificate(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var crtob            : TFRE_DB_CERTIFICATE;
    data             : IFRE_DB_Object;
    lSchemeObject    : IFRE_DB_SchemeObject;
    res              : TFRE_DB_Errortype;

begin
  if not conn.sys.CheckClassRight4MyDomain(sr_STORE,TFRE_DB_CERTIFICATE) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  data             := input.Field('DATA').asobject;

  if not GFRE_DBI.GetSystemScheme(TFRE_DB_CERTIFICATE,lSchemeObject) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'the scheme [%s] is unknown!',[TFRE_DB_CA]);

  crtob             := TFRE_DB_CERTIFICATE.CreateForDB;
  lSchemeObject.SetObjectFieldsWithScheme(data,crtob,true,conn);

  if crtob.Create_SSL_Certificate(conn) then
    begin
      if conn.GetCollection(CFRE_DB_CERTIFICATE_COLLECTION).Store(crtob)=edb_OK then
        Result:=TFRE_DB_CLOSE_DIALOG_DESC.create.Describe()
      else
        raise EFRE_DB_Exception.Create('could not store created certificate object');
    end
  else
    result := TFRE_DB_MESSAGE_DESC.create.Describe(app.FetchAppTextShort(ses,'crt_add_diag_cap'),app.FetchAppTextShort(ses,'crt_add_error_msg'),fdbmt_error,nil);
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_revokeCertificate(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  sel_guid   : TFRE_DB_GUID;
  crt        : IFRE_DB_Object;
begin
  if not conn.sys.CheckClassRight4MyDomain(sr_DELETE,TFRE_DB_Certificate) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
    sel_guid := input.Field('SELECTED').AsGUID;
    if conn.Fetch(sel_guid,crt)=edb_OK then begin
      ((crt.Implementor_HC) as TFRE_DB_Certificate).WEB_Revoke(input,ses,app,conn);
      if conn.Update(crt)<>edb_OK then begin
        raise EFRE_DB_Exception.Create('Error on updating crt object');
      end;
      result := GFRE_DB_NIL_DESC;
    end else begin
      raise EFRE_DB_Exception.Create('exception on fetchin crt');
    end;
  end else begin
    result := GFRE_DB_NIL_DESC;
  end;
end;


function TFRE_CERTIFICATE_CA_MOD.WEB_restoreCertificateAuthority(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res        : TFRE_DB_FORM_DIALOG_DESC;
begin

  if not conn.sys.CheckClassRight4MyDomain(sr_STORE,TFRE_DB_Certificate) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  TFRE_DB_CA.RestoreCA(conn,'/fre/hal/ca_backup.cfg',ses.GetDomain);
  result := GFRE_DB_NIL_DESC;
end;

function TFRE_CERTIFICATE_CA_MOD.WEB_backupCertificateAuthority(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  scheme     : IFRE_DB_SchemeObject;
  res        : TFRE_DB_FORM_DIALOG_DESC;
  ca_uid     : TFRE_DB_GUID;
  caobj      : IFRE_DB_Object;

begin

  if not conn.sys.CheckClassRight4MyDomain(sr_STORE,TFRE_DB_CA) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then
    begin
      ca_uid := input.Field('SELECTED').AsGUID;
      CheckDBResult(conn.Fetch(ca_uid,caobj),'could not fetch ca');
      (caobj.Implementor_HC as TFRE_DB_CA).BackupCA(conn,'/fre/hal/ca_backup.cfg');
      Result:=GFRE_DB_NIL_DESC;
    end
  else
    result := GFRE_DB_NIL_DESC;
end;


{ TFRE_CERTIFICATE_APP }

procedure TFRE_CERTIFICATE_APP.SetupApplicationStructure;
begin
  inherited SetupApplicationStructure;
  InitApp('description');
  AddApplicationModule(TFRE_CERTIFICATE_CA_MOD.create);
end;

procedure TFRE_CERTIFICATE_APP._UpdateSitemap(const session: TFRE_DB_UserSession);
var
  SiteMapData  : IFRE_DB_Object;
  conn         : IFRE_DB_CONNECTION;
begin
  conn:=session.GetDBConnection;
  SiteMapData  := GFRE_DBI.NewObject;
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Status',FetchAppTextShort(session,'sitemap_main'),'images_apps/certificate/main_white.svg','',0,conn.sys.CheckClassRight4MyDomain(sr_FETCH,TFRE_CERTIFICATE_APP));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Status/Ca',FetchAppTextShort(session,'sitemap_ca'),'images_apps/certificate/ca.svg',TFRE_CERTIFICATE_CA_MOD.ClassName,0,conn.sys.CheckClassRight4MyDomain(sr_FETCH,TFRE_CERTIFICATE_CA_MOD));
  FREDB_SiteMap_RadialAutoposition(SiteMapData);
  session.GetSessionAppData(ClassName).Field('SITEMAP').AsObject := SiteMapData;
end;

procedure TFRE_CERTIFICATE_APP.MySessionInitialize(const session: TFRE_DB_UserSession);
begin
  inherited MySessionInitialize(session);
  if session.IsInteractiveSession then begin
    _UpdateSitemap(session);
  end;
end;

procedure TFRE_CERTIFICATE_APP.MySessionPromotion(const session: TFRE_DB_UserSession);
begin
  inherited MySessionPromotion(session);
  if session.IsInteractiveSession then
    _UpdateSitemap(session);
end;

procedure TFRE_CERTIFICATE_APP.MyServerInitialize(const admin_dbc: IFRE_DB_CONNECTION);
begin
  inherited MyServerInitialize(admin_dbc);
end;

class procedure TFRE_CERTIFICATE_APP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION');
end;

class procedure TFRE_CERTIFICATE_APP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  inherited;

  newVersionId:='1.0';

  if (currentVersionId='') then
    begin
      currentVersionId:='1.0';

      CreateAppText(conn,'caption','Certificate','Certificate','Certificate');
      CreateAppText(conn,'sitemap_main','Certificate','Certificate','Certificate');
      CreateAppText(conn,'sitemap_ca','Certificate Authorities','Certificate Authorities','Certificate Authorities');

      CreateAppText(conn,'cert_description','Certificate');
      CreateAppText(conn,'create_ca','Create CA');
      CreateAppText(conn,'import_ca','Import CA');
      CreateAppText(conn,'delete_ca','Delete CA');
      CreateAppText(conn,'backup_ca','Backup CA');
      CreateAppText(conn,'restore_ca','Restore CA');
      CreateAppText(conn,'ca_name','CA Commonname');
      CreateAppText(conn,'ca_content_header','Properties');
      CreateAppText(conn,'certificate_certificates','Certificates');
      CreateAppText(conn,'certificate_CA','Certificate Authority');
      CreateAppText(conn,'create_crt','Create');
      CreateAppText(conn,'revoke_crt','Revoke');
      CreateAppText(conn,'crt_cn','Commonname');
      CreateAppText(conn,'crt_email','E-Mail');
      CreateAppText(conn,'crt_issued','Issued');
      CreateAppText(conn,'crt_revoked','Revoked');
      CreateAppText(conn,'crt_content_header','Properties');
      CreateAppText(conn,'crt_add_diag_cap','Create Certificate');
      CreateAppText(conn,'crt_download_crt','Download Certificate');
      CreateAppText(conn,'crt_download_key','Download Private Key');
      CreateAppText(conn,'crl_download','Download Certificate Revocation List');
      CreateAppText(conn,'ca_add_diag_cap','Create CA');
      CreateAppText(conn,'ca_add_no_ca_msg','Please select a Certificate Authority first.');
      CreateAppText(conn,'ca_add_import_cap','Import CA');
      CreateAppText(conn,'ca_import_diag_cap','Import CA');
      CreateAppText(conn,'ca_delete_diag_cap','Delete CA');
      CreateAppText(conn,'ca_delete_diag_msg','Do you really want to delete the Certificate Authority ?');

      CreateAppText(conn,'button_save','Save');
    end;
  if (currentVersionId='1.0') then
    begin
    //next update code
    end;
end;

class procedure TFRE_CERTIFICATE_APP.InstallDBObjects4Domain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TFRE_DB_GUID);
begin
  inherited InstallDBObjects4Domain(conn, currentVersionId, domainUID);

  if (currentVersionId='') then
    begin
      currentVersionId:='1.0';
      CheckDbResult(conn.AddGroup('CAADMIN','Group for Certificate Authority Administrators','CA admin',domainUID),'could not create CA admin group');
      CheckDbResult(conn.AddGroup('CERTIFICATEADMIN','Group for Certificate Administrators','Certificate admin',domainUID),'could not create certificate admin group');
      CheckDbResult(conn.AddGroup('CERTIFICATEVIEWER','Group for Certificate Viewers','Certificate viewer',domainUID),'could not create certificate viewer group');

      CheckDbResult(conn.AddRolesToGroup('CAADMIN',domainUID,TFRE_DB_StringArray.Create(
        TFRE_CERTIFICATE_APP.GetClassRoleNameFetch ,
         TFRE_CERTIFICATE_CA_MOD.GetClassRoleNameFetch
        )),'could not add roles for group CAADMIN');
      CheckDbResult(conn.AddRolesToGroup('CAADMIN',domainUID, TFRE_DB_CA.GetClassStdRoles(true,true,true,true)),'could not add roles of TFRE_DB_CA to group CAADMIN');
      CheckDbResult(conn.AddRolesToGroup('CAADMIN',domainUID, TFRE_DB_CERTIFICATE.GetClassStdRoles),'could not add roles of TFRE_DB_CERTIFICATE to group CAADMIN');


      CheckDbResult(conn.AddRolesToGroup('CERTIFICATEADMIN',domainUID,TFRE_DB_StringArray.Create(
        TFRE_CERTIFICATE_APP.GetClassRoleNameFetch ,
         TFRE_CERTIFICATE_CA_MOD.GetClassRoleNameFetch
        )),'could not add roles for group CERTIFICATEADMIN');
      CheckDbResult(conn.AddRolesToGroup('CERTIFICATEADMIN',domainUID, TFRE_DB_CA.GetClassStdRoles(false,true,false,true)),'could not add roles of TFRE_DB_CA to group CERTIFICATEADMIN');
      CheckDbResult(conn.AddRolesToGroup('CERTIFICATEADMIN',domainUID, TFRE_DB_CERTIFICATE.GetClassStdRoles),'could not add roles of TFRE_DB_CERTIFICATE to group CERTIFICATEADMIN');



      CheckDbResult(conn.AddRolesToGroup('CERTIFICATEVIEWER',domainUID,TFRE_DB_StringArray.Create(
        TFRE_CERTIFICATE_APP.GetClassRoleNameFetch ,
         TFRE_CERTIFICATE_CA_MOD.GetClassRoleNameFetch
        )),'could not add roles for group CERTIFICATEVIEWER');

      CheckDbResult(conn.AddRolesToGroup('CERTIFICATEVIEWER',domainUID, TFRE_DB_CA.GetClassStdRoles(false,false,false,true)),'could not add roles of TFRE_DB_CA to group CERTIFICATEVIEWER');
      CheckDbResult(conn.AddRolesToGroup('CERTIFICATEVIEWER',domainUID, TFRE_DB_CERTIFICATE.GetClassStdRoles(false,false,false,true)),'could not add roles of TFRE_DB_CERTIFICATE to group CERTIFICATEVIEWER');
    end;
end;

class procedure TFRE_CERTIFICATE_APP.InstallUserDBObjects4Domain(const conn: IFRE_DB_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TFRE_DB_GUID);
var
  coll: IFRE_DB_COLLECTION;
begin
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
  if currentVersionID = '1.0' then
    begin
      currentVersionId := '1.1';
      if not conn.CollectionExists(CFRE_DB_CA_COLLECTION) then
        conn.CreateCollection(CFRE_DB_CA_COLLECTION,false);
        //coll.DefineIndexOnField('name',fdbft_String,true,true);
      if not conn.CollectionExists(CFRE_DB_CERTIFICATE_COLLECTION) then
        conn.CreateCollection(CFRE_DB_CERTIFICATE_COLLECTION,false);
      //coll.DefineIndexOnField('name',fdbft_String,true,true);
    end;
end;

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_CERTIFICATE_CA_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_CERTIFICATE_APP);
  GFRE_DBI.Initialize_Extension_Objects;
end;

end.

