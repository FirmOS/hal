unit fosillu_hal_svcctrl;


{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils,fre_db_interface,fos_tool_interfaces,
    fos_illumos_defs,fosillu_libscf;

var

  gsfc_handle :  Pscf_handle_t=nil;


procedure fre_init_libsvc;
procedure fre_create_service                               (const svcobj: IFRE_DB_Object);
procedure fre_destroy_service                              (const svcobj: IFRE_DB_Object);
function  fre_get_servicelist                              (const substring_filter: string=''): IFRE_DB_Object;
procedure fre_remove_dependency                            (const service_name: string; const dependency:string);

function  fre_get_local_service_scope                      : Pscf_scope_t;
procedure fre_destroy_service_scope                        (const scope: Pscf_scope_t);
function  fre_create_service_handle                        : Pscf_service_t;
procedure fre_destroy_service_handle                       (const service: Pscf_service_t);
function  fre_create_add_service                           (const service_name: string; const scope: Pscf_scope_t) : Pscf_service_t;
function  fre_get_service                                  (const service_name: string; const scope: Pscf_scope_t) : Pscf_service_t;
function  fre_create_add_service_propertygroup             (const groupname:string;const grouptype:string; const service: Pscf_service_t): Pscf_propertygroup_t;
procedure fre_destroy_propertgroup_handle                  (const pg : Pscf_propertygroup_t);
function  fre_create_start_transaction                     (const pg : Pscf_propertygroup_t) : Pscf_transaction_t;
procedure fre_commit_destroy_transaction                   (const tx : Pscf_transaction_t);
procedure fre_add_property_to_propertygroup_string         (const property_name:string; const property_value:string; const property_type:scf_type_t;const tx : Pscf_transaction_t);
procedure fre_add_property_to_propertygroup_boolean        (const property_name:string; const property_value:boolean; const tx : Pscf_transaction_t);
procedure fre_add_property_to_propertygroup_count          (const property_name:string; const property_value:Uint64; const tx : Pscf_transaction_t);
function  fre_create_value                                 : Pscf_value_t;
procedure fre_add_value_to_entry                           (const value: Pscf_value_t; const entry: Pscf_transaction_entry_t);
function  fre_create_add_entry_to_transaction              (const property_name:string; const property_type:scf_type_t;const tx : Pscf_transaction_t): Pscf_transaction_entry_t;
function  fre_create_add_instance_to_service               (const instance_name: string; const service: Pscf_service_t) : Pscf_instance_t;
procedure fre_destroy_instance_handle                      (const instance : Pscf_instance_t);
function  fre_create_add_instance_propertygroup            (const groupname:string;const grouptype:string; const instance: Pscf_instance_t): Pscf_propertygroup_t;
function  fre_get_instance                                 (const instance_name: string; const service: Pscf_service_t) : Pscf_instance_t;
function  fre_create_pg                                    : Pscf_propertygroup_t;



implementation

function  fre_get_local_service_scope: Pscf_scope_t;
var err : integer;
    msg : string;
begin
  if not assigned(gsfc_handle) then
    fre_init_libsvc;
  GFRE_DBI.LogDebug(dblc_APPLICATION,'create scope');
  result:=scf_scope_create(gsfc_handle);
  if result=nil then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not create scope:'+' '+msg);
    end;

  GFRE_DBI.LogDebug(dblc_APPLICATION,'create local scope');
  err := scf_handle_get_scope(gsfc_handle,PChar(SCF_SCOPE_LOCAL),result);
  if err<>0 then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not get local scope:'+inttostr(err)+' '+msg);
    end;
end;

procedure fre_destroy_service_scope(const scope: Pscf_scope_t);
begin
  GFRE_DBI.LogDebug(dblc_APPLICATION,'destroy scope');
  scf_scope_destroy(scope);
end;

function fre_create_service_handle: Pscf_service_t;
var err : integer;
    msg : string;
begin
  GFRE_DBI.LogDebug(dblc_APPLICATION,'create service');
  result:=scf_service_create(gsfc_handle);
  if result=nil then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not create service:'+' '+msg);
    end;
end;

procedure fre_destroy_service_handle(const service: Pscf_service_t);
begin
  scf_service_destroy(service);
end;

function fre_create_add_service(const service_name: string; const scope: Pscf_scope_t): Pscf_service_t;
var err : integer;
    msg : string;
begin
  result := fre_create_service_handle;

  GFRE_DBI.LogDebug(dblc_APPLICATION,'add service');
  err := scf_scope_add_service(scope,PChar(service_name),result);
  if err<>0 then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not add service:'+inttostr(err)+' '+msg);
    end;
end;

function fre_get_service(const service_name: string; const scope: Pscf_scope_t): Pscf_service_t;
var err : integer;
    msg : string;
begin
  result := fre_create_service_handle;

  GFRE_DBI.LogDebug(dblc_APPLICATION,'get service ');
  err := scf_scope_get_service(scope,PChar(service_name),result);
  if err<>0 then
    begin
      if scf_error=SCF_ERROR_NOT_FOUND then
        begin
          fre_destroy_service_handle(result);
          result := nil;
        end;
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not get service:'+inttostr(err)+' '+msg);
    end;
end;

function fre_create_add_service_propertygroup(const groupname: string; const grouptype: string; const service: Pscf_service_t): Pscf_propertygroup_t;
var err : integer;
    msg : string;
begin
  result := fre_create_pg;

  GFRE_DBI.LogDebug(dblc_APPLICATION,'add propertygroup');
  err := scf_service_add_pg(service,Pchar(groupname),Pchar(grouptype),0,result);
  if err<>0 then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not property group:'+inttostr(err)+' '+msg);
    end;
end;

procedure fre_init_libsvc;
var err : integer;
    msg : string;
begin
  if assigned(gsfc_handle) then
    exit;

//  GFRE_DBI.LogDebug(dblc_APPLICATION,('SWL: get handle');
  gsfc_handle      := scf_handle_create(SCF_VERSION);
  if gsfc_handle=nil then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not get scf_handle:'+' '+msg);
    end;

//  GFRE_DBI.LogDebug(dblc_APPLICATION,('SWL: bind handle');
  err := scf_handle_bind(gsfc_handle);
  if err<>0 then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not bind scf_handle:'+inttostr(err)+' '+msg);
    end;

end;

procedure fre_create_service(const svcobj: IFRE_DB_Object);
var service_name : string;

    imp_scope     : Pscf_scope_t;
    imp_service   : Pscf_service_t;
    imp_pg        : Pscf_propertygroup_t;
    imp_instance  : Pscf_instance_t;
    current_tx    : Pscf_transaction_t;
    err           : Integer;
    msg           : string;
    isEnabled     : boolean;

    i             : NativeInt;
    dependency    : IFRE_DB_Object;
    deb_service   : PScf_service_t;
    deb_pg        : PScf_propertygroup_t;

begin
  service_name := svcobj.Field('svc_name').asstring;
  if svcobj.FieldExists('svc_enabled') then
    isEnabled    := svcobj.Field('svc_enabled').AsBoolean
  else
    isEnabled    := false;

  fre_init_libsvc;

  imp_scope    := fre_get_local_service_scope;

  imp_service  := fre_create_add_service(service_name,imp_scope);

  imp_pg       := fre_create_add_service_propertygroup('startd',SCF_GROUP_FRAMEWORK,imp_service);
  try
    current_tx := fre_create_start_transaction(imp_pg);
    try
      fre_add_property_to_propertygroup_string(SCF_PROPERTY_DURATION,svcobj.Field('svc_duration').asstring,SCF_TYPE_ASTRING,current_tx);
      if svcobj.FieldExists('svc_ignore_error') then
        fre_add_property_to_propertygroup_string(SCF_PROPERTY_IGNORE,svcobj.Field('svc_ignore_error').AsString,SCF_TYPE_ASTRING,current_tx);
    finally
      fre_commit_destroy_transaction(current_tx);
    end;
  finally
    fre_destroy_propertgroup_handle(imp_pg);
  end;

  if svcobj.FieldExists('svc_dependency') then
    begin
      for i := 0 to svcobj.Field('svc_dependency').ValueCount-1 do
        begin
          dependency   := svcobj.Field('svc_dependency').AsObjectItem[i];
          imp_pg       := fre_create_add_service_propertygroup(dependency.Field('name').asstring,SCF_GROUP_DEPENDENCY,imp_service);
          try
            current_tx := fre_create_start_transaction(imp_pg);
            try
              fre_add_property_to_propertygroup_string(SCF_PROPERTY_GROUPING,dependency.Field('grouping').asstring,SCF_TYPE_ASTRING,current_tx);
              fre_add_property_to_propertygroup_string(SCF_PROPERTY_RESTART_ON,dependency.Field('restart_on').asstring,SCF_TYPE_ASTRING,current_tx);
              fre_add_property_to_propertygroup_string(SCF_PROPERTY_TYPE_,'service',SCF_TYPE_ASTRING,current_tx);
              fre_add_property_to_propertygroup_string(SCF_PROPERTY_ENTITIES,dependency.Field('fmri').asstring,SCF_TYPE_FMRI,current_tx);
            finally
              fre_commit_destroy_transaction(current_tx);
            end;
          finally
            fre_destroy_propertgroup_handle(imp_pg);
          end;
        end;
    end;

  if svcobj.FieldExists('svc_dependent') then
    begin
      for i := 0 to svcobj.Field('svc_dependent').ValueCount-1 do
        begin
          dependency   := svcobj.Field('svc_dependent').AsObjectItem[i];
          writeln('SWL DEPENDENT: ',dependency.Field('fmri').asstring);
          deb_service  := fre_get_service(Copy(dependency.Field('fmri').asstring,6,maxint),imp_scope);
          try
            deb_pg       := fre_create_add_service_propertygroup(dependency.Field('name').asstring,SCF_GROUP_DEPENDENCY,deb_service);
            try
              current_tx := fre_create_start_transaction(deb_pg);
              try
                fre_add_property_to_propertygroup_string(SCF_PROPERTY_GROUPING,dependency.Field('grouping').asstring,SCF_TYPE_ASTRING,current_tx);
                fre_add_property_to_propertygroup_string(SCF_PROPERTY_RESTART_ON,dependency.Field('restart_on').asstring,SCF_TYPE_ASTRING,current_tx);
                fre_add_property_to_propertygroup_string(SCF_PROPERTY_ENTITIES,'svc:/'+service_name,SCF_TYPE_FMRI,current_tx);
                fre_add_property_to_propertygroup_string(SCF_PROPERTY_TYPE_,'service',SCF_TYPE_ASTRING,current_tx);
                fre_add_property_to_propertygroup_boolean('external',true,current_tx);
              finally
                fre_commit_destroy_transaction(current_tx);
              end;
            finally
              fre_destroy_propertgroup_handle(deb_pg);
            end;
            err  := _smf_refresh_all_instances(deb_service);
            if err<>0 then
              begin
                msg := StrPas(scf_strerror(scf_error));
                raise Exception.Create('could not refresh all instances for dependent fmri:'+inttostr(err)+' '+msg);
              end;
          finally
            fre_destroy_service_handle(deb_service);
          end;

          imp_pg       := fre_create_add_service_propertygroup(SCF_PG_DEPENDENTS,SCF_GROUP_FRAMEWORK,imp_service);
          try
            current_tx := fre_create_start_transaction(imp_pg);
            try
              fre_add_property_to_propertygroup_string(dependency.Field('name').asstring,dependency.Field('fmri').asstring,SCF_TYPE_FMRI,current_tx);
            finally
              fre_commit_destroy_transaction(current_tx);
            end;
          finally
            fre_destroy_propertgroup_handle(imp_pg);
          end;
        end;
    end;
   writeln('SWL DEPENDENT DONE');

  imp_pg       := fre_create_add_service_propertygroup(SCF_PG_GENERAL,SCF_GROUP_FRAMEWORK,imp_service);
  try
    current_tx := fre_create_start_transaction(imp_pg);
    try
      fre_add_property_to_propertygroup_string('entity_stability','Evolving',SCF_TYPE_ASTRING,current_tx);
      fre_add_property_to_propertygroup_boolean(SCF_PROPERTY_SINGLE_INSTANCE,true,current_tx);
    finally
      fre_commit_destroy_transaction(current_tx);
    end;
  finally
    fre_destroy_propertgroup_handle(imp_pg);
  end;

  imp_pg       := fre_create_add_service_propertygroup('start',SCF_GROUP_METHOD,imp_service);
  try
    current_tx := fre_create_start_transaction(imp_pg);
    try
      fre_add_property_to_propertygroup_string(SCF_PROPERTY_TYPE_,'method',SCF_TYPE_ASTRING,current_tx);
      fre_add_property_to_propertygroup_string(SCF_PROPERTY_EXEC,svcobj.Field('svc_start_exec').asstring,SCF_TYPE_ASTRING,current_tx);
      fre_add_property_to_propertygroup_count(SCF_PROPERTY_TIMEOUT,svcobj.Field('svc_start_timeout').AsUInt64,current_tx);
    finally
      fre_commit_destroy_transaction(current_tx);
    end;
  finally
    fre_destroy_propertgroup_handle(imp_pg);
  end;

  imp_pg       := fre_create_add_service_propertygroup('stop',SCF_GROUP_METHOD,imp_service);
  try
    current_tx := fre_create_start_transaction(imp_pg);
    try
      fre_add_property_to_propertygroup_string(SCF_PROPERTY_TYPE_,'method',SCF_TYPE_ASTRING,current_tx);
      fre_add_property_to_propertygroup_string(SCF_PROPERTY_EXEC,svcobj.Field('svc_stop_exec').asstring,SCF_TYPE_ASTRING,current_tx);
      fre_add_property_to_propertygroup_count(SCF_PROPERTY_TIMEOUT,svcobj.Field('svc_stop_timeout').AsUInt64,current_tx);
    finally
      fre_commit_destroy_transaction(current_tx);
    end;
  finally
    fre_destroy_propertgroup_handle(imp_pg);
  end;

  if svcobj.FieldExists('svc_restart_exec') then
    begin
      imp_pg       := fre_create_add_service_propertygroup('restart',SCF_GROUP_METHOD,imp_service);
      try
        current_tx := fre_create_start_transaction(imp_pg);
        try
          fre_add_property_to_propertygroup_string(SCF_PROPERTY_TYPE_,'method',SCF_TYPE_ASTRING,current_tx);
          fre_add_property_to_propertygroup_string(SCF_PROPERTY_EXEC,svcobj.Field('svc_restart_exec').asstring,SCF_TYPE_ASTRING,current_tx);
          fre_add_property_to_propertygroup_count(SCF_PROPERTY_TIMEOUT,svcobj.Field('svc_restart_timeout').AsUInt64,current_tx);
        finally
          fre_commit_destroy_transaction(current_tx);
        end;
      finally
        fre_destroy_propertgroup_handle(imp_pg);
      end;
    end;

  imp_pg       := fre_create_add_service_propertygroup(SCF_PG_METHOD_CONTEXT,SCF_GROUP_FRAMEWORK,imp_service);
  try
    current_tx := fre_create_start_transaction(imp_pg);
    try
      fre_add_property_to_propertygroup_boolean(SCF_PROPERTY_USE_PROFILE,false,current_tx);
      if svcobj.FieldExists('svc_group') then
        fre_add_property_to_propertygroup_string(SCF_PROPERTY_GROUP,svcobj.Field('svc_group').asstring,SCF_TYPE_ASTRING,current_tx);
      if svcobj.FieldExists('svc_user') then
        fre_add_property_to_propertygroup_string(SCF_PROPERTY_USER,svcobj.Field('svc_user').asstring,SCF_TYPE_ASTRING,current_tx);
      if svcobj.FieldExists('svc_working_directory') then
        fre_add_property_to_propertygroup_string(SCF_PROPERTY_WORKING_DIRECTORY,svcobj.Field('svc_working_directory').asstring,SCF_TYPE_ASTRING,current_tx);
      if svcobj.FieldExists('svc_environment') then
        fre_add_property_to_propertygroup_string(SCF_PROPERTY_ENVIRONMENT,svcobj.Field('svc_environment').asstring,SCF_TYPE_ASTRING,current_tx);
      if (svcobj.FieldExists('svc_privileges')) and (svcobj.Field('svc_privileges').asstring<>'') then
        fre_add_property_to_propertygroup_string(SCF_PROPERTY_PRIVILEGES,svcobj.Field('svc_privileges').asstring,SCF_TYPE_ASTRING,current_tx);
    finally
      fre_commit_destroy_transaction(current_tx);
    end;
  finally
    fre_destroy_propertgroup_handle(imp_pg);
  end;

  if svcobj.FieldExists('svc_common_name') then
    imp_pg       := fre_create_add_service_propertygroup(SCF_PG_TM_COMMON_NAME,SCF_GROUP_TEMPLATE,imp_service);
    try
      current_tx := fre_create_start_transaction(imp_pg);
      try
          fre_add_property_to_propertygroup_string('C',svcobj.Field('svc_common_name').asstring,SCF_TYPE_USTRING,current_tx);
      finally
        fre_commit_destroy_transaction(current_tx);
      end;
    finally
      fre_destroy_propertgroup_handle(imp_pg);
    end;


  imp_instance := fre_create_add_instance_to_service('default',imp_service);
  try
    imp_pg       := fre_create_add_instance_propertygroup(SCF_PG_GENERAL,SCF_GROUP_FRAMEWORK,imp_instance);
    try
      current_tx := fre_create_start_transaction(imp_pg);
      try
        fre_add_property_to_propertygroup_boolean(SCF_PROPERTY_ENABLED,isEnabled,current_tx);
      finally
        fre_commit_destroy_transaction(current_tx);
      end;
    finally
      fre_destroy_propertgroup_handle(imp_pg);
    end;
  finally
    fre_destroy_instance_handle(imp_instance);
  end;

  fre_destroy_service_handle(imp_service);
  fre_destroy_service_scope(imp_scope);
end;

procedure fre_destroy_service(const svcobj: IFRE_DB_Object);
var service_name  : string;
    imp_scope     : Pscf_scope_t;
    imp_service   : Pscf_service_t;
    imp_instance  : Pscf_instance_t;
    err           : Integer;
    msg           : string;
begin
  service_name := svcobj.Field('svc_name').asstring;

  fre_init_libsvc;

  imp_scope    := fre_get_local_service_scope;
  imp_service  := fre_get_service(service_name,imp_scope);
  if assigned(imp_service) then
    begin
      imp_instance:= fre_get_instance('default',imp_service);
      if assigned(imp_instance) then
        begin
          GFRE_DBI.LogDebug(dblc_APPLICATION,'delete instance');
          err := scf_instance_delete(imp_instance);
          if err<>0 then
            begin
              msg := StrPas(scf_strerror(scf_error));
              raise Exception.Create('could not delete instance:'+inttostr(err)+' '+msg);
            end;
          fre_destroy_instance_handle(imp_instance);
        end;
      GFRE_DBI.LogDebug(dblc_APPLICATION,'delete service');
      err := scf_service_delete(imp_service);
      if err<>0 then
        begin
          msg := StrPas(scf_strerror(scf_error));
          raise Exception.Create('could not delete service:'+inttostr(err)+' '+msg);
        end;
      fre_destroy_service_handle(imp_service);
    end;
  fre_destroy_service_scope(imp_scope);
end;

function fre_get_servicelist(const substring_filter: string): IFRE_DB_Object;
var scope           : Pscf_scope_t;
    sciter          : Pscf_iter_t;
    service         : Pscf_service_t;
    err             : integer;
    msg             : string;
    service_name    : PChar;
    fmri            : string;
    max_fmri_length : integer;
    svcdbo          : IFRE_DB_Object;
begin
  result := GFRE_DBI.NewObject;

  max_fmri_length := scf_limit(SCF_LIMIT_MAX_FMRI_LENGTH);
  service_name    := GetMem(max_fmri_length);

  scope  := fre_get_local_service_scope;
  try
    GFRE_DBI.LogDebug(dblc_APPLICATION,'create iter');
    sciter:=scf_iter_create(gsfc_handle);
    try
      if sciter=nil then
        begin
          msg := StrPas(scf_strerror(scf_error));
          raise Exception.Create('could not create iter:'+' '+msg);
        end;
      err  := scf_iter_scope_services(sciter,scope);
      if err<>0 then
        begin
          msg := StrPas(scf_strerror(scf_error));
          raise Exception.Create('could not iter services:'+inttostr(err)+' '+msg);
        end;
      service := fre_create_service_handle;
      try
        while (true) do
          begin
            err := scf_iter_next_service(sciter,service);
            if err=0 then
              break;
            if err<>1 then
              begin
                msg := StrPas(scf_strerror(scf_error));
                raise Exception.Create('could not iter next service:'+inttostr(err)+' '+msg);
              end;
            err := scf_service_to_fmri(service,service_name,max_fmri_length+1);
            if err<0 then
              begin
                msg := StrPas(scf_strerror(scf_error));
                raise Exception.Create('could not get fmri from service:'+inttostr(err)+' '+msg);
              end;
            fmri := StrPas(service_name);
            if (length(substring_filter)=0) or (Pos(substring_filter,fmri)>0) then
              begin
                svcdbo:= GFRE_DBI.NewObject;
                svcdbo.Field('fmri').AsString:=StrPas(service_name);
                result.Field(svcdbo.UID.AsHexString).AsObject:=svcdbo;
              end;
          end;
      finally
        fre_destroy_service_handle(service);
      end;
    finally
      scf_iter_destroy(sciter);
    end;
  finally
    fre_destroy_service_scope(scope);
    FreeMem(service_name);
  end;
end;

procedure fre_remove_dependency(const service_name: string; const dependency: string);
var   scope     : Pscf_scope_t;
      service   : Pscf_service_t;
      pg        : Pscf_propertygroup_t;
      err       : integer;
      msg       : string;
begin
  fre_init_libsvc;

  scope    := fre_get_local_service_scope;

  service  := fre_get_service(service_name,scope);
  try
    pg  := fre_create_pg;
    try
      err := scf_service_get_pg(service,PChar(dependency),pg);
      if err<>0 then
        begin
          msg := StrPas(scf_strerror(scf_error))+' '+dependency;
          raise Exception.Create('could not get propertygroup:'+inttostr(err)+' '+msg);
        end;
      writeln('SWL: PROPERTY GROUP FOUND');
      err := scf_pg_delete(pg);
      if err<>0 then
        begin
          msg := StrPas(scf_strerror(scf_error))+' '+dependency;
          raise Exception.Create('could not delete propertygroup:'+inttostr(err)+' '+msg);
        end;
      writeln('SWL: PROPERTY GROUP DELETED '+dependency);
    finally
      fre_destroy_propertgroup_handle(pg);
    end;
  finally
    fre_destroy_service_handle(service);
  end;
end;

procedure fre_destroy_propertgroup_handle(const pg: Pscf_propertygroup_t);
begin
  scf_pg_destroy(pg);
end;

function fre_create_start_transaction(const pg: Pscf_propertygroup_t): Pscf_transaction_t;
var err : integer;
    msg : string;
begin

  GFRE_DBI.LogDebug(dblc_APPLICATION,'create transaction');
  result := scf_transaction_create(gsfc_handle);
  if result=nil then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not create transaction:'+' '+msg);
    end;

  GFRE_DBI.LogDebug(dblc_APPLICATION,'start transaction');
  err := scf_transaction_start(result,pg);
  if err<>0 then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not start transaction:'+inttostr(err)+' '+msg);
    end;
end;

procedure fre_commit_destroy_transaction(const tx: Pscf_transaction_t);
var err : integer;
    msg : string;
begin
  GFRE_DBI.LogDebug(dblc_APPLICATION,'commit transaction');
  err := scf_transaction_commit(tx);
  if err<0 then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not commit transaction:'+inttostr(err)+' '+msg);
    end;

  scf_transaction_destroy_children(tx);
  scf_transaction_destroy(tx);
end;

procedure fre_add_property_to_propertygroup_string(const property_name: string; const property_value: string; const property_type: scf_type_t; const tx: Pscf_transaction_t);
var err    : integer;
    msg    : string;
    entry  : Pscf_transaction_entry_t;
    value  : Pscf_value_t;
begin
  GFRE_DBI.LogDebug(dblc_APPLICATION,'Adding Property '+property_name+' '+property_value);

  entry    := fre_create_add_entry_to_transaction(property_name,property_type,tx);
  value    := fre_create_value;

  GFRE_DBI.LogDebug(dblc_APPLICATION,'value set from string');
  err := scf_value_set_from_string(value,property_type,PChar(property_value));
  if err<>0 then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not set value from string:'+inttostr(err)+' '+msg);
    end;

  fre_add_value_to_entry(value,entry);

end;

procedure fre_add_property_to_propertygroup_boolean( const property_name: string; const property_value: boolean; const tx: Pscf_transaction_t);
var err    : integer;
    msg    : string;
    entry  : Pscf_transaction_entry_t;
    value  : Pscf_value_t;
    boolean_value : Uint8;
begin
  GFRE_DBI.LogDebug(dblc_APPLICATION,'Adding Property '+property_name+' '+BoolToStr(property_value));

  if property_value then
    boolean_value:=1
  else
    boolean_value:=0;

  entry    := fre_create_add_entry_to_transaction(property_name,SCF_TYPE_BOOLEAN,tx);
  value    := fre_create_value;

  GFRE_DBI.LogDebug(dblc_APPLICATION,'value set from boolean');
  scf_value_set_boolean(value,boolean_value);

  fre_add_value_to_entry(value,entry);
end;

procedure fre_add_property_to_propertygroup_count(const property_name: string; const property_value: Uint64; const tx: Pscf_transaction_t);
var err    : integer;
    msg    : string;
    entry  : Pscf_transaction_entry_t;
    value  : Pscf_value_t;
begin
  GFRE_DBI.LogDebug(dblc_APPLICATION,'Adding Property '+property_name+' '+inttostr(property_value));

  entry    := fre_create_add_entry_to_transaction(property_name,SCF_TYPE_COUNT,tx);
  value    := fre_create_value;

  GFRE_DBI.LogDebug(dblc_APPLICATION,'value set from count');
  scf_value_set_count(value,property_value);

  fre_add_value_to_entry(value,entry);
end;

function fre_create_value: Pscf_value_t;
var err : integer;
    msg : string;
begin
  GFRE_DBI.LogDebug(dblc_APPLICATION,'create value');
  result := scf_value_create(gsfc_handle);
  if result=nil then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not create value:'+inttostr(err)+' '+msg);
    end;
end;

procedure fre_add_value_to_entry(const value: Pscf_value_t; const entry: Pscf_transaction_entry_t);
var err : integer;
    msg : string;
begin
  GFRE_DBI.LogDebug(dblc_APPLICATION,'add value');
  err := scf_entry_add_value(entry,value);
  if err<>0 then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not add value:'+inttostr(err)+' '+msg);
    end;
end;

function fre_create_add_entry_to_transaction(const property_name: string; const property_type: scf_type_t; const tx: Pscf_transaction_t): Pscf_transaction_entry_t;
var err : integer;
    msg : string;
begin
  GFRE_DBI.LogDebug(dblc_APPLICATION,'create entry');
  result := scf_entry_create(gsfc_handle);
  if result=nil then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not create entry:'+' '+msg);
    end;

  err := scf_transaction_property_new(tx,result,PChar(property_name),property_type);
  if err<>0 then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not set transaction property new:'+inttostr(err)+' '+msg);
    end;
end;

function fre_create_add_instance_to_service(const instance_name: string; const service: Pscf_service_t): Pscf_instance_t;
var err : integer;
    msg : string;
begin
  GFRE_DBI.LogDebug(dblc_APPLICATION,'create instance');
  result := scf_instance_create(gsfc_handle);
  if result=nil then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not create instance:'+' '+msg);
    end;

  GFRE_DBI.LogDebug(dblc_APPLICATION,'add instance');
  err := scf_service_add_instance(service,PChar(instance_name),result);
  if err<>0 then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not add instance:'+inttostr(err)+' '+msg);
    end;
end;

procedure fre_destroy_instance_handle(const instance: Pscf_instance_t);
begin
  scf_instance_destroy(instance);
end;

function fre_create_add_instance_propertygroup(const groupname: string; const grouptype: string; const instance: Pscf_instance_t): Pscf_propertygroup_t;
var err : integer;
    msg : string;
begin
  GFRE_DBI.LogDebug(dblc_APPLICATION,'create propertygroup');
  result := scf_pg_create(gsfc_handle);
  if result=nil then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not create property group:'+' '+msg);
    end;

  GFRE_DBI.LogDebug(dblc_APPLICATION,'add propertygroup');
  err := scf_instance_add_pg(instance,Pchar(groupname),Pchar(grouptype),0,result);
  if err<>0 then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not property group:'+inttostr(err)+' '+msg);
    end;
end;

function fre_get_instance(const instance_name: string;const service: Pscf_service_t): Pscf_instance_t;
var err : integer;
    msg : string;
begin
  GFRE_DBI.LogDebug(dblc_APPLICATION,'create instance handle');
  result := scf_instance_create(gsfc_handle);
  if result=nil then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not create instance:'+' '+msg);
    end;

  GFRE_DBI.LogDebug(dblc_APPLICATION,'get instance ');
  err := scf_service_get_instance(service,PChar(instance_name),result);
  if err<>0 then
    begin
      if scf_error=SCF_ERROR_NOT_FOUND then
        begin
          fre_destroy_instance_handle(result);
          result := nil;
        end;
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not get instance:'+inttostr(err)+' '+msg);
    end;
end;

function fre_create_pg: Pscf_propertygroup_t;
var err : integer;
    msg : string;
begin
  GFRE_DBI.LogDebug(dblc_APPLICATION,'create propertygroup');
  result := scf_pg_create(gsfc_handle);
  if result=nil then
    begin
      msg := StrPas(scf_strerror(scf_error));
      raise Exception.Create('could not create property group:'+' '+msg);
    end;
end;



end.

