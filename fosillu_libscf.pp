unit fosillu_libscf;

interface

uses
  ctypes,unixtype,fos_illumos_defs,fosillu_nvpair;

{
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License (the "License").
 * You may not use this file except in compliance with the License.
 *
 * You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
 * or http://www.opensolaris.org/os/licensing.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at usr/src/OPENSOLARIS.LICENSE.
 * If applicable, add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your own identifying
 * information: Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
  }
{
 * Copyright (c) 2004, 2010, Oracle and/or its affiliates. All rights reserved.
  }

const
  External_library='libscf'; {Setup as you need}

{ * Return codes }
  SCF_SUCCESS = 0;
  SCF_COMPLETE = 1;
  SCF_FAILED = -(1);

{ * scf_tmpl_strerror() human readable flag }

  SCF_TMPL_STRERROR_HUMAN = $1;

const
    SCF_SERVICE_CONFIGD = 'svc:/system/svc/repository:default';
    SCF_INSTANCE_GLOBAL = ('svc:/system/svc/global:default');
    SCF_SERVICE_GLOBAL  = ('svc:/system/svc/global');
    SCF_SERVICE_STARTD  = ('svc:/system/svc/restarter:default');
    SCF_INSTANCE_EMI    =('svc:/system/early-manifest-import:default');
    SCF_INSTANCE_FS_MINIMAL =('svc:/system/filesystem/minimal:default');
    SCF_INSTANCE_MI         =('svc:/system/manifest-import:default');
    SCF_MILESTONE_SINGLE_USER=('svc:/milestone/single-user:default');
    SCF_MILESTONE_MULTI_USER=('svc:/milestone/multi-user:default');
    SCF_MILESTONE_MULTI_USER_SERVER=('svc:/milestone/multi-user-server:default');
    SCF_SCOPE_LOCAL=('localhost');
    SCF_GROUP_APPLICATION=('application');
    SCF_GROUP_FRAMEWORK=('framework');
    SCF_GROUP_DEPENDENCY=('dependency');
    SCF_GROUP_METHOD=('method');
    SCF_GROUP_TEMPLATE=('template');
    SCF_GROUP_TEMPLATE_PG_PATTERN=('template_pg_pattern');
    SCF_GROUP_TEMPLATE_PROP_PATTERN=('template_prop_pattern');
    SCF_DEP_REQUIRE_ALL=('require_all');
    SCF_DEP_REQUIRE_ANY=('require_any');
    SCF_DEP_EXCLUDE_ALL=('exclude_all');
    SCF_DEP_OPTIONAL_ALL=('optional_all');
    SCF_DEP_RESET_ON_ERROR=('error');
    SCF_DEP_RESET_ON_RESTART=('restart');
    SCF_DEP_RESET_ON_REFRESH=('refresh');
    SCF_DEP_RESET_ON_NONE=('none');
    SCF_PG_GENERAL=('general');
    SCF_PG_GENERAL_OVR=('general_ovr');
    SCF_PG_RESTARTER=('restarter');
    SCF_PG_RESTARTER_ACTIONS=('restarter_actions');
    SCF_PG_METHOD_CONTEXT=('method_context');
    SCF_PG_APP_DEFAULT=('application');
    SCF_PG_DEPENDENTS=('dependents');
    SCF_PG_OPTIONS=('options');
    SCF_PG_OPTIONS_OVR=('options_ovr');
    SCF_PG_STARTD=('startd');
    SCF_PG_STARTD_PRIVATE=('svc-startd-private');
    SCF_PG_DEATHROW=('deathrow');
    SCF_PG_MANIFESTFILES=('manifestfiles');
    SCF_PG_TM_COMMON_NAME=('tm_common_name');
    SCF_PG_TM_DESCRIPTION=('tm_description');
    SCF_PG_TM_MAN_PREFIX=('tm_man_');
    SCF_PG_TM_DOC_PREFIX=('tm_doc_');
    SCF_PROPERTY_ACTIVE_POSTFIX=('active');
    SCF_PROPERTY_AUX_STATE=('auxiliary_state');
    SCF_PROPERTY_AUX_FMRI=('auxiliary_fmri');
    SCF_PROPERTY_AUX_TTY=('auxiliary_tty');
    SCF_PROPERTY_CONTRACT=('contract');
    SCF_PROPERTY_COREFILE_PATTERN=('corefile_pattern');
    SCF_PROPERTY_DEGRADED=('degraded');
    SCF_PROPERTY_DEGRADE_IMMEDIATE=('degrade_immediate');
    SCF_PROPERTY_DURATION=('duration');
    SCF_PROPERTY_ENABLED=('enabled');
    SCF_PROPERTY_DEATHROW=('deathrow');
    SCF_PROPERTY_ENTITY_STABILITY=('entity_stability');
    SCF_PROPERTY_ENTITIES=('entities');
    SCF_PROPERTY_EXEC=('exec');
    SCF_PROPERTY_GROUP=('group');
    SCF_PROPERTY_GROUPING=('grouping');
    SCF_PROPERTY_IGNORE=('ignore_error');
    SCF_PROPERTY_INTERNAL_SEPARATORS=('internal_separators');
    SCF_PROPERTY_LIMIT_PRIVILEGES=('limit_privileges');
    SCF_PROPERTY_MAINT_OFF=('maint_off');
    SCF_PROPERTY_MAINT_ON=('maint_on');
    SCF_PROPERTY_MAINT_ON_IMMEDIATE=('maint_on_immediate');
    SCF_PROPERTY_MAINT_ON_IMMTEMP=('maint_on_immtemp');
    SCF_PROPERTY_MAINT_ON_TEMPORARY=('maint_on_temporary');
    SCF_PROPERTY_METHOD_PID=('method_pid');
    SCF_PROPERTY_MILESTONE=('milestone');
    SCF_PROPERTY_NEED_SESSION=('need_session');
    SCF_PROPERTY_NEXT_STATE=('next_state');
    SCF_PROPERTY_PACKAGE=('package');
    SCF_PROPERTY_PRIVILEGES=('privileges');
    SCF_PROPERTY_PROFILE=('profile');
    SCF_PROPERTY_PROJECT=('project');
    SCF_PROPERTY_REFRESH=('refresh');
    SCF_PROPERTY_RESOURCE_POOL=('resource_pool');
    SCF_PROPERTY_ENVIRONMENT=('environment');
    SCF_PROPERTY_RESTART=('restart');
    SCF_PROPERTY_RESTARTER=('restarter');
    SCF_PROPERTY_RESTART_INTERVAL=('restart_interval');
    SCF_PROPERTY_RESTART_ON=('restart_on');
    SCF_PROPERTY_RESTORE=('restore');
    SCF_PROPERTY_SINGLE_INSTANCE=('single_instance');
    SCF_PROPERTY_START_METHOD_TIMESTAMP=('start_method_timestamp');
    SCF_PROPERTY_START_METHOD_WAITSTATUS=('start_method_waitstatus');
    SCF_PROPERTY_START_PID=('start_pid');
    SCF_PROPERTY_STATE=('state');
    SCF_PROPERTY_STABILITY=('stability');
    SCF_PROPERTY_STATE_TIMESTAMP=('state_timestamp');
    SCF_PROPERTY_SUPP_GROUPS=('supp_groups');
    SCF_PROPERTY_TIMEOUT=('timeout_seconds');
    SCF_PROPERTY_TIMEOUT_RETRY=('timeout_retry');
    SCF_PROPERTY_TRANSIENT_CONTRACT=('transient_contract');
    SCF_PROPERTY_TYPE_=('type');
    SCF_PROPERTY_USE_PROFILE=('use_profile');
    SCF_PROPERTY_USER=('user');
    SCF_PROPERTY_UTMPX_PREFIX=('utmpx_prefix');
    SCF_PROPERTY_WORKING_DIRECTORY=('working_directory');
    SCF_PROPERTY_TM_CARDINALITY_MIN=('cardinality_min');
    SCF_PROPERTY_TM_CARDINALITY_MAX=('cardinality_max');
    SCF_PROPERTY_TM_CHOICES_INCLUDE_VALUES=('choices_include_values');
    SCF_PROPERTY_TM_CHOICES_NAME=('choices_name');
    SCF_PROPERTY_TM_CHOICES_RANGE=('choices_range');
    SCF_PROPERTY_TM_CONSTRAINT_NAME=('constraint_name');
    SCF_PROPERTY_TM_CONSTRAINT_RANGE=('constraint_range');
    SCF_PROPERTY_TM_MANPATH=('manpath');
    SCF_PROPERTY_TM_NAME=('name');
    SCF_PROPERTY_TM_PG_PATTERN=('pg_pattern');
    SCF_PROPERTY_TM_REQUIRED=('required');
    SCF_PROPERTY_TM_SECTION=('section');
    SCF_PROPERTY_TM_TARGET=('target');
    SCF_PROPERTY_TM_TITLE=('title');
    SCF_PROPERTY_TM_TYPE=('type');
    SCF_PROPERTY_TM_URI=('uri');
    SCF_PROPERTY_TM_VALUE_PREFIX=('value_');
    SCF_PROPERTY_TM_VALUES_NAME=('values_name');
    SCF_PROPERTY_TM_VISIBILITY=('visibility');
    SCF_PROPERTY_TM_COMMON_NAME_PREFIX=('common_name_');
    SCF_PROPERTY_TM_DESCRIPTION_PREFIX=('description_');
    SCF_PROPERTY_TM_UNITS_PREFIX=('units_');
    SCF_TMPL_WILDCARD=('*');
    SCF_STATE_STRING_NONE=('none');
    SCF_STATE_STRING_UNINIT=('uninitialized');
    SCF_STATE_STRING_MAINT=('maintenance');
    SCF_STATE_STRING_OFFLINE=('offline');
    SCF_STATE_STRING_DISABLED=('disabled');
    SCF_STATE_STRING_ONLINE=('online');
    SCF_STATE_STRING_DEGRADED=('degraded');
    SCF_STATE_STRING_LEGACY=('legacy_run');
    SCF_NOTIFY_NAME_FMRI=('fmri');
    SCF_NOTIFY_NAME_VERSION=('version');
    SCF_NOTIFY_NAME_TSET=('tset');
    SCF_NOTIFY_PG_POSTFIX=('fmnotify');
    SCF_NOTIFY_PARAMS=('notify-params');
    SCF_NOTIFY_PARAMS_INST=('svc:/system/fm/notify-params:default');
    SCF_SVC_TRANSITION_CLASS=('ireport.os.smf.state-transition');
    SCF_NOTIFY_PARAMS_PG_TYPE=('notify_params');
    SCF_STN_PREFIX_FROM=('from-');
    SCF_STN_PREFIX_TO=('to-');
    SCF_TM_TARGET_DELEGATE_=('delegate');
    SCF_TM_TARGET_INSTANCE_=('instance');
    SCF_TM_TARGET_THIS_=('this');
    SCF_TM_VISIBILITY_HIDDEN_=('hidden');
    SCF_TM_VISIBILITY_READONLY_=('readonly');
    SCF_TM_VISIBILITY_READWRITE_=('readwrite');


Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

  Pint32_t  = ^int32_t;
  Pint64_t  = ^int64_t;
  Pnvlist_t  = ^nvlist_t;
  Pscf_count_ranges  = ^scf_count_ranges;
  Pscf_count_ranges_t  = ^scf_count_ranges_t;
  //Pscf_error  = ^scf_error;
  Pscf_error_t  = ^scf_error_t;
  Pscf_handle_t  = ^scf_handle_t;
  Pscf_instance_t  = ^scf_instance_t;
  Pscf_int_ranges  = ^scf_int_ranges;
  Pscf_int_ranges_t  = ^scf_int_ranges_t;
  Pscf_iter_t  = ^scf_iter_t;
  Pscf_pg_tmpl_t  = ^scf_pg_tmpl_t;
  Pscf_prop_tmpl_t  = ^scf_prop_tmpl_t;
  Pscf_property_t  = ^scf_property_t;
  Pscf_propertygroup_t  = ^scf_propertygroup_t;
  Pscf_scope_t  = ^scf_scope_t;
  Pscf_service_t  = ^scf_service_t;
  Pscf_simple_app_props_t  = ^scf_simple_app_props_t;
  Pscf_simple_prop_t  = ^scf_simple_prop_t;
  Pscf_snaplevel_t  = ^scf_snaplevel_t;
  Pscf_snapshot_t  = ^scf_snapshot_t;
  Pscf_time  = ^scf_time;
  Pscf_time_t  = ^scf_time_t;
  Pscf_tmpl_error_t  = ^scf_tmpl_error_t;
  Pscf_tmpl_error_type_t  = ^scf_tmpl_error_type_t;
  Pscf_tmpl_errors_t  = ^scf_tmpl_errors_t;
  PPscf_tmpl_errors_t  = ^Pscf_tmpl_errors_t;
  Pscf_transaction_entry_t  = ^scf_transaction_entry_t;
  Pscf_transaction_t  = ^scf_transaction_t;
  Pscf_type_t  = ^scf_type_t;
  Pscf_value_t  = ^scf_value_t;
  Pscf_values  = ^scf_values;
  Pscf_values_t  = ^scf_values_t;
  //Pscf_version  = ^scf_version;
  //Pscf_version_t  = ^scf_version_t;
  Psize_t  = ^size_t;
  Puint32_t  = ^uint32_t;
  Puint64_t  = ^uint64_t;
  Puint8_t  = ^uint8_t;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  Pscf_version_t = ^scf_version_t;
  scf_version_t  = record end;

{ was #define dname def_expr }
//function SCF_VERSION : scf_version_t;

{
 * Opaque structures
  }
  scf_handle_t             = record end;
  scf_scope_t              = record end;
  scf_service_t            = record end;
  scf_instance_t           = record end;
  scf_propertygroup_t      = record end;
  scf_property_t           = record end;
  scf_snapshot_t           = record end;
  scf_snaplevel_t          = record end;
  scf_transaction_t        = record end;
  scf_transaction_entry_t  = record end;
  scf_value_t              = record end;
  scf_iter_t               = record end;
  scf_pg_tmpl_t            = record end;
  scf_prop_tmpl_t          = record end;
  scf_tmpl_errors_t        = record end;
  scf_simple_app_props_t   = record end;
  scf_simple_prop_t        = record end;
{
 * Types
  }

  scf_type_t = (SCF_TYPE_INVALID := 0,SCF_TYPE_BOOLEAN,
    SCF_TYPE_COUNT,SCF_TYPE_INTEGER,SCF_TYPE_TIME,
    SCF_TYPE_ASTRING,SCF_TYPE_OPAQUE,SCF_TYPE_USTRING := 100,
    SCF_TYPE_URI := 200,SCF_TYPE_FMRI,SCF_TYPE_HOST := 300,
    SCF_TYPE_HOSTNAME,SCF_TYPE_NET_ADDR_V4,
    SCF_TYPE_NET_ADDR_V6,SCF_TYPE_NET_ADDR
    );

  scf_time = record
      t_seconds : int64_t;
      t_ns : int32_t;
    end;
  scf_time_t = scf_time;
{
 * There is no explicit initializer for this structure.  Functions
 * which set or populate this structure assume that it is either
 * uninitialized or destroyed.
  }
{ reserved for future use  }

  scf_values = record
      value_type : scf_type_t;
      reserved : pointer;
      value_count : cint;
      values_as_strings : ^pcchar;
      values : record
          case longint of
            0 : ( v_count : Puint64_t );
            1 : ( v_boolean : Puint8_t );
            2 : ( v_integer : Pint64_t );
            3 : ( v_astring : ^pcchar );
            4 : ( v_ustring : ^pcchar );
            5 : ( v_opaque : ^pcchar );
            6 : ( v_time : Pscf_time_t );
          end;
    end;
  scf_values_t = scf_values;

  scf_count_ranges = record
      scr_num_ranges : cint;
      scr_min : Puint64_t;
      scr_max : Puint64_t;
    end;
  scf_count_ranges_t = scf_count_ranges;

  scf_int_ranges = record
      sir_num_ranges : cint;
      sir_min : Pint64_t;
      sir_max : Pint64_t;
    end;
  scf_int_ranges_t = scf_int_ranges;

{ no error  }
{ handle not bound  }
{ cannot use unset argument  }
{ nothing of that name found  }
{ type does not match value  }
{ cannot modify while in-use  }
{ repository connection gone  }
{ bad argument  }
{ no memory available  }
{ required constraint not met  }
{ object already exists  }
{ repository server unavailable  }
{ server has insufficient resources  }
{ insufficient privileges for action  }
{ backend refused access  }
{ mismatched SCF handles  }
{ object bound to destroyed handle  }
{ incompatible SCF version  }
{ backend is read-only  }
{ object has been deleted  }
{ template data is invalid  }
{ user callback function failed  }
{ internal error  }

  scf_error_ = (SCF_ERROR_NONE := 1000,SCF_ERROR_NOT_BOUND,
    SCF_ERROR_NOT_SET,SCF_ERROR_NOT_FOUND,SCF_ERROR_TYPE_MISMATCH,
    SCF_ERROR_IN_USE,SCF_ERROR_CONNECTION_BROKEN,
    SCF_ERROR_INVALID_ARGUMENT,SCF_ERROR_NO_MEMORY,
    SCF_ERROR_CONSTRAINT_VIOLATED,SCF_ERROR_EXISTS,
    SCF_ERROR_NO_SERVER,SCF_ERROR_NO_RESOURCES,
    SCF_ERROR_PERMISSION_DENIED,SCF_ERROR_BACKEND_ACCESS,
    SCF_ERROR_HANDLE_MISMATCH,SCF_ERROR_HANDLE_DESTROYED,
    SCF_ERROR_VERSION_MISMATCH,SCF_ERROR_BACKEND_READONLY,
    SCF_ERROR_DELETED,SCF_ERROR_TEMPLATE_INVALID,
    SCF_ERROR_CALLBACK_FAILED := 1080,SCF_ERROR_INTERNAL := 1101
    );
  scf_error_t = scf_error_;
{
 * This enum MUST be kept in sync with
 * struct _scf_tmpl_error_desc em_desc() in scf_tmpl.c
  }
{ property group missing  }
{ property group type incorrect  }
{ missing required property  }
{ property type incorrect  }
{ wrong number of values  }
{ constraint violated for value  }
{ value violated specified range  }
{ global or restarter pg_pattern  }
{ redefined by the instance  }
{ property and value type mismatch  }
{ value is out of range in template  }
{ value is not valid for the  }
{ template  }
{ pg_pattern conflicts with higher  }
{ level definition  }
{ prop_pattern conflicts with higher  }
{ level definition  }
{ global or restarter template  }
{ redefined  }
{ No supporting constraints or  }
{ values for include_values  }
{ Required pg_pattern is missing  }
{ name or type attribute.  }
{ Required prop_pattern is  }
{ missing a type attribute.  }

  scf_tmpl_error_type_ = (SCF_TERR_MISSING_PG,SCF_TERR_WRONG_PG_TYPE,
    SCF_TERR_MISSING_PROP,SCF_TERR_WRONG_PROP_TYPE,
    SCF_TERR_CARDINALITY_VIOLATION,SCF_TERR_VALUE_CONSTRAINT_VIOLATED,
    SCF_TERR_RANGE_VIOLATION,SCF_TERR_PG_REDEFINE,
    SCF_TERR_PROP_TYPE_MISMATCH,SCF_TERR_VALUE_OUT_OF_RANGE,
    SCF_TERR_INVALID_VALUE,SCF_TERR_PG_PATTERN_CONFLICT,
    SCF_TERR_PROP_PATTERN_CONFLICT,SCF_TERR_GENERAL_REDEFINE,
    SCF_TERR_INCLUDE_VALUES,SCF_TERR_PG_PATTERN_INCOMPLETE,
    SCF_TERR_PROP_PATTERN_INCOMPLETE);
  scf_tmpl_error_type_t = scf_tmpl_error_type_;
  scf_tmpl_error_t = record end;


const
  SCF_STATE_UNINIT = $00000001;  
  SCF_STATE_MAINT = $00000002;  
  SCF_STATE_OFFLINE = $00000004;  
  SCF_STATE_DISABLED = $00000008;  
  SCF_STATE_ONLINE = $00000010;  
  SCF_STATE_DEGRADED = $00000020;  
  SCF_STATE_ALL = $0000003F;  
  SCF_PG_FLAG_NONPERSISTENT = $1;
  SCF_TRACE_LIBRARY = $1;  
  SCF_TRACE_DAEMON = $2;  
  SMF_IMMEDIATE = $1;  
  SMF_TEMPORARY = $2;  
  SMF_AT_NEXT_BOOT = $4;  

function scf_error : scf_error_t;cdecl;external External_library name 'scf_error';

function scf_strerror(_para1:scf_error_t):pcchar;cdecl;external External_library name 'scf_strerror';
function scf_limit(code:uint32_t):ssize_t;cdecl;external External_library name 'scf_limit';
const
  SCF_LIMIT_MAX_NAME_LENGTH = -(2000);  
  SCF_LIMIT_MAX_VALUE_LENGTH = -(2001);  
  SCF_LIMIT_MAX_PG_TYPE_LENGTH = -(2002);  
  SCF_LIMIT_MAX_FMRI_LENGTH = -(2003);  

function scf_handle_create(_para1 : Pscf_version_t):Pscf_handle_t;cdecl;external External_library name 'scf_handle_create';

function scf_handle_decorate(_para1:Pscf_handle_t; _para2:pcchar; _para3:Pscf_value_t):cint;cdecl;external External_library name 'scf_handle_decorate';
{ was #define dname def_expr }
function SCF_DECORATE_CLEAR : Pscf_value_t;  

function scf_handle_bind(_para1:Pscf_handle_t):cint;cdecl;external External_library name 'scf_handle_bind';
function scf_handle_unbind(_para1:Pscf_handle_t):cint;cdecl;external External_library name 'scf_handle_unbind';
procedure scf_handle_destroy(_para1:Pscf_handle_t);cdecl;external External_library name 'scf_handle_destroy';
function scf_type_base_type(_type:scf_type_t; out_:Pscf_type_t):cint;cdecl;external External_library name 'scf_type_base_type';

function scf_type_to_string(_para1:scf_type_t):pcchar;cdecl;external External_library name 'scf_type_to_string';

function scf_string_to_type(_para1:pcchar):scf_type_t;cdecl;external External_library name 'scf_string_to_type';
{ values  }
function scf_value_create(_para1:Pscf_handle_t):Pscf_value_t;cdecl;external External_library name 'scf_value_create';

function scf_value_handle(_para1:Pscf_value_t):Pscf_handle_t;cdecl;external External_library name 'scf_value_handle';
procedure scf_value_destroy(_para1:Pscf_value_t);cdecl;external External_library name 'scf_value_destroy';

function scf_value_base_type(_para1:Pscf_value_t):scf_type_t;cdecl;external External_library name 'scf_value_base_type';

function scf_value_type(_para1:Pscf_value_t):scf_type_t;cdecl;external External_library name 'scf_value_type';

function scf_value_is_type(_para1:Pscf_value_t; _para2:scf_type_t):cint;cdecl;external External_library name 'scf_value_is_type';
procedure scf_value_reset(_para1:Pscf_value_t);cdecl;external External_library name 'scf_value_reset';

function scf_value_get_boolean(_para1:Pscf_value_t; _para2:Puint8_t):cint;cdecl;external External_library name 'scf_value_get_boolean';

function scf_value_get_count(_para1:Pscf_value_t; _para2:Puint64_t):cint;cdecl;external External_library name 'scf_value_get_count';

function scf_value_get_integer(_para1:Pscf_value_t; _para2:Pint64_t):cint;cdecl;external External_library name 'scf_value_get_integer';

function scf_value_get_time(_para1:Pscf_value_t; _para2:Pint64_t; _para3:Pint32_t):cint;cdecl;external External_library name 'scf_value_get_time';

function scf_value_get_astring(_para1:Pscf_value_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_value_get_astring';

function scf_value_get_ustring(_para1:Pscf_value_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_value_get_ustring';

function scf_value_get_opaque(_para1:Pscf_value_t; _para2:pointer; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_value_get_opaque';
procedure scf_value_set_boolean(_para1:Pscf_value_t; _para2:uint8_t);cdecl;external External_library name 'scf_value_set_boolean';
procedure scf_value_set_count(_para1:Pscf_value_t; _para2:uint64_t);cdecl;external External_library name 'scf_value_set_count';
procedure scf_value_set_integer(_para1:Pscf_value_t; _para2:int64_t);cdecl;external External_library name 'scf_value_set_integer';
function scf_value_set_time(_para1:Pscf_value_t; _para2:int64_t; _para3:int32_t):cint;cdecl;external External_library name 'scf_value_set_time';

function scf_value_set_astring(_para1:Pscf_value_t; _para2:pcchar):cint;cdecl;external External_library name 'scf_value_set_astring';

function scf_value_set_ustring(_para1:Pscf_value_t; _para2:pcchar):cint;cdecl;external External_library name 'scf_value_set_ustring';

function scf_value_set_opaque(_para1:Pscf_value_t; _para2:pointer; _para3:size_t):cint;cdecl;external External_library name 'scf_value_set_opaque';

function scf_value_get_as_string(_para1:Pscf_value_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_value_get_as_string';

function scf_value_get_as_string_typed(_para1:Pscf_value_t; _para2:scf_type_t; _para3:pcchar; _para4:size_t):ssize_t;cdecl;external External_library name 'scf_value_get_as_string_typed';

function scf_value_set_from_string(_para1:Pscf_value_t; _para2:scf_type_t; _para3:pcchar):cint;cdecl;external External_library name 'scf_value_set_from_string';
function scf_iter_create(_para1:Pscf_handle_t):Pscf_iter_t;cdecl;external External_library name 'scf_iter_create';

function scf_iter_handle(_para1:Pscf_iter_t):Pscf_handle_t;cdecl;external External_library name 'scf_iter_handle';
procedure scf_iter_reset(_para1:Pscf_iter_t);cdecl;external External_library name 'scf_iter_reset';
procedure scf_iter_destroy(_para1:Pscf_iter_t);cdecl;external External_library name 'scf_iter_destroy';

function scf_iter_handle_scopes(_para1:Pscf_iter_t; _para2:Pscf_handle_t):cint;cdecl;external External_library name 'scf_iter_handle_scopes';

function scf_iter_scope_services(_para1:Pscf_iter_t; _para2:Pscf_scope_t):cint;cdecl;external External_library name 'scf_iter_scope_services';

function scf_iter_service_instances(_para1:Pscf_iter_t; _para2:Pscf_service_t):cint;cdecl;external External_library name 'scf_iter_service_instances';

function scf_iter_service_pgs(_para1:Pscf_iter_t; _para2:Pscf_service_t):cint;cdecl;external External_library name 'scf_iter_service_pgs';

function scf_iter_instance_pgs(_para1:Pscf_iter_t; _para2:Pscf_instance_t):cint;cdecl;external External_library name 'scf_iter_instance_pgs';


function scf_iter_instance_pgs_composed(_para1:Pscf_iter_t; _para2:Pscf_instance_t; _para3:Pscf_snapshot_t):cint;cdecl;external External_library name 'scf_iter_instance_pgs_composed';


function scf_iter_service_pgs_typed(_para1:Pscf_iter_t; _para2:Pscf_service_t; _para3:pcchar):cint;cdecl;external External_library name 'scf_iter_service_pgs_typed';


function scf_iter_instance_pgs_typed(_para1:Pscf_iter_t; _para2:Pscf_instance_t; _para3:pcchar):cint;cdecl;external External_library name 'scf_iter_instance_pgs_typed';



function scf_iter_instance_pgs_typed_composed(_para1:Pscf_iter_t; _para2:Pscf_instance_t; _para3:Pscf_snapshot_t; _para4:pcchar):cint;cdecl;external External_library name 'scf_iter_instance_pgs_typed_composed';

function scf_iter_snaplevel_pgs(_para1:Pscf_iter_t; _para2:Pscf_snaplevel_t):cint;cdecl;external External_library name 'scf_iter_snaplevel_pgs';


function scf_iter_snaplevel_pgs_typed(_para1:Pscf_iter_t; _para2:Pscf_snaplevel_t; _para3:pcchar):cint;cdecl;external External_library name 'scf_iter_snaplevel_pgs_typed';

function scf_iter_instance_snapshots(_para1:Pscf_iter_t; _para2:Pscf_instance_t):cint;cdecl;external External_library name 'scf_iter_instance_snapshots';

function scf_iter_pg_properties(_para1:Pscf_iter_t; _para2:Pscf_propertygroup_t):cint;cdecl;external External_library name 'scf_iter_pg_properties';

function scf_iter_property_values(_para1:Pscf_iter_t; _para2:Pscf_property_t):cint;cdecl;external External_library name 'scf_iter_property_values';
function scf_iter_next_scope(_para1:Pscf_iter_t; _para2:Pscf_scope_t):cint;cdecl;external External_library name 'scf_iter_next_scope';
function scf_iter_next_service(_para1:Pscf_iter_t; _para2:Pscf_service_t):cint;cdecl;external External_library name 'scf_iter_next_service';
function scf_iter_next_instance(_para1:Pscf_iter_t; _para2:Pscf_instance_t):cint;cdecl;external External_library name 'scf_iter_next_instance';
function scf_iter_next_pg(_para1:Pscf_iter_t; _para2:Pscf_propertygroup_t):cint;cdecl;external External_library name 'scf_iter_next_pg';
function scf_iter_next_property(_para1:Pscf_iter_t; _para2:Pscf_property_t):cint;cdecl;external External_library name 'scf_iter_next_property';
function scf_iter_next_snapshot(_para1:Pscf_iter_t; _para2:Pscf_snapshot_t):cint;cdecl;external External_library name 'scf_iter_next_snapshot';
function scf_iter_next_value(_para1:Pscf_iter_t; _para2:Pscf_value_t):cint;cdecl;external External_library name 'scf_iter_next_value';
function scf_scope_create(_para1:Pscf_handle_t):Pscf_scope_t;cdecl;external External_library name 'scf_scope_create';

function scf_scope_handle(_para1:Pscf_scope_t):Pscf_handle_t;cdecl;external External_library name 'scf_scope_handle';

///* XXX eventually remove this */
//#define	scf_handle_get_local_scope(h, s) \
//	scf_handle_get_scope((h), SCF_SCOPE_LOCAL, (s))


function scf_handle_get_scope(_para1:Pscf_handle_t; _para2:pcchar; _para3:Pscf_scope_t):cint;cdecl;external External_library name 'scf_handle_get_scope';
procedure scf_scope_destroy(_para1:Pscf_scope_t);cdecl;external External_library name 'scf_scope_destroy';

function scf_scope_get_name(_para1:Pscf_scope_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_scope_get_name';

function scf_scope_to_fmri(_para1:Pscf_scope_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_scope_to_fmri';
function scf_service_create(_para1:Pscf_handle_t):Pscf_service_t;cdecl;external External_library name 'scf_service_create';

function scf_service_handle(_para1:Pscf_service_t):Pscf_handle_t;cdecl;external External_library name 'scf_service_handle';
procedure scf_service_destroy(_para1:Pscf_service_t);cdecl;external External_library name 'scf_service_destroy';

function scf_scope_get_parent(_para1:Pscf_scope_t; _para2:Pscf_scope_t):cint;cdecl;external External_library name 'scf_scope_get_parent';

function scf_service_get_name(_para1:Pscf_service_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_service_get_name';

function scf_service_to_fmri(_para1:Pscf_service_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_service_to_fmri';

function scf_service_get_parent(_para1:Pscf_service_t; _para2:Pscf_scope_t):cint;cdecl;external External_library name 'scf_service_get_parent';


function scf_scope_get_service(_para1:Pscf_scope_t; _para2:pcchar; _para3:Pscf_service_t):cint;cdecl;external External_library name 'scf_scope_get_service';


function scf_scope_add_service(_para1:Pscf_scope_t; _para2:pcchar; _para3:Pscf_service_t):cint;cdecl;external External_library name 'scf_scope_add_service';
function scf_service_delete(_para1:Pscf_service_t):cint;cdecl;external External_library name 'scf_service_delete';
function scf_instance_create(_para1:Pscf_handle_t):Pscf_instance_t;cdecl;external External_library name 'scf_instance_create';

function scf_instance_handle(_para1:Pscf_instance_t):Pscf_handle_t;cdecl;external External_library name 'scf_instance_handle';
procedure scf_instance_destroy(_para1:Pscf_instance_t);cdecl;external External_library name 'scf_instance_destroy';

function scf_instance_get_name(_para1:Pscf_instance_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_instance_get_name';

function scf_instance_to_fmri(_para1:Pscf_instance_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_instance_to_fmri';


function scf_service_get_instance(_para1:Pscf_service_t; _para2:pcchar; _para3:Pscf_instance_t):cint;cdecl;external External_library name 'scf_service_get_instance';


function scf_service_add_instance(_para1:Pscf_service_t; _para2:pcchar; _para3:Pscf_instance_t):cint;cdecl;external External_library name 'scf_service_add_instance';
function scf_instance_delete(_para1:Pscf_instance_t):cint;cdecl;external External_library name 'scf_instance_delete';
function scf_snapshot_create(_para1:Pscf_handle_t):Pscf_snapshot_t;cdecl;external External_library name 'scf_snapshot_create';

function scf_snapshot_handle(_para1:Pscf_snapshot_t):Pscf_handle_t;cdecl;external External_library name 'scf_snapshot_handle';
procedure scf_snapshot_destroy(_para1:Pscf_snapshot_t);cdecl;external External_library name 'scf_snapshot_destroy';

function scf_snapshot_get_name(_para1:Pscf_snapshot_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_snapshot_get_name';

function scf_snapshot_get_parent(_para1:Pscf_snapshot_t; _para2:Pscf_instance_t):cint;cdecl;external External_library name 'scf_snapshot_get_parent';


function scf_instance_get_snapshot(_para1:Pscf_instance_t; _para2:pcchar; _para3:Pscf_snapshot_t):cint;cdecl;external External_library name 'scf_instance_get_snapshot';
function scf_snapshot_update(_para1:Pscf_snapshot_t):cint;cdecl;external External_library name 'scf_snapshot_update';
function scf_snaplevel_create(_para1:Pscf_handle_t):Pscf_snaplevel_t;cdecl;external External_library name 'scf_snaplevel_create';

function scf_snaplevel_handle(_para1:Pscf_snaplevel_t):Pscf_handle_t;cdecl;external External_library name 'scf_snaplevel_handle';
procedure scf_snaplevel_destroy(_para1:Pscf_snaplevel_t);cdecl;external External_library name 'scf_snaplevel_destroy';

function scf_snaplevel_get_parent(_para1:Pscf_snaplevel_t; _para2:Pscf_snapshot_t):cint;cdecl;external External_library name 'scf_snaplevel_get_parent';

function scf_snaplevel_get_scope_name(_para1:Pscf_snaplevel_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_snaplevel_get_scope_name';

function scf_snaplevel_get_service_name(_para1:Pscf_snaplevel_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_snaplevel_get_service_name';

function scf_snaplevel_get_instance_name(_para1:Pscf_snaplevel_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_snaplevel_get_instance_name';


function scf_snaplevel_get_pg(_para1:Pscf_snaplevel_t; _para2:pcchar; pg:Pscf_propertygroup_t):cint;cdecl;external External_library name 'scf_snaplevel_get_pg';

function scf_snapshot_get_base_snaplevel(_para1:Pscf_snapshot_t; _para2:Pscf_snaplevel_t):cint;cdecl;external External_library name 'scf_snapshot_get_base_snaplevel';

function scf_snaplevel_get_next_snaplevel(_para1:Pscf_snaplevel_t; _para2:Pscf_snaplevel_t):cint;cdecl;external External_library name 'scf_snaplevel_get_next_snaplevel';
function scf_pg_create(_para1:Pscf_handle_t):Pscf_propertygroup_t;cdecl;external External_library name 'scf_pg_create';

function scf_pg_handle(_para1:Pscf_propertygroup_t):Pscf_handle_t;cdecl;external External_library name 'scf_pg_handle';
procedure scf_pg_destroy(_para1:Pscf_propertygroup_t);cdecl;external External_library name 'scf_pg_destroy';

function scf_pg_to_fmri(_para1:Pscf_propertygroup_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_pg_to_fmri';

function scf_pg_get_name(_para1:Pscf_propertygroup_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_pg_get_name';

function scf_pg_get_type(_para1:Pscf_propertygroup_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_pg_get_type';

function scf_pg_get_flags(_para1:Pscf_propertygroup_t; _para2:Puint32_t):cint;cdecl;external External_library name 'scf_pg_get_flags';

function scf_pg_get_parent_service(_para1:Pscf_propertygroup_t; _para2:Pscf_service_t):cint;cdecl;external External_library name 'scf_pg_get_parent_service';

function scf_pg_get_parent_instance(_para1:Pscf_propertygroup_t; _para2:Pscf_instance_t):cint;cdecl;external External_library name 'scf_pg_get_parent_instance';

function scf_pg_get_parent_snaplevel(_para1:Pscf_propertygroup_t; _para2:Pscf_snaplevel_t):cint;cdecl;external External_library name 'scf_pg_get_parent_snaplevel';


function scf_service_get_pg(_para1:Pscf_service_t; _para2:pcchar; _para3:Pscf_propertygroup_t):cint;cdecl;external External_library name 'scf_service_get_pg';


function scf_instance_get_pg(_para1:Pscf_instance_t; _para2:pcchar; _para3:Pscf_propertygroup_t):cint;cdecl;external External_library name 'scf_instance_get_pg';



function scf_instance_get_pg_composed(_para1:Pscf_instance_t; _para2:Pscf_snapshot_t; _para3:pcchar; _para4:Pscf_propertygroup_t):cint;cdecl;external External_library name 'scf_instance_get_pg_composed';



function scf_service_add_pg(_para1:Pscf_service_t; _para2:pcchar; _para3:pcchar; _para4:uint32_t; _para5:Pscf_propertygroup_t):cint;cdecl;external External_library name 'scf_service_add_pg';



function scf_instance_add_pg(_para1:Pscf_instance_t; _para2:pcchar; _para3:pcchar; _para4:uint32_t; _para5:Pscf_propertygroup_t):cint;cdecl;external External_library name 'scf_instance_add_pg';
function scf_pg_delete(_para1:Pscf_propertygroup_t):cint;cdecl;external External_library name 'scf_pg_delete';

function scf_pg_get_underlying_pg(_para1:Pscf_propertygroup_t; _para2:Pscf_propertygroup_t):cint;cdecl;external External_library name 'scf_pg_get_underlying_pg';

function scf_instance_get_parent(_para1:Pscf_instance_t; _para2:Pscf_service_t):cint;cdecl;external External_library name 'scf_instance_get_parent';
function scf_pg_update(_para1:Pscf_propertygroup_t):cint;cdecl;external External_library name 'scf_pg_update';
function scf_property_create(_para1:Pscf_handle_t):Pscf_property_t;cdecl;external External_library name 'scf_property_create';

function scf_property_handle(_para1:Pscf_property_t):Pscf_handle_t;cdecl;external External_library name 'scf_property_handle';
procedure scf_property_destroy(_para1:Pscf_property_t);cdecl;external External_library name 'scf_property_destroy';

function scf_property_is_type(_para1:Pscf_property_t; _para2:scf_type_t):cint;cdecl;external External_library name 'scf_property_is_type';

function scf_property_type(_para1:Pscf_property_t; _para2:Pscf_type_t):cint;cdecl;external External_library name 'scf_property_type';

function scf_property_get_name(_para1:Pscf_property_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_property_get_name';

function scf_property_get_value(_para1:Pscf_property_t; _para2:Pscf_value_t):cint;cdecl;external External_library name 'scf_property_get_value';

function scf_property_to_fmri(_para1:Pscf_property_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_property_to_fmri';


function scf_pg_get_property(_para1:Pscf_propertygroup_t; _para2:pcchar; _para3:Pscf_property_t):cint;cdecl;external External_library name 'scf_pg_get_property';
function scf_transaction_create(_para1:Pscf_handle_t):Pscf_transaction_t;cdecl;external External_library name 'scf_transaction_create';

function scf_transaction_handle(_para1:Pscf_transaction_t):Pscf_handle_t;cdecl;external External_library name 'scf_transaction_handle';
function scf_transaction_start(_para1:Pscf_transaction_t; _para2:Pscf_propertygroup_t):cint;cdecl;external External_library name 'scf_transaction_start';
procedure scf_transaction_destroy(_para1:Pscf_transaction_t);cdecl;external External_library name 'scf_transaction_destroy';
procedure scf_transaction_destroy_children(_para1:Pscf_transaction_t);cdecl;external External_library name 'scf_transaction_destroy_children';
procedure scf_transaction_reset(_para1:Pscf_transaction_t);cdecl;external External_library name 'scf_transaction_reset';
procedure scf_transaction_reset_all(_para1:Pscf_transaction_t);cdecl;external External_library name 'scf_transaction_reset_all';
function scf_transaction_commit(_para1:Pscf_transaction_t):cint;cdecl;external External_library name 'scf_transaction_commit';
function scf_entry_create(_para1:Pscf_handle_t):Pscf_transaction_entry_t;cdecl;external External_library name 'scf_entry_create';

function scf_entry_handle(_para1:Pscf_transaction_entry_t):Pscf_handle_t;cdecl;external External_library name 'scf_entry_handle';
procedure scf_entry_reset(_para1:Pscf_transaction_entry_t);cdecl;external External_library name 'scf_entry_reset';
procedure scf_entry_destroy(_para1:Pscf_transaction_entry_t);cdecl;external External_library name 'scf_entry_destroy';
procedure scf_entry_destroy_children(_para1:Pscf_transaction_entry_t);cdecl;external External_library name 'scf_entry_destroy_children';

function scf_transaction_property_change(_para1:Pscf_transaction_t; _para2:Pscf_transaction_entry_t; _para3:pcchar; _para4:scf_type_t):cint;cdecl;external External_library name 'scf_transaction_property_change';

function scf_transaction_property_delete(_para1:Pscf_transaction_t; _para2:Pscf_transaction_entry_t; _para3:pcchar):cint;cdecl;external External_library name 'scf_transaction_property_delete';

function scf_transaction_property_new(_para1:Pscf_transaction_t; _para2:Pscf_transaction_entry_t; _para3:pcchar; _para4:scf_type_t):cint;cdecl;external External_library name 'scf_transaction_property_new';

function scf_transaction_property_change_type(_para1:Pscf_transaction_t; _para2:Pscf_transaction_entry_t; _para3:pcchar; _para4:scf_type_t):cint;cdecl;external External_library name 'scf_transaction_property_change_type';
function scf_entry_add_value(_para1:Pscf_transaction_entry_t; _para2:Pscf_value_t):cint;cdecl;external External_library name 'scf_entry_add_value';

function scf_handle_decode_fmri(_para1:Pscf_handle_t; _para2:pcchar; _para3:Pscf_scope_t; _para4:Pscf_service_t; _para5:Pscf_instance_t; 
           _para6:Pscf_propertygroup_t; _para7:Pscf_property_t; _para8:cint):cint;cdecl;external External_library name 'scf_handle_decode_fmri';
const
  SCF_DECODE_FMRI_EXACT = $00000001;  
  SCF_DECODE_FMRI_TRUNCATE = $00000002;  
  SCF_DECODE_FMRI_REQUIRE_INSTANCE = $00000004;  
  SCF_DECODE_FMRI_REQUIRE_NO_INSTANCE = $00000008;  

function scf_myname(_para1:Pscf_handle_t; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_myname';
{
 * Property group template interfaces.
  }
function scf_tmpl_pg_create(_para1:Pscf_handle_t):Pscf_pg_tmpl_t;cdecl;external External_library name 'scf_tmpl_pg_create';
procedure scf_tmpl_pg_destroy(_para1:Pscf_pg_tmpl_t);cdecl;external External_library name 'scf_tmpl_pg_destroy';
procedure scf_tmpl_pg_reset(_para1:Pscf_pg_tmpl_t);cdecl;external External_library name 'scf_tmpl_pg_reset';
function scf_tmpl_get_by_pg(_para1:Pscf_propertygroup_t; _para2:Pscf_pg_tmpl_t; _para3:cint):cint;cdecl;external External_library name 'scf_tmpl_get_by_pg';




function scf_tmpl_get_by_pg_name(_para1:pcchar; _para2:pcchar; _para3:pcchar; _para4:pcchar; _para5:Pscf_pg_tmpl_t; 
           _para6:cint):cint;cdecl;external External_library name 'scf_tmpl_get_by_pg_name';



function scf_tmpl_iter_pgs(_para1:Pscf_pg_tmpl_t; _para2:pcchar; _para3:pcchar; _para4:pcchar; _para5:cint):cint;cdecl;external External_library name 'scf_tmpl_iter_pgs';
const
  SCF_PG_TMPL_FLAG_REQUIRED = $1;  
  SCF_PG_TMPL_FLAG_EXACT = $2;  
  SCF_PG_TMPL_FLAG_CURRENT = $4;  


function scf_tmpl_pg_name(_para1:Pscf_pg_tmpl_t; _para2:Ppcchar):ssize_t;cdecl;external External_library name 'scf_tmpl_pg_name';


function scf_tmpl_pg_common_name(_para1:Pscf_pg_tmpl_t; _para2:pcchar; _para3:Ppcchar):ssize_t;cdecl;external External_library name 'scf_tmpl_pg_common_name';


function scf_tmpl_pg_description(_para1:Pscf_pg_tmpl_t; _para2:pcchar; _para3:Ppcchar):ssize_t;cdecl;external External_library name 'scf_tmpl_pg_description';

function scf_tmpl_pg_type(_para1:Pscf_pg_tmpl_t; _para2:Ppcchar):ssize_t;cdecl;external External_library name 'scf_tmpl_pg_type';

function scf_tmpl_pg_target(_para1:Pscf_pg_tmpl_t; _para2:Ppcchar):ssize_t;cdecl;external External_library name 'scf_tmpl_pg_target';

function scf_tmpl_pg_required(_para1:Pscf_pg_tmpl_t; _para2:Puint8_t):cint;cdecl;external External_library name 'scf_tmpl_pg_required';
{
 * Property template interfaces.
  }
function scf_tmpl_prop_create(_para1:Pscf_handle_t):Pscf_prop_tmpl_t;cdecl;external External_library name 'scf_tmpl_prop_create';
procedure scf_tmpl_prop_destroy(_para1:Pscf_prop_tmpl_t);cdecl;external External_library name 'scf_tmpl_prop_destroy';
procedure scf_tmpl_prop_reset(_para1:Pscf_prop_tmpl_t);cdecl;external External_library name 'scf_tmpl_prop_reset';

function scf_tmpl_get_by_prop(_para1:Pscf_pg_tmpl_t; _para2:pcchar; _para3:Pscf_prop_tmpl_t; _para4:cint):cint;cdecl;external External_library name 'scf_tmpl_get_by_prop';
function scf_tmpl_iter_props(_para1:Pscf_pg_tmpl_t; _para2:Pscf_prop_tmpl_t; _para3:cint):cint;cdecl;external External_library name 'scf_tmpl_iter_props';
const
  SCF_PROP_TMPL_FLAG_REQUIRED = $1;  


function scf_tmpl_prop_name(_para1:Pscf_prop_tmpl_t; _para2:Ppcchar):ssize_t;cdecl;external External_library name 'scf_tmpl_prop_name';

function scf_tmpl_prop_type(_para1:Pscf_prop_tmpl_t; _para2:Pscf_type_t):cint;cdecl;external External_library name 'scf_tmpl_prop_type';

function scf_tmpl_prop_required(_para1:Pscf_prop_tmpl_t; _para2:Puint8_t):cint;cdecl;external External_library name 'scf_tmpl_prop_required';


function scf_tmpl_prop_common_name(_para1:Pscf_prop_tmpl_t; _para2:pcchar; _para3:Ppcchar):ssize_t;cdecl;external External_library name 'scf_tmpl_prop_common_name';


function scf_tmpl_prop_description(_para1:Pscf_prop_tmpl_t; _para2:pcchar; _para3:Ppcchar):ssize_t;cdecl;external External_library name 'scf_tmpl_prop_description';


function scf_tmpl_prop_units(_para1:Pscf_prop_tmpl_t; _para2:pcchar; _para3:Ppcchar):ssize_t;cdecl;external External_library name 'scf_tmpl_prop_units';

function scf_tmpl_prop_cardinality(prop:Pscf_prop_tmpl_t; _para2:Puint64_t; _para3:Puint64_t):cint;cdecl;external External_library name 'scf_tmpl_prop_cardinality';

function scf_tmpl_prop_internal_seps(_para1:Pscf_prop_tmpl_t; _para2:Pscf_values_t):cint;cdecl;external External_library name 'scf_tmpl_prop_internal_seps';

function scf_tmpl_prop_visibility(_para1:Pscf_prop_tmpl_t; _para2:Puint8_t):cint;cdecl;external External_library name 'scf_tmpl_prop_visibility';
const
  SCF_TMPL_VISIBILITY_HIDDEN = 1;  
  SCF_TMPL_VISIBILITY_READONLY = 2;  
  SCF_TMPL_VISIBILITY_READWRITE = 3;
type
  scf_simple_walk_cb = function (_para1:Pscf_handle_t; _para2:Pscf_instance_t; _para3:pointer):cint; cdecl;


function scf_tmpl_visibility_to_string(_para1:uint8_t):pcchar;cdecl;external External_library name 'scf_tmpl_visibility_to_string';


function scf_tmpl_value_name_constraints(prop:Pscf_prop_tmpl_t; vals:Pscf_values_t):cint;cdecl;external External_library name 'scf_tmpl_value_name_constraints';
procedure scf_count_ranges_destroy(_para1:Pscf_count_ranges_t);cdecl;external External_library name 'scf_count_ranges_destroy';
procedure scf_int_ranges_destroy(_para1:Pscf_int_ranges_t);cdecl;external External_library name 'scf_int_ranges_destroy';

function scf_tmpl_value_count_range_constraints(_para1:Pscf_prop_tmpl_t; _para2:Pscf_count_ranges_t):cint;cdecl;external External_library name 'scf_tmpl_value_count_range_constraints';

function scf_tmpl_value_int_range_constraints(_para1:Pscf_prop_tmpl_t; _para2:Pscf_int_ranges_t):cint;cdecl;external External_library name 'scf_tmpl_value_int_range_constraints';

function scf_tmpl_value_count_range_choices(_para1:Pscf_prop_tmpl_t; _para2:Pscf_count_ranges_t):cint;cdecl;external External_library name 'scf_tmpl_value_count_range_choices';

function scf_tmpl_value_int_range_choices(_para1:Pscf_prop_tmpl_t; _para2:Pscf_int_ranges_t):cint;cdecl;external External_library name 'scf_tmpl_value_int_range_choices';

function scf_tmpl_value_name_choices(prop:Pscf_prop_tmpl_t; vals:Pscf_values_t):cint;cdecl;external External_library name 'scf_tmpl_value_name_choices';
procedure scf_values_destroy(_para1:Pscf_values_t);cdecl;external External_library name 'scf_values_destroy';



function scf_tmpl_value_common_name(_para1:Pscf_prop_tmpl_t; _para2:pcchar; _para3:pcchar; _para4:Ppcchar):ssize_t;cdecl;external External_library name 'scf_tmpl_value_common_name';



function scf_tmpl_value_description(_para1:Pscf_prop_tmpl_t; _para2:pcchar; _para3:pcchar; _para4:Ppcchar):ssize_t;cdecl;external External_library name 'scf_tmpl_value_description';

function scf_tmpl_value_in_constraint(pt:Pscf_prop_tmpl_t; value:Pscf_value_t; errs:PPscf_tmpl_errors_t):cint;cdecl;external External_library name 'scf_tmpl_value_in_constraint';
{
 * Template validation interfaces
  }


function scf_tmpl_validate_fmri(_para1:Pscf_handle_t; _para2:pcchar; _para3:pcchar; _para4:PPscf_tmpl_errors_t; _para5:cint):cint;cdecl;external External_library name 'scf_tmpl_validate_fmri';
const
  SCF_TMPL_VALIDATE_FLAG_CURRENT = $1;  

procedure scf_tmpl_errors_destroy(errs:Pscf_tmpl_errors_t);cdecl;external External_library name 'scf_tmpl_errors_destroy';
function scf_tmpl_next_error(_para1:Pscf_tmpl_errors_t):Pscf_tmpl_error_t;cdecl;external External_library name 'scf_tmpl_next_error';
procedure scf_tmpl_reset_errors(errs:Pscf_tmpl_errors_t);cdecl;external External_library name 'scf_tmpl_reset_errors';
function scf_tmpl_strerror(err:Pscf_tmpl_error_t; s:pcchar; n:size_t; flag:cint):cint;cdecl;external External_library name 'scf_tmpl_strerror';

function scf_tmpl_error_source_fmri(_para1:Pscf_tmpl_error_t; _para2:Ppcchar):cint;cdecl;external External_library name 'scf_tmpl_error_source_fmri';

function scf_tmpl_error_type(_para1:Pscf_tmpl_error_t; _para2:Pscf_tmpl_error_type_t):cint;cdecl;external External_library name 'scf_tmpl_error_type';

function scf_tmpl_error_pg_tmpl(_para1:Pscf_tmpl_error_t; _para2:Ppcchar; _para3:Ppcchar):cint;cdecl;external External_library name 'scf_tmpl_error_pg_tmpl';

function scf_tmpl_error_pg(_para1:Pscf_tmpl_error_t; _para2:Ppcchar; _para3:Ppcchar):cint;cdecl;external External_library name 'scf_tmpl_error_pg';

function scf_tmpl_error_prop_tmpl(_para1:Pscf_tmpl_error_t; _para2:Ppcchar; _para3:Ppcchar):cint;cdecl;external External_library name 'scf_tmpl_error_prop_tmpl';

function scf_tmpl_error_prop(_para1:Pscf_tmpl_error_t; _para2:Ppcchar; _para3:Ppcchar):cint;cdecl;external External_library name 'scf_tmpl_error_prop';

function scf_tmpl_error_value(_para1:Pscf_tmpl_error_t; _para2:Ppcchar):cint;cdecl;external External_library name 'scf_tmpl_error_value';
{
 * Simplified calls
  }

function smf_enable_instance(_para1:pcchar; _para2:cint):cint;cdecl;external External_library name 'smf_enable_instance';

function smf_disable_instance(_para1:pcchar; _para2:cint):cint;cdecl;external External_library name 'smf_disable_instance';

function smf_refresh_instance(_para1:pcchar):cint;cdecl;external External_library name 'smf_refresh_instance';

function smf_restart_instance(_para1:pcchar):cint;cdecl;external External_library name 'smf_restart_instance';

function smf_maintain_instance(_para1:pcchar; _para2:cint):cint;cdecl;external External_library name 'smf_maintain_instance';

function smf_degrade_instance(_para1:pcchar; _para2:cint):cint;cdecl;external External_library name 'smf_degrade_instance';

function smf_restore_instance(_para1:pcchar):cint;cdecl;external External_library name 'smf_restore_instance';

function smf_get_state(_para1:pcchar):pcchar;cdecl;external External_library name 'smf_get_state';
function scf_simple_walk_instances(_para1:uint_t; _para2:pointer; inst_callback:scf_simple_walk_cb):cint;cdecl;external External_library name 'scf_simple_walk_instances';



function scf_simple_prop_get(_para1:Pscf_handle_t; _para2:pcchar; _para3:pcchar; _para4:pcchar):Pscf_simple_prop_t;cdecl;external External_library name 'scf_simple_prop_get';
procedure scf_simple_prop_free(_para1:Pscf_simple_prop_t);cdecl;external External_library name 'scf_simple_prop_free';

function scf_simple_app_props_get(_para1:Pscf_handle_t; _para2:pcchar):Pscf_simple_app_props_t;cdecl;external External_library name 'scf_simple_app_props_get';
procedure scf_simple_app_props_free(_para1:Pscf_simple_app_props_t);cdecl;external External_library name 'scf_simple_app_props_free';


function scf_simple_app_props_next(_para1:Pscf_simple_app_props_t; _para2:Pscf_simple_prop_t):Pscf_simple_prop_t;cdecl;external External_library name 'scf_simple_app_props_next';




function scf_simple_app_props_search(_para1:Pscf_simple_app_props_t; _para2:pcchar; _para3:pcchar):Pscf_simple_prop_t;cdecl;external External_library name 'scf_simple_app_props_search';

function scf_simple_prop_numvalues(_para1:Pscf_simple_prop_t):ssize_t;cdecl;external External_library name 'scf_simple_prop_numvalues';

function scf_simple_prop_type(_para1:Pscf_simple_prop_t):scf_type_t;cdecl;external External_library name 'scf_simple_prop_type';

function scf_simple_prop_name(_para1:Pscf_simple_prop_t):pcchar;cdecl;external External_library name 'scf_simple_prop_name';

function scf_simple_prop_pgname(_para1:Pscf_simple_prop_t):pcchar;cdecl;external External_library name 'scf_simple_prop_pgname';
function scf_simple_prop_next_boolean(_para1:Pscf_simple_prop_t):Puint8_t;cdecl;external External_library name 'scf_simple_prop_next_boolean';
function scf_simple_prop_next_count(_para1:Pscf_simple_prop_t):Puint64_t;cdecl;external External_library name 'scf_simple_prop_next_count';
function scf_simple_prop_next_integer(_para1:Pscf_simple_prop_t):Pint64_t;cdecl;external External_library name 'scf_simple_prop_next_integer';
function scf_simple_prop_next_time(_para1:Pscf_simple_prop_t; _para2:Pint32_t):Pint64_t;cdecl;external External_library name 'scf_simple_prop_next_time';
function scf_simple_prop_next_astring(_para1:Pscf_simple_prop_t):pcchar;cdecl;external External_library name 'scf_simple_prop_next_astring';
function scf_simple_prop_next_ustring(_para1:Pscf_simple_prop_t):pcchar;cdecl;external External_library name 'scf_simple_prop_next_ustring';
function scf_simple_prop_next_opaque(_para1:Pscf_simple_prop_t; _para2:Psize_t):pointer;cdecl;external External_library name 'scf_simple_prop_next_opaque';
procedure scf_simple_prop_next_reset(_para1:Pscf_simple_prop_t);cdecl;external External_library name 'scf_simple_prop_next_reset';
{
 * smf_state_from_string()
 * return SCF_STATE_* value for the input
 * -1 on error. String "all" maps to SCF_STATE_ALL macro
  }

function smf_state_from_string(_para1:pcchar):int32_t;cdecl;external External_library name 'smf_state_from_string';
{
 * smf_state_to_string()
 * return SCF_STATE_STRING* value for the input
 * NULL on error.
  }

function smf_state_to_string(_para1:int32_t):pcchar;cdecl;external External_library name 'smf_state_to_string';
{
 * Notification interfaces
  }

function smf_notify_set_params(_para1:pcchar; _para2:Pnvlist_t):cint;cdecl;external External_library name 'smf_notify_set_params';
function smf_notify_get_params(_para1:PPnvlist_t; _para2:Pnvlist_t):cint;cdecl;external External_library name 'smf_notify_get_params';


function smf_notify_del_params(_para1:pcchar; _para2:pcchar; _para3:int32_t):cint;cdecl;external External_library name 'smf_notify_del_params';
{
 * SMF exit status definitions
  }
const
  SMF_EXIT_OK = 0;  
  SMF_EXIT_ERR_FATAL = 95;  
  SMF_EXIT_ERR_CONFIG = 96;  
  SMF_EXIT_MON_DEGRADE = 97;  
  SMF_EXIT_MON_OFFLINE = 98;  
  SMF_EXIT_ERR_NOSMF = 99;  
  SMF_EXIT_ERR_PERM = 100;


{ private headers }

const
    SCF_PG_GENERAL_TYPE = SCF_GROUP_FRAMEWORK;
    SCF_PG_GENERAL_FLAGS = 0;
    SCF_PG_GENERAL_OVR_TYPE = SCF_GROUP_FRAMEWORK;
    SCF_PG_GENERAL_OVR_FLAGS = SCF_PG_FLAG_NONPERSISTENT;
    SCF_PG_DEATHROW_TYPE = SCF_GROUP_FRAMEWORK;
    SCF_PG_DEATHROW_FLAGS = SCF_PG_FLAG_NONPERSISTENT;
    SCF_PG_OPTIONS_TYPE = SCF_GROUP_FRAMEWORK;
    SCF_PG_OPTIONS_FLAGS = 0;
    SCF_PG_OPTIONS_OVR_TYPE = SCF_GROUP_FRAMEWORK;
    SCF_PG_OPTIONS_OVR_FLAGS = SCF_PG_FLAG_NONPERSISTENT;
    SCF_PG_RESTARTER_TYPE = SCF_GROUP_FRAMEWORK;
    SCF_PG_RESTARTER_FLAGS = SCF_PG_FLAG_NONPERSISTENT;
    SCF_PG_RESTARTER_ACTIONS_TYPE = SCF_GROUP_FRAMEWORK;
    SCF_PG_RESTARTER_ACTIONS_FLAGS = SCF_PG_FLAG_NONPERSISTENT;

    SCF_FMRI_TYPE_SVC = $1;
    SCF_FMRI_TYPE_FILE = $2;
  {
   * Strings for use in constructing FMRIs
    }
    SCF_FMRI_SVC_PREFIX = 'svc:';
    SCF_FMRI_FILE_PREFIX = 'file:';
    SCF_FMRI_SCOPE_PREFIX = '//';
    SCF_FMRI_LOCAL_SCOPE = 'localhost';
    SCF_FMRI_SCOPE_SUFFIX = '@localhost';
    SCF_FMRI_SERVICE_PREFIX = '/';
    SCF_FMRI_INSTANCE_PREFIX = ':';
    SCF_FMRI_PROPERTYGRP_PREFIX = '/:properties/';
    SCF_FMRI_PROPERTY_PREFIX = '/';
    SCF_FMRI_LEGACY_PREFIX = 'lrc:';

    SCF_PROPERTY_CLEAR = ('maint_off');
    SCF_PROPERTY_MAINTENANCE =('maint_on');
    SCF_PROPERTY_LOGFILE =('logfile');
    SCF_PROPERTY_ALT_LOGFILE =('alt_logfile');
    SCF_LEGACY_SERVICE =('smf/legacy_run');
    SCF_LEGACY_PROPERTY_NAME =('name');
    SCF_LEGACY_PROPERTY_INODE =('inode');
    SCF_LEGACY_PROPERTY_SUFFIX =('suffix');
    SVC_SULOGIN_FMRI =('svc:/system/sulogin');
    SCF_NOTIFY_PARAMS_SOURCE_NAME =('preference_source');

type
  { can be SCF_DECORATE_CLEAR  }
    Pscf_decoration_info = ^scf_decoration_info;
    scf_decoration_info = record
        sdi_name : pcchar;
        sdi_type : scf_type_t;
        sdi_value : Pscf_value_t;
      end;
    scf_decoration_info_t = scf_decoration_info;
    Pscf_decoration_info_t = ^scf_decoration_info_t;


Pscf_decoration_func = function (_para1:Pscf_decoration_info_t; _para2:pointer):cint;cdecl;
{
* calls a callback function for each decoration on the handle.  If the
* callback returns 0, the iteration stops and returns 0.  If the callback
* returns a non-zero value, the iteration continues.  After full completion,
* 1 is returned.  On error, -1 is returned.
}
function _scf_handle_decorations(_para1:Pscf_handle_t; _para2:Pscf_decoration_func; _para3:Pscf_value_t; _para4:pointer):cint;cdecl;external External_library name '_scf_handle_decorations';
{
 * wait for a change to the propertygroup -- may return early.
 * For now, only one of these can be outstanding at a time.
 *
 * The second argument is how long, in seconds, to wait for a response.
 *
 * Returns SCF_COMPLETE on timeout, -1 on error, and SCF_SUCCESS in every
 * other case.  You must call scf_pg_update() to see if the object has
 * actually changed.
  }
function _scf_pg_wait(_para1:Pscf_propertygroup_t; _para2:cint):cint;cdecl;external External_library name '_scf_pg_wait';
{
 * set up notifications for changes to a class of property groups (by name
 * and type)
 *
 * Only one thread can be sleeping in _scf_notify_wait() -- others will
 * fail.  Deletions give an fmri in the output path.
 *
 * These do not survive unbind()->bind() -- in fact, that is currently the
 * only way to clear them.
  }

function _scf_notify_add_pgname(_para1:Pscf_handle_t; _para2:pcchar):cint;cdecl;external External_library name '_scf_notify_add_pgname';

function _scf_notify_add_pgtype(_para1:Pscf_handle_t; _para2:pcchar):cint;cdecl;external External_library name '_scf_notify_add_pgtype';
function _scf_notify_wait(_para1:Pscf_propertygroup_t; _para2:pcchar; _para3:size_t):cint;cdecl;external External_library name '_scf_notify_wait';
{
 * Internal interfaces for snapshot creation:
 *	_scf_snapshot_take_new(), _scf_snapshot_take_new_named(), and
 *	_scf_snapshot_take_attach() create a set of snaplevels
 *	containing frozen versions of both the instance's property groups and
 *	its parent service's property groups. _scf_snapshot_take_new() and
 *	_scf_snapshot_take_new_named() create a new snapshot to which the
 *	new snaplevels are attached, while _scf_snapshot_take_attach()
 *	attaches the new snaplevels to a pre-existing snapshot.
 *
 *	_scf_snapshot_take_new_named() records the passed in names into the
 *	snaplevel instead of the instance and service name.  This creates
 *	an inconsistency, which should be resolved by using
 *	_scf_snapshot_attach() to attach the new snaplevels to a snapshot
 *	underneath the appropriate instance.  The first snapshot can
 *	then be deleted.
 *
 *	_scf_snapshot_attach(snap1, snap2) points snap2 at the snaplevels
 *	pointed to by snap1.  After a call to either
 *	_scf_snapshot_take_attach(snap1, snap2) or
 *	_scf_snapshot_attach(inst, snap), scf_snapshot_update() will be
 *	required for any open references to snap or snap2 to see the new
 *	snaplevels.
 *
 *	_scf_snapshot_delete() deletes the snapshot object.  While
 *	snaplevels, being only loosely connected to snapshots, stay
 *	around until they are no longer referenced, any references *through
 *	this snapshot object* will be invalidated.
 *
 * _scf_snapshot_take_new() can fail with at least _HANDLE_MISMATCH,
 * _CONNECTION_BROKEN, _INVALID_ARGUMENT, _NO_RESOURCES, _PERMISSION_DENIED,
 * _NOT_SET, _EXISTS.
 *
 * _scf_snapshot_take_new_named() can fail with at least _HANDLE_MISMATCH,
 * _CONNECTION_BROKEN, _INVALID_ARGUMENT, _NO_RESOURCES, _PERMISSION_DENIED,
 * _NOT_SET, _EXISTS.
 *
 * _scf_snapshot_take_attach() can fail with _CONNECTION_BROKEN, _NOT_SET,
 * _PERMISSION_DENIED, _NO_RESOURCES, _INVALID_ARGUMENT.
 *
 * _scf_snapshot_attach() can fail with _HANDLE_MISMATCH, _CONNECTION_BROKEN,
 * _NOT_SET, _NO_RESOURCES, _PERMISSION_DENIED.
  }

function _scf_snapshot_take_new(_para1:Pscf_instance_t; _para2:pcchar; _para3:Pscf_snapshot_t):cint;cdecl;external External_library name '_scf_snapshot_take_new';



function _scf_snapshot_take_new_named(_para1:Pscf_instance_t; _para2:pcchar; _para3:pcchar; _para4:pcchar; _para5:Pscf_snapshot_t):cint;cdecl;external External_library name '_scf_snapshot_take_new_named';
function _scf_snapshot_take_attach(_para1:Pscf_instance_t; _para2:Pscf_snapshot_t):cint;cdecl;external External_library name '_scf_snapshot_take_attach';
function _scf_snapshot_attach(_para1:Pscf_snapshot_t; _para2:Pscf_snapshot_t):cint;cdecl;external External_library name '_scf_snapshot_attach';
function _scf_snapshot_delete(_para1:Pscf_snapshot_t):cint;cdecl;external External_library name '_scf_snapshot_delete';
{
 * Destructively portions up the first argument into the different portions
 * of a svc: fmri, and returns pointers to the applicable portions.  Omitted
 * portions are set to NULL, except for the scope, which is set to the
 * default local scope if not specified.
 *
 * Parsing is attempted in the order of: svc:, file:. The identified type
 * of the service is returned in the second argument and may take a value
 * of: SCF_FMRI_TYPE_SVC or SCF_FMRI_TYPE_FILE.
 *
 * Note that some of the returned pointers (in particular the scope) may not
 * point into the passed buffer.
  }





function scf_parse_fmri(_para1:pcchar; _para2:pcint; _para3:Ppcchar; _para4:Ppcchar; _para5:Ppcchar;
           _para6:Ppcchar; _para7:Ppcchar):cint;cdecl;external External_library name 'scf_parse_fmri';





function scf_parse_svc_fmri(_para1:pcchar; _para2:Ppcchar; _para3:Ppcchar; _para4:Ppcchar; _para5:Ppcchar;
           _para6:Ppcchar):cint;cdecl;external External_library name 'scf_parse_svc_fmri';


function scf_parse_file_fmri(fmri:pcchar; scope:Ppcchar; path:Ppcchar):cint;cdecl;external External_library name 'scf_parse_file_fmri';

function scf_canonify_fmri(_para1:pcchar; _para2:pcchar; _para3:size_t):ssize_t;cdecl;external External_library name 'scf_canonify_fmri';
function _smf_refresh_instance_i(_para1:Pscf_instance_t):cint;cdecl;external External_library name '_smf_refresh_instance_i';

type
  Pscf_simple_handle = ^scf_simple_handle;
  scf_simple_handle = record
      h : Pscf_handle_t;
      snap : Pscf_snapshot_t;
      inst : Pscf_instance_t;
      running_pg : Pscf_propertygroup_t;
      editing_pg : Pscf_propertygroup_t;
    end;
  scf_simple_handle_t = scf_simple_handle;
  Pscf_simple_handle_t = ^scf_simple_handle_t;

procedure scf_simple_handle_destroy(_para1:Pscf_simple_handle_t);cdecl;external External_library name 'scf_simple_handle_destroy';


function scf_general_pg_setup(_para1:pcchar; _para2:pcchar):Pscf_simple_handle_t;cdecl;external External_library name 'scf_general_pg_setup';
function scf_transaction_setup(_para1:Pscf_simple_handle_t):Pscf_transaction_t;cdecl;external External_library name 'scf_transaction_setup';
function scf_transaction_restart(_para1:Pscf_simple_handle_t; _para2:Pscf_transaction_t):cint;cdecl;external External_library name 'scf_transaction_restart';
function scf_read_count_property(_para1:Pscf_simple_handle_t; _para2:pcchar; _para3:Puint64_t):cint;cdecl;external External_library name 'scf_read_count_property';
function scf_set_count_property(_para1:Pscf_transaction_t; _para2:pcchar; _para3:uint64_t; _para4:boolean_t):cint;cdecl;external External_library name 'scf_set_count_property';
{
 * Walks all the instances matching a given fmri list.  Each fmri in the array
 * can be one of the following:
 *
 * 	- Full instance name
 * 	- Full service name
 * 	- Full property group or property name
 * 	- Partial service or instance name
 * 	- A globbed pattern
 *
 * The matching rules for partial fmris are a slightly more complex.  We allow
 * for any substring anchored at the end of the instance or service name,
 * provided it begins with a complete element in the fmri.  For example, given
 * the fmri "svc:/system/filesystem/local:default", any of the following would
 * be acceptable matches: 'default', 'local', 'local:default',
 * 'filesystem/local'.  The following would not be acceptable:
 * 'system/filesystem', 'filesystem/loc', 'system/local'.  Possible flag values:
 *
 * 	SCF_WALK_MULTIPLE	Allow individual arguments to correspond to
 * 				multiple instances.
 *
 * 	SCF_WALK_LEGACY		Walk legacy services (indicated by a non-NULL
 * 				propery group).
 *
 * 	SCF_WALK_SERVICE	If the user specifies a service, pass the
 * 				service to the callback without iterating over
 * 				its instances.
 *
 * 	SCF_WALK_PROPERTY	Allow FMRIs which match property groups or
 * 				individual properties.  Incompatible with
 * 				SCF_WALK_LEGACY.
 *
 * 	SCF_WALK_NOINSTANCE	Walk only services.  Must be used in
 * 				conjunction with SCF_WALK_SERVICE.
 *
 * 	SCF_WALK_EXPLICIT	Walk only services if the match is exact
 *				else return instances. Must be used in
 *				conjunction with SCF_WALK_SERVICE.
 *
 * If no arguments are given, then all instances in the service graph are
 * walked.
 *
 * The second to last parameter is set to UU_EXIT_FATAL if one of the arguments
 * is an invalid FMRI or matches multiple FMRIs when SCF_WALK_MULTIPLE is not
 * set.
 *
 * The last parameter is a user-supplied error function that is called when
 * reporting invalid arguments.
  }

const
  SCF_WALK_MULTIPLE = $01;
  SCF_WALK_LEGACY = $02;
  SCF_WALK_SERVICE = $04;
  SCF_WALK_PROPERTY = $08;
  SCF_WALK_NOINSTANCE = $10;
  SCF_WALK_EXPLICIT = $20;
{
 * The default locations of the repository dbs
  }
  REPOSITORY_DB = '/etc/svc/repository.db';
  NONPERSIST_DB = '/etc/svc/volatile/svc_nonpersist.db';
  FAST_REPOSITORY_DB = '/etc/svc/volatile/fast_repository.db';
  REPOSITORY_CHECKPOINT = '/etc/svc/volatile/checkpoint_repository.db';

{ svcprop special  }
type
  Pscf_walkinfo = ^scf_walkinfo;
  scf_walkinfo = record
      fmri : pcchar;
      scope : Pscf_scope_t;
      svc : Pscf_service_t;
      inst : Pscf_instance_t;
      pg : Pscf_propertygroup_t;
      prop : Pscf_property_t;
      count : cint;
    end;
  scf_walkinfo_t = scf_walkinfo;
  Pscf_walkinfo_t = ^scf_walkinfo_t;

  scf_walk_callback  = function  (_para1:pointer; _para2:Pscf_walkinfo_t):cint;cdecl;
  scf_error_callback = procedure (_para1: PChar ; _para2: array of const);

function scf_walk_fmri(_para1:Pscf_handle_t; _para2:cint; _para3:Ppcchar; _para4:cint; _para5:scf_walk_callback; _para6:pointer; _para7:pcint; _para8:scf_error_callback):scf_error_t;cdecl;external External_library name 'scf_walk_fmri';

{
 * Requests a backup of the repository with a particular name, which
 * can be any alphabetic string.  Only privileged users can do this.
 *
 * Can fail with:
 *	_NOT_BOUND, _CONNECTION_BROKEN, _PERMISSION_DENIED, _INVALID_ARGUMENT,
 *	_INTERNAL (path too long, or the backup failed for an odd reason),
 *	_BACKEND_READONLY (filesystem is still read-only)
  }

function _scf_request_backup(_para1:Pscf_handle_t; _para2:pcchar):cint;cdecl;external External_library name '_scf_request_backup';
{
 * Repository switch client
  }
function _scf_repository_switch(_para1:Pscf_handle_t; _para2:cint):cint;cdecl;external External_library name '_scf_repository_switch';
{
 * Determines whether a property group requires authorization to read; this
 * does not in any way reflect whether the caller has that authorization.
 * To determine that, the caller must attempt to read the value of one of the
 * group's properties.
 *
 * Can fail with:
 *	_NOT_BOUND, _CONNECTION_BROKEN, _INVALID_ARGUMENT, _INTERNAL,
 *	_NO_RESOURCES, _CONSTRAINT_VIOLATED, _DELETED.
  }

function _scf_pg_is_read_protected(_para1:Pscf_propertygroup_t; _para2:Pboolean_t):cint;cdecl;external External_library name '_scf_pg_is_read_protected';
{
 * Sets annotation data for SMF audit logging.  Once this function has been
 * set, the next audit record will be preceded by an ADT_smf_annotation
 * with the information provided in this function.  This function is used
 * to mark operations which comprise multiple primitive operations such as
 * svccfg import.
  }


function _scf_set_annotation(h:Pscf_handle_t; operation:pcchar; file_:pcchar):cint;cdecl;external External_library name '_scf_set_annotation';
{
 * scf_pattern_t
  }
{ Uninitialized state  }
{ Original argument  }
{ List of matches  }
{ # of matches  }
type
  Pscf_pattern = ^scf_pattern;
  scf_match    = record end;
  Pscf_match   = ^scf_match;
  scf_pattern = record
      sp_type : (PATTERN_INVALID,PATTERN_EXACT,PATTERN_GLOB,
        PATTERN_PARTIAL);
      sp_arg : pcchar;
      sp_matches : Pscf_match;
      sp_matchcount : cint;
    end;
  scf_pattern_t = scf_pattern;
  Pscf_pattern_t = ^scf_pattern_t;

function scf_cmp_pattern(_para1:pcchar; _para2:Pscf_pattern_t):cint;cdecl;external External_library name 'scf_cmp_pattern';


function gen_filenms_from_fmri(_para1:pcchar; _para2:pcchar; _para3:pcchar; _para4:pcchar):cint;cdecl;external External_library name 'gen_filenms_from_fmri';
{
 * Interfaces for bulk access to SMF-stored configuration.
 *
 * Each scf_propvec_t represents a single property to be read (with
 * scf_read_propvec) or written (with scf_write_propvec).
 *
 * The fields of a scf_propvec_t have the following meanings:
 *
 *   pv_prop - the name of the property
 *   pv_desc - a description string (optional; to be consumed by the caller)
 *   pv_type - the type of the property
 *   pv_ptr  - where to store the data read, or a pointer to the data to
 *             be written
 *   pv_aux  - additional data influencing the interpretation of pv_ptr
 *
 * The meaning of pv_ptr and pv_aux depends on the type of property.  For:
 *
 *   boolean - if pv_aux is 0, pv_ptr is a pointer to a boolean_t
 *             if pv_aux is non-0, pv_ptr is a pointer to a uint64_t,
 *             where pv_aux indicates the bit holding the truth value.
 *   count   - pv_ptr is a pointer to a uint64_t; pv_aux is unused
 *   integer - pv_ptr is a pointer to an int64_t; pv_aux is unused
 *   time    - pv_ptr is a pointer to an scf_time_t; pv_aux is unused
 *   opaque  - pv_ptr is a pointer to an scf_opaque_t; pv_aux is unused
 *   strings - (scf_read_propvec) pv_ptr is a pointer to a char *
 *             (scf_write_propvec) pv_ptr is a pointer to an array of char
 *             (both) pv_aux is unused
  }
type
  Pscf_opaque_t = ^scf_opaque_t;
  scf_opaque_t = record
      so_addr : pointer;
      so_size : size_t;
    end;



  Pscf_propvec_t  = ^scf_propvec_t;
  PPscf_propvec_t = ^Pscf_propvec_t;
  scf_propvec_t = record
      pv_prop : pcchar;
      pv_desc : pcchar;
      pv_type : scf_type_t;
      pv_ptr : pointer;
      pv_aux : uint64_t;
    end;

procedure scf_clean_propvec(_para1:Pscf_propvec_t);cdecl;external External_library name 'scf_clean_propvec';


function scf_read_propvec(_para1:pcchar; _para2:pcchar; _para3:boolean_t; _para4:Pscf_propvec_t; _para5:PPscf_propvec_t):cint;cdecl;external External_library name 'scf_read_propvec';


function scf_write_propvec(_para1:pcchar; _para2:pcchar; _para3:Pscf_propvec_t; _para4:PPscf_propvec_t):cint;cdecl;external External_library name 'scf_write_propvec';

function _scf_create_errors(_para1:pcchar; _para2:cint):Pscf_tmpl_errors_t;cdecl;external External_library name '_scf_create_errors';










function _scf_tmpl_add_error(errs:Pscf_tmpl_errors_t; _type:scf_tmpl_error_type_t; pg_name:pcchar; prop_name:pcchar; ev1:pcchar;
           ev2:pcchar; actual:pcchar; tmpl_fmri:pcchar; tmpl_pg_name:pcchar; tmpl_pg_type:pcchar;
           tmpl_prop_name:pcchar; tmpl_prop_type:pcchar):cint;cdecl;external External_library name '_scf_tmpl_add_error';

function _scf_tmpl_error_set_prefix(_para1:Pscf_tmpl_errors_t; _para2:pcchar):cint;cdecl;external External_library name '_scf_tmpl_error_set_prefix';
{
 * Templates definitions
  }
{
 * For CARDINALITY_VIOLATION and RANGE_VIOLATION, te_ev1 holds
 * the min value and te_ev2 holds the max value
 *
 * For MISSING_PG te_ev1 should hold the expected pg_name and
 * expected2 holds the expected pg_type.
 *
 * For SCF_TERR_PG_PATTERN_CONFLICT and SCF_TERR_GENERAL_REDEFINE te_ev1 is
 * the FMRI holding the conflicting pg_pattern.  te_ev2 is the name of the
 * conflicting pg_pattern, and actual is the type of the conflicting
 * pg_pattern.
 *
 * SCF_TERR_PROP_PATTERN_CONFLICT te_ev1 is the FMRI holding the
 * conflicting prop_pattern.  te_ev2 is the name of the conflicting
 * prop_pattern, and actual is the type of the conflicting prop_pattern.
 *
 * For SCF_TERR_INCLUDE_VALUES te_ev1 is the type specified for the
 * include_values element.
 *
 * For all other errors, te_ev1 should hold the expected value and
 * te_ev2 is ignored
 *
 * te_actual holds the current value of the property
  }

type
  Pscf_tmpl_error = ^scf_tmpl_error;
  scf_tmpl_error = record
      te_errs : Pscf_tmpl_errors_t;
      te_type : scf_tmpl_error_type_t;
      te_pg_name : pcchar;
      te_prop_name : pcchar;
      te_ev1 : pcchar;
      te_ev2 : pcchar;
      te_actual : pcchar;
      te_tmpl_fmri : pcchar;
      te_tmpl_pg_name : pcchar;
      te_tmpl_pg_type : pcchar;
      te_tmpl_prop_name : pcchar;
      te_tmpl_prop_type : pcchar;
    end;

{
 * The pg_pattern element has two optional attributes that play a part in
 * selecting the appropriate prefix for the name of the pg_pattern property
 * group. The two attributes are name and type.  The appropriate prefix
 * encodes the presence are absence of these attributes.
 *
 *	SCF_PG_TM_PG_PATTERN_PREFIX	neither attribute
 *	SCF_PG_TM_PG_PATTERN_N_PREFIX	name only
 *	SCF_PG_TM_PG_PATTERN_T_PREFIX	type only
 *	SCF_PG_TM_PG_PATTERN_NT_PREFIX	both name and type
  }

const
  SCF_PG_TM_PG_PAT_BASE = 'tm_pgpat';
  SCF_PG_TM_PG_PATTERN_PREFIX = 'tm_pgpat_';
  SCF_PG_TM_PG_PATTERN_N_PREFIX = 'tm_pgpatn_';
  SCF_PG_TM_PG_PATTERN_T_PREFIX = 'tm_pgpatt_';
  SCF_PG_TM_PG_PATTERN_NT_PREFIX = 'tm_pgpatnt_';
  SCF_PG_TM_PROP_PATTERN_PREFIX = 'tm_proppat_';
{
 * Pad character to use when encoding strings for property names.
  }
  SCF_ENCODE32_PAD = '-';
{
 * Functions for base 32 encoding/decoding
  }


function scf_decode32(_para1:pcchar; _para2:size_t; _para3:pcchar; _para4:size_t; _para5:Psize_t;
           _para6:cchar):cint;cdecl;external External_library name 'scf_decode32';

function scf_encode32(_para1:pcchar; _para2:size_t; _para3:pcchar; _para4:size_t; _para5:Psize_t;
           _para6:cchar):cint;cdecl;external External_library name 'scf_encode32';
{
 * handy functions
  }
{
 * _scf_sanitize_locale
 * Make sure a locale string has only alpha-numeric or '_' characters
  }
procedure _scf_sanitize_locale(_para1:pcchar);cdecl;external External_library name '_scf_sanitize_locale';
{
 * _scf_read_tmpl_prop_type_as_string()
 * Handy function to get template property type as a string
  }

function _scf_read_tmpl_prop_type_as_string(_para1:Pscf_prop_tmpl_t):pcchar;cdecl;external External_library name '_scf_read_tmpl_prop_type_as_string';
{
 * _scf_read_single_astring_from_pg()
 * Given a property group (pg) and a property name (pn), this function
 * retrives an astring value from pg/pn.
  }

function _scf_read_single_astring_from_pg(_para1:Pscf_propertygroup_t; _para2:pcchar):pcchar;cdecl;external External_library name '_scf_read_single_astring_from_pg';
{
 * scf_instance_delete_prop()
 * Given instance, property group, and property, delete the property.
  }


function scf_instance_delete_prop(_para1:Pscf_instance_t; _para2:pcchar; _para3:pcchar):cint;cdecl;external External_library name 'scf_instance_delete_prop';
{
 * Functions to extract boot config information from FMRI_BOOT_CONFIG
  }
procedure scf_get_boot_config(_para1:Puint8_t);cdecl;external External_library name 'scf_get_boot_config';
procedure scf_get_boot_config_ovr(_para1:Puint8_t);cdecl;external External_library name 'scf_get_boot_config_ovr';
function scf_is_fastboot_default:cint;cdecl;external External_library name 'scf_is_fastboot_default';
{
 * Set value of "config_ovr/fastreboot_default".
  }
function scf_fastreboot_default_set_transient(_para1:boolean_t):cint;cdecl;external External_library name 'scf_fastreboot_default_set_transient';
{
 * scf_is_compatible_type()
 * Return true if the second type is the same type, or a subtype of the
 * first.
  }
function scf_is_compatible_type(_para1:scf_type_t; _para2:scf_type_t):cint;cdecl;external External_library name 'scf_is_compatible_type';
{
 * Check an array of services and enable any that don't have the
 * "application/auto_enable" property set to "false", which is
 * the interface to turn off this behaviour (see PSARC 2004/739).
  }
procedure _check_services(_para1:Ppcchar);cdecl;external External_library name '_check_services';
{
 * _scf_handle_create_and_bind()
 * convenience function that creates and binds a handle
  }
function _scf_handle_create_and_bind(_para1:scf_version_t):Pscf_handle_t;cdecl;external External_library name '_scf_handle_create_and_bind';
{
 * _smf_refresh_all_instances()
 * refresh all intances of a service
 * return SCF_SUCCESS or SCF_FAILED on _PERMISSION_DENIED, _BACKEND_ACCESS
 * or _BACKEND_READONLY.
  }
function _smf_refresh_all_instances(_para1:Pscf_service_t):cint;cdecl;external External_library name '_smf_refresh_all_instances';
{
 * _scf_get_fma_notify_params()
 * Specialized fuction to get fma notifitation parameters
  }

function _scf_get_fma_notify_params(_para1:pcchar; _para2:Pnvlist_t; _para3:cint):cint;cdecl;external External_library name '_scf_get_fma_notify_params';
{
 * _scf_get_svc_notify_params()
 * Specialized function to get SMF state transition notification parameters
  }

function _scf_get_svc_notify_params(_para1:pcchar; _para2:Pnvlist_t; _para3:int32_t; _para4:cint; _para5:cint):cint;cdecl;external External_library name '_scf_get_svc_notify_params';
{
 * _scf_notify_get_params()
 * Specialized function to get notification parametes from a pg into an
 * nvlist_t
  }
function _scf_notify_get_params(_para1:Pscf_propertygroup_t; _para2:Pnvlist_t):cint;cdecl;external External_library name '_scf_notify_get_params';




implementation

function SCF_TRANS_SHIFT_INITIAL_STATE(s : longint) : longint;
begin
  SCF_TRANS_SHIFT_INITIAL_STATE:=s shl 16;
end;

function SCF_TRANSITION_ALL : longint; { return type might be wrong }
  begin
    SCF_TRANSITION_ALL:=(SCF_TRANS_SHIFT_INITIAL_STATE(SCF_STATE_ALL)) or SCF_STATE_ALL;
  end;

function SCF_TRANS(f,t : longint) : longint;
begin
  SCF_TRANS:=(SCF_TRANS_SHIFT_INITIAL_STATE(f)) or t;
end;

//function SCF_TRANS_VALID(t : longint) : longint;
//begin
//  SCF_TRANS_VALID:= not (t(@( not (SCF_TRANSITION_ALL))));
//end;

function SCF_TRANS_INITIAL_STATE(t : longint) : longint;
begin
  SCF_TRANS_INITIAL_STATE:=(t shr 16) and SCF_STATE_ALL;
end;

//function SCF_TRANS_FINAL_STATE(t : longint) : t;
//begin
//  SCF_TRANS_FINAL_STATE:=t(@(SCF_STATE_ALL));
//end;

function SCF_DECORATE_CLEAR : Pscf_value_t;
begin
  SCF_DECORATE_CLEAR:=Pscf_value_t(0);
end;

//function scf_handle_get_local_scope(h,s : longint) : longint;
//begin
//  scf_handle_get_local_scope:=scf_handle_get_scope(h,SCF_SCOPE_LOCAL,s);
//end;


end.
