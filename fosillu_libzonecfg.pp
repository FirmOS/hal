unit fosillu_libzonecfg;
interface

uses
  ctypes,fos_illumos_defs,fosillu_mnttab,Unix,unixtype,Sockets;

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
 * Copyright (c) 2003, 2010, Oracle and/or its affiliates. All rights reserved.
  }
{
 * Zone configuration header file.
  }

const
  External_library=''; {Setup as you need}
  {$LINKLIB libzonecfg.so.1}

const

  ZONE_ID_UNDEFINED : zoneid_t = -(1);
  Z_OK = 0;              { XML doc root element is null  }
  Z_EMPTY_DOCUMENT = 1;  { top-level XML doc element != zone  }
  Z_WRONG_DOC_TYPE = 2;  { libxml-level property problem  }
  Z_BAD_PROPERTY = 3;    { problem creating temporary file  }
  Z_TEMP_FILE = 4;       { libxml error saving or validating  }
  Z_SAVING_FILE = 5;     { no such entry  }
  Z_NO_ENTRY = 6;        { illegal zone name  }
  Z_BOGUS_ZONE_NAME = 7; { required resource missing  }
  Z_REQD_RESOURCE_MISSING = 8; { required property missing  }
  Z_REQD_PROPERTY_MISSING = 9; { bad document handle  }
  Z_BAD_HANDLE = 10;           { out of memory (like ENOMEM)  }
  Z_NOMEM = 11;                { invalid argument (like EINVAL)  }
  Z_INVAL = 12;                { permission denied (like EACCES)  }
  Z_ACCES = 13;                { string won't fit in char array  }
  Z_TOO_BIG = 14;              { miscellaneous file-system error  }
  Z_MISC_FS = 15;              { no such zone  }
  Z_NO_ZONE = 16;              { no/wrong resource type  }
  Z_NO_RESOURCE_TYPE = 17;     { no/wrong resource id  }
  Z_NO_RESOURCE_ID = 18;       { no/wrong property type  }
  Z_NO_PROPERTY_TYPE = 19;     { no/wrong property id  }
  Z_NO_PROPERTY_ID = 20;       { zone state invalid for given task  }
  Z_BAD_ZONE_STATE = 21;       { libxml can't validate against DTD  }
  Z_INVALID_DOCUMENT = 22;     { zone name already in use (rename)  }
  Z_NAME_IN_USE = 23;          { delete_index: no old ID  }
  Z_NO_SUCH_ID = 24;           { add/modify/delete_index problem  }
  Z_UPDATING_INDEX = 25;       { problem locking index file  }
  Z_LOCKING_FILE = 26;         { problem unlocking index file  }
  Z_UNLOCKING_FILE = 27;       { consult errno instead  }
  Z_SYSTEM = 28;               { resource insufficiently specified  }
  Z_INSUFFICIENT_SPEC = 29;    { resolved path mismatch  }
  Z_RESOLVED_PATH = 34;        { IPv6 address prefix length needed  }
  Z_IPV6_ADDR_PREFIX_LEN = 35; { not IPv[4|6] address or host name  }
  Z_BOGUS_ADDRESS = 36;        { specified privilege is prohibited  }
  Z_PRIV_PROHIBITED = 37;      { required privilege is missing  }
  Z_PRIV_REQUIRED = 38;        { specified privilege is unknown  }
  Z_PRIV_UNKNOWN = 39;         { brand-specific error  }
  Z_BRAND_ERROR = 40;          { incompatible settings  }
  Z_INCOMPATIBLE = 41;         { rctl alias disallowed  }
  Z_ALIAS_DISALLOW = 42;       { clear property disallowed  }
  Z_CLEAR_DISALLOW = 43;       { generic libpool error  }
  Z_POOL = 44;                 { pool service not enabled  }
  Z_POOLS_NOT_ACTIVE = 45;     { pools enable failed  }
  Z_POOL_ENABLE = 46;          { no such pool configured  }
  Z_NO_POOL = 47;              { pool create failed  }
  Z_POOL_CREATE = 48;          { pool bind failed  }
  Z_POOL_BIND = 49;            { invalid property value  }
  Z_INVALID_PROPERTY = 50;

{
 * Warning: these are shared with the admin/install consolidation.
 * Do not insert states between any of the currently defined states,
 * and any new states must be evaluated for impact on range comparisons.
  }
  ZONE_STATE_CONFIGURED = 0;
  ZONE_STATE_INCOMPLETE = 1;
  ZONE_STATE_INSTALLED = 2;
  ZONE_STATE_READY = 3;
  ZONE_STATE_RUNNING = 4;
  ZONE_STATE_SHUTTING_DOWN = 5;
  ZONE_STATE_DOWN = 6;
  ZONE_STATE_MOUNTED = 7;
  ZONE_STATE_MAXSTRLEN = 14;
  LIBZONECFG_PATH = 'libzonecfg.so.1';
  ZONE_CONFIG_ROOT = '/etc/zones';
  ZONE_INDEX_FILE = '/etc/zones/index';

  MAXAUTHS = 4096;
  ZONE_MGMT_PROF = 'Zone Management';
{ Owner, group, and mode (defined by packaging) for the config directory  }
{ root  }
  ZONE_CONFIG_UID = 0;
{ sys  }
  ZONE_CONFIG_GID = 3;
  ZONE_CONFIG_MODE = 0755;
{ Owner, group, and mode (defined by packaging) for the index file  }
{ root  }
  ZONE_INDEX_UID = 0;
{ sys  }
  ZONE_INDEX_GID = 3;
  ZONE_INDEX_MODE = 0644;
{ The maximum length of the VERSION string in the pkginfo(4) file.  }
  ZONE_PKG_VERSMAX = 256;
{
 * Shortened alias names for the zones rctls.
  }
  ALIAS_MAXLWPS      = 'max-lwps';
  ALIAS_MAXSHMMEM    = 'max-shm-memory';
  ALIAS_MAXSHMIDS    = 'max-shm-ids';
  ALIAS_MAXMSGIDS    = 'max-msg-ids';
  ALIAS_MAXSEMIDS    = 'max-sem-ids';
  ALIAS_MAXLOCKEDMEM = 'locked';
  ALIAS_MAXSWAP      = 'swap';
  ALIAS_SHARES       = 'cpu-shares';
  ALIAS_CPUCAP       = 'cpu-cap';
  ALIAS_MAXPROCS     = 'max-processes';
{ Default name for zone detached manifest  }
  ZONE_DETACHED = 'SUNWdetached.xml';
{
 * Bit flag definitions for passing into libzonecfg functions.
  }
  ZONE_DRY_RUN = $01;


Type
  zone_cmd_arg_t = record end;
  //Pboolean_t  = ^boolean_t;
  PFILE            = ^FILE;
  //Pint64_t  = ^int64_t;
  Plifreq          = ^lifreq;
  Pmnttab          = ^mnttab;
 // Ppriv_set_t  = ^priv_set_t;
  //Prctlblk_t        = ^rctlblk_t;
  Puint64_t         = ^uint64_t;
  //Putmpx            = ^utmpx;
  //Puu_avl_pool_t    = ^uu_avl_pool_t;
  //Puu_avl_t         = ^uu_avl_t;
  Pzone_admintab    = ^zone_admintab;
  Pzone_attrtab     = ^zone_attrtab;
  Pzone_cmd_arg_t   = ^zone_cmd_arg_t;
  Pzone_devpermtab  = ^zone_devpermtab;
  Pzone_devtab      = ^zone_devtab;
//  Pzone_dochandle    = ^zone_dochandle;
  Pzone_dstab        = ^zone_dstab;
  Pzone_fsopt        = ^zone_fsopt;
  Pzone_fsopt_t      = ^zone_fsopt_t;
  Pzone_fstab        = ^zone_fstab;
  Pzone_iptype       = ^zone_iptype;
  Pzone_iptype_t     = ^zone_iptype_t;
  Pzone_mcaptab      = ^zone_mcaptab;
  Pzone_nwiftab      = ^zone_nwiftab;
  Pzone_pkg_entry_t  = ^zone_pkg_entry_t;
  Pzone_pkgtab       = ^zone_pkgtab;
  Pzone_psettab      = ^zone_psettab;
  Pzone_rctltab      = ^zone_rctltab;
  Pzone_rctlvaltab   = ^zone_rctlvaltab;
  Pzone_state_t      = ^zone_state_t;
  Pzone_userauths    = ^zone_userauths;
  Pzone_userauths_t  = ^zone_userauths_t;
  Pzoneent           = ^zoneent;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{ sys/socket.h is required by net/if.h, which has a constant needed here  }
//{$include <sys/param.h>}
//{$include <sys/fstyp.h>}
//{$include <sys/mount.h>}
//{$include <priv.h>}
//{$include <netinet/in.h>}
//{$include <sys/socket.h>}
//{$include <net/if.h>}
//{$include <stdio.h>}
//{$include <rctl.h>}
//{$include <zone.h>}
//{$include <libbrand.h>}
//{$include <sys/uuid.h>}
//{$include <libuutil.h>}
//{$include <sys/mnttab.h>}
//{$include <limits.h>}
//{$include <utmpx.h>}

{
 * The integer field expresses the current values on a get.
 * On a put, it represents the new values if >= 0 or "don't change" if < 0.
  }
{ name of the zone  }
{ configured | incomplete | installed  }
{ path to zone storage  }
{ unique ID for zone  }
{ for doing renames  }
  zoneent = record
      zone_name    : array[0..(ZONENAME_MAX)-1] of cchar;
      zone_state   : cint;
      zone_path    : array[0..(MAXPATHLEN)-1] of cchar;
      zone_uuid    : uuid_t;
      zone_newname : array[0..(ZONENAME_MAX)-1] of cchar;
    end;

  zone_dochandle   = record end;
  zone_dochandle_t = ^zone_dochandle;

  zone_state_t = uint_t;

  zone_fsopt = record
      zone_fsopt_next : Pzone_fsopt;
      zone_fsopt_opt : array[0..(MAX_MNTOPT_STR)-1] of cchar;
    end;
  zone_fsopt_t = zone_fsopt;

{ special file  }
{ mount point  }
{ e.g. ufs  }
{ mount options  }
{ device to fsck  }
  zone_fstab = record
      zone_fs_special : array[0..(MAXPATHLEN)-1] of cchar;
      zone_fs_dir : array[0..(MAXPATHLEN)-1] of cchar;
      zone_fs_type : array[0..(FSTYPSZ)-1] of cchar;
      zone_fs_options : Pzone_fsopt_t;
      zone_fs_raw : array[0..(MAXPATHLEN)-1] of cchar;
    end;

{ shared-ip only  }
{ excl-ip only  }
  zone_nwiftab = record
      zone_nwif_address : array[0..(INET6_ADDRSTRLEN)-1] of cchar;
      zone_nwif_allowed_address : array[0..(INET6_ADDRSTRLEN)-1] of cchar;
      zone_nwif_physical : array[0..(LIFNAMSIZ)-1] of cchar;
      zone_nwif_defrouter : array[0..(INET6_ADDRSTRLEN)-1] of cchar;
    end;

  zone_devtab = record
      zone_dev_match : array[0..(MAXPATHLEN)-1] of cchar;
    end;

  zone_rctlvaltab = record
      zone_rctlval_priv : array[0..(MAXNAMELEN)-1] of cchar;
      zone_rctlval_limit : array[0..(MAXNAMELEN)-1] of cchar;
      zone_rctlval_action : array[0..(MAXNAMELEN)-1] of cchar;
      zone_rctlval_next : Pzone_rctlvaltab;
    end;

  zone_rctltab = record
      zone_rctl_name : array[0..(MAXNAMELEN)-1] of cchar;
      zone_rctl_valptr : Pzone_rctlvaltab;
    end;

  zone_attrtab = record
      zone_attr_name : array[0..(MAXNAMELEN)-1] of cchar;
      zone_attr_type : array[0..(MAXNAMELEN)-1] of cchar;
      zone_attr_value : array[0..(2*BUFSIZ)-1] of cchar;
    end;

  zone_dstab = record
      zone_dataset_name : array[0..(MAXNAMELEN)-1] of cchar;
    end;

  zone_psettab = record
      zone_ncpu_min : array[0..(MAXNAMELEN)-1] of cchar;
      zone_ncpu_max : array[0..(MAXNAMELEN)-1] of cchar;
      zone_importance : array[0..(MAXNAMELEN)-1] of cchar;
    end;

  zone_mcaptab = record
      zone_physmem_cap : array[0..(MAXNAMELEN)-1] of cchar;
    end;

  zone_pkgtab = record
      zone_pkg_name : array[0..(MAXNAMELEN)-1] of cchar;
      zone_pkg_version : array[0..(ZONE_PKG_VERSMAX)-1] of cchar;
    end;

  zone_devpermtab = record
      zone_devperm_name : array[0..(MAXPATHLEN)-1] of cchar;
      zone_devperm_uid : uid_t;
      zone_devperm_gid : gid_t;
      zone_devperm_mode : mode_t;
      zone_devperm_acl : pchar;
    end;

  zone_admintab = record
      zone_admin_user : array[0..(MAXUSERNAME)-1] of cchar;
      zone_admin_auths : array[0..(MAXAUTHS)-1] of cchar;
    end;


  zone_userauths = record
      user : array[0..(MAXUSERNAME)-1] of cchar;
      zonename : array[0..(ZONENAME_MAX)-1] of cchar;
      next : Pzone_userauths;
    end;
  zone_userauths_t = zone_userauths;

  zone_pkg_entry_t = record
      zpe_entry : uu_avl_node_t;
      zpe_name : pchar;
      zpe_vers : pchar;
    end;

  zone_iptype   = (ZS_SHARED,ZS_EXCLUSIVE);
  zone_iptype_t = zone_iptype;
{
 * Basic configuration management routines.
  }
  zoncfg_find_mounts_cb =  function (_para1:Pmnttab; _para2:pointer):cint;cdecl;

function zonecfg_init_handle:zone_dochandle_t;cdecl;external External_library name 'zonecfg_init_handle';
function zonecfg_get_handle(_para1:pchar; _para2:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_get_handle';
function zonecfg_get_snapshot_handle(_para1:pchar; _para2:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_get_snapshot_handle';
function zonecfg_get_template_handle(_para1:pchar; _para2:pchar; _para3:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_get_template_handle';
function zonecfg_get_xml_handle(_para1:pchar; _para2:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_get_xml_handle';
function zonecfg_check_handle(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_check_handle';
procedure zonecfg_fini_handle(_para1:zone_dochandle_t);cdecl;external External_library name 'zonecfg_fini_handle';
function zonecfg_destroy(_para1:pchar; _para2:boolean_t):cint;cdecl;external External_library name 'zonecfg_destroy';
function zonecfg_destroy_snapshot(_para1:pchar):cint;cdecl;external External_library name 'zonecfg_destroy_snapshot';
function zonecfg_save(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_save';
function zonecfg_create_snapshot(_para1:pchar):cint;cdecl;external External_library name 'zonecfg_create_snapshot';
function zonecfg_strerror(_para1:cint):pchar;cdecl;external External_library name 'zonecfg_strerror';
function zonecfg_access(_para1:pchar; _para2:cint):cint;cdecl;external External_library name 'zonecfg_access';
procedure zonecfg_set_root(_para1:pchar);cdecl;external External_library name 'zonecfg_set_root';
function zonecfg_get_root:pchar;cdecl;external External_library name 'zonecfg_get_root';
function zonecfg_in_alt_root:boolean_t;cdecl;external External_library name 'zonecfg_in_alt_root';
function zonecfg_num_resources(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_num_resources';
function zonecfg_del_all_resources(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_del_all_resources';
function zonecfg_valid_ncpus(_para1:pchar; _para2:pchar):boolean_t;cdecl;external External_library name 'zonecfg_valid_ncpus';
function zonecfg_valid_importance(_para1:pchar):boolean_t;cdecl;external External_library name 'zonecfg_valid_importance';
function zonecfg_str_to_bytes(_para1:pchar; _para2:Puint64_t):cint;cdecl;external External_library name 'zonecfg_str_to_bytes';
function zonecfg_valid_memlimit(_para1:pchar; _para2:Puint64_t):boolean_t;cdecl;external External_library name 'zonecfg_valid_memlimit';
function zonecfg_valid_alias_limit(_para1:pchar; _para2:pchar; _para3:Puint64_t):boolean_t;cdecl;external External_library name 'zonecfg_valid_alias_limit';
{
 * Zone name, path to zone directory, autoboot setting, pool, boot
 * arguments, and scheduling-class.
  }

function zonecfg_validate_zonename(_para1:pchar):cint;cdecl;external External_library name 'zonecfg_validate_zonename';
function zonecfg_get_name(_para1:zone_dochandle_t; _para2:pchar; _para3:size_t):cint;cdecl;external External_library name 'zonecfg_get_name';
function zonecfg_set_name(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_set_name';
function zonecfg_get_zonepath(_para1:zone_dochandle_t; _para2:pchar; _para3:size_t):cint;cdecl;external External_library name 'zonecfg_get_zonepath';
function zonecfg_set_zonepath(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_set_zonepath';
function zonecfg_get_autoboot(_para1:zone_dochandle_t; _para2:Pboolean_t):cint;cdecl;external External_library name 'zonecfg_get_autoboot';
function zonecfg_set_autoboot(_para1:zone_dochandle_t; _para2:boolean_t):cint;cdecl;external External_library name 'zonecfg_set_autoboot';
function zonecfg_get_iptype(_para1:zone_dochandle_t; _para2:Pzone_iptype_t):cint;cdecl;external External_library name 'zonecfg_get_iptype';
function zonecfg_set_iptype(_para1:zone_dochandle_t; _para2:zone_iptype_t):cint;cdecl;external External_library name 'zonecfg_set_iptype';
function zonecfg_get_pool(_para1:zone_dochandle_t; _para2:pchar; _para3:size_t):cint;cdecl;external External_library name 'zonecfg_get_pool';
function zonecfg_set_pool(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_set_pool';
function zonecfg_get_bootargs(_para1:zone_dochandle_t; _para2:pchar; _para3:size_t):cint;cdecl;external External_library name 'zonecfg_get_bootargs';
function zonecfg_set_bootargs(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_set_bootargs';
function zonecfg_get_sched_class(_para1:zone_dochandle_t; _para2:pchar; _para3:size_t):cint;cdecl;external External_library name 'zonecfg_get_sched_class';
function zonecfg_set_sched(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_set_sched';
function zonecfg_get_dflt_sched_class(_para1:zone_dochandle_t; _para2:pchar; _para3:cint):cint;cdecl;external External_library name 'zonecfg_get_dflt_sched_class';
{
 * Set/retrieve the brand for the zone
  }
function zonecfg_get_brand(_para1:zone_dochandle_t; _para2:pchar; _para3:size_t):cint;cdecl;external External_library name 'zonecfg_get_brand';
function zonecfg_set_brand(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_set_brand';
{
 * Filesystem configuration.
  }
function zonecfg_add_filesystem(_para1:zone_dochandle_t; _para2:Pzone_fstab):cint;cdecl;external External_library name 'zonecfg_add_filesystem';
function zonecfg_delete_filesystem(_para1:zone_dochandle_t; _para2:Pzone_fstab):cint;cdecl;external External_library name 'zonecfg_delete_filesystem';
function zonecfg_modify_filesystem(_para1:zone_dochandle_t; _para2:Pzone_fstab; _para3:Pzone_fstab):cint;cdecl;external External_library name 'zonecfg_modify_filesystem';
function zonecfg_lookup_filesystem(_para1:zone_dochandle_t; _para2:Pzone_fstab):cint;cdecl;external External_library name 'zonecfg_lookup_filesystem';
function zonecfg_add_fs_option(_para1:Pzone_fstab; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_add_fs_option';
function zonecfg_remove_fs_option(_para1:Pzone_fstab; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_remove_fs_option';
procedure zonecfg_free_fs_option_list(_para1:Pzone_fsopt_t);cdecl;external External_library name 'zonecfg_free_fs_option_list';


function zonecfg_find_mounts(_para1:pchar; _para2:zoncfg_find_mounts_cb; _para3:pointer):cint;cdecl;external External_library name 'zonecfg_find_mounts';
{
 * Network interface configuration.
  }
function zonecfg_add_nwif(_para1:zone_dochandle_t; _para2:Pzone_nwiftab):cint;cdecl;external External_library name 'zonecfg_add_nwif';
function zonecfg_delete_nwif(_para1:zone_dochandle_t; _para2:Pzone_nwiftab):cint;cdecl;external External_library name 'zonecfg_delete_nwif';
function zonecfg_modify_nwif(_para1:zone_dochandle_t; _para2:Pzone_nwiftab; _para3:Pzone_nwiftab):cint;cdecl;external External_library name 'zonecfg_modify_nwif';
function zonecfg_lookup_nwif(_para1:zone_dochandle_t; _para2:Pzone_nwiftab):cint;cdecl;external External_library name 'zonecfg_lookup_nwif';
{
 * Hostid emulation configuration.
  }
function zonecfg_get_hostid(_para1:zone_dochandle_t; _para2:pchar; _para3:size_t):cint;cdecl;external External_library name 'zonecfg_get_hostid';

function zonecfg_set_hostid(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_set_hostid';
{
 * Allowed FS mounts configuration.
  }
function zonecfg_get_fs_allowed(_para1:zone_dochandle_t; _para2:pchar; _para3:size_t):cint;cdecl;external External_library name 'zonecfg_get_fs_allowed';

function zonecfg_set_fs_allowed(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_set_fs_allowed';
{
 * Device configuration and rule matching.
  }
function zonecfg_add_dev(_para1:zone_dochandle_t; _para2:Pzone_devtab):cint;cdecl;external External_library name 'zonecfg_add_dev';
function zonecfg_delete_dev(_para1:zone_dochandle_t; _para2:Pzone_devtab):cint;cdecl;external External_library name 'zonecfg_delete_dev';
function zonecfg_modify_dev(_para1:zone_dochandle_t; _para2:Pzone_devtab; _para3:Pzone_devtab):cint;cdecl;external External_library name 'zonecfg_modify_dev';
function zonecfg_lookup_dev(_para1:zone_dochandle_t; _para2:Pzone_devtab):cint;cdecl;external External_library name 'zonecfg_lookup_dev';
{
 * Resource control configuration.
  }
function zonecfg_add_rctl(_para1:zone_dochandle_t; _para2:Pzone_rctltab):cint;cdecl;external External_library name 'zonecfg_add_rctl';
function zonecfg_delete_rctl(_para1:zone_dochandle_t; _para2:Pzone_rctltab):cint;cdecl;external External_library name 'zonecfg_delete_rctl';
function zonecfg_modify_rctl(_para1:zone_dochandle_t; _para2:Pzone_rctltab; _para3:Pzone_rctltab):cint;cdecl;external External_library name 'zonecfg_modify_rctl';
function zonecfg_lookup_rctl(_para1:zone_dochandle_t; _para2:Pzone_rctltab):cint;cdecl;external External_library name 'zonecfg_lookup_rctl';
function zonecfg_add_rctl_value(_para1:Pzone_rctltab; _para2:Pzone_rctlvaltab):cint;cdecl;external External_library name 'zonecfg_add_rctl_value';
function zonecfg_remove_rctl_value(_para1:Pzone_rctltab; _para2:Pzone_rctlvaltab):cint;cdecl;external External_library name 'zonecfg_remove_rctl_value';
procedure zonecfg_free_rctl_value_list(_para1:Pzone_rctlvaltab);cdecl;external External_library name 'zonecfg_free_rctl_value_list';
function zonecfg_aliased_rctl_ok(_para1:zone_dochandle_t; _para2:pchar):boolean_t;cdecl;external External_library name 'zonecfg_aliased_rctl_ok';
function zonecfg_set_aliased_rctl(_para1:zone_dochandle_t; _para2:pchar; _para3:uint64_t):cint;cdecl;external External_library name 'zonecfg_set_aliased_rctl';
function zonecfg_get_aliased_rctl(_para1:zone_dochandle_t; _para2:pchar; _para3:Puint64_t):cint;cdecl;external External_library name 'zonecfg_get_aliased_rctl';
function zonecfg_rm_aliased_rctl(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_rm_aliased_rctl';
function zonecfg_apply_rctls(_para1:pchar; _para2:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_apply_rctls';
{
 * Generic attribute configuration and type/value extraction.
  }
function zonecfg_add_attr(_para1:zone_dochandle_t; _para2:Pzone_attrtab):cint;cdecl;external External_library name 'zonecfg_add_attr';
function zonecfg_delete_attr(_para1:zone_dochandle_t; _para2:Pzone_attrtab):cint;cdecl;external External_library name 'zonecfg_delete_attr';
function zonecfg_modify_attr(_para1:zone_dochandle_t; _para2:Pzone_attrtab; _para3:Pzone_attrtab):cint;cdecl;external External_library name 'zonecfg_modify_attr';
function zonecfg_lookup_attr(_para1:zone_dochandle_t; _para2:Pzone_attrtab):cint;cdecl;external External_library name 'zonecfg_lookup_attr';

function zonecfg_get_attr_boolean(_para1:Pzone_attrtab; _para2:Pboolean_t):cint;cdecl;external External_library name 'zonecfg_get_attr_boolean';

function zonecfg_get_attr_int(_para1:Pzone_attrtab; _para2:Pint64_t):cint;cdecl;external External_library name 'zonecfg_get_attr_int';

function zonecfg_get_attr_string(_para1:Pzone_attrtab; _para2:pchar; _para3:size_t):cint;cdecl;external External_library name 'zonecfg_get_attr_string';

function zonecfg_get_attr_uint(_para1:Pzone_attrtab; _para2:Puint64_t):cint;cdecl;external External_library name 'zonecfg_get_attr_uint';
{
 * ZFS configuration.
  }
function zonecfg_add_ds(_para1:zone_dochandle_t; _para2:Pzone_dstab):cint;cdecl;external External_library name 'zonecfg_add_ds';
function zonecfg_delete_ds(_para1:zone_dochandle_t; _para2:Pzone_dstab):cint;cdecl;external External_library name 'zonecfg_delete_ds';
function zonecfg_modify_ds(_para1:zone_dochandle_t; _para2:Pzone_dstab; _para3:Pzone_dstab):cint;cdecl;external External_library name 'zonecfg_modify_ds';
function zonecfg_lookup_ds(_para1:zone_dochandle_t; _para2:Pzone_dstab):cint;cdecl;external External_library name 'zonecfg_lookup_ds';
{
 * cpu-set configuration.
  }
function zonecfg_add_pset(_para1:zone_dochandle_t; _para2:Pzone_psettab):cint;cdecl;external External_library name 'zonecfg_add_pset';
function zonecfg_delete_pset(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_delete_pset';
function zonecfg_modify_pset(_para1:zone_dochandle_t; _para2:Pzone_psettab):cint;cdecl;external External_library name 'zonecfg_modify_pset';
function zonecfg_lookup_pset(_para1:zone_dochandle_t; _para2:Pzone_psettab):cint;cdecl;external External_library name 'zonecfg_lookup_pset';
{
 * mem-cap configuration.
  }
function zonecfg_delete_mcap(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_delete_mcap';
function zonecfg_modify_mcap(_para1:zone_dochandle_t; _para2:Pzone_mcaptab):cint;cdecl;external External_library name 'zonecfg_modify_mcap';
function zonecfg_lookup_mcap(_para1:zone_dochandle_t; _para2:Pzone_mcaptab):cint;cdecl;external External_library name 'zonecfg_lookup_mcap';
{
 * Temporary pool support functions.
  }
function zonecfg_destroy_tmp_pool(_para1:pchar; _para2:pchar; _para3:cint):cint;cdecl;external External_library name 'zonecfg_destroy_tmp_pool';
function zonecfg_bind_tmp_pool(_para1:zone_dochandle_t; _para2:zoneid_t; _para3:pchar; _para4:cint):cint;cdecl;external External_library name 'zonecfg_bind_tmp_pool';
function zonecfg_bind_pool(_para1:zone_dochandle_t; _para2:zoneid_t; _para3:pchar; _para4:cint):cint;cdecl;external External_library name 'zonecfg_bind_pool';
function zonecfg_warn_poold(_para1:zone_dochandle_t):boolean_t;cdecl;external External_library name 'zonecfg_warn_poold';
function zonecfg_get_poolname(_para1:zone_dochandle_t; _para2:pchar; _para3:pchar; _para4:size_t):cint;cdecl;external External_library name 'zonecfg_get_poolname';
{
 * Miscellaneous utility functions.
  }
function zonecfg_enable_rcapd(_para1:pchar; _para2:cint):cint;cdecl;external External_library name 'zonecfg_enable_rcapd';
{
 * attach/detach support.
  }



function zonecfg_get_attach_handle(_para1:pchar; _para2:pchar; _para3:pchar; _para4:boolean_t; _para5:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_get_attach_handle';
function zonecfg_attach_manifest(_para1:cint; _para2:zone_dochandle_t; _para3:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_attach_manifest';
function zonecfg_detach_save(_para1:zone_dochandle_t; _para2:uint_t):cint;cdecl;external External_library name 'zonecfg_detach_save';

function zonecfg_detached(_para1:pchar):boolean_t;cdecl;external External_library name 'zonecfg_detached';
procedure zonecfg_rm_detached(_para1:zone_dochandle_t; forced:boolean_t);cdecl;external External_library name 'zonecfg_rm_detached';
function zonecfg_dev_manifest(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_dev_manifest';


function zonecfg_devperms_apply(_para1:zone_dochandle_t; _para2:pchar; _para3:uid_t; _para4:gid_t; _para5:mode_t;
           _para6:pchar):cint;cdecl;external External_library name 'zonecfg_devperms_apply';
procedure zonecfg_set_swinv(_para1:zone_dochandle_t);cdecl;external External_library name 'zonecfg_set_swinv';
function zonecfg_add_pkg(_para1:zone_dochandle_t; _para2:pchar; _para3:pchar):cint;cdecl;external External_library name 'zonecfg_add_pkg';
{
 * External zone verification support.
  }
function zonecfg_verify_save(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_verify_save';
{
 * '*ent' iterator routines.
  }
function zonecfg_setfsent(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_setfsent';
function zonecfg_getfsent(_para1:zone_dochandle_t; _para2:Pzone_fstab):cint;cdecl;external External_library name 'zonecfg_getfsent';
function zonecfg_endfsent(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_endfsent';
function zonecfg_setnwifent(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_setnwifent';
function zonecfg_getnwifent(_para1:zone_dochandle_t; _para2:Pzone_nwiftab):cint;cdecl;external External_library name 'zonecfg_getnwifent';
function zonecfg_endnwifent(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_endnwifent';
function zonecfg_setdevent(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_setdevent';
function zonecfg_getdevent(_para1:zone_dochandle_t; _para2:Pzone_devtab):cint;cdecl;external External_library name 'zonecfg_getdevent';
function zonecfg_enddevent(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_enddevent';
function zonecfg_setattrent(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_setattrent';
function zonecfg_getattrent(_para1:zone_dochandle_t; _para2:Pzone_attrtab):cint;cdecl;external External_library name 'zonecfg_getattrent';
function zonecfg_endattrent(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_endattrent';
function zonecfg_setrctlent(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_setrctlent';
function zonecfg_getrctlent(_para1:zone_dochandle_t; _para2:Pzone_rctltab):cint;cdecl;external External_library name 'zonecfg_getrctlent';
function zonecfg_endrctlent(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_endrctlent';
function zonecfg_setdsent(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_setdsent';
function zonecfg_getdsent(_para1:zone_dochandle_t; _para2:Pzone_dstab):cint;cdecl;external External_library name 'zonecfg_getdsent';
function zonecfg_enddsent(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_enddsent';
function zonecfg_getpsetent(_para1:zone_dochandle_t; _para2:Pzone_psettab):cint;cdecl;external External_library name 'zonecfg_getpsetent';
function zonecfg_getmcapent(_para1:zone_dochandle_t; _para2:Pzone_mcaptab):cint;cdecl;external External_library name 'zonecfg_getmcapent';
function zonecfg_getpkgdata(_para1:zone_dochandle_t; _para2:Puu_avl_pool_t; _para3:Puu_avl_t):cint;cdecl;external External_library name 'zonecfg_getpkgdata';
function zonecfg_setdevperment(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_setdevperment';
function zonecfg_getdevperment(_para1:zone_dochandle_t; _para2:Pzone_devpermtab):cint;cdecl;external External_library name 'zonecfg_getdevperment';
function zonecfg_enddevperment(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_enddevperment';
function zonecfg_setadminent(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_setadminent';
function zonecfg_getadminent(_para1:zone_dochandle_t; _para2:Pzone_admintab):cint;cdecl;external External_library name 'zonecfg_getadminent';
function zonecfg_endadminent(_para1:zone_dochandle_t):cint;cdecl;external External_library name 'zonecfg_endadminent';
{
 * Privilege-related functions.
  }

function zonecfg_default_privset(_para1:Ppriv_set_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_default_privset';
function zonecfg_get_privset(_para1:zone_dochandle_t; _para2:Ppriv_set_t; _para3:Ppchar):cint;cdecl;external External_library name 'zonecfg_get_privset';
function zonecfg_get_limitpriv(_para1:zone_dochandle_t; _para2:Ppchar):cint;cdecl;external External_library name 'zonecfg_get_limitpriv';
function zonecfg_set_limitpriv(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_set_limitpriv';
{
 * Higher-level routines.
  }
function zone_get_brand(_para1:pchar; _para2:pchar; _para3:size_t):cint;cdecl;external External_library name 'zone_get_brand';
function zone_get_rootpath(_para1:pchar; _para2:pchar; _para3:size_t):cint;cdecl;external External_library name 'zone_get_rootpath';
function zone_get_devroot(_para1:pchar; _para2:pchar; _para3:size_t):cint;cdecl;external External_library name 'zone_get_devroot';
function zone_get_zonepath(_para1:pchar; _para2:pchar; _para3:size_t):cint;cdecl;external External_library name 'zone_get_zonepath';
function zone_get_state(_para1:pchar; _para2:Pzone_state_t):cint;cdecl;external External_library name 'zone_get_state';
function zone_set_state(_para1:pchar; _para2:zone_state_t):cint;cdecl;external External_library name 'zone_set_state';
function zone_state_str(_para1:zone_state_t):pchar;cdecl;external External_library name 'zone_state_str';

function zonecfg_get_name_by_uuid(_para1:uuid_t; _para2:pchar; _para3:size_t):cint;cdecl;external External_library name 'zonecfg_get_name_by_uuid';

function zonecfg_get_uuid(_para1:pchar; var _para2:uuid_t):cint;cdecl;external External_library name 'zonecfg_get_uuid';
function zonecfg_default_brand(_para1:pchar; _para2:size_t):cint;cdecl;external External_library name 'zonecfg_default_brand';
{
 * Iterator for configured zones.
  }
function setzoneent:PFILE;cdecl;external External_library name 'setzoneent';
function getzoneent(_para1:PFILE):pchar;cdecl;external External_library name 'getzoneent';
function getzoneent_private(_para1:PFILE):Pzoneent;cdecl;external External_library name 'getzoneent_private';
procedure endzoneent(_para1:PFILE);cdecl;external External_library name 'endzoneent';
{
 * File-system-related convenience functions.
  }

function zonecfg_valid_fs_type(_para1:pchar):boolean_t;cdecl;external External_library name 'zonecfg_valid_fs_type';
{
 * Network-related convenience functions.
  }
function zonecfg_same_net_address(_para1:pchar; _para2:pchar):boolean_t;cdecl;external External_library name 'zonecfg_same_net_address';
function zonecfg_valid_net_address(_para1:pchar; _para2:Plifreq):cint;cdecl;external External_library name 'zonecfg_valid_net_address';
function zonecfg_ifname_exists(_para1:sa_family_t; _para2:pchar):boolean_t;cdecl;external External_library name 'zonecfg_ifname_exists';
{
 * Rctl-related common functions.
  }

function zonecfg_is_rctl(_para1:pchar):boolean_t;cdecl;external External_library name 'zonecfg_is_rctl';

function zonecfg_valid_rctlname(_para1:pchar):boolean_t;cdecl;external External_library name 'zonecfg_valid_rctlname';

function zonecfg_valid_rctlblk(_para1:Prctlblk_t):boolean_t;cdecl;external External_library name 'zonecfg_valid_rctlblk';


function zonecfg_valid_rctl(_para1:pchar; _para2:Prctlblk_t):boolean_t;cdecl;external External_library name 'zonecfg_valid_rctl';

function zonecfg_construct_rctlblk(_para1:Pzone_rctlvaltab; _para2:Prctlblk_t):cint;cdecl;external External_library name 'zonecfg_construct_rctlblk';
{
 * Live Upgrade support functions.  Shared between ON and install gate.
  }

function zonecfg_open_scratch(_para1:pchar; _para2:boolean_t):PFILE;cdecl;external External_library name 'zonecfg_open_scratch';
function zonecfg_lock_scratch(_para1:PFILE):cint;cdecl;external External_library name 'zonecfg_lock_scratch';
procedure zonecfg_close_scratch(_para1:PFILE);cdecl;external External_library name 'zonecfg_close_scratch';
function zonecfg_get_scratch(_para1:PFILE; _para2:pchar; _para3:size_t; _para4:pchar; _para5:size_t;
           _para6:pchar; _para7:size_t):cint;cdecl;external External_library name 'zonecfg_get_scratch';


function zonecfg_find_scratch(_para1:PFILE; _para2:pchar; _para3:pchar; _para4:pchar; _para5:size_t):cint;cdecl;external External_library name 'zonecfg_find_scratch';

function zonecfg_reverse_scratch(_para1:PFILE; _para2:pchar; _para3:pchar; _para4:size_t; _para5:pchar;
           _para6:size_t):cint;cdecl;external External_library name 'zonecfg_reverse_scratch';



function zonecfg_add_scratch(_para1:PFILE; _para2:pchar; _para3:pchar; _para4:pchar):cint;cdecl;external External_library name 'zonecfg_add_scratch';

function zonecfg_delete_scratch(_para1:PFILE; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_delete_scratch';

function zonecfg_is_scratch(_para1:pchar):boolean_t;cdecl;external External_library name 'zonecfg_is_scratch';
{
 * zoneadmd support functions.  Shared between zoneadm and brand hook code.
  }

procedure zonecfg_init_lock_file(_para1:pchar; _para2:Ppchar);cdecl;external External_library name 'zonecfg_init_lock_file';

procedure zonecfg_release_lock_file(_para1:pchar; _para2:cint);cdecl;external External_library name 'zonecfg_release_lock_file';

function zonecfg_grab_lock_file(_para1:pchar; _para2:pcint):cint;cdecl;external External_library name 'zonecfg_grab_lock_file';
function zonecfg_lock_file_held(_para1:pcint):boolean_t;cdecl;external External_library name 'zonecfg_lock_file_held';

function zonecfg_ping_zoneadmd(_para1:pchar):cint;cdecl;external External_library name 'zonecfg_ping_zoneadmd';

function zonecfg_call_zoneadmd(_para1:pchar; _para2:Pzone_cmd_arg_t; _para3:pchar; _para4:boolean_t):cint;cdecl;external External_library name 'zonecfg_call_zoneadmd';
function zonecfg_insert_userauths(_para1:zone_dochandle_t; _para2:pchar; _para3:pchar):cint;cdecl;external External_library name 'zonecfg_insert_userauths';
function zonecfg_remove_userauths(_para1:zone_dochandle_t; _para2:pchar; _para3:pchar; _para4:boolean_t):cint;cdecl;external External_library name 'zonecfg_remove_userauths';
function zonecfg_add_admin(_para1:zone_dochandle_t; _para2:Pzone_admintab; _para3:pchar):cint;cdecl;external External_library name 'zonecfg_add_admin';
function zonecfg_delete_admin(_para1:zone_dochandle_t; _para2:Pzone_admintab; _para3:pchar):cint;cdecl;external External_library name 'zonecfg_delete_admin';
function zonecfg_modify_admin(_para1:zone_dochandle_t; _para2:Pzone_admintab; _para3:Pzone_admintab; _para4:pchar):cint;cdecl;external External_library name 'zonecfg_modify_admin';
function zonecfg_delete_admins(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_delete_admins';
function zonecfg_lookup_admin(_para1:zone_dochandle_t; _para2:Pzone_admintab):cint;cdecl;external External_library name 'zonecfg_lookup_admin';
function zonecfg_authorize_users(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_authorize_users';
function zonecfg_update_userauths(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_update_userauths';
function zonecfg_deauthorize_user(_para1:zone_dochandle_t; _para2:pchar; _para3:pchar):cint;cdecl;external External_library name 'zonecfg_deauthorize_user';
function zonecfg_deauthorize_users(_para1:zone_dochandle_t; _para2:pchar):cint;cdecl;external External_library name 'zonecfg_deauthorize_users';
function zonecfg_valid_auths(_para1:pchar; _para2:pchar):boolean_t;cdecl;external External_library name 'zonecfg_valid_auths';


//---- Internal defines from zone.h (common)

const
  MIN_USERZONEID = 1;       //* lowest user-creatable zone ID */
  MIN_ZONEID     = 0;       //* minimum zone ID on system */
  GLOBAL_ZONEID  = 0;
  ALL_ZONES      = -1;

  ZONE_ATTR_ROOT        =  1;
  ZONE_ATTR_NAME        =  2;
  ZONE_ATTR_STATUS      =  3;
  ZONE_ATTR_PRIVSET     =  4;
  ZONE_ATTR_UNIQID      =  5;
  ZONE_ATTR_POOLID      =  6;
  ZONE_ATTR_INITPID     =  7;
  ZONE_ATTR_SLBL        =  8;
  ZONE_ATTR_INITNAME    =  9;
  ZONE_ATTR_BOOTARGS    =  10;
  ZONE_ATTR_BRAND       =  11;
  ZONE_ATTR_PHYS_MCAP   =  12;
  ZONE_ATTR_SCHED_CLASS =  13;
  ZONE_ATTR_FLAGS       =  14;
  ZONE_ATTR_HOSTID      =  15;
  ZONE_ATTR_FS_ALLOWED  =  16;
  ZONE_ATTR_NETWORK     =  17;
  ZONE_ATTR_BRAND_ATTRS =  32768;

  ZCF_NET_EXCL		=  1;
  ZF_REFCOUNTS_LOGGED   =  1;     //* a thread logged the zone's refs */
  ZF_HASHED_LABEL       =  2;     //* zone has a unique label */
  ZF_IS_SCRATCH         =  4;     //* scratch zone */
  ZF_NET_EXCL           =  8;     //* Zone has an exclusive IP stack */

implementation


end.
