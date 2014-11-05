unit fosillu_libzfs;

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
 * Copyright (c) 2005, 2010, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2012 by Delphix. All rights reserved.
 * Copyright (c) 2012, Joyent, Inc. All rights reserved.
 * Copyright (c) 2013 Steven Hartland. All rights reserved.
 * Copyright 2013 Nexenta Systems, Inc.  All rights reserved.
 * Copyright (c) 2014 FirmOS Business Solutions GmbH
}

interface

uses
  unix,baseunix,fos_illumos_defs,fosillu_zfs,fosillu_nvpair;

const
  external_zfs_library='zfs';

  ZFS_MAXNAMELEN   = 256;
  ZPOOL_MAXNAMELEN = 256;
  ZFS_MAXPROPLEN   = 1024;
  ZPOOL_MAXPROPLEN = 1024;

  ZFS_GET_NCOLS    = 5;

Type
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  Pavl_tree_t         = ^avl_tree_t;
  Pddt_histogram_t    = ^ddt_histogram_t;
  Pddt_stat_t         = ^ddt_stat_t;
  Pget_all_cb_t       = ^get_all_cb_t;
  Pimportargs_t       = ^importargs_t;
  Plibzfs_handle_t    = ^libzfs_handle_t;
  //Pmnttab             = ^mnttab;
  Ppool_state_t       = ^pool_state_t;
  Precvflags_t        = ^recvflags_t;
  Psendflags_t        = ^sendflags_t;
  Pvdev_state_t       = ^vdev_state_t;
  Pzfs_cmd            = ^zfs_cmd;
  Pzfs_handle_t       = ^zfs_handle_t;
  Pzpool_handle_t     = ^zpool_handle_t;
  Pzprop_get_cbdata_t = ^zprop_get_cbdata_t;
  Pzprop_list_t       = ^zprop_list_t;
  PPzprop_list_t      = ^Pzprop_list_t;
  Pzprop_source_t     = ^zprop_source_t;
 {
 * Miscellaneous ZFS constants
 }
{
 * libzfs errors
  }
  { no error -- success  }
  { out of memory  }
  { invalid property value  }
  { cannot set readonly property  }
  { property does not apply to dataset type  }
  { property is not inheritable  }
  { bad quota or reservation  }
  { dataset is not of appropriate type  }
  { pool or dataset is busy  }
  { pool or dataset already exists  }
  { no such pool or dataset  }
  { bad backup stream  }
  { dataset is readonly  }
  { volume is too large for 32-bit system  }
  { invalid dataset name  }
  { unable to restore to destination  }
  { backup failed  }
  { bad attach/detach/replace target  }
  { no such device in pool  }
  { invalid device to add  }
  { no valid replicas  }
  { currently resilvering  }
  { unsupported version  }
  { pool is currently unavailable  }
  { too many devices in one vdev  }
  { must be an absolute path  }
  { rename or clone across pool or dataset  }
  { used improperly in local zone  }
  { failed to mount dataset  }
  { failed to unmount dataset  }
  { unshare(1M) failed  }
  { share(1M) failed  }
  { permission denied  }
  { out of space  }
  { bad address  }
  { I/O error  }
  { signal received  }
  { device is a hot spare  }
  { invalid vdev configuration  }
  { recursive dependency  }
  { no history object  }
  { couldn't retrieve pool props  }
  { ops not supported for this type of pool  }
  { invalid argument for this pool operation  }
  { dataset name is too long  }
  { open of device failed  }
  { couldn't get capacity  }
  { write of label failed  }
  { invalid permission who  }
  { invalid permission  }
  { invalid permission set name  }
  { delegated administration is disabled  }
  { failed to unshare over smb  }
  { failed to share over smb  }
  { bad cache file  }
  { device is for the level 2 ARC  }
  { unsupported vdev type  }
  { ops not supported on this dataset  }
  { pool has active shared spare devices  }
  { log device has unplayed logs  }
  { snapshot release: tag not found  }
  { snapshot hold: tag already exists  }
  { snapshot hold/rele: tag too long  }
  { pipe create failed  }
  { thread create failed  }
  { onlining a disk after splitting it  }
  { currently scrubbing  }
  { no active scrub  }
  { general failure of zfs diff  }
  { bad zfs diff data  }
  { pool is in read-only mode  }
  zfs_error = (EZFS_SUCCESS := 0,EZFS_NOMEM := 2000,EZFS_BADPROP,
    EZFS_PROPREADONLY,EZFS_PROPTYPE,EZFS_PROPNONINHERIT,
    EZFS_PROPSPACE,EZFS_BADTYPE,EZFS_BUSY,EZFS_EXISTS,
    EZFS_NOENT,EZFS_BADSTREAM,EZFS_DSREADONLY,
    EZFS_VOLTOOBIG,EZFS_INVALIDNAME,EZFS_BADRESTORE,
    EZFS_BADBACKUP,EZFS_BADTARGET,EZFS_NODEVICE,
    EZFS_BADDEV,EZFS_NOREPLICAS,EZFS_RESILVERING,
    EZFS_BADVERSION,EZFS_POOLUNAVAIL,EZFS_DEVOVERFLOW,
    EZFS_BADPATH,EZFS_CROSSTARGET,EZFS_ZONED,
    EZFS_MOUNTFAILED,EZFS_UMOUNTFAILED,EZFS_UNSHARENFSFAILED,
    EZFS_SHARENFSFAILED,EZFS_PERM,EZFS_NOSPC,
    EZFS_FAULT,EZFS_IO,EZFS_INTR,EZFS_ISSPARE,
    EZFS_INVALCONFIG,EZFS_RECURSIVE,EZFS_NOHISTORY,
    EZFS_POOLPROPS,EZFS_POOL_NOTSUP,EZFS_POOL_INVALARG,
    EZFS_NAMETOOLONG,EZFS_OPENFAILED,EZFS_NOCAP,
    EZFS_LABELFAILED,EZFS_BADWHO,EZFS_BADPERM,
    EZFS_BADPERMSET,EZFS_NODELEGATION,EZFS_UNSHARESMBFAILED,
    EZFS_SHARESMBFAILED,EZFS_BADCACHE,EZFS_ISL2CACHE,
    EZFS_VDEVNOTSUP,EZFS_NOTSUP,EZFS_ACTIVE_SPARE,
    EZFS_UNPLAYED_LOGS,EZFS_REFTAG_RELE,EZFS_REFTAG_HOLD,
    EZFS_TAGTOOLONG,EZFS_PIPEFAILED,EZFS_THREADCREATEFAILED,
    EZFS_POSTSPLIT_ONLINE,EZFS_SCRUBBING,EZFS_NO_SCRUB,
    EZFS_DIFF,EZFS_DIFFDATA,EZFS_POOLREADONLY,
    EZFS_UNKNOWN);
  zfs_error_t = zfs_error;
{
 * The following data structures are all part
 * of the zfs_allow_t data structure which is
 * used for printing 'allow' permissions.
 * It is a linked list of zfs_allow_t's which
 * then contain avl tree's for user/group/sets/...
 * and each one of the entries in those trees have
 * avl tree's for the permissions they belong to and
 * whether they are local,descendent or local+descendent
 * permissions.  The AVL trees are used primarily for
 * sorting purposes, but also so that we can quickly find
 * a given user and or permission.
  }

  zfs_perm_node = record
      z_node : avl_node_t;
      z_pname : array[0..(MAXPATHLEN)-1] of cchar;
    end;
  zfs_perm_node_t = zfs_perm_node;
{ name, such as joe  }
{ local+descendent perms  }
{ local permissions  }
{ descendent permissions  }

  zfs_allow_node = record
      z_node : avl_node_t;
      z_key : array[0..(MAXPATHLEN)-1] of cchar;
      z_localdescend : avl_tree_t;
      z_local : avl_tree_t;
      z_descend : avl_tree_t;
    end;
  zfs_allow_node_t = zfs_allow_node;

  zfs_allow = record
      z_next : ^zfs_allow;
      z_setpoint : array[0..(MAXPATHLEN)-1] of cchar;
      z_sets : avl_tree_t;
      z_crperms : avl_tree_t;
      z_user : avl_tree_t;
      z_group : avl_tree_t;
      z_everyone : avl_tree_t;
    end;
  zfs_allow_t = zfs_allow;
{
 * Basic handle types
  }

  zfs_handle_t    = record end;
  zpool_handle_t  = record end;
  libzfs_handle_t = record end;
  PPzfs_handle_t  = ^Pzfs_handle_t;


  get_all_cb = record
      cb_handles : PPzfs_handle_t;
      cb_alloc   : size_t;
      cb_used    : size_t;
      cb_verbose : boolean_t;
      cb_getone  : function (_para1:Pzfs_handle_t; _para2:pointer):cint;cdecl;
    end;
  get_all_cb_t = get_all_cb;

  {
   * Search for pools to import
    }
  { a list of paths to search		 }
  { number of paths to search		 }
  { name of a pool to find		 }
  { guid of a pool to find		 }
  { cachefile to use for import		 }
  { can the pool be active?		 }
  { does 'poolname' already exist?	 }
  { set on return if pool already exists	 }
  importargs = record
      path : Ppchar;
      paths : cint;
      poolname : pchar;
      guid : uint64_t;
      cachefile : pchar;
      flag0 : word;
    end;
  importargs_t = importargs;

  { print informational messages (ie, -v was specified)  }
  { the destination is a prefix, not the exact fs (ie, -d)  }
  {
  	 * Only the tail of the sent snapshot path is appended to the
  	 * destination to determine the received snapshot name (ie, -e).
  	  }
  { do not actually do the recv, just check if it would work (ie, -n)  }
  { rollback/destroy filesystems as necessary (eg, -F)  }
  { set "canmount=off" on all modified filesystems  }
  { byteswap flag is used internally; callers need not specify  }
  { do not mount file systems as they are extracted (private)  }

  recvflags = record
      verbose : boolean_t;
      isprefix : boolean_t;
      istail : boolean_t;
      dryrun : boolean_t;
      force : boolean_t;
      canmountoff : boolean_t;
      byteswap : boolean_t;
      nomount : boolean_t;
    end;
  recvflags_t = recvflags;

  { print informational messages (ie, -v was specified)  }
  { recursive send  (ie, -R)  }
  { for incrementals, do all intermediate snapshots  }
  { if dataset is a clone, do incremental from its origin  }
  { do deduplication  }
  { send properties (ie, -p)  }
  { do not send (no-op, ie. -n)  }
  { parsable verbose output (ie. -P)  }
  { show progress (ie. -v)  }

  sendflags = record
      verbose : boolean_t;
      replicate : boolean_t;
      doall : boolean_t;
      fromorigin : boolean_t;
      dedup : boolean_t;
      props : boolean_t;
      dryrun : boolean_t;
      parsable : boolean_t;
      progress : boolean_t;
    end;
  sendflags_t = sendflags;

  zfs_cmd = record
      {undefined structure}
    end;

  zfs_get_column_t = (GET_COL_NONE,GET_COL_NAME,GET_COL_PROPERTY,
    GET_COL_VALUE,GET_COL_RECVD,GET_COL_SOURCE
    );

  zprop_get_cbdata = record
      cb_sources : cint;
      cb_columns : array[0..(ZFS_GET_NCOLS)-1] of zfs_get_column_t;
      cb_colwidths : array[0..(ZFS_GET_NCOLS+1)-1] of cint;
      cb_scripted : boolean_t;
      cb_literal : boolean_t;
      cb_first : boolean_t;
      cb_proplist : ^zprop_list_t;
      cb_type : zfs_type_t;
    end;
  zprop_get_cbdata_t = zprop_get_cbdata;

  zprop_list = record
      pl_prop : cint;
      pl_user_prop : ^cchar;
      pl_next : ^zprop_list;
      pl_all : boolean_t;
      pl_width : size_t;
      pl_recvd_width : size_t;
      pl_fixed : boolean_t;
    end;
  zprop_list_t = zprop_list;


 {
 * Library initialization
  }

function libzfs_init:Plibzfs_handle_t;cdecl;external external_zfs_library name 'libzfs_init';
procedure libzfs_fini(_para1:Plibzfs_handle_t);cdecl;external external_zfs_library name 'libzfs_fini';
function zpool_get_handle(_para1:Pzpool_handle_t):Plibzfs_handle_t;cdecl;external external_zfs_library name 'zpool_get_handle';
function zfs_get_handle(_para1:Pzfs_handle_t):Plibzfs_handle_t;cdecl;external external_zfs_library name 'zfs_get_handle';
procedure libzfs_print_on_error(_para1:Plibzfs_handle_t; _para2:boolean_t);cdecl;external external_zfs_library name 'libzfs_print_on_error';

procedure libzfs_set_cachedprops(_para1:Plibzfs_handle_t; _para2:boolean_t);cdecl;external external_zfs_library name 'libzfs_set_cachedprops';

procedure zfs_save_arguments(argc:cint; _para2:Ppchar; _para3:pchar; _para4:cint);cdecl;external external_zfs_library name 'zfs_save_arguments';

function zpool_log_history(_para1:Plibzfs_handle_t; _para2:pchar):cint;cdecl;external external_zfs_library name 'zpool_log_history';
function libzfs_errno(_para1:Plibzfs_handle_t):cint;cdecl;external external_zfs_library name 'libzfs_errno';

function libzfs_error_action(_para1:Plibzfs_handle_t):pchar;cdecl;external external_zfs_library name 'libzfs_error_action';

function libzfs_error_description(_para1:Plibzfs_handle_t):pchar;cdecl;external external_zfs_library name 'libzfs_error_description';
procedure libzfs_mnttab_init(_para1:Plibzfs_handle_t);cdecl;external external_zfs_library name 'libzfs_mnttab_init';
procedure libzfs_mnttab_fini(_para1:Plibzfs_handle_t);cdecl;external external_zfs_library name 'libzfs_mnttab_fini';
procedure libzfs_mnttab_cache(_para1:Plibzfs_handle_t; _para2:boolean_t);cdecl;external external_zfs_library name 'libzfs_mnttab_cache';

//FOSTODO function libzfs_mnttab_find(_para1:Plibzfs_handle_t; _para2:pchar; _para3:Pmnttab):cint;cdecl;external external_zfs_library name 'libzfs_mnttab_find';



procedure libzfs_mnttab_add(_para1:Plibzfs_handle_t; _para2:pchar; _para3:pchar; _para4:pchar);cdecl;external external_zfs_library name 'libzfs_mnttab_add';

procedure libzfs_mnttab_remove(_para1:Plibzfs_handle_t; _para2:pchar);cdecl;external external_zfs_library name 'libzfs_mnttab_remove';
{
 * Basic handle functions
  }

function zpool_open(_para1:Plibzfs_handle_t; _para2:pchar):Pzpool_handle_t;cdecl;external external_zfs_library name 'zpool_open';

function zpool_open_canfail(_para1:Plibzfs_handle_t; _para2:pchar):Pzpool_handle_t;cdecl;external external_zfs_library name 'zpool_open_canfail';
procedure zpool_close(_para1:Pzpool_handle_t);cdecl;external external_zfs_library name 'zpool_close';

function zpool_get_name(_para1:Pzpool_handle_t):pchar;cdecl;external external_zfs_library name 'zpool_get_name';
function zpool_get_state(_para1:Pzpool_handle_t):cint;cdecl;external external_zfs_library name 'zpool_get_state';
function zpool_state_to_name(_para1:vdev_state_t; _para2:vdev_aux_t):pchar;cdecl;external external_zfs_library name 'zpool_state_to_name';
procedure zpool_free_handles(_para1:Plibzfs_handle_t);cdecl;external external_zfs_library name 'zpool_free_handles';
{
 * Iterate over all active pools in the system.
  }
type

  zpool_iter_f = function (_para1:Pzpool_handle_t; _para2:pointer):cint;cdecl;

function zpool_iter(_para1:Plibzfs_handle_t; _para2:zpool_iter_f; _para3:pointer):cint;cdecl;external external_zfs_library name 'zpool_iter';
{
 * Functions to create and destroy pools
  }

function zpool_create(_para1:Plibzfs_handle_t; _para2:pchar; _para3:Pnvlist_t; _para4:Pnvlist_t; _para5:Pnvlist_t):cint;cdecl;external external_zfs_library name 'zpool_create';

function zpool_destroy(_para1:Pzpool_handle_t; _para2:pchar):cint;cdecl;external external_zfs_library name 'zpool_destroy';
function zpool_add(_para1:Pzpool_handle_t; _para2:Pnvlist_t):cint;cdecl;external external_zfs_library name 'zpool_add';
{ do not split, but return the config that would be split off  }
{ after splitting, import the pool  }
type
  splitflags = record
      flag0 : word;
    end;
  splitflags_t = splitflags;

const
  bm_splitflags_dryrun = $1;
  bp_splitflags_dryrun = 0;
  bm_splitflags_import = $2;
  bp_splitflags_import = 1;

function dryrun(var a : splitflags) : cint;
procedure set_dryrun(var a : splitflags; __dryrun : cint);
function import(var a : splitflags) : cint;
procedure set_import(var a : splitflags; __import : cint);
{
 * Functions to manipulate pool and vdev state
  }

function zpool_scan(_para1:Pzpool_handle_t; _para2:pool_scan_func_t):cint;cdecl;external external_zfs_library name 'zpool_scan';

function zpool_clear(_para1:Pzpool_handle_t; _para2:pchar; _para3:Pnvlist_t):cint;cdecl;external external_zfs_library name 'zpool_clear';
function zpool_reguid(_para1:Pzpool_handle_t):cint;cdecl;external external_zfs_library name 'zpool_reguid';
function zpool_reopen(_para1:Pzpool_handle_t):cint;cdecl;external external_zfs_library name 'zpool_reopen';

function zpool_vdev_online(_para1:Pzpool_handle_t; _para2:pchar; _para3:cint; _para4:Pvdev_state_t):cint;cdecl;external external_zfs_library name 'zpool_vdev_online';

function zpool_vdev_offline(_para1:Pzpool_handle_t; _para2:pchar; _para3:boolean_t):cint;cdecl;external external_zfs_library name 'zpool_vdev_offline';


function zpool_vdev_attach(_para1:Pzpool_handle_t; _para2:pchar; _para3:pchar; _para4:Pnvlist_t; _para5:cint):cint;cdecl;external external_zfs_library name 'zpool_vdev_attach';

function zpool_vdev_detach(_para1:Pzpool_handle_t; _para2:pchar):cint;cdecl;external external_zfs_library name 'zpool_vdev_detach';

function zpool_vdev_remove(_para1:Pzpool_handle_t; _para2:pchar):cint;cdecl;external external_zfs_library name 'zpool_vdev_remove';
function zpool_vdev_split(_para1:Pzpool_handle_t; _para2:pchar; _para3:PPnvlist_t; _para4:Pnvlist_t; _para5:splitflags_t):cint;cdecl;external external_zfs_library name 'zpool_vdev_split';
function zpool_vdev_fault(_para1:Pzpool_handle_t; _para2:uint64_t; _para3:vdev_aux_t):cint;cdecl;external external_zfs_library name 'zpool_vdev_fault';
function zpool_vdev_degrade(_para1:Pzpool_handle_t; _para2:uint64_t; _para3:vdev_aux_t):cint;cdecl;external external_zfs_library name 'zpool_vdev_degrade';
function zpool_vdev_clear(_para1:Pzpool_handle_t; _para2:uint64_t):cint;cdecl;external external_zfs_library name 'zpool_vdev_clear';

function zpool_find_vdev(_para1:Pzpool_handle_t; _para2:pchar; _para3:Pboolean_t; _para4:Pboolean_t; _para5:Pboolean_t):Pnvlist_t;cdecl;external external_zfs_library name 'zpool_find_vdev';

function zpool_find_vdev_by_physpath(_para1:Pzpool_handle_t; _para2:pchar; _para3:Pboolean_t; _para4:Pboolean_t; _para5:Pboolean_t):Pnvlist_t;cdecl;external external_zfs_library name 'zpool_find_vdev_by_physpath';
function zpool_label_disk(_para1:Plibzfs_handle_t; _para2:Pzpool_handle_t; _para3:pchar):cint;cdecl;external external_zfs_library name 'zpool_label_disk';
{
 * Functions to manage pool properties
  }


function zpool_set_prop(_para1:Pzpool_handle_t; _para2:pchar; _para3:pchar):cint;cdecl;external external_zfs_library name 'zpool_set_prop';
function zpool_get_prop(_para1:Pzpool_handle_t; _para2:zpool_prop_t; _para3:pchar; proplen:size_t; _para5:Pzprop_source_t):cint;cdecl;external external_zfs_library name 'zpool_get_prop';
function zpool_get_prop_int(_para1:Pzpool_handle_t; _para2:zpool_prop_t; _para3:Pzprop_source_t):uint64_t;cdecl;external external_zfs_library name 'zpool_get_prop_int';

function zpool_prop_to_name(_para1:zpool_prop_t):pchar;cdecl;external external_zfs_library name 'zpool_prop_to_name';

function zpool_prop_values(_para1:zpool_prop_t):pchar;cdecl;external external_zfs_library name 'zpool_prop_values';
{
 * Pool health statistics.
  }
{
	 * The following correspond to faults as defined in the (fault.fs.zfs.*)
	 * event namespace.  Each is associated with a corresponding message ID.
	  }
{ corrupt /kernel/drv/zpool.cache  }
{ missing device with replicas  }
{ missing device with no replicas  }
{ bad device label with replicas  }
{ bad device label with no replicas  }
{ sum of device guids didn't match  }
{ pool metadata is corrupted  }
{ data errors in user (meta)data  }
{ device experiencing errors  }
{ newer on-disk version  }
{ last accessed by another system  }
{ failed I/O, failmode 'wait'  }
{ failed I/O, failmode 'continue'  }
{ cannot read log chain(s)  }
{
	 * If the pool has unsupported features but can still be opened in
	 * read-only mode, its status is ZPOOL_STATUS_UNSUP_FEAT_WRITE. If the
	 * pool has unsupported features but cannot be opened at all, its
	 * status is ZPOOL_STATUS_UNSUP_FEAT_READ.
	  }
{ unsupported features for read  }
{ unsupported features for write  }
{
	 * These faults have no corresponding message ID.  At the time we are
	 * checking the status, the original reason for the FMA fault (I/O or
	 * checksum errors) has been lost.
	  }
{ faulted device with replicas  }
{ faulted device with no replicas  }
{
	 * The following are not faults per se, but still an error possibly
	 * requiring administrative attention.  There is no corresponding
	 * message ID.
	  }
{ older legacy on-disk version  }
{ supported features are disabled  }
{ device being resilvered  }
{ device online  }
{ removed device  }
{
	 * Finally, the following indicates a healthy pool.
	  }
type
  zpool_status_t = (ZPOOL_STATUS_CORRUPT_CACHE,ZPOOL_STATUS_MISSING_DEV_R,
    ZPOOL_STATUS_MISSING_DEV_NR,ZPOOL_STATUS_CORRUPT_LABEL_R,
    ZPOOL_STATUS_CORRUPT_LABEL_NR,ZPOOL_STATUS_BAD_GUID_SUM,
    ZPOOL_STATUS_CORRUPT_POOL,ZPOOL_STATUS_CORRUPT_DATA,
    ZPOOL_STATUS_FAILING_DEV,ZPOOL_STATUS_VERSION_NEWER,
    ZPOOL_STATUS_HOSTID_MISMATCH,ZPOOL_STATUS_IO_FAILURE_WAIT,
    ZPOOL_STATUS_IO_FAILURE_CONTINUE,ZPOOL_STATUS_BAD_LOG,
    ZPOOL_STATUS_UNSUP_FEAT_READ,ZPOOL_STATUS_UNSUP_FEAT_WRITE,
    ZPOOL_STATUS_FAULTED_DEV_R,ZPOOL_STATUS_FAULTED_DEV_NR,
    ZPOOL_STATUS_VERSION_OLDER,ZPOOL_STATUS_FEAT_DISABLED,
    ZPOOL_STATUS_RESILVERING,ZPOOL_STATUS_OFFLINE_DEV,
    ZPOOL_STATUS_REMOVED_DEV,ZPOOL_STATUS_OK
    );

function zpool_get_status(_para1:Pzpool_handle_t; _para2:Ppchar):zpool_status_t;cdecl;external external_zfs_library name 'zpool_get_status';
function zpool_import_status(_para1:Pnvlist_t; _para2:Ppchar):zpool_status_t;cdecl;external external_zfs_library name 'zpool_import_status';


procedure zpool_dump_ddt(dds:Pddt_stat_t; ddh:Pddt_histogram_t);cdecl;external external_zfs_library name 'zpool_dump_ddt';
{
 * Statistics and configuration functions.
  }
function zpool_get_config(_para1:Pzpool_handle_t; _para2:PPnvlist_t):Pnvlist_t;cdecl;external external_zfs_library name 'zpool_get_config';
function zpool_get_features(_para1:Pzpool_handle_t):Pnvlist_t;cdecl;external external_zfs_library name 'zpool_get_features';
function zpool_refresh_stats(_para1:Pzpool_handle_t; _para2:Pboolean_t):cint;cdecl;external external_zfs_library name 'zpool_refresh_stats';
function zpool_get_errlog(_para1:Pzpool_handle_t; _para2:PPnvlist_t):cint;cdecl;external external_zfs_library name 'zpool_get_errlog';
{
 * Import and export functions
  }

function zpool_export(_para1:Pzpool_handle_t; _para2:boolean_t; _para3:pchar):cint;cdecl;external external_zfs_library name 'zpool_export';

function zpool_export_force(_para1:Pzpool_handle_t; _para2:pchar):cint;cdecl;external external_zfs_library name 'zpool_export_force';

function zpool_import(_para1:Plibzfs_handle_t; _para2:Pnvlist_t; _para3:pchar; altroot:pchar):cint;cdecl;external external_zfs_library name 'zpool_import';

function zpool_import_props(_para1:Plibzfs_handle_t; _para2:Pnvlist_t; _para3:pchar; _para4:Pnvlist_t; _para5:cint):cint;cdecl;external external_zfs_library name 'zpool_import_props';
procedure zpool_print_unsup_feat(config:Pnvlist_t);cdecl;external external_zfs_library name 'zpool_print_unsup_feat';

const
  bm_importargs_can_be_active = $1;
  bp_importargs_can_be_active = 0;
  bm_importargs_unique = $2;
  bp_importargs_unique = 1;
  bm_importargs_exists = $4;
  bp_importargs_exists = 2;

function can_be_active(var a : importargs) : cint;
procedure set_can_be_active(var a : importargs; __can_be_active : cint);
function unique(var a : importargs) : cint;
procedure set_unique(var a : importargs; __unique : cint);
function exists(var a : importargs) : cint;
procedure set_exists(var a : importargs; __exists : cint);

function zpool_search_import(_para1:Plibzfs_handle_t; _para2:Pimportargs_t):Pnvlist_t;cdecl;external external_zfs_library name 'zpool_search_import';
{ legacy pool search routines  }
function zpool_find_import(_para1:Plibzfs_handle_t; _para2:cint; _para3:Ppchar):Pnvlist_t;cdecl;external external_zfs_library name 'zpool_find_import';

function zpool_find_import_cached(_para1:Plibzfs_handle_t; _para2:pchar; _para3:pchar; _para4:uint64_t):Pnvlist_t;cdecl;external external_zfs_library name 'zpool_find_import_cached';
{
 * Miscellaneous pool functions
  }


  var
    zfs_history_event_names : Ppchar;cvar;external;

function zpool_vdev_name(_para1:Plibzfs_handle_t; _para2:Pzpool_handle_t; _para3:Pnvlist_t; verbose:boolean_t):pchar;cdecl;external external_zfs_library name 'zpool_vdev_name';
function zpool_upgrade(_para1:Pzpool_handle_t; _para2:uint64_t):cint;cdecl;external external_zfs_library name 'zpool_upgrade';
function zpool_get_history(_para1:Pzpool_handle_t; _para2:PPnvlist_t):cint;cdecl;external external_zfs_library name 'zpool_get_history';
function zpool_history_unpack(_para1:pchar; _para2:uint64_t; _para3:Puint64_t; _para4:PPPnvlist_t; _para5:Puint_t):cint;cdecl;external external_zfs_library name 'zpool_history_unpack';
procedure zpool_obj_to_path(_para1:Pzpool_handle_t; _para2:uint64_t; _para3:uint64_t; _para4:pchar; len:size_t);cdecl;external external_zfs_library name 'zpool_obj_to_path';
function zfs_ioctl(_para1:Plibzfs_handle_t; _para2:cint; _para3:Pzfs_cmd):cint;cdecl;external external_zfs_library name 'zfs_ioctl';
function zpool_get_physpath(_para1:Pzpool_handle_t; _para2:pchar; _para3:size_t):cint;cdecl;external external_zfs_library name 'zpool_get_physpath';

procedure zpool_explain_recover(_para1:Plibzfs_handle_t; _para2:pchar; _para3:cint; _para4:Pnvlist_t);cdecl;external external_zfs_library name 'zpool_explain_recover';
{
 * Basic handle manipulations.  These functions do not create or destroy the
 * underlying datasets, only the references to them.
  }

function zfs_open(_para1:Plibzfs_handle_t; _para2:pchar; _para3:cint):Pzfs_handle_t;cdecl;external external_zfs_library name 'zfs_open';
function zfs_handle_dup(_para1:Pzfs_handle_t):Pzfs_handle_t;cdecl;external external_zfs_library name 'zfs_handle_dup';
procedure zfs_close(_para1:Pzfs_handle_t);cdecl;external external_zfs_library name 'zfs_close';

function zfs_get_type(_para1:Pzfs_handle_t):zfs_type_t;cdecl;external external_zfs_library name 'zfs_get_type';


function zfs_get_name(_para1:Pzfs_handle_t):pchar;cdecl;external external_zfs_library name 'zfs_get_name';

function zfs_get_pool_handle(_para1:Pzfs_handle_t):Pzpool_handle_t;cdecl;external external_zfs_library name 'zfs_get_pool_handle';
{
 * Property management functions.  Some functions are shared with the kernel,
 * and are found in sys/fs/zfs.h.
  }
{
 * zfs dataset property management
  }

function zfs_prop_default_string(_para1:zfs_prop_t):pchar;cdecl;external external_zfs_library name 'zfs_prop_default_string';
function zfs_prop_default_numeric(_para1:zfs_prop_t):uint64_t;cdecl;external external_zfs_library name 'zfs_prop_default_numeric';

function zfs_prop_column_name(_para1:zfs_prop_t):pchar;cdecl;external external_zfs_library name 'zfs_prop_column_name';
function zfs_prop_align_right(_para1:zfs_prop_t):boolean_t;cdecl;external external_zfs_library name 'zfs_prop_align_right';

function zfs_valid_proplist(_para1:Plibzfs_handle_t; _para2:zfs_type_t; _para3:Pnvlist_t; _para4:uint64_t; _para5:Pzfs_handle_t; 
           _para6:pchar):Pnvlist_t;cdecl;external external_zfs_library name 'zfs_valid_proplist';

function zfs_prop_to_name(_para1:zfs_prop_t):pchar;cdecl;external external_zfs_library name 'zfs_prop_to_name';

function zfs_prop_set(_para1:Pzfs_handle_t; _para2:pchar; _para3:pchar):cint;cdecl;external external_zfs_library name 'zfs_prop_set';
function zfs_prop_get(hp:Pzfs_handle_t; prop :zfs_prop_t; propbuf:pchar; proplen:size_t; src:Pzprop_source_t;
           statbuf:pchar; statlen:size_t; literal:boolean_t):cint;cdecl;external external_zfs_library name 'zfs_prop_get';

function zfs_prop_get_recvd(_para1:Pzfs_handle_t; _para2:pchar; _para3:pchar; _para4:size_t; _para5:boolean_t):cint;cdecl;external external_zfs_library name 'zfs_prop_get_recvd';
function zfs_prop_get_numeric(_para1:Pzfs_handle_t; _para2:zfs_prop_t; _para3:Puint64_t; _para4:Pzprop_source_t; _para5:pchar;
           _para6:size_t):cint;cdecl;external external_zfs_library name 'zfs_prop_get_numeric';

function zfs_prop_get_userquota_int(zhp:Pzfs_handle_t; propname:pchar; propvalue:Puint64_t):cint;cdecl;external external_zfs_library name 'zfs_prop_get_userquota_int';

function zfs_prop_get_userquota(zhp:Pzfs_handle_t; propname:pchar; propbuf:pchar; proplen:cint; literal:boolean_t):cint;cdecl;external external_zfs_library name 'zfs_prop_get_userquota';

function zfs_prop_get_written_int(zhp:Pzfs_handle_t; propname:pchar; propvalue:Puint64_t):cint;cdecl;external external_zfs_library name 'zfs_prop_get_written_int';

function zfs_prop_get_written(zhp:Pzfs_handle_t; propname:pchar; propbuf:pchar; proplen:cint; literal:boolean_t):cint;cdecl;external external_zfs_library name 'zfs_prop_get_written';

function zfs_prop_get_feature(zhp:Pzfs_handle_t; propname:pchar; buf:pchar; len:size_t):cint;cdecl;external external_zfs_library name 'zfs_prop_get_feature';
function zfs_prop_get_int(_para1:Pzfs_handle_t; _para2:zfs_prop_t):uint64_t;cdecl;external external_zfs_library name 'zfs_prop_get_int';

function zfs_prop_inherit(_para1:Pzfs_handle_t; _para2:pchar; _para3:boolean_t):cint;cdecl;external external_zfs_library name 'zfs_prop_inherit';

function zfs_prop_values(_para1:zfs_prop_t):pchar;cdecl;external external_zfs_library name 'zfs_prop_values';
function zfs_prop_is_string(prop:zfs_prop_t):cint;cdecl;external external_zfs_library name 'zfs_prop_is_string';
function zfs_get_user_props(_para1:Pzfs_handle_t):Pnvlist_t;cdecl;external external_zfs_library name 'zfs_get_user_props';
function zfs_get_recvd_props(_para1:Pzfs_handle_t):Pnvlist_t;cdecl;external external_zfs_library name 'zfs_get_recvd_props';
function zfs_get_clones_nvl(_para1:Pzfs_handle_t):Pnvlist_t;cdecl;external external_zfs_library name 'zfs_get_clones_nvl';

function  zfs_expand_proplist(_para1:Pzfs_handle_t; _para2:PPzprop_list_t; received:boolean_t; literal:boolean_t):cint;cdecl;external external_zfs_library name 'zfs_expand_proplist';
procedure zfs_prune_proplist(_para1:Pzfs_handle_t; props:Puint8_t);cdecl;external external_zfs_library name 'zfs_prune_proplist';

const
  ZFS_MOUNTPOINT_NONE = 'none';  
  ZFS_MOUNTPOINT_LEGACY = 'legacy';  
  ZFS_FEATURE_DISABLED = 'disabled';  
  ZFS_FEATURE_ENABLED = 'enabled';  
  ZFS_FEATURE_ACTIVE = 'active';  
  ZFS_UNSUPPORTED_INACTIVE = 'inactive';  
  ZFS_UNSUPPORTED_READONLY = 'readonly';  
{
 * zpool property management
  }

function zpool_expand_proplist(_para1:Pzpool_handle_t; _para2:PPzprop_list_t):cint;cdecl;external external_zfs_library name 'zpool_expand_proplist';

function zpool_prop_get_feature(_para1:Pzpool_handle_t; _para2:pchar; _para3:pchar; _para4:size_t):cint;cdecl;external external_zfs_library name 'zpool_prop_get_feature';

function zpool_prop_default_string(_para1:zpool_prop_t):pchar;cdecl;external external_zfs_library name 'zpool_prop_default_string';
function zpool_prop_default_numeric(_para1:zpool_prop_t):uint64_t;cdecl;external external_zfs_library name 'zpool_prop_default_numeric';

function zpool_prop_column_name(_para1:zpool_prop_t):pchar;cdecl;external external_zfs_library name 'zpool_prop_column_name';
function zpool_prop_align_right(_para1:zpool_prop_t):boolean_t;cdecl;external external_zfs_library name 'zpool_prop_align_right';
{
 * Functions shared by zfs and zpool property management.
  }
function zprop_iter(func:zprop_func; cb:pointer; show_all:boolean_t; ordered:boolean_t; _type:zfs_type_t):cint;cdecl;external external_zfs_library name 'zprop_iter';
function zprop_get_list(_para1:Plibzfs_handle_t; _para2:pchar; _para3:PPzprop_list_t; _para4:zfs_type_t):cint;cdecl;external external_zfs_library name 'zprop_get_list';

procedure zprop_free_list(_para1:Pzprop_list_t);cdecl;external external_zfs_library name 'zprop_free_list';

{
 * Functions for printing zfs or zpool properties
  }

procedure zprop_print_one_property(_para1:pchar; _para2:Pzprop_get_cbdata_t; _para3:pchar; _para4:pchar; _para5:zprop_source_t;
            _para6:pchar; _para7:pchar);cdecl;external external_zfs_library name 'zprop_print_one_property';
{
 * Iterator functions.
  }
type

  zfs_iter_f = function (_para1:Pzfs_handle_t; _para2:pointer):cint;cdecl;

function zfs_iter_root(_para1:Plibzfs_handle_t; _para2:zfs_iter_f; _para3:pointer):cint;cdecl;external external_zfs_library name 'zfs_iter_root';
function zfs_iter_children(_para1:Pzfs_handle_t; _para2:zfs_iter_f; _para3:pointer):cint;cdecl;external external_zfs_library name 'zfs_iter_children';
function zfs_iter_dependents(_para1:Pzfs_handle_t; _para2:boolean_t; _para3:zfs_iter_f; _para4:pointer):cint;cdecl;external external_zfs_library name 'zfs_iter_dependents';
function zfs_iter_filesystems(_para1:Pzfs_handle_t; _para2:zfs_iter_f; _para3:pointer):cint;cdecl;external external_zfs_library name 'zfs_iter_filesystems';
function zfs_iter_snapshots(_para1:Pzfs_handle_t; _para2:zfs_iter_f; _para3:pointer):cint;cdecl;external external_zfs_library name 'zfs_iter_snapshots';
function zfs_iter_bookmarks(_para1:Pzfs_handle_t; _para2:zfs_iter_f; _para3:pointer):cint;cdecl;external external_zfs_library name 'zfs_iter_bookmarks';
function zfs_iter_snapshots_sorted(_para1:Pzfs_handle_t; _para2:zfs_iter_f; _para3:pointer):cint;cdecl;external external_zfs_library name 'zfs_iter_snapshots_sorted';

function zfs_iter_snapspec(_para1:Pzfs_handle_t; _para2:pchar; _para3:zfs_iter_f; _para4:pointer):cint;cdecl;external external_zfs_library name 'zfs_iter_snapspec';

procedure libzfs_add_handle(_para1:Pget_all_cb_t; _para2:Pzfs_handle_t);cdecl;external external_zfs_library name 'libzfs_add_handle';


function libzfs_dataset_cmp(_para1:pointer; _para2:pointer):cint;cdecl;external external_zfs_library name 'libzfs_dataset_cmp';
{
 * Functions to create and destroy datasets.
  }

function zfs_create(_para1:Plibzfs_handle_t; _para2:pchar; _para3:zfs_type_t; _para4:Pnvlist_t):cint;cdecl;external external_zfs_library name 'zfs_create';

function zfs_create_ancestors(_para1:Plibzfs_handle_t; _para2:pchar):cint;cdecl;external external_zfs_library name 'zfs_create_ancestors';
function zfs_destroy(_para1:Pzfs_handle_t; _para2:boolean_t):cint;cdecl;external external_zfs_library name 'zfs_destroy';
function zfs_destroy_snaps(_para1:Pzfs_handle_t; _para2:pchar; _para3:boolean_t):cint;cdecl;external external_zfs_library name 'zfs_destroy_snaps';
function zfs_destroy_snaps_nvl(_para1:Plibzfs_handle_t; _para2:Pnvlist_t; _para3:boolean_t):cint;cdecl;external external_zfs_library name 'zfs_destroy_snaps_nvl';

function zfs_clone(_para1:Pzfs_handle_t; _para2:pchar; _para3:Pnvlist_t):cint;cdecl;external external_zfs_library name 'zfs_clone';

function zfs_snapshot(_para1:Plibzfs_handle_t; _para2:pchar; _para3:boolean_t; _para4:Pnvlist_t):cint;cdecl;external external_zfs_library name 'zfs_snapshot';
function zfs_snapshot_nvl(hdl:Plibzfs_handle_t; snaps:Pnvlist_t; props:Pnvlist_t):cint;cdecl;external external_zfs_library name 'zfs_snapshot_nvl';
function zfs_rollback(_para1:Pzfs_handle_t; _para2:Pzfs_handle_t; _para3:boolean_t):cint;cdecl;external external_zfs_library name 'zfs_rollback';

function zfs_rename(_para1:Pzfs_handle_t; _para2:pchar; _para3:boolean_t; _para4:boolean_t):cint;cdecl;external external_zfs_library name 'zfs_rename';

type
  snapfilter_cb_t = function (h : Pzfs_handle_t  ;  p : pointer):boolean_t;cdecl;
//typedef boolean_t (snapfilter_cb_t)(zfs_handle_t *, void *);

function zfs_send(_para1:Pzfs_handle_t; _para2:pchar; _para3:pchar; _para4:Psendflags_t; _para5:cint;
           _para6:snapfilter_cb_t; _para7:pointer; _para8:PPnvlist_t):cint;cdecl;external external_zfs_library name 'zfs_send';
function zfs_promote(_para1:Pzfs_handle_t):cint;cdecl;external external_zfs_library name 'zfs_promote';


function zfs_hold(_para1:Pzfs_handle_t; _para2:pchar; _para3:pchar; _para4:boolean_t; _para5:cint):cint;cdecl;external external_zfs_library name 'zfs_hold';
function zfs_hold_nvl(_para1:Pzfs_handle_t; _para2:cint; _para3:Pnvlist_t):cint;cdecl;external external_zfs_library name 'zfs_hold_nvl';


function zfs_release(_para1:Pzfs_handle_t; _para2:pchar; _para3:pchar; _para4:boolean_t):cint;cdecl;external external_zfs_library name 'zfs_release';
function zfs_get_holds(_para1:Pzfs_handle_t; _para2:PPnvlist_t):cint;cdecl;external external_zfs_library name 'zfs_get_holds';
function zvol_volsize_to_reservation(_para1:uint64_t; _para2:Pnvlist_t):uint64_t;cdecl;external external_zfs_library name 'zvol_volsize_to_reservation';

type

  zfs_userspace_cb_t = function (arg:pointer; domain:pchar; rid:uid_t; space:uint64_t):cint;cdecl;

function zfs_userspace(_para1:Pzfs_handle_t; _para2:zfs_userquota_prop_t; _para3:zfs_userspace_cb_t; _para4:pointer):cint;cdecl;external external_zfs_library name 'zfs_userspace';
function zfs_get_fsacl(_para1:Pzfs_handle_t; _para2:PPnvlist_t):cint;cdecl;external external_zfs_library name 'zfs_get_fsacl';
function zfs_set_fsacl(_para1:Pzfs_handle_t; _para2:boolean_t; _para3:Pnvlist_t):cint;cdecl;external external_zfs_library name 'zfs_set_fsacl';

function zfs_receive(_para1:Plibzfs_handle_t; _para2:pchar; _para3:Precvflags_t; _para4:cint; _para5:Pavl_tree_t):cint;cdecl;external external_zfs_library name 'zfs_receive';
type
  diff_flags = (ZFS_DIFF_PARSEABLE := $1,ZFS_DIFF_TIMESTAMP := $2,
    ZFS_DIFF_CLASSIFY := $4);
  diff_flags_t = diff_flags;



function zfs_show_diffs(_para1:Pzfs_handle_t; _para2:cint; _para3:pchar; _para4:pchar; _para5:cint):cint;cdecl;external external_zfs_library name 'zfs_show_diffs';
{
 * Miscellaneous functions.
  }

function zfs_type_to_name(_para1:zfs_type_t):pchar;cdecl;external external_zfs_library name 'zfs_type_to_name';
procedure zfs_refresh_properties(_para1:Pzfs_handle_t);cdecl;external external_zfs_library name 'zfs_refresh_properties';

function zfs_name_valid(_para1:pchar; _para2:zfs_type_t):cint;cdecl;external external_zfs_library name 'zfs_name_valid';
function zfs_path_to_zhandle(_para1:Plibzfs_handle_t; _para2:pchar; _para3:zfs_type_t):Pzfs_handle_t;cdecl;external external_zfs_library name 'zfs_path_to_zhandle';

function zfs_dataset_exists(_para1:Plibzfs_handle_t; _para2:pchar; _para3:zfs_type_t):boolean_t;cdecl;external external_zfs_library name 'zfs_dataset_exists';
function zfs_spa_version(_para1:Pzfs_handle_t; _para2:Pcint):cint;cdecl;external external_zfs_library name 'zfs_spa_version';
{
 * Mount support functions.
  }

function is_mounted(_para1:Plibzfs_handle_t; special:pchar; _para3:Ppchar):boolean_t;cdecl;external external_zfs_library name 'is_mounted';
function zfs_is_mounted(_para1:Pzfs_handle_t; _para2:Ppchar):boolean_t;cdecl;external external_zfs_library name 'zfs_is_mounted';

function zfs_mount(_para1:Pzfs_handle_t; _para2:pchar; _para3:cint):cint;cdecl;external external_zfs_library name 'zfs_mount';

function zfs_unmount(_para1:Pzfs_handle_t; _para2:pchar; _para3:cint):cint;cdecl;external external_zfs_library name 'zfs_unmount';
function zfs_unmountall(_para1:Pzfs_handle_t; _para2:cint):cint;cdecl;external external_zfs_library name 'zfs_unmountall';
{
 * Share support functions.
  }
function zfs_is_shared(_para1:Pzfs_handle_t):boolean_t;cdecl;external external_zfs_library name 'zfs_is_shared';
function zfs_share(_para1:Pzfs_handle_t):cint;cdecl;external external_zfs_library name 'zfs_share';
function zfs_unshare(_para1:Pzfs_handle_t):cint;cdecl;external external_zfs_library name 'zfs_unshare';
{
 * Protocol-specific share support functions.
  }
function zfs_is_shared_nfs(_para1:Pzfs_handle_t; _para2:Ppchar):boolean_t;cdecl;external external_zfs_library name 'zfs_is_shared_nfs';
function zfs_is_shared_smb(_para1:Pzfs_handle_t; _para2:Ppchar):boolean_t;cdecl;external external_zfs_library name 'zfs_is_shared_smb';
function zfs_share_nfs(_para1:Pzfs_handle_t):cint;cdecl;external external_zfs_library name 'zfs_share_nfs';
function zfs_share_smb(_para1:Pzfs_handle_t):cint;cdecl;external external_zfs_library name 'zfs_share_smb';
function zfs_shareall(_para1:Pzfs_handle_t):cint;cdecl;external external_zfs_library name 'zfs_shareall';

function zfs_unshare_nfs(_para1:Pzfs_handle_t; _para2:pchar):cint;cdecl;external external_zfs_library name 'zfs_unshare_nfs';

function zfs_unshare_smb(_para1:Pzfs_handle_t; _para2:pchar):cint;cdecl;external external_zfs_library name 'zfs_unshare_smb';
function zfs_unshareall_nfs(_para1:Pzfs_handle_t):cint;cdecl;external external_zfs_library name 'zfs_unshareall_nfs';
function zfs_unshareall_smb(_para1:Pzfs_handle_t):cint;cdecl;external external_zfs_library name 'zfs_unshareall_smb';

function zfs_unshareall_bypath(_para1:Pzfs_handle_t; _para2:pchar):cint;cdecl;external external_zfs_library name 'zfs_unshareall_bypath';
function zfs_unshareall(_para1:Pzfs_handle_t):cint;cdecl;external external_zfs_library name 'zfs_unshareall';
function zfs_deleg_share_nfs(_para1:Plibzfs_handle_t; _para2:pchar; _para3:pchar; _para4:pchar; _para5:pointer;
           _para6:pointer; _para7:cint; _para8:zfs_share_op_t):cint;cdecl;external external_zfs_library name 'zfs_deleg_share_nfs';
{
 * When dealing with nvlists, verify() is extremely useful
  }
//{$ifdef NDEBUG}
//function verify(EX : longint) : pointer;
//{$else}
//function verify(EX : longint) : longint;
//{$endif}
{
 * Utility function to convert a number to a human-readable form.
  }

procedure zfs_nicenum(_para1:uint64_t; _para2:pchar; _para3:size_t);cdecl;external external_zfs_library name 'zfs_nicenum';

function zfs_nicestrtonum(_para1:Plibzfs_handle_t; _para2:pchar; _para3:Puint64_t):cint;cdecl;external external_zfs_library name 'zfs_nicestrtonum';
{
 * Given a device or file, determine if it is part of a pool.
  }
function zpool_in_use(_para1:Plibzfs_handle_t; _para2:cint; _para3:Ppool_state_t; _para4:Ppchar; _para5:Pboolean_t):cint;cdecl;external external_zfs_library name 'zpool_in_use';
{
 * Label manipulation.
  }
function zpool_read_label(_para1:cint; _para2:PPnvlist_t):cint;cdecl;external external_zfs_library name 'zpool_read_label';
function zpool_clear_label(_para1:cint):cint;cdecl;external external_zfs_library name 'zpool_clear_label';
{ is this zvol valid for use as a dump device?  }
function zvol_check_dump_config(_para1:pchar):cint;cdecl;external external_zfs_library name 'zvol_check_dump_config';
{
 * Management interfaces for SMB ACL files
  }
function zfs_smb_acl_add(_para1:Plibzfs_handle_t; _para2:pchar; _para3:pchar; _para4:pchar):cint;cdecl;external external_zfs_library name 'zfs_smb_acl_add';
function zfs_smb_acl_remove(_para1:Plibzfs_handle_t; _para2:pchar; _para3:pchar; _para4:pchar):cint;cdecl;external external_zfs_library name 'zfs_smb_acl_remove';
function zfs_smb_acl_purge(_para1:Plibzfs_handle_t; _para2:pchar; _para3:pchar):cint;cdecl;external external_zfs_library name 'zfs_smb_acl_purge';
function zfs_smb_acl_rename(_para1:Plibzfs_handle_t; _para2:pchar; _para3:pchar; _para4:pchar; _para5:pchar):cint;cdecl;external external_zfs_library name 'zfs_smb_acl_rename';
{
 * Enable and disable datasets within a pool by mounting/unmounting and
 * sharing/unsharing them.
  }

function zpool_enable_datasets(_para1:Pzpool_handle_t; _para2:pchar; _para3:cint):cint;cdecl;external external_zfs_library name 'zpool_enable_datasets';
function zpool_disable_datasets(_para1:Pzpool_handle_t; _para2:boolean_t):cint;cdecl;external external_zfs_library name 'zpool_disable_datasets';
{
 * Mappings between vdev and FRU.
  }
procedure libzfs_fru_refresh(_para1:Plibzfs_handle_t);cdecl;external external_zfs_library name 'libzfs_fru_refresh';


function libzfs_fru_lookup(_para1:Plibzfs_handle_t; _para2:pchar):pchar;cdecl;external external_zfs_library name 'libzfs_fru_lookup';


function libzfs_fru_devpath(_para1:Plibzfs_handle_t; _para2:pchar):pchar;cdecl;external external_zfs_library name 'libzfs_fru_devpath';


function libzfs_fru_compare(_para1:Plibzfs_handle_t; _para2:pchar; _para3:pchar):boolean_t;cdecl;external external_zfs_library name 'libzfs_fru_compare';

function libzfs_fru_notself(_para1:Plibzfs_handle_t; _para2:pchar):boolean_t;cdecl;external external_zfs_library name 'libzfs_fru_notself';

function zpool_fru_set(_para1:Pzpool_handle_t; _para2:uint64_t; _para3:pchar):cint;cdecl;external external_zfs_library name 'zpool_fru_set';

implementation

function dryrun(var a : splitflags) : cint;
begin
  dryrun:=(a.flag0 and bm_splitflags_dryrun) shr bp_splitflags_dryrun;
end;

procedure set_dryrun(var a : splitflags; __dryrun : cint);
begin
  a.flag0:=a.flag0 or ((__dryrun shl bp_splitflags_dryrun) and bm_splitflags_dryrun);
end;

function import(var a : splitflags) : cint;
begin
  import:=(a.flag0 and bm_splitflags_import) shr bp_splitflags_import;
end;

procedure set_import(var a : splitflags; __import : cint);
begin
  a.flag0:=a.flag0 or ((__import shl bp_splitflags_import) and bm_splitflags_import);
end;

function can_be_active(var a : importargs) : cint;
begin
  can_be_active:=(a.flag0 and bm_importargs_can_be_active) shr bp_importargs_can_be_active;
end;

procedure set_can_be_active(var a : importargs; __can_be_active : cint);
begin
  a.flag0:=a.flag0 or ((__can_be_active shl bp_importargs_can_be_active) and bm_importargs_can_be_active);
end;

function unique(var a : importargs) : cint;
begin
  unique:=(a.flag0 and bm_importargs_unique) shr bp_importargs_unique;
end;

procedure set_unique(var a : importargs; __unique : cint);
begin
  a.flag0:=a.flag0 or ((__unique shl bp_importargs_unique) and bm_importargs_unique);
end;

function exists(var a : importargs) : cint;
begin
  exists:=(a.flag0 and bm_importargs_exists) shr bp_importargs_exists;
end;

procedure set_exists(var a : importargs; __exists : cint);
begin
  a.flag0:=a.flag0 or ((__exists shl bp_importargs_exists) and bm_importargs_exists);
end;

//function verify(EX : longint) : pointer;
//begin
//  verify:=pointer(EX);
//end;
//
//function verify(EX : longint) : longint;
//begin
//  verify:=assert(EX<>0);
//end;


end.
