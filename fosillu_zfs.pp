unit fosillu_zfs;
interface

uses
  unix,unixtype,fos_illumos_defs;

const
  External_library=''; {Setup as you need}

Type
Puint64_t  = ^uint64_t;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


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
 * Copyright (c) 2013 by Delphix. All rights reserved.
 * Copyright 2011 Nexenta Systems, Inc.  All rights reserved.
 * Copyright (c) 2012, Joyent, Inc. All rights reserved.
  }
{ Portions Copyright 2010 Robert Milkowski  }

{
 * Types and constants shared between userland and the kernel.
  }
{
 * Each dataset can be one of the following types.  These constants can be
 * combined into masks that can be passed to various functions.
  }
const
  ZFS_TYPE_FILESYSTEM =  1;
  ZFS_TYPE_SNAPSHOT   =  2;
  ZFS_TYPE_VOLUME     =  4;
  ZFS_TYPE_POOL       =  8;
  ZFS_TYPE_BOOKMARK   = 16;

type
  zfs_type_t = cint; { FOS: was enum }

{ For testing only!  }
{ Be careful!  }

  dmu_objset_type = (DMU_OST_NONE,DMU_OST_META,DMU_OST_ZFS,DMU_OST_ZVOL,
    DMU_OST_OTHER,DMU_OST_ANY,DMU_OST_NUMTYPES
    );
  dmu_objset_type_t = dmu_objset_type;

const
  ZFS_TYPE_DATASET   = (ZFS_TYPE_FILESYSTEM or ZFS_TYPE_VOLUME) or ZFS_TYPE_SNAPSHOT;
  ZAP_MAXNAMELEN     = 256;
  ZAP_MAXVALUELEN    = 1024*8;
  ZAP_OLDMAXVALUELEN = 1024;  
{
 * Dataset properties are identified by these constants and must be added to
 * the end of this list to ensure that external consumers are not affected
 * by the change. If you make any changes to this list, be sure to update
 * the property table in usr/src/common/zfs/zfs_prop.c.
  }

type
  zfs_prop_t = (ZFS_PROP_TYPE,ZFS_PROP_CREATION,ZFS_PROP_USED,
    ZFS_PROP_AVAILABLE,ZFS_PROP_REFERENCED,
    ZFS_PROP_COMPRESSRATIO,ZFS_PROP_MOUNTED,
    ZFS_PROP_ORIGIN,ZFS_PROP_QUOTA,ZFS_PROP_RESERVATION,
    ZFS_PROP_VOLSIZE,ZFS_PROP_VOLBLOCKSIZE,
    ZFS_PROP_RECORDSIZE,ZFS_PROP_MOUNTPOINT,
    ZFS_PROP_SHARENFS,ZFS_PROP_CHECKSUM,ZFS_PROP_COMPRESSION,
    ZFS_PROP_ATIME,ZFS_PROP_DEVICES,ZFS_PROP_EXEC,
    ZFS_PROP_SETUID,ZFS_PROP_READONLY,ZFS_PROP_ZONED,
    ZFS_PROP_SNAPDIR,ZFS_PROP_ACLMODE,ZFS_PROP_ACLINHERIT,
    ZFS_PROP_CREATETXG,ZFS_PROP_NAME,ZFS_PROP_CANMOUNT,
    ZFS_PROP_ISCSIOPTIONS,ZFS_PROP_XATTR,ZFS_PROP_NUMCLONES,
    ZFS_PROP_COPIES,ZFS_PROP_VERSION,ZFS_PROP_UTF8ONLY,
    ZFS_PROP_NORMALIZE,ZFS_PROP_CASE,ZFS_PROP_VSCAN,
    ZFS_PROP_NBMAND,ZFS_PROP_SHARESMB,ZFS_PROP_REFQUOTA,
    ZFS_PROP_REFRESERVATION,ZFS_PROP_GUID,ZFS_PROP_PRIMARYCACHE,
    ZFS_PROP_SECONDARYCACHE,ZFS_PROP_USEDSNAP,
    ZFS_PROP_USEDDS,ZFS_PROP_USEDCHILD,ZFS_PROP_USEDREFRESERV,
    ZFS_PROP_USERACCOUNTING,ZFS_PROP_STMF_SHAREINFO,
    ZFS_PROP_DEFER_DESTROY,ZFS_PROP_USERREFS,
    ZFS_PROP_LOGBIAS,ZFS_PROP_UNIQUE,ZFS_PROP_OBJSETID,
    ZFS_PROP_DEDUP,ZFS_PROP_MLSLABEL,ZFS_PROP_SYNC,
    ZFS_PROP_REFRATIO,ZFS_PROP_WRITTEN,ZFS_PROP_CLONES,
    ZFS_PROP_LOGICALUSED,ZFS_PROP_LOGICALREFERENCED,
    ZFS_PROP_INCONSISTENT,ZFS_NUM_PROPS);

  zfs_userquota_prop_t = (ZFS_PROP_USERUSED,ZFS_PROP_USERQUOTA,ZFS_PROP_GROUPUSED,
    ZFS_PROP_GROUPQUOTA,ZFS_NUM_USERQUOTA_PROPS
    );

  //var
  //  zfs_userquota_prop_prefixes : array[0..(ZFS_NUM_USERQUOTA_PROPS)-1] of ^cchar;cvar;external;
{
 * Pool properties are identified by these constants and must be added to the
 * end of this list to ensure that external consumers are not affected
 * by the change. If you make any changes to this list, be sure to update
 * the property table in usr/src/common/zfs/zpool_prop.c.
  }
type
  zpool_prop_t = (ZPOOL_PROP_NAME,ZPOOL_PROP_SIZE,ZPOOL_PROP_CAPACITY,
    ZPOOL_PROP_ALTROOT,ZPOOL_PROP_HEALTH,ZPOOL_PROP_GUID,
    ZPOOL_PROP_VERSION,ZPOOL_PROP_BOOTFS,ZPOOL_PROP_DELEGATION,
    ZPOOL_PROP_AUTOREPLACE,ZPOOL_PROP_CACHEFILE,
    ZPOOL_PROP_FAILUREMODE,ZPOOL_PROP_LISTSNAPS,
    ZPOOL_PROP_AUTOEXPAND,ZPOOL_PROP_DEDUPDITTO,
    ZPOOL_PROP_DEDUPRATIO,ZPOOL_PROP_FREE,ZPOOL_PROP_ALLOCATED,
    ZPOOL_PROP_READONLY,ZPOOL_PROP_COMMENT,
    ZPOOL_PROP_EXPANDSZ,ZPOOL_PROP_FREEING,
    ZPOOL_NUM_PROPS);
{ Small enough to not hog a whole line of printout in zpool(1M).  }

const
  ZPROP_MAX_COMMENT = 32;  
  ZPROP_CONT = -(2);  
  ZPROP_INVAL = -(1);  
  ZPROP_VALUE = 'value';  
  ZPROP_SOURCE = 'source';  
type
  zprop_source_t = (ZPROP_SRC_NONE := $1,ZPROP_SRC_DEFAULT := $2,
    ZPROP_SRC_TEMPORARY := $4,ZPROP_SRC_LOCAL := $8,
    ZPROP_SRC_INHERITED := $10,ZPROP_SRC_RECEIVED := $20
    );

const
  ZPROP_SRC_ALL = $3f;  
  ZPROP_SOURCE_VAL_RECVD = '$recvd';  
  ZPROP_N_MORE_ERRORS = 'N_MORE_ERRORS';  
{
 * Dataset flag implemented as a special entry in the props zap object
 * indicating that the dataset has received properties on or after
 * SPA_VERSION_RECVD_PROPS. The first such receive blows away local properties
 * just as it did in earlier versions, and thereafter, local properties are
 * preserved.
  }
  ZPROP_HAS_RECVD = '$hasrecvd';  
{ failure to clear existing props  }
{ failure to restore props on error  }
type
  zprop_errflags_t = (ZPROP_ERR_NOCLEAR := $1,ZPROP_ERR_NORESTORE := $2
    );

  zprop_func = function (_para1:cint; _para2:pointer):cint;cdecl;
{
 * Properties to be set on the root file system of a new pool
 * are stuffed into their own nvlist, which is then included in
 * the properties nvlist with the pool properties.
  }

const
  ZPOOL_ROOTFS_PROPS = 'root-props-nvl';  
{
 * Dataset property functions shared between libzfs and kernel.
  }


function zfs_prop_default_string(_para1:zfs_prop_t):pchar;cdecl;external External_library name 'zfs_prop_default_string';
function zfs_prop_default_numeric(_para1:zfs_prop_t):uint64_t;cdecl;external External_library name 'zfs_prop_default_numeric';
function zfs_prop_readonly_(_para1:zfs_prop_t):boolean_t;cdecl;external External_library name 'zfs_prop_readonly';
function zfs_prop_inheritable(_para1:zfs_prop_t):boolean_t;cdecl;external External_library name 'zfs_prop_inheritable';
function zfs_prop_setonce(_para1:zfs_prop_t):boolean_t;cdecl;external External_library name 'zfs_prop_setonce';

function zfs_prop_to_name(_para1:zfs_prop_t):pchar;cdecl;external External_library name 'zfs_prop_to_name';

function zfs_name_to_prop(_para1:pchar):zfs_prop_t;cdecl;external External_library name 'zfs_name_to_prop';

function zfs_prop_user(_para1:pchar):boolean_t;cdecl;external External_library name 'zfs_prop_user';

function zfs_prop_userquota_(_para1:pchar):boolean_t;cdecl;external External_library name 'zfs_prop_userquota';

function zfs_prop_written_(_para1:pchar):boolean_t;cdecl;external External_library name 'zfs_prop_written';

function zfs_prop_index_to_string(_para1:zfs_prop_t; _para2:uint64_t; _para3:Ppchar):cint;cdecl;external External_library name 'zfs_prop_index_to_string';

function zfs_prop_string_to_index(_para1:zfs_prop_t; _para2:pchar; _para3:Puint64_t):cint;cdecl;external External_library name 'zfs_prop_string_to_index';
function zfs_prop_random_value(_para1:zfs_prop_t; seed:uint64_t):uint64_t;cdecl;external External_library name 'zfs_prop_random_value';
function zfs_prop_valid_for_type(_para1:cint; _para2:zfs_type_t):boolean_t;cdecl;external External_library name 'zfs_prop_valid_for_type';
{
 * Pool property functions shared between libzfs and kernel.
  }

function zpool_name_to_prop(_para1:pchar):zpool_prop_t;cdecl;external External_library name 'zpool_name_to_prop';

function zpool_prop_to_name(_para1:zpool_prop_t):pchar;cdecl;external External_library name 'zpool_prop_to_name';

function zpool_prop_default_string(_para1:zpool_prop_t):pchar;cdecl;external External_library name 'zpool_prop_default_string';
function zpool_prop_default_numeric(_para1:zpool_prop_t):uint64_t;cdecl;external External_library name 'zpool_prop_default_numeric';
function _zpool_prop_readonly(_para1:zpool_prop_t):boolean_t;cdecl;external External_library name 'zpool_prop_readonly';

function zpool_prop_feature(_para1:pchar):boolean_t;cdecl;external External_library name 'zpool_prop_feature';

function zpool_prop_unsupported(name:pchar):boolean_t;cdecl;external External_library name 'zpool_prop_unsupported';

function zpool_prop_index_to_string(_para1:zpool_prop_t; _para2:uint64_t; _para3:Ppchar):cint;cdecl;external External_library name 'zpool_prop_index_to_string';

function zpool_prop_string_to_index(_para1:zpool_prop_t; _para2:pchar; _para3:Puint64_t):cint;cdecl;external External_library name 'zpool_prop_string_to_index';
function zpool_prop_random_value(_para1:zpool_prop_t; seed:uint64_t):uint64_t;cdecl;external External_library name 'zpool_prop_random_value';
{
 * Definitions for the Delegation.
  }
type
  zfs_deleg_who_type_t = (ZFS_DELEG_WHO_UNKNOWN := 0,ZFS_DELEG_USER := 'u',
    ZFS_DELEG_USER_SETS := 'U',ZFS_DELEG_GROUP := 'g',
    ZFS_DELEG_GROUP_SETS := 'G',ZFS_DELEG_EVERYONE := 'e',
    ZFS_DELEG_EVERYONE_SETS := 'E',ZFS_DELEG_CREATE := 'c',
    ZFS_DELEG_CREATE_SETS := 'C',ZFS_DELEG_NAMED_SET := 's',
    ZFS_DELEG_NAMED_SET_SETS := 'S');

  zfs_deleg_inherit_t = (ZFS_DELEG_NONE := 0,ZFS_DELEG_PERM_LOCAL := 1,
    ZFS_DELEG_PERM_DESCENDENT := 2,ZFS_DELEG_PERM_LOCALDESCENDENT := 3,
    ZFS_DELEG_PERM_CREATE := 4);

const
  ZFS_DELEG_PERM_UID = 'uid';  
  ZFS_DELEG_PERM_GID = 'gid';  
  ZFS_DELEG_PERM_GROUPS = 'groups';  
  ZFS_MLSLABEL_DEFAULT = 'none';  
  ZFS_SMB_ACL_SRC = 'src';  
  ZFS_SMB_ACL_TARGET = 'target';  
type
  zfs_canmount_type_t = (ZFS_CANMOUNT_OFF := 0,ZFS_CANMOUNT_ON := 1,
    ZFS_CANMOUNT_NOAUTO := 2);

  zfs_logbias_op_t = (ZFS_LOGBIAS_LATENCY := 0,ZFS_LOGBIAS_THROUGHPUT := 1
    );

  zfs_share_op = (ZFS_SHARE_NFS := 0,ZFS_UNSHARE_NFS := 1,
    ZFS_SHARE_SMB := 2,ZFS_UNSHARE_SMB := 3
    );
  zfs_share_op_t = zfs_share_op;

  zfs_smb_acl_op = (ZFS_SMB_ACL_ADD,ZFS_SMB_ACL_REMOVE,ZFS_SMB_ACL_RENAME,
    ZFS_SMB_ACL_PURGE);
  zfs_smb_acl_op_t = zfs_smb_acl_op;

  zfs_cache_type = (ZFS_CACHE_NONE := 0,ZFS_CACHE_METADATA := 1,
    ZFS_CACHE_ALL := 2);
  zfs_cache_type_t = zfs_cache_type;

  zfs_sync_type_t = (ZFS_SYNC_STANDARD := 0,ZFS_SYNC_ALWAYS := 1,
    ZFS_SYNC_DISABLED := 2);
{
 * On-disk version number.
  }

const
  SPA_VERSION_1 = 1;  
  SPA_VERSION_2 = 2;  
  SPA_VERSION_3 = 3;  
  SPA_VERSION_4 = 4;  
  SPA_VERSION_5 = 5;  
  SPA_VERSION_6 = 6;  
  SPA_VERSION_7 = 7;  
  SPA_VERSION_8 = 8;  
  SPA_VERSION_9 = 9;  
  SPA_VERSION_10 = 10;  
  SPA_VERSION_11 = 11;  
  SPA_VERSION_12 = 12;  
  SPA_VERSION_13 = 13;  
  SPA_VERSION_14 = 14;  
  SPA_VERSION_15 = 15;  
  SPA_VERSION_16 = 16;  
  SPA_VERSION_17 = 17;  
  SPA_VERSION_18 = 18;  
  SPA_VERSION_19 = 19;  
  SPA_VERSION_20 = 20;  
  SPA_VERSION_21 = 21;  
  SPA_VERSION_22 = 22;  
  SPA_VERSION_23 = 23;  
  SPA_VERSION_24 = 24;  
  SPA_VERSION_25 = 25;  
  SPA_VERSION_26 = 26;  
  SPA_VERSION_27 = 27;  
  SPA_VERSION_28 = 28;  
  SPA_VERSION_5000 = 5000;  
{
 * When bumping up SPA_VERSION, make sure GRUB ZFS understands the on-disk
 * format change. Go to usr/src/grub/grub-0.97/stage2/zfs-include/, fsys_zfs*,
 * and do the appropriate changes.  Also bump the version number in
 * usr/src/grub/capability.
  }
  SPA_VERSION = SPA_VERSION_5000;  
  SPA_VERSION_STRING = '5000';  
{
 * Symbolic names for the changes that caused a SPA_VERSION switch.
 * Used in the code when checking for presence or absence of a feature.
 * Feel free to define multiple symbolic names for each version if there
 * were multiple changes to on-disk structures during that version.
 *
 * NOTE: When checking the current SPA_VERSION in your code, be sure
 *       to use spa_version() since it reports the version of the
 *       last synced uberblock.  Checking the in-flight version can
 *       be dangerous in some cases.
  }
  SPA_VERSION_INITIAL = SPA_VERSION_1;  
  SPA_VERSION_DITTO_BLOCKS = SPA_VERSION_2;  
  SPA_VERSION_SPARES = SPA_VERSION_3;  
  SPA_VERSION_RAIDZ2 = SPA_VERSION_3;  
  SPA_VERSION_BPOBJ_ACCOUNT = SPA_VERSION_3;  
  SPA_VERSION_RAIDZ_DEFLATE = SPA_VERSION_3;  
  SPA_VERSION_DNODE_BYTES = SPA_VERSION_3;  
  SPA_VERSION_ZPOOL_HISTORY = SPA_VERSION_4;  
  SPA_VERSION_GZIP_COMPRESSION = SPA_VERSION_5;  
  SPA_VERSION_BOOTFS = SPA_VERSION_6;  
  SPA_VERSION_SLOGS = SPA_VERSION_7;  
  SPA_VERSION_DELEGATED_PERMS = SPA_VERSION_8;  
  SPA_VERSION_FUID = SPA_VERSION_9;  
  SPA_VERSION_REFRESERVATION = SPA_VERSION_9;  
  SPA_VERSION_REFQUOTA = SPA_VERSION_9;  
  SPA_VERSION_UNIQUE_ACCURATE = SPA_VERSION_9;  
  SPA_VERSION_L2CACHE = SPA_VERSION_10;  
  SPA_VERSION_NEXT_CLONES = SPA_VERSION_11;  
  SPA_VERSION_ORIGIN = SPA_VERSION_11;  
  SPA_VERSION_DSL_SCRUB = SPA_VERSION_11;  
  SPA_VERSION_SNAP_PROPS = SPA_VERSION_12;  
  SPA_VERSION_USED_BREAKDOWN = SPA_VERSION_13;  
  SPA_VERSION_PASSTHROUGH_X = SPA_VERSION_14;  
  SPA_VERSION_USERSPACE = SPA_VERSION_15;  
  SPA_VERSION_STMF_PROP = SPA_VERSION_16;  
  SPA_VERSION_RAIDZ3 = SPA_VERSION_17;  
  SPA_VERSION_USERREFS = SPA_VERSION_18;  
  SPA_VERSION_HOLES = SPA_VERSION_19;  
  SPA_VERSION_ZLE_COMPRESSION = SPA_VERSION_20;  
  SPA_VERSION_DEDUP = SPA_VERSION_21;  
  SPA_VERSION_RECVD_PROPS = SPA_VERSION_22;  
  SPA_VERSION_SLIM_ZIL = SPA_VERSION_23;  
  SPA_VERSION_SA = SPA_VERSION_24;  
  SPA_VERSION_SCAN = SPA_VERSION_25;  
  SPA_VERSION_DIR_CLONES = SPA_VERSION_26;  
  SPA_VERSION_DEADLISTS = SPA_VERSION_26;  
  SPA_VERSION_FAST_SNAP = SPA_VERSION_27;  
  SPA_VERSION_MULTI_REPLACE = SPA_VERSION_28;  
  SPA_VERSION_BEFORE_FEATURES = SPA_VERSION_28;  
  SPA_VERSION_FEATURES = SPA_VERSION_5000;  
{#define	SPA_VERSION_IS_SUPPORTED(v) \ }
{	(((v) >= SPA_VERSION_INITIAL && (v) <= SPA_VERSION_BEFORE_FEATURES) || \ }
{	((v) >= SPA_VERSION_FEATURES && (v) <= SPA_VERSION)) }
{
 * ZPL version - rev'd whenever an incompatible on-disk format change
 * occurs.  This is independent of SPA/DMU/ZAP versioning.  You must
 * also update the version_table[] and help message in zfs_prop.c.
 *
 * When changing, be sure to teach GRUB how to read the new format!
 * See usr/src/grub/grub-0.97/stage2/zfs-include/,fsys_zfs*
  }
  ZPL_VERSION_1 = 1;  
  ZPL_VERSION_2 = 2;  
  ZPL_VERSION_3 = 3;  
  ZPL_VERSION_4 = 4;  
  ZPL_VERSION_5 = 5;  
  ZPL_VERSION = ZPL_VERSION_5;  
  ZPL_VERSION_STRING = '5';  
  ZPL_VERSION_INITIAL = ZPL_VERSION_1;  
  ZPL_VERSION_DIRENT_TYPE = ZPL_VERSION_2;  
  ZPL_VERSION_FUID = ZPL_VERSION_3;  
  ZPL_VERSION_NORMALIZATION = ZPL_VERSION_3;  
  ZPL_VERSION_SYSATTR = ZPL_VERSION_3;  
  ZPL_VERSION_USERSPACE = ZPL_VERSION_4;  
  ZPL_VERSION_SA = ZPL_VERSION_5;  
{ Rewind request information  }
{ No policy - default behavior  }
  ZPOOL_NO_REWIND = 1;  
{ Do not search for best txg or rewind  }
  ZPOOL_NEVER_REWIND = 2;  
{ Search for best txg, but do not rewind  }
  ZPOOL_TRY_REWIND = 4;  
{ Rewind to best txg w/in deferred frees  }
  ZPOOL_DO_REWIND = 8;  
{ Allow extreme measures to find best txg  }
  ZPOOL_EXTREME_REWIND = 16;  
{ All the possible rewind bits  }
  ZPOOL_REWIND_MASK = 28;  
{ All the possible policy bits  }
  ZPOOL_REWIND_POLICIES = 31;  
{ rewind behavior requested  }
{ max acceptable meta-data errors  }
{ max acceptable data errors  }
{ specific txg to load  }
type
  zpool_rewind_policy = record
      zrp_request : uint32_t;
      zrp_maxmeta : uint64_t;
      zrp_maxdata : uint64_t;
      zrp_txg : uint64_t;
    end;
  zpool_rewind_policy_t = zpool_rewind_policy;
{
 * The following are configuration names used in the nvlist describing a pool's
 * configuration.
  }

const
  ZPOOL_CONFIG_VERSION = 'version';  
  ZPOOL_CONFIG_POOL_NAME = 'name';  
  ZPOOL_CONFIG_POOL_STATE = 'state';  
  ZPOOL_CONFIG_POOL_TXG = 'txg';  
  ZPOOL_CONFIG_POOL_GUID = 'pool_guid';  
  ZPOOL_CONFIG_CREATE_TXG = 'create_txg';  
  ZPOOL_CONFIG_TOP_GUID = 'top_guid';  
  ZPOOL_CONFIG_VDEV_TREE = 'vdev_tree';  
  ZPOOL_CONFIG_TYPE = 'type';  
  ZPOOL_CONFIG_CHILDREN = 'children';  
  ZPOOL_CONFIG_ID = 'id';  
  ZPOOL_CONFIG_GUID = 'guid';  
  ZPOOL_CONFIG_PATH = 'path';  
  ZPOOL_CONFIG_DEVID = 'devid';  
  ZPOOL_CONFIG_METASLAB_ARRAY = 'metaslab_array';  
  ZPOOL_CONFIG_METASLAB_SHIFT = 'metaslab_shift';  
  ZPOOL_CONFIG_ASHIFT = 'ashift';  
  ZPOOL_CONFIG_ASIZE = 'asize';  
  ZPOOL_CONFIG_DTL = 'DTL';  
{ not stored on disk  }
  ZPOOL_CONFIG_SCAN_STATS = 'scan_stats';  
{ not stored on disk  }
  ZPOOL_CONFIG_VDEV_STATS = 'vdev_stats';  
  ZPOOL_CONFIG_WHOLE_DISK = 'whole_disk';  
  ZPOOL_CONFIG_ERRCOUNT = 'error_count';  
  ZPOOL_CONFIG_NOT_PRESENT = 'not_present';  
  ZPOOL_CONFIG_SPARES = 'spares';  
  ZPOOL_CONFIG_IS_SPARE = 'is_spare';  
  ZPOOL_CONFIG_NPARITY = 'nparity';  
  ZPOOL_CONFIG_HOSTID = 'hostid';  
  ZPOOL_CONFIG_HOSTNAME = 'hostname';  
  ZPOOL_CONFIG_LOADED_TIME = 'initial_load_time';  
  ZPOOL_CONFIG_UNSPARE = 'unspare';  
  ZPOOL_CONFIG_PHYS_PATH = 'phys_path';  
  ZPOOL_CONFIG_IS_LOG = 'is_log';  
  ZPOOL_CONFIG_L2CACHE = 'l2cache';  
  ZPOOL_CONFIG_HOLE_ARRAY = 'hole_array';  
  ZPOOL_CONFIG_VDEV_CHILDREN = 'vdev_children';  
  ZPOOL_CONFIG_IS_HOLE = 'is_hole';  
  ZPOOL_CONFIG_DDT_HISTOGRAM = 'ddt_histogram';  
  ZPOOL_CONFIG_DDT_OBJ_STATS = 'ddt_object_stats';  
  ZPOOL_CONFIG_DDT_STATS = 'ddt_stats';  
  ZPOOL_CONFIG_SPLIT = 'splitcfg';  
  ZPOOL_CONFIG_ORIG_GUID = 'orig_guid';  
  ZPOOL_CONFIG_SPLIT_GUID = 'split_guid';  
  ZPOOL_CONFIG_SPLIT_LIST = 'guid_list';  
  ZPOOL_CONFIG_REMOVING = 'removing';  
  ZPOOL_CONFIG_RESILVER_TXG = 'resilver_txg';  
  ZPOOL_CONFIG_COMMENT = 'comment';  
{ not stored on disk  }
  ZPOOL_CONFIG_SUSPENDED = 'suspended';  
{ not stored on disk  }
  ZPOOL_CONFIG_TIMESTAMP = 'timestamp';  
{ not stored on disk  }
  ZPOOL_CONFIG_BOOTFS = 'bootfs';  
{ not stored on disk  }
  ZPOOL_CONFIG_MISSING_DEVICES = 'missing_vdevs';  
{ not stored on disk  }
  ZPOOL_CONFIG_LOAD_INFO = 'load_info';  
{ not stored on disk  }
  ZPOOL_CONFIG_REWIND_INFO = 'rewind_info';  
{ not stored on disk  }
  ZPOOL_CONFIG_UNSUP_FEAT = 'unsup_feat';  
{ not stored on disk  }
  ZPOOL_CONFIG_ENABLED_FEAT = 'enabled_feat';  
{ not stored on disk  }
  ZPOOL_CONFIG_CAN_RDONLY = 'can_rdonly';  
  ZPOOL_CONFIG_FEATURES_FOR_READ = 'features_for_read';  
{ not stored on disk  }
  ZPOOL_CONFIG_FEATURE_STATS = 'feature_stats';  
{
 * The persistent vdev state is stored as separate values rather than a single
 * 'vdev_state' entry.  This is because a device can be in multiple states, such
 * as offline and degraded.
  }
  ZPOOL_CONFIG_OFFLINE = 'offline';  
  ZPOOL_CONFIG_FAULTED = 'faulted';  
  ZPOOL_CONFIG_DEGRADED = 'degraded';  
  ZPOOL_CONFIG_REMOVED = 'removed';  
  ZPOOL_CONFIG_FRU = 'fru';  
  ZPOOL_CONFIG_AUX_STATE = 'aux_state';  
{ Rewind policy parameters  }
  ZPOOL_REWIND_POLICY_ = 'rewind-policy';
  ZPOOL_REWIND_REQUEST = 'rewind-request';  
  ZPOOL_REWIND_REQUEST_TXG = 'rewind-request-txg';  
  ZPOOL_REWIND_META_THRESH = 'rewind-meta-thresh';  
  ZPOOL_REWIND_DATA_THRESH = 'rewind-data-thresh';  
{ Rewind data discovered  }
  ZPOOL_CONFIG_LOAD_TIME = 'rewind_txg_ts';  
  ZPOOL_CONFIG_LOAD_DATA_ERRORS = 'verify_data_errors';  
  ZPOOL_CONFIG_REWIND_TIME = 'seconds_of_rewind';  
  VDEV_TYPE_ROOT = 'root';  
  VDEV_TYPE_MIRROR = 'mirror';  
  VDEV_TYPE_REPLACING = 'replacing';  
  VDEV_TYPE_RAIDZ = 'raidz';  
  VDEV_TYPE_DISK = 'disk';  
  VDEV_TYPE_FILE = 'file';  
  VDEV_TYPE_MISSING = 'missing';  
  VDEV_TYPE_HOLE = 'hole';  
  VDEV_TYPE_SPARE = 'spare';  
  VDEV_TYPE_LOG = 'log';  
  VDEV_TYPE_L2CACHE = 'l2cache';  
{
 * This is needed in userland to report the minimum necessary device size.
  }
  SPA_MINDEVSIZE = 64 shl 20;  
{
 * The location of the pool configuration repository, shared between kernel and
 * userland.
  }
  ZPOOL_CACHE = '/etc/zfs/zpool.cache';  
{
 * vdev states are ordered from least to most healthy.
 * A vdev that's CANT_OPEN or below is considered unusable.
  }
{ Uninitialized vdev			 }
{ Not currently open			 }
{ Not allowed to open			 }
{ Explicitly removed from system	 }
{ Tried to open, but failed		 }
{ External request to fault device	 }
{ Replicated vdev with unhealthy kids	 }
{ Presumed good			 }
type
  vdev_state = (VDEV_STATE_UNKNOWN := 0,VDEV_STATE_CLOSED,
    VDEV_STATE_OFFLINE,VDEV_STATE_REMOVED,VDEV_STATE_CANT_OPEN,
    VDEV_STATE_FAULTED,VDEV_STATE_DEGRADED,
    VDEV_STATE_HEALTHY);
  vdev_state_t = vdev_state;

const
  VDEV_STATE_ONLINE = VDEV_STATE_HEALTHY;  
{
 * vdev aux states.  When a vdev is in the CANT_OPEN state, the aux field
 * of the vdev stats structure uses these constants to distinguish why.
  }
{ no error				 }
{ ldi_open_*() or vn_open() failed	 }
{ bad label or disk contents		 }
{ insufficient number of replicas	 }
{ vdev guid sum doesn't match		 }
{ vdev size is too small		 }
{ the label is OK but invalid		 }
{ on-disk version is too new		 }
{ on-disk version is too old		 }
{ unsupported features			 }
{ hot spare used in another pool	 }
{ too many errors			 }
{ experienced I/O failure		 }
{ cannot read log chain(s)		 }
{ external diagnosis			 }
{ vdev was split off into another pool	 }
type
  vdev_aux = (VDEV_AUX_NONE,VDEV_AUX_OPEN_FAILED,VDEV_AUX_CORRUPT_DATA,
    VDEV_AUX_NO_REPLICAS,VDEV_AUX_BAD_GUID_SUM,
    VDEV_AUX_TOO_SMALL,VDEV_AUX_BAD_LABEL,VDEV_AUX_VERSION_NEWER,
    VDEV_AUX_VERSION_OLDER,VDEV_AUX_UNSUP_FEAT,
    VDEV_AUX_SPARED,VDEV_AUX_ERR_EXCEEDED,VDEV_AUX_IO_FAILURE,
    VDEV_AUX_BAD_LOG,VDEV_AUX_EXTERNAL,VDEV_AUX_SPLIT_POOL
    );
  vdev_aux_t = vdev_aux;
{
 * pool state.  The following states are written to disk as part of the normal
 * SPA lifecycle: ACTIVE, EXPORTED, DESTROYED, SPARE, L2CACHE.  The remaining
 * states are software abstractions used at various levels to communicate
 * pool state.
  }
{ In active use		 }
{ Explicitly exported		 }
{ Explicitly destroyed		 }
{ Reserved for hot spare use	 }
{ Level 2 ARC device		 }
{ Internal spa_t state		 }
{ Internal libzfs state	 }
{ Internal libzfs state	 }

  pool_state = (POOL_STATE_ACTIVE := 0,POOL_STATE_EXPORTED,
    POOL_STATE_DESTROYED,POOL_STATE_SPARE,POOL_STATE_L2CACHE,
    POOL_STATE_UNINITIALIZED,POOL_STATE_UNAVAIL,
    POOL_STATE_POTENTIALLY_ACTIVE);
  pool_state_t = pool_state;
{
 * Scan Functions.
  }

  pool_scan_func = (POOL_SCAN_NONE,POOL_SCAN_SCRUB,POOL_SCAN_RESILVER,
    POOL_SCAN_FUNCS);
  pool_scan_func_t = pool_scan_func;
{
 * ZIO types.  Needed to interpret vdev statistics below.
  }

  zio_type = (ZIO_TYPE_NULL := 0,ZIO_TYPE_READ,ZIO_TYPE_WRITE,
    ZIO_TYPE_FREE,ZIO_TYPE_CLAIM,ZIO_TYPE_IOCTL,
    ZIO_TYPES);
  zio_type_t = zio_type;
{
 * Pool statistics.  Note: all fields should be 64-bit because this
 * is passed between kernel and userland as an nvlist uint64 array.
  }
{ values stored on disk  }
{ pool_scan_func_t  }
{ dsl_scan_state_t  }
{ scan start time  }
{ scan end time  }
{ total bytes to scan  }
{ total examined bytes	 }
{ total bytes to process  }
{ total processed bytes  }
{ scan errors	 }
{ values not stored on disk  }
{ examined bytes per scan pass  }
{ start time of a scan pass  }

  pool_scan_stat = record
      pss_func : uint64_t;
      pss_state : uint64_t;
      pss_start_time : uint64_t;
      pss_end_time : uint64_t;
      pss_to_examine : uint64_t;
      pss_examined : uint64_t;
      pss_to_process : uint64_t;
      pss_processed : uint64_t;
      pss_errors : uint64_t;
      pss_pass_exam : uint64_t;
      pss_pass_start : uint64_t;
    end;
  pool_scan_stat_t = pool_scan_stat;
  Ppool_scan_stat_t = ^pool_scan_stat_t;

  dsl_scan_state = (DSS_NONE,DSS_SCANNING,DSS_FINISHED,DSS_CANCELED,
    DSS_NUM_STATES);
  dsl_scan_state_t = dsl_scan_state;
{
 * Vdev statistics.  Note: all fields should be 64-bit because this
 * is passed between kernel and userland as an nvlist uint64 array.
  }
{ time since vdev load	 }
{ vdev state		 }
{ see vdev_aux_t	 }
{ space allocated	 }
{ total capacity	 }
{ deflated capacity	 }
{ replaceable dev size  }
{ expandable dev size  }
{ operation count	 }
{ bytes read/written	 }
{ read errors		 }
{ write errors		 }
{ checksum errors	 }
{ self-healed bytes	 }
{ removing?	 }
{ scan processed bytes	 }

  vdev_stat = record
      vs_timestamp : hrtime_t;
      vs_state : uint64_t;
      vs_aux : uint64_t;
      vs_alloc : uint64_t;
      vs_space : uint64_t;
      vs_dspace : uint64_t;
      vs_rsize : uint64_t;
      vs_esize : uint64_t;
      vs_ops : array[0..ord(ZIO_TYPES)-1] of uint64_t;
      vs_bytes : array[0..ord(ZIO_TYPES)-1] of uint64_t;
      vs_read_errors : uint64_t;
      vs_write_errors : uint64_t;
      vs_checksum_errors : uint64_t;
      vs_self_healed : uint64_t;
      vs_scan_removing : uint64_t;
      vs_scan_processed : uint64_t;
    end;
  vdev_stat_t  = vdev_stat;
  Pvdev_stat_t = ^vdev_stat_t;
{
 * DDT statistics.  Note: all fields should be 64-bit because this
 * is passed between kernel and userland as an nvlist uint64 array.
  }
{ number of elments in ddt 	 }
{ size of ddt on disk		 }
{ size of ddt in-core		 }

  ddt_object = record
      ddo_count : uint64_t;
      ddo_dspace : uint64_t;
      ddo_mspace : uint64_t;
    end;
  ddt_object_t = ddt_object;
{ blocks			 }
{ logical size			 }
{ physical size		 }
{ deflated allocated size	 }
{ referenced blocks		 }
{ referenced lsize * refcnt	 }
{ referenced psize * refcnt	 }
{ referenced dsize * refcnt	 }

  ddt_stat = record
      dds_blocks : uint64_t;
      dds_lsize : uint64_t;
      dds_psize : uint64_t;
      dds_dsize : uint64_t;
      dds_ref_blocks : uint64_t;
      dds_ref_lsize : uint64_t;
      dds_ref_psize : uint64_t;
      dds_ref_dsize : uint64_t;
    end;
  ddt_stat_t = ddt_stat;
{ power-of-two histogram buckets  }

  ddt_histogram = record
      ddh_stat : array[0..63] of ddt_stat_t;
    end;
  ddt_histogram_t = ddt_histogram;

const
  ZVOL_DRIVER = 'zvol';  
  ZFS_DRIVER = 'zfs';  
  ZFS_DEV = '/dev/zfs';  
{ general zvol path  }
  ZVOL_DIR = '/dev/zvol';  
{ expansion  }
  ZVOL_PSEUDO_DEV = '/devices/pseudo/zfs@0:';  
{ for dump and swap  }
  ZVOL_FULL_DEV_DIR = '/dev/zvol/dsk/';  
  ZVOL_FULL_RDEV_DIR = '/dev/zvol/rdsk/';  
  ZVOL_PROP_NAME = 'name';  
  ZVOL_DEFAULT_BLOCKSIZE = 8192;  
{
 * /dev/zfs ioctl numbers.
  }
type
    zfs_ioc_ = (ZFS_IOC_FIRST := byte('Z') shl 8,ZFS_IOC := ZFS_IOC_FIRST,ZFS_IOC_POOL_CREATE := ZFS_IOC_FIRST,
    ZFS_IOC_POOL_DESTROY,ZFS_IOC_POOL_IMPORT,
    ZFS_IOC_POOL_EXPORT,ZFS_IOC_POOL_CONFIGS,
    ZFS_IOC_POOL_STATS,ZFS_IOC_POOL_TRYIMPORT,
    ZFS_IOC_POOL_SCAN,ZFS_IOC_POOL_FREEZE,ZFS_IOC_POOL_UPGRADE,
    ZFS_IOC_POOL_GET_HISTORY,ZFS_IOC_VDEV_ADD,
    ZFS_IOC_VDEV_REMOVE,ZFS_IOC_VDEV_SET_STATE,
    ZFS_IOC_VDEV_ATTACH,ZFS_IOC_VDEV_DETACH,
    ZFS_IOC_VDEV_SETPATH,ZFS_IOC_VDEV_SETFRU,
    ZFS_IOC_OBJSET_STATS,ZFS_IOC_OBJSET_ZPLPROPS,
    ZFS_IOC_DATASET_LIST_NEXT,ZFS_IOC_SNAPSHOT_LIST_NEXT,
    ZFS_IOC_SET_PROP,ZFS_IOC_CREATE,ZFS_IOC_DESTROY,
    ZFS_IOC_ROLLBACK,ZFS_IOC_RENAME,ZFS_IOC_RECV,
    ZFS_IOC_SEND,ZFS_IOC_INJECT_FAULT,ZFS_IOC_CLEAR_FAULT,
    ZFS_IOC_INJECT_LIST_NEXT,ZFS_IOC_ERROR_LOG,
    ZFS_IOC_CLEAR,ZFS_IOC_PROMOTE,ZFS_IOC_SNAPSHOT,
    ZFS_IOC_DSOBJ_TO_DSNAME,ZFS_IOC_OBJ_TO_PATH,
    ZFS_IOC_POOL_SET_PROPS,ZFS_IOC_POOL_GET_PROPS,
    ZFS_IOC_SET_FSACL,ZFS_IOC_GET_FSACL,ZFS_IOC_SHARE,
    ZFS_IOC_INHERIT_PROP,ZFS_IOC_SMB_ACL,ZFS_IOC_USERSPACE_ONE,
    ZFS_IOC_USERSPACE_MANY,ZFS_IOC_USERSPACE_UPGRADE,
    ZFS_IOC_HOLD,ZFS_IOC_RELEASE,ZFS_IOC_GET_HOLDS,
    ZFS_IOC_OBJSET_RECVD_PROPS,ZFS_IOC_VDEV_SPLIT,
    ZFS_IOC_NEXT_OBJ,ZFS_IOC_DIFF,ZFS_IOC_TMP_SNAPSHOT,
    ZFS_IOC_OBJ_TO_STATS,ZFS_IOC_SPACE_WRITTEN,
    ZFS_IOC_SPACE_SNAPS,ZFS_IOC_DESTROY_SNAPS,
    ZFS_IOC_POOL_REGUID,ZFS_IOC_POOL_REOPEN,
    ZFS_IOC_SEND_PROGRESS,ZFS_IOC_LOG_HISTORY,
    ZFS_IOC_SEND_NEW,ZFS_IOC_SEND_SPACE,ZFS_IOC_CLONE,
    ZFS_IOC_LAST);
  zfs_ioc_t = zfs_ioc_;
{
 * Internal SPA load state.  Used by FMA diagnosis engine.
  }
{ no load in progress	 }
{ normal open		 }
{ import in progress	 }
{ tryimport in progress  }
{ recovery requested	 }
{ load failed		 }

  spa_load_state_t = (SPA_LOAD_NONE,SPA_LOAD_OPEN,SPA_LOAD_IMPORT,
    SPA_LOAD_TRYIMPORT,SPA_LOAD_RECOVER,SPA_LOAD_ERROR
    );
{
 * Bookmark name values.
  }

const
  ZPOOL_ERR_LIST = 'error list';  
  ZPOOL_ERR_DATASET = 'dataset';  
  ZPOOL_ERR_OBJECT = 'object';  
  HIS_MAX_RECORD_LEN = (MAXPATHLEN+MAXPATHLEN)+1;  
{
 * The following are names used in the nvlist describing
 * the pool's history log.
  }
  ZPOOL_HIST_RECORD = 'history record';  
  ZPOOL_HIST_TIME = 'history time';  
  ZPOOL_HIST_CMD = 'history command';  
  ZPOOL_HIST_WHO = 'history who';  
  ZPOOL_HIST_ZONE = 'history zone';  
  ZPOOL_HIST_HOST = 'history hostname';  
  ZPOOL_HIST_TXG = 'history txg';  
  ZPOOL_HIST_INT_EVENT = 'history internal event';  
  ZPOOL_HIST_INT_STR = 'history internal str';  
  ZPOOL_HIST_INT_NAME = 'internal_name';  
  ZPOOL_HIST_IOCTL = 'ioctl';  
  ZPOOL_HIST_INPUT_NVL = 'in_nvl';  
  ZPOOL_HIST_OUTPUT_NVL = 'out_nvl';  
  ZPOOL_HIST_DSNAME = 'dsname';  
  ZPOOL_HIST_DSID = 'dsid';  
{
 * Flags for ZFS_IOC_VDEV_SET_STATE
  }
  ZFS_ONLINE_CHECKREMOVE = $1;  
  ZFS_ONLINE_UNSPARE = $2;  
  ZFS_ONLINE_FORCEFAULT = $4;  
  ZFS_ONLINE_EXPAND = $8;  
  ZFS_OFFLINE_TEMPORARY = $1;  
{
 * Flags for ZFS_IOC_POOL_IMPORT
  }
  ZFS_IMPORT_NORMAL = $0;  
  ZFS_IMPORT_VERBATIM = $1;  
  ZFS_IMPORT_ANY_HOST = $2;  
  ZFS_IMPORT_MISSING_LOG = $4;  
  ZFS_IMPORT_ONLY = $8;  
{
 * Sysevent payload members.  ZFS will generate the following sysevents with the
 * given payloads:
 *
 *	ESC_ZFS_RESILVER_START
 *	ESC_ZFS_RESILVER_END
 *	ESC_ZFS_POOL_DESTROY
 *	ESC_ZFS_POOL_REGUID
 *
 *		ZFS_EV_POOL_NAME	DATA_TYPE_STRING
 *		ZFS_EV_POOL_GUID	DATA_TYPE_UINT64
 *
 *	ESC_ZFS_VDEV_REMOVE
 *	ESC_ZFS_VDEV_CLEAR
 *	ESC_ZFS_VDEV_CHECK
 *
 *		ZFS_EV_POOL_NAME	DATA_TYPE_STRING
 *		ZFS_EV_POOL_GUID	DATA_TYPE_UINT64
 *		ZFS_EV_VDEV_PATH	DATA_TYPE_STRING	(optional)
 *		ZFS_EV_VDEV_GUID	DATA_TYPE_UINT64
  }
  ZFS_EV_POOL_NAME = 'pool_name';  
  ZFS_EV_POOL_GUID = 'pool_guid';  
  ZFS_EV_VDEV_PATH = 'vdev_path';  
  ZFS_EV_VDEV_GUID = 'vdev_guid';  

implementation


end.
