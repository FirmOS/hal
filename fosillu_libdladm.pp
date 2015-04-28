unit fosillu_libdladm;

interface

uses ctypes,unixtype,fos_illumos_defs,fosillu_nvpair,fosillu_sysnet_common;

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
   *h for Java files.
  -D<prog> - Set full path to the "ppudump" program.
  -I<list> - Include the list of specified objects in the output. T Copyright (c) 2005, 2010, Oracle and/or its affiliates. All rights reserved.
   * Copyright (c) 2011, Joyent, Inc. All rights reserved.
   * Copyright (c) 2014, FirmOS All rights reserved.
    }
  {
   * This file includes structures, macros and common routines shared by all
   * data-link administration, and routines which do not directly administrate
   * links. For example, dladm_status2str().
    }

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}
{$LINKLIB libdladm}

const
  External_library=''; {Setup as you need}

  LINKID_STR_WIDTH   = 10;
  DLADM_STRSIZE      = 256;
  //ds_confid          = ds_u.dsu_confid;
  //ds_nvl             = ds_u.dsu_nvl;
  DLADM_INVALID_CONF = 0;


  {
   * option flags taken by the libdladm functions
   *
   *  - DLADM_OPT_ACTIVE:
   *    The function requests to bringup some configuration that only take
   *    effect on active system (not persistent).
   *
   *  - DLADM_OPT_PERSIST:
   *    The function requests to persist some configuration.
   *
   *  - DLADM_OPT_CREATE:
   *    Today, only used by dladm_set_secobj() - requests to create a secobj.
   *
   *  - DLADM_OPT_FORCE:
   *    The function requests to execute a specific operation forcefully.
   *
   *  - DLADM_OPT_PREFIX:
   *    The function requests to generate a link name using the specified prefix.
   *
   *  - DLADM_OPT_VLAN:
   *    Signifies VLAN creation code path
   *
   *  - DLADM_OPT_NOREFRESH:
   *    Do not refresh the daemon after setting parameter (used by STP mcheck).
   *
   *  - DLADM_OPT_BOOT:
   *    Bypass check functions during boot (used by pool property since pools
   *    can come up after link properties are set)
   *
   *  - DLADM_OPT_TRANSIENT:
   *    Indicates that the link assigned to a zone is transient and will be
   *    removed when the zone shuts down.
    }
    DLADM_OPT_ACTIVE = $00000001;    
    DLADM_OPT_PERSIST = $00000002;    
    DLADM_OPT_CREATE = $00000004;    
    DLADM_OPT_FORCE = $00000008;    
    DLADM_OPT_PREFIX = $00000010;    
    DLADM_OPT_ANCHOR = $00000020;    
    DLADM_OPT_VLAN = $00000040;    
    DLADM_OPT_NOREFRESH = $00000080;    
    DLADM_OPT_BOOT = $00000100;    
    DLADM_OPT_TRANSIENT = $00000200;    
    DLADM_WALK_TERMINATE = 0;    
    DLADM_WALK_CONTINUE = -(1);    
    DLADM_MAX_ARG_CNT = 32;    
    DLADM_MAX_ARG_VALS = 64;



  type
    dladm_status_t = (DLADM_STATUS_OK := 0,DLADM_STATUS_BADARG,
      DLADM_STATUS_FAILED,DLADM_STATUS_TOOSMALL,
      DLADM_STATUS_NOTSUP,DLADM_STATUS_NOTFOUND,
      DLADM_STATUS_BADVAL,DLADM_STATUS_NOMEM,
      DLADM_STATUS_EXIST,DLADM_STATUS_LINKINVAL,
      DLADM_STATUS_PROPRDONLY,DLADM_STATUS_BADVALCNT,
      DLADM_STATUS_DBNOTFOUND,DLADM_STATUS_DENIED,
      DLADM_STATUS_IOERR,DLADM_STATUS_TEMPONLY,
      DLADM_STATUS_TIMEDOUT,DLADM_STATUS_ISCONN,
      DLADM_STATUS_NOTCONN,DLADM_STATUS_REPOSITORYINVAL,
      DLADM_STATUS_MACADDRINVAL,DLADM_STATUS_KEYINVAL,
      DLADM_STATUS_INVALIDMACADDRLEN,DLADM_STATUS_INVALIDMACADDRTYPE,
      DLADM_STATUS_LINKBUSY,DLADM_STATUS_VIDINVAL,
      DLADM_STATUS_NONOTIF,DLADM_STATUS_TRYAGAIN,
      DLADM_STATUS_IPTUNTYPE,DLADM_STATUS_IPTUNTYPEREQD,
      DLADM_STATUS_BADIPTUNLADDR,DLADM_STATUS_BADIPTUNRADDR,
      DLADM_STATUS_ADDRINUSE,DLADM_STATUS_BADTIMEVAL,
      DLADM_STATUS_INVALIDMACADDR,DLADM_STATUS_INVALIDMACADDRNIC,
      DLADM_STATUS_INVALIDMACADDRINUSE,DLADM_STATUS_MACFACTORYSLOTINVALID,
      DLADM_STATUS_MACFACTORYSLOTUSED,DLADM_STATUS_MACFACTORYSLOTALLUSED,
      DLADM_STATUS_MACFACTORYNOTSUP,DLADM_STATUS_INVALIDMACPREFIX,
      DLADM_STATUS_INVALIDMACPREFIXLEN,DLADM_STATUS_BADCPUID,
      DLADM_STATUS_CPUERR,DLADM_STATUS_CPUNOTONLINE,
      DLADM_STATUS_BADRANGE,DLADM_STATUS_TOOMANYELEMENTS,
      DLADM_STATUS_DB_NOTFOUND,DLADM_STATUS_DB_PARSE_ERR,
      DLADM_STATUS_PROP_PARSE_ERR,DLADM_STATUS_ATTR_PARSE_ERR,
      DLADM_STATUS_FLOW_DB_ERR,DLADM_STATUS_FLOW_DB_OPEN_ERR,
      DLADM_STATUS_FLOW_DB_PARSE_ERR,DLADM_STATUS_FLOWPROP_DB_PARSE_ERR,
      DLADM_STATUS_FLOW_ADD_ERR,DLADM_STATUS_FLOW_WALK_ERR,
      DLADM_STATUS_FLOW_IDENTICAL,DLADM_STATUS_FLOW_INCOMPATIBLE,
      DLADM_STATUS_FLOW_EXISTS,DLADM_STATUS_PERSIST_FLOW_EXISTS,
      DLADM_STATUS_INVALID_IP,DLADM_STATUS_INVALID_PREFIXLEN,
      DLADM_STATUS_INVALID_PROTOCOL,DLADM_STATUS_INVALID_PORT,
      DLADM_STATUS_INVALID_DSF,DLADM_STATUS_INVALID_DSFMASK,
      DLADM_STATUS_INVALID_MACMARGIN,DLADM_STATUS_NOTDEFINED,
      DLADM_STATUS_BADPROP,DLADM_STATUS_MINMAXBW,
      DLADM_STATUS_NO_HWRINGS,DLADM_STATUS_PERMONLY,
      DLADM_STATUS_OPTMISSING,DLADM_STATUS_POOLCPU,
      DLADM_STATUS_INVALID_PORT_INSTANCE,DLADM_STATUS_PORT_IS_DOWN,
      DLADM_STATUS_PKEY_NOT_PRESENT,DLADM_STATUS_PARTITION_EXISTS,
      DLADM_STATUS_INVALID_PKEY,DLADM_STATUS_NO_IB_HW_RESOURCE,
      DLADM_STATUS_INVALID_PKEY_TBL_SIZE,DLADM_STATUS_PORT_NOPROTO,
      DLADM_STATUS_INVALID_MTU);

   var
    CDLADM_STATUS_CODES:array [dladm_status_t] of String;
  type
    dladm_datatype_t = (DLADM_TYPE_STR,DLADM_TYPE_BOOLEAN,DLADM_TYPE_UINT64);

    dladm_conf_t = record
        ds_readonly : boolean_t;
        ds_u : record
            case longint of
              0 : ( dsu_confid : longint );
              1 : ( dsu_nvl : ^nvlist_t );
            end;
      end;
    Pdladm_conf_t = ^dladm_conf_t;

  { opaque dladm handle to libdladm functions  }
  type
    dladm_handle = record
        {undefined structure}
      end;

    Pdladm_arg_list_t      = ^dladm_arg_list_t;
    PPdladm_arg_list_t     = ^Pdladm_arg_list_t;
    Pdladm_handle_t        = ^dladm_handle_t;
    Pdladm_usage_t         = ^dladm_usage_t;
    Pmac_priority_level_t  = ^mac_priority_level_t;
    Pmac_propval_range_t   = ^mac_propval_range_t;
    PPmac_propval_range_t  = ^Pmac_propval_range_t;
    Puint32_t  = ^uint32_t;
    Puint64_t  = ^uint64_t;
    Puint_t  = ^uint_t;


    dladm_handle_t = ^dladm_handle;

    dladm_arg_info = record
        ai_name : ^char;
        ai_val : array[0..(DLADM_MAX_ARG_VALS)-1] of ^char;
        ai_count : uint_t;
      end;
    dladm_arg_info_t = dladm_arg_info;

    dladm_arg_list = record
        al_info : array[0..(DLADM_MAX_ARG_CNT)-1] of dladm_arg_info_t;
        al_count : uint_t;
        al_buf : ^char;
      end;
    dladm_arg_list_t = dladm_arg_list;

    dladm_logtype_t = (DLADM_LOGTYPE_LINK := 1,DLADM_LOGTYPE_FLOW);

    dladm_usage = record
        du_name : array[0..(MAXLINKNAMELEN)-1] of char;
        du_duration : uint64_t;
        du_stime : uint64_t;
        du_etime : uint64_t;
        du_ipackets : uint64_t;
        du_rbytes : uint64_t;
        du_opackets : uint64_t;
        du_obytes : uint64_t;
        du_bandwidth : uint64_t;
        du_last : boolean_t;
      end;
    dladm_usage_t = dladm_usage;

  function dladm_open(_para1:Pdladm_handle_t):dladm_status_t;cdecl;external External_library name 'dladm_open';
  procedure dladm_close(_para1:dladm_handle_t);cdecl;external External_library name 'dladm_close';

  { * retrieve the dld file descriptor from handle, only libdladm and
    * dlmgmtd are given access to the door file descriptor.}
  function dladm_dld_fd(_para1:dladm_handle_t):longint;cdecl;external External_library name 'dladm_dld_fd';
  function dladm_status2str(_para1:dladm_status_t; _para2:Pchar):Pchar;cdecl;external External_library name 'dladm_status2str';
  function dladm_set_rootdir(_para1:Pchar):dladm_status_t;cdecl;external External_library name 'dladm_set_rootdir';
  function dladm_class2str(_para1:datalink_class_t; _para2:Pchar):Pchar;cdecl;external External_library name 'dladm_class2str';
  function dladm_media2str(_para1:uint32_t; _para2:Pchar):Pchar;cdecl;external External_library name 'dladm_media2str';
  function dladm_str2media(_para1:Pchar):uint32_t;cdecl;external External_library name 'dladm_str2media';
  function dladm_valid_linkname(_para1:Pchar):boolean_t;cdecl;external External_library name 'dladm_valid_linkname';
  function dladm_str2interval(_para1:Pchar; _para2:Puint32_t):boolean_t;cdecl;external External_library name 'dladm_str2interval';
  function dladm_str2bw(_para1:Pchar; _para2:Puint64_t):dladm_status_t;cdecl;external External_library name 'dladm_str2bw';

  function dladm_bw2str(_para1:int64_t; _para2:Pchar):Pchar;cdecl;external External_library name 'dladm_bw2str';
  function dladm_str2pri(_para1:Pchar; _para2:Pmac_priority_level_t):dladm_status_t;cdecl;external External_library name 'dladm_str2pri';

  function dladm_pri2str(_para1:mac_priority_level_t; _para2:Pchar):Pchar;cdecl;external External_library name 'dladm_pri2str';
  function dladm_str2protect(_para1:Pchar; _para2:Puint32_t):dladm_status_t;cdecl;external External_library name 'dladm_str2protect';

  function dladm_protect2str(_para1:uint32_t; _para2:Pchar):Pchar;cdecl;external External_library name 'dladm_protect2str';
  function dladm_str2ipv4addr(_para1:Pchar; _para2:pointer):dladm_status_t;cdecl;external External_library name 'dladm_str2ipv4addr';

  function dladm_ipv4addr2str(_para1:pointer; _para2:Pchar):Pchar;cdecl;external External_library name 'dladm_ipv4addr2str';
  function dladm_str2ipv6addr(_para1:Pchar; _para2:pointer):dladm_status_t;cdecl;external External_library name 'dladm_str2ipv6addr';

  function dladm_ipv6addr2str(_para1:pointer; _para2:Pchar):Pchar;cdecl;external External_library name 'dladm_ipv6addr2str';

  function dladm_parse_flow_props(_para1:Pchar; _para2:PPdladm_arg_list_t; _para3:boolean_t):dladm_status_t;cdecl;external External_library name 'dladm_parse_flow_props';
  function dladm_parse_link_props(_para1:Pchar; _para2:PPdladm_arg_list_t; novalues:boolean_t):dladm_status_t;cdecl;external External_library name 'dladm_parse_link_props';
  procedure dladm_free_props(_para1:Pdladm_arg_list_t);cdecl;external External_library name 'dladm_free_props';
  function dladm_parse_flow_attrs(_para1:Pchar; _para2:PPdladm_arg_list_t; _para3:boolean_t):dladm_status_t;cdecl;external External_library name 'dladm_parse_flow_attrs';
  procedure dladm_free_attrs(_para1:Pdladm_arg_list_t);cdecl;external External_library name 'dladm_free_attrs';
  function dladm_start_usagelog(_para1:dladm_handle_t; _para2:dladm_logtype_t; _para3:uint_t):dladm_status_t;cdecl;external External_library name 'dladm_start_usagelog';
  function dladm_stop_usagelog(_para1:dladm_handle_t; _para2:dladm_logtype_t):dladm_status_t;cdecl;external External_library name 'dladm_stop_usagelog';

  type
  dladm_walk_usage_cb = function (_para1:Pdladm_usage_t; _para2:pointer):longint;cdecl;

  function dladm_walk_usage_res(_para1:dladm_walk_usage_cb; _para2:longint; _para3:Pchar; _para4:Pchar; _para5:Pchar;_para6:Pchar; _para7:pointer):dladm_status_t;cdecl;external External_library name 'dladm_walk_usage_res';
  function dladm_walk_usage_time(_para1:dladm_walk_usage_cb; _para2:longint; _para3:Pchar; _para4:Pchar; _para5:Pchar;_para6:pointer):dladm_status_t;cdecl;external External_library name 'dladm_walk_usage_time';

  function dladm_usage_summary(_para1:dladm_walk_usage_cb; _para2:longint; _para3:Pchar; _para4:pointer):dladm_status_t;cdecl;external External_library name 'dladm_usage_summary';
  function dladm_usage_dates(_para1:dladm_walk_usage_cb; _para2:longint; _para3:Pchar; _para4:Pchar; _para5:pointer):dladm_status_t;cdecl;external External_library name 'dladm_usage_dates';
  function dladm_zone_boot(_para1:dladm_handle_t; _para2:zoneid_t):dladm_status_t;cdecl;external External_library name 'dladm_zone_boot';
  function dladm_zone_halt(_para1:dladm_handle_t; _para2:zoneid_t):dladm_status_t;cdecl;external External_library name 'dladm_zone_halt';
  function dladm_strs2range(_para1:PPchar; _para2:uint_t; _para3:mac_propval_type_t; _para4:PPmac_propval_range_t):dladm_status_t;cdecl;external External_library name 'dladm_strs2range';
  function dladm_range2list(_para1:Pmac_propval_range_t; _para2:pointer; _para3:Puint_t):dladm_status_t;cdecl;external External_library name 'dladm_range2list';
  function dladm_range2strs(_para1:Pmac_propval_range_t; _para2:PPchar):longint;cdecl;external External_library name 'dladm_range2strs';
  function dladm_list2range(_para1:pointer; _para2:uint_t; _para3:mac_propval_type_t; _para4:PPmac_propval_range_t):dladm_status_t;cdecl;external External_library name 'dladm_list2range';




  // libdlvnic.h
  type
    dladm_vnic_attr = record
        va_vnic_id : datalink_id_t;
        va_link_id : datalink_id_t;
        va_mac_addr_type : vnic_mac_addr_type_t;
        va_mac_len : uint_t;
        va_mac_addr : array[0..(MAXMACADDRLEN)-1] of uchar_t;
        va_mac_slot : longint;
        va_mac_prefix_len : uint_t;
        va_vid : uint16_t;
        va_force : boolean_t;
        va_vrid : vrid_t;
        va_af : longint;
        va_resource_props : mac_resource_props_t;
      end;
    dladm_vnic_attr_t = dladm_vnic_attr;
    Pdladm_vnic_attr_t  = ^dladm_vnic_attr_t;
    Pvnic_mac_addr_type_t  = ^vnic_mac_addr_type_t;


(* Const before type ignored *)

  function dladm_vnic_create(handle:dladm_handle_t; vnicname:Pchar; linkid:datalink_id_t; vnic_mac_add_type:vnic_mac_addr_type_t;
                             mac_addr:Puchar_t; mac_len:uint_t; mac_slot:Plongint; mac_prefix_len:uint_t; vid:uint16_t; vrid:vrid_t;
                             af:longint; vnic_id_out:Pdatalink_id_t; proplist:Pdladm_arg_list_t; flags:uint32_t):dladm_status_t;cdecl;external External_library name 'dladm_vnic_create';

  function dladm_vnic_delete(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:uint32_t):dladm_status_t;cdecl;external External_library name 'dladm_vnic_delete';

  function dladm_vnic_info(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:Pdladm_vnic_attr_t; _para4:uint32_t):dladm_status_t;cdecl;external External_library name 'dladm_vnic_info';

  function dladm_vnic_up(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:uint32_t):dladm_status_t;cdecl;external External_library name 'dladm_vnic_up';

(* Const before type ignored *)
  function dladm_vnic_str2macaddrtype(_para1:Pchar; _para2:Pvnic_mac_addr_type_t):dladm_status_t;cdecl;external External_library name 'dladm_vnic_str2macaddrtype';

{ C++ end of extern C conditionnal removed }
  { _LIBDLVNIC_H  }

  // libdlsim.c

  function dladm_simnet_create(_para1:dladm_handle_t; simnetname:PChar; media: uint_t; flags: uint32_t) : dladm_status_t; cdecl; external External_library name 'dladm_simnet_create';
  function dladm_simnet_delete(_para1:dladm_handle_t; _para2:datalink_id_t; flags: uint32_t) : dladm_status_t; cdecl; external External_Library name 'dladm_simnet_delete';

// dlpi.h

// DLPI media types supported
const
  DL_CSMACD = $0;
  DL_TPB = $1;
  DL_TPR = $2;
  DL_METRO = $3;
  DL_ETHER = $4;
  DL_HDLC = $05;
  DL_CHAR = $06;
  DL_CTCA = $07;
  DL_FDDI = $08;
  DL_FC = $10;
  DL_ATM = $11;
  DL_IPATM = $12;
  DL_X25 = $13;
  DL_ISDN = $14;
  DL_HIPPI = $15;
  DL_100VG = $16;
  DL_100VGTPR = $17;
  DL_ETH_CSMA = $18;
  DL_100BT = $19;
  DL_IB = $1a;
  DL_FRAME = $0a;
  DL_MPFRAME = $0b;
  DL_ASYNC = $0c;
  DL_IPX25 = $0d;
  DL_LOOP = $0e;
  DL_OTHER = $09;

  DL_IPV4 = $80000001;
  DL_IPV6 = $80000002;
  SUNW_DL_VNI = $80000003;
  DL_WIFI = $80000004;
  DL_IPNET = $80000005;
  SUNW_DL_IPMP = $80000006;
  DL_6TO4 = $80000007;




// >--- LIBDLLINK ---

type
  dladm_attr = record
      da_max_sdu : uint_t;
    end;
  dladm_attr_t = dladm_attr;
  Pdladm_attr_t = ^dladm_attr_t;
{
	 * Whether this physical link supports vanity naming (links with media
	 * types not supported by GLDv3 don't have vanity naming support).
	  }

  dladm_phys_attr = record
      dp_dev : array[0..(MAXLINKNAMELEN)-1] of char;
      dp_novanity : boolean_t;
    end;
  dladm_phys_attr_t  = dladm_phys_attr;
  Pdladm_phys_attr_t = ^dladm_phys_attr_t;
  dladm_prop_type_t = (DLADM_PROP_VAL_CURRENT := 1,DLADM_PROP_VAL_DEFAULT,
    DLADM_PROP_VAL_PERM,DLADM_PROP_VAL_MODIFIABLE,
    DLADM_PROP_VAL_PERSISTENT);
{
 * Maximum size of secobj value. Note that it should not be greater than
 * DLD_SECOBJ_VAL_MAX.
  }

const
  DLADM_SECOBJ_VAL_MAX = 256;
{
 * Maximum size of secobj name. Note that it should not be greater than
 * DLD_SECOBJ_NAME_MAX.
  }
  DLADM_SECOBJ_NAME_MAX = 32;
  DLADM_MAX_PROP_VALCNT = 32;
{
 * Size of prop_val buffer passed to pd_get function must be at
 * least DLADM_PROP_VAL_MAX
  }
  DLADM_PROP_VAL_MAX = 128;
  DLADM_SECOBJ_CLASS_WEP = 0;
  DLADM_SECOBJ_CLASS_WPA = 1;

type
  dladm_secobj_class_t  = longint;
  Pdladm_secobj_class_t = ^dladm_secobj_class_t;
(* Const before type ignored *)
{ possible flags for ma_flags below  }
  dladm_walk_cb_t  = function (p1 : PChar ; data:Pointer):cint;cdecl;
  Pdladm_walkcb_t = ^dladm_walk_cb_t;

const
  DLADM_MACADDR_USED = $1;

type
  dladm_hwgrp_type_t = (DLADM_HWGRP_TYPE_RX := $1,DLADM_HWGRP_TYPE_TX);

  dladm_hwgrp_attr = record
      hg_link_name : array[0..(MAXLINKNAMELEN)-1] of char;
      hg_grp_num : uint_t;
      hg_grp_type : dladm_hwgrp_type_t;
      hg_n_rings : uint_t;
      hg_rings : array[0..(MAX_RINGS_PER_GROUP)-1] of uint_t;
      hg_n_clnts : uint_t;
      hg_client_names : array[0..(MAXCLIENTNAMELEN)-1] of char;
    end;
  dladm_hwgrp_attr_t  = dladm_hwgrp_attr;
  Pdladm_hwgrp_attr_t = ^dladm_hwgrp_attr_t;

  dladm_macaddr_attr = record
      ma_slot : uint_t;
      ma_flags : uint_t;
      ma_addr : array[0..(MAXMACADDRLEN)-1] of uchar_t;
      ma_addrlen : uint_t;
      ma_client_name : array[0..(MAXNAMELEN)-1] of char;
      ma_client_linkid : datalink_id_t;
    end;
  dladm_macaddr_attr_t  = dladm_macaddr_attr;
  Pdladm_macaddr_attr_t = ^dladm_macaddr_attr_t;

  dladm_walk_linkprop_cb_t   = function (_para1:dladm_handle_t; _para2:datalink_id_t; _para3:Pchar; _para4:pointer):longint; cdecl;
  dladm_walk_secprop_cb_t    = function (_para1:dladm_handle_t; _para2:pointer; _para3:Pchar):boolean_t; cdecl;
  dladm_walk_datalinkid_cb_t = function (_para1:dladm_handle_t; _para2:datalink_id_t; _para3:pointer):longint; cdecl;
  dladm_walk_macaddr_cb_t    = function (_para1:pointer; _para2:Pdladm_macaddr_attr_t):boolean_t; cdecl;
  dladm_walk_hwgrp_cb_t      = function (_para1:pointer; _para2:Pdladm_hwgrp_attr_t):boolean_t;

function dladm_walk(_para1:Pdladm_walkcb_t; _para2:dladm_handle_t; _para3:pointer; _para4:datalink_class_t; _para5:datalink_media_t; _para6:uint32_t):dladm_status_t;cdecl;external External_library name 'dladm_walk';
function dladm_mac_walk(_para1:Pdladm_walkcb_t; _para2:pointer):dladm_status_t;cdecl;external External_library name 'dladm_mac_walk';
function dladm_info(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:Pdladm_attr_t):dladm_status_t;cdecl;external External_library name 'dladm_info';
function dladm_rename_link(_para1:dladm_handle_t; _para2:Pchar; _para3:Pchar; _para4:Pchar):dladm_status_t;cdecl;external External_library name 'dladm_rename_link';
function dladm_set_linkprop(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:Pchar; _para4:PPchar; _para5:uint_t;
           _para6:uint_t):dladm_status_t;cdecl;external External_library name 'dladm_set_linkprop';
function dladm_get_linkprop(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:dladm_prop_type_t; _para4:Pchar; _para5:PPchar;
           _para6:Puint_t):dladm_status_t;cdecl;external External_library name 'dladm_get_linkprop';
function dladm_get_linkprop_values(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:dladm_prop_type_t; _para4:Pchar; _para5:Puint_t;
           _para6:Puint_t):dladm_status_t;cdecl;external External_library name 'dladm_get_linkprop_values';
function dladm_walk_linkprop(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:pointer; _para4:dladm_walk_linkprop_cb_t):dladm_status_t;cdecl;external External_library name 'dladm_walk_linkprop';
function dladm_attr_is_linkprop(name:Pchar):boolean_t;cdecl;external External_library name 'dladm_attr_is_linkprop';
function dladm_linkprop_is_set(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:dladm_prop_type_t; _para4:Pchar; _para5:Pboolean_t):dladm_status_t;cdecl;external External_library name 'dladm_linkprop_is_set';
function dladm_set_secobj(_para1:dladm_handle_t; _para2:Pchar; _para3:dladm_secobj_class_t; _para4:Puint8_t; _para5:uint_t;
           _para6:uint_t):dladm_status_t;cdecl;external External_library name 'dladm_set_secobj';
function dladm_get_secobj(_para1:dladm_handle_t; _para2:Pchar; _para3:Pdladm_secobj_class_t; _para4:Puint8_t; _para5:Puint_t;
           _para6:uint_t):dladm_status_t;cdecl;external External_library name 'dladm_get_secobj';
function dladm_unset_secobj(_para1:dladm_handle_t; _para2:Pchar; _para3:uint_t):dladm_status_t;cdecl;external External_library name 'dladm_unset_secobj';
function dladm_walk_secobj(_para1:dladm_handle_t; _para2:pointer; _para3:dladm_walk_secprop_cb_t; _para4:uint_t):dladm_status_t;cdecl;external External_library name 'dladm_walk_secobj';
function dladm_linkstate2str(_para1:link_state_t; _para2:Pchar):Pchar;cdecl;external External_library name 'dladm_linkstate2str';

function dladm_linkduplex2str(_para1:link_duplex_t; _para2:Pchar):Pchar;cdecl;external External_library name 'dladm_linkduplex2str';
function dladm_secobjclass2str(_para1:dladm_secobj_class_t; _para2:Pchar):Pchar;cdecl;external External_library name 'dladm_secobjclass2str';
function dladm_str2secobjclass(_para1:Pchar; _para2:Pdladm_secobj_class_t):dladm_status_t;cdecl;external External_library name 'dladm_str2secobjclass';
function dladm_init_linkprop(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:boolean_t):dladm_status_t;cdecl;external External_library name 'dladm_init_linkprop';
function dladm_init_secobj(_para1:dladm_handle_t):dladm_status_t;cdecl;external External_library name 'dladm_init_secobj';

function dladm_valid_secobj_name(_para1:Pchar):boolean_t;cdecl;external External_library name 'dladm_valid_secobj_name';
function dladm_create_datalink_id(_para1:dladm_handle_t; _para2:Pchar; _para3:datalink_class_t; _para4:uint_t; _para5:uint32_t;
           _para6:Pdatalink_id_t):dladm_status_t;cdecl;external External_library name 'dladm_create_datalink_id';
function dladm_destroy_datalink_id(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:uint32_t):dladm_status_t;cdecl;external External_library name 'dladm_destroy_datalink_id';

function dladm_remap_datalink_id(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:Pchar):dladm_status_t;cdecl;external External_library name 'dladm_remap_datalink_id';
function dladm_up_datalink_id(_para1:dladm_handle_t; _para2:datalink_id_t):dladm_status_t;cdecl;external External_library name 'dladm_up_datalink_id';

function dladm_name2info(_para1:dladm_handle_t; _para2:Pchar; _para3:Pdatalink_id_t; _para4:Puint32_t; _para5:Pdatalink_class_t;
           _para6:Puint32_t):dladm_status_t;cdecl;external External_library name 'dladm_name2info';
function dladm_zname2info(_para1:dladm_handle_t; _para2:Pchar; _para3:Pchar; _para4:Pdatalink_id_t; _para5:Puint32_t;
           _para6:Pdatalink_class_t; _para7:Puint32_t):dladm_status_t;cdecl;external External_library name 'dladm_zname2info';
function dladm_datalink_id2info(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:Puint32_t; _para4:Pdatalink_class_t; _para5:Puint32_t;
           _para6:Pchar; _para7:size_t):dladm_status_t;cdecl;external External_library name 'dladm_datalink_id2info';
function dladm_walk_datalink_id(_para1:dladm_walk_datalinkid_cb_t; _para2:dladm_handle_t; _para3:pointer; _para4:datalink_class_t; _para5:datalink_media_t;
           _para6:uint32_t):dladm_status_t;cdecl;external External_library name 'dladm_walk_datalink_id';
function dladm_create_conf(_para1:dladm_handle_t; _para2:Pchar; _para3:datalink_id_t; _para4:datalink_class_t; _para5:uint32_t;
           _para6:Pdladm_conf_t):dladm_status_t;cdecl;external External_library name 'dladm_create_conf';
function dladm_open_conf(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:Pdladm_conf_t):dladm_status_t;cdecl;external External_library name 'dladm_open_conf';
function dladm_getsnap_conf(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:Pdladm_conf_t):dladm_status_t;cdecl;external External_library name 'dladm_getsnap_conf';
function dladm_write_conf(_para1:dladm_handle_t; _para2:dladm_conf_t):dladm_status_t;cdecl;external External_library name 'dladm_write_conf';

function dladm_remove_conf(_para1:dladm_handle_t; _para2:datalink_id_t):dladm_status_t;cdecl;external External_library name 'dladm_remove_conf';
procedure dladm_destroy_conf(_para1:dladm_handle_t; _para2:dladm_conf_t);cdecl;external External_library name 'dladm_destroy_conf';
function dladm_get_conf_field(_para1:dladm_handle_t; _para2:dladm_conf_t; _para3:Pchar; _para4:pointer; _para5:size_t):dladm_status_t;cdecl;external External_library name 'dladm_get_conf_field';

function dladm_getnext_conf_linkprop(_para1:dladm_handle_t; _para2:dladm_conf_t; _para3:Pchar; _para4:Pchar; _para5:pointer;
           _para6:size_t; _para7:Psize_t):dladm_status_t;cdecl;external External_library name 'dladm_getnext_conf_linkprop';

function dladm_set_conf_field(_para1:dladm_handle_t; _para2:dladm_conf_t; _para3:Pchar; _para4:dladm_datatype_t; _para5:pointer):dladm_status_t;cdecl;external External_library name 'dladm_set_conf_field';
function dladm_unset_conf_field(_para1:dladm_handle_t; _para2:dladm_conf_t; _para3:Pchar):dladm_status_t;cdecl;external External_library name 'dladm_unset_conf_field';
function dladm_dev2linkid(_para1:dladm_handle_t; _para2:Pchar; _para3:Pdatalink_id_t):dladm_status_t;cdecl;external External_library name 'dladm_dev2linkid';
function dladm_linkid2legacyname(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:Pchar; _para4:size_t):dladm_status_t;cdecl;external External_library name 'dladm_linkid2legacyname';
function dladm_phys_delete(_para1:dladm_handle_t; _para2:datalink_id_t):dladm_status_t;cdecl;external External_library name 'dladm_phys_delete';
function dladm_phys_info(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:dladm_phys_attr_t; _para4:uint32_t):dladm_status_t;cdecl;external External_library name 'dladm_phys_info';

function dladm_parselink(_para1:Pchar; _para2:Pchar; _para3:Puint_t):dladm_status_t;cdecl;external External_library name 'dladm_parselink';
function dladm_walk_macaddr(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:pointer; _para4:dladm_walk_macaddr_cb_t):longint;cdecl;external External_library name 'dladm_walk_macaddr';
function dladm_walk_hwgrp(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:pointer; _para4:dladm_walk_hwgrp_cb_t):longint;cdecl;external External_library name 'dladm_walk_hwgrp';

function dladm_link_get_proplist(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:PPdladm_arg_list_t):dladm_status_t;cdecl;external External_library name 'dladm_link_get_proplist';
function i_dladm_set_link_proplist_db(_para1:Pchar; _para2:Pdladm_arg_list_t):dladm_status_t;cdecl;external External_library name 'i_dladm_set_link_proplist_db';
// <---LIBDLLINK ---


//ether.h

const
  ETHERADDRL = 6;
{ ethernet FCS length in octets  }
  ETHERFCSL = 4;
{
 * Ethernet address - 6 octets
  }

type
  ether_addr_t = array[0..(ETHERADDRL)-1] of uchar_t;

// uid_stp.h

const
  STP_DBG = 1;
  NAME_LEN = 20;


type
  UID_PORT_ID = word;
  UID_STP_MODE_T = (STP_DISABLED,STP_ENABLED);

  UID_BRIDGE_ID_T = record
      prio : word;
      addr : array[0..5] of byte;
    end;
{ name of the VLAN, key of the bridge  }
{ 1-create, 0- delete  }

  UID_STP_BR_CTRL_T = record
      vlan_name : array[0..(NAME_LEN)-1] of char;
      action : char;
    end;

const
  BR_CFG_STATE = 1 shl 0;
  BR_CFG_PRIO = 1 shl 1;
  BR_CFG_AGE = 1 shl 2;
  BR_CFG_HELLO = 1 shl 3;
  BR_CFG_DELAY = 1 shl 4;
  BR_CFG_FORCE_VER = 1 shl 5;
  BR_CFG_AGE_MODE = 1 shl 6;
  BR_CFG_AGE_TIME = 1 shl 7;
  BR_CFG_HOLD_TIME = 1 shl 8;

type
  UID_STP_CFG_T = record
      field_mask : dword;
      stp_enabled : UID_STP_MODE_T;
      vlan_name : array[0..(NAME_LEN)-1] of char;
      bridge_priority : longint;
      max_age : longint;
      hello_time : longint;
      forward_delay : longint;
      force_version : longint;
      hold_time : longint;
    end;

  UID_STP_STATE_T = record
      vlan_name : array[0..(NAME_LEN)-1] of char;
      vlan_id : dword;
      stp_enabled : UID_STP_MODE_T;
      designated_root : UID_BRIDGE_ID_T;
      root_path_cost : dword;
      timeSince_Topo_Change : dword;
      Topo_Change_Count : dword;
      Topo_Change : byte;
      root_port : word;
      max_age : longint;
      hello_time : longint;
      forward_delay : longint;
      bridge_id : UID_BRIDGE_ID_T;
    end;

  RSTP_PORT_STATE = (UID_PORT_DISABLED := 0,UID_PORT_DISCARDING,
    UID_PORT_LEARNING,UID_PORT_FORWARDING,
    UID_PORT_NON_STP,UID_PORT_BADSDU);

type
  UID_STP_PORT_STATE_T = record
      vlan_name : array[0..(NAME_LEN)-1] of char;
      port_no : dword;
      port_id : UID_PORT_ID;
      state : RSTP_PORT_STATE;
      path_cost : dword;
      designated_root : UID_BRIDGE_ID_T;
      designated_cost : dword;
      designated_bridge : UID_BRIDGE_ID_T;
      designated_port : UID_PORT_ID;
      infoIs : longint;
      handshake_flags : word;
      rx_cfg_bpdu_cnt : dword;
      rx_rstp_bpdu_cnt : dword;
      rx_tcn_bpdu_cnt : dword;
      fdWhile : longint;
      helloWhen : longint;
      mdelayWhile : longint;
      rbWhile : longint;
      rcvdInfoWhile : longint;
      rrWhile : longint;
      tcWhile : longint;
      txCount : longint;
      lnkWhile : longint;
      uptime : dword;
      oper_port_path_cost : dword;
      role : byte;
      oper_point2point : byte;
      oper_edge : byte;
      oper_stp_neigb : byte;
      top_change_ack : byte;
      tc : byte;
    end;

//stp_in.h

const
  DEF_BR_PRIO = 32768;
  MIN_BR_PRIO = 0;
  MAX_BR_PRIO = 61440;
  DEF_BR_HELLOT = 2;
  MIN_BR_HELLOT = 1;
  MAX_BR_HELLOT = 10;
  DEF_BR_MAXAGE = 20;
  MIN_BR_MAXAGE = 6;
  MAX_BR_MAXAGE = 40;
  DEF_BR_FWDELAY = 15;
  MIN_BR_FWDELAY = 4;
  MAX_BR_FWDELAY = 30;
  IEEE_TIMER_SCALE = 256;
  DEF_FORCE_VERS = 2;

// libdlbridge.h

type
  dladm_bridge_prot_t = (DLADM_BRIDGE_PROT_UNKNOWN := 0,DLADM_BRIDGE_PROT_STP,
    DLADM_BRIDGE_PROT_TRILL);

Type
  Pboolean_t  = ^boolean_t;
  Pbridge_listfwd_t  = ^bridge_listfwd_t;
  Pchar  = ^char;
  Pdatalink_id_t  = ^datalink_id_t;
  Pdladm_bridge_prot_t  = ^dladm_bridge_prot_t;
  Plongint  = ^longint;
//  Ptrill_listnick_t  = ^trill_listnick_t;
  PUID_STP_CFG_T  = ^UID_STP_CFG_T;
  PUID_STP_PORT_STATE_T  = ^UID_STP_PORT_STATE_T;
  PUID_STP_STATE_T  = ^UID_STP_STATE_T;

  bridge_listfwd_s = record
      blf_name : array[0..(MAXNAMELEN)-1] of char;
      blf_dest : ether_addr_t;
      blf_trill_nick : uint16_t;
      blf_ms_age : uint_t;
      blf_is_local : boolean_t;
      blf_linkid : datalink_id_t;
    end;
  bridge_listfwd_t = bridge_listfwd_s;

{ Utility functions to accept bridge protection options  }
(* Const before type ignored *)

function dladm_bridge_str2prot(_para1:Pchar; _para2:Pdladm_bridge_prot_t):dladm_status_t;cdecl;external External_library name 'dladm_bridge_str2prot';

(* Const before type ignored *)
function dladm_bridge_prot2str(_para1:dladm_bridge_prot_t):Pchar;cdecl;external External_library name 'dladm_bridge_prot2str';

{ Retrieve bridge properties from SMF  }
(* Const before type ignored *)
function dladm_bridge_get_properties(_para1:Pchar; _para2:PUID_STP_CFG_T; _para3:Pdladm_bridge_prot_t):dladm_status_t;cdecl;external External_library name 'dladm_bridge_get_properties';

(* Const before type ignored *)
function dladm_bridge_run_properties(_para1:Pchar; _para2:PUID_STP_CFG_T; _para3:Pdladm_bridge_prot_t):dladm_status_t;cdecl;external External_library name 'dladm_bridge_run_properties';

{ Create new bridge and configure SMF properties  }
(* Const before type ignored *)
(* Const before type ignored *)
function dladm_bridge_configure(_para1:dladm_handle_t; _para2:Pchar; _para3:PUID_STP_CFG_T; _para4:dladm_bridge_prot_t; _para5:uint32_t):dladm_status_t;cdecl;external External_library name 'dladm_bridge_configure';

{ Enable a newly created bridge in SMF  }
(* Const before type ignored *)
function dladm_bridge_enable(_para1:Pchar):dladm_status_t;cdecl;external External_library name 'dladm_bridge_enable';

{ Delete a previously created bridge  }
(* Const before type ignored *)
function dladm_bridge_delete(_para1:dladm_handle_t; _para2:Pchar; _para3:uint32_t):dladm_status_t;cdecl;external External_library name 'dladm_bridge_delete';

{ Retrieve bridge state from running bridge daemon and get bridge port list  }
(* Const before type ignored *)
function dladm_bridge_state(_para1:Pchar; _para2:PUID_STP_STATE_T):dladm_status_t;cdecl;external External_library name 'dladm_bridge_state';

(* Const before type ignored *)
function dladm_bridge_get_portlist(_para1:Pchar; _para2:Puint_t):pdatalink_id_t;cdecl;external External_library name 'dladm_bridge_get_portlist';

procedure dladm_bridge_free_portlist(_para1:Pdatalink_id_t);cdecl;external External_library name 'dladm_bridge_free_portlist';

{ Set/remove bridge link membership and retreive bridge from member link  }
(* Const before type ignored *)
function dladm_bridge_setlink(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:Pchar):dladm_status_t;cdecl;external External_library name 'dladm_bridge_setlink';

function dladm_bridge_getlink(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:Pchar; _para4:size_t):dladm_status_t;cdecl;external External_library name 'dladm_bridge_getlink';

{ Retrieve bridge port status  }
function dladm_bridge_link_state(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:PUID_STP_PORT_STATE_T):dladm_status_t;cdecl;external External_library name 'dladm_bridge_link_state';

{ Check valid bridge name  }
(* Const before type ignored *)
function dladm_valid_bridgename(_para1:Pchar):boolean_t;cdecl;external External_library name 'dladm_valid_bridgename';

{ Convert bridge observability node name to bridge name  }
function dladm_observe_to_bridge(_para1:Pchar):boolean_t;cdecl;external External_library name 'dladm_observe_to_bridge';

{ Retrieve bridge forwarding table entries  }
(* Const before type ignored *)
function dladm_bridge_get_fwdtable(_para1:dladm_handle_t; _para2:Pchar; _para3:Puint_t):pbridge_listfwd_t;cdecl;external External_library name 'dladm_bridge_get_fwdtable';

procedure dladm_bridge_free_fwdtable(_para1:Pbridge_listfwd_t);cdecl;external External_library name 'dladm_bridge_free_fwdtable';

{ Retrive TRILL nicknames list  }
(* Const before type ignored *)
//function dladm_bridge_get_trillnick(_para1:Pchar; _para2:Puint_t):^trill_listnick_t;cdecl;external External_library name 'dladm_bridge_get_trillnick';

//procedure dladm_bridge_free_trillnick(_para1:Ptrill_listnick_t);cdecl;external External_library name 'dladm_bridge_free_trillnick';

{ Store and retrieve TRILL nickname from TRILL SMF service configuration   }
(* Const before type ignored *)
function dladm_bridge_get_nick(_para1:Pchar):uint16_t;cdecl;external External_library name 'dladm_bridge_get_nick';

(* Const before type ignored *)
procedure dladm_bridge_set_nick(_para1:Pchar; _para2:uint16_t);cdecl;external External_library name 'dladm_bridge_set_nick';

{ Retrieve undocumented private properties from bridge SMF service config  }
(* Const before type ignored *)
function dladm_bridge_get_privprop(_para1:Pchar; _para2:Pboolean_t; _para3:Puint32_t):dladm_status_t;cdecl;external External_library name 'dladm_bridge_get_privprop';

{ Internal to libdladm  }
function dladm_bridge_get_port_cfg(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:longint; _para4:Plongint):dladm_status_t;cdecl;external External_library name 'dladm_bridge_get_port_cfg';

function dladm_bridge_get_forwarding(_para1:dladm_handle_t; _para2:datalink_id_t; _para3:Puint_t):dladm_status_t;cdecl;external External_library name 'dladm_bridge_get_forwarding';

function dladm_bridge_refresh(_para1:dladm_handle_t; _para2:datalink_id_t):dladm_status_t;cdecl;external External_library name 'dladm_bridge_refresh';

{ Bridge connection; used only between libdladm and bridged for status  }
const
  DOOR_DIRNAME = '/var/run/bridge_door';

type
  bridge_door_type_e = (bdcBridgeGetConfig,bdcBridgeGetState,
    bdcBridgeGetPorts,bdcBridgeGetRefreshCount,
    bdcPortGetConfig,bdcPortGetState,bdcPortGetForwarding
    );
  bridge_door_type_t = bridge_door_type_e;

  bridge_door_cmd_s = record
      bdc_type : bridge_door_type_t;
      bdc_linkid : datalink_id_t;
    end;
  bridge_door_cmd_t = bridge_door_cmd_s;

  bridge_door_cfg_s = record
      bdcf_cfg : UID_STP_CFG_T;
      bdcf_prot : dladm_bridge_prot_t;
    end;
  bridge_door_cfg_t = bridge_door_cfg_s;

// end libdlbridge

implementation

procedure InitdladmStatusCodes;
var sc : dladm_status_t;
begin
  for sc in dladm_status_t do
    WriteStr(CDLADM_STATUS_CODES[sc],sc);
end;

initialization
  InitDladmStatusCodes;

end.
