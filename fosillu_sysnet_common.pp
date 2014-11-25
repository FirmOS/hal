unit fosillu_sysnet_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,ctypes,unixtype,fos_illumos_defs,Sockets;


{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{ ----------------------------------- }
{ socket.h from uts/common/sys/socket.h}
{ ----------------------------------- }

const
  External_library_illusock=''; {Setup as you need}

Type
  //Plongint    = ^longint;
  //Pmsghdr    = ^msghdr;
  //Psockaddr  = ^sockaddr;

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
* Copyright (c) 1989, 2010, Oracle and/or its affiliates. All rights reserved.
}
{	Copyright (c) 1983, 1984, 1985, 1986, 1987, 1988, 1989 AT&T	 }
{	  All Rights Reserved	 }
{
* University Copyright- Copyright (c) 1982, 1986, 1988
* The Regents of the University of California
* All Rights Reserved
*
* University Acknowledgment- Portions of this document are derived from
* software developed by the University of California, Berkeley, and its
* contributors.
}
{ Copyright (c) 2013, OmniTI Computer Consulting, Inc. All rights reserved.  }
{
* Copyright (c) 2014, Joyent, Inc. All rights reserved.
}
{
* The socklen definitions are reproduced in netinet/in.h for the inet6_
* functions.  Exposing all of sys/socket.h via netinet/in.h breaks existing
* applications and is not required by austin.
}

  socklen_t  = uint32_t;
  PSocklen_t = ^socklen_t;
{typedef	void		*_RESTRICT_KYWD Psocklen_t; }

{
* Definitions related to sockets: types, address families, options.
}
{ must agree with netconfig.h  }

const
NC_TPI_CLTS = 1;
{ must agree with netconfig.h  }
NC_TPI_COTS = 2;
{ must agree with netconfig.h  }
NC_TPI_COTS_ORD = 3;
{ must agree with netconfig.h  }
NC_TPI_RAW = 4;
{
* Types
}
{ stream socket  }

const
SOCK_STREAM = NC_TPI_COTS;
{ datagram socket  }
SOCK_DGRAM = NC_TPI_CLTS;
{ raw-protocol interface  }
SOCK_RAW = NC_TPI_RAW;

const
SOCK_RDM = 5;
{ sequenced packet stream  }
SOCK_SEQPACKET = 6;
{ type reside in these bits only  }
SOCK_TYPE_MASK = $ffff;
{
* Flags for socket() and accept4()
}
{ like open(2) O_CLOEXEC for socket  }
SOCK_CLOEXEC = $080000;
{ like O_NONBLOCK  }
SOCK_NONBLOCK = $100000;
{ like O_NDELAY  }
SOCK_NDELAY = $200000;
{
* Option flags per-socket.
}
{ turn on debugging info recording  }
SO_DEBUG = $0001;
{ socket has had listen()  }
SO_ACCEPTCONN = $0002;
{ allow local address reuse  }
SO_REUSEADDR = $0004;
{ keep connections alive  }
SO_KEEPALIVE = $0008;
{ just use interface addresses  }
SO_DONTROUTE = $0010;
{ permit sending of broadcast msgs  }
SO_BROADCAST = $0020;
{ bypass hardware when possible  }
SO_USELOOPBACK = $0040;
{ linger on close if data present  }
SO_LINGER = $0080;
{ leave received OOB data in line  }
SO_OOBINLINE = $0100;
{ Application wants delayed error  }
SO_DGRAM_ERRIND = $0200;
{ Application wants ucred of sender  }
SO_RECVUCRED = $0400;
{
* Socket options are passed using a signed integer, but it is also rare
* for more than one to ever be passed at the same time with setsockopt
* and only one at a time can be retrieved with getsockopt.
*
* Since the lower numbers cannot be renumbered for compatibility reasons,
* it would seem that we need to start a new number space (0x40000000 -
* 0x7fffffff) for those that don't need to be stored as a bit flag
* somewhere. This limits the flag options to 30 but that seems to be
* plenty, anyway. 0x40000000 is reserved for future use.
}
SO_ATTACH_FILTER = $40000001;
SO_DETACH_FILTER = $40000002;
{$ifdef _KERNEL}
{ Internal: use zero-copy  }
SO_SND_COPYAVOID = $0800;
{ Internal: get buffer info  }
SO_SND_BUFINFO = $1000;
{ when doing zero-copy  }
{ Write offset  }
{ Max size of a single mblk  }
{ Max total size of a mblk chain  }
{ Extra space available at the end  }

type
so_snd_bufinfo = record
    sbi_wroff : ushort_t;
    sbi_maxblk : ssize_t;
    sbi_maxpsz : ssize_t;
    sbi_tail : ushort_t;
  end;

{$endif}
{ _KERNEL  }
{
* N.B.: The following definition is present only for compatibility
* with release 3.0.  It will disappear in later releases.
}
{ ~SO_LINGER  }

const
SO_DONTLINGER =  not (SO_LINGER);
{
* Additional options, not kept in so_options.
}
{ send buffer size  }
SO_SNDBUF = $1001;
{ receive buffer size  }
SO_RCVBUF = $1002;
{ send low-water mark  }
SO_SNDLOWAT = $1003;
{ receive low-water mark  }
SO_RCVLOWAT = $1004;
{ send timeout  }
SO_SNDTIMEO = $1005;
{ receive timeout  }
SO_RCVTIMEO = $1006;
{ get error status and clear  }
SO_ERROR = $1007;
{ get socket type  }
SO_TYPE = $1008;
{ get/set protocol type  }
SO_PROTOTYPE = $1009;
{ create MLP on anonymous bind  }
SO_ANON_MLP = $100a;
{ allow dominated unlabeled peers  }
SO_MAC_EXEMPT = $100b;
{ get socket domain  }
SO_DOMAIN = $100c;
{ receive interval to push data  }
SO_RCVPSH = $100d;
{ "Socket"-level control message types:  }
{ access rights (array of int)  }
SCM_RIGHTS = $1010;
{ socket's security attributes  }
SO_SECATTR = $1011;
{ sender's ucred  }
SCM_UCRED = $1012;
{ socket-level timestamp option  }
SO_TIMESTAMP = $1013;
{ socket control message timestamp  }
SCM_TIMESTAMP = SO_TIMESTAMP;
{ bind in all zones  }
SO_ALLZONES = $1014;
{ exclusive binding  }
SO_EXCLBIND = $1015;
{ hide mac labels on wire  }
SO_MAC_IMPLICIT = $1016;
{ VRRP control socket  }
SO_VRRP = $1017;
{$ifdef _KERNEL}
{ Internal: AF_UNIX source address  }
SO_SRCADDR = $2001;
{ Internal: AF_UNIX file pointer  }
SO_FILEP = $2002;
{ Internal: AF_UNIX peer closed  }
SO_UNIX_CLOSE = $2003;
{$endif}
{ _KERNEL  }
{
* Socket filter options
}
{ attach filter  }

const
FIL_ATTACH = $1;
{ detach filter  }
FIL_DETACH = $2;
{ list attached filters  }
FIL_LIST = $3;
FILNAME_MAX = 32;
{
* Structure returned by FIL_LIST
}
{ see below (FILF_*)  }
{ position (0 is bottom)  }
{ filter name  }

type
fil_info = record
    fi_flags : longint;
    fi_pos : longint;
    fi_name : array[0..(FILNAME_MAX)-1] of char;
  end;

{ programmatic attach  }

const
FILF_PROG = $1;
{ automatic attach  }
FILF_AUTO = $2;
{ filter is not active  }
FILF_BYPASS = $4;
{$ifdef _KERNEL}
{
* new socket open flags to identify socket and acceptor streams
}
{ acceptor socket  }
SO_ACCEPTOR = $20000;
{ normal socket stream  }
SO_SOCKSTR = $40000;
{ fallback to TPI socket  }
SO_FALLBACK = $80000;
{
* Flags for socket_create() and socket_newconn()
}
SOCKET_SLEEP = KM_SLEEP;
SOCKET_NOSLEEP = KM_NOSLEEP;
{$endif}
{ _KERNEL  }
{
* Structure used for manipulating linger option.
}
{ option on/off  }
{ linger time  }

type
linger = record
    l_onoff : longint;
    l_linger : longint;
  end;

{
* Levels for (get/set)sockopt() that don't apply to a specific protocol.
}
{ options for socket level  }

const
  SOL_SOCKET = $ffff;
  SOL_ROUTE = $fffe;
{ options for packet level  }

  SOL_PACKET = $fffd;
{ options for socket filter level  }
  SOL_FILTER = $fffc;
{
* Address families.
*
* Some of these constant names are copied for the DTrace IP provider in
* usr/src/lib/libdtrace/common/ip.d.in, ip.sed.in, which should be kept
* in sync.
}
{ unspecified  }
AF_UNSPEC = 0;
{ local to host (pipes, portals)  }
AF_UNIX = 1;
{ Synonym for AF_UNIX  }
AF_LOCAL = AF_UNIX;
{ Synonym for AF_UNIX  }
AF_FILE = AF_UNIX;
{ internetwork: UDP, TCP, etc.  }
AF_INET = 2;
{ arpanet imp addresses  }
AF_IMPLINK = 3;
{ pup protocols: e.g. BSP  }
AF_PUP = 4;
{ mit CHAOS protocols  }
AF_CHAOS = 5;
{ XEROX NS protocols  }
AF_NS = 6;
{ nbs protocols  }
AF_NBS = 7;
{ european computer manufacturers  }
AF_ECMA = 8;
{ datakit protocols  }
AF_DATAKIT = 9;
{ CCITT protocols, X.25 etc  }
AF_CCITT = 10;
{ IBM SNA  }
AF_SNA = 11;
{ DECnet  }
AF_DECnet = 12;
{ Direct data link interface  }
AF_DLI = 13;
{ LAT  }
AF_LAT = 14;
{ NSC Hyperchannel  }
AF_HYLINK = 15;
{ Apple Talk  }
AF_APPLETALK = 16;
{ Network Interface Tap  }
AF_NIT = 17;
{ IEEE 802.2, also ISO 8802  }
AF_802 = 18;
{ umbrella for all families used  }
AF_OSI = 19;
{ CCITT X.25 in particular  }
AF_X25 = 20;
{ AFI = 47, IDI = 4  }
AF_OSINET = 21;
{ U.S. Government OSI  }
AF_GOSIP = 22;
{ Novell Internet Protocol  }
AF_IPX = 23;
{ Internal Routing Protocol  }
AF_ROUTE = 24;
{ Link-layer interface  }
AF_LINK = 25;
{ Internet Protocol, Version 6  }
AF_INET6 = 26;
{ Security Association DB socket  }
AF_KEY = 27;
{ NCA socket  }
AF_NCA = 28;
{ Security Policy DB socket  }
AF_POLICY = 29;
{ Sun private; do not use  }
AF_INET_OFFLOAD = 30;
{ TRILL interface  }
AF_TRILL = 31;
{ PF_PACKET Linux socket interface  }
AF_PACKET = 32;
{ Linux-compatible netlink  }
AF_LX_NETLINK = 33;
AF_MAX = 33;
{
* Protocol families, same as address families for now.
}
PF_UNSPEC = AF_UNSPEC;
PF_UNIX = AF_UNIX;
PF_LOCAL = PF_UNIX;
PF_FILE = PF_UNIX;
PF_INET = AF_INET;
PF_IMPLINK = AF_IMPLINK;
PF_PUP = AF_PUP;
PF_CHAOS = AF_CHAOS;
PF_NS = AF_NS;
PF_NBS = AF_NBS;
PF_ECMA = AF_ECMA;
PF_DATAKIT = AF_DATAKIT;
PF_CCITT = AF_CCITT;
PF_SNA = AF_SNA;
PF_DECnet = AF_DECnet;
PF_DLI = AF_DLI;
PF_LAT = AF_LAT;
PF_HYLINK = AF_HYLINK;
PF_APPLETALK = AF_APPLETALK;
PF_NIT = AF_NIT;
PF_802 = AF_802;
PF_OSI = AF_OSI;
PF_X25 = AF_X25;
PF_OSINET = AF_OSINET;
PF_GOSIP = AF_GOSIP;
PF_IPX = AF_IPX;
PF_ROUTE = AF_ROUTE;
PF_LINK = AF_LINK;
PF_INET6 = AF_INET6;
PF_KEY = AF_KEY;
PF_NCA = AF_NCA;
PF_POLICY = AF_POLICY;
{ Sun private; do not use  }
PF_INET_OFFLOAD = AF_INET_OFFLOAD;
PF_TRILL = AF_TRILL;
PF_PACKET = AF_PACKET;
PF_LX_NETLINK = AF_LX_NETLINK;
PF_MAX = AF_MAX;
{
* Maximum queue length specifiable by listen.
}
SOMAXCONN = 128;
{
* Message header for recvmsg and sendmsg calls.
}
{ optional address  }
{ size of address  }
{ scatter/gather array  }
{ # elements in msg_iov  }

//type
//msghdr = record
//    msg_name : pointer;
//    msg_namelen : socklen_t;
//    msg_iov : ^iovec;
//    msg_iovlen : longint;
//    msg_control : pointer;
//    msg_controllen : socklen_t;
//    msg_flags : longint;
//    msg_accrights : caddr_t;
//    msg_accrightslen : longint;
//  end;


const
MSG_OOB = $1;
{ peek at incoming message  }
MSG_PEEK = $2;
{ send without using routing tables  }
MSG_DONTROUTE = $4;
{ Added for XPGv2 compliance  }
{ Terminates a record  }
MSG_EOR = $8;
{ Control data truncated  }
MSG_CTRUNC = $10;
{ Normal data truncated  }
MSG_TRUNC = $20;
{ Wait for complete recv or error  }
MSG_WAITALL = $40;
{ Save control message for use with  }
MSG_DUPCTRL = $800;
{ with left over data  }
{ End of XPGv2 compliance  }
{ Don't block for this recv  }
MSG_DONTWAIT = $80;
{ Notification, not data  }
MSG_NOTIFICATION = $100;
{ Private: XPG4.2 flag  }
MSG_XPG4_2 = $8000;
MSG_MAXIOVLEN = 16;

//function accept(_para1:longint; _RESTRICT_KYWD:Psockaddr; _para3:Psocklen_t):longint;cdecl;external External_library_illusock name 'accept';
//function accept4(_para1:longint; _RESTRICT_KYWD:Psockaddr; _para3:Psocklen_t; _para4:longint):longint;cdecl;external External_library_illusock name 'accept4';
//
//(* Const before type ignored *)
//function bind(_para1:longint; _para2:Psockaddr; _para3:socklen_t):longint;cdecl;external External_library_illusock name 'bind';
//
//(* Const before type ignored *)
//function connect(_para1:longint; _para2:Psockaddr; _para3:socklen_t):longint;cdecl;external External_library_illusock name 'connect';
//
//function getpeername(_para1:longint; _RESTRICT_KYWD:Psockaddr; _para3:Psocklen_t):longint;cdecl;external External_library_illusock name 'getpeername';
//
//function getsockname(_para1:longint; _RESTRICT_KYWD:Psockaddr; _para3:Psocklen_t):longint;cdecl;external External_library_illusock name 'getsockname';
//
//function getsockopt(_para1:longint; _para2:longint; _para3:longint; _RESTRICT_KYWD:pointer; _para5:Psocklen_t):longint;cdecl;external External_library_illusock name 'getsockopt';
//
//function listen(_para1:longint; _para2:longint):longint;cdecl;external External_library_illusock name 'listen';
//
//{ XXX - fixme???  where do I go  }
//function socketpair(_para1:longint; _para2:longint; _para3:longint; _para4:Plongint):longint;cdecl;external External_library_illusock name 'socketpair';
//
//function recv(_para1:longint; _para2:pointer; _para3:size_t; _para4:longint):ssize_t;cdecl;external External_library_illusock name 'recv';
//
//function recvfrom(_para1:longint; _RESTRICT_KYWD:pointer; _para3:size_t; _para4:longint; _RESTRICT_KYWD:Psockaddr;
//           _para6:Psocklen_t):ssize_t;cdecl;external External_library_illusock name 'recvfrom';
//
//function recvmsg(_para1:longint; _para2:Pmsghdr; _para3:longint):ssize_t;cdecl;external External_library_illusock name 'recvmsg';
//
//(* Const before type ignored *)
//function send(_para1:longint; _para2:pointer; _para3:size_t; _para4:longint):ssize_t;cdecl;external External_library_illusock name 'send';
//
//(* Const before type ignored *)
//function sendmsg(_para1:longint; _para2:Pmsghdr; _para3:longint):ssize_t;cdecl;external External_library_illusock name 'sendmsg';
//
//(* Const before type ignored *)
//(* Const before type ignored *)
//function sendto(_para1:longint; _para2:pointer; _para3:size_t; _para4:longint; _para5:Psockaddr;
//           _para6:socklen_t):ssize_t;cdecl;external External_library_illusock name 'sendto';
//
//(* Const before type ignored *)
//function setsockopt(_para1:longint; _para2:longint; _para3:longint; _para4:pointer; _para5:socklen_t):longint;cdecl;external External_library_illusock name 'setsockopt';
//
//function shutdown(_para1:longint; _para2:longint):longint;cdecl;external External_library_illusock name 'shutdown';
//
//function socket(_para1:longint; _para2:longint; _para3:longint):longint;cdecl;external External_library_illusock name 'socket';
//
//{$if !defined(_XPG4_2) || defined(_XPG6) || defined(__EXTENSIONS__)}
//
//function sockatmark(_para1:longint):longint;cdecl;external External_library_illusock name 'sockatmark';

{ __STDC__  }

function accept:longint;cdecl;external External_library_illusock name 'accept';

function accept4:longint;cdecl;external External_library_illusock name 'accept4';

function bind:longint;cdecl;external External_library_illusock name 'bind';

function connect:longint;cdecl;external External_library_illusock name 'connect';

function getpeername:longint;cdecl;external External_library_illusock name 'getpeername';

function getsockname:longint;cdecl;external External_library_illusock name 'getsockname';

function getsockopt:longint;cdecl;external External_library_illusock name 'getsockopt';

function listen:longint;cdecl;external External_library_illusock name 'listen';

function recv:longint;cdecl;external External_library_illusock name 'recv';

function recvfrom:longint;cdecl;external External_library_illusock name 'recvfrom';

function send:longint;cdecl;external External_library_illusock name 'send';

function sendto:longint;cdecl;external External_library_illusock name 'sendto';

function setsockopt:longint;cdecl;external External_library_illusock name 'setsockopt';

function sockatmark:longint;cdecl;external External_library_illusock name 'sockatmark';

function socket:longint;cdecl;external External_library_illusock name 'socket';

function recvmsg:longint;cdecl;external External_library_illusock name 'recvmsg';

function sendmsg:longint;cdecl;external External_library_illusock name 'sendmsg';

function shutdown:longint;cdecl;external External_library_illusock name 'shutdown';

function socketpair:longint;cdecl;external External_library_illusock name 'socketpair';


//implementation
//
//{ was #define dname def_expr }
//function _CMSG_DATA_ALIGNMENT : longint; { return type might be wrong }
//begin
//  _CMSG_DATA_ALIGNMENT:=sizeof(longint);
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }
//function _CMSG_HDR_ALIGN(x : longint) : longint;
//begin
//_CMSG_HDR_ALIGN:=((uintptr_t(x(+(_CMSG_HDR_ALIGNMENT))))-1) and ( not (_CMSG_HDR_ALIGNMENT-1));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }
//function _CMSG_DATA_ALIGN(x : longint) : longint;
//begin
//_CMSG_DATA_ALIGN:=((uintptr_t(x(+(_CMSG_DATA_ALIGNMENT))))-1) and ( not (_CMSG_DATA_ALIGNMENT-1));
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//function CMSG_DATA(c : longint) : pbyte;
//begin
//CMSG_DATA:=pbyte(_CMSG_DATA_ALIGN(pcmsghdr(c(+(1)))));
//end;

{ ----------------------------------- }
{ socket.h from uts/common/sys/socket.h}
{ ----------------------------------- }


{ ----------------------------------- }
{ vnic.h from uts/common/sys/vnic.h}
{ ----------------------------------- }
type
  vnic_ioc_diag_t = (VNIC_IOC_DIAG_NONE,VNIC_IOC_DIAG_MACADDR_NIC,
    VNIC_IOC_DIAG_MACADDR_INUSE,VNIC_IOC_DIAG_MACADDR_INVALID,
    VNIC_IOC_DIAG_MACADDRLEN_INVALID,VNIC_IOC_DIAG_MACFACTORYSLOTINVALID,
    VNIC_IOC_DIAG_MACFACTORYSLOTUSED,VNIC_IOC_DIAG_MACFACTORYSLOTALLUSED,
    VNIC_IOC_DIAG_MACFACTORYNOTSUP,VNIC_IOC_DIAG_MACPREFIX_INVALID,
    VNIC_IOC_DIAG_MACPREFIXLEN_INVALID,VNIC_IOC_DIAG_MACMARGIN_INVALID,
    VNIC_IOC_DIAG_NO_HWRINGS,VNIC_IOC_DIAG_MACMTU_INVALID
    );
{
 * Allowed VNIC MAC address types.
 *
 * - VNIC_MAC_ADDR_TYPE_FIXED, VNIC_MAC_ADDR_TYPE_RANDOM:
 *   The MAC address is specified by value by the caller, which
 *   itself can obtain it from the user directly,
 *   or pick it in a random fashion. Which method is used by the
 *   caller is irrelevant to the VNIC driver. However two different
 *   types are provided so that the information can be made available
 *   back to user-space when listing the kernel defined VNICs.
 *
 *   When a VNIC is created, the address in passed through the
 *   vc_mac_addr and vc_mac_len fields of the vnic_ioc_create_t
 *   structure.
 *
 * - VNIC_MAC_ADDR_TYPE_FACTORY: the MAC address is obtained from
 *   one of the MAC factory MAC addresses of the underyling NIC.
 *
 * - VNIC_MAC_ADDR_TYPE_AUTO: the VNIC driver attempts to
 *   obtain the address from one of the factory MAC addresses of
 *   the underlying NIC. If none is available, the specified
 *   MAC address value is used.
 *
 * - VNIC_MAC_ADDR_TYPE_PRIMARY: this is a VNIC based VLAN. The
 *   address for this is the address of the primary MAC client.
 *
  }

  vnic_mac_addr_type_t = (VNIC_MAC_ADDR_TYPE_UNKNOWN := -(1),VNIC_MAC_ADDR_TYPE_FIXED,
    VNIC_MAC_ADDR_TYPE_RANDOM,VNIC_MAC_ADDR_TYPE_FACTORY,
    VNIC_MAC_ADDR_TYPE_AUTO,VNIC_MAC_ADDR_TYPE_PRIMARY,
    VNIC_MAC_ADDR_TYPE_VRID);

{ was #define dname def_expr }
//function VNIC_IOC_CREATE : longint; { return type might be wrong }

const

  MPT_MACNOSPOOF = $00000001;
  MPT_RESTRICTED = $00000002;
  MPT_IPNOSPOOF = $00000004;
  MPT_DHCPNOSPOOF = $00000008;
  MPT_ALL = $0000000f;
  MPT_RESET = $ffffffff;
  MPT_MAXCNT = 32;
  MPT_MAXIPADDR = MPT_MAXCNT;
  MPT_MAXCID = MPT_MAXCNT;
  MPT_MAXCIDLEN = 256;


  VNIC_IOC_CREATE_NODUPCHECK = $00000001;
  VNIC_IOC_CREATE_ANCHOR = $00000002;
{
 * Force creation of VLAN based VNIC without checking if the
 * undelying MAC supports the margin size.
  }
  VNIC_IOC_CREATE_FORCE = $00000004;
  MRP_NCPUS = 128;
{
 * In MCM_CPUS mode, cpu bindings is user specified. In MCM_FANOUT mode,
 * user only specifies a fanout count.
 * mc_rx_fanout_cnt gives the number of CPUs used for fanout soft rings.
 * mc_rx_fanout_cpus[] array stores the CPUs used for fanout soft rings.
  }

type
  mac_cpu_mode_t = (MCM_FANOUT := 1,MCM_CPUS);
{
 * Structure to store the value of the CPUs to be used to re-target
 * Tx interrupt.
  }
{ cpu value to re-target intr to  }
{ re-targeted CPU or -1 if failed  }

  mac_tx_intr_cpus_s = record
      mtc_intr_cpu : array[0..(MRP_NCPUS)-1] of int32_t;
      mtc_retargeted_cpu : array[0..(MRP_NCPUS)-1] of int32_t;
    end;
  mac_tx_intr_cpu_t = mac_tx_intr_cpus_s;
{ num of cpus  }
{ cpu list  }
{ soft ring cpu cnt  }
{ SR cpu list  }
{ poll thr binding  }
{ worker thr binding  }
{
	 * interrupt cpu: mrp_intr_cpu less than 0 implies platform limitation
	 * in retargetting the interrupt assignment.
	  }
{ fanout mode  }

  mac_cpus_props_s = record
      mc_ncpus : uint32_t;
      mc_cpus : array[0..(MRP_NCPUS)-1] of uint32_t;
      mc_rx_fanout_cnt : uint32_t;
      mc_rx_fanout_cpus : array[0..(MRP_NCPUS)-1] of uint32_t;
      mc_rx_pollid : uint32_t;
      mc_rx_workerid : uint32_t;
      mc_rx_intr_cpu : int32_t;
      mc_tx_fanout_cpus : array[0..(MRP_NCPUS)-1] of int32_t;
      mc_tx_intr_cpus : mac_tx_intr_cpu_t;
      mc_fanout_mode : mac_cpu_mode_t;
    end;
  mac_cpus_t = mac_cpus_props_s;

  {
   * Note: Static initalizers of "union" type assume
   * the constant on the RHS is the type of the first member
   * of union.
   * To make static initializers (and efficient usage) work,
   * the order of members exposed to user and kernel view of
   * this data structure is different.
   * User environment sees specified uint8_t type as first
   * member whereas kernel sees most efficient type as
   * first member.
  }
  in6_addr = record
      _S6_un : record
          case longint of
            {$ifdef _KERNEL}
            0 : ( _S6_u32 : array[0..3] of uint32_t );
            1 : ( _S6_u8 : array[0..15] of uint8_t );
            {$else}
            2 : ( _S6_u8 : array[0..15] of uint8_t );
            3 : ( _S6_u32 : array[0..3] of uint32_t );
            {$endif}
            4 : ( __S6_align : uint32_t );
          end;
    end;

    in6_addr_t = in6_addr;
  {$ifndef _SA_FAMILY_T}
  {$define _SA_FAMILY_T}
    sa_family_t = uint16_t;
  {$endif}

  mac_ipaddr_s = record
      ip_version : uint32_t;
      ip_addr    : in6_addr_t;
      ip_netmask : uint8_t;
    end;
  mac_ipaddr_t = mac_ipaddr_s;

  mac_dhcpcid_form_t = (CIDFORM_TYPED := 1,CIDFORM_HEX,CIDFORM_STR);

  mac_dhcpcid_s = record
      dc_id : array[0..(MPT_MAXCIDLEN)-1] of uchar_t;
      dc_len : uint32_t;
      dc_form : mac_dhcpcid_form_t;
    end;
  mac_dhcpcid_t = mac_dhcpcid_s;

  mac_protect_s = record
      mp_types : uint32_t;
      mp_ipaddrcnt : uint32_t;
      mp_ipaddrs : array[0..(MPT_MAXIPADDR)-1] of mac_ipaddr_t;
      mp_cidcnt : uint32_t;
      mp_cids : array[0..(MPT_MAXCID)-1] of mac_dhcpcid_t;
    end;
  mac_protect_t = mac_protect_s;
  { The default priority for links  }


  { Priority values  }
  mac_priority_level_t = (MPL_LOW,MPL_MEDIUM,MPL_HIGH,MPL_RESET);
  mac_resource_props_s = record
      mrp_mask : uint32_t;
      mrp_maxbw : uint64_t;
      mrp_priority : mac_priority_level_t;
      mrp_cpus : mac_cpus_t;
      mrp_protect : mac_protect_t;
      mrp_nrxrings : uint32_t;
      mrp_ntxrings : uint32_t;
      mrp_pool : array[0..(MAXPATHLEN)-1] of char;
    end;
  mac_resource_props_t = mac_resource_props_s;


  vnic_ioc_create = record
      vc_vnic_id : datalink_id_t;
      vc_link_id : datalink_id_t;
      vc_mac_addr_type : vnic_mac_addr_type_t;
      vc_mac_len : uint_t;
      vc_mac_addr : array[0..(MAXMACADDRLEN)-1] of uchar_t;
      vc_mac_prefix_len : uint_t;
      vc_mac_slot : longint;
      vc_vid : uint16_t;
      vc_vrid : vrid_t;
      vc_af : longint;
      vc_status : uint_t;
      vc_flags : uint_t;
      vc_diag : vnic_ioc_diag_t;
      vc_resource_props : mac_resource_props_t;
    end;
  vnic_ioc_create_t = vnic_ioc_create;

{ was #define dname def_expr }
//function VNIC_IOC_DELETE : longint; { return type might be wrong }


type
  vnic_ioc_delete = record
      vd_vnic_id : datalink_id_t;
    end;
  vnic_ioc_delete_t = vnic_ioc_delete;

{ was #define dname def_expr }
//function VNIC_IOC_INFO : longint; { return type might be wrong }


type
  vnic_info = record
      vn_vnic_id : datalink_id_t;
      vn_link_id : datalink_id_t;
      vn_mac_addr_type : vnic_mac_addr_type_t;
      vn_mac_len : uint_t;
      vn_mac_addr : array[0..(MAXMACADDRLEN)-1] of uchar_t;
      vn_mac_slot : uint_t;
      vn_mac_prefix_len : uint32_t;
      vn_vid : uint16_t;
      vn_vrid : vrid_t;
      vn_af : longint;
      vn_force : boolean_t;
      vn_resource_props : mac_resource_props_t;
    end;
  vnic_info_t = vnic_info;

  vnic_ioc_info = record
      vi_info : vnic_info_t;
    end;
  vnic_ioc_info_t = vnic_ioc_info;

{ was #define dname def_expr }
//function VNIC_IOC_MODIFY : longint; { return type might be wrong }

const
  VNIC_IOC_MODIFY_ADDR = $01;
  VNIC_IOC_MODIFY_RESOURCE_CTL = $02;

type
  vnic_ioc_modify = record
      vm_vnic_id : datalink_id_t;
      vm_modify_mask : uint_t;
      vm_mac_len : uint_t;
      vm_mac_slot : longint;
      vm_mac_addr : array[0..(MAXMACADDRLEN)-1] of uchar_t;
      vm_mac_addr_type : vnic_mac_addr_type_t;
      vm_resource_props : mac_resource_props_t;
      vm_diag : vnic_ioc_diag_t;
    end;
  vnic_ioc_modify_t = vnic_ioc_modify;
{ _SYS_VNIC_H  }

//implementation
//
//{ was #define dname def_expr }
//function VNIC_IOC_CREATE : longint; { return type might be wrong }
//  begin
//    VNIC_IOC_CREATE:=VNICIOC(1);
//  end;
//
//{ was #define dname def_expr }
//function VNIC_IOC_DELETE : longint; { return type might be wrong }
//  begin
//    VNIC_IOC_DELETE:=VNICIOC(2);
//  end;
//
//{ was #define dname def_expr }
//function VNIC_IOC_INFO : longint; { return type might be wrong }
//  begin
//    VNIC_IOC_INFO:=VNICIOC(3);
//  end;
//
//{ was #define dname def_expr }
//function VNIC_IOC_MODIFY : longint; { return type might be wrong }
//  begin
//    VNIC_IOC_MODIFY:=VNICIOC(4);
//  end;


{ END ----------------------------------- }
{ END vnic.h from uts/common/sys/vnic.h}
{ END ----------------------------------- }


{ ----------------------------------- }
{ dls_mgmt.h from uts/common/sys/mac.h}
{ ----------------------------------- }

{
   * Data-Link Services Module
}
{ C++ extern C conditionnal removed }

    datalink_class_t = (DATALINK_CLASS_PHYS := $01,DATALINK_CLASS_VLAN := $02,
      DATALINK_CLASS_AGGR := $04,DATALINK_CLASS_VNIC := $08,
      DATALINK_CLASS_ETHERSTUB := $10,DATALINK_CLASS_SIMNET := $20,
      DATALINK_CLASS_BRIDGE := $40,DATALINK_CLASS_IPTUN := $80,
      DATALINK_CLASS_PART := $100);
    Pdatalink_class_t = ^datalink_class_t;
  //const
  //  DATALINK_CLASS_ALL = (((((((DATALINK_CLASS_PHYS or DATALINK_CLASS_VLAN) or DATALINK_CLASS_AGGR) or DATALINK_CLASS_VNIC) or DATALINK_CLASS_ETHERSTUB) or DATALINK_CLASS_SIMNET) or DATALINK_CLASS_BRIDGE) or DATALINK_CLASS_IPTUN) or DATALINK_CLASS_PART;
  {
   * A combination of flags and media.
   *   flags is the higher 32 bits, and if it is 0x01, it indicates all media
   *   types can be accepted; otherwise, only the given media type (specified
   *   in the lower 32 bits) is accepted.
    }

  type
    datalink_media_t = uint64_t;

  { was #define dname def_expr }
  //function DATALINK_ANY_MEDIATYPE : datalink_media_t;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }
  //function DATALINK_MEDIA_ACCEPTED(dmedia,media : longint) : longint;

  const
    MAXLINKATTRLEN = 32;
    MAXLINKATTRVALLEN = 1024;
  {
   * Link attributes used by the kernel.
    }
  {
   * The major number and instance number of the underlying physical device
   * are kept as FPHYMAJ and FPHYINST (major, instance + 1).
   *
   * Set for physical links only.
    }
  { uint64_t  }
    FPHYMAJ = 'phymaj';
  { uint64_t  }
    FPHYINST = 'phyinst';
  {
   * The devname of the physical link. For example, bge0, ce1. Set for physical
   * links only.
    }
  { string  }
    FDEVNAME = 'devname';
  {
   * The door file for the dlmgmtd (data-link management) daemon.
    }
    DLMGMT_TMPFS_DIR = '/etc/svc/volatile/dladm';
(* error
#define	DLMGMT_DOOR		DLMGMT_TMPFS_DIR "/dlmgmt_door"
in define line 98 *)
    {
     * Door upcall commands.
      }
      DLMGMT_CMD_DLS_CREATE = 1;
      DLMGMT_CMD_DLS_GETATTR = 2;
      DLMGMT_CMD_DLS_DESTROY = 3;
      DLMGMT_CMD_GETNAME = 4;
      DLMGMT_CMD_GETLINKID = 5;
      DLMGMT_CMD_GETNEXT = 6;
      DLMGMT_CMD_DLS_UPDATE = 7;
      DLMGMT_CMD_LINKPROP_INIT = 8;
      DLMGMT_CMD_SETZONEID = 9;
      DLMGMT_CMD_BASE = 128;
    {
     * Indicate the link mapping is active or persistent
      }
      DLMGMT_ACTIVE = $01;
      DLMGMT_PERSIST = $02;
    { upcall argument  }

    type
      dlmgmt_door_arg = record
          ld_cmd : uint_t;
        end;
      dlmgmt_door_arg_t = dlmgmt_door_arg;

      dlmgmt_upcall_arg_create = record
          ld_cmd : longint;
          ld_class : datalink_class_t;
          ld_media : uint32_t;
          ld_persist : boolean_t;
          ld_phymaj : uint64_t;
          ld_phyinst : uint64_t;
          ld_devname : array[0..(MAXNAMELEN)-1] of char;
        end;
      dlmgmt_upcall_arg_create_t = dlmgmt_upcall_arg_create;
    {
     * Note: ld_padding is necessary to keep the size of the structure the
     * same on amd64 and i386.  The same note applies to other ld_padding
     * and lr_paddding fields in structures throughout this file.
      }

      dlmgmt_upcall_arg_destroy = record
          ld_cmd : longint;
          ld_linkid : datalink_id_t;
          ld_persist : boolean_t;
          ld_padding : longint;
        end;
      dlmgmt_upcall_arg_destroy_t = dlmgmt_upcall_arg_destroy;

      dlmgmt_upcall_arg_update = record
          ld_cmd : longint;
          ld_novanity : boolean_t;
          ld_media : uint32_t;
          ld_padding : uint32_t;
          ld_devname : array[0..(MAXNAMELEN)-1] of char;
        end;
      dlmgmt_upcall_arg_update_t = dlmgmt_upcall_arg_update;

      dlmgmt_upcall_arg_getattr = record
          ld_cmd : longint;
          ld_linkid : datalink_id_t;
          ld_attr : array[0..(MAXLINKATTRLEN)-1] of char;
        end;
      dlmgmt_upcall_arg_getattr_t = dlmgmt_upcall_arg_getattr;

      dlmgmt_door_getname = record
          ld_cmd : longint;
          ld_linkid : datalink_id_t;
        end;
      dlmgmt_door_getname_t = dlmgmt_door_getname;

      dlmgmt_door_getlinkid = record
          ld_cmd : longint;
          ld_link : array[0..(MAXLINKNAMELEN)-1] of char;
          ld_zoneid : zoneid_t;
        end;
      dlmgmt_door_getlinkid_t = dlmgmt_door_getlinkid;

      dlmgmt_door_getnext_s = record
          ld_cmd : longint;
          ld_linkid : datalink_id_t;
          ld_class : datalink_class_t;
          ld_flags : uint32_t;
          ld_dmedia : datalink_media_t;
        end;
      dlmgmt_door_getnext_t = dlmgmt_door_getnext_s;

      dlmgmt_door_linkprop_init = record
          ld_cmd : longint;
          ld_linkid : datalink_id_t;
        end;
      dlmgmt_door_linkprop_init_t = dlmgmt_door_linkprop_init;

      dlmgmt_door_setzoneid = record
          ld_cmd : longint;
          ld_linkid : datalink_id_t;
          ld_zoneid : zoneid_t;
        end;
      dlmgmt_door_setzoneid_t = dlmgmt_door_setzoneid;
    { upcall return value  }
    { return error code  }

      dlmgmt_retval_s = record
          lr_err : uint_t;
        end;
      dlmgmt_retval_t = dlmgmt_retval_s;

      dlmgmt_destroy_retval_t = dlmgmt_retval_t;
      dlmgmt_linkprop_init_retval_t = dlmgmt_destroy_retval_t;
      dlmgmt_setzoneid_retval_t = dlmgmt_destroy_retval_t;
      dlmgmt_linkid_retval_s = record
          lr_err : uint_t;
          lr_linkid : datalink_id_t;
          lr_flags : uint32_t;
          lr_class : datalink_class_t;
          lr_media : uint32_t;
          lr_padding : uint32_t;
        end;

(* error
typedef struct dlmgmt_linkid_retval_s	dlmgmt_create_retval_t,
 in member_list *)

      //dlmgmt_linkid_retval_s = record
      //  end;
      dlmgmt_getname_retval_t = dlmgmt_linkid_retval_s;

      dlmgmt_getattr_retval_s = record
          lr_err : uint_t;
          lr_type : uint_t;
          lr_attrsz : uint_t;
          lr_padding : uint_t;
          lr_attrval : array[0..(MAXLINKATTRVALLEN)-1] of char;
        end;
      dlmgmt_getattr_retval_t = dlmgmt_getattr_retval_s;
{ C++ end of extern C conditionnal removed }

{ _DLS_MGMT_H  }

//implementation
//
//  { was #define dname def_expr }
//  function DATALINK_ANY_MEDIATYPE : datalink_media_t;
//    begin
//      DATALINK_ANY_MEDIATYPE:=datalink_media_t((datalink_media_t($01)) shl 32);
//    end;
//
//  { was #define dname(params) para_def_expr }
//  { argument types are unknown }
//  { return type might be wrong }
//  function DATALINK_MEDIA_ACCEPTED(dmedia,media : longint) : longint;
//  var
//     if_local1 : longint;
//  (* result types are not known *)
//  begin
//    if (uint32_t((dmedia shr 32) and $ffffffff)) and $01 then
//      if_local1:=B_TRUE
//    else
//      if_local1:=(uint32_t(dmedia(@($ffffffff))))=media;
//    DATALINK_MEDIA_ACCEPTED:=if_local1;
//  end;


{ END ----------------------------------- }
{ END dls_mgmt.h from uts/common/sys/mac.h}
{ END ----------------------------------- }


{ ----------------------------------- }
{ mac.h from uts/common/sys/mac.h     }
{ ----------------------------------- }

const
  External_library_mac=''; {Setup as you need}
  MAC_INFO = 'MAC Services';
{
 * MAC-Type version identifier.  This is used by mactype_alloc() and
 * mactype_register() to verify that incompatible MAC-Type plugins don't
 * register.
  }
  MACTYPE_VERSION = $1;
  DATALINK_INVALID_LINKID = 0;
  DATALINK_ALL_LINKID = 0;
  DATALINK_MAX_LINKID = $ffffffff;

  //const
  //  mpr_range_uint32 = u.mpr_uint32;
  {
   * Maximum MAC address length
    }
  MAXMACADDRLEN  = 20;
  MPT_MAXMACADDR = 32;

{ max property name len  }
  MAXLINKPROPNAME = 256;

  {
   * Flags to figure out r/w status of legacy ndd props.
    }

  const
    MAC_PROP_PERM_READ = $0001;
    MAC_PROP_PERM_WRITE = $0010;
    MAC_PROP_MAP_KSTAT = $0100;
    MAC_PROP_PERM_RW = MAC_PROP_PERM_READ or MAC_PROP_PERM_WRITE;
    MAC_PROP_FLAGS_RK = MAC_PROP_PERM_READ or MAC_PROP_MAP_KSTAT;

Type
//  Pmac_handle_t  = ^mac_handle_t;
//  Pmac_header_info_t  = ^mac_header_info_t;
//  Pmac_resource_t  = ^mac_resource_t;
//  Pmac_ring_handle_t  = ^mac_ring_handle_t;
//  Pmactype_register_t  = ^mactype_register_t;
//  Pmblk_t  = ^mblk_t;

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
 * Copyright (c) 2014, Joyent, Inc.  All rights reserved.
  }

 {
 * MAC Services Module
  }
{ C++ extern C conditionnal removed }
{
 * MAC Information (text emitted by modinfo(1m))
  }

{
 * Opaque handle types
  }

  //mac_handle_t = ^__mac_handle;
  //mac_resource_handle_t = ^__mac_resource_handle;
  //mac_notify_handle_t = ^__mac_notify_handle;
  //mac_tx_notify_handle_t = ^__mac_tx_notify_handle;
  //mac_intr_handle_t = ^__mac_intr_handle;
  //mac_ring_handle_t = ^__mac_ring_handle;
  //mac_group_handle_t = ^__mac_group_handle;

  link_state_t = (LINK_STATE_UNKNOWN := -(1),LINK_STATE_DOWN,
    LINK_STATE_UP);

  link_duplex_t = (LINK_DUPLEX_UNKNOWN := 0,LINK_DUPLEX_HALF,
    LINK_DUPLEX_FULL);

  link_flowctrl_t = (LINK_FLOWCTRL_NONE := 0,LINK_FLOWCTRL_RX,
    LINK_FLOWCTRL_TX,LINK_FLOWCTRL_BI);

  link_tagmode_t = (LINK_TAGMODE_VLANONLY := 0,LINK_TAGMODE_NORMAL
    );
{
 * Defines range of uint32_t values
  }

  mac_propval_uint32_range_s = record
      mpur_min : uint32_t;
      mpur_max : uint32_t;
    end;
  mac_propval_uint32_range_t = mac_propval_uint32_range_s;
{
 * Data type of property values.
  }

  mac_propval_type_t = (MAC_PROPVAL_UINT8,MAC_PROPVAL_UINT32,
    MAC_PROPVAL_STR);
{
 * Captures possible values for a given property. A property can have
 * range of values (int32, int64, uint32, uint64, et al) or collection/
 * enumeration of values (strings).
 * Can be used as a value-result parameter.
  }
{ count of ranges  }
{ type of value  }

  mac_propval_range_s = record
      mpr_count : uint_t;
      mpr_type : mac_propval_type_t;
      u : record
          case longint of
            0 : ( mpr_uint32 : array[0..0] of mac_propval_uint32_range_t );
          end;
    end;
  mac_propval_range_t = mac_propval_range_s;

  mac_secondary_addr_s = record
      ms_addrcnt : uint32_t;
      ms_addrs : array[0..(MPT_MAXMACADDR)-1] of array[0..(MAXMACADDRLEN)-1] of uint8_t;
    end;
  mac_secondary_addr_t = mac_secondary_addr_s;

  mac_logtype_t = (MAC_LOGTYPE_LINK := 1,MAC_LOGTYPE_FLOW
    );

{
 * Public properties.
 *
 * Note that there are 2 sets of parameters: the *_EN_* values are
 * those that the Administrator configures for autonegotiation. The
 * _ADV_* values are those that are currently exposed over the wire.
  }

  mac_prop_id_t = (MAC_PROP_DUPLEX := $00000001,MAC_PROP_SPEED,
    MAC_PROP_STATUS,MAC_PROP_AUTONEG,MAC_PROP_EN_AUTONEG,
    MAC_PROP_MTU,MAC_PROP_ZONE,MAC_PROP_AUTOPUSH,
    MAC_PROP_FLOWCTRL,MAC_PROP_ADV_1000FDX_CAP,
    MAC_PROP_EN_1000FDX_CAP,MAC_PROP_ADV_1000HDX_CAP,
    MAC_PROP_EN_1000HDX_CAP,MAC_PROP_ADV_100FDX_CAP,
    MAC_PROP_EN_100FDX_CAP,MAC_PROP_ADV_100HDX_CAP,
    MAC_PROP_EN_100HDX_CAP,MAC_PROP_ADV_10FDX_CAP,
    MAC_PROP_EN_10FDX_CAP,MAC_PROP_ADV_10HDX_CAP,
    MAC_PROP_EN_10HDX_CAP,MAC_PROP_ADV_100T4_CAP,
    MAC_PROP_EN_100T4_CAP,MAC_PROP_IPTUN_HOPLIMIT,
    MAC_PROP_IPTUN_ENCAPLIMIT,MAC_PROP_WL_ESSID,
    MAC_PROP_WL_BSSID,MAC_PROP_WL_BSSTYPE,
    MAC_PROP_WL_LINKSTATUS,MAC_PROP_WL_DESIRED_RATES,
    MAC_PROP_WL_SUPPORTED_RATES,MAC_PROP_WL_AUTH_MODE,
    MAC_PROP_WL_ENCRYPTION,MAC_PROP_WL_RSSI,
    MAC_PROP_WL_PHY_CONFIG,MAC_PROP_WL_CAPABILITY,
    MAC_PROP_WL_WPA,MAC_PROP_WL_SCANRESULTS,
    MAC_PROP_WL_POWER_MODE,MAC_PROP_WL_RADIO,
    MAC_PROP_WL_ESS_LIST,MAC_PROP_WL_KEY_TAB,
    MAC_PROP_WL_CREATE_IBSS,MAC_PROP_WL_SETOPTIE,
    MAC_PROP_WL_DELKEY,MAC_PROP_WL_KEY,MAC_PROP_WL_MLME,
    MAC_PROP_TAGMODE,MAC_PROP_ADV_10GFDX_CAP,
    MAC_PROP_EN_10GFDX_CAP,MAC_PROP_PVID,
    MAC_PROP_LLIMIT,MAC_PROP_LDECAY,MAC_PROP_RESOURCE,
    MAC_PROP_RESOURCE_EFF,MAC_PROP_RXRINGSRANGE,
    MAC_PROP_TXRINGSRANGE,MAC_PROP_MAX_TX_RINGS_AVAIL,
    MAC_PROP_MAX_RX_RINGS_AVAIL,MAC_PROP_MAX_RXHWCLNT_AVAIL,
    MAC_PROP_MAX_TXHWCLNT_AVAIL,MAC_PROP_IB_LINKMODE,
    MAC_PROP_VN_PROMISC_FILTERED,MAC_PROP_SECONDARY_ADDRS,
    MAC_PROP_PRIVATE := -(1));

 {$ifdef _KERNEL}
{
 * There are three ranges of statistics values.  0 to 1 - MAC_STAT_MIN are
 * interface statistics maintained by the mac module.  MAC_STAT_MIN to 1 -
 * MACTYPE_STAT_MIN are common MAC statistics defined by the mac module and
 * maintained by each driver.  MACTYPE_STAT_MIN and above are statistics
 * defined by MAC-Type plugins and maintained by each driver.
  }
  MAC_STAT_MIN = 1000;
  MACTYPE_STAT_MIN = 2000;
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IS_MAC_STAT(stat : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function IS_MACTYPE_STAT(stat : longint) : longint;

{
 * Statistics maintained by the mac module, and possibly populated as link
 * statistics.
  }

type
  mac_mod_stat = (MAC_STAT_LINK_STATE,MAC_STAT_LINK_UP,
    MAC_STAT_PROMISC,MAC_STAT_LOWLINK_STATE,
    MAC_STAT_HDROPS);

{
 * Do not reorder, and add only to the end of this list.
  }
{ MIB-II stats (RFC 1213 and RFC 1573)  }
  mac_driver_stat = (MAC_STAT_IFSPEED := MAC_STAT_MIN,MAC_STAT_MULTIRCV,
    MAC_STAT_BRDCSTRCV,MAC_STAT_MULTIXMT,
    MAC_STAT_BRDCSTXMT,MAC_STAT_NORCVBUF,
    MAC_STAT_IERRORS,MAC_STAT_UNKNOWNS,MAC_STAT_NOXMTBUF,
    MAC_STAT_OERRORS,MAC_STAT_COLLISIONS,
    MAC_STAT_RBYTES,MAC_STAT_IPACKETS,MAC_STAT_OBYTES,
    MAC_STAT_OPACKETS,MAC_STAT_UNDERFLOWS,
    MAC_STAT_OVERFLOWS);


const
  MAC_NSTAT = (MAC_STAT_OVERFLOWS-MAC_STAT_IFSPEED)+1;
{#define	MAC_STAT_ISACOUNTER(_stat) (		\ }
{	    (_stat) == MAC_STAT_MULTIRCV ||	\ }
{	    (_stat) == MAC_STAT_BRDCSTRCV ||	\ }
{	    (_stat) == MAC_STAT_MULTIXMT ||	\ }
{	    (_stat) == MAC_STAT_BRDCSTXMT ||	\ }
{	    (_stat) == MAC_STAT_NORCVBUF ||	\ }
{	    (_stat) == MAC_STAT_IERRORS ||	\ }
{	    (_stat) == MAC_STAT_UNKNOWNS ||	\ }
{	    (_stat) == MAC_STAT_NOXMTBUF ||	\ }
{	    (_stat) == MAC_STAT_OERRORS ||	\ }
{	    (_stat) == MAC_STAT_COLLISIONS ||	\ }
{	    (_stat) == MAC_STAT_RBYTES ||	\ }
{	    (_stat) == MAC_STAT_IPACKETS ||	\ }
{	    (_stat) == MAC_STAT_OBYTES ||	\ }
{	    (_stat) == MAC_STAT_OPACKETS ||	\ }
{	    (_stat) == MAC_STAT_UNDERFLOWS ||	\ }
{	    (_stat) == MAC_STAT_OVERFLOWS) }
{
 * Immutable information. (This may not be modified after registration).
  }

type
  mac_info_s = record
      mi_media : uint_t;
      mi_nativemedia : uint_t;
      mi_addr_length : uint_t;
      mi_unicst_addr : ^uint8_t;
      mi_brdcst_addr : ^uint8_t;
    end;
  mac_info_t = mac_info_s;
{
 * When VNICs are created on top of the NIC, there are two levels
 * of MAC layer, a lower MAC, which is the MAC layer at the level of the
 * physical NIC, and an upper MAC, which is the MAC layer at the level
 * of the VNIC. Each VNIC maps to a MAC client at the lower MAC, and
 * the SRS and classification is done at the lower MAC level. The upper
 * MAC is therefore for the most part pass-through, and therefore
 * special processing needs to be done at the upper MAC layer when
 * dealing with a VNIC.
 *
 * This capability allows the MAC layer to detect when a VNIC is being
 * access, and implement the required shortcuts.
 *
 * In addition, this capability is used to keep the VNIC's secondary
 * mac_clients in sync when the primary MAC is updated.
  }

  mac_client_handle_fn_t = function (_para1:pointer):pointer;cdecl;

  mac_client_update_fn_t = procedure (_para1:pointer);cdecl;

  mac_capab_vnic_s = record
      mcv_arg : pointer;
      mcv_mac_client_handle : mac_client_handle_fn_t;
      mcv_mac_secondary_update : mac_client_update_fn_t;
    end;
  mac_capab_vnic_t = mac_capab_vnic_s;
(* Const before type ignored *)

  mac_rename_fn_t = procedure (_para1:Pchar; _para2:pointer);cdecl;

  mac_tx_ring_fn_t = function (_para1:pointer; _para2:Pmblk_t; _para3:uintptr_t; _para4:Pmac_ring_handle_t):Pmblk_t;cdecl;
(* Const before type ignored *)

  mac_capab_aggr_s = record
      mca_rename_fn : mac_rename_fn_t;
      mca_unicst : function (_para1:pointer; _para2:Puint8_t):longint;cdecl;
      mca_find_tx_ring_fn : mac_tx_ring_fn_t;
      mca_arg : pointer;
    end;
  mac_capab_aggr_t = mac_capab_aggr_s;
{ Bridge transmit and receive function signatures  }

  mac_bridge_tx_t = function (_para1:mac_handle_t; _para2:mac_ring_handle_t; _para3:Pmblk_t):Pmblk_t;cdecl;

  mac_bridge_rx_t = procedure (_para1:mac_handle_t; _para2:mac_resource_handle_t; _para3:Pmblk_t);cdecl;

  mac_bridge_ref_t = procedure (_para1:mac_handle_t; _para2:boolean_t);cdecl;

  mac_bridge_ls_t = function (_para1:mac_handle_t; _para2:link_state_t):link_state_t;cdecl;
{ must change mac_notify_cb_list[] in mac_provider.c if this is changed  }
{ must be the last entry  }

  mac_notify_type_t = (MAC_NOTE_LINK,MAC_NOTE_UNICST,MAC_NOTE_TX,
    MAC_NOTE_DEVPROMISC,MAC_NOTE_FASTPATH_FLUSH,
    MAC_NOTE_SDU_SIZE,MAC_NOTE_DEST,MAC_NOTE_MARGIN,
    MAC_NOTE_CAPAB_CHG,MAC_NOTE_LOWLINK,MAC_NOTE_ALLOWED_IPS,
    MAC_NNOTE);

  mac_notify_t = procedure (_para1:pointer; _para2:mac_notify_type_t);cdecl;

  mac_rx_t = procedure (_para1:pointer; _para2:mac_resource_handle_t; _para3:Pmblk_t; _para4:boolean_t);cdecl;

  mac_receive_t = function (_para1:pointer; _para2:longint):Pmblk_t;cdecl;
{
 * MAC resource types
  }

  mac_resource_type_t = (MAC_RX_FIFO := 1);

  mac_intr_enable_t = function (_para1:mac_intr_handle_t):longint;cdecl;

  mac_intr_disable_t = function (_para1:mac_intr_handle_t):longint;cdecl;

  mac_intr_s = record
      mi_handle : mac_intr_handle_t;
      mi_enable : mac_intr_enable_t;
      mi_disable : mac_intr_disable_t;
      mi_ddi_handle : ddi_intr_handle_t;
      mi_ddi_shared : boolean_t;
    end;
  mac_intr_t = mac_intr_s;
{ MAC_RX_FIFO  }
{
	 * The CPU this flow is to be processed on. With intrd and future
	 * things, we should know which CPU the flow needs to be processed
	 * and get a squeue assigned on that CPU.
	  }

  mac_rx_fifo_s = record
      mrf_type : mac_resource_type_t;
      mrf_intr : mac_intr_t;
      mrf_receive : mac_receive_t;
      mrf_rx_arg : pointer;
      mrf_flow_priority : uint32_t;
      mrf_cpu_id : uint_t;
    end;
  mac_rx_fifo_t = mac_rx_fifo_s;

const
  mrf_intr_handle = mrf_intr.mi_handle;
  mrf_intr_enable = mrf_intr.mi_enable;
  mrf_intr_disable = mrf_intr.mi_disable;

type
  mac_resource_u = record
      case longint of
        0 : ( mr_type : mac_resource_type_t );
        1 : ( mr_fifo : mac_rx_fifo_t );
      end;
  mac_resource_t = mac_resource_u;

  mac_addrtype_t = (MAC_ADDRTYPE_UNICAST,MAC_ADDRTYPE_MULTICAST,
    MAC_ADDRTYPE_BROADCAST);
(* Const before type ignored *)
(* Const before type ignored *)

  mac_header_info_s = record
      mhi_hdrsize : size_t;
      mhi_pktsize : size_t;
      mhi_daddr : ^uint8_t;
      mhi_saddr : ^uint8_t;
      mhi_origsap : uint32_t;
      mhi_bindsap : uint32_t;
      mhi_dsttype : mac_addrtype_t;
      mhi_tci : uint16_t;
      mhi_istagged : boolean_t;
      mhi_ispvid : boolean_t;
    end;
  mac_header_info_t = mac_header_info_s;
{
 * Function pointer to match dls client signature. Should be same as
 * dls_rx_t to allow a soft ring to bypass DLS layer and call a DLS
 * client directly.
  }

  mac_direct_rx_t = procedure (_para1:pointer; _para2:mac_resource_handle_t; _para3:Pmblk_t; _para4:Pmac_header_info_t);cdecl;

  mac_resource_add_t = function (_para1:pointer; _para2:Pmac_resource_t):mac_resource_handle_t;cdecl;

  mac_resource_bind_t = function (_para1:pointer; _para2:mac_resource_handle_t; _para3:processorid_t):longint;cdecl;

  mac_resource_remove_t = procedure (_para1:pointer; _para2:pointer);cdecl;

  mac_resource_quiesce_t = procedure (_para1:pointer; _para2:pointer);cdecl;

  mac_resource_restart_t = procedure (_para1:pointer; _para2:pointer);cdecl;

  mac_resource_modify_t = function (_para1:pointer; _para2:pointer; _para3:Pmac_resource_t):longint;cdecl;

  mac_change_upcall_t = procedure (_para1:pointer; _para2:mac_direct_rx_t; _para3:pointer);cdecl;
{
 * MAC-Type plugin interfaces
  }
(* Const before type ignored *)

  mtops_addr_verify_t = function (_para1:pointer; _para2:pointer):longint;cdecl;

  mtops_sap_verify_t = function (_para1:uint32_t; _para2:Puint32_t; _para3:pointer):boolean_t;cdecl;
(* Const before type ignored *)
(* Const before type ignored *)

  mtops_header_t = function (_para1:pointer; _para2:pointer; _para3:uint32_t; _para4:pointer; _para5:Pmblk_t;
               _para6:size_t):Pmblk_t;cdecl;

  mtops_header_info_t = function (_para1:Pmblk_t; _para2:pointer; _para3:Pmac_header_info_t):longint;cdecl;

  mtops_pdata_verify_t = function (_para1:pointer; _para2:size_t):boolean_t;cdecl;

  mtops_header_modify_t = function (_para1:Pmblk_t; _para2:pointer):Pmblk_t;cdecl;

  mtops_link_details_t = procedure (_para1:Pchar; _para2:size_t; _para3:mac_handle_t; _para4:pointer);cdecl;
{
	 * mtops_unicst_verify() returns 0 if the given address is a valid
	 * unicast address, or a non-zero errno otherwise.
	  }
{
	 * mtops_multicst_verify() returns 0 if the given address is a
	 * valid multicast address, or a non-zero errno otherwise.  If the
	 * media doesn't support multicast, ENOTSUP should be returned (for
	 * example).
	  }
{
	 * mtops_sap_verify() returns B_TRUE if the given SAP is a valid
	 * SAP value, or B_FALSE otherwise.
	  }
{
	 * mtops_header() is used to allocate and construct a MAC header.
	  }
{
	 * mtops_header_info() is used to gather information on a given MAC
	 * header.
	  }
{
	 * mtops_pdata_verify() is used to verify the validity of MAC
	 * plugin data.  It is called by mac_register() if the driver has
	 * supplied MAC plugin data, and also by mac_pdata_update() when
	 * drivers update the data.
	  }
{
	 * mtops_header_cook() is an optional callback that converts (or
	 * "cooks") the given raw header (as sent by a raw DLPI consumer)
	 * into one that is appropriate to send down to the MAC driver.
	 * Following the example above, an Ethernet header sent down by a
	 * DLPI consumer would be converted to whatever header the MAC
	 * driver expects.
	  }
{
	 * mtops_header_uncook() is an optional callback that does the
	 * opposite of mtops_header_cook().  It "uncooks" a given MAC
	 * header (as received from the driver) for consumption by raw DLPI
	 * consumers.  For example, for a non-Ethernet plugin that wants
	 * raw DLPI consumers to be fooled into thinking that the device
	 * provides Ethernet access, this callback would modify the given
	 * mblk_t such that the MAC header is converted to an Ethernet
	 * header.
	  }
{
	 * mtops_link_details() is an optional callback that provides
	 * extended information about the link state.  Its primary purpose
	 * is to provide type-specific support for syslog contents on
	 * link up events.  If no implementation is provided, then a default
	 * implementation will be used.
	  }

  mactype_ops_s = record
      mtops_ops : uint_t;
      mtops_unicst_verify : mtops_addr_verify_t;
      mtops_multicst_verify : mtops_addr_verify_t;
      mtops_sap_verify : mtops_sap_verify_t;
      mtops_header : mtops_header_t;
      mtops_header_info : mtops_header_info_t;
      mtops_pdata_verify : mtops_pdata_verify_t;
      mtops_header_cook : mtops_header_modify_t;
      mtops_header_uncook : mtops_header_modify_t;
      mtops_link_details : mtops_link_details_t;
    end;
  mactype_ops_t = mactype_ops_s;
{
 * mtops_ops exists for the plugin to enumerate the optional callback
 * entrypoints it has defined.  This allows the mac module to define
 * additional plugin entrypoints in mactype_ops_t without breaking backward
 * compatibility with old plugins.
  }

const
  MTOPS_PDATA_VERIFY = $001;
  MTOPS_HEADER_COOK = $002;
  MTOPS_HEADER_UNCOOK = $004;
  MTOPS_LINK_DETAILS = $008;
{
 * Provide mapping for legacy ndd ioctls relevant to that mactype.
 * Note that the ndd ioctls are obsolete, and may be removed in a future
 * release of Solaris. The ndd ioctls are not typically used in legacy
 * ethernet drivers. New datalink drivers of all link-types should use
 * dladm(1m) interfaces for administering tunables and not have to provide
 * a mapping.
  }

type
  mac_ndd_mapping_s = record
      mp_name : ^char;
      u_mp_id : record
          case longint of
            0 : ( u_id : mac_prop_id_t );
            1 : ( u_kstat : uint_t );
          end;
      mp_minval : longint;
      mp_maxval : longint;
      mp_valsize : size_t;
      mp_flags : longint;
    end;
  mac_ndd_mapping_t = mac_ndd_mapping_s;

const
  mp_prop_id = u_mp_id.u_id;
  mp_kstat = u_mp_id.u_kstat;
{ as defined in kstat_named_init(9F)  }

type
  mac_stat_info_s = record
      msi_stat : uint_t;
      msi_name : ^char;
      msi_type : uint_t;
      msi_default : uint64_t;
    end;
  mac_stat_info_t = mac_stat_info_s;
{ set by mactype_alloc()  }
(* Const before type ignored *)

  mactype_register_s = record
      mtr_version : uint_t;
      mtr_ident : ^char;
      mtr_ops : ^mactype_ops_t;
      mtr_mactype : uint_t;
      mtr_nativetype : uint_t;
      mtr_addrlen : uint_t;
      mtr_brdcst_addr : ^uint8_t;
      mtr_stats : ^mac_stat_info_t;
      mtr_statcount : size_t;
      mtr_mapping : ^mac_ndd_mapping_t;
      mtr_mappingcount : size_t;
    end;
  mactype_register_t = mactype_register_s;
{
 * Driver interface functions.
  }

function mac_open_by_linkid(_para1:datalink_id_t; _para2:Pmac_handle_t):longint;cdecl;external External_library_mac name 'mac_open_by_linkid';

(* Const before type ignored *)
function mac_open_by_linkname(_para1:Pchar; _para2:Pmac_handle_t):longint;cdecl;external External_library_mac name 'mac_open_by_linkname';

(* Const before type ignored *)
function mac_name(_para1:mac_handle_t):^char;cdecl;external External_library_mac name 'mac_name';

function mac_minor(_para1:mac_handle_t):minor_t;cdecl;external External_library_mac name 'mac_minor';

function mac_minor_hold(_para1:boolean_t):minor_t;cdecl;external External_library_mac name 'mac_minor_hold';

procedure mac_minor_rele(_para1:minor_t);cdecl;external External_library_mac name 'mac_minor_rele';

procedure mac_sdu_get(_para1:mac_handle_t; _para2:Puint_t; _para3:Puint_t);cdecl;external External_library_mac name 'mac_sdu_get';

procedure mac_sdu_get2(_para1:mac_handle_t; _para2:Puint_t; _para3:Puint_t; _para4:Puint_t);cdecl;external External_library_mac name 'mac_sdu_get2';

function mac_maxsdu_update(_para1:mac_handle_t; _para2:uint_t):longint;cdecl;external External_library_mac name 'mac_maxsdu_update';

function mac_maxsdu_update2(_para1:mac_handle_t; _para2:uint_t; _para3:uint_t):longint;cdecl;external External_library_mac name 'mac_maxsdu_update2';

function mac_addr_len(_para1:mac_handle_t):uint_t;cdecl;external External_library_mac name 'mac_addr_len';

function mac_type(_para1:mac_handle_t):longint;cdecl;external External_library_mac name 'mac_type';

function mac_nativetype(_para1:mac_handle_t):longint;cdecl;external External_library_mac name 'mac_nativetype';

(* Const before type ignored *)
procedure mac_unicst_update(_para1:mac_handle_t; _para2:Puint8_t);cdecl;external External_library_mac name 'mac_unicst_update';

procedure mac_capab_update(_para1:mac_handle_t);cdecl;external External_library_mac name 'mac_capab_update';

function mac_pdata_update(_para1:mac_handle_t; _para2:pointer; _para3:size_t):longint;cdecl;external External_library_mac name 'mac_pdata_update';

function mac_margin_update(_para1:mac_handle_t; _para2:uint32_t):boolean_t;cdecl;external External_library_mac name 'mac_margin_update';

procedure mac_margin_get(_para1:mac_handle_t; _para2:Puint32_t);cdecl;external External_library_mac name 'mac_margin_get';

function mac_margin_remove(_para1:mac_handle_t; _para2:uint32_t):longint;cdecl;external External_library_mac name 'mac_margin_remove';

function mac_margin_add(_para1:mac_handle_t; _para2:Puint32_t; _para3:boolean_t):longint;cdecl;external External_library_mac name 'mac_margin_add';

function mac_mtu_add(_para1:mac_handle_t; _para2:Puint32_t; _para3:boolean_t):longint;cdecl;external External_library_mac name 'mac_mtu_add';

function mac_mtu_remove(_para1:mac_handle_t; _para2:uint32_t):longint;cdecl;external External_library_mac name 'mac_mtu_remove';

function mac_fastpath_disable(_para1:mac_handle_t):longint;cdecl;external External_library_mac name 'mac_fastpath_disable';

procedure mac_fastpath_enable(_para1:mac_handle_t);cdecl;external External_library_mac name 'mac_fastpath_enable';

procedure mac_no_active(_para1:mac_handle_t);cdecl;external External_library_mac name 'mac_no_active';

function mactype_alloc(_para1:uint_t):^mactype_register_t;cdecl;external External_library_mac name 'mactype_alloc';

procedure mactype_free(_para1:Pmactype_register_t);cdecl;external External_library_mac name 'mactype_free';

function mactype_register(_para1:Pmactype_register_t):longint;cdecl;external External_library_mac name 'mactype_register';

(* Const before type ignored *)
function mactype_unregister(_para1:Pchar):longint;cdecl;external External_library_mac name 'mactype_unregister';

function mac_start_logusage(_para1:mac_logtype_t; _para2:uint_t):longint;cdecl;external External_library_mac name 'mac_start_logusage';

procedure mac_stop_logusage(_para1:mac_logtype_t);cdecl;external External_library_mac name 'mac_stop_logusage';

function mac_get_lower_mac_handle(_para1:mac_handle_t):mac_handle_t;cdecl;external External_library_mac name 'mac_get_lower_mac_handle';

function mac_is_vnic_primary(_para1:mac_handle_t):boolean_t;cdecl;external External_library_mac name 'mac_is_vnic_primary';

{
 * Packet hashing for distribution to multiple ports and rings.
  }
const
  MAC_PKT_HASH_L2 = $01;
  MAC_PKT_HASH_L3 = $02;
  MAC_PKT_HASH_L4 = $04;

function mac_pkt_hash(_para1:uint_t; _para2:Pmblk_t; _para3:uint8_t; _para4:boolean_t):uint64_t;cdecl;external External_library_mac name 'mac_pkt_hash';

{
 * Bridging linkage
  }
procedure mac_rx_common(_para1:mac_handle_t; _para2:mac_resource_handle_t; _para3:Pmblk_t);cdecl;external External_library_mac name 'mac_rx_common';

function mac_bridge_set(_para1:mac_handle_t; _para2:mac_handle_t):longint;cdecl;external External_library_mac name 'mac_bridge_set';

procedure mac_bridge_clear(_para1:mac_handle_t; _para2:mac_handle_t);cdecl;external External_library_mac name 'mac_bridge_clear';

procedure mac_bridge_vectors(_para1:mac_bridge_tx_t; _para2:mac_bridge_rx_t; _para3:mac_bridge_ref_t; _para4:mac_bridge_ls_t);cdecl;external External_library_mac name 'mac_bridge_vectors';

{ special case function for TRILL observability  }
procedure mac_trill_snoop(_para1:mac_handle_t; _para2:Pmblk_t);cdecl;external External_library_mac name 'mac_trill_snoop';

{$endif}
{ _KERNEL  }
{ C++ end of extern C conditionnal removed }
{ _SYS_MAC_H  }

//implementation
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }
//function IS_MAC_STAT(stat : longint) : longint;
//begin
//  IS_MAC_STAT:=(stat>=(MAC_STAT_MIN and (@(stat))))<MACTYPE_STAT_MIN;
//end;
//
//{ was #define dname(params) para_def_expr }
//{ argument types are unknown }
//{ return type might be wrong }
//function IS_MACTYPE_STAT(stat : longint) : longint;
//begin
//  IS_MACTYPE_STAT:=stat>=MACTYPE_STAT_MIN;
//end;


{ END ----------------------------------- }
{ END mac.h from uts/common/sys/mac.h     }
{ END ----------------------------------- }


{ ----------------------------------- }
{ socket_impl.h from uts/netinet/in.h }
{ ----------------------------------- }

  //type
  //  sa_family_t = uint16_t;
  {
   * Structure used by kernel to store most
   * addresses.
    }
  { address family  }
  { up to 14 bytes of direct address  }

  type
    sockaddr = record
        sa_family : sa_family_t;
        sa_data : array[0..13] of char;
      end;

  {
   * sockaddr_storage:
   * Common superset of at least AF_INET, AF_INET6 and AF_LINK sockaddr
   * structures. Has sufficient size and alignment for those sockaddrs.
    }
  {
   * Desired maximum size, alignment size and related types.
    }
  { Implementation specific max size  }

  const
    _SS_MAXSIZE = 256;
  {
   * To represent desired sockaddr max alignment for platform, a
   * type is chosen which may depend on implementation platform architecture.
   * Type chosen based on alignment size restrictions from <sys/isa_defs.h>.
   * We desire to force up to (but no more than) 64-bit (8 byte) alignment,
   * on platforms where it is possible to do so. (e.g not possible on ia32).
   * For all currently supported platforms by our implementation
   * in <sys/isa_defs.h>, (i.e. sparc, sparcv9, ia32, ia64)
   * type "double" is suitable for that intent.
   *
   * Note: Type "double" is chosen over the more obvious integer type int64_t.
   *   int64_t is not a valid type for strict ANSI/ISO C compilation on ILP32.
    }

  type
    sockaddr_maxalign_t = cdouble;

   const
      _SS_ALIGNSIZE = sizeof(sockaddr_maxalign_t);
      _SS_PAD1SIZE  = _SS_ALIGNSIZE-(sizeof(sa_family_t));
      _SS_PAD2SIZE  =_SS_MAXSIZE-(((sizeof(sa_family_t))+_SS_PAD1SIZE)+_SS_ALIGNSIZE);

  type
    sockaddr_storage = record
        ss_family : sa_family_t;
        _ss_pad1 : array[0..(_SS_PAD1SIZE)-1] of char;
        _ss_align : sockaddr_maxalign_t;
        _ss_pad2 : array[0..(_SS_PAD2SIZE)-1] of char;
      end;

  { !defined(_XPG4_2) || defined(_XPG6) || defined(__EXTENSIONS__)  }
  {
   * To be compatible with the Linux interfaces used, this structure is
   * placed in socket_impl.h so that an include for <sys/socket.h> will
   * pickup this structure. This structure is for use with PF_PACKET
   * sockets.
    }

  type
    sockaddr_ll = record
        sll_family : uint16_t;
        sll_protocol : uint16_t;
        sll_ifindex : int32_t;
        sll_hatype : uint16_t;
        sll_pkttype : uint8_t;
        sll_halen : uint8_t;
        sll_addr : array[0..7] of uint8_t;
      end;

  const
    LINUX_SLL_HOST = 0;
    LINUX_SLL_BROADCAST = 1;
    LINUX_SLL_MULTICAST = 2;
    LINUX_SLL_OTHERHOST = 3;
    LINUX_SLL_OUTGOING = 4;

{ END ----------------------------------- }
{ END socket_impl.h from uts/netinet/in.h }
{ END ----------------------------------- }


{ ----------------------------------- }
{ in.h from uts/netinet/in.h }
{ ----------------------------------- }

const
  External_library=''; {Setup as you need}

Type
  Pin6_addr  = ^in6_addr;
  Pin_addr  = ^in_addr;
  Psockaddr  = ^sockaddr;
  Psockaddr_storage  = ^sockaddr_storage;
//Puint32_t  = ^uint32_t;
//Puint8_t  = ^uint8_t;
//Puint_t  = ^uint_t;

{
 * Copyright 2009 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
 *
 * Copyright 2011 Nexenta Systems, Inc. All rights reserved.
  }
{
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
  }
{
 * Constants and structures defined by the internet system,
 * according to following documents
 *
 * Internet ASSIGNED NUMBERS (RFC1700) and its successors:
 *	http://www.iana.org/assignments/protocol-numbers
 *	http://www.iana.org/assignments/port-numbers
 * Basic Socket Interface Extensions for IPv6 (RFC2133 and its successors)
 *
//  }
//{$ifndef _SOCKLEN_T}
//{$define _SOCKLEN_T}
//{
// * The socklen definitions are reproduced here from sys/socket.h so as to
// * not introduce that namespace into existing users of netinet/in.h.
//  }
//{$if defined(_XPG4_2) && !defined(_XPG5) && !defined(_LP64)}
//
//type
//  socklen_t = size_t;
//{$else}
//
//type
//  socklen_t = uint32_t;
//{$endif}
//{ defined(_XPG4_2) && !defined(_XPG5) && !defined(_LP64)  }
//{$if defined(_XPG4_2) || defined(_BOOT)}
//
//type
//  Psocklen_t = ^socklen_t;
//{$else}
//
//type
//  Psocklen_t = pointer;
//{$endif}
//{ defined(_XPG4_2) || defined(_BOOT)  }
//{$endif}
//{ _SOCKLEN_T  }
//{$if !defined(_XPG4_2) || defined(__EXTENSIONS__)}
//{$include <sys/stream.h>}
//{$endif}
//{ !defined(_XPG4_2) || defined(__EXTENSIONS__)  }
//{
// * Symbols such as htonl() are required to be exposed through this file,
// * per XNS Issue 5. This is achieved by inclusion of <sys/byteorder.h>
//  }
//{$if !defined(_XPG4_2) || defined(__EXTENSIONS__) || defined(_XPG5)}
//{$include <sys/byteorder.h>}
//{$endif}
//{$ifndef _IN_PORT_T}
//{$define _IN_PORT_T}
//
type
  in_port_t = uint16_t;
//{$endif}
//{
// * Note: IPv4 address data structures usage conventions.
// * The "in_addr_t" type below (required by Unix standards)
// * is NOT a typedef of "struct in_addr" and violates the usual
// * conventions where "struct <name>" and <name>_t are corresponding
// * typedefs.
// * To minimize confusion, kernel data structures/usage prefers use
// * of "ipaddr_t" as atomic uint32_t type and avoid using "in_addr_t"
// * The user level APIs continue to follow the historic popular
// * practice of using "struct in_addr".
//  }
//{$ifndef _IN_ADDR_T}
//{$define _IN_ADDR_T}
//
//type
//  in_addr_t = uint32_t;
//{$endif}
//{$ifndef _IPADDR_T}
//{$define _IPADDR_T}
//
//type
//  ipaddr_t = uint32_t;
//{$endif}


//const
//  s6_addr = _S6_un._S6_u8;
//{$ifdef _KERNEL}
//  s6_addr8 = _S6_un._S6_u8;
//  s6_addr32 = _S6_un._S6_u32;
//{$endif}

{
 * Protocols
 *
 * Some of these constant names are copied for the DTrace IP provider in
 * usr/src/lib/libdtrace/common/ip.d.in, ip.sed.in, which should be kept
 * in sync.
  }
{ dummy for IP  }

const
  IPPROTO_IP = 0;
{ Hop by hop header for IPv6  }
  IPPROTO_HOPOPTS = 0;
{ control message protocol  }
  IPPROTO_ICMP = 1;
{ group control protocol  }
  IPPROTO_IGMP = 2;
{ gateway^2 (deprecated)  }
  IPPROTO_GGP = 3;
{ IP in IP encapsulation  }
  IPPROTO_ENCAP = 4;
{ tcp  }
  IPPROTO_TCP = 6;
{ exterior gateway protocol  }
  IPPROTO_EGP = 8;
{ pup  }
  IPPROTO_PUP = 12;
{ user datagram protocol  }
  IPPROTO_UDP = 17;
{ xns idp  }
  IPPROTO_IDP = 22;
{ IPv6 encapsulated in IP  }
  IPPROTO_IPV6 = 41;
{ Routing header for IPv6  }
  IPPROTO_ROUTING = 43;
{ Fragment header for IPv6  }
  IPPROTO_FRAGMENT = 44;
{ rsvp  }
  IPPROTO_RSVP = 46;
{ IPsec Encap. Sec. Payload  }
  IPPROTO_ESP = 50;
{ IPsec Authentication Hdr.  }
  IPPROTO_AH = 51;
{ ICMP for IPv6  }
  IPPROTO_ICMPV6 = 58;
{ No next header for IPv6  }
  IPPROTO_NONE = 59;
{ Destination options  }
  IPPROTO_DSTOPTS = 60;
{ "hello" routing protocol  }
  IPPROTO_HELLO = 63;
{ UNOFFICIAL net disk proto  }
  IPPROTO_ND = 77;
{ ISO clnp  }
  IPPROTO_EON = 80;
{ OSPF  }
  IPPROTO_OSPF = 89;
{ PIM routing protocol  }
  IPPROTO_PIM = 103;
{ Stream Control  }
  IPPROTO_SCTP = 132;
{ Transmission Protocol  }
{ raw IP packet  }
  IPPROTO_RAW = 255;
  IPPROTO_MAX = 256;

{ Sockets Direct Protocol  }
  PROTO_SDP = 257;

{
 * Port/socket numbers: network standard functions
 *
 * Entries should exist here for each port number compiled into an ON
 * component, such as snoop.
  }

const
  IPPORT_ECHO = 7;
  IPPORT_DISCARD = 9;
  IPPORT_SYSTAT = 11;
  IPPORT_DAYTIME = 13;
  IPPORT_NETSTAT = 15;
  IPPORT_CHARGEN = 19;
  IPPORT_FTP = 21;
  IPPORT_TELNET = 23;
  IPPORT_SMTP = 25;
  IPPORT_TIMESERVER = 37;
  IPPORT_NAMESERVER = 42;
  IPPORT_WHOIS = 43;
  IPPORT_DOMAIN = 53;
  IPPORT_MDNS = 5353;
  IPPORT_MTP = 57;
{
 * Port/socket numbers: host specific functions
  }
  IPPORT_BOOTPS = 67;
  IPPORT_BOOTPC = 68;
  IPPORT_TFTP = 69;
  IPPORT_RJE = 77;
  IPPORT_FINGER = 79;
  IPPORT_HTTP = 80;
  IPPORT_HTTP_ALT = 8080;
  IPPORT_TTYLINK = 87;
  IPPORT_SUPDUP = 95;
  IPPORT_NTP = 123;
  IPPORT_NETBIOS_NS = 137;
  IPPORT_NETBIOS_DGM = 138;
  IPPORT_NETBIOS_SSN = 139;
  IPPORT_LDAP = 389;
  IPPORT_SLP = 427;
  IPPORT_MIP = 434;
{ a.k.a. microsoft-ds  }
  IPPORT_SMB = 445;
{
 * Internet Key Exchange (IKE) ports
  }
  IPPORT_IKE = 500;
  IPPORT_IKE_NATT = 4500;
{
 * UNIX TCP sockets
  }
  IPPORT_EXECSERVER = 512;
  IPPORT_LOGINSERVER = 513;
  IPPORT_CMDSERVER = 514;
  IPPORT_PRINTER = 515;
  IPPORT_EFSSERVER = 520;
{
 * UNIX UDP sockets
  }
  IPPORT_BIFFUDP = 512;
  IPPORT_WHOSERVER = 513;
  IPPORT_SYSLOG = 514;
  IPPORT_TALK = 517;
  IPPORT_ROUTESERVER = 520;
  IPPORT_RIPNG = 521;
{
 * DHCPv6 UDP ports
  }
  IPPORT_DHCPV6C = 546;
  IPPORT_DHCPV6S = 547;
  IPPORT_SOCKS = 1080;
{
 * Ports < IPPORT_RESERVED are reserved for
 * privileged processes (e.g. root).
 * Ports > IPPORT_USERRESERVED are reserved
 * for servers, not necessarily privileged.
  }
  IPPORT_RESERVED = 1024;
  IPPORT_USERRESERVED = 5000;
{
 * Link numbers
  }
  IMPLINK_IP = 155;
  IMPLINK_LOWEXPER = 156;
  IMPLINK_HIGHEXPER = 158;
{
 * IPv4 Internet address
 *	This definition contains obsolete fields for compatibility
 *	with SunOS 3.x and 4.2bsd.  The presence of subnets renders
 *	divisions into fixed fields misleading at best.  New code
 *	should use only the s_addr field.
}
//const
//  _S_un_b = S_un_b;
//  _S_un_w = S_un_w;
//  _S_addr = S_addr;
//  _S_un = S_un;
//{$endif}
//{ !defined(_XPG4_2) || defined(__EXTENSIONS__)  }
//{$if !defined(_XPG4_2) || defined(__EXTENSIONS__)}
//{$else}
//{$endif}
//{ !defined(_XPG4_2) || defined(__EXTENSIONS__)  }
//{#define	s_addr	_S_un._S_addr		/* should be used for all code */ }
//{#define	s_host	_S_un._S_un_b.s_b2	/* OBSOLETE: host on imp */ }
//{#define	s_net	_S_un._S_un_b.s_b1	/* OBSOLETE: network */ }
//{#define	s_imp	_S_un._S_un_w.s_w2	/* OBSOLETE: imp */ }
//{#define	s_impno	_S_un._S_un_b.s_b4	/* OBSOLETE: imp # */ }
//{#define	s_lh	_S_un._S_un_b.s_b3	/* OBSOLETE: logical host */ }

type
  in_addr = record
      _S_un : record
          case longint of
            0 : ( _S_un_b : record
                s_b1 : uint8_t;
                s_b2 : uint8_t;
                s_b3 : uint8_t;
                s_b4 : uint8_t;
              end );
            1 : ( _S_un_w : record
                s_w1 : uint16_t;
                s_w2 : uint16_t;
              end );
                   //#if !defined(_XPG4_2) || defined(__EXTENSIONS__)
                   //		uint32_t _S_addr;
                   //#else
                   //		in_addr_t _S_addr;
                   //#endif /* !defined(_XPG4_2) || defined(__EXTENSIONS__) */
            2 : ( _S_addr : uint32_t );
//            3 : ( _S_addr : in_addr_t );
          end;
    end;

{
 * Definitions of bits in internet address integers.
 * On subnets, the decomposition of addresses to host and net parts
 * is done according to subnet mask, not the masks here.
 *
 * Note that with the introduction of CIDR, IN_CLASSA, IN_CLASSB,
 * IN_CLASSC, IN_CLASSD and IN_CLASSE macros have become "de-facto
 * obsolete". IN_MULTICAST macro should be used to test if a address
 * is a multicast address.
  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN_CLASSA(i : longint) : longint;

const
  IN_CLASSA_NET = $ff000000;
  IN_CLASSA_NSHIFT = 24;
  IN_CLASSA_HOST = $00ffffff;
  IN_CLASSA_MAX = 128;
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN_CLASSB(i : longint) : longint;

const
  IN_CLASSB_NET = $ffff0000;
  IN_CLASSB_NSHIFT = 16;
  IN_CLASSB_HOST = $0000ffff;
  IN_CLASSB_MAX = 65536;
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN_CLASSC(i : longint) : longint;

const
  IN_CLASSC_NET = $ffffff00;
  IN_CLASSC_NSHIFT = 8;
  IN_CLASSC_HOST = $000000ff;
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN_CLASSD(i : longint) : longint;

{ These aren't really   }
const
  IN_CLASSD_NET = $f0000000;
{ net and host fields, but  }
  IN_CLASSD_NSHIFT = 28;
{ routing needn't know  }
  IN_CLASSD_HOST = $0fffffff;
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN_CLASSE(i : longint) : longint;

const
  IN_CLASSE_NET = $ffffffff;
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN_MULTICAST(i : longint) : longint;

{
 * We have removed CLASS E checks from the kernel
 * But we preserve these defines for userland in order
 * to avoid compile  breakage of some 3rd party piece of software
  }
{$ifndef _KERNEL}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN_EXPERIMENTAL(i : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
//function IN_BADCLASS(i : longint) : longint;

{$endif}

const
  INADDR_ANY = $00000000;
  INADDR_LOOPBACK = $7F000001;
{ must be masked  }
  INADDR_BROADCAST = $ffffffff;
  INADDR_NONE = $ffffffff;
{ 224.0.0.0    }
  INADDR_UNSPEC_GROUP = $e0000000;
{ 224.0.0.1    }
  INADDR_ALLHOSTS_GROUP = $e0000001;
{ 224.0.0.2    }
  INADDR_ALLRTRS_GROUP = $e0000002;
{ 224.0.0.22, IGMPv3  }
  INADDR_ALLRPTS_GROUP = $e0000016;
{ 224.0.0.255  }
  INADDR_MAX_LOCAL_GROUP = $e00000ff;
{ Scoped IPv4 prefixes (in host byte-order)  }
{ 169.254/16  }
  IN_AUTOCONF_NET = $a9fe0000;
  IN_AUTOCONF_MASK = $ffff0000;
{ 10/8  }
  IN_PRIVATE8_NET = $0a000000;
  IN_PRIVATE8_MASK = $ff000000;
{ 172.16/12  }
  IN_PRIVATE12_NET = $ac100000;
  IN_PRIVATE12_MASK = $fff00000;
{ 192.168/16  }
  IN_PRIVATE16_NET = $c0a80000;
  IN_PRIVATE16_MASK = $ffff0000;
{ RFC 3927 IPv4 link local address (i in host byte-order)  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN_LINKLOCAL(i : longint) : longint;

{ Well known 6to4 Relay Router Anycast address defined in RFC 3068  }
{ 192.88.99.1  }

const
  INADDR_6TO4RRANYCAST = $c0586301;
{ official!  }

const
  IN_LOOPBACKNET = 127;
{
 * Define a macro to stuff the loopback address into an Internet address
  }
{#if !defined(_XPG4_2) || !defined(__EXTENSIONS__) }
{#define	IN_SET_LOOPBACK_ADDR(a) \ }
{	 (a)->sin_addr.s_addr  = htonl(INADDR_LOOPBACK); \ }
{	(a)->sin_family = AF_INET;  }
{#endif /* !defined(_XPG4_2) || !defined(__EXTENSIONS__) */ }
{
 * IPv4 Socket address.
  }
//{$if !defined(_XPG4_2) || defined(__EXTENSIONS__)}
//{$else}
//{$endif}
{ !defined(_XPG4_2) || defined(__EXTENSIONS__)  }

type
  sockaddr_in = record
      sin_family : sa_family_t;
      sin_port : in_port_t;
      sin_addr : in_addr;
      //sin_zero : array[0..7] of char;
      sin_zero : array[0..7] of byte;
    end;

//{$if !defined(_XPG4_2) || defined(_XPG6) || defined(__EXTENSIONS__)}
{
 * IPv6 socket address.
  }
{ Depends on scope of sin6_addr  }
{ Impl. specific - UDP replies  }

type
  sockaddr_in6 = record
      sin6_family : sa_family_t;
      sin6_port : in_port_t;
      sin6_flowinfo : uint32_t;
      sin6_addr : in6_addr;
      sin6_scope_id : uint32_t;
      __sin6_src_id : uint32_t;
    end;

{
 * Macros for accessing the traffic class and flow label fields from
 * sin6_flowinfo.
 * These are designed to be applied to a 32-bit value.
  }
{$ifdef _BIG_ENDIAN}
{ masks  }

const
  IPV6_FLOWINFO_FLOWLABEL = $000fffff;
  IPV6_FLOWINFO_TCLASS = $0ff00000;
{$else}
{ _BIG_ENDIAN  }
{ masks  }

const
  IPV6_FLOWINFO_FLOWLABEL = $ffff0f00;
  IPV6_FLOWINFO_TCLASS = $0000f00f;
{$endif}
{ _BIG_ENDIAN  }
{
 * Note: Macros IN6ADDR_ANY_INIT and IN6ADDR_LOOPBACK_INIT are for
 * use as RHS of Static initializers of "struct in6_addr" (or in6_addr_t)
 * only. They need to be different for User/Kernel versions because union
 * component data structure is defined differently (it is identical at
 * binary representation level).
 *
 * const struct in6_addr IN6ADDR_ANY_INIT;
 * const struct in6_addr IN6ADDR_LOOPBACK_INIT;
  }
{#ifdef _KERNEL }
{#define	IN6ADDR_ANY_INIT		 0, 0, 0, 0  }
{ }
{#ifdef _BIG_ENDIAN }
{#define	IN6ADDR_LOOPBACK_INIT		 0, 0, 0, 0x00000001U  }
{#else /* _BIG_ENDIAN */ }
{#define	IN6ADDR_LOOPBACK_INIT		 0, 0, 0, 0x01000000U  }
{#endif /* _BIG_ENDIAN */ }
{ }
{#else }
{ }
{#define	IN6ADDR_ANY_INIT	    	0, 0, 0, 0,	\ }
{					0, 0, 0, 0,	\ }
{					0, 0, 0, 0, 	\ }
{					0, 0, 0, 0  }
{ }
{#define	IN6ADDR_LOOPBACK_INIT	    	0, 0, 0, 0,	\ }
{					0, 0, 0, 0,	\ }
{					0, 0, 0, 0,	\ }
{					0, 0, 0, 0x1U  }
{#endif /* _KERNEL */ }
{
 * RFC 2553 specifies the following macros. Their type is defined
 * as "int" in the RFC but they only have boolean significance
 * (zero or non-zero). For the purposes of our comment notation,
 * we assume a hypothetical type "bool" defined as follows to
 * write the prototypes assumed for macros in our comments better.
 *
 * typedef int bool;
  }
{
 * IN6 macros used to test for special IPv6 addresses
 * (Mostly from spec)
 *
 * bool  IN6_IS_ADDR_UNSPECIFIED (const struct in6_addr *);
 * bool  IN6_IS_ADDR_LOOPBACK    (const struct in6_addr *);
 * bool  IN6_IS_ADDR_MULTICAST   (const struct in6_addr *);
 * bool  IN6_IS_ADDR_LINKLOCAL   (const struct in6_addr *);
 * bool  IN6_IS_ADDR_SITELOCAL   (const struct in6_addr *);
 * bool  IN6_IS_ADDR_V4MAPPED    (const struct in6_addr *);
 * bool  IN6_IS_ADDR_V4MAPPED_ANY(const struct in6_addr *); -- Not from RFC2553
 * bool  IN6_IS_ADDR_V4COMPAT    (const struct in6_addr *);
 * bool  IN6_IS_ADDR_MC_RESERVED (const struct in6_addr *); -- Not from RFC2553
 * bool  IN6_IS_ADDR_MC_NODELOCAL(const struct in6_addr *);
 * bool  IN6_IS_ADDR_MC_LINKLOCAL(const struct in6_addr *);
 * bool  IN6_IS_ADDR_MC_SITELOCAL(const struct in6_addr *);
 * bool  IN6_IS_ADDR_MC_ORGLOCAL (const struct in6_addr *);
 * bool  IN6_IS_ADDR_MC_GLOBAL   (const struct in6_addr *);
 * bool  IN6_IS_ADDR_6TO4	 (const struct in6_addr *); -- Not from RFC2553
 * bool  IN6_ARE_6TO4_PREFIX_EQUAL(const struct in6_addr *,
 *	     const struct in6_addr *);			    -- Not from RFC2553
 * bool  IN6_IS_ADDR_LINKSCOPE	 (const struct in6addr  *); -- Not from RFC2553
  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_UNSPECIFIED(addr : longint) : longint;

{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function IN6_IS_ADDR_LOOPBACK(addr : longint) : longint;

{$else}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_LOOPBACK(addr : longint) : longint;

{$endif}
{ _BIG_ENDIAN  }
{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IN6_IS_ADDR_MULTICAST(addr : longint) : longint;

{$else}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_MULTICAST(addr : longint) : longint;

{$endif}
{ _BIG_ENDIAN  }
{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IN6_IS_ADDR_LINKLOCAL(addr : longint) : longint;

{$else}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_LINKLOCAL(addr : longint) : longint;

{$endif}
{ _BIG_ENDIAN  }
{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IN6_IS_ADDR_SITELOCAL(addr : longint) : longint;

{$else}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_SITELOCAL(addr : longint) : longint;

{$endif}
{ _BIG_ENDIAN  }
{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IN6_IS_ADDR_V4MAPPED(addr : longint) : longint;

{$else}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_V4MAPPED(addr : longint) : longint;

{$endif}
{ _BIG_ENDIAN  }
{
 * IN6_IS_ADDR_V4MAPPED - A IPv4 mapped INADDR_ANY
 * Note: This macro is currently NOT defined in RFC2553 specification
 * and not a standard macro that portable applications should use.
  }
{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IN6_IS_ADDR_V4MAPPED_ANY(addr : longint) : longint;

{$else}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_V4MAPPED_ANY(addr : longint) : longint;

{$endif}
{ _BIG_ENDIAN  }
{ Exclude loopback and unspecified address  }
{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IN6_IS_ADDR_V4COMPAT(addr : longint) : longint;

{$else}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_V4COMPAT(addr : longint) : longint;

{$endif}
{ _BIG_ENDIAN  }
{
 * Note:
 * IN6_IS_ADDR_MC_RESERVED macro is currently NOT defined in RFC2553
 * specification and not a standard macro that portable applications
 * should use.
  }
{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IN6_IS_ADDR_MC_RESERVED(addr : longint) : longint;

{$else}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_MC_RESERVED(addr : longint) : longint;

{$endif}
{ _BIG_ENDIAN  }
{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IN6_IS_ADDR_MC_NODELOCAL(addr : longint) : longint;

{$else}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_MC_NODELOCAL(addr : longint) : longint;

{$endif}
{ _BIG_ENDIAN  }
{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IN6_IS_ADDR_MC_LINKLOCAL(addr : longint) : longint;

{$else}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_MC_LINKLOCAL(addr : longint) : longint;

{$endif}
{ _BIG_ENDIAN  }
{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IN6_IS_ADDR_MC_SITELOCAL(addr : longint) : longint;

{$else}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_MC_SITELOCAL(addr : longint) : longint;

{$endif}
{ _BIG_ENDIAN  }
{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IN6_IS_ADDR_MC_ORGLOCAL(addr : longint) : longint;

{$else}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_MC_ORGLOCAL(addr : longint) : longint;

{$endif}
{ _BIG_ENDIAN  }
{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IN6_IS_ADDR_MC_GLOBAL(addr : longint) : longint;

{$else}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_MC_GLOBAL(addr : longint) : longint;

{$endif}
{ _BIG_ENDIAN  }
{
 * The IN6_IS_ADDR_MC_SOLICITEDNODE macro is not defined in any standard or
 * RFC, and shouldn't be used by portable applications.  It is used to see
 * if an address is a solicited-node multicast address, which is prefixed
 * with ff02:0:0:0:0:1:ff00::/104.
  }
{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IN6_IS_ADDR_MC_SOLICITEDNODE(addr : longint) : longint;

{$else}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_MC_SOLICITEDNODE(addr : longint) : longint;

{$endif}
{
 * Macros to a) test for 6to4 IPv6 address, and b) to test if two
 * 6to4 addresses have the same /48 prefix, and, hence, are from the
 * same 6to4 site.
  }
{$ifdef _BIG_ENDIAN}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

function IN6_IS_ADDR_6TO4(addr : longint) : longint;

{$else}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_IS_ADDR_6TO4(addr : longint) : longint;

{$endif}
{ _BIG_ENDIAN  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//function IN6_ARE_6TO4_PREFIX_EQUAL(addr1,addr2 : longint) : longint;

{
 * IN6_IS_ADDR_LINKSCOPE
 * Identifies an address as being either link-local, link-local multicast or
 * node-local multicast.  All types of addresses are considered to be unique
 * within the scope of a given link.
  }
{#define	IN6_IS_ADDR_LINKSCOPE(addr) (IN6_IS_ADDR_LINKLOCAL(addr) || IN6_IS_ADDR_MC_LINKLOCAL(addr) || IN6_IS_ADDR_MC_NODELOCAL(addr)) }
{
 * Useful utility macros for operations with IPv6 addresses
 * Note: These macros are NOT defined in the RFC2553 or any other
 * standard specification and are not standard macros that portable
 * applications should use.
  }
{
 * IN6_V4MAPPED_TO_INADDR
 * IN6_V4MAPPED_TO_IPADDR
 *	Assign a IPv4-Mapped IPv6 address to an IPv4 address.
 *	Note: These macros are NOT defined in RFC2553 or any other standard
 *	specification and are not macros that portable applications should
 *	use.
 *
 * void IN6_V4MAPPED_TO_INADDR(const in6_addr_t *v6, struct in_addr *v4);
 * void IN6_V4MAPPED_TO_IPADDR(const in6_addr_t *v6, ipaddr_t v4);
 *
  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
//function IN6_V4MAPPED_TO_INADDR(v6,v4 : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
//function IN6_V4MAPPED_TO_IPADDR(v6,v4 : longint) : longint;

{
 * IN6_INADDR_TO_V4MAPPED
 * IN6_IPADDR_TO_V4MAPPED
 *	Assign a IPv4 address address to an IPv6 address as a IPv4-mapped
 *	address.
 *	Note: These macros are NOT defined in RFC2553 or any other standard
 *	specification and are not macros that portable applications should
 *	use.
 *
 * void IN6_INADDR_TO_V4MAPPED(const struct in_addr *v4, in6_addr_t *v6);
 * void IN6_IPADDR_TO_V4MAPPED(const ipaddr_t v4, in6_addr_t *v6);
 *
  }
{#ifdef _BIG_ENDIAN }
{#define	IN6_INADDR_TO_V4MAPPED(v4, v6) \ }
{	((v6)->_S6_un._S6_u32[3] = (v4)->s_addr, \ }
{	(v6)->_S6_un._S6_u32[2] = 0x0000ffff, \ }
{	(v6)->_S6_un._S6_u32[1] = 0, \ }
{	(v6)->_S6_un._S6_u32[0] = 0) }
{#define	IN6_IPADDR_TO_V4MAPPED(v4, v6) \ }
{	((v6)->_S6_un._S6_u32[3] = (v4), \ }
{	(v6)->_S6_un._S6_u32[2] = 0x0000ffff, \ }
{	(v6)->_S6_un._S6_u32[1] = 0, \ }
{	(v6)->_S6_un._S6_u32[0] = 0) }
{#else /* _BIG_ENDIAN */ }
{#define	IN6_INADDR_TO_V4MAPPED(v4, v6) \ }
{	((v6)->_S6_un._S6_u32[3] = (v4)->s_addr, \ }
{	(v6)->_S6_un._S6_u32[2] = 0xffff0000U, \ }
{	(v6)->_S6_un._S6_u32[1] = 0, \ }
{	(v6)->_S6_un._S6_u32[0] = 0) }
{#define	IN6_IPADDR_TO_V4MAPPED(v4, v6) \ }
{	((v6)->_S6_un._S6_u32[3] = (v4), \ }
{	(v6)->_S6_un._S6_u32[2] = 0xffff0000U, \ }
{	(v6)->_S6_un._S6_u32[1] = 0, \ }
{	(v6)->_S6_un._S6_u32[0] = 0) }
{#endif /* _BIG_ENDIAN */ }
{
 * IN6_6TO4_TO_V4ADDR
 *	Extract the embedded IPv4 address from the prefix to a 6to4 IPv6
 *      address.
 *	Note: This macro is NOT defined in RFC2553 or any other standard
 *	specification and is not a macro that portable applications should
 *	use.
 *	Note: we don't use the IPADDR form of the macro because we need
 *	to do a bytewise copy; the V4ADDR in the 6to4 address is not
 *	32-bit aligned.
 *
 * void IN6_6TO4_TO_V4ADDR(const in6_addr_t *v6, struct in_addr *v4);
 *
  }
{#define	IN6_6TO4_TO_V4ADDR(v6, v4) \ }
{	((v4)->_S_un._S_un_b.s_b1 = (v6)->_S6_un._S6_u8[2], \ }
{	(v4)->_S_un._S_un_b.s_b2 = (v6)->_S6_un._S6_u8[3],  \ }
{	(v4)->_S_un._S_un_b.s_b3 = (v6)->_S6_un._S6_u8[4],  \ }
{	(v4)->_S_un._S_un_b.s_b4 = (v6)->_S6_un._S6_u8[5]) }
{
 * IN6_V4ADDR_TO_6TO4
 *	Given an IPv4 address and an IPv6 address for output, a 6to4 address
 *	will be created from the IPv4 Address.
 *	Note:  This method for creating 6to4 addresses is not standardized
 *	outside of Solaris.  The newly created 6to4 address will be of the form
 *	2002:<V4ADDR>:<SUBNETID>::<HOSTID>, where SUBNETID will equal 0 and
 *	HOSTID will equal 1.
 *
 * void IN6_V4ADDR_TO_6TO4(const struct in_addr *v4, in6_addr_t *v6)
 *
  }
{#ifdef _BIG_ENDIAN }
{#define	IN6_V4ADDR_TO_6TO4(v4, v6) \ }
{	((v6)->_S6_un._S6_u8[0] = 0x20, \ }
{	(v6)->_S6_un._S6_u8[1] = 0x02, \ }
{	(v6)->_S6_un._S6_u8[2] = (v4)->_S_un._S_un_b.s_b1, \ }
{	(v6)->_S6_un._S6_u8[3] = (v4)->_S_un._S_un_b.s_b2, \ }
{	(v6)->_S6_un._S6_u8[4] = (v4)->_S_un._S_un_b.s_b3, \ }
{	(v6)->_S6_un._S6_u8[5] = (v4)->_S_un._S_un_b.s_b4, \ }
{	(v6)->_S6_un._S6_u8[6] = 0, \ }
{	(v6)->_S6_un._S6_u8[7] = 0, \ }
{	(v6)->_S6_un._S6_u32[2] = 0, \ }
{	(v6)->_S6_un._S6_u32[3] = 0x00000001U) }
{#else }
{#define	IN6_V4ADDR_TO_6TO4(v4, v6) \ }
{	((v6)->_S6_un._S6_u8[0] = 0x20, \ }
{	(v6)->_S6_un._S6_u8[1] = 0x02, \ }
{	(v6)->_S6_un._S6_u8[2] = (v4)->_S_un._S_un_b.s_b1, \ }
{	(v6)->_S6_un._S6_u8[3] = (v4)->_S_un._S_un_b.s_b2, \ }
{	(v6)->_S6_un._S6_u8[4] = (v4)->_S_un._S_un_b.s_b3, \ }
{	(v6)->_S6_un._S6_u8[5] = (v4)->_S_un._S_un_b.s_b4, \ }
{	(v6)->_S6_un._S6_u8[6] = 0, \ }
{	(v6)->_S6_un._S6_u8[7] = 0, \ }
{	(v6)->_S6_un._S6_u32[2] = 0, \ }
{	(v6)->_S6_un._S6_u32[3] = 0x01000000U) }
{#endif /* _BIG_ENDIAN */ }
{
 * IN6_ARE_ADDR_EQUAL (defined in RFC2292)
 *	 Compares if IPv6 addresses are equal.
 * Note: Compares in order of high likelyhood of a miss so we minimize
 * compares. (Current heuristic order, compare in reverse order of
 * uint32_t units)
 *
 * bool  IN6_ARE_ADDR_EQUAL(const struct in6_addr *,
 *			    const struct in6_addr *);
  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
//function IN6_ARE_ADDR_EQUAL(addr1,addr2 : longint) : longint;

{
 * IN6_ARE_PREFIXEDADDR_EQUAL (not defined in RFCs)
 *	Compares if prefixed parts of IPv6 addresses are equal.
 *
 * uint32_t IN6_MASK_FROM_PREFIX(int, int);
 * bool     IN6_ARE_PREFIXEDADDR_EQUAL(const struct in6_addr *,
 *				       const struct in6_addr *,
 *				       int);
  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
//function IN6_MASK_FROM_PREFIX(qoctet,prefix : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
//function IN6_ARE_PREFIXEDADDR_EQUAL(addr1,addr2,prefix : longint) : longint;

//{$endif}
{ !defined(_XPG4_2) || defined(_XPG6) || defined(__EXTENSIONS__)  }
{
 * Options for use with [gs]etsockopt at the IP level.
 *
 * Note: Some of the IP_ namespace has conflict with and
 * and is exposed through <xti.h>. (It also requires exposing
 * options not implemented). The options with potential
 * for conflicts use #ifndef guards.
  }
{$ifndef IP_OPTIONS}
{ set/get IP per-packet options    }

const
  IP_OPTIONS = 1;
{$endif}
{ int; header is included with data (raw)  }

const
  IP_HDRINCL = 2;
{$ifndef IP_TOS}
{ int; IP type of service and precedence  }

const
  IP_TOS = 3;
{$endif}
{$ifndef IP_TTL}
{ int; IP time to live  }

const
  IP_TTL = 4;
{$endif}
{ int; receive all IP options w/datagram  }

const
  IP_RECVOPTS = $5;
{ int; receive IP options for response  }
  IP_RECVRETOPTS = $6;
{ int; receive IP dst addr w/datagram  }
  IP_RECVDSTADDR = $7;
{ ip_opts; set/get IP per-packet options  }
  IP_RETOPTS = $8;
{ int; receive the inbound interface index  }
  IP_RECVIF = $9;
{ sockaddr_dl; get source link layer address  }
  IP_RECVSLLA = $a;
{ uint8_t; get TTL for inbound packet  }
  IP_RECVTTL = $b;
{ set/get IP multicast interface   }
  IP_MULTICAST_IF = $10;
{ set/get IP multicast timetolive  }
  IP_MULTICAST_TTL = $11;
{ set/get IP multicast loopback    }
  IP_MULTICAST_LOOP = $12;
{ add	an IP group membership	    }
  IP_ADD_MEMBERSHIP = $13;
{ drop an IP group membership	    }
  IP_DROP_MEMBERSHIP = $14;
{ block   mcast pkts from source   }
  IP_BLOCK_SOURCE = $15;
{ unblock mcast pkts from source   }
  IP_UNBLOCK_SOURCE = $16;
{ add  mcast group/source pair	    }
  IP_ADD_SOURCE_MEMBERSHIP = $17;
{ drop mcast group/source pair	    }
  IP_DROP_SOURCE_MEMBERSHIP = $18;
{ send directly to next hop	    }
  IP_NEXTHOP = $19;
{
 * IP_PKTINFO and IP_RECVPKTINFO have same value. Size of argument passed in
 * is used to differentiate b/w the two.
  }
{ specify src address and/or index  }
  IP_PKTINFO = $1a;
{ recv dest/matched addr and index  }
  IP_RECVPKTINFO = $1a;
{ don't fragment packets  }
  IP_DONTFRAG = $1b;
//{$if !defined(_XPG4_2) || defined(__EXTENSIONS__)}
{
 * Different preferences that can be requested from IPSEC protocols.
  }
{ Used to set IPSEC options  }

const
  IP_SEC_OPT = $22;
  IPSEC_PREF_NEVER = $01;
  IPSEC_PREF_REQUIRED = $02;
  IPSEC_PREF_UNIQUE = $04;
{
 * This can be used with the setsockopt() call to set per socket security
 * options. When the application uses per-socket API, we will reflect
 * the request on both outbound and inbound packets.
  }
{ AH request  }
{ ESP request  }
{ Self-Encap request  }
{ Auth algs for AH  }
{ Encr algs for ESP  }
{ Auth algs for ESP  }

type
  ipsec_req = record
      ipsr_ah_req : uint_t;
      ipsr_esp_req : uint_t;
      ipsr_self_encap_req : uint_t;
      ipsr_auth_alg : uint8_t;
      ipsr_esp_alg : uint8_t;
      ipsr_esp_auth_alg : uint8_t;
    end;
  ipsec_req_t = ipsec_req;
{
 * MCAST_* options are protocol-independent.  The actual definitions
 * are with the v6 options below; this comment is here to note the
 * namespace usage.
 *
 * #define	MCAST_JOIN_GROUP	0x29
 * #define	MCAST_LEAVE_GROUP	0x2a
 * #define	MCAST_BLOCK_SOURCE	0x2b
 * #define	MCAST_UNBLOCK_SOURCE	0x2c
 * #define	MCAST_JOIN_SOURCE_GROUP	0x2d
 * #define	MCAST_LEAVE_SOURCE_GROUP 0x2e
  }
//{$endif}
{ !defined(_XPG4_2) || defined(__EXTENSIONS__)  }
{
 * SunOS private (potentially not portable) IP_ option names
  }
{ bind socket to an ifindex	    }

const
  IP_BOUND_IF = $41;
{ use unspecified source address   }
  IP_UNSPEC_SRC = $42;
{ use specific TTL for broadcast   }
  IP_BROADCAST_TTL = $43;
{ can be reused		0x44  }
{ accept all unicast DHCP traffic  }
  IP_DHCPINIT_IF = $45;
{
 * Option values and names (when !_XPG5) shared with <xti_inet.h>
  }
{$ifndef IP_REUSEADDR}

const
  IP_REUSEADDR = $104;
{$endif}
{$ifndef IP_DONTROUTE}

const
  IP_DONTROUTE = $105;
{$endif}
{$ifndef IP_BROADCAST}

const
  IP_BROADCAST = $106;
{$endif}
{
 * The following option values are reserved by <xti_inet.h>
 *
 * T_IP_OPTIONS	0x107	 -  IP per-packet options
 * T_IP_TOS	0x108	 -  IP per packet type of service
  }
{
 * Default value constants for multicast attributes controlled by
 * IP*_MULTICAST_LOOP and IP*_MULTICAST_TTL,HOPS options.
  }
{ normally limit m'casts to 1 hop  }

const
  IP_DEFAULT_MULTICAST_TTL = 1;
{ normally hear sends if a member  }
  IP_DEFAULT_MULTICAST_LOOP = 1;
//{$if !defined(_XPG4_2) || defined(__EXTENSIONS__)}
{
 * Argument structure for IP_ADD_MEMBERSHIP and IP_DROP_MEMBERSHIP.
  }
{ IP multicast address of group  }
{ local IP address of interface  }

type
  ip_mreq = record
      imr_multiaddr : in_addr;
      imr_interface : in_addr;
    end;

{
 * Argument structure for IP_BLOCK_SOURCE, IP_UNBLOCK_SOURCE,
 * IP_ADD_SOURCE_MEMBERSHIP, and IP_DROP_SOURCE_MEMBERSHIP.
  }
{ IP address of group  }
{ IP address of source  }
{ IP address of interface  }
  ip_mreq_source = record
      imr_multiaddr : in_addr;
      imr_sourceaddr : in_addr;
      imr_interface : in_addr;
    end;

{
 * Argument structure for IPV6_JOIN_GROUP and IPV6_LEAVE_GROUP on
 * IPv6 addresses.
  }
{ IPv6 multicast addr  }
{ interface index  }
  ipv6_mreq = record
      ipv6mr_multiaddr : in6_addr;
      ipv6mr_interface : dword;
    end;

{
 * Use #pragma pack() construct to force 32-bit alignment on amd64.
 * This is needed to keep the structure size and offsets consistent
 * between a 32-bit app and the 64-bit amd64 kernel in structures
 * where 64-bit alignment would create gaps (in this case, structures
 * which have a uint32_t followed by a struct sockaddr_storage).
  }
//{$if _LONG_LONG_ALIGNMENT == 8 && _LONG_LONG_ALIGNMENT_32 == 4}
(** unsupported pragma#pragma pack(4)*)
//{$endif}
{
 * Argument structure for MCAST_JOIN_GROUP and MCAST_LEAVE_GROUP.
  }
{ interface index  }
{ group address  }

type
  group_req = record
      gr_interface : uint32_t;
      gr_group : sockaddr_storage;
    end;

{
 * Argument structure for MCAST_BLOCK_SOURCE, MCAST_UNBLOCK_SOURCE,
 * MCAST_JOIN_SOURCE_GROUP, MCAST_LEAVE_SOURCE_GROUP.
  }
{ interface index  }
{ group address  }
{ source address  }
  group_source_req = record
      gsr_interface : uint32_t;
      gsr_group : sockaddr_storage;
      gsr_source : sockaddr_storage;
    end;

{
 * Argument for SIOC[GS]MSFILTER ioctls
  }
{ interface index  }
{ multicast address  }
{ filter mode  }
{ number of sources  }
{ source address  }
  group_filter = record
      gf_interface : uint32_t;
      gf_group : sockaddr_storage;
      gf_fmode : uint32_t;
      gf_numsrc : uint32_t;
      gf_slist : array[0..0] of sockaddr_storage;
    end;

//{$if _LONG_LONG_ALIGNMENT == 8 && _LONG_LONG_ALIGNMENT_32 == 4}
(** unsupported pragma#pragma pack()*)
//{$endif}
{#define	GROUP_FILTER_SIZE(numsrc) \ }
{	(sizeof (struct group_filter) - sizeof (struct sockaddr_storage) \ }
{	+ (numsrc) * sizeof (struct sockaddr_storage)) }
{
 * Argument for SIOC[GS]IPMSFILTER ioctls (IPv4-specific)
  }
{ IP multicast address of group  }
{ local IP address of interface  }
{ filter mode  }
{ number of sources in src_list  }
{ start of source list  }

type
  ip_msfilter = record
      imsf_multiaddr : in_addr;
      imsf_interface : in_addr;
      imsf_fmode : uint32_t;
      imsf_numsrc : uint32_t;
      imsf_slist : array[0..0] of in_addr;
    end;

{#define	IP_MSFILTER_SIZE(numsrc) \ }
{	(sizeof (struct ip_msfilter) - sizeof (struct in_addr) \ }
{	+ (numsrc) * sizeof (struct in_addr)) }
{
 * Multicast source filter manipulation functions in libsocket;
 * defined in RFC 3678.
  }

function setsourcefilter(_para1:longint; _para2:uint32_t; _para3:Psockaddr; _para4:socklen_t; _para5:uint32_t;_para6:uint_t; _para7:Psockaddr_storage):longint;cdecl;external External_library name 'setsourcefilter';
function getsourcefilter(_para1:longint; _para2:uint32_t; _para3:Psockaddr; _para4:socklen_t; _para5:Puint32_t;_para6:Puint_t; _para7:Psockaddr_storage):longint;cdecl;external External_library name 'getsourcefilter';
function setipv4sourcefilter(_para1:longint; _para2:in_addr; _para3:in_addr; _para4:uint32_t; _para5:uint32_t;_para6:Pin_addr):longint;cdecl;external External_library name 'setipv4sourcefilter';
function getipv4sourcefilter(_para1:longint; _para2:in_addr; _para3:in_addr; _para4:Puint32_t; _para5:Puint32_t;_para6:Pin_addr):longint;cdecl;external External_library name 'getipv4sourcefilter';

{
 * Definitions needed for [gs]etsourcefilter(), [gs]etipv4sourcefilter()
  }
const
  MCAST_INCLUDE = 1;
  MCAST_EXCLUDE = 2;
{
 * Argument struct for IP_PKTINFO option
  }
{ send/recv interface index  }
{ matched source address  }
{ src/dst address in IP hdr  }

type
  in_pktinfo = record
      ipi_ifindex : dword;
      ipi_spec_dst : in_addr;
      ipi_addr : in_addr;
    end;
  in_pktinfo_t = in_pktinfo;
{
 * Argument struct for IPV6_PKTINFO option
  }
{ src/dst IPv6 address  }
{ send/recv interface index  }
  in6_pktinfo = record
      ipi6_addr : in6_addr;
      ipi6_ifindex : dword;
    end;

{
 * Argument struct for IPV6_MTUINFO option
  }
{ dst address including zone ID  }
{ path MTU in host byte order  }
  ip6_mtuinfo = record
      ip6m_addr : sockaddr_in6;
      ip6m_mtu : uint32_t;
    end;

{
 * IPv6 routing header types
  }

const
  IPV6_RTHDR_TYPE_0 = 0;

function inet6_rth_space(_type:longint; segments:longint):socklen_t;cdecl;external External_library name 'inet6_rth_space';

function inet6_rth_init(bp:pointer; bp_len:socklen_t; _type:longint; segments:longint):pointer;cdecl;external External_library name 'inet6_rth_init';

(* Const before type ignored *)
function inet6_rth_add(bp:pointer; addr:Pin6_addr):longint;cdecl;external External_library name 'inet6_rth_add';

(* Const before type ignored *)
function inet6_rth_reverse(inp:pointer; outp:pointer):longint;cdecl;external External_library name 'inet6_rth_reverse';

(* Const before type ignored *)
function inet6_rth_segments(bp:pointer):longint;cdecl;external External_library name 'inet6_rth_segments';

(* Const before type ignored *)
function inet6_rth_getaddr(bp:pointer; index:longint):Pin6_addr;cdecl;external External_library name 'inet6_rth_getaddr';

function inet6_opt_init(extbuf:pointer; extlen:socklen_t):longint;cdecl;external External_library name 'inet6_opt_init';

function inet6_opt_append(extbuf:pointer; extlen:socklen_t; offset:longint; _type:uint8_t; len:socklen_t;
           align:uint_t; databufp:Ppointer):longint;cdecl;external External_library name 'inet6_opt_append';

function inet6_opt_finish(extbuf:pointer; extlen:socklen_t; offset:longint):longint;cdecl;external External_library name 'inet6_opt_finish';

function inet6_opt_set_val(databuf:pointer; offset:longint; val:pointer; vallen:socklen_t):longint;cdecl;external External_library name 'inet6_opt_set_val';

function inet6_opt_next(extbuf:pointer; extlen:socklen_t; offset:longint; typep:Puint8_t; lenp:Psocklen_t;
           databufp:Ppointer):longint;cdecl;external External_library name 'inet6_opt_next';

function inet6_opt_find(extbufp:pointer; extlen:socklen_t; offset:longint; _type:uint8_t; lenp:Psocklen_t;
           databufp:Ppointer):longint;cdecl;external External_library name 'inet6_opt_find';

function inet6_opt_get_val(databuf:pointer; offset:longint; val:pointer; vallen:socklen_t):longint;cdecl;external External_library name 'inet6_opt_get_val';

//{$endif}
{ !defined(_XPG4_2) || defined(__EXTENSIONS__)  }
{
 * Argument structure for IP_ADD_PROXY_ADDR.
 * Note that this is an unstable, experimental interface. It may change
 * later. Don't use it unless you know what it is.
  }

type
  in_prefix_t = record
      in_prefix_addr : in_addr;
      in_prefix_len : dword;
    end;
//{$if !defined(_XPG4_2) || defined(_XPG6) || defined(__EXTENSIONS__)}
{
 * IPv6 options
  }
{ hop limit value for unicast  }

const
  IPV6_UNICAST_HOPS = $5;
{ packets.  }
{ argument type: uint_t  }
{ outgoing interface for  }
  IPV6_MULTICAST_IF = $6;
{ multicast packets.  }
{ argument type: struct in6_addr  }
{ hop limit value to use for  }
  IPV6_MULTICAST_HOPS = $7;
{ multicast packets.  }
{ argument type: uint_t  }
{ enable/disable delivery of  }
  IPV6_MULTICAST_LOOP = $8;
{ multicast packets on same socket.  }
{ argument type: uint_t  }
{ join an IPv6 multicast group.  }
  IPV6_JOIN_GROUP = $9;
{ argument type: struct ipv6_mreq  }
{ leave an IPv6 multicast group  }
  IPV6_LEAVE_GROUP = $a;
{ argument type: struct ipv6_mreq  }
{
 * Other XPG6 constants.
  }
{ max len IPv4 addr in ascii dotted  }
  INET_ADDRSTRLEN = 16;
{ decimal notation.  }
{ max len of IPv6 addr in ascii  }
  INET6_ADDRSTRLEN = 46;
{ standard colon-hex notation.  }
//{$endif}
//{ !defined(_XPG4_2) || defined(_XPG6) || defined(__EXTENSIONS__)  }
//{$if !defined(_XPG4_2) || defined(__EXTENSIONS__)}
{
 * IPV6_ADD_MEMBERSHIP and IPV6_DROP_MEMBERSHIP are being kept
 * for backward compatibility. They have the same meaning as IPV6_JOIN_GROUP
 * and IPV6_LEAVE_GROUP respectively.
  }
{ join an IPv6 multicast group.  }

const
  IPV6_ADD_MEMBERSHIP = $9;
{ argument type: struct ipv6_mreq  }
{ leave an IPv6 multicast group  }
  IPV6_DROP_MEMBERSHIP = $a;
{ argument type: struct ipv6_mreq  }
{ addr plus interface index  }
  IPV6_PKTINFO = $b;
{ arg type: "struct in6_pktingo" -  }
{ hoplimit for datagram  }
  IPV6_HOPLIMIT = $c;
{ next hop address   }
  IPV6_NEXTHOP = $d;
{ hop by hop options  }
  IPV6_HOPOPTS = $e;
{ destination options - after  }
  IPV6_DSTOPTS = $f;
{ the routing header  }
{ routing header   }
  IPV6_RTHDR = $10;
{ destination options - before  }
  IPV6_RTHDRDSTOPTS = $11;
{ the routing header  }
{ enable/disable IPV6_PKTINFO  }
  IPV6_RECVPKTINFO = $12;
{ enable/disable IPV6_HOPLIMIT  }
  IPV6_RECVHOPLIMIT = $13;
{ enable/disable IPV6_HOPOPTS  }
  IPV6_RECVHOPOPTS = $14;
{
 * This options exists for backwards compatability and should no longer be
 * used.  Use IPV6_RECVDSTOPTS instead.
  }
  _OLD_IPV6_RECVDSTOPTS = $15;
{ enable/disable IPV6_RTHDR  }
  IPV6_RECVRTHDR = $16;
{
 * enable/disable IPV6_RTHDRDSTOPTS.  Now obsolete.  IPV6_RECVDSTOPTS enables
 * the receipt of both headers.
  }
  IPV6_RECVRTHDRDSTOPTS = $17;
{ Control checksum on raw sockets  }
  IPV6_CHECKSUM = $18;
{ enable/disable IPV6_CLASS  }
  IPV6_RECVTCLASS = $19;
{ send packets with minimum MTU  }
  IPV6_USE_MIN_MTU = $20;
{ don't fragment packets  }
  IPV6_DONTFRAG = $21;
{ Used to set IPSEC options  }
  IPV6_SEC_OPT = $22;
{ Control socket's src addr select  }
  IPV6_SRC_PREFERENCES = $23;
{ receive PMTU info  }
  IPV6_RECVPATHMTU = $24;
{ get the PMTU  }
  IPV6_PATHMTU = $25;
{ traffic class  }
  IPV6_TCLASS = $26;
{ v6 only socket option  }
  IPV6_V6ONLY = $27;
{
 * enable/disable receipt of both both IPV6_DSTOPTS headers.
  }
  IPV6_RECVDSTOPTS = $28;
{
 * protocol-independent multicast membership options.
  }
{ join group for all sources  }
  MCAST_JOIN_GROUP = $29;
{ leave group  }
  MCAST_LEAVE_GROUP = $2a;
{ block specified source  }
  MCAST_BLOCK_SOURCE = $2b;
{ unblock specified source  }
  MCAST_UNBLOCK_SOURCE = $2c;
{ join group for specified source  }
  MCAST_JOIN_SOURCE_GROUP = $2d;
{ leave source/group pair  }
  MCAST_LEAVE_SOURCE_GROUP = $2e;
{ 32Bit field for IPV6_SRC_PREFERENCES  }
  IPV6_PREFER_SRC_HOME = $00000001;
  IPV6_PREFER_SRC_COA = $00000002;
  IPV6_PREFER_SRC_PUBLIC = $00000004;
  IPV6_PREFER_SRC_TMP = $00000008;
  IPV6_PREFER_SRC_NONCGA = $00000010;
  IPV6_PREFER_SRC_CGA = $00000020;
  IPV6_PREFER_SRC_MIPMASK = IPV6_PREFER_SRC_HOME or IPV6_PREFER_SRC_COA;
  IPV6_PREFER_SRC_MIPDEFAULT = IPV6_PREFER_SRC_HOME;
  IPV6_PREFER_SRC_TMPMASK = IPV6_PREFER_SRC_PUBLIC or IPV6_PREFER_SRC_TMP;
  IPV6_PREFER_SRC_TMPDEFAULT = IPV6_PREFER_SRC_PUBLIC;
  IPV6_PREFER_SRC_CGAMASK = IPV6_PREFER_SRC_NONCGA or IPV6_PREFER_SRC_CGA;
  IPV6_PREFER_SRC_CGADEFAULT = IPV6_PREFER_SRC_NONCGA;
  IPV6_PREFER_SRC_MASK = (IPV6_PREFER_SRC_MIPMASK or IPV6_PREFER_SRC_TMPMASK) or IPV6_PREFER_SRC_CGAMASK;
  IPV6_PREFER_SRC_DEFAULT = (IPV6_PREFER_SRC_MIPDEFAULT or IPV6_PREFER_SRC_TMPDEFAULT) or IPV6_PREFER_SRC_CGADEFAULT;
{
 * SunOS private (potentially not portable) IPV6_ option names
  }
{ bind to an ifindex  }
  IPV6_BOUND_IF = $41;
{ source of packets set to  }
  IPV6_UNSPEC_SRC = $42;
{ unspecified (all zeros)  }
{
 * Miscellaneous IPv6 constants.
  }
{ pad byte in IPv6 extension hdrs  }
  IPV6_PAD1_OPT = 0;
//{$endif}
{ !defined(_XPG4_2) || defined(__EXTENSIONS__)  }
{
 * Extern declarations for pre-defined global const variables
  }
//{$if !defined(_XPG4_2) || defined(_XPG6) || defined(__EXTENSIONS__)}
{$ifndef _KERNEL}
//{$ifdef __STDC__}
(* Const before type ignored *)

  var
    in6addr_any : in6_addr;cvar;external;
(* Const before type ignored *)
    in6addr_loopback : in6_addr;cvar;external;
{$else}

  var
    in6addr_any : in6_addr;cvar;external;
    in6addr_loopback : in6_addr;cvar;external;
{$endif}

{ END ----------------------------------- }
{ END in.h from uts/netinet/in.h }
{ END----------------------------------- }




{ ----------------------------------- }
{ mac_flow from uts/common/mac_flow.h }
{ ----------------------------------- }
const
  MAX_RINGS_PER_GROUP = 128;

{
 * MAXFLOWNAMELEN defines the longest possible permitted flow name,
 * including the terminating NUL.
  }
  MAXFLOWNAMELEN = 128;
{ need to use MAXMACADDRLEN from dld.h instead of this one  }
  MAXMACADDR = 20;
{ Bit-mask for the selectors carried in the flow descriptor  }

type
  flow_mask_t = uint64_t;
{ Destination MAC addr  }

const
  FLOW_LINK_DST = $00000001;
{ Source MAC address  }
  FLOW_LINK_SRC = $00000002;
{ VLAN ID  }
  FLOW_LINK_VID = $00000004;
{ SAP value  }
  FLOW_LINK_SAP = $00000008;
{ V4 or V6  }
  FLOW_IP_VERSION = $00000010;
{ Protocol type  }
  FLOW_IP_PROTOCOL = $00000020;
{ Local address  }
  FLOW_IP_LOCAL = $00000040;
{ Remote address  }
  FLOW_IP_REMOTE = $00000080;
{ DSfield value  }
  FLOW_IP_DSFIELD = $00000100;
{ ULP local port  }
  FLOW_ULP_PORT_LOCAL = $00001000;
{ ULP remote port  }
  FLOW_ULP_PORT_REMOTE = $00002000;

type
  flow_desc_s = record
      fd_mask : flow_mask_t;
      fd_mac_len : uint32_t;
      fd_dst_mac : array[0..(MAXMACADDR)-1] of uint8_t;
      fd_src_mac : array[0..(MAXMACADDR)-1] of uint8_t;
      fd_vid : uint16_t;
      fd_sap : uint32_t;
      fd_ipversion : uint8_t;
      fd_protocol : uint8_t;
      fd_local_addr : in6_addr_t;
      fd_local_netmask : in6_addr_t;
      fd_remote_addr : in6_addr_t;
      fd_remote_netmask : in6_addr_t;
      fd_local_port : in_port_t;
      fd_remote_port : in_port_t;
      fd_dsfield : uint8_t;
      fd_dsfield_mask : uint8_t;
    end;
  flow_desc_t = flow_desc_s;

//const
//  MRP_NCPUS = 128;
//{
// * In MCM_CPUS mode, cpu bindings is user specified. In MCM_FANOUT mode,
// * user only specifies a fanout count.
// * mc_rx_fanout_cnt gives the number of CPUs used for fanout soft rings.
// * mc_rx_fanout_cpus[] array stores the CPUs used for fanout soft rings.
//  }
//
//type
//  mac_cpu_mode_t = (MCM_FANOUT := 1,MCM_CPUS);
//{
// * Structure to store the value of the CPUs to be used to re-target
// * Tx interrupt.
//  }
//{ cpu value to re-target intr to  }
//{ re-targeted CPU or -1 if failed  }
//
//  mac_tx_intr_cpus_s = record
//      mtc_intr_cpu : array[0..(MRP_NCPUS)-1] of int32_t;
//      mtc_retargeted_cpu : array[0..(MRP_NCPUS)-1] of int32_t;
//    end;
//  mac_tx_intr_cpu_t = mac_tx_intr_cpus_s;
//{ num of cpus  }
//{ cpu list  }
//{ soft ring cpu cnt  }
//{ SR cpu list  }
//{ poll thr binding  }
//{ worker thr binding  }
//{
//	 * interrupt cpu: mrp_intr_cpu less than 0 implies platform limitation
//	 * in retargetting the interrupt assignment.
//	  }
//{ fanout mode  }
//
//  mac_cpus_props_s = record
//      mc_ncpus : uint32_t;
//      mc_cpus : array[0..(MRP_NCPUS)-1] of uint32_t;
//      mc_rx_fanout_cnt : uint32_t;
//      mc_rx_fanout_cpus : array[0..(MRP_NCPUS)-1] of uint32_t;
//      mc_rx_pollid : uint32_t;
//      mc_rx_workerid : uint32_t;
//      mc_rx_intr_cpu : int32_t;
//      mc_tx_fanout_cpus : array[0..(MRP_NCPUS)-1] of int32_t;
//      mc_tx_intr_cpus : mac_tx_intr_cpu_t;
//      mc_fanout_mode : mac_cpu_mode_t;
//    end;
//  mac_cpus_t = mac_cpus_props_s;

//const
//  mc_tx_intr_cpu = mc_tx_intr_cpus.mtc_intr_cpu;
//  mc_tx_retargeted_cpu = mc_tx_intr_cpus.mtc_retargeted_cpu;
//{ Priority values  }
//
//type
//  mac_priority_level_t = (MPL_LOW,MPL_MEDIUM,MPL_HIGH,MPL_RESET);
{ Protection types  }

const
  MPL_LINK_DEFAULT = MPL_HIGH;
{ The default priority for flows  }
  MPL_SUBFLOW_DEFAULT = MPL_MEDIUM;
{ Limit set  }
  MRP_MAXBW = $00000001;
{ CPU/fanout set  }
  MRP_CPUS = $00000002;
{ CPU/fanout from user  }
  MRP_CPUS_USERSPEC = $00000004;
{ Priority set  }
  MRP_PRIORITY = $00000008;
{ Protection set  }
  MRP_PROTECT = $00000010;
{ Rx rings  }
  MRP_RX_RINGS = $00000020;
{ Tx rings  }
  MRP_TX_RINGS = $00000040;
{ unspecified rings  }
  MRP_RXRINGS_UNSPEC = $00000080;
{ unspecified rings  }
  MRP_TXRINGS_UNSPEC = $00000100;
{ resetting rings  }
  MRP_RINGS_RESET = $00000200;
{ CPU pool  }
  MRP_POOL = $00000400;
  MRP_THROTTLE = MRP_MAXBW;
{ 3 levels - low, medium, high  }
  MRP_PRIORITY_LEVELS = 3;
{ Special value denoting no bandwidth control  }
  MRP_MAXBW_RESETVAL = -(1);
{
 * Until sub-megabit limit is implemented,
 * reject values lower than 1 MTU per tick or 1.2Mbps
  }
  MRP_MAXBW_MINVAL = 1200000;
{
	 * Bit-mask for the network resource control types types
	  }
{ bandwidth limit in bps  }
{ relative flow priority  }
{ CPU pool  }
  //mac_resource_props_s = record
  //    mrp_mask : uint32_t;
  //    mrp_maxbw : uint64_t;
  //    mrp_priority : mac_priority_level_t;
  //    mrp_cpus : mac_cpus_t;
  //    mrp_protect : mac_protect_t;
  //    mrp_nrxrings : uint32_t;
  //    mrp_ntxrings : uint32_t;
  //    mrp_pool : array[0..(MAXPATHLEN)-1] of char;
  //  end;
  //mac_resource_props_t = mac_resource_props_s;

//const
//  mrp_ncpus = mrp_cpus.mc_ncpus;
//  mrp_cpu = mrp_cpus.mc_cpus;
//  mrp_rx_fanout_cnt = mrp_cpus.mc_rx_fanout_cnt;
//  mrp_rx_pollid = mrp_cpus.mc_rx_pollid;
//  mrp_rx_workerid = mrp_cpus.mc_rx_workerid;
//  mrp_rx_intr_cpu = mrp_cpus.mc_rx_intr_cpu;
//  mrp_fanout_mode = mrp_cpus.mc_fanout_mode;

{ END ----------------------------------- }
{ END mac_flow from uts/common/mac_flow.h }
{ END ----------------------------------- }


implementation


{  ----------------------------------- }
{  in.h macros }
{  ----------------------------------- }

//function IN_CLASSA(i : longint) : longint;
//begin
//  IN_CLASSA:=(i(@($80000000)))=0;
//end;
//
//function IN_CLASSB(i : longint) : longint;
//begin
//  IN_CLASSB:=(i(@($c0000000)))=$80000000;
//end;
//
//function IN_CLASSC(i : longint) : longint;
//begin
//  IN_CLASSC:=(i(@($e0000000)))=$c0000000;
//end;
//
//function IN_CLASSD(i : longint) : longint;
//begin
//  IN_CLASSD:=(i(@($f0000000)))=$e0000000;
//end;
//
//function IN_CLASSE(i : longint) : longint;
//begin
//  IN_CLASSE:=(i(@($f0000000)))=$f0000000;
//end;
//
//function IN_MULTICAST(i : longint) : longint;
//begin
//  IN_MULTICAST:=IN_CLASSD(i);
//end;
//
//function IN_EXPERIMENTAL(i : longint) : longint;
//begin
//  IN_EXPERIMENTAL:=(i(@($e0000000)))=$e0000000;
//end;
//
//function IN_BADCLASS(i : longint) : longint;
//begin
//  IN_BADCLASS:=(i(@($f0000000)))=$f0000000;
//end;
//
//function IN_LINKLOCAL(i : longint) : longint;
//begin
//  IN_LINKLOCAL:=(i(@(IN_AUTOCONF_MASK)))=IN_AUTOCONF_NET;
//end;
//
//function IN6_IS_ADDR_UNSPECIFIED(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_UNSPECIFIED:=((((addr^.(_S6_un.(_S6_u32[3])))=0) and (@((addr^.(_S6_un.(_S6_u32[2])))=0))) and (@((addr^.(_S6_un.(_S6_u32[1])))=0))) and (@((addr^.(_S6_un.(_S6_u32[0])))=0));
//end;
//
//function IN6_IS_ADDR_LOOPBACK(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_LOOPBACK:=((((addr^.(_S6_un.(_S6_u32[3])))=$00000001) and (@((addr^.(_S6_un.(_S6_u32[2])))=0))) and (@((addr^.(_S6_un.(_S6_u32[1])))=0))) and (@((addr^.(_S6_un.(_S6_u32[0])))=0));
//end;
//
//function IN6_IS_ADDR_LOOPBACK(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_LOOPBACK:=((((addr^.(_S6_un.(_S6_u32[3])))=$01000000) and (@((addr^.(_S6_un.(_S6_u32[2])))=0))) and (@((addr^.(_S6_un.(_S6_u32[1])))=0))) and (@((addr^.(_S6_un.(_S6_u32[0])))=0));
//end;
//
//function IN6_IS_ADDR_MULTICAST(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MULTICAST:=((addr^.(_S6_un.(_S6_u32[0]))) and $ff000000)=$ff000000;
//end;
//
//function IN6_IS_ADDR_MULTICAST(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MULTICAST:=((addr^.(_S6_un.(_S6_u32[0]))) and $000000ff)=$000000ff;
//end;
//
//function IN6_IS_ADDR_LINKLOCAL(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_LINKLOCAL:=((addr^.(_S6_un.(_S6_u32[0]))) and $ffc00000)=$fe800000;
//end;
//
//function IN6_IS_ADDR_LINKLOCAL(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_LINKLOCAL:=((addr^.(_S6_un.(_S6_u32[0]))) and $0000c0ff)=$000080fe;
//end;
//
//function IN6_IS_ADDR_SITELOCAL(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_SITELOCAL:=((addr^.(_S6_un.(_S6_u32[0]))) and $ffc00000)=$fec00000;
//end;
//
//function IN6_IS_ADDR_SITELOCAL(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_SITELOCAL:=((addr^.(_S6_un.(_S6_u32[0]))) and $0000c0ff)=$0000c0fe;
//end;
//
//function IN6_IS_ADDR_V4MAPPED(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_V4MAPPED:=(((addr^.(_S6_un.(_S6_u32[2])))=$0000ffff) and (@((addr^.(_S6_un.(_S6_u32[1])))=0))) and (@((addr^.(_S6_un.(_S6_u32[0])))=0));
//end;
//
//function IN6_IS_ADDR_V4MAPPED(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_V4MAPPED:=(((addr^.(_S6_un.(_S6_u32[2])))=$ffff0000) and (@((addr^.(_S6_un.(_S6_u32[1])))=0))) and (@((addr^.(_S6_un.(_S6_u32[0])))=0));
//end;
//
//function IN6_IS_ADDR_V4MAPPED_ANY(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_V4MAPPED_ANY:=((((addr^.(_S6_un.(_S6_u32[3])))=0) and (@((addr^.(_S6_un.(_S6_u32[2])))=$0000ffff))) and (@((addr^.(_S6_un.(_S6_u32[1])))=0))) and (@((addr^.(_S6_un.(_S6_u32[0])))=0));
//end;
//
//function IN6_IS_ADDR_V4MAPPED_ANY(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_V4MAPPED_ANY:=((((addr^.(_S6_un.(_S6_u32[3])))=0) and (@((addr^.(_S6_un.(_S6_u32[2])))=$ffff0000))) and (@((addr^.(_S6_un.(_S6_u32[1])))=0))) and (@((addr^.(_S6_un.(_S6_u32[0])))=0));
//end;
//
//function IN6_IS_ADDR_V4COMPAT(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_V4COMPAT:=(((((addr^.(_S6_un.(_S6_u32[2])))=0) and (@((addr^.(_S6_un.(_S6_u32[1])))=0))) and (@((addr^.(_S6_un.(_S6_u32[0])))=0))) and (@( not ((addr^.(_S6_un.(_S6_u32[3])))=0)))) and (@( not ((addr^.(_S6_un.(_S6_u32[3])))=$00000001)));
//end;
//
//function IN6_IS_ADDR_V4COMPAT(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_V4COMPAT:=(((((addr^.(_S6_un.(_S6_u32[2])))=0) and (@((addr^.(_S6_un.(_S6_u32[1])))=0))) and (@((addr^.(_S6_un.(_S6_u32[0])))=0))) and (@( not ((addr^.(_S6_un.(_S6_u32[3])))=0)))) and (@( not ((addr^.(_S6_un.(_S6_u32[3])))=$01000000)));
//end;
//
//function IN6_IS_ADDR_MC_RESERVED(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MC_RESERVED:=((addr^.(_S6_un.(_S6_u32[0]))) and $ff0f0000)=$ff000000;
//end;
//
//function IN6_IS_ADDR_MC_RESERVED(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MC_RESERVED:=((addr^.(_S6_un.(_S6_u32[0]))) and $00000fff)=$000000ff;
//end;
//
//function IN6_IS_ADDR_MC_NODELOCAL(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MC_NODELOCAL:=((addr^.(_S6_un.(_S6_u32[0]))) and $ff0f0000)=$ff010000;
//end;
//
//function IN6_IS_ADDR_MC_NODELOCAL(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MC_NODELOCAL:=((addr^.(_S6_un.(_S6_u32[0]))) and $00000fff)=$000001ff;
//end;
//
//function IN6_IS_ADDR_MC_LINKLOCAL(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MC_LINKLOCAL:=((addr^.(_S6_un.(_S6_u32[0]))) and $ff0f0000)=$ff020000;
//end;
//
//function IN6_IS_ADDR_MC_LINKLOCAL(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MC_LINKLOCAL:=((addr^.(_S6_un.(_S6_u32[0]))) and $00000fff)=$000002ff;
//end;
//
//function IN6_IS_ADDR_MC_SITELOCAL(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MC_SITELOCAL:=((addr^.(_S6_un.(_S6_u32[0]))) and $ff0f0000)=$ff050000;
//end;
//
//function IN6_IS_ADDR_MC_SITELOCAL(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MC_SITELOCAL:=((addr^.(_S6_un.(_S6_u32[0]))) and $00000fff)=$000005ff;
//end;
//
//function IN6_IS_ADDR_MC_ORGLOCAL(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MC_ORGLOCAL:=((addr^.(_S6_un.(_S6_u32[0]))) and $ff0f0000)=$ff080000;
//end;
//
//function IN6_IS_ADDR_MC_ORGLOCAL(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MC_ORGLOCAL:=((addr^.(_S6_un.(_S6_u32[0]))) and $00000fff)=$000008ff;
//end;
//
//function IN6_IS_ADDR_MC_GLOBAL(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MC_GLOBAL:=((addr^.(_S6_un.(_S6_u32[0]))) and $ff0f0000)=$ff0e0000;
//end;
//
//function IN6_IS_ADDR_MC_GLOBAL(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MC_GLOBAL:=((addr^.(_S6_un.(_S6_u32[0]))) and $00000fff)=$00000eff;
//end;
//
//function IN6_IS_ADDR_MC_SOLICITEDNODE(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MC_SOLICITEDNODE:=((((addr^.(_S6_un.(_S6_u32[0])))=$ff020000) and (@((addr^.(_S6_un.(_S6_u32[1])))=$00000000))) and (@((addr^.(_S6_un.(_S6_u32[2])))=$00000001))) and (@(((addr^.(_S6_un.(_S6_u32[3]))) and $ff000000)=$ff000000));
//end;
//
//function IN6_IS_ADDR_MC_SOLICITEDNODE(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_MC_SOLICITEDNODE:=((((addr^.(_S6_un.(_S6_u32[0])))=$000002ff) and (@((addr^.(_S6_un.(_S6_u32[1])))=$00000000))) and (@((addr^.(_S6_un.(_S6_u32[2])))=$01000000))) and (@(((addr^.(_S6_un.(_S6_u32[3]))) and $000000ff)=$000000ff));
//end;
//
//function IN6_IS_ADDR_6TO4(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_6TO4:=((addr^.(_S6_un.(_S6_u32[0]))) and $ffff0000)=$20020000;
//end;
//
//function IN6_IS_ADDR_6TO4(addr : longint) : longint;
//begin
//  IN6_IS_ADDR_6TO4:=((addr^.(_S6_un.(_S6_u32[0]))) and $0000ffff)=$00000220;
//end;
//
//function IN6_ARE_6TO4_PREFIX_EQUAL(addr1,addr2 : longint) : longint;
//begin
//  IN6_ARE_6TO4_PREFIX_EQUAL:=(((addr1^.(_S6_un.(_S6_u32[0])))=(addr2^.(_S6_un.(_S6_u32[0])))) and (@((addr1^.(_S6_un.(_S6_u8[4])))=(addr2^.(_S6_un.(_S6_u8[4])))))) and (@((addr1^.(_S6_un.(_S6_u8[5])))=(addr2^.(_S6_un.(_S6_u8[5])))));
//end;
//
//function IN6_V4MAPPED_TO_INADDR(v6,v4 : longint) : longint;
//begin
//  IN6_V4MAPPED_TO_INADDR:=(v4^.s_addr):=(v6^.(_S6_un.(_S6_u32[3])));
//end;
//
//function IN6_V4MAPPED_TO_IPADDR(v6,v4 : longint) : longint;
//begin
//  IN6_V4MAPPED_TO_IPADDR:=v4:=(v6^.(_S6_un.(_S6_u32[3])));
//end;
//
//function IN6_ARE_ADDR_EQUAL(addr1,addr2 : longint) : longint;
//begin
//  IN6_ARE_ADDR_EQUAL:=((((addr1^.(_S6_un.(_S6_u32[3])))=(addr2^.(_S6_un.(_S6_u32[3])))) and (@((addr1^.(_S6_un.(_S6_u32[2])))=(addr2^.(_S6_un.(_S6_u32[2])))))) and (@((addr1^.(_S6_un.(_S6_u32[1])))=(addr2^.(_S6_un.(_S6_u32[1])))))) and (@((addr1^.(_S6_u
//end;
//
//function IN6_MASK_FROM_PREFIX(qoctet,prefix : longint) : longint;
//var
//   if_local1, if_local2 : longint;
//(* result types are not known *)
//begin
//  if (qoctet*32)>=prefix then
//    if_local1:=$00000000
//  else
//    if_local1:=$FFFFFFFF shl (((qoctet(+(1)))*32)-prefix);
//  if ((qoctet(+(1)))*32)<prefix then
//    if_local2:=$FFFFFFFF
//  else
//    if_local2:=if_local1;
//  IN6_MASK_FROM_PREFIX:=if_local2;
//end;
//
//function IN6_ARE_PREFIXEDADDR_EQUAL(addr1,addr2,prefix : longint) : longint;
//begin
//  IN6_ARE_PREFIXEDADDR_EQUAL:=(((((ntohl(addr1^.(_S6_un.(_S6_u32[0])))) and (IN6_MASK_FROM_PREFIX(0,prefix)))=((ntohl(addr2^.(_S6_un.(_S6_u32[0])))) and (IN6_MASK_FROM_PREFIX(0,prefix)))) and (@(((ntohl(addr1^.(_S6_un.(_S6_u32[1])))) and (IN6_MASK_FROM_
//end;

{ END ----------------------------------- }
{ END in.h macros }
{ END  ----------------------------------- }



end.

