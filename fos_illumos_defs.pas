unit fos_illumos_defs;

{
  This gathers headers needed for other interfaces:
  avl.h }

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
 * Copyright 2009 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
 * Copyright 2014 FirmOS Business Solutions GmbH
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,ctypes,unix,baseunix,unixtype;

const
  MAXUSERNAME       = 32;   { (sizeof (((struct utmpx *)0)->ut_name)) }
  ZONENAME_MAX      = 64;
  MAX_MNTOPT_STR    = 1024; { mount.h}
  FSTYPSZ           = 16;   { max size of fs identifier }
  INET6_ADDRSTRLEN  = 46;
  _LIFNAMSIZ	    = 32;
  LIFNAMSIZ	    = _LIFNAMSIZ; {kernel interface and ifgroup name size (same size)}
  LIFGRNAMSIZ       = LIFNAMSIZ;
  MAXNAMELEN        = 256;
  MAXPATHLEN        = 1024;
  BUFSIZ            = 4096; // ?
  MAXMACADDRLEN     = 20;
  UUID_PRINTABLE_STRING_LENGTH = 37;
  MS_FORCE          = $400; { forced unmount }

type
  {FOS Alias}
  boolean_t   = (B_FALSE,B_TRUE);
  int16_t     = cInt16;
  int32_t     = cint32;
  Hrtime_t    = cuint64;
  Uchar_t     = cuchar;
  Uint64_t    = cuint64;
  int64_t     = cint64;
  int8_t      = cint8;
  uint8_t     = cuint8;
  ulong_t     = culong;
  uint16_t    = cuint16;
  UInt32_t    = cuint32;
  uint_t      = cuint;
  cuintptr_t  = PtrUInt;


  Pboolean_t       = ^boolean_t;
  PPboolean_t      = ^Pboolean_t;
  Pdouble          = ^double;
  Phrtime_t        = ^hrtime_t;
  Pint16_t         = ^int16_t;
  Pint32_t         = ^int32_t;
  Pint64_t         = ^int64_t;
  PPint64_t        = ^Pint64_t;
  Pint8_t          = ^int8_t;
  PPcchar          = ^Pcchar;
  PPPcchar         = ^PPcchar;
  PPUchar_t        = ^Puchar_t;
  PPint8_t         = ^Pint8_t;

  Psize_t          = ^csize_t;
  Puchar_t         = ^uchar_t;
  Puint16_t        = ^uint16_t;
  PPuint16_t       = ^Puint16_t;
  Puint32_t        = ^uint32_t;
  PPuint32_t       = ^Puint32_t;
  Puint64_t        = ^uint64_t;
  PPuint64_t       = ^Puint64_t;
  Puint8_t         = ^uint8_t;
  PPuint8_t        = ^Puint8_t;
  Puint_t          = ^uint_t;
  PPint16_t        = ^Pint16_t;
  PPint32_t        = ^Pint32_t;

  uuid_t           = TGuid; {array[0..15] of Byte;}
  uu_avl_node_t    = record end;
  uu_avl_pool_t    = record end;
  uu_avl_t         = record end; { should be defined in  libuutil (userspace utils) }
  Puu_avl_pool_t   = ^uu_avl_pool_t;
  Puu_avl_t        = ^uu_avl_t;
  priv_set_t       = record end;
  Ppriv_set_t      = ^priv_set_t;
  rctlblk_t        = record end;
  Prctlblk_t       = ^rctlblk_t;
  id_t             = PtrUInt;
  zoneid_t         = id_t;
  Pzoneid_t        = ^zoneid_t;
  uid_t            = id_t;
  git_t            = uid_t;


type
  lifreq = record end; { currently defined opaque, usesd in illumos for ioctls}

  //libc interfaces
  function getuid              :uid_t;cdecl;external;
  function geteuid             :uid_t;cdecl;external;
  function getgid              :gid_t;cdecl;external;
  function getegid             :gid_t;cdecl;external;
  function getzoneid           :zoneid_t;cdecl;external;
  function getzoneidbyname     (name : Pchar):zoneid_t;cdecl;external;
  function getzonenamebyid     (id : zoneid_t; buf : pchar ; buflen : size_t):ssize_t;cdecl;external;
  function fos_getzonenamebyid (id : zoneid_t):string;

  Function  illu_malloc (Size : ptruint) : Pointer; cdecl; external name 'malloc';
  Procedure illu_Free (P : pointer); cdecl; external name 'free';
  function  illu_ReAlloc (P : Pointer; Size : ptruint) : pointer; cdecl; external name 'realloc';
  Function  illu_CAlloc (unitSize,UnitCount : ptruint) : pointer; cdecl; external name 'calloc';

  //libc nonpublic interfaces

  function zone_getattr        (zoneid   : zoneid_t;  attr : cint ; valp : Pointer ; size : size_t):ssize_t;cdecl;external;
  function zone_list           (zonelist : Pzoneid_t; var numzones:uint_t):cint;cdecl;external;
  //function zone_get_zonepath   ();

{ * Direction constants used for avl_nearest(). }
const
  AVL_BEFORE = 0;
  AVL_AFTER = 1;
  external_avl_library = 'avl'; // need to link in illumos(omnios) ?

Type
  Pavl_index_t  = ^avl_index_t;
  Pavl_tree_t  = ^avl_tree_t;


  avl_tree_t = record end;
  avl_node_t = record end;
  avl_index_t = cuintptr_t;

  TAVLCompar = function (_para1:pointer; _para2:pointer):cint ;cdecl;

procedure avl_create(tree:Pavl_tree_t; compar : TAVLCompar; size:size_t; offset:size_t);cdecl;external external_avl_library name 'avl_create';
function avl_find(tree:Pavl_tree_t; node:pointer; where:Pavl_index_t):pointer;cdecl;external external_avl_library name 'avl_find';
procedure avl_insert(tree:Pavl_tree_t; node:pointer; where:avl_index_t);cdecl;external external_avl_library name 'avl_insert';
procedure avl_insert_here(tree:Pavl_tree_t; new_data:pointer; here:pointer; direction:cint);cdecl;external external_avl_library name 'avl_insert_here';
function avl_first(tree:Pavl_tree_t):pointer;cdecl;external external_avl_library name 'avl_first';
function avl_last(tree:Pavl_tree_t):pointer;cdecl;external external_avl_library name 'avl_last';
function AVL_NEXT(tree,node : longint) : longint;
function AVL_PREV(tree,node : longint) : longint;
function avl_nearest(tree:Pavl_tree_t; where:avl_index_t; direction:cint):pointer;cdecl;external external_avl_library name 'avl_nearest';
procedure avl_add(tree:Pavl_tree_t; node:pointer);cdecl;external external_avl_library name 'avl_add';
procedure avl_remove(tree:Pavl_tree_t; node:pointer);cdecl;external external_avl_library name 'avl_remove';
function avl_update(_para1:Pavl_tree_t; _para2:pointer):boolean_t;cdecl;external external_avl_library name 'avl_update';
function avl_update_lt(_para1:Pavl_tree_t; _para2:pointer):boolean_t;cdecl;external external_avl_library name 'avl_update_lt';
function avl_update_gt(_para1:Pavl_tree_t; _para2:pointer):boolean_t;cdecl;external external_avl_library name 'avl_update_gt';
function avl_numnodes(tree:Pavl_tree_t):ulong_t;cdecl;external external_avl_library name 'avl_numnodes';
function avl_is_empty(tree:Pavl_tree_t):boolean_t;cdecl;external external_avl_library name 'avl_is_empty';
function avl_destroy_nodes(tree:Pavl_tree_t; cookie:Ppointer):pointer;cdecl;external external_avl_library name 'avl_destroy_nodes';
procedure avl_destroy(tree:Pavl_tree_t);cdecl;external external_avl_library name 'avl_destroy';


implementation

function fos_getzonenamebyid(id: zoneid_t): string;
var len : NativeInt;
begin
  SetLength(Result,ZONENAME_MAX);
  len := getzonenamebyid(id,@result[1],Length(Result));
  if len>0 then
    SetLength(result,len)
  else
    result := '';
end;


function AVL_NEXT(tree,node : longint) : longint;
begin
  abort;
end;

function AVL_PREV(tree,node : longint) : longint;
begin
  abort;
end;

end.

