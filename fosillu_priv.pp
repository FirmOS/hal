unit fosillu_priv;

interface

uses
  ctypes,unixtype,fos_illumos_defs;

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
 * Copyright 2010 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
  }

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  External_library='';

  PRIV_STR_PORT = $00;
 { literal output  }
  PRIV_STR_LIT = $01;
{ shortest output  }
  PRIV_STR_SHORT = $02;
{ for priv_set()  }

type

  priv_ptype_t = Pchar;
  priv_t       = Pchar;

  //Ppriv_impl_info_t  = ^priv_impl_info_t;
  Ppriv_set_t        = ^priv_set_t;
  priv_op_t          = (PRIV_ON,PRIV_OFF,PRIV_SET_);


function PRIV_ALLSETS : priv_ptype_t;

{
 * library functions prototype.
}

function setppriv(_para1:priv_op_t; _para2:priv_ptype_t; _para3:Ppriv_set_t):cint;cdecl;external External_library name 'setppriv';
function getppriv(_para1:priv_ptype_t; _para2:Ppriv_set_t):cint;cdecl;external External_library name 'getppriv';
function setpflags(_para1:uint_t; _para2:uint_t):cint;cdecl;external External_library name 'setpflags';
function getpflags(_para1:uint_t):uint_t;cdecl;external External_library name 'getpflags';
//function getprivimplinfo:Ppriv_impl_info_t;cdecl;external External_library name 'getprivimplinfo';
function priv_set(_para1:priv_op_t; _para2:priv_ptype_t; args:array of const):cint;cdecl;external External_library name 'priv_set';
//function priv_set(_para1:priv_op_t; _para2:priv_ptype_t):cint;cdecl;external External_library name 'priv_set';
function priv_ineffect(_para1:pcchar):boolean_t;cdecl;external External_library name 'priv_ineffect';
function priv_str_to_set(_para1:pcchar; _para2:pcchar; _para3:Ppcchar):Ppriv_set_t;cdecl;external External_library name 'priv_str_to_set';
function priv_set_to_str(_para1:Ppriv_set_t; _para2:cchar; _para3:cint):pcchar;cdecl;external External_library name 'priv_set_to_str';
function priv_getbyname(_para1:pcchar):cint;cdecl;external External_library name 'priv_getbyname';
function priv_getbynum(_para1:cint):pcchar;cdecl;external External_library name 'priv_getbynum';
function priv_getsetbyname(_para1:pcchar):cint;cdecl;external External_library name 'priv_getsetbyname';
function priv_getsetbynum(_para1:cint):pcchar;cdecl;external External_library name 'priv_getsetbynum';
function priv_gettext(_para1:pcchar):pcchar;cdecl;external External_library name 'priv_gettext';
function priv_allocset:Ppriv_set_t;cdecl;external External_library name 'priv_allocset';
procedure priv_freeset(_para1:Ppriv_set_t);cdecl;external External_library name 'priv_freeset';
procedure priv_emptyset(_para1:Ppriv_set_t);cdecl;external External_library name 'priv_emptyset';
procedure priv_basicset(_para1:Ppriv_set_t);cdecl;external External_library name 'priv_basicset';
procedure priv_fillset(_para1:Ppriv_set_t);cdecl;external External_library name 'priv_fillset';
function priv_isemptyset(_para1:Ppriv_set_t):boolean_t;cdecl;external External_library name 'priv_isemptyset';
function priv_isfullset(_para1:Ppriv_set_t):boolean_t;cdecl;external External_library name 'priv_isfullset';
function priv_isequalset(_para1:Ppriv_set_t; _para2:Ppriv_set_t):boolean_t;cdecl;external External_library name 'priv_isequalset';
function priv_issubset(_para1:Ppriv_set_t; _para2:Ppriv_set_t):boolean_t;cdecl;external External_library name 'priv_issubset';
procedure priv_intersect(_para1:Ppriv_set_t; _para2:Ppriv_set_t);cdecl;external External_library name 'priv_intersect';
procedure priv_union(_para1:Ppriv_set_t; _para2:Ppriv_set_t);cdecl;external External_library name 'priv_union';
procedure priv_inverse(_para1:Ppriv_set_t);cdecl;external External_library name 'priv_inverse';
function priv_addset(_para1:Ppriv_set_t; _para2:pcchar):cint;cdecl;external External_library name 'priv_addset';
procedure priv_copyset(_para1:Ppriv_set_t; _para2:Ppriv_set_t);cdecl;external External_library name 'priv_copyset';
function priv_delset(_para1:Ppriv_set_t; _para2:pcchar):cint;cdecl;external External_library name 'priv_delset';
function priv_ismember(_para1:Ppriv_set_t; _para2:pcchar):boolean_t;cdecl;external External_library name 'priv_ismember';

implementation

{ was #define dname def_expr }
function PRIV_ALLSETS : priv_ptype_t;
begin
  PRIV_ALLSETS := priv_ptype_t(0);
end;


end.
