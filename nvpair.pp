unit nvpair; { includes libnvpair }

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
 * Copyright (c) 2000, 2010, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2012 by Delphix. All rights reserved.
 * Copyright (c) 2014 by FirmOS Business Solutions Gmbh. All reights reserved.
}

interface

uses
  sysutils,ctypes,Unix,unixtype,BaseUnix,fos_illumos_defs;

const
  External_library='nvpair';

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

Type
  Pnv_alloc_ops_t  = ^nv_alloc_ops_t;
  Pnv_alloc_t      = ^nv_alloc_t;
  Pnvlist_t        = ^nvlist_t;
  PPnvlist_t       = ^Pnvlist_t;
  Pnvpair_t        = ^nvpair_t;
  PPnvpair_t       = ^Pnvpair_t;
  PPPnvlist_t      = ^PPnvlist_t;


  data_type_t = (DATA_TYPE_UNKNOWN := 0,DATA_TYPE_BOOLEAN,
    DATA_TYPE_BYTE,DATA_TYPE_INT16,DATA_TYPE_UINT16,
    DATA_TYPE_INT32,DATA_TYPE_UINT32,DATA_TYPE_INT64,
    DATA_TYPE_UINT64,DATA_TYPE_STRING,DATA_TYPE_BYTE_ARRAY,
    DATA_TYPE_INT16_ARRAY,DATA_TYPE_UINT16_ARRAY,
    DATA_TYPE_INT32_ARRAY,DATA_TYPE_UINT32_ARRAY,
    DATA_TYPE_INT64_ARRAY,DATA_TYPE_UINT64_ARRAY,
    DATA_TYPE_STRING_ARRAY,DATA_TYPE_HRTIME,
    DATA_TYPE_NVLIST,DATA_TYPE_NVLIST_ARRAY,
    DATA_TYPE_BOOLEAN_VALUE,DATA_TYPE_INT8,
    DATA_TYPE_UINT8,DATA_TYPE_BOOLEAN_ARRAY,
    DATA_TYPE_INT8_ARRAY,DATA_TYPE_UINT8_ARRAY,
    DATA_TYPE_DOUBLE);
{ size of this nvpair  }
{ length of name string  }
{ not used  }
{ number of elements for array types  }
{ type of value  }
{ name string  }
{ aligned ptr array for string arrays  }
{ aligned array of data for value  }

  Rnvpair = record
      nvp_size : int32_t;
      nvp_name_sz : int16_t;
      nvp_reserve : int16_t;
      nvp_value_elem : int32_t;
      nvp_type : data_type_t;
    end;
  nvpair_t = Rnvpair;
{ nvlist header  }
{ persistent flags  }
{ ptr to private data if not packed  }
{ currently not used, for alignment  }

  nvlist = record
      nvl_version : int32_t;
      nvl_nvflag : uint32_t;
      nvl_priv : uint64_t;
      nvl_flag : uint32_t;
      nvl_pad : int32_t;
    end;
  nvlist_t = nvlist;

  nv_alloc = record
      nva_ops : ^nv_alloc_ops_t;
      nva_arg : pointer;
    end;
  nv_alloc_t = nv_alloc;
  nv_alloc_ops = record
//      nv_ao_init  : function  (_para1:Pnv_alloc_t; _para2:__va_list):cint;cdecl;
      nv_ao_init  : function  (_para1:Pnv_alloc_t):cint;cdecl;varargs;
      nv_ao_fini  : procedure (_para1:Pnv_alloc_t);cdecl;
      nv_ao_alloc : function  (_para1:Pnv_alloc_t; _para2:size_t):pointer;cdecl;
      nv_ao_free  : procedure (_para1:Pnv_alloc_t; _para2:pointer; _para3:size_t);cdecl;
      nv_ao_reset : procedure (_para1:Pnv_alloc_t);cdecl;
    end;

  { NV allocator framework  }
  nv_alloc_ops_t = nv_alloc_ops;


{ nvp implementation version  }

const
  NV_VERSION = 0;  
{ nvlist pack encoding  }
  NV_ENCODE_NATIVE = 0;  
  NV_ENCODE_XDR = 1;  
{ nvlist persistent unique name flags, stored in nvl_nvflags  }
  NV_UNIQUE_NAME = $1;  
  NV_UNIQUE_NAME_TYPE = $2;  
{ nvlist lookup pairs related flags  }
  NV_FLAG_NOENTOK = $1;  
{ convenience macros  }

function NV_ALIGN    (x : pointer)      : pointer;
function NV_ALIGN4   (x : pointer)      : pointer;
function NVP_SIZE    (nvp : Pnvpair_t)  : longint;
function NVP_NAME    (nvp : Pnvpair_t)  : pcchar;
function NVP_TYPE    (nvp : Pnvpair_t)  : data_type_t;
function NVP_NELEM   (nvp : Pnvpair_t)  : longint;
function NVP_VALUE   (nvp : Pnvpair_t)  : pcchar;
function NVL_VERSION (nvl : Pnvlist_t)  : longint;
function NVL_SIZE    (nvl : Pnvlist_t)  : longint;
function NVL_FLAG    (nvl : Pnvlist_t)  : uint32_t;

  var
    nv_fixed_ops : ^nv_alloc_ops_t;cvar;external;
    nv_alloc_nosleep : ^nv_alloc_t;cvar;external;
{ args  }

function nv_alloc_init(_para1:Pnv_alloc_t; _para2:Pnv_alloc_ops_t; args:array of const):cint;cdecl;external External_library name 'nv_alloc_init';
function nv_alloc_init(_para1:Pnv_alloc_t; _para2:Pnv_alloc_ops_t):cint;cdecl;external External_library name 'nv_alloc_init';
procedure nv_alloc_reset(_para1:Pnv_alloc_t);cdecl;external External_library name 'nv_alloc_reset';
procedure nv_alloc_fini(_para1:Pnv_alloc_t);cdecl;external External_library name 'nv_alloc_fini';
{ list management  }
function nvlist_alloc(_para1:PPnvlist_t; _para2:uint_t; _para3:cint):cint;cdecl;external External_library name 'nvlist_alloc';
procedure nvlist_free(_para1:Pnvlist_t);cdecl;external External_library name 'nvlist_free';
function nvlist_size(_para1:Pnvlist_t; _para2:Psize_t; _para3:cint):cint;cdecl;external External_library name 'nvlist_size';
function nvlist_pack(_para1:Pnvlist_t; _para2:PPcchar; _para3:Psize_t; _para4:cint; _para5:cint):cint;cdecl;external External_library name 'nvlist_pack';
function nvlist_unpack(_para1:Pcchar; _para2:size_t; _para3:PPnvlist_t; _para4:cint):cint;cdecl;external External_library name 'nvlist_unpack';
function nvlist_dup(_para1:Pnvlist_t; _para2:PPnvlist_t; _para3:cint):cint;cdecl;external External_library name 'nvlist_dup';
function nvlist_merge(_para1:Pnvlist_t; _para2:Pnvlist_t; _para3:cint):cint;cdecl;external External_library name 'nvlist_merge';
function nvlist_nvflag(_para1:Pnvlist_t):uint_t;cdecl;external External_library name 'nvlist_nvflag';
function nvlist_xalloc(_para1:PPnvlist_t; _para2:uint_t; _para3:Pnv_alloc_t):cint;cdecl;external External_library name 'nvlist_xalloc';
function nvlist_xpack(_para1:Pnvlist_t; _para2:PPcchar; _para3:Psize_t; _para4:cint; _para5:Pnv_alloc_t):cint;cdecl;external External_library name 'nvlist_xpack';
function nvlist_xunpack(_para1:Pcchar; _para2:size_t; _para3:PPnvlist_t; _para4:Pnv_alloc_t):cint;cdecl;external External_library name 'nvlist_xunpack';
function nvlist_xdup(_para1:Pnvlist_t; _para2:PPnvlist_t; _para3:Pnv_alloc_t):cint;cdecl;external External_library name 'nvlist_xdup';
function nvlist_lookup_nv_alloc(_para1:Pnvlist_t):Pnv_alloc_t;cdecl;external External_library name 'nvlist_lookup_nv_alloc';
function nvlist_add_nvpair(_para1:Pnvlist_t; _para2:Pnvpair_t):cint;cdecl;external External_library name 'nvlist_add_nvpair';
function nvlist_add_boolean(_para1:Pnvlist_t; _para2:Pcchar):cint;cdecl;external External_library name 'nvlist_add_boolean';
function nvlist_add_boolean_value(_para1:Pnvlist_t; _para2:Pcchar; _para3:boolean_t):cint;cdecl;external External_library name 'nvlist_add_boolean_value';
function nvlist_add_byte(_para1:Pnvlist_t; _para2:Pcchar; _para3:uchar_t):cint;cdecl;external External_library name 'nvlist_add_byte';
function nvlist_add_int8(_para1:Pnvlist_t; _para2:Pcchar; _para3:int8_t):cint;cdecl;external External_library name 'nvlist_add_int8';
function nvlist_add_uint8(_para1:Pnvlist_t; _para2:Pcchar; _para3:uint8_t):cint;cdecl;external External_library name 'nvlist_add_uint8';
function nvlist_add_int16(_para1:Pnvlist_t; _para2:Pcchar; _para3:int16_t):cint;cdecl;external External_library name 'nvlist_add_int16';
function nvlist_add_uint16(_para1:Pnvlist_t; _para2:Pcchar; _para3:uint16_t):cint;cdecl;external External_library name 'nvlist_add_uint16';
function nvlist_add_int32(_para1:Pnvlist_t; _para2:Pcchar; _para3:int32_t):cint;cdecl;external External_library name 'nvlist_add_int32';
function nvlist_add_uint32(_para1:Pnvlist_t; _para2:Pcchar; _para3:uint32_t):cint;cdecl;external External_library name 'nvlist_add_uint32';
function nvlist_add_int64(_para1:Pnvlist_t; _para2:Pcchar; _para3:int64_t):cint;cdecl;external External_library name 'nvlist_add_int64';
function nvlist_add_uint64(_para1:Pnvlist_t; _para2:Pcchar; _para3:uint64_t):cint;cdecl;external External_library name 'nvlist_add_uint64';
function nvlist_add_string(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pcchar):cint;cdecl;external External_library name 'nvlist_add_string';
function nvlist_add_nvlist(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pnvlist_t):cint;cdecl;external External_library name 'nvlist_add_nvlist';
function nvlist_add_boolean_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pboolean_t; _para4:uint_t):cint;cdecl;external External_library name 'nvlist_add_boolean_array';
function nvlist_add_byte_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puchar_t; _para4:uint_t):cint;cdecl;external External_library name 'nvlist_add_byte_array';
function nvlist_add_int8_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pint8_t; _para4:uint_t):cint;cdecl;external External_library name 'nvlist_add_int8_array';
function nvlist_add_uint8_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puint8_t; _para4:uint_t):cint;cdecl;external External_library name 'nvlist_add_uint8_array';
function nvlist_add_int16_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pint16_t; _para4:uint_t):cint;cdecl;external External_library name 'nvlist_add_int16_array';
function nvlist_add_uint16_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puint16_t; _para4:uint_t):cint;cdecl;external External_library name 'nvlist_add_uint16_array';
function nvlist_add_int32_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pint32_t; _para4:uint_t):cint;cdecl;external External_library name 'nvlist_add_int32_array';
function nvlist_add_uint32_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puint32_t; _para4:uint_t):cint;cdecl;external External_library name 'nvlist_add_uint32_array';
function nvlist_add_int64_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pint64_t; _para4:uint_t):cint;cdecl;external External_library name 'nvlist_add_int64_array';
function nvlist_add_uint64_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puint64_t; _para4:uint_t):cint;cdecl;external External_library name 'nvlist_add_uint64_array';
function nvlist_add_string_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPcchar; _para4:uint_t):cint;cdecl;external External_library name 'nvlist_add_string_array';
function nvlist_add_nvlist_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPnvlist_t; _para4:uint_t):cint;cdecl;external External_library name 'nvlist_add_nvlist_array';
function nvlist_add_hrtime(_para1:Pnvlist_t; _para2:Pcchar; _para3:hrtime_t):cint;cdecl;external External_library name 'nvlist_add_hrtime';
function nvlist_add_double(_para1:Pnvlist_t; _para2:Pcchar; _para3:double):cint;cdecl;external External_library name 'nvlist_add_double';

function nvlist_remove(_para1:Pnvlist_t; _para2:Pcchar; _para3:data_type_t):cint;cdecl;external External_library name 'nvlist_remove';
function nvlist_remove_all(_para1:Pnvlist_t; _para2:Pcchar):cint;cdecl;external External_library name 'nvlist_remove_all';
function nvlist_remove_nvpair(_para1:Pnvlist_t; _para2:Pnvpair_t):cint;cdecl;external External_library name 'nvlist_remove_nvpair';
function nvlist_lookup_boolean(_para1:Pnvlist_t; _para2:Pcchar):cint;cdecl;external External_library name 'nvlist_lookup_boolean';
function nvlist_lookup_boolean_value(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pboolean_t):cint;cdecl;external External_library name 'nvlist_lookup_boolean_value';
function nvlist_lookup_byte(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puchar_t):cint;cdecl;external External_library name 'nvlist_lookup_byte';
function nvlist_lookup_int8(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pint8_t):cint;cdecl;external External_library name 'nvlist_lookup_int8';
function nvlist_lookup_uint8(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puint8_t):cint;cdecl;external External_library name 'nvlist_lookup_uint8';
function nvlist_lookup_int16(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pint16_t):cint;cdecl;external External_library name 'nvlist_lookup_int16';
function nvlist_lookup_uint16(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puint16_t):cint;cdecl;external External_library name 'nvlist_lookup_uint16';
function nvlist_lookup_int32(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pint32_t):cint;cdecl;external External_library name 'nvlist_lookup_int32';
function nvlist_lookup_uint32(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puint32_t):cint;cdecl;external External_library name 'nvlist_lookup_uint32';
function nvlist_lookup_int64(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pint64_t):cint;cdecl;external External_library name 'nvlist_lookup_int64';
function nvlist_lookup_uint64(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puint64_t):cint;cdecl;external External_library name 'nvlist_lookup_uint64';
function nvlist_lookup_string(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPcchar):cint;cdecl;external External_library name 'nvlist_lookup_string';
function nvlist_lookup_nvlist(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPnvlist_t):cint;cdecl;external External_library name 'nvlist_lookup_nvlist';
function nvlist_lookup_boolean_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPboolean_t; _para4:Puint_t):cint;cdecl;external External_library name 'nvlist_lookup_boolean_array';
function nvlist_lookup_byte_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPuchar_t; _para4:Puint_t):cint;cdecl;external External_library name 'nvlist_lookup_byte_array';
function nvlist_lookup_int8_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPint8_t; _para4:Puint_t):cint;cdecl;external External_library name 'nvlist_lookup_int8_array';
function nvlist_lookup_uint8_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPuint8_t; _para4:Puint_t):cint;cdecl;external External_library name 'nvlist_lookup_uint8_array';
function nvlist_lookup_int16_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPint16_t; _para4:Puint_t):cint;cdecl;external External_library name 'nvlist_lookup_int16_array';
function nvlist_lookup_uint16_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPuint16_t; _para4:Puint_t):cint;cdecl;external External_library name 'nvlist_lookup_uint16_array';
function nvlist_lookup_int32_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPint32_t; _para4:Puint_t):cint;cdecl;external External_library name 'nvlist_lookup_int32_array';
function nvlist_lookup_uint32_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPuint32_t; _para4:Puint_t):cint;cdecl;external External_library name 'nvlist_lookup_uint32_array';
function nvlist_lookup_int64_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPint64_t; _para4:Puint_t):cint;cdecl;external External_library name 'nvlist_lookup_int64_array';
function nvlist_lookup_uint64_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPuint64_t; _para4:Puint_t):cint;cdecl;external External_library name 'nvlist_lookup_uint64_array';
function nvlist_lookup_string_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPPcchar; _para4:Puint_t):cint;cdecl;external External_library name 'nvlist_lookup_string_array';
function nvlist_lookup_nvlist_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPPnvlist_t; _para4:Puint_t):cint;cdecl;external External_library name 'nvlist_lookup_nvlist_array';
function nvlist_lookup_hrtime(_para1:Pnvlist_t; _para2:Pcchar; _para3:Phrtime_t):cint;cdecl;external External_library name 'nvlist_lookup_hrtime';
function nvlist_lookup_pairs(_para1:Pnvlist_t; _para2:cint; args:array of const):cint;cdecl;external External_library name 'nvlist_lookup_pairs';
function nvlist_lookup_pairs(_para1:Pnvlist_t; _para2:cint):cint;cdecl;external External_library name 'nvlist_lookup_pairs';
function nvlist_lookup_double(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pdouble):cint;cdecl;external External_library name 'nvlist_lookup_double';
function nvlist_lookup_nvpair(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPnvpair_t):cint;cdecl;external External_library name 'nvlist_lookup_nvpair';
function nvlist_lookup_nvpair_embedded_index(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPnvpair_t; _para4:Pcint; _para5:PPcchar):cint;cdecl;external External_library name 'nvlist_lookup_nvpair_embedded_index';
function nvlist_exists(_para1:Pnvlist_t; _para2:Pcchar):boolean_t;cdecl;external External_library name 'nvlist_exists';
function nvlist_empty(_para1:Pnvlist_t):boolean_t;cdecl;external External_library name 'nvlist_empty';
function nvlist_next_nvpair(_para1:Pnvlist_t; _para2:Pnvpair_t):Pnvpair_t;cdecl;external External_library name 'nvlist_next_nvpair';
function nvlist_prev_nvpair(_para1:Pnvlist_t; _para2:Pnvpair_t):Pnvpair_t;cdecl;external External_library name 'nvlist_prev_nvpair';
function nvpair_name(_para1:Pnvpair_t):Pcchar;cdecl;external External_library name 'nvpair_name';
function nvpair_type(_para1:Pnvpair_t):data_type_t;cdecl;external External_library name 'nvpair_type';
function nvpair_type_is_array(_para1:Pnvpair_t):cint;cdecl;external External_library name 'nvpair_type_is_array';
function nvpair_value_boolean_value(_para1:Pnvpair_t; _para2:Pboolean_t):cint;cdecl;external External_library name 'nvpair_value_boolean_value';
function nvpair_value_byte(_para1:Pnvpair_t; _para2:Puchar_t):cint;cdecl;external External_library name 'nvpair_value_byte';
function nvpair_value_int8(_para1:Pnvpair_t; _para2:Pint8_t):cint;cdecl;external External_library name 'nvpair_value_int8';
function nvpair_value_uint8(_para1:Pnvpair_t; _para2:Puint8_t):cint;cdecl;external External_library name 'nvpair_value_uint8';
function nvpair_value_int16(_para1:Pnvpair_t; _para2:Pint16_t):cint;cdecl;external External_library name 'nvpair_value_int16';
function nvpair_value_uint16(_para1:Pnvpair_t; _para2:Puint16_t):cint;cdecl;external External_library name 'nvpair_value_uint16';
function nvpair_value_int32(_para1:Pnvpair_t; _para2:Pint32_t):cint;cdecl;external External_library name 'nvpair_value_int32';
function nvpair_value_uint32(_para1:Pnvpair_t; _para2:Puint32_t):cint;cdecl;external External_library name 'nvpair_value_uint32';
function nvpair_value_int64(_para1:Pnvpair_t; _para2:Pint64_t):cint;cdecl;external External_library name 'nvpair_value_int64';
function nvpair_value_uint64(_para1:Pnvpair_t; _para2:Puint64_t):cint;cdecl;external External_library name 'nvpair_value_uint64';
function nvpair_value_string(_para1:Pnvpair_t; _para2:PPcchar):cint;cdecl;external External_library name 'nvpair_value_string';
function nvpair_value_nvlist(_para1:Pnvpair_t; _para2:PPnvlist_t):cint;cdecl;external External_library name 'nvpair_value_nvlist';
function nvpair_value_boolean_array(_para1:Pnvpair_t; _para2:PPboolean_t; _para3:Puint_t):cint;cdecl;external External_library name 'nvpair_value_boolean_array';
function nvpair_value_byte_array(_para1:Pnvpair_t; _para2:PPuchar_t; _para3:Puint_t):cint;cdecl;external External_library name 'nvpair_value_byte_array';
function nvpair_value_int8_array(_para1:Pnvpair_t; _para2:PPint8_t; _para3:Puint_t):cint;cdecl;external External_library name 'nvpair_value_int8_array';
function nvpair_value_uint8_array(_para1:Pnvpair_t; _para2:PPuint8_t; _para3:Puint_t):cint;cdecl;external External_library name 'nvpair_value_uint8_array';
function nvpair_value_int16_array(_para1:Pnvpair_t; _para2:PPint16_t; _para3:Puint_t):cint;cdecl;external External_library name 'nvpair_value_int16_array';
function nvpair_value_uint16_array(_para1:Pnvpair_t; _para2:PPuint16_t; _para3:Puint_t):cint;cdecl;external External_library name 'nvpair_value_uint16_array';
function nvpair_value_int32_array(_para1:Pnvpair_t; _para2:PPint32_t; _para3:Puint_t):cint;cdecl;external External_library name 'nvpair_value_int32_array';
function nvpair_value_uint32_array(_para1:Pnvpair_t; _para2:PPuint32_t; _para3:Puint_t):cint;cdecl;external External_library name 'nvpair_value_uint32_array';
function nvpair_value_int64_array(_para1:Pnvpair_t; _para2:PPint64_t; _para3:Puint_t):cint;cdecl;external External_library name 'nvpair_value_int64_array';
function nvpair_value_uint64_array(_para1:Pnvpair_t; _para2:PPuint64_t; _para3:Puint_t):cint;cdecl;external External_library name 'nvpair_value_uint64_array';
function nvpair_value_string_array(_para1:Pnvpair_t; _para2:PPPcchar; _para3:Puint_t):cint;cdecl;external External_library name 'nvpair_value_string_array';
function nvpair_value_nvlist_array(_para1:Pnvpair_t; _para2:PPPnvlist_t; _para3:Puint_t):cint;cdecl;external External_library name 'nvpair_value_nvlist_array';
function nvpair_value_hrtime(_para1:Pnvpair_t; _para2:Phrtime_t):cint;cdecl;external External_library name 'nvpair_value_hrtime';
function nvpair_value_double(_para1:Pnvpair_t; _para2:Pdouble):cint;cdecl;external External_library name 'nvpair_value_double';
function fnvlist_alloc:Pnvlist_t;cdecl;external External_library name 'fnvlist_alloc';
procedure fnvlist_free(_para1:Pnvlist_t);cdecl;external External_library name 'fnvlist_free';
function fnvlist_size(_para1:Pnvlist_t):size_t;cdecl;external External_library name 'fnvlist_size';
function fnvlist_pack(_para1:Pnvlist_t; _para2:Psize_t):Pcchar;cdecl;external External_library name 'fnvlist_pack';
procedure fnvlist_pack_free(_para1:Pcchar; _para2:size_t);cdecl;external External_library name 'fnvlist_pack_free';
function fnvlist_unpack(_para1:Pcchar; _para2:size_t):Pnvlist_t;cdecl;external External_library name 'fnvlist_unpack';
function fnvlist_dup(_para1:Pnvlist_t):Pnvlist_t;cdecl;external External_library name 'fnvlist_dup';
procedure fnvlist_merge(_para1:Pnvlist_t; _para2:Pnvlist_t);cdecl;external External_library name 'fnvlist_merge';
function fnvlist_num_pairs(_para1:Pnvlist_t):size_t;cdecl;external External_library name 'fnvlist_num_pairs';
procedure fnvlist_add_boolean(_para1:Pnvlist_t; _para2:Pcchar);cdecl;external External_library name 'fnvlist_add_boolean';
procedure fnvlist_add_boolean_value(_para1:Pnvlist_t; _para2:Pcchar; _para3:boolean_t);cdecl;external External_library name 'fnvlist_add_boolean_value';
procedure fnvlist_add_byte(_para1:Pnvlist_t; _para2:Pcchar; _para3:uchar_t);cdecl;external External_library name 'fnvlist_add_byte';
procedure fnvlist_add_int8(_para1:Pnvlist_t; _para2:Pcchar; _para3:int8_t);cdecl;external External_library name 'fnvlist_add_int8';
procedure fnvlist_add_uint8(_para1:Pnvlist_t; _para2:Pcchar; _para3:uint8_t);cdecl;external External_library name 'fnvlist_add_uint8';
procedure fnvlist_add_int16(_para1:Pnvlist_t; _para2:Pcchar; _para3:int16_t);cdecl;external External_library name 'fnvlist_add_int16';
procedure fnvlist_add_uint16(_para1:Pnvlist_t; _para2:Pcchar; _para3:uint16_t);cdecl;external External_library name 'fnvlist_add_uint16';
procedure fnvlist_add_int32(_para1:Pnvlist_t; _para2:Pcchar; _para3:int32_t);cdecl;external External_library name 'fnvlist_add_int32';
procedure fnvlist_add_uint32(_para1:Pnvlist_t; _para2:Pcchar; _para3:uint32_t);cdecl;external External_library name 'fnvlist_add_uint32';
procedure fnvlist_add_int64(_para1:Pnvlist_t; _para2:Pcchar; _para3:int64_t);cdecl;external External_library name 'fnvlist_add_int64';
procedure fnvlist_add_uint64(_para1:Pnvlist_t; _para2:Pcchar; _para3:uint64_t);cdecl;external External_library name 'fnvlist_add_uint64';
procedure fnvlist_add_string(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pcchar);cdecl;external External_library name 'fnvlist_add_string';
procedure fnvlist_add_nvlist(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pnvlist_t);cdecl;external External_library name 'fnvlist_add_nvlist';
procedure fnvlist_add_nvpair(_para1:Pnvlist_t; _para2:Pnvpair_t);cdecl;external External_library name 'fnvlist_add_nvpair';
procedure fnvlist_add_boolean_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pboolean_t; _para4:uint_t);cdecl;external External_library name 'fnvlist_add_boolean_array';
procedure fnvlist_add_byte_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puchar_t; _para4:uint_t);cdecl;external External_library name 'fnvlist_add_byte_array';
procedure fnvlist_add_int8_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pint8_t; _para4:uint_t);cdecl;external External_library name 'fnvlist_add_int8_array';
procedure fnvlist_add_uint8_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puint8_t; _para4:uint_t);cdecl;external External_library name 'fnvlist_add_uint8_array';
procedure fnvlist_add_int16_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pint16_t; _para4:uint_t);cdecl;external External_library name 'fnvlist_add_int16_array';
procedure fnvlist_add_uint16_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puint16_t; _para4:uint_t);cdecl;external External_library name 'fnvlist_add_uint16_array';
procedure fnvlist_add_int32_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pint32_t; _para4:uint_t);cdecl;external External_library name 'fnvlist_add_int32_array';
procedure fnvlist_add_uint32_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puint32_t; _para4:uint_t);cdecl;external External_library name 'fnvlist_add_uint32_array';
procedure fnvlist_add_int64_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Pint64_t; _para4:uint_t);cdecl;external External_library name 'fnvlist_add_int64_array';
procedure fnvlist_add_uint64_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:Puint64_t; _para4:uint_t);cdecl;external External_library name 'fnvlist_add_uint64_array';
procedure fnvlist_add_string_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPcchar; _para4:uint_t);cdecl;external External_library name 'fnvlist_add_string_array';
procedure fnvlist_add_nvlist_array(_para1:Pnvlist_t; _para2:Pcchar; _para3:PPnvlist_t; _para4:uint_t);cdecl;external External_library name 'fnvlist_add_nvlist_array';
procedure fnvlist_remove(_para1:Pnvlist_t; _para2:Pcchar);cdecl;external External_library name 'fnvlist_remove';
procedure fnvlist_remove_nvpair(_para1:Pnvlist_t; _para2:Pnvpair_t);cdecl;external External_library name 'fnvlist_remove_nvpair';
function fnvlist_lookup_nvpair(nvl:Pnvlist_t; name:pcchar):Pnvpair_t;cdecl;external External_library name 'fnvlist_lookup_nvpair';
function fnvlist_lookup_boolean(nvl:Pnvlist_t; name:pcchar):boolean_t;cdecl;external External_library name 'fnvlist_lookup_boolean';
function fnvlist_lookup_boolean_value(nvl:Pnvlist_t; name:pcchar):boolean_t;cdecl;external External_library name 'fnvlist_lookup_boolean_value';
function fnvlist_lookup_byte(nvl:Pnvlist_t; name:pcchar):uchar_t;cdecl;external External_library name 'fnvlist_lookup_byte';
function fnvlist_lookup_int8(nvl:Pnvlist_t; name:pcchar):int8_t;cdecl;external External_library name 'fnvlist_lookup_int8';
function fnvlist_lookup_int16(nvl:Pnvlist_t; name:pcchar):int16_t;cdecl;external External_library name 'fnvlist_lookup_int16';
function fnvlist_lookup_int32(nvl:Pnvlist_t; name:pcchar):int32_t;cdecl;external External_library name 'fnvlist_lookup_int32';
function fnvlist_lookup_int64(nvl:Pnvlist_t; name:pcchar):int64_t;cdecl;external External_library name 'fnvlist_lookup_int64';
function fnvlist_lookup_uint8_t(nvl:Pnvlist_t; name:pcchar):uint8_t;cdecl;external External_library name 'fnvlist_lookup_uint8_t';
function fnvlist_lookup_uint16(nvl:Pnvlist_t; name:pcchar):uint16_t;cdecl;external External_library name 'fnvlist_lookup_uint16';
function fnvlist_lookup_uint32(nvl:Pnvlist_t; name:pcchar):uint32_t;cdecl;external External_library name 'fnvlist_lookup_uint32';
function fnvlist_lookup_uint64(nvl:Pnvlist_t; name:pcchar):uint64_t;cdecl;external External_library name 'fnvlist_lookup_uint64';
function fnvlist_lookup_string(nvl:Pnvlist_t; name:pcchar):Pcchar;cdecl;external External_library name 'fnvlist_lookup_string';
function fnvlist_lookup_nvlist(nvl:Pnvlist_t; name:pcchar):Pnvlist_t;cdecl;external External_library name 'fnvlist_lookup_nvlist';
function fnvpair_value_boolean_value(nvp:Pnvpair_t):boolean_t;cdecl;external External_library name 'fnvpair_value_boolean_value';
function fnvpair_value_byte(nvp:Pnvpair_t):uchar_t;cdecl;external External_library name 'fnvpair_value_byte';
function fnvpair_value_int8(nvp:Pnvpair_t):int8_t;cdecl;external External_library name 'fnvpair_value_int8';
function fnvpair_value_int16(nvp:Pnvpair_t):int16_t;cdecl;external External_library name 'fnvpair_value_int16';
function fnvpair_value_int32(nvp:Pnvpair_t):int32_t;cdecl;external External_library name 'fnvpair_value_int32';
function fnvpair_value_int64(nvp:Pnvpair_t):int64_t;cdecl;external External_library name 'fnvpair_value_int64';
function fnvpair_value_uint8_t(nvp:Pnvpair_t):uint8_t;cdecl;external External_library name 'fnvpair_value_uint8_t';
function fnvpair_value_uint16(nvp:Pnvpair_t):uint16_t;cdecl;external External_library name 'fnvpair_value_uint16';
function fnvpair_value_uint32(nvp:Pnvpair_t):uint32_t;cdecl;external External_library name 'fnvpair_value_uint32';
function fnvpair_value_uint64(nvp:Pnvpair_t):uint64_t;cdecl;external External_library name 'fnvpair_value_uint64';
function fnvpair_value_string(nvp:Pnvpair_t):Pcchar;cdecl;external External_library name 'fnvpair_value_string';
function fnvpair_value_nvlist(nvp:Pnvpair_t):Pnvlist_t;cdecl;external External_library name 'fnvpair_value_nvlist';


{ libnvpair starts here }


///*
// * All interfaces described in this file are private to Solaris, and
// * are subject to change at any time and without notice.  The public
// * nvlist/nvpair interfaces, as documented in manpage sections 3NVPAIR,
// * are all imported from <sys/nvpair.h> included above.
// */
//
//extern int nvpair_value_match(nvpair_t *, int, char *, char **);
//extern int nvpair_value_match_regex(nvpair_t *, int, char *, regex_t *,
//    char **);
//
//extern void nvlist_print(FILE *, nvlist_t *);
//extern void dump_nvlist(nvlist_t *, int);
//
///*
// * Private nvlist printing interface that allows the caller some control
// * over output rendering (as opposed to nvlist_print and dump_nvlist).
// *
// * Obtain an opaque nvlist_prtctl_t cookie using nvlist_prtctl_alloc
// * (NULL on failure);  on return the cookie is set up for default formatting
// * and rendering.  Quote the cookie in subsequent customisation functions and
// * then pass the cookie to nvlist_prt to render the nvlist.  Finally,
// * use nvlist_prtctl_free to release the cookie.
// *
// * For all nvlist_lookup_xxx and nvlist_lookup_xxx_array functions
// * we have a corresponding brace of functions that appoint replacement
// * rendering functions:
// *
// *	extern void nvlist_prtctl_xxx(nvlist_prtctl_t,
// *	    void (*)(nvlist_prtctl_t ctl, void *private, const char *name,
// *	    xxxtype value))
// *
// *	and
// *
// *	extern void nvlist_prtctl_xxx_array(nvlist_prtctl_t,
// *	    void (*)(nvlist_prtctl_t ctl, void *private, const char *name,
// *	    xxxtype value, uint_t count))
// *
// * where xxxtype is the C datatype corresponding to xxx, eg int8_t for "int8"
// * and char * for "string".  The function that is appointed to render the
// * specified datatype receives as arguments the cookie, the nvlist
// * member name, the value of that member (or a pointer for array function),
// * and (for array rendering functions) a count of the number of elements.
// */
//
//typedef struct nvlist_prtctl *nvlist_prtctl_t;	/* opaque */
//
//enum nvlist_indent_mode {
//	NVLIST_INDENT_ABS,	/* Absolute indentation */
//	NVLIST_INDENT_TABBED	/* Indent with tabstops */
//};
//
//extern nvlist_prtctl_t nvlist_prtctl_alloc(void);
//extern void nvlist_prtctl_free(nvlist_prtctl_t);
//extern void nvlist_prt(nvlist_t *, nvlist_prtctl_t);
//
///* Output stream */
//extern void nvlist_prtctl_setdest(nvlist_prtctl_t, FILE *);
//extern FILE *nvlist_prtctl_getdest(nvlist_prtctl_t);
//
///* Indentation mode, start indent, indent increment; default tabbed/0/1 */
//extern void nvlist_prtctl_setindent(nvlist_prtctl_t, enum nvlist_indent_mode,
//    int, int);
//extern void nvlist_prtctl_doindent(nvlist_prtctl_t, int);
//
//enum nvlist_prtctl_fmt {
//	NVLIST_FMT_MEMBER_NAME,		/* name fmt; default "%s = " */
//	NVLIST_FMT_MEMBER_POSTAMBLE,	/* after nvlist member; default "\n" */
//	NVLIST_FMT_BTWN_ARRAY		/* between array members; default " " */
//};
//
//extern void nvlist_prtctl_setfmt(nvlist_prtctl_t, enum nvlist_prtctl_fmt,
//    const char *);
//extern void nvlist_prtctl_dofmt(nvlist_prtctl_t, enum nvlist_prtctl_fmt, ...);
//
///*
// * Function prototypes for interfaces that appoint a new rendering function
// * for single-valued nvlist members.
// *
// * A replacement function receives arguments as follows:
// *
// *	nvlist_prtctl_t	Print control structure; do not change preferences
// *			for this object from a print callback function.
// *
// *	void *		The function-private cookie argument registered
// *			when the replacement function was appointed.
// *
// *	nvlist_t *	The full nvlist that is being processed.  The
// *			rendering function is called to render a single
// *			member (name and value passed as below) but it may
// *			want to reference or incorporate other aspects of
// *			the full nvlist.
// *
// *	const char *	Member name to render
// *
// *	valtype		Value of the member to render
// *
// * The function must return non-zero if it has rendered output for this
// * member, or 0 if it wants to default to standard rendering for this
// * one member.
// */
//
//
//#define	NVLIST_PRINTCTL_SVDECL(funcname, valtype) \
//    extern void funcname(nvlist_prtctl_t, \
//    int (*)(nvlist_prtctl_t, void *, nvlist_t *, const char *, valtype), \
//    void *)
//
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_boolean, int);
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_boolean_value, boolean_t);
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_byte, uchar_t);
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_int8, int8_t);
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_uint8, uint8_t);
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_int16, int16_t);
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_uint16, uint16_t);
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_int32, int32_t);
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_uint32, uint32_t);
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_int64, int64_t);
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_uint64, uint64_t);
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_double, double);
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_string, char *);
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_hrtime, hrtime_t);
//NVLIST_PRINTCTL_SVDECL(nvlist_prtctlop_nvlist, nvlist_t *);
//
//#undef	NVLIST_PRINTCTL_SVDECL	/* was just for "clarity" above */
//
///*
// * Function prototypes for interfaces that appoint a new rendering function
// * for array-valued nvlist members.
// *
// * One additional argument is taken: uint_t for the number of array elements
// *
// * Return values as above.
// */
//#define	NVLIST_PRINTCTL_AVDECL(funcname, vtype) \
//    extern void funcname(nvlist_prtctl_t, \
//    int (*)(nvlist_prtctl_t, void *, nvlist_t *, const char *, vtype, uint_t), \
//    void *)
//
//NVLIST_PRINTCTL_AVDECL(nvlist_prtctlop_boolean_array, boolean_t *);
//NVLIST_PRINTCTL_AVDECL(nvlist_prtctlop_byte_array, uchar_t *);
//NVLIST_PRINTCTL_AVDECL(nvlist_prtctlop_int8_array, int8_t *);
//NVLIST_PRINTCTL_AVDECL(nvlist_prtctlop_uint8_array, uint8_t *);
//NVLIST_PRINTCTL_AVDECL(nvlist_prtctlop_int16_array, int16_t *);
//NVLIST_PRINTCTL_AVDECL(nvlist_prtctlop_uint16_array, uint16_t *);
//NVLIST_PRINTCTL_AVDECL(nvlist_prtctlop_int32_array, int32_t *);
//NVLIST_PRINTCTL_AVDECL(nvlist_prtctlop_uint32_array, uint32_t *);
//NVLIST_PRINTCTL_AVDECL(nvlist_prtctlop_int64_array, int64_t *);
//NVLIST_PRINTCTL_AVDECL(nvlist_prtctlop_uint64_array, uint64_t *);
//NVLIST_PRINTCTL_AVDECL(nvlist_prtctlop_string_array, char **);
//NVLIST_PRINTCTL_AVDECL(nvlist_prtctlop_nvlist_array, nvlist_t **);
//


implementation

///* nvp implementation version */
//#define	NV_VERSION	0
//
///* nvlist pack encoding */
//#define	NV_ENCODE_NATIVE	0
//#define	NV_ENCODE_XDR		1
//
///* nvlist persistent unique name flags, stored in nvl_nvflags */
//#define	NV_UNIQUE_NAME		0x1
//#define	NV_UNIQUE_NAME_TYPE	0x2
//
///* nvlist lookup pairs related flags */
//#define	NV_FLAG_NOENTOK		0x1
//
///* convenience macros */
//#define	NV_ALIGN(x)		(((ulong_t)(x) + 7ul) & ~7ul)
//#define	NV_ALIGN4(x)		(((x) + 3) & ~3)
//
//#define	NVP_SIZE(nvp)		((nvp)->nvp_size)
//#define	NVP_NAME(nvp)		((char *)(nvp) + sizeof (nvpair_t))
//#define	NVP_TYPE(nvp)		((nvp)->nvp_type)
//#define	NVP_NELEM(nvp)		((nvp)->nvp_value_elem)
//#define	NVP_VALUE(nvp)		((char *)(nvp) + NV_ALIGN(sizeof (nvpair_t) \
//				+ (nvp)->nvp_name_sz))
//
//#define	NVL_VERSION(nvl)	((nvl)->nvl_version)
//#define	NVL_SIZE(nvl)		((nvl)->nvl_size)
//#define	NVL_FLAG(nvl)		((nvl)->nvl_flag)

function NV_ALIGN(x : Pointer) : Pointer;
begin
  abort;
  NV_ALIGN := Pointer(Longint(x+7) and ( not (7)));
end;

function NV_ALIGN4(x : Pointer) : Pointer;
begin
  abort;
  NV_ALIGN4 := Pointer(Longint(x+3) and ( not (3)));
end;

function NVP_SIZE(nvp : Pnvpair_t) : int32_t;
begin
  abort;
  NVP_SIZE := nvp^.nvp_size;
end;

function NVP_NAME(nvp : Pnvpair_t) : pcchar;
begin
  abort;
  //NVP_NAME:=pcchar(nvp(+(sizeof(nvpair_t))));
end;

function NVP_TYPE(nvp : Pnvpair_t) : data_type_t;
begin
  NVP_TYPE:=nvp^.nvp_type;
end;

function NVP_NELEM(nvp : Pnvpair_t) : int32_t;
begin
  NVP_NELEM:=nvp^.nvp_value_elem;
end;

function NVP_VALUE(nvp : Pnvpair_t) : pcchar;
begin
  abort;
  //NVP_VALUE:=pcchar(nvp(+(NV_ALIGN((sizeof(nvpair_t))+(nvp^.nvp_name_sz)))));
end;

function NVL_VERSION(nvl : Pnvlist_t) : int32_t;
begin
  NVL_VERSION:=nvl^.nvl_version;
end;

function NVL_SIZE(nvl : Pnvlist_t) : int32_t;
begin
  abort;
  //NVL_SIZE:=nvl^.nvl_size;
end;

function NVL_FLAG(nvl : Pnvlist_t) : uint32_t;
begin
  NVL_FLAG:=nvl^.nvl_flag;
end;


end.
