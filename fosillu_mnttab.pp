unit fosillu_mnttab;

interface
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
{	Copyright (c) 1984, 1986, 1987, 1988, 1989 AT&T	 }
{	  All Rights Reserved  	 }
{
 * Copyright 2009 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
  }

uses
  ctypes,fos_illumos_defs,unixtype;

const
  External_library=''; {Setup as you need}

  MNTTAB_      = '/etc/mnttab';
  MNT_LINE_MAX = 1024;  { entry exceeds MNT_LINE_MAX  }
  MNT_TOOLONG  = 1;     { too many fields in line  }
  MNT_TOOMANY  = 2;     { too few fields in line  }
  MNT_TOOFEW   = 3;

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

  Pextmnttab  = ^extmnttab;
  PFILE  = ^FILE;
  Pmntentbuf  = ^mntentbuf;
  Pmnttab  = ^mnttab;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



  {#define	mntnull(mp)\ }
{	((mp)->mnt_special = (mp)->mnt_mountp = \ }
{	    (mp)->mnt_fstype = (mp)->mnt_mntopts = \ }
{	    (mp)->mnt_time = NULL) }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

{
 * The fields in struct extmnttab should match those in struct mnttab until new
 * fields are encountered. This allows hasmntopt(), getmntent_common() and
 * mntioctl() to cast one type to the other safely.
 *
 * The fields in struct mnttab, struct extmnttab and struct mntentbuf must all
 * match those in the corresponding 32-bit versions defined in mntvnops.c.
  }

   mnttab = record
      mnt_special : pcchar;
      mnt_mountp : pcchar;
      mnt_fstype : pcchar;
      mnt_mntopts : pcchar;
      mnt_time : pcchar;
    end;

  extmnttab = record
      mnt_special : pcchar;
      mnt_mountp : pcchar;
      mnt_fstype : pcchar;
      mnt_mntopts : pcchar;
      mnt_time : pcchar;
      mnt_major : uint_t;
      mnt_minor : uint_t;
    end;

  mntentbuf = record
      mbuf_emp : Pextmnttab;
      mbuf_bufsize : size_t;
      mbuf_buf : pcchar;
    end;

function  putmntent(fd,mp : longint) : longint;
procedure resetmnttab(_para1:PFILE);cdecl;external External_library name 'resetmnttab';
function  getmntent(_para1:PFILE; _para2:Pmnttab):cint;cdecl;external External_library name 'getmntent';
function  getextmntent(_para1:PFILE; _para2:Pextmnttab; _para3:size_t):cint;cdecl;external External_library name 'getextmntent';
function  getmntany(_para1:PFILE; _para2:Pmnttab; _para3:Pmnttab):cint;cdecl;external External_library name 'getmntany';
function  hasmntopt(_para1:Pmnttab; _para2:pcchar):pcchar;cdecl;external External_library name 'hasmntopt';
function  mntopt(_para1:Ppcchar):pcchar;cdecl;external External_library name 'mntopt';

implementation

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function putmntent(fd,mp : longint) : longint;
begin
  putmntent:=-(1);
end;


end.
