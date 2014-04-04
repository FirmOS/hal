unit fosillu_hal_dbo_zfs_dataset;

{
(§LIC)
  (c) Autor,Copyright
      Dipl.Ing.- Helmut Hartl, Dipl.Ing.- Franz Schober, Dipl.Ing.- Christian Koch
      FirmOS Business Solutions GmbH
      www.openfirmos.org
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2013, FirmOS Business Solutions GmbH
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

      * Redistributions of source code must retain the above copyright notice,
        this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright notice,
        this list of conditions and the following disclaimer in the documentation
        and/or other materials provided with the distribution.
      * Neither the name of the <FirmOS Business Solutions GmbH> nor the names
        of its contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(§LIC_END)
}

{$mode objfpc}{$H+}
{$codepage UTF8}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils,ctypes,fosillu_nvpair, fosillu_libzfs, fosillu_zfs,fos_illumos_defs,fosillu_hal_dbo_common;

procedure zfs_list_all;

implementation

//typedef struct list_cbdata {
//	boolean_t	cb_first;
//	boolean_t	cb_literal;
//	boolean_t	cb_scripted;
//	zprop_list_t	*cb_proplist;
//} list_cbdata_t;

function FOS_ZFS_TYPE2STR(const ztype:zfs_type_t):ShortString;
begin
  case ztype of
    ZFS_TYPE_FILESYSTEM : result := 'FILESYSTEM';
    ZFS_TYPE_SNAPSHOT   : result := 'SNAPSHOT';
    ZFS_TYPE_DATASET    : result := 'DATASET';
    ZFS_TYPE_VOLUME     : result := 'VOLUME';
    ZFS_TYPE_POOL       : result := 'POOL';
    ZFS_TYPE_BOOKMARK   : result := 'BOOKMARK';
    else
      result := 'UNKNOWN';
  end;
end;

type
  TZFS_ITER=record
    parent       : string;
    iterfunction : zfs_iter_f;
  end;
  PZFS_ITER=^TZFS_ITER;

function FOS_GET_ZFS_PROP_STR(const zfs_handle : Pzfs_handle_t ; const prop : zfs_prop_t):string;
var propertystr : string;
    res         : cint;
begin
  SetLength(propertystr,ZFS_MAXPROPLEN);
  res := zfs_prop_get(zfs_handle,prop,pchar(propertystr),length(propertystr),nil,nil,0,B_TRUE);
  if res<>0 then
    raise Exception.Create('could not fetch property '+zfs_prop_to_name(prop));
  result := Copy(pchar(@propertystr[1]),1,maxint);
end;

function zfs_iterate(zhp:Pzfs_handle_t; mydata : pointer):cint;cdecl;
var PZITER      : PZFS_ITER;
    ztype       : zfs_type_t;
    userprops   : Pnvlist_t;
    elem        : Pnvpair_t;
    propertystr : string;
    propname    : string;
    res         : cint;
    proptyp     : zfs_prop_t;
    name        : string;

begin
  PZITER:= PZFS_iter(mydata);
  ztype := zfs_get_type(zhp);

  name  := FOS_GET_ZFS_PROP_STR(zhp,ZFS_PROP_NAME);

  writeln('ZFS TYPE : ',FOS_ZFS_TYPE2STR(ztype),' : ',name);
//  for proptyp := low(zfs_prop_t) to ZFS_PROP_INCONSISTENT do
////  proptyp:= ZFS_PROP_NAME;
//   begin
//      SetLength(propertystr,ZFS_MAXPROPLEN);
//      res := zfs_prop_get(zhp,proptyp,pchar(propertystr),length(propertystr),nil,nil,0,B_TRUE);
//      propname    := zfs_prop_to_name(proptyp);
//      propertystr := Copy(pchar(@propertystr[1]),1,maxint);
//      write(propname,'="',propertystr,'" ');
//    end;

  if ztype=ZFS_TYPE_FILESYSTEM then
    begin
      PZITER^.parent:='';
      zfs_iter_filesystems(zhp,pziter^.iterfunction,pziter);
    end;
  if ztype=ZFS_TYPE_FILESYSTEM then
    zfs_iter_snapshots(zhp, pziter^.iterfunction,pziter);

  //if ztype=ZFS_TYPE_BOOKMARK then
  //  zfs_iter_bookmarks(zhp, pziter^.iterfunction,pziter);
  zfs_close(zhp);
  result := 0;
end;

procedure zfs_list_all;
var ZITER          : TZFS_ITER;
    res            : cint;
    fields         : string;
begin
  ZITER:=default(TZFS_ITER);
  ZITER.iterfunction:=@zfs_iterate;
  CheckIllumosZFSLib;
  //res := zprop_get_list(GILLUMOS_LIBZFS_HANDLE,pchar(fields),@ZITER.cb_proplist,ZFS_TYPE_DATASET);
  //writeln('ZPGL ',res);
  writeln('ZFS HANDLE OK -> ITERATING');
  readln;
  zfs_iter_root(GILLUMOS_LIBZFS_HANDLE,ziter.iterfunction,@ZITER);
  //writeln('TEST HISTORY');
  //zpool_log_history(GILLUMOS_LIBZFS_HANDLE,'FIRMOZ WAZ HERE!');

end;

end.

