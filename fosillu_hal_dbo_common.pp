unit fosillu_hal_dbo_common;

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
  Classes, SysUtils,
  fosillu_nvpair,fosillu_libzfs, fosillu_zfs, fos_illumos_defs,fosillu_libdladm,fosillu_dladm;

var
  GILLUMOS_LIBZFS_HANDLE : Plibzfs_handle_t = nil;

procedure InitIllumosLibraryHandles;
procedure FinishIllumosLibraryHandles;
procedure CheckIllumosZFSLib;

function  FOSNVPAIR_NAME   (const elem:Pnvpair_t):string;
function  FOSNVGET_STRING  (const elem:Pnvpair_t ; var val:string):boolean;
function  FOSNVGET_U64ARR  (const elem:Pnvpair_t ; var val:PPuint64_t ; var cnt : Uint64_t):boolean;
function  FOSNVGET_NVLIST  (const elem:Pnvpair_t ; var nvlist : Pnvlist_t):boolean;
function  FOSNVGET_NVARR   (const elem:Pnvpair_t ; var nvlistarr : PPnvlist_t ; var cnt : uint_t):boolean;


implementation

procedure CheckIllumosZFSLib;
begin
  if not Assigned(GILLUMOS_LIBZFS_HANDLE) then
    raise EXception.Create('error : libzfshandle not assigned');
end;

procedure InitIllumosLibraryHandles;
var dlres : dladm_status_t;
begin
  if not Assigned(GILLUMOS_LIBZFS_HANDLE) then
    GILLUMOS_LIBZFS_HANDLE := libzfs_init();
  CheckIllumosZFSLib;

  dlres := dladm_open(@GILLU_DLADM);
  if dlres<>DLADM_STATUS_OK then
    raise Exception.Create('could not initialze libdlam');


end;

procedure FinishIllumosLibraryHandles;
begin
  if Assigned(GILLUMOS_LIBZFS_HANDLE) then
    begin
      libzfs_fini(GILLUMOS_LIBZFS_HANDLE);
      GILLUMOS_LIBZFS_HANDLE := nil;
    end;
end;

function FOSNVPAIR_NAME(const elem:Pnvpair_t):string;
begin
  result := pchar(nvpair_name(elem));
end;

function FOSNVGET_STRING(const elem:Pnvpair_t ; var val:string):boolean;
var s : Pchar;
begin
  result := nvpair_value_string(elem,@s)=0;
  val    := s;
end;

function FOSNVGET_U64ARR(const elem:Pnvpair_t ; var val:PPuint64_t ; var cnt : Uint64_t):boolean;
begin
  result := nvpair_value_uint64_array(elem,@val,@cnt)=0;
end;


function FOSNVGET_NVLIST(const elem:Pnvpair_t ; var nvlist : Pnvlist_t):boolean;
begin
  result := nvpair_value_nvlist(elem,@nvlist)=0;
end;

function FOSNVGET_NVARR(const elem:Pnvpair_t ; var nvlistarr : PPnvlist_t ; var cnt : uint_t):boolean;
begin
  result := nvpair_value_nvlist_array(elem,@nvlistarr,@cnt)=0;
end;

end.

