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
  Classes, SysUtils,
  fre_zfs, fre_db_interface, fos_basis_tools, fos_tool_interfaces, ctypes,
  fosillu_nvpair,fosillu_libzfs, fosillu_zfs, fos_illumos_defs,fosillu_hal_dbo_common;


procedure zfs_ds_test;

function  fosillu_zfs_GetZFSFilesystems  (out error: string; out zfsfs:IFRE_DB_Object) : integer;
function  fosillu_zfs_create_ds          (const ds_name:string ; out error:string):integer;
function  fosillu_zfs_create_zvol        (const ds_name:string ; volsize : QWord ; volblocksize : NativeUInt ; const ommit_reservation : boolean ; out error:string):integer;
function  fosillu_zfs_destroy_ds         (const ds_name: string; recurse,defer : boolean ; out error: string): integer;
function  fosillu_zfs_clone_ds           (const source_snap : string ; const destination : string ; out error:string):integer;

implementation

type
  zfs_cb_data=record
    zproplist : Pzprop_list_t;
    parent    : IFRE_DB_Object;
  end;

  Pzfs_cb_data = ^zfs_cb_data;

var funcp:zfs_iter_f;
    glo_cnt : integer=0;

    property_  : array [0..ZFS_MAXPROPLEN] of char;
    sourcename : array [0..ZFS_MAXNAMELEN] of char;

function dataset_cb(z_hdl:Pzfs_handle_t; data:pointer):cint;cdecl;
var ztyp       : zfs_type_t;
   cbd        : Pzfs_cb_data;
   userprops  : Pnvlist_t;
   propval    : Pnvlist_t;
   propstr    : Pchar;
   //source     : zprop_source_t;
   //src        : string;
   new_obj    : IFRE_DB_Object;


   procedure setup_props;
   var pl  : Pzprop_list_t;
       zpn : string;
   begin
     userprops := zfs_get_user_props(z_hdl);
     pl := cbd^.zproplist;
     while(pl<>Nil) do
       begin
         if pl^.pl_prop <> ZPROP_INVAL then
           begin
             if zfs_prop_get(z_hdl,zfs_prop_t(pl^.pl_prop),@property_,sizeof(property_),nil,nil,0,B_TRUE)=0 then
               begin
                 zpn :=  zfs_prop_to_name(zfs_prop_t(pl^.pl_prop));
                 (new_obj.Implementor_HC as TFRE_DB_ZFS_BASE).SetupPropFieldFromName(zpn,pchar(property_));
                 //writeln('  SYS:',zfs_prop_to_name(zfs_prop_t(pl^.pl_prop)),': ',pchar(property_),' SOURCE : ',src);//' ',pchar(sourcename));
               end;
           end
         else
         if ord(zfs_prop_userquota_(pchar(pl^.pl_user_prop)))<>0 then
           begin
              writeln('userquota!');
           end
         else
         if ord(zfs_prop_written_(pchar(pl^.pl_user_prop)))<>0 then
           begin
             writeln('written!');
           end
         else
           begin
             if nvlist_lookup_nvlist(userprops,Pcchar(pl^.pl_user_prop),@propval)<>0 then
               propstr:='-'
             else
               begin
                 if nvlist_lookup_string(propval,PCChar(ZPROP_VALUE),@propstr)<>0 then
                   propstr := 'lookup failure';
                  zpn := Pchar(pl^.pl_user_prop);
                 (new_obj.Implementor_HC as TFRE_DB_ZFS_BASE).SetupPropFieldFromName(zpn,propstr);
               end;
           end;
         pl :=  pl^.pl_next;
       end;
   end;

   function GET_FOS_ZFS_OBJECT : IFRE_DB_OBJECT;
   begin
     if (ztyp and ZFS_TYPE_POOL)>0 then
       raise EFRE_DB_Exception.Create(edb_ERROR,'unexpected pool zfs type '+inttostr(ztyp));
     if (ztyp and (ZFS_TYPE_SNAPSHOT or ZFS_TYPE_BOOKMARK)) = 0 then
       begin { is a plain filesystem}
         if (ztyp and ZFS_TYPE_VOLUME)=0 then
           result := TFRE_DB_ZFS_DATASET_FILE.CreateForDB
         else
           result := TFRE_DB_ZFS_DATASET_ZVOL.CreateForDB;
       end
     else
     if (ztyp and (ZFS_TYPE_SNAPSHOT))>0 then
       begin
         result := TFRE_DB_ZFS_SNAPSHOT.CreateForDB;
       end
     else
     if (ztyp and (ZFS_TYPE_BOOKMARK))>0 then
       begin
         result := TFRE_DB_ZFS_BOOKMARK.CreateForDB;
       end
     else
       raise EFRE_DB_Exception.Create(edb_ERROR,'unexpected zfs type while searching '+inttostr(ztyp));
     result.Field('objname').AsString := zfs_get_name(z_hdl);
     //writeln('ZFS OBJ ',result.SchemeClass,' ',result.Field('objname').AsString);
   end;

 var res : integer;

begin
  inc(glo_cnt);
  cbd := Pzfs_cb_data(data);
  ztyp := zfs_get_type(z_hdl);

  new_obj := GET_FOS_ZFS_OBJECT;
  //if not assigned(cbd^.zproplist) then { expand proplist to include all properties, once}
  //  begin
  //    res := zfs_expand_proplist(z_hdl,@cbd^.zproplist,B_FALSE,B_FALSE);
  //
  //  end;
  setup_props;
  cbd^.parent.Field('zfs').AddObject(new_obj);
  if ztyp = ZFS_TYPE_FILESYSTEM then
    zfs_iter_filesystems(z_hdl,funcp,data);
  if (ztyp and (ZFS_TYPE_SNAPSHOT or ZFS_TYPE_BOOKMARK)) = 0 then
    zfs_iter_snapshots(z_hdl,funcp,data);
  //if (ztyp and (ZFS_TYPE_SNAPSHOT or ZFS_TYPE_BOOKMARK)) = 0 then
  //  begin { maybe a leak in iter bookmarks function, so leave bookmarks for now }
  //    //res := zfs_iter_bookmarks(z_hdl,funcp,data);
  //    //writeln('res bm ',res,' ',ztyp);
  //  end;
  result := 0;
  zfs_close(z_hdl);
end;


function fosillu_zfs_GetZFSFilesystems(out error: string; out zfsfs: IFRE_DB_Object): integer;
var data_cb      : zfs_cb_data;
    startt,endt  : TFRE_DB_DateTime64;
    allpropnames : string;
    i            : zfs_prop_t;
    res          : integer;
begin
  startt := GFRE_DT.Now_UTC;
  zfsfs := GFRE_DBI.NewObject;
  data_cb.zproplist:=Nil;
  data_cb.parent := zfsfs;
  for i:=low(zfs_prop_t) to pred(ZFS_NUM_PROPS) do
    allpropnames := allpropnames+zfs_prop_to_name(i)+',';
  allpropnames:=Copy(allpropnames,1,Length(allpropnames)-1);
  funcp := @dataset_cb;
  res := zprop_get_list(GILLUMOS_LIBZFS_HANDLE,pchar(allpropnames),@data_cb.zproplist,ZFS_TYPE_DATASET);
  libzfs_set_cachedprops(GILLUMOS_LIBZFS_HANDLE,B_TRUE);
  zfs_iter_root(GILLUMOS_LIBZFS_HANDLE,funcp,@data_cb);
  zprop_free_list(data_cb.zproplist);
  endt := GFRE_DT.Now_UTC;
  result := endt-startt;
end;

function fosillu_zfs_create_ds(const ds_name: string; out error: string): integer;  {}
begin
  error  :='';
  Result := zfs_create(GILLUMOS_LIBZFS_HANDLE,pchar(ds_name),ZFS_TYPE_FILESYSTEM,nil);
  if result<>0 then
    begin
      error := libzfs_error_description(GILLUMOS_LIBZFS_HANDLE);
      exit;
    end;
end;

function fosillu_zfs_clone_ds(const source_snap : string ; const destination : string ; out error:string):integer;
var zhp   : pzfs_handle_t;
    clone : pzfs_handle_t;
begin
  error  :='';
  zhp := zfs_open(GILLUMOS_LIBZFS_HANDLE,pchar(source_snap),ZFS_TYPE_SNAPSHOT);
  if not assigned(zhp) then
    begin
      error := 'OPEN: '+libzfs_error_description(GILLUMOS_LIBZFS_HANDLE);
      exit;
    end;
  result := zfs_clone(zhp,pchar(destination),nil);
  if result<>0 then
    begin
      error := 'CLONE: '+libzfs_error_description(GILLUMOS_LIBZFS_HANDLE);
      exit;
    end;
  clone := zfs_open(GILLUMOS_LIBZFS_HANDLE,pchar(destination),ZFS_TYPE_DATASET);
  if assigned(clone) then
    begin
      if zfs_get_type(clone) <> ZFS_TYPE_VOLUME then
        begin
          Result := zfs_mount(clone,nil,0);
          if Result=0 then
            begin
              result := zfs_share(clone);
            end;
        end;
      zfs_close(clone);
    end;
end;

type
  destroy_cbdata=record
    cb_first,
    cb_force,
    cb_recurse,
    cb_error,
    cb_doclones     : boolean;
    cb_target       : pzfs_handle_t;
    cb_deferdestroy : boolean_t;
    cb_parsable     : boolean;
    cb_nvl          : Pnvlist_t;
    cb_batchedsnaps : Pnvlist_t;
    cb_firstsnap    : Pchar;
    cb_prevsnap     : PChar;
    cb_snapused     : int64_t;
    cb_snapspec     : PChar;
    cb_bookmark     : PChar;
    cb_myerror      : string;
  end;
  pdestroy_cbdata=^destroy_cbdata;

function destroy_check_dependent(zhp : Pzfs_handle_t; data:pointer):cint;cdecl;
var cbp   : pdestroy_cbdata;
    tname  : string;
    name   : string;
    zttype : zfs_type_t;
    zttyps : string;
    lastc  : char;
    label myout;

begin
  cbp    := data;
  name   := zfs_get_name(zhp);
  tname  := zfs_get_name(cbp^.cb_target);
  zttype := zfs_get_type(cbp^.cb_target);
  zttyps := zfs_type_to_name(zttype);
  lastc  := name[Length(tname)+1];
  if (Pos(tname,name)=1) and
      ((lastc='@') or (lastc='/')) then
        begin { direct descendent }
          if cbp^.cb_recurse then
            goto myout;
          if cbp^.cb_first then
            begin
              cbp^.cb_myerror := 'cannot destroy '+tname+': '+zttyps+' has children ['+name+']';
              cbp^.cb_error   := true;
              cbp^.cb_first   := false;
              zfs_close(zhp); { stop iteration }
              exit(1);
            end;
          //writeln('DD:>> ',name);
        end
  else
    begin { this is a clone }
      if (cbp^.cb_recurse=false) and
         (zttype=ZFS_TYPE_SNAPSHOT) then
           goto myout;
      if cbp^.cb_first then
        begin
          cbp^.cb_myerror := 'cannot destroy '+tname+': '+zttyps+' has dependend clones';
          cbp^.cb_error   := true;
          cbp^.cb_first   := false;
          zfs_close(zhp); { stop iterating }
          exit(1);
        end;
      //writeln('CL:>> ',name);
    end;
  myout:
    zfs_close(zhp);
    result := 0;
end;

function destroy_callback(zhp : Pzfs_handle_t; data:pointer):cint;cdecl;
var cb        : pdestroy_cbdata;
    name      : pchar;
    ztype     : zfs_type_t;
    error     : NativeInt;
    umounmask : cint;
begin
  cb     := data;
  name   := zfs_get_name(zhp);
  ztype  := zfs_get_type(zhp);
  if cb^.cb_force then
    umounmask:=MS_FORCE
  else
    umounmask:=0;
  //tname  := zfs_get_name(cbp^.cb_target);
  //zttyps := zfs_type_to_name(zttype);
  //lastc  := name[Length(tname)+1];

  if (pos('/',name)=0) and { ignore pools }
     (ztype=ZFS_TYPE_FILESYSTEM) then
       begin
         zfs_close(zhp);
         exit(0);
       end;
  if (ztype=ZFS_TYPE_SNAPSHOT) then
    fnvlist_add_boolean(cb^.cb_batchedsnaps,pcchar(name))
  else
    begin
      error := zfs_destroy_snaps_nvl(GILLUMOS_LIBZFS_HANDLE,cb^.cb_batchedsnaps,B_FALSE);
      fnvlist_free(cb^.cb_batchedsnaps);
      cb^.cb_batchedsnaps := fnvlist_alloc;
      if (error<>0) or
         (zfs_unmount(zhp,nil,umounmask)<>0) or
         (zfs_destroy(zhp,cb^.cb_deferdestroy)<>0) then
           begin
             zfs_close(zhp);
             exit(-1);
           end;
    end;
  zfs_close(zhp);
  result := 0;
end;

function fosillu_zfs_create_zvol(const ds_name: string; volsize: QWord; volblocksize: NativeUInt; const ommit_reservation: boolean; out error: string): integer;
var props      : Pnvlist_t;
    real_props : Pnvlist_t;
    zhp        : pzfs_handle_t;
    msg        : array [0..1023] of char;
    strval     : pchar;
begin
  if nvlist_alloc(@props,NV_UNIQUE_NAME,0)<>0 then
    begin
      error := 'no memory';
      exit(1);
    end;
  try
    if nvlist_add_uint64(props,PCchar(zfs_prop_to_name(ZFS_PROP_VOLSIZE)),volsize)<>0 then
      begin
        error := 'no memory/vsize';
        exit(1);
      end;
    if volblocksize=0 then
      volblocksize := 1024*8;

    if nvlist_add_uint64(props,PCchar(zfs_prop_to_name(ZFS_PROP_VOLBLOCKSIZE)),volblocksize)<>0 then
      begin
        error := 'no memory/vsize';
        exit(1);
      end;

    if not ommit_reservation then
      begin
        real_props := zfs_valid_proplist(GILLUMOS_LIBZFS_HANDLE,ZFS_TYPE_VOLUME,props,0,nil,@msg[0]);
        try
          if not assigned(real_props) then
            begin
              error := 'RESERVE: '+pchar(msg);
              exit(1);
            end;
          volsize := zvol_volsize_to_reservation(volsize,real_props);
        finally
          nvlist_free(real_props);
        end;
        if nvlist_lookup_string(props,pcchar(zfs_prop_to_name(ZFS_PROP_REFRESERVATION)),ppcchar(strval))<>0 then
          begin
            if nvlist_add_uint64(props,pcchar(zfs_prop_to_name(ZFS_PROP_REFRESERVATION)),volsize)<>0 then
              begin
                error := 'RESERVE LOOKUP';
                exit(1);
              end;
          end;
      end;

    error  :='';
    Result := zfs_create(GILLUMOS_LIBZFS_HANDLE,pchar(ds_name),ZFS_TYPE_VOLUME,props);
    if result<>0 then
      begin
        error := libzfs_error_description(GILLUMOS_LIBZFS_HANDLE);
        exit;
      end;
  finally
    nvlist_free(props);
  end;

 // zhp :=
end;

function fosillu_zfs_destroy_ds(const ds_name: string; recurse,defer:boolean ; out error: string): integer;

var at,pound        : boolean;
    slash           : boolean;
    zhp             : Pzfs_handle_t;
    typ             : zfs_type_t = ZFS_TYPE_DATASET;
    cb              : destroy_cbdata;
    zname           : string;
    err             : NativeInt;
    rv              : NativeInt;
    label myout;

begin
  rv := 0;
  cb := Default(destroy_cbdata);
  cb.cb_recurse      := recurse;
  if defer then
    cb.cb_deferdestroy := B_TRUE
  else
    cb.cb_deferdestroy := B_FALSE;
  at    := pos('@',ds_name)>0;
  pound := pos('@',ds_name)>0;
  if at then
    begin
      result := -1;
      error  := 'fos:implement snapshot destroy';
    end
  else
  if pound then
    begin
      result := -1;
      error  := 'fos:implement bookmark destroy';
    end
  else
    begin
      zhp := zfs_open(GILLUMOS_LIBZFS_HANDLE,PChar(ds_name),typ);
      if not assigned(zhp) then
        begin
          error := 'OPEN: '+libzfs_error_description(GILLUMOS_LIBZFS_HANDLE);
          exit;
        end;
      cb.cb_target := zhp;
      zname        := zfs_get_name(zhp);
      slash        := pos('/',zname)>0;
      if (((cb.cb_recurse=false) and
           (not slash)) and
           ( zfs_get_type(zhp)=ZFS_TYPE_FILESYSTEM)) then
             begin
               error := 'cannot destroy a pool with dataset destroy functions';
               exit(1);
             end;
      cb.cb_first := true;
      if (cb.cb_doclones=false) then
        if zfs_iter_dependents(zhp,B_TRUE,@destroy_check_dependent,@cb)<>0 then
          begin
            error := cb.cb_myerror;
            exit(1);
          end;
        if cb.cb_error then
          begin
            error := cb.cb_myerror;
            exit(1);
          end;
        cb.cb_batchedsnaps := fnvlist_alloc;
        err := zfs_iter_dependents(zhp,B_FALSE,@destroy_callback,@cb);
        if err<>0 then
          begin
            rv := 1;
            goto myout;
          end;

        err := destroy_callback(zhp,@cb);
        zhp := nil;
        if err=0 then
          err := zfs_destroy_snaps_nvl(GILLUMOS_LIBZFS_HANDLE,cb.cb_batchedsnaps,cb.cb_deferdestroy);

    end;
   myout:
     fnvlist_free(cb.cb_batchedsnaps);
     fnvlist_free(cb.cb_nvl);
     if assigned(zhp) then
       zfs_close(zhp);
     exit(rv);
end;

procedure zfs_ds_test;
var err : string;
    res : integer;
begin
  InitIllumosLibraryHandles;
  CheckIllumosZFSLib;
  //writeln('create ds with illegal name');
  //res := fosillu_zfs_create_ds(err,'1hallo');
  //writeln('Illegal create ',res,'  ',err);
  if ParamStr(1)='destroy' then
    begin
      writeln('DESTROY TEST');
      writeln(fosillu_zfs_destroy_ds('syspool/zoneguid',true,false,err),err);
      writeln('---');
      exit;
    end;
  //writeln(fosillu_zfs_destroy_ds('syspool/zoneguid',true,false,false,err),err);
  writeln(fosillu_zfs_create_ds('syspool/zoneguid',err),err);
  writeln(fosillu_zfs_create_ds('syspool/zoneguid/vmdisk',err),err);
  writeln(fosillu_zfs_create_zvol('syspool/zoneguid/vmdisk/linux1',1024*1024*1024,1024*8,false,err),err);
  writeln(fosillu_zfs_create_ds('syspool/zoneguid/zonedata',err),err);
  writeln(fosillu_zfs_create_ds('syspool/zoneguid/zonedata/vfiler',err),err);
  writeln(fosillu_zfs_clone_ds('syspool/template/fbz093/etc@final','syspool/zoneguid/zonedata/etc',err),err);
  writeln(fosillu_zfs_clone_ds('syspool/template/fbz093/var@final','syspool/zoneguid/zonedata/var',err),err);
  writeln(fosillu_zfs_clone_ds('syspool/template/fbz093/optetc@final','syspool/zoneguid/zonedata/optetc',err),err);
  writeln(fosillu_zfs_clone_ds('syspool/template/fbz093/optfre@final','syspool/zoneguid/zonedata/optfre',err),err);
end;

end.

