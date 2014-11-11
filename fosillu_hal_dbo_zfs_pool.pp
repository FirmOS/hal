unit fosillu_hal_dbo_zfs_pool;

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


  function       fosillu_zfs_GetPoolStatusDBO   (const poolname: string; out error : string; out outpool: IFRE_DB_Object) : integer;
  function       fosillu_zfs_GetActivePoolsDBO  (out error: string; out pools:IFRE_DB_Object) : integer;


implementation

function fosillu_zfs_GetPoolStatusDBO(const poolname: string; out error: string; out outpool: IFRE_DB_Object): integer;
type
    TParsestate = (psUndefined, psVdev, psVdevTree,psSubVdev);

var zp         : Pzpool_handle_t;
    zs         : Pnvlist_t;
    parsestate : Tparsestate;
    pool       : TFRE_DB_ZFS_POOL;
    zlog       : TFRE_DB_ZFS_LOG;


  procedure _SetFieldFromNVElement(const obj:IFRE_DB_Object;const elem: Pnvpair_t);
  var nvdatatype: string;
      pc        : pchar;
      s         : string;
      u64       : UInt64;
      elemname  : string;
      mapname   : string;
      devicename: string;
      poolstate : string;

    function _MapNVElementNameToFieldName(const elementname: string): string;
    begin
      result := elementname;
      result := StringReplace(result,'.','_',[rfReplaceAll]);
      if (elementname= ZPOOL_CONFIG_GUID) or (elementname= ZPOOL_CONFIG_POOL_GUID) then result := 'zfs_guid';
      if (elementname= ZPOOL_CONFIG_POOL_NAME) then result := 'objname';
      if (uppercase(result)='COM_DELPHIX:EMPTY_BPOBJ') then result :='';  //skip
    end;

  begin
    elemname := FOSNVPAIR_NAME(elem);
    mapname  := _MapNVElementNameToFieldName(elemname);
    case nvpair_type(elem) of
      DATA_TYPE_STRING: begin
          if nvpair_value_string(elem,@pc)<>0 then
            raise EFRE_DB_Exception.Create('invalid conversion of element'+elemname);
          obj.Field(mapname).AsString := pc;
          if mapname=ZPOOL_CONFIG_PATH then
            begin
              if Pos('/dev/dsk',pc)=1 then
                devicename:=ExtractFileName(pc)
              else
                devicename:=pc;
//                writeln(Pos('s0',devicename), ' ',length(devicename)-1);
              if Pos('s0',devicename)=(length(devicename)-1)then
                devicename := Copy(devicename,1,length(devicename)-2);
              obj.Field('devicename').asstring := devicename;
//                writeln('SWL:DEVICENAME',devicename);
            end;
        end;
      DATA_TYPE_UINT64: begin
          if nvpair_value_uint64(elem,@u64)<>0 then
            raise EFRE_DB_Exception.Create('invalid conversion of element'+elemname);
          if (mapname='zfs_guid') then
            begin
              obj.Field(mapname).asstring := IntToStr(u64);
              if (obj.Implementor_HC is TFRE_DB_ZFS_DATASTORAGE) then
                obj.Field(mapname).asstring := 'ROOTVDEV'+IntToStr(u64);
            end
          else if ((mapname='state') and (obj.Implementor_HC is TFRE_DB_ZFS_POOL)) then
            begin
              WriteStr(poolstate,pool_state_t(u64));
              obj.Field('state').AsString     := poolstate;
              obj.Field('state_ord').AsUInt64 := u64;
              //writeln('SWL:POOLSTATE',poolstate);
            end
          else
            begin
              if mapname<>'' then
                obj.Field(mapname).asuint64 := u64;
            end;
      end;
    else
      begin
        WriteStr(nvdatatype,data_type_t(nvpair_type(elem)));
        raise EFRE_DB_Exception.Create('invalid datatype for setting field from nvelement:'+nvdatatype);
      end;
    end;
  end;


  procedure fetch_zfs_scan_stats(const zfs_obj: TFRE_DB_ZFS_OBJ; const elem : Pnvpair_t);
  var c            : Uint64_t=0;
      uia          : PPuint64_t=nil;
      ps           : Ppool_scan_stat_t;
      func_s       : string;
      state_s      : string;
      zpool_iostat : TFRE_DB_ZPOOL_IOSTAT;
  begin
    if FOSNVGET_U64ARR(elem,uia,c) then
      begin
        ps := Ppool_scan_stat_t(uia);
        WriteStr(func_s,pool_scan_func_t(ps^.pss_func));
//        WriteLn(StringOfChar(' ',10),'FUNC = ', func_s);
        WriteStr(state_s,dsl_scan_state_t(ps^.pss_state));
//        WriteLn(StringOfChar(' ',10),'STATE = ', state_s);
        if zfs_obj.FieldExists('zpooliostat') then
          zpool_iostat := zfs_obj.getZpoolIoStatEmbedded
        else
          begin
            zpool_iostat := TFRE_DB_ZPOOL_IOSTAT.CreateForDB;
            zfs_obj.setZpoolIoStatEmbedded(zpool_iostat);
          end;
        zpool_iostat.Field('pss_func').asstring := func_s;
        zpool_iostat.Field('pss_func_ord').AsUInt64 := ps^.pss_func;
        zpool_iostat.Field('pss_state').asstring := state_s;
        zpool_iostat.Field('pss_start_time').AsDateTimeUTC := ps^.pss_start_time*1000;
        zpool_iostat.Field('pss_end_time').AsDateTimeUTC := ps^.pss_end_time*1000;
        zpool_iostat.Field('pss_to_examine').AsUInt64 := ps^.pss_to_examine;
        zpool_iostat.Field('pss_examined').AsUInt64 := ps^.pss_examined;
        zpool_iostat.Field('pss_to_process').AsUInt64 := ps^.pss_to_process;
        zpool_iostat.Field('pss_processed').AsUInt64 := ps^.pss_processed;
        zpool_iostat.Field('pss_errors').AsUInt64 := ps^.pss_errors;
        zpool_iostat.Field('pss_pass_exam').AsUInt64 := ps^.pss_pass_exam;
        zpool_iostat.Field('pss_pass_start').AsUInt64 := ps^.pss_pass_start;
      end;
  end;

  procedure fetch_zfs_state(const zfs_obj: TFRE_DB_ZFS_OBJ; const  elem : Pnvpair_t);
  var vs           : Pvdev_stat_t;
      c            : Uint64_t=0;
      uia          : PPuint64_t=nil;
      i            : integer;
      state_s      : string='';
      aux_s        : string='';
      type_s       : string='';
      zpool_iostat : TFRE_DB_ZPOOL_IOSTAT;
  begin
    if FOSNVGET_U64ARR(elem,uia,c) then
      begin
        if zfs_obj.FieldExists('zpooliostat') then
          zpool_iostat := zfs_obj.getZpoolIoStatEmbedded
        else
          begin
            zpool_iostat := TFRE_DB_ZPOOL_IOSTAT.CreateForDB;
            zfs_obj.setZpoolIoStatEmbedded(zpool_iostat);
          end;
        vs := Pvdev_stat_t(uia);
        WriteStr(state_s,vdev_state(vs^.vs_state));
        zpool_iostat.Field('state').asstring := state_s;
        zpool_iostat.Field('state_ord').asUint64 := vs^.vs_state;
        zpool_iostat.Field('timestamp').AsUint64 := vs^.vs_timestamp;
        zfs_obj.Field('aux').asstring := state_s;
        WriteStr(aux_s,vdev_aux(vs^.vs_aux));
        zfs_obj.Field('aux').asstring := aux_s;
        zfs_obj.Field('aux_ord').asUint64 := vs^.vs_aux;
        zpool_iostat.Field('alloc').asUint64 := vs^.vs_alloc;
        zpool_iostat.Field('space').asUint64 := vs^.vs_space;
        zpool_iostat.Field('dspace').asUint64 := vs^.vs_dspace;
        zpool_iostat.Field('rsize').asUint64 := vs^.vs_rsize;
        zpool_iostat.Field('esize').asUint64 := vs^.vs_esize;
        zpool_iostat.Field('r_errors').asUint64 := vs^.vs_read_errors;
        zpool_iostat.Field('w_errors').asUint64 := vs^.vs_write_errors;
        zpool_iostat.Field('c_errors').asUint64 := vs^.vs_checksum_errors;
        zpool_iostat.Field('scan_removing').asUint64 := vs^.vs_scan_removing;
        zpool_iostat.Field('scan_processed').asUint64 := vs^.vs_scan_processed;
        for i:=0 to ord(ZIO_TYPES)-1 do
          begin
            WriteStr(type_s,zio_type_t(i));
            zpool_iostat.Field(type_s+'_OPS').AsUInt64   := vs^.vs_ops[i];
            zpool_iostat.Field(type_s+'_BYTES').AsUInt64 := vs^.vs_bytes[i];
          end;
      end;
  end;


  function _createLog : TFRE_DB_ZFS_LOG;
  begin
    zlog:=pool.createLogEmbedded;
    zlog.setZFSGuid('LOG'+pool.getZFSGuid);
    zlog.setname('logs');
    result := zlog;
  end;


  procedure parse_nvlist(const list : Pnvlist_t ;  indent : cint; const parent_obj: TFRE_DB_ZFS_OBJ);
   var elem       : Pnvpair_t  = nil;
       nvlist     : Pnvlist_t  = nil;
       nvlistarr  : PPnvlist_t = nil;
       count      : uint_t=0;
       u64val     : UInt64=0;
       strval     : string='';
       i          : integer;
       name       : string;
       actual_obj : TFRE_DB_ZFS_OBJ;
       temp_obj   : TFRE_DB_ZFS_OBJ;
       newparent_obj  : TFRE_DB_ZFS_OBJ;
       vtype      : string;
       rl        : TFRE_DB_ZFS_RAID_LEVEL;
   begin
     actual_obj := parent_obj;
     temp_obj   := nil;
     if parsestate = psSubVdev then
       temp_obj  := TFRE_DB_ZFS_OBJ.CreateForDB;  { temporary object }
     try
       repeat
  //       writeln(StringOfChar(' ',indent),'NVLIST ',actual_obj.ClassName,' ',actual_obj.UID_String);
         elem := nvlist_next_nvpair(list, elem);
         if not assigned(elem) then
           exit;
         name := FOSNVPAIR_NAME(elem);
         case nvpair_type(elem) of
           DATA_TYPE_STRING,DATA_TYPE_UINT64 : begin
                 if parsestate=psVdev then
                   begin
                     _SetFieldFromNVElement(actual_obj,elem);
                   end
                 else if parsestate=psVdevTree then
                   begin
                     if name=ZPOOL_CONFIG_TYPE then
                       begin
                         FOSNVGET_STRING(elem,strval);
                         case strval of
                           VDEV_TYPE_ROOT :
                             begin
                               actual_obj:=pool.createDatastorageEmbedded;
                               actual_obj.SetName(VDEV_TYPE_ROOT);
                               parsestate:=psVdev;
                             end
                         else
                             raise EFRE_DB_Exception.Create('unknown type of vdev in vdev_tree '+strval);
                         end;
                       end
                     else
                       raise EFRE_DB_Exception.Create('first element of vdev_tree is not type as excpected :'+name);
                   end
                 else if parsestate=psSubVdev then
                   begin
                     _SetFieldFromNVElement(temp_obj,elem);
                     if (temp_obj.FieldExists(ZPOOL_CONFIG_TYPE) and temp_obj.FieldExists('zfs_guid')) then
                       begin
                         vtype := temp_obj.Field(ZPOOL_CONFIG_TYPE).asstring;
                         case vtype of
                         VDEV_TYPE_RAIDZ,VDEV_TYPE_MIRROR :
                           begin
                             if temp_obj.FieldExists(ZPOOL_CONFIG_IS_LOG) then
                               begin
                                 if temp_obj.Field(ZPOOL_CONFIG_IS_LOG).AsUInt64=0 then
                                   begin
                                     actual_obj:=(parent_obj as TFRE_DB_ZFS_VDEVCONTAINER).createVdevEmbedded(temp_obj.Field('zfs_guid').asstring);
                                     actual_obj.SetAllSimpleObjectFieldsFromObject(temp_obj);
                                     if vtype=VDEV_TYPE_MIRROR then
                                       begin
                                        (actual_obj as TFRE_DB_ZFS_DISKCONTAINER).raidlevel:=zfs_rl_mirror;
                                        actual_obj.SetName(VDEV_TYPE_MIRROR);
                                       end
                                     else
                                       begin
                                         case actual_obj.Field(ZPOOL_CONFIG_NPARITY).AsUInt64 of
                                           1: rl := zfs_rl_z1;
                                           2: rl := zfs_rl_z2;
                                           3: rl := zfs_rl_z3;
                                         else
                                           raise EFRE_DB_Exception.Create('invalid parity nparity '+inttostr(actual_obj.Field(ZPOOL_CONFIG_NPARITY).AsUInt64));
                                         end;
                                         (actual_obj as TFRE_DB_ZFS_DISKCONTAINER).RaidLevel:=rl;
                                         actual_obj.SetName(VDEV_TYPE_RAIDZ+inttostr(actual_obj.Field(ZPOOL_CONFIG_NPARITY).AsUInt64));
                                       end
                                   end
                                 else
                                   begin
                                     actual_obj := _createLog;
                                     actual_obj :=(actual_obj as TFRE_DB_ZFS_VDEVCONTAINER).createVdevEmbedded(temp_obj.Field('zfs_guid').asstring);
                                     actual_obj.SetAllSimpleObjectFieldsFromObject(temp_obj);
                                     if vtype=VDEV_TYPE_MIRROR then
                                       begin
                                        (actual_obj as TFRE_DB_ZFS_DISKCONTAINER).raidlevel:=zfs_rl_mirror;
                                        actual_obj.SetName(VDEV_TYPE_MIRROR);
                                       end;
                                   end;
                                 parsestate:=psVdev;
                               end;
                           end;
                         VDEV_TYPE_DISK, VDEV_TYPE_FILE :
                           begin
                             actual_obj:=(actual_obj as TFRE_DB_ZFS_DISKCONTAINER).createBlockdeviceEmbedded(temp_obj.Field('zfs_guid').asstring);
                             actual_obj.SetAllSimpleObjectFieldsFromObject(temp_obj);
                             parsestate:=psVdev;
                           end;
                         VDEV_TYPE_SPARE :
                           begin
                             actual_obj:=(parent_obj as TFRE_DB_ZFS_VDEV).createDiskSpareContainerEmbedded(temp_obj.Field('zfs_guid').asstring);
                             actual_obj.setname(VDEV_TYPE_SPARE);
                             actual_obj.SetAllSimpleObjectFieldsFromObject(temp_obj);
                             parsestate:=psVdev;
                           end;
                         VDEV_TYPE_REPLACING :
                           begin
                             actual_obj:=(parent_obj as TFRE_DB_ZFS_VDEV).createDiskReplaceContainerEmbedded(temp_obj.Field('zfs_guid').asstring);
                             actual_obj.setname(VDEV_TYPE_REPLACING);
                             actual_obj.SetAllSimpleObjectFieldsFromObject(temp_obj);
                             parsestate:=psVdev;
                           end;
                         VDEV_TYPE_HOLE :
                           begin
                             //ignoring hole
                           end;
                         else
                             raise EFRE_DB_Exception.Create('unknown type of children vdev:'+vtype);
                       end;
                     end;
                   end;
               end;
           DATA_TYPE_NVLIST :
             begin
               if name=ZPOOL_CONFIG_VDEV_TREE then
                 parsestate:=psVdevTree;

               if FOSNVGET_NVLIST(elem,nvlist) then
                 begin
                   //writeln(StringOfChar(' ',indent),'L',FOSNVPAIR_NAME(elem));
                   parse_nvlist(nvlist,indent+4,actual_obj);
                 end;
             end;
           DATA_TYPE_NVLIST_ARRAY :
             begin
               if FOSNVGET_NVARR(elem,nvlistarr,count) then
                 if name=ZPOOL_CONFIG_SPARES then
                   begin
                     newparent_obj := pool.createSpareEmbedded;
                     newparent_obj.setZFSGuid('SPARE'+pool.getZFSGuid);
                     newparent_obj.setname('spares');
                   end
                 else if name=ZPOOL_CONFIG_L2CACHE then
                   begin
                     newparent_obj := pool.createCacheEmbedded;
                     newparent_obj.setZFSGuid('CACHE'+pool.getZFSGuid);
                     newparent_obj.setname('cache');
                   end
                 else
                   newparent_obj := actual_obj;
                 for i := 0 to count-1 do
                   begin
  //                   writeln(StringOfChar(' ',indent),'A',FOSNVPAIR_NAME(elem),'[',i,']');
                     if (name=ZPOOL_CONFIG_CHILDREN) or (name=ZPOOL_CONFIG_SPARES) or (name=ZPOOL_CONFIG_L2CACHE) then
                       parsestate:=psSubVdev;
                     parse_nvlist(nvlistarr[i],indent+8,newparent_obj);
                   end;
           end;
           DATA_TYPE_UINT64_ARRAY :
               begin
                 name := FOSNVPAIR_NAME(elem);
                 if name = ZPOOL_CONFIG_VDEV_STATS then
                   fetch_zfs_state(actual_obj,elem)
                 else if name = ZPOOL_CONFIG_SCAN_STATS then
                   begin
                     if (actual_obj is TFRE_DB_ZFS_DATASTORAGE) then begin
                       fetch_zfs_scan_stats(pool,elem);
                     end;
                   end
                 else
                   begin
                     // skipping
                     // writeln(StringOfChar(' ',indent),name,' <array of uint64_t>');
                   end;
               end;
           DATA_TYPE_BOOLEAN :
               begin
                 { skip here to cleanup output (feature flags) }
               end;
           else
             begin
               writeln('unhandled config type ',nvpair_type(elem),' for name=',FOSNVPAIR_NAME(elem));
             end;
         end;
       until false;
     finally
       if assigned(temp_obj) then
         temp_obj.Finalize;
     end;
   end;

  procedure _checkLogDevices;
  var ds: TFRE_DB_ZFS_DATASTORAGE;

    procedure _checkdevices(const fld: IFRE_DB_Field);
    var dev    : TFRE_DB_ZFS_BLOCKDEVICE;
        co     : IFRE_DB_Object;
    begin
      if fld.FieldType=fdbft_Object then
        if fld.AsObject.isA(TFRE_DB_ZFS_BLOCKDEVICE,dev) then
          begin
            if dev.FieldExists(ZPOOL_CONFIG_IS_LOG) then
              if dev.Field(ZPOOL_CONFIG_IS_LOG).AsUInt64=1 then
                begin
                  //writeln('SWL: FOUND BLOCK LOG');
                  if not assigned(zlog) then
                    _createLog;
                  co := fld.CheckOutObject;
                  //writeln('SWL: ADDING EMBEDDED');
                  zlog.addBlockdeviceEmbedded(co.Implementor_HC as TFRE_DB_ZFS_BLOCKDEVICE);
                end;
          end;
    end;

  begin
    ds := pool.GetDatastorageEmbedded;
    ds.ForAllFields(@_checkdevices);
  end;

begin
  parsestate := psUndefined;
  zp := zpool_open(GILLUMOS_LIBZFS_HANDLE, Pchar(@poolname[1]));
  try
    if not assigned(zp) then
      begin
        error :='Could not open pool '+poolname;
        exit(-1);
      end;
    zlog       := nil;
    pool       := TFRE_DB_ZFS_POOL.Create;
    parsestate := psVdev;
    zs         := zpool_get_config(zp, nil);
    pool.Field('config_ts').AsDateTimeUTC:=GFRE_DT.Now_UTC;
    parse_nvlist(zs, 4, pool);
    _checkLogDevices;
    outpool := pool;
    result     :=0;
  finally
    if assigned(zp) then
      zpool_close(zp);
  end;
end;


function _zpoolGetActiveIterator(_ph:Pzpool_handle_t; _para2:pointer):cint;cdecl;
var poolname   : string;
    pcpoolname : pchar;
    poolstate  : cint;
    poolstate_s: string;
    pools      : IFRE_DB_Object;
    zfs_guid   : Uint64;
    src        : zprop_source_t;
    pool       : IFRE_DB_Object;
begin
//  writeln('SWL:POOLHANDLE',Int64(_ph),' ',int64(_para2));
  poolstate  := zpool_get_state(_ph);

  if pool_state_t(poolstate)=POOL_STATE_ACTIVE then
    begin
      pcpoolname := zpool_get_name(_ph);
      poolname   := StrPas(PChar(pcpoolname));

      WriteStr(poolstate_s,pool_state_t(poolstate));

      src := ZPROP_SRC_DEFAULT;
      zfs_guid   := zpool_get_prop_int(_ph,ZPOOL_PROP_GUID,@src);

      pool                              := GFRE_DBI.NewObject;
      pool.Field('zfs_guid').asstring   := inttostr(zfs_guid);
      pool.Field('state').asstring      := poolstate_s;
      pool.Field('state_ord').asUint64  := poolstate;
      pool.Field('name').asstring       := poolname;

      pools := IFRE_DB_Object(_para2);
      pools.Field(inttostr(zfs_guid)).AsObject := pool;
    //  writeln(poolname,' ',poolstate_s,' ',zfs_guid);
    end;
  zpool_close(_ph);
end;


function fosillu_zfs_GetActivePoolsDBO(out error: string; out pools: IFRE_DB_Object): integer;
var res: cint;
begin
  pools := GFRE_DBI.NewObject;
  res   := zpool_iter(GILLUMOS_LIBZFS_HANDLE,@_zpoolGetActiveIterator,pools);
end;



end.

