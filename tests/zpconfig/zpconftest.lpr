program zpconftest;

{$mode objfpc}{$H+}

uses
  cthreads,nvpair,libzfs,zfs,Classes, fos_illumos_defs,ctypes,sysutils,strutils;

var
  zph : Plibzfs_handle_t;


  function FOSNVGET_U64(const elem:Pnvpair_t ; var val:UInt64):boolean;
  begin
    result := nvpair_value_uint64(elem,@val)=0;
  end;

  function FOSNVGET_U64ARR(const elem:Pnvpair_t ; var val:PPuint64_t ; var cnt : Uint64_t):boolean;
  begin
    result := nvpair_value_uint64_array(elem,@val,@cnt)=0;
  end;

  function FOSNVGET_STRING(const elem:Pnvpair_t ; var val:string):boolean;
  var s : Pchar;
  begin
    result := nvpair_value_string(elem,@s)=0;
    val    := s;
  end;

  function FOSNVGET_NVLIST(const elem:Pnvpair_t ; var nvlist : Pnvlist_t):boolean;
  begin
    result := nvpair_value_nvlist(elem,@nvlist)=0;
  end;

  function FOSNVGET_NVARR(const elem:Pnvpair_t ; var nvlistarr : PPnvlist_t ; var cnt : uint_t):boolean;
  begin
    result := nvpair_value_nvlist_array(elem,@nvlistarr,@cnt)=0;
  end;

  function FOSNVPAIR_NAME(const elem:Pnvpair_t):string;
  begin
    result := pchar(nvpair_name(elem));
  end;

{
 * Vdev statistics.  Note: all fields should be 64-bit because this
 * is passed between kernel and userland as an nvlist uint64 array.
  }
{ time since vdev load	 }
{ vdev state		 }
{ see vdev_aux_t	 }
{ space allocated	 }
{ total capacity	 }
{ deflated capacity	 }
{ replaceable dev size  }
{ expandable dev size  }
{ operation count	 }
{ bytes read/written	 }
{ read errors		 }
{ write errors		 }
{ checksum errors	 }
{ self-healed bytes	 }
{ removing?	 }
{ scan processed bytes	 }

 // vs_timestamp : hrtime_t;
 // vs_state : uint64_t;
 // vs_aux : uint64_t;
 // vs_alloc : uint64_t;
 // vs_space : uint64_t;
 // vs_dspace : uint64_t;
 // vs_rsize : uint64_t;
 // vs_esize : uint64_t;
 // vs_ops : array[0..ord(ZIO_TYPES)-1] of uint64_t;
 // vs_bytes : array[0..ord(ZIO_TYPES)-1] of uint64_t;
 // vs_read_errors : uint64_t;
 // vs_write_errors : uint64_t;
 // vs_checksum_errors : uint64_t;
 // vs_self_healed : uint64_t;
 // vs_scan_removing : uint64_t;
 // vs_scan_processed : uint64_t;

  procedure dump_zfs_state(const  elem : Pnvpair_t ; const indent : integer);
  var vs   : Pvdev_stat_t;
       c   : Uint64_t=0;
      uia  : PPuint64_t=nil;
      i    : integer;
      demo : string='';
  begin
    if FOSNVGET_U64ARR(elem,uia,c) then
      begin
        vs := Pvdev_stat_t(uia);
        WriteStr(demo,vdev_state(vs^.vs_state));
        WriteLn(StringOfChar(' ',indent),'--VDEV-STATS--');
        WriteLn(StringOfChar(' ',indent),'STATE           = ',vdev_state(vs^.vs_state),' :: ',demo);
        WriteLn(StringOfChar(' ',indent),'TIMESTAMP       = ',vs^.vs_timestamp);
        WriteLn(StringOfChar(' ',indent),'AUX             = ',vdev_aux(vs^.vs_aux));
        WriteLn(StringOfChar(' ',indent),'ALLOC           = ',(vs^.vs_alloc));
        WriteLn(StringOfChar(' ',indent),'SPACE           = ',(vs^.vs_space));
        WriteLn(StringOfChar(' ',indent),'DSPACE          = ',(vs^.vs_dspace));
        WriteLn(StringOfChar(' ',indent),'RSIZE           = ',(vs^.vs_rsize));
        WriteLn(StringOfChar(' ',indent),'ESIZE           = ',(vs^.vs_esize));
        WriteLn(StringOfChar(' ',indent),'DSPACE          = ',(vs^.vs_dspace));
        WriteLn(StringOfChar(' ',indent),'READ ERRORS     = ',(vs^.vs_read_errors));
        WriteLn(StringOfChar(' ',indent),'WRITE ERRORS    = ',(vs^.vs_write_errors));
        WriteLn(StringOfChar(' ',indent),'CHECKSUM ERRORS = ',(vs^.vs_checksum_errors));
        WriteLn(StringOfChar(' ',indent),'SCAN REMOVING   = ',(vs^.vs_scan_removing));
        WriteLn(StringOfChar(' ',indent),'SCAN PROCESSED  = ',(vs^.vs_scan_processed));
        for i:=0 to ord(ZIO_TYPES)-1 do
          begin
            WriteLn(StringOfChar(' ',indent+2),zio_type_t(i):15,' = ',vs^.vs_ops[i]:10,' / ',vs^.vs_bytes[i]);
          end;
      end;
  end;

{
	vdev_stat_t *vs;
	uint_t	c;

	VERIFY(nvpair_value_uint64_array(elem, (uint64_t **) &vs, &c) == 0);
	printf("%*sstate=%llu\n", indent, "", (u_longlong_t) vs->vs_state);
	printf("%*saux=%llu\n", indent, "", (u_longlong_t) vs->vs_aux);
	printf("%*s...\n", indent, "");
}

  procedure dump_nvlist(const list : Pnvlist_t ;  indent : cint);
  var elem      : Pnvpair_t  = nil;
      nvlist    : Pnvlist_t  = nil;
      nvlistarr : PPnvlist_t = nil;
      count     : uint_t=0;
      u64val    : UInt64=0;
      strval    : string='';
      i         : integer;
      name      : string;
  begin
    repeat
      elem := nvlist_next_nvpair(list, elem);
      if not assigned(elem) then
                    exit;
      case nvpair_type(elem) of
        DATA_TYPE_STRING :
            if FOSNVGET_STRING(elem,strval) then
              writeln(StringOfChar(' ',indent),FOSNVPAIR_NAME(elem),'=',strval);
        DATA_TYPE_UINT64 :
            if FOSNVGET_U64(elem,u64val) then
              writeln(StringOfChar(' ',indent),FOSNVPAIR_NAME(elem),'=',u64val);
        DATA_TYPE_NVLIST :
            if FOSNVGET_NVLIST(elem,nvlist) then
              begin
                writeln(StringOfChar(' ',indent),FOSNVPAIR_NAME(elem));
                dump_nvlist(nvlist,indent+4);
              end;
        DATA_TYPE_NVLIST_ARRAY :
            begin
              if FOSNVGET_NVARR(elem,nvlistarr,count) then
                for i := 0 to count-1 do
                  begin
                    writeln(StringOfChar(' ',indent),FOSNVPAIR_NAME(elem),'[',count,']');
                    dump_nvlist(nvlistarr[i],indent+8);
                  end;
            end;
        DATA_TYPE_UINT64_ARRAY :
            begin
              name := FOSNVPAIR_NAME(elem);
              if name = 'vdev_stats' then
                dump_zfs_state(elem,indent+4)
              else
                writeln(StringOfChar(' ',indent),name,' <array of uint64_t>')
            end;
        else
          begin
            writeln('unhandled config type ',nvpair_type(elem),' for name=',FOSNVPAIR_NAME(elem));
          end;
      end;
    until false;
  end;

  procedure dump_config(const pooln : string);
  var zp : Pzpool_handle_t;
      zs : Pnvlist_t;
  begin
    writeln(pooln);
    zp := zpool_open(zph, Pcchar(@pooln[1]));
    if not assigned(zp) then
      begin
        writeln('cannot open zpool ',pooln);
        exit;
      end;
    zs := zpool_get_config(zp, nil);
    dump_nvlist(zs, 4);
    if assigned(zp) then
      zpool_close(zp);
  end;

begin
  writeln('FOS ZPC Test');
  zph := nil;
  zph := libzfs_init();
  if paramstr(1)='' then
    dump_config('rpool')
  else
    dump_config(paramstr(1));
  libzfs_fini(zph);
end.

