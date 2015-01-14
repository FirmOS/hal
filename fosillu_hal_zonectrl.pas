unit fosillu_hal_zonectrl;

{$mode objfpc}{$H+}
{$codepage UTF8}

interface

uses
  Classes, SysUtils,unix,ctypes,unixtype,fos_illumos_defs,fosillu_priv,fosillu_libzonecfg,fosillu_priv_names,
  fos_tool_interfaces,fosillu_libzfs,fre_db_interface,fosillu_hal_dbo_zfs_dataset,fre_process,fre_testcase;

const
  CMD_HELP      =   0;
  CMD_BOOT      =   1;
  CMD_HALT      =   2;
  CMD_READY     =   3;
  CMD_SHUTDOWN  =   4;
  CMD_REBOOT    =   5;
  CMD_LIST      =   6;
  CMD_VERIFY    =   7;
  CMD_INSTALL   =   8;
  CMD_UNINSTALL =   9;
  CMD_MOUNT     =  10;
  CMD_UNMOUNT   =  11;
  CMD_CLONE     =  12;
  CMD_MOVE      =  13;
  CMD_DETACH    =  14;
  CMD_ATTACH    =  15;
  CMD_MARK      =  16;
  CMD_APPLY     =  17;
  CMD_SYSBOOT   =  18;
  CMD_MIN       =  CMD_HELP;
  CMD_MAX       =  CMD_SYSBOOT;

type
  zone_entry_t=record
    valid      : boolean;
    error      : string;
    zid        : zoneid_t;
    zname      : string;
    zstate     : string;
    zstate_num : zone_state_t;
    zbrand     : string;
    zroot      : string;
    zuuid      : string;
    ziptype    : zone_iptype_t;
  end;

  PZone_entry_t = ^zone_entry_t;

var zents  : array of zone_entry_t;
    nzents : uint_t;

procedure sanity_check(zone : string; cmd_num : integer; running, unsafe_when_running, force : boolean);

procedure list_zones;
function  fre_install_zone  (const zone_dbo:IFRE_DB_Object; const job:TFRE_DB_Job): IFRE_DB_Object;
function  fre_create_zonecfg(const zone_dbo:IFRE_DB_Object; const job:TFRE_DB_Job): IFRE_DB_Object;
function  fre_boot_zone     (const zone_dbo:IFRE_DB_Object; const job:TFRE_DB_Job): IFRE_DB_Object;
function  fre_halt_zone     (const zone_dbo:IFRE_DB_Object; const job:TFRE_DB_Job): IFRE_DB_Object;
function  fre_shutdown_zone (const zone_dbo:IFRE_DB_Object; const job:TFRE_DB_Job): IFRE_DB_Object;
function  fre_destroy_zone  (const zone_dbo:IFRE_DB_Object; const job:TFRE_DB_Job): IFRE_DB_Object;

function  fre_set_zonestate (const zonename:string; const state:uint_t): boolean;

implementation

procedure add_filesystem      (const handle: zone_dochandle_t; const fs_dir,fs_type,fs_special:string;const job:TFRE_DB_JOB);
var
  fstab           : zone_fstab;
  err             : integer;
  msg             : string;
begin
 //zone_fs_special : array[0..(MAXPATHLEN)-1] of cchar;
 //zone_fs_dir : array[0..(MAXPATHLEN)-1] of cchar;
 //zone_fs_type : array[0..(FSTYPSZ)-1] of cchar;
 //zone_fs_options : Pzone_fsopt_t;
 //zone_fs_raw : array[0..(MAXPATHLEN)-1] of cchar;

//  writeln('adding fs');
  FillByte(fstab,sizeof(fstab),0);
  StrPLCopy(Pchar(@fstab.zone_fs_dir),fs_dir,MAXPATHLEN);
  StrPLCopy(Pchar(@fstab.zone_fs_type),fs_type,FSTYPSZ);
  StrPLCopy(PChar(@fstab.zone_fs_special),fs_special,MAXPATHLEN);

//  Writeln(Strpas(@fstab.zone_fs_dir));
  err := zonecfg_add_filesystem(handle,@fstab);
  if err<>Z_OK then
    begin
      msg := StrPas(zonecfg_strerror(err));
      raise Exception.Create('error on adding fs :'+inttostr(err)+' '+msg);
    end;
  if Assigned(job) then job.AddProgressLog('Added Filesystem '+fs_dir+' '+fs_special);
end;

procedure add_device       (const handle: zone_dochandle_t; const dev_match : string);
var
  devtab          : zone_devtab;
  err             : integer;
  msg             : string;
begin
 //zone_dev_match : array[0..(MAXPATHLEN)-1] of cchar;
 FillByte(devtab,sizeof(devtab),0);
 StrPLCopy(Pchar(@devtab.zone_dev_match),dev_match,MAXPATHLEN);
// writeln('adding device');

 err := zonecfg_add_dev(handle,@devtab);
 if err<>Z_OK then
   begin
     msg := StrPas(zonecfg_strerror(err));
     raise Exception.Create('error on adding device :'+inttostr(err)+' '+msg);
   end;
end;

procedure add_dataset      (const handle: zone_dochandle_t; const ds_name : string; const job:TFRE_DB_JOB);
var
  err             : integer;
  msg             : string;
  dstab           : zone_dstab;
begin
// zone_dataset_name : array[0..(MAXNAMELEN)-1] of cchar;
  FillByte(dstab,sizeof(dstab),0);
  StrPLCopy(Pchar(@dstab.zone_dataset_name),ds_name,MAXNAMELEN);
  writeln('adding dataset');
  err := zonecfg_add_ds(handle,@dstab);
  if err<>Z_OK then
    begin
      msg := StrPas(zonecfg_strerror(err));
      raise Exception.Create('error on adding dataset :'+inttostr(err)+' '+msg);
    end;
  if Assigned(job) then job.AddProgressLog('Added Dataset '+ds_name);
end;

procedure lookup_zone_info(const zone_name:string; zid : zoneid_t ; var zent : zone_entry_t);
var uuid   : uuid_t;
    scratch: string;
    flags  : cint;
    handle : zone_dochandle_t;
begin
 uuid := Default(uuid_t);
 zent.zname  := zone_name;
 zent.zbrand := '?';
 zent.zroot  := '?';
 zent.zstate := '?';
 zent.valid  := true;
 zent.zid    := zid;
 try
   if zonecfg_get_uuid(Pchar(zone_name),uuid)=0 then
     begin
       zent.zuuid := GFRE_BT.GUID_2_HexString(uuid);
     end;
    SetLength(scratch,MAXPATHLEN);
    if zone_get_zonepath(Pchar(zone_name),@scratch[1],Length(scratch))<>0 then
      raise Exception.create('cannot get zonepath');
 //   zent.zroot:=copy(pchar(scratch),1,maxint);
    zent.zroot := pchar(scratch);

    if ((zone_get_state(pchar(zone_name), @zent.zstate_num)) <> Z_OK) then
      raise Exception.create('cannot get zonestate');
    zent.zstate := zone_state_str(zent.zstate_num);

    SetLength(scratch,256);
    if zone_get_brand(pchar(zone_name),@scratch[1],length(scratch))<>0 then
      raise Exception.create('cannot get zonebrand');
    zent.zbrand := copy(pchar(scratch),1,maxint);
    if (zent.zid=ZONE_ID_UNDEFINED) and (zent.zstate_num=ZONE_STATE_RUNNING) then
       zent.zid := getzoneidbyname(Pchar(zone_name));
    if zent.zid=GLOBAL_ZONEID then
      begin
        zent.ziptype:=ZS_SHARED;
        exit;
      end;

    if (zent.zstate_num=ZONE_STATE_RUNNING) and (zent.zid<>ZONE_ID_UNDEFINED) then
      begin
         if zone_getattr(zent.zid,ZONE_ATTR_FLAGS,@flags,sizeof(flags))<=0 then
           raise Exception.create('cannot get zone attr');
         if (flags and ZF_NET_EXCL)=ZF_NET_EXCL then
           zent.ziptype:=ZS_EXCLUSIVE
         else
           zent.ziptype:=ZS_SHARED;
        exit;
      end;
      handle := zonecfg_init_handle();
      if handle = Nil then
        raise Exception.create('could not init handle');

      if zonecfg_get_handle(pchar(zone_name), handle)<>0 then
        begin
          zonecfg_fini_handle(handle);
          raise Exception.create('could not get handle');
        end;

      if zonecfg_get_iptype(handle,@zent.ziptype)<>0 then
        begin
          zonecfg_fini_handle(handle);
          raise Exception.create('could not get iptype');
        end;
      zonecfg_fini_handle(handle);
  except
    on e:exception do
      begin
        zent.valid:=false;
        zent.error:=e.Message;
      end;
  end;
end;


function fetch_zents():boolean;
var zids         : array of zoneid_t;
    nzents_saved : uint_t;
    i,retv       : cint;
    zentp        : PZone_entry_t;
    name         : string;
    label again;
begin
  result := false;
  if (nzents > 0) then
    exit(true);
 if zone_list(nil, nzents)<>0 then
   raise EXception.create('failed to get zoneid list');

again:
  if (nzents =0) then
    exit(true);

  SetLength(zids,nzents);
  nzents_saved := nzents;

  if (zone_list(@zids[0], nzents) <> 0) then
    raise Exception.Create('failed to get zonelist');


 if (nzents <> nzents_saved) then
   goto again;

 SetLength(zents,nzents);

 for i:=0 to high(zents) do
   begin
     name := fos_getzonenamebyid(zids[i]);
     if name='' then
       begin
         zents[i].valid := false;
         continue;
       end;
     lookup_zone_info(name,zids[i],zents[i]);
   end;
end;

procedure lookup_running_zone(const str:string;var result:zone_entry_t);
var i : integer;
begin
  result := default(zone_entry_t);
  if not fetch_zents then
    exit;
  for i := 0 to high(zents) do
    begin
      if str=zents[i].zname then
        begin
          result :=zents[i];
          break;
        end;
    end;
end;

procedure sanity_check(zone : string; cmd_num : integer; running, unsafe_when_running, force : boolean);
var
  zent             : zone_entry_t;
  privset          : Ppriv_set_t ;
  state, min_state : zone_state_t ;
  kernzone         : string[ZONENAME_MAX];
  fp               : FILE;
begin
 if getzoneid<>GLOBAL_ZONEID then
   raise Exception.Create('must be in global zone to use administrative functions');
 privset := nil;
 privset := priv_allocset;
 if not assigned(privset) then
   raise Exception.Create('could not allocate privset');
 if getppriv(PRIV_EFFECTIVE,privset)<>0 then
   raise Exception.Create('privset');
 if priv_isfullset(privset)=B_FALSE then
   raise Exception.Create('must be a privileged user');
 writeln('SANITY OK');
 priv_freeset(privset);

 if (zonecfg_in_alt_root=B_TRUE) then
   raise Exception.Create('zone in alternate root not supported');
 lookup_running_zone(zone,zent);
 if running and (not force) then
   begin
     if zent.valid=false then
       raise Exception.Create('zone not running');
   end
 else
   begin
     //if (unsafe_when_running && zent != NULL) {
     //        /* check whether the zone is ready or running */
     //        if ((err = zone_get_state(zent->zname,
     //            &zent->zstate_num)) != Z_OK) {
     //                errno = err;
     //                zperror2(zent->zname,
     //                    gettext("could not get state"));
     //                /* can't tell, so hedge */
     //                zent->zstate_str = "ready/running";
     //        } else {
     //                zent->zstate_str =
     //                    zone_state_str(zent->zstate_num);
     //        }
     //        zerror(gettext("%s operation is invalid for %s zones."),
     //            cmd_to_str(cmd_num), zent->zstate_str);
     //        return (Z_ERR);
     //}
     //if ((err = zone_get_state(zone, &state)) != Z_OK) {
     //        errno = err;
     //        zperror2(zone, gettext("could not get state"));
     //        return (Z_ERR);
     //}
     //switch (cmd_num) {
     //case CMD_UNINSTALL:
     //        if (state == ZONE_STATE_CONFIGURED) {
     //                zerror(gettext("is already in state '%s'."),
     //                    zone_state_str(ZONE_STATE_CONFIGURED));
     //                return (Z_ERR);
     //        }
     //        break;
     //case CMD_ATTACH:
     //        if (state == ZONE_STATE_INSTALLED) {
     //                zerror(gettext("is already %s."),
     //                    zone_state_str(ZONE_STATE_INSTALLED));
     //                return (Z_ERR);
     //        } else if (state == ZONE_STATE_INCOMPLETE && !force) {
     //                zerror(gettext("zone is %s; %s required."),
     //                    zone_state_str(ZONE_STATE_INCOMPLETE),
     //                    cmd_to_str(CMD_UNINSTALL));
     //                return (Z_ERR);
     //        }
     //        break;
     //case CMD_CLONE:
     //case CMD_INSTALL:
     //        if (state == ZONE_STATE_INSTALLED) {
     //                zerror(gettext("is already %s."),
     //                    zone_state_str(ZONE_STATE_INSTALLED));
     //                return (Z_ERR);
     //        } else if (state == ZONE_STATE_INCOMPLETE) {
     //                zerror(gettext("zone is %s; %s required."),
     //                    zone_state_str(ZONE_STATE_INCOMPLETE),
     //                    cmd_to_str(CMD_UNINSTALL));
     //                return (Z_ERR);
     //        }
     //        break;
     //case CMD_DETACH:
     //case CMD_MOVE:
     //case CMD_READY:
     //case CMD_BOOT:
     //case CMD_MOUNT:
     //case CMD_MARK:
     //        if ((cmd_num == CMD_BOOT || cmd_num == CMD_MOUNT) &&
     //            force)
     //                min_state = ZONE_STATE_INCOMPLETE;
     //        else if (cmd_num == CMD_MARK)
     //                min_state = ZONE_STATE_CONFIGURED;
     //        else
     //                min_state = ZONE_STATE_INSTALLED;
     //
     //        if (state < min_state) {
     //                zerror(gettext("must be %s before %s."),
     //                    zone_state_str(min_state),
     //                    cmd_to_str(cmd_num));
     //                return (Z_ERR);
     //        }
     //        break;
     //case CMD_VERIFY:
     //        if (state == ZONE_STATE_INCOMPLETE) {
     //                zerror(gettext("zone is %s; %s required."),
     //                    zone_state_str(ZONE_STATE_INCOMPLETE),
     //                    cmd_to_str(CMD_UNINSTALL));
     //                return (Z_ERR);
     //        }
     //        break;
     //case CMD_UNMOUNT:
     //        if (state != ZONE_STATE_MOUNTED) {
     //                zerror(gettext("must be %s before %s."),
     //                    zone_state_str(ZONE_STATE_MOUNTED),
     //                    cmd_to_str(cmd_num));
     //                return (Z_ERR);
     //        }
     //        break;
     //case CMD_SYSBOOT:
     //        if (state != ZONE_STATE_INSTALLED) {
     //                zerror(gettext("%s operation is invalid for %s "
     //                    "zones."), cmd_to_str(cmd_num),
     //                    zone_state_str(state));
     //                return (Z_ERR);
     //        }
     //        break;
     //}
   end;
end;

procedure list_zones;
var
    i      : Integer;
    cookie : PFILE;
    name   : string;
    cname  : PChar;
    zent   : zone_entry_t;

begin
 //fetch_zents;
 //for i := 0 to high(zents) do
 //  begin
 //    with zents[i] do
 //      writeln(valid,' ',error,' ',zid,' ',zname,' ',ziptype,' ',zroot,' ',zstate,' ',zstate_num,' ',zuuid,' ',zbrand);
 //  end;
   cookie := setzoneent;
   repeat
      cname := getzoneent(cookie);
      if not assigned(cname) then
        break;
      name  := string(cname);
      illu_Free(cname);
      lookup_zone_info(name,ZONE_ID_UNDEFINED,zent);
      with zent do
        writeln(valid,' ',error,' ',zid,' ',zname,' ',ziptype,' ',zroot,' ',zstate,' ',zstate_num,' ',zuuid,' ',zbrand);
   until false;
   endzoneent(cookie);
   //sleep(1000);
   //writeln;
   //writeln;
end;

function fre_install_zone(const zone_dbo: IFRE_DB_Object; const job: TFRE_DB_Job): IFRE_DB_Object;
var
  zone_caption    : string;
  domainname      : string;
  errs            : string;
  master_dataset  : string;
  master_dspath   : string;
  template_dataset: string;
  zone_dataset    : string;


  procedure       setzfsproperties(const dsname:string;const key,val:string);
  var
    errs          : string;
  begin
    fosillu_zfs_set_property(dsname,key,val,errs);
    if errs<>'' then
      begin
        raise Exception.Create('error on setting property '+key+' on '+dsname+' for zone :'+errs);
      end;
  end;

  procedure       create_dataset(const dsname:string);
  var
    errs          : string;
  begin
    fosillu_zfs_create_ds(dsname,errs);
    if errs<>'' then
      begin
        raise Exception.Create('error on creating '+dsname+' dataset for zone :'+errs);
      end;
    setzfsproperties(dsname,'fre:zonename',zone_caption);
    setzfsproperties(dsname,'fre:domainname',domainname);
    if assigned(job) then job.AddProgressLog('Created dataset '+dsname);
  end;

  procedure       clone_dataset(const snapshot_name:string; const dsname:string);
  var
    errs          : string;
  begin
    fosillu_zfs_clone_ds(snapshot_name,dsname,errs);
    if errs<>'' then
      begin
        raise Exception.Create('error on clone zone dataset '+dsname+'for zone :'+errs);
      end;
    setzfsproperties(dsname,'fre:zonename',zone_caption);
    setzfsproperties(dsname,'fre:domainname',domainname);
    if assigned(job) then job.AddProgressLog('Cloned dataset '+snapshot_name+' '+dsname);
  end;

begin
  zone_caption     := zone_dbo.Field('objname').asstring;
  domainname       := zone_dbo.Field('domainname').asstring;
  master_dataset   := zone_dbo.Field('masterdataset').asstring;
  master_dspath    := zone_dbo.Field('masterdatasetpath').asstring;
  template_dataset := zone_dbo.Field('templatedataset').asstring;
  zone_dataset     := zone_dbo.Field('zonedataset').asstring;

  if assigned(job) then job.AddProgressLog('Installing Zone '+zone_caption+' '+zone_dataset,65);

//  writeln('SWL: DOMAINDATASET');
  fosillu_zfs_create_ds(master_dataset+'/domains',errs);
//  writeln('SWL: DOMAINDATASET DONE');

  // ignore errors here
  fosillu_zfs_create_ds(master_dataset+'/domains/'+zone_dbo.DomainID.AsHexString,errs);
  // ignore errors here
  fosillu_zfs_create_ds(master_dataset+'/zones',errs);
  // ignore errors here
  fosillu_zfs_create_ds(master_dataset+'/zones/'+zone_dbo.UID.AsHexString,errs);
  // ignore errors here

//  writeln('SWL: ZONEDATASET');

  clone_dataset (template_dataset+'@final',zone_dataset);
  create_dataset(zone_dataset+'/vmdisk');

  create_dataset(zone_dataset+'/zonedata');
  create_dataset(zone_dataset+'/zonedata/vfiler');
  clone_dataset (template_dataset+'/etc@final',zone_dataset+'/zonedata/etc');
  clone_dataset (template_dataset+'/var@final',zone_dataset+'/zonedata/var');
  clone_dataset (template_dataset+'/optetc@final',zone_dataset+'/zonedata/optetc');
  clone_dataset (template_dataset+'/optfre@final',zone_dataset+'/zonedata/optfre');

  fre_set_zonestate(zone_dbo.UID_String,ZONE_STATE_INSTALLED);
  if assigned(job) then job.AddProgressLog('Zonestate set to INSTALLED',90);
end;

function fre_create_zonecfg(const zone_dbo: IFRE_DB_Object;const job: TFRE_DB_Job): IFRE_DB_Object;
var
  handle          : zone_dochandle_t;
  zone_template   : string;
  zone_name       : string;
  zone_path       : string;
  zone_dataset    : string;
  zone_brand      : string;
  zone_dbodataset :string;
  master_dataset  : string;
  master_dspath   : string;
  template_dataset: string;
  err             : integer;
  errs            : string;
  msg             : string;
  czonename       : Array[0..ZONENAME_MAX] of char;
  czonepath       : Array[0..MAXPATHLEN] of char;
  privs           : Ppriv_set_t;
  privname        : PChar;
  zone_caption    : string;
  domainname      : string;

begin
// writeln('SWL ZONE:',zone_dbo.DumpToString());
// writeln('SWL JOB:',job.DumpToString());

 zone_caption     := zone_dbo.Field('objname').asstring;
 domainname       := zone_dbo.Field('domainname').asstring;
 zone_name        := zone_dbo.UID.AsHexString;
 zone_path        := zone_dbo.Field('zonepath').asstring;
 zone_dataset     := zone_dbo.Field('zonedataset').asstring;
 master_dataset   := zone_dbo.Field('masterdataset').asstring;
 master_dspath    := zone_dbo.Field('masterdatasetpath').asstring;
 template_dataset := zone_dbo.Field('templatedataset').asstring;
 zone_brand       := 'fbz';

 handle := zonecfg_init_handle();
 if handle = Nil then
   raise Exception.create('could not init handle');

 zone_template:='SUNWblank';

 err := zonecfg_get_template_handle(PChar(zone_template),PChar(zone_name),handle);

 if err<>Z_OK then
   begin
     msg := StrPas(zonecfg_strerror(err));
     raise Exception.Create('error on zone creation :'+msg);
   end;

 // writeln('got template handle');

 err := zonecfg_set_zonepath(handle,PChar(zone_path));
 if err<>Z_OK then
   begin
     msg := StrPas(zonecfg_strerror(err));
     raise Exception.Create('error on setting zonepath :'+msg);
   end;

 err := zonecfg_set_brand(handle,PChar(zone_brand));
 if err<>Z_OK then
   begin
     msg := StrPas(zonecfg_strerror(err));
     raise Exception.Create('error on setting zonebrand :'+msg);
   end;

 err := zonecfg_set_iptype(handle,ZS_EXCLUSIVE);
 if err<>Z_OK then
   begin
     msg := StrPas(zonecfg_strerror(err));
     raise Exception.Create('error on setting ip type :'+msg);
   end;

 err := zonecfg_set_autoboot(handle,B_TRUE);
 if err<>Z_OK then
   begin
     msg := StrPas(zonecfg_strerror(err));
     raise Exception.Create('error on setting autoboot :'+msg);
   end;

 err:=zonecfg_get_name(handle,czonename,ZONENAME_MAX);
 if err<>Z_OK then
   begin
     msg := StrPas(zonecfg_strerror(err));
     raise Exception.Create('error on getting zonename :'+msg);
   end;

 if assigned(job) then job.AddProgressLog('Assigned zonename '+czonename+',15');

 add_filesystem(handle,'/etc','lofs',zone_path+'/zonedata/etc',job);
 add_filesystem(handle,'/var','lofs',zone_path+'/zonedata/var',job);
 add_filesystem(handle,'/opt/local/etc','lofs',zone_path+'/zonedata/optetc',job);
 add_filesystem(handle,'/opt/local/fre','lofs',zone_path+'/zonedata/optfre',job);
 add_filesystem(handle,'/vfiler','lofs',zone_path+'/zonedata/vfiler',job);
 add_filesystem(handle,'/zonedbo','lofs',master_dspath+'/zones/'+zone_dbo.UID.asHexstring,job);

 if assigned(job) then job.AddProgressLog('Added Datasets ',20);

 add_device (handle,'/dev/zvol/rdsk/'+zone_dataset+'/vmdisk/*');
 add_dataset(handle,zone_dataset+'/vmdisk',job);

 err := zonecfg_save(handle);
 if err<>Z_OK then
   begin
     msg := StrPas(zonecfg_strerror(err));
     raise Exception.Create('error on saving zone :'+inttostr(err)+' '+msg);
   end;

 if assigned(job) then job.AddProgressLog('Zonecfg saved in System',25);

 // copying zone dbo dataset
 writeln('SWL: Saving zones file ',master_dspath+'/zones/'+zone_dbo.UID.asHexstring+'/zone.dbo');
 zone_dbo.SaveToFile(master_dspath+'/zones/'+zone_dbo.UID.asHexstring+'/zone.dbo');

 if assigned(job) then job.AddProgressLog('Zonecfg saved to zone dbo file',30);

 fre_set_zonestate(zone_name,ZONE_STATE_CONFIGURED);

 if assigned(job) then job.AddProgressLog('Zonestate set to CONFIGURED',40);
end;

function fre_boot_zone(const zone_dbo: IFRE_DB_Object; const job: TFRE_DB_Job
  ): IFRE_DB_Object;
var
  zarg         : zone_cmd_arg;
  zone_name    : string;
  err          : integer;
  msg          : string;
begin
  writeln('SWL ZONE:',zone_dbo.DumpToString());
  zone_name     := zone_dbo.UID.AsHexString;

  FillByte(zarg,sizeof(zarg),0);
  StrPLCopy(Pchar(@zarg.bootbuf),'-m verbose',BOOTARGS_MAX);
  zarg.cmd      := Z_BOOT;

  err := zonecfg_call_zoneadmd(Pchar(zone_name),@zarg,Pchar('C'),B_TRUE);
  if err<>Z_OK then
    begin
      msg := StrPas(zonecfg_strerror(err));
      raise Exception.Create('error on booting zone :'+inttostr(err)+' '+msg);
    end;
end;

function fre_halt_zone(const zone_dbo: IFRE_DB_Object; const job: TFRE_DB_Job): IFRE_DB_Object;
var
  zarg         : zone_cmd_arg;
  zone_name    : string;
  err          : integer;
  msg          : string;
  czone_lock_env  : PChar;
begin
//  writeln('SWL ZONE:',zone_dbo.DumpToString());
  zone_name     := zone_dbo.UID.AsHexString;

  FillByte(zarg,sizeof(zarg),0);
  zarg.cmd      := Z_HALT;

//  writeln('init lock file');
  zonecfg_init_lock_file(PChar(zone_name),@czone_lock_env);

  writeln('SWL BEFORE CALL ZONEADMD');
  err := zonecfg_call_zoneadmd(Pchar(zone_name),@zarg,Pchar('C'),B_TRUE);
  if err<>Z_OK then
    begin
      msg := StrPas(zonecfg_strerror(err));
      raise Exception.Create('error on halting zone :'+inttostr(err)+' '+msg);
    end;
end;

function fre_shutdown_zone(const zone_dbo: IFRE_DB_Object;
  const job: TFRE_DB_Job): IFRE_DB_Object;
var
  zone_name    : string;
  cmd          : string;
begin
  writeln('SWL ZONE:',zone_dbo.DumpToString());
  zone_name     := zone_dbo.UID.AsHexString;
  FRE_ProcessCMDException('zlogin '+zone_name+' shutdown -i5 -g0 -y');
end;

function fre_destroy_zone(const zone_dbo: IFRE_DB_Object; const job: TFRE_DB_Job
  ): IFRE_DB_Object;
var
  zone_name    : string;
  zone_dataset : string;
  err          : integer;
  msg          : string;
  errs         : string;
begin
  writeln('SWL ZONE:',zone_dbo.DumpToString());
  zone_name     := zone_dbo.UID.AsHexString;
  zone_dataset  := zone_dbo.Field('zonedataset').AsString;

  err := zonecfg_destroy(Pchar(zone_name),B_TRUE);
  if err<>Z_OK then
    begin
      msg := StrPas(zonecfg_strerror(err));
      raise Exception.Create('error on destroying zone :'+inttostr(err)+' '+msg);
    end;

  fosillu_zfs_destroy_ds(zone_dataset,true,false,errs);
  if errs<>'' then
    begin
      raise Exception.Create('error on destroy zone dataset for zone :'+errs);
    end;
end;

function fre_set_zonestate(const zonename: string; const state: uint_t): boolean;
var
  lockfd          : NativeInt;
  czone_lock_env  : PChar;
  err             : integer;
  msg             : string;

begin
 result := false;

 writeln('init lock file');
 zonecfg_init_lock_file(PChar(zonename),@czone_lock_env);

 writeln('grab lock file');
 err := zonecfg_grab_lock_file(PChar(zonename),@lockfd);
 if err<>Z_OK then
   begin
     msg := StrPas(zonecfg_strerror(err));
     raise Exception.Create('error on grabbing lock file :'+inttostr(err)+' '+msg);
   end;
 try
   //err := zone_set_state(Pchar(zonename),ZONE_STATE_INCOMPLETE);
   //if err<>Z_OK then
   //  begin
   //    msg := StrPas(zonecfg_strerror(err));
   //    raise Exception.Create('error on setting state INCOMPLETE:'+inttostr(err)+' '+msg);
   //  end;
   //
   err := zone_set_state(Pchar(zonename), state);
   if err<>Z_OK then
     begin
       msg := StrPas(zonecfg_strerror(err));
       raise Exception.Create('error on setting state INSTALLED:'+inttostr(err)+' '+msg);
     end;

 finally
   writeln('release lock file');
   zonecfg_release_lock_file(PChar(zonename),lockfd);
 end;
 result :=true;
end;


end.

