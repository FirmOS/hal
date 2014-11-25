unit fosillu_dladm;

{$mode objfpc}{$H+}
{$codepage utf-8}
{$modeswitch advancedrecords }

interface

uses
  Classes, SysUtils,fre_db_interface,
  fos_illumos_defs,
  fosillu_libscf,
  fosillu_nvpair,
  fosillu_libdladm,
  ctypes, fosillu_sysnet_common;

var GILLU_DLADM : dladm_handle_t;

function  create_etherstub  (const linkname : string ; out error : string):boolean;
function  delete_etherstub  (const linkname : string ; out error : string):boolean;
function  create_vnic       (const vnic, for_linkname: string ; var macaddr: TFOS_MAC_ADDR; out error: String ; const for_zone:string='' ; const vid : cint = 0 ;const vrid : vrid_t = VRRP_VRID_NONE ; linkprops:string=''): boolean;
function  delete_vnic       (const linkname : string ; out error : string;const zonename:string=''):boolean;
function  rename_vnic       (const from_linkname,to_linkname : string ; out error:string ; for_zone : string=''):boolean;
function  vnic_set_linkprop (const linkname: string; out error: string; prop,val: string; const zonename: string): boolean;
function  get_linkprops     (const linkname : string ; out error:string ; for_zone : string=''):boolean;

implementation

function i_vnic_set_linkprop(const link : datalink_id_t ; prop,val: string ; out error:string):boolean;
var status : dladm_status_t;
    vall   : Pchar;
begin
  vall := Pchar(val);
  status := dladm_set_linkprop(GILLU_DLADM,link,pchar(prop),@vall,1,DLADM_OPT_ACTIVE);
  result := status=DLADM_STATUS_OK;
  if not result then
    begin
      error := 'SETLINKPROP: ['+prop+'='+val+'] failed ['+CDLADM_STATUS_CODES[status]+']';
    end;
end;

function vnic_set_linkprop(const linkname: string; out error: string; prop,val: string; const zonename: string): boolean;
begin
  abort;
end;

function get_linkprops(const linkname: string; out error: string; for_zone: string): boolean;
begin
  abort;
end;



function create_etherstub(const linkname: string; out error: string): boolean;
var flags    : UInt32_t;
    status   : dladm_status_t;
    lname    : Pchar;
    mac_addr : array [0..ETHERADDRLEN-1] of Uchar_t;
begin
  lname  := PChar(linkname);
  if (Length(linkname)>MAXLINKNAMELEN) or (dladm_valid_linkname(lname)<>B_TRUE) then
    begin
      error := 'CREATEETHERSTUB: invalid linkname ['+linkname+']';
      exit(false);
    end;

  flags  := DLADM_OPT_ANCHOR or DLADM_OPT_ACTIVE; { non persistent per default }

  status := dladm_vnic_create(GILLU_DLADM,lname,DATALINK_INVALID_LINKID,VNIC_MAC_ADDR_TYPE_AUTO,@mac_addr,ETHERADDRLEN,nil,0,0,VRRP_VRID_NONE,AF_UNSPEC,nil,nil,flags);

  result := status = DLADM_STATUS_OK;
  if not result then
    begin
      error := 'CREATEETHERSTUB: ['+linkname+'] failed ['+CDLADM_STATUS_CODES[status]+']';
      exit(false);
    end;
end;

function check_etherstub_vnic(const name:PChar ; const link_id : datalink_id_t;const  check_for_etherstub : Boolean ; const flags:UInt32_t ; out error:string):boolean;
var is_etherstub : boolean;
    attr         : dladm_vnic_attr_t;
begin
  if dladm_vnic_info(GILLU_DLADM,link_id,@attr,flags)<>DLADM_STATUS_OK then
    exit(true); { delete is ok, info not availlable }
  is_etherstub := (attr.va_link_id = DATALINK_INVALID_LINKID);
  if check_for_etherstub <> is_etherstub then
    if check_for_etherstub then
      begin
        error := name+' is not an etherstub';
        exit(false);
      end
    else
     begin
       error := name+' is not a vnic';
       exit(false);
     end;
  result := true;
end;

function i_delete_vnic(const linkname : string ; out error:string ; const for_vnic:boolean ; const zonename:pchar):boolean;
var flags    : UInt32_t;
    status   : dladm_status_t;
    lname    : Pchar;
    mac_addr : array [0..ETHERADDRLEN-1] of Uchar_t;
    linkid   : datalink_id_t;
begin
  lname  := PChar(linkname);
  flags  := DLADM_OPT_ACTIVE;
  status := dladm_zname2info(GILLU_DLADM,zonename,lname,@linkid,nil,nil,nil);
  if status<>DLADM_STATUS_OK then
    begin
      error := 'DELETEVNIC: ['+linkname+'] invalid ['+CDLADM_STATUS_CODES[status]+']';
      exit(false);
    end;
  if not check_etherstub_vnic(lname,linkid,not for_vnic,DLADM_OPT_ACTIVE,error) then
    exit(false);

  if assigned(zonename) then
    begin { must set linkprop zone=global to force down then vnic, the vnic remains in the zone: sort of inconsistent}
      if not i_vnic_set_linkprop(linkid,'zone','global',error) then
        exit;
    end;
  status := dladm_vnic_delete(GILLU_DLADM,linkid,flags);
  if status<>DLADM_STATUS_OK then
    if for_vnic then
      begin
        error := 'vnic delete ['+linkname+'] failed ['+CDLADM_STATUS_CODES[status]+']';
        exit(false);
      end
    else
      begin
        error := 'etherstub delete ['+linkname+'] failed ['+CDLADM_STATUS_CODES[status]+']';
        exit(false);
      end;
  result:=true;
  error:='';
end;

function delete_etherstub(const linkname: string; out error: string): boolean;
begin
  result := i_delete_vnic(linkname,error,false,nil);
end;


function create_vnic(const vnic, for_linkname: string; var macaddr: TFOS_MAC_ADDR; out error: String ; const for_zone:string='' ; const vid : cint = 0 ;const vrid : vrid_t = VRRP_VRID_NONE ; linkprops:string=''): boolean;
var linkid,dev_linkid : datalink_id_t;
    flags             : UInt32_t  = DLADM_OPT_ACTIVE;
    status            : dladm_status_t;
    mac_addr_type     : vnic_mac_addr_type_t = VNIC_MAC_ADDR_TYPE_UNKNOWN;
    af                : cint = AF_UNSPEC;
    proplist          : Pdladm_arg_list_t=nil;
    mac_addr          : Puchar_t;
    mac_slot          : cint=-1;
    name              : pchar;
    vnicname          : string;
begin
  if Length(for_linkname)>MAXLINKNAMELEN then
    begin
      error := 'linkname ['+vnic+'/'+for_linkname+'] is too long';
      exit(false);
    end;
  if vid<>0 then
    if (vid>4094) or (vid<1) then
      begin
        error := 'invalid vlan id ['+inttostR(vid)+']';
        exit(false);
      end;
  status := dladm_name2info(GILLU_DLADM,pchar(for_linkname),@linkid,nil,nil,nil);
  if status<>DLADM_STATUS_OK then
    begin
      error := 'CREATEVNIC: link ['+vnic+'/'+for_linkname+'] invalid ['+CDLADM_STATUS_CODES[status]+']';
      exit(false);
    end;
  if pos('zone',linkprops)>0 then
    begin
      error := 'CREATEVNIC: dont specify zone prop in linkprops, use the for_zone string';
      exit(false);
    end;
  vnicname := vnic;
  if for_zone<>'' then
    begin
      flags := flags+DLADM_OPT_TRANSIENT;
      vnicname := 'xx'+FREDB_GetRandomChars(20,rpm_OnlyChars)+'0';
      if linkprops<>'' then
        linkprops:='zone='+trim(for_zone)+','+linkprops
      else
        linkprops:='zone='+trim(for_zone);
    end;
  if linkprops<>'' then
    begin
      status := dladm_parse_link_props(pchar(linkprops),@proplist,B_FALSE);
      if status<>DLADM_STATUS_OK then
        begin
          error := 'CREATEVNIC: ['+vnic+'/'+for_linkname+' : '+linkprops+'] properties invalid ['+CDLADM_STATUS_CODES[status]+']';
          exit(false);
        end;
    end;
  try
    name          := pchar(vnicname);
    mac_addr_type := VNIC_MAC_ADDR_TYPE_FIXED;
    mac_addr      := macaddr.GetPointer;
    writeln('VID=',vid,' ',vrid);
    status        := dladm_vnic_create(GILLU_DLADM,name,linkid,mac_addr_type,mac_addr,6,@mac_slot,0,vid,vrid,af,@dev_linkid,proplist,flags);
    result        := status=DLADM_STATUS_OK;
    if not result then
      begin
        error := 'CREATEVNIC: ['+vnic+'/'+for_linkname+'] failed ['+CDLADM_STATUS_CODES[status]+']';
        exit(false);
      end
    else
      if for_zone<>'' then
        begin
          result := rename_vnic(vnicname,vnic,error,for_zone);
        end;
  finally
    if assigned(proplist) then
      dladm_free_props(proplist);
  end;
end;

function delete_vnic(const linkname: string; out error: string;const zonename:string): boolean;
var zname : pchar;
begin
  if zonename<>'' then
    zname:=pchar(zonename)
  else
    zname:=nil;
  result := i_delete_vnic(linkname,error,true,zname);
end;

function rename_vnic(const from_linkname, to_linkname: string; out error: string; for_zone: string): boolean;
var status : dladm_status_t;
begin
  status := dladm_rename_link(GILLU_DLADM,pchar(for_zone),pchar(from_linkname),pchar(to_linkname));
  result := status=DLADM_STATUS_OK;
  if not result then
    begin
      error := 'RENAMEVNIC: ['+for_zone+' '+from_linkname+'->'+to_linkname+'] failed ['+CDLADM_STATUS_CODES[status]+']';
    end;
end;



end.

