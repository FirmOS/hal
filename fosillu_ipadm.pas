unit fosillu_ipadm;

{$mode objfpc}{$H+}
{$codepage utf-8}

interface

uses
  Classes, SysUtils,fre_db_interface,
  fos_illumos_defs, fre_process,
  ctypes;

function  create_ipv4address(const linkname : string ; const addr_alias: string; const ip_hostsubnet:string; out error : string):boolean;
function  create_ipv6address(const linkname : string ; const addr_alias: string; const ip_hostsubnet:string; out error : string):boolean;
function  create_ipv6slaac  (const linkname : string ; const addr_alias: string; out error : string): boolean;
function  delete_ipaddress  (const linkname : string ; const addr_alias: string; out error : string): boolean;
function  add_ipv4routing   (const gateway  : string ; const ip_hostsubnet: string; out error : string):boolean;
function  delete_ipv4routing(const gateway  : string ; const ip_hostsubnet: string; out error : string):boolean;

implementation

function create_ipv4address(const linkname: string; const addr_alias: string; const ip_hostsubnet: string; out error: string): boolean;
var cmd : string;
    outstring, errorstring: string;
    res : integer;
begin
 cmd :='ipadm create-addr -t -T static -a '+ip_hostsubnet+' '+linkname+'/'+addr_alias;
 writeln('SWL: CREATE IPV4ADDRESS ',cmd);
 res := FRE_ProcessCMD(cmd,outstring,errorstring);
 if res=0 then
   begin
     result := true;
   end
 else
   begin
     result := false;
     error  := 'RES:'+inttostr(res)+' '+outstring +' '+ errorstring;
     writeln('SWL: ERROR ',error);
   end;
end;

function delete_ipaddress(const linkname: string; const addr_alias: string; out error: string): boolean;
var cmd : string;
    outstring, errorstring: string;
    res : integer;
begin
 cmd :='ipadm delete-addr '+linkname+'/'+addr_alias;
 writeln('SWL: DELETE IPADDRESS ',cmd);
 res := FRE_ProcessCMD(cmd,outstring,errorstring);
 if res=0 then
   begin
     result := true;
   end
 else
   begin
     result := false;
     error  := 'RES:'+inttostr(res)+' '+outstring +' '+ errorstring;
     writeln('SWL: ERROR ',error);
   end;
end;

function add_ipv4routing(const gateway: string; const ip_hostsubnet: string; out error: string): boolean;
var cmd : string;
    outstring, errorstring: string;
    res : integer;
begin
 cmd :='route add '+ip_hostsubnet+' '+gateway;
 writeln('SWL: ADD IPV4ROUTING ',cmd);
 res := FRE_ProcessCMD(cmd,outstring,errorstring);
 if res=0 then
   begin
     result := true;
   end
 else
   begin
     result := false;
     error  := 'RES:'+inttostr(res)+' '+outstring +' '+ errorstring;
     writeln('SWL: ERROR ',error);
   end;
end;

function delete_ipv4routing(const gateway: string; const ip_hostsubnet: string; out error: string): boolean;
var cmd : string;
    outstring, errorstring: string;
    res : integer;
begin
 cmd :='route delete '+ip_hostsubnet+' '+gateway;
 writeln('SWL: DELETE IPV4ROUTING ',cmd);
 res := FRE_ProcessCMD(cmd,outstring,errorstring);
 if res=0 then
   begin
     result := true;
   end
 else
   begin
     result := false;
     error  := 'RES:'+inttostr(res)+' '+outstring +' '+ errorstring;
     writeln('SWL: ERROR ',error);
   end;
end;

function create_ipv6address(const linkname: string; const addr_alias: string; const ip_hostsubnet: string; out error: string): boolean;
var cmd : string;
    outstring, errorstring: string;
    res : integer;
begin
 cmd :='ipadm create-addr -t -T static -a '+ip_hostsubnet+' '+linkname+'/'+addr_alias;
 writeln('SWL: CREATE IPV6ADDRESS ',cmd);
 res := FRE_ProcessCMD(cmd,outstring,errorstring);
 if res=0 then
   begin
     result := true;
   end
 else
   begin
     result := false;
     error  := 'RES:'+inttostr(res)+' '+outstring +' '+ errorstring;
     writeln('SWL: ERROR ',error);
   end;
end;

function create_ipv6slaac(const linkname: string; const addr_alias: string; out error: string): boolean;
var cmd : string;
    outstring, errorstring: string;
    res : integer;
begin
 cmd :='ipadm create-addr -t -T addrconf '+linkname+'/'+addr_alias;
 writeln('SWL: CREATE IPV6SLAAC ',cmd);
 res := FRE_ProcessCMD(cmd,outstring,errorstring);
 if res=0 then
   begin
     result := true;
   end
 else
   begin
     result := false;
     error  := 'RES:'+inttostr(res)+' '+outstring +' '+ errorstring;
     writeln('SWL: ERROR ',error);
   end;
end;

end.

