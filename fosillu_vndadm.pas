unit fosillu_vndadm;

{$mode objfpc}{$H+}
{$codepage utf-8}

interface

uses
  Classes, SysUtils,fre_db_interface,
  fos_illumos_defs, fre_process,
  ctypes;

function  create_vnd        (const linkname : string ; const zonename: string; const devicename : string; out error : string):boolean;

implementation


function create_vnd(const linkname: string; const zonename: string; const devicename: string; out error: string): boolean;
var cmd : string;
    outstring, errorstring: string;
    res : integer;
begin
 cmd :='vndadm create ';
 if zonename<>'' then
   cmd := cmd+'-z '+zonename+' ';
 cmd := cmd+'-l '+linkname+' '+devicename;
 writeln('SWL: CREATE VND DEVICE ',cmd);
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

