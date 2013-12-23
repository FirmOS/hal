unit fre_hal_redirect;

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
{$modeswitch nestedprocvars}
{$codepage utf-8}

interface

uses
  Classes, SysUtils,FOS_TOOL_INTERFACES,FRE_DB_INTERFACE,FRE_HAL_UTILS,process;

  type

   { TFRE_HAL_REDIRECT }

   TFRE_HAL_REDIRECT = class (TFRE_HAL_Service)
   protected
    function  ServiceSchemeName : string; override;
   public
    function  ConfigureService(const hal_object:IFRE_DB_Object) :IFRE_DB_Object; override;
    procedure GetServiceHAL   (const conn: IFRE_DB_Connection;const serviceobj: IFRE_DB_Object; const result_obj: IFRE_DB_Object); override;
   end;


implementation

{ TFRE_HAL_REDIRECT }

function TFRE_HAL_REDIRECT.ServiceSchemeName: string;
begin
  Result:='TFRE_DB_CAPTIVEPORTAL';
end;

function TFRE_HAL_REDIRECT.ConfigureService(const hal_object: IFRE_DB_Object): IFRE_DB_Object;
begin
end;

procedure TFRE_HAL_REDIRECT.GetServiceHAL(const conn: IFRE_DB_Connection; const serviceobj: IFRE_DB_Object; const result_obj: IFRE_DB_Object);
var coll   :IFRE_DB_Collection;
    cmsob  :IFRE_DB_Object;
    baseurl:string;
    ads    :IFRE_DB_Object;
    hlt    :boolean;

 procedure _findcms(const obj:IFRE_DB_object; var halt : boolean);
 begin
//  writeln(obj.SchemeClass);
//  writeln(obj.DumpToString());
  if FREDB_Guids_Same(obj.Field('servicegroup').AsObjectLink,serviceobj.Field('servicegroup').AsObjectLink) then begin
   if obj.SchemeClass='TFRE_DB_CMS' then begin
    cmsob:=obj;
    hlt:=true;
   end;
  end;
 end;



 procedure _addnetworks(const ep:IFRE_DB_Object);
 var i     :integer;
     j     :integer;
     nw_uid:TGUID;
     nw    :IFRE_DB_Object;
     rnwo  :IFRE_DB_Object;
     nn    :string;
     url   :string;
     referenced : TFRE_DB_GUIDArray;

     procedure _addredirection(const fname:string);
     var  rd     :IFRE_DB_Object;
          rd_uid :TGUID;
     begin
      if nw.FieldExists(fname)=false then exit;
      rd_uid:=nw.Field(fname).AsObjectLink;
      if conn.Fetch(rd_uid,rd)<>edb_OK then begin
       LogError('Could not find RedirectionPage '+GFRE_BT.GUID_2_HexString(rd_uid)+' for Network '+nw.UID_String);
       exit;
      end;
      url:=rd.Field('url').AsString;
      if rd.Field('relativeurl').AsBoolean then begin
//       if nn='10_220_250_' then begin
//        url:='http://wlanclocal.firmos.at'+url;
//       end else begin
        url:=baseurl+url;
//       end;
      end;
      rnwo.Field(fname).AddString(lowercase(url));
     end;

 begin
  referenced:=conn.GetReferences(ep.UID,false,'');
  for i:=0 to high(referenced) do begin
   if conn.Fetch(referenced[i],nw)<>edb_OK then begin
    LogError('Could not find Network '+GFRE_BT.GUID_2_HexString(nw_uid)+' for Endpoint '+ep.UID_String);
    continue;
   end;
   if nw.FieldExists('endpoint') then begin
    rnwo:=GFRE_DBI.NewObject;
    nn:=GetNetworkID(nw);

    if Pos('10_101_',nn)>0 then continue;

    rnwo.Field('sessiontimeout').AsUInt32:=nw.Field('sessiontimeout').AsUInt32;
    for j:=0 to nw.Field('urlexceptions').ValueCount-1 do begin
     rnwo.Field('urlexceptions').AddString(lowercase(nw.Field('urlexceptions').AsStringItem[j]));
    end;
    _addredirection('redirection_start');
    _addredirection('redirection_customer');
    _addredirection('redirection_agb');
    _addredirection('redirection_end');
//    writeln(nw.DumpToString());
    result_obj.Field(nn).AsObject:=rnwo;
   end;
  end;

 end;

 procedure _findsitecap(const obj:IFRE_DB_Object);
 var i     :integer;
     ep_uid:TGUID;
     ep    :IFRE_DB_Object;
     referenced: TFRE_DB_GUIDArray;
     site  :IFRE_DB_Object;
 begin
  if FREDB_Guids_Same(obj.Field('captiveportal').AsObjectLink,serviceobj.UID) then begin
//   writeln(obj.DumpToString());
   if conn.Fetch(obj.Field('site').AsObjectLink,site)<>edb_OK then begin
    LogError('Could not find Site for SiteExtension '+obj.UID_String);
    exit;
   end;
   referenced:=conn.GetReferences(site.UID,false,'');
   for i:=0 to high(referenced) do begin
    if conn.Fetch(referenced[i],ep)<>edb_OK then begin
     LogError('Could not find Endpoint for Site '+site.UID_String);
     exit;
    end;
    writeln(ep.SchemeClass);
    if ep.IsA('TFRE_DB_ENDPOINT') then begin
     _addnetworks(ep);
    end;
   end;
   setlength(referenced,0);
  end;
 end;

 procedure _getbasedata;
 var i:integer;
 begin
  result_obj.Field('baseurl').AsString:=baseurl;
  for i:=0 to cmsob.Field('urlexceptions').ValueCount-1 do begin
    result_obj.Field('urlexceptions').AddString(lowercase(cmsob.Field('urlexceptions').AsStringItem[i]));
  end;
  result_obj.Field('baseurl').AsString:=baseurl;
  for i:=0 to cmsob.Field('statetxt').ValueCount-1 do begin
    result_obj.Field('statetxt').AddString(cmsob.Field('statetxt').AsStringItem[i]);
  end;
 end;

 procedure _getcmspage(const obj:IFRE_DB_Object);
 var ao          : IFRE_DB_Object;
     url         : string;
     insertpoint : integer;
     i           : integer;
     j           : integer;
     nwg_uid     : TGUID;
     nwg         : IFRE_DB_Object;
     nn          : string;
     intnw       : IFRE_DB_Object;
     nowd        : TFRE_DB_DateTime64;
     nw          : IFRE_DB_Object;
     nw_uid      : TGUID;
     k           : integer;
     fn          : string;
     found       : boolean;
     ad_uid      : TGUID;
     c           : integer;
begin
  nowd:=GFRE_DT.Now_UTC;
  if FREDB_Guids_Same(obj.Field('cms').AsObjectLink,cmsob.uid) then begin
   if obj.SchemeClass='TFRE_DB_CMS_ADPAGE' then begin
    if obj.Field('active').AsBoolean then begin
     // preselect endtime
     if nowd>obj.Field('end_time').AsDateTimeUTC then begin
      exit;
     end;
     ao:=GFRE_DBI.NewObject;
     url:=obj.Field('url').AsString;
     if obj.Field('relativeurl').AsBoolean then begin
      url:=baseurl+url;
     end;
     insertpoint:=obj.Field('insertpoint').asint16;
     ao.Field('url').AsString:=url;
     ao.Field('max_inserts').AsUInt32   := obj.Field('max_inserts').AsUInt32;
     ao.Field('shown_inserts').asUint32 := obj.Field('shown_inserts').AsUInt32;
     ao.Field('start_daily').AsString   := obj.Field('start_daily').asstring;
     ao.Field('end_daily').AsString     := obj.Field('end_daily').AsString;
     ao.Field('start_time').AsdatetimeUTC   := obj.Field('start_time').asdatetimeutc;
     ao.Field('end_time').AsDateTimeUTC     := obj.Field('end_time').Asdatetimeutc;
     // add to networks
     for i:=0 to obj.Field('networkgroups').ValueCount-1 do begin
      nwg_uid:=obj.Field('networkgroups').AsObjectLinkItem[i];
      if conn.Fetch(nwg_uid,nwg)<>edb_OK then begin
       LogError('Could not find Network Group'+GFRE_BT.GUID_2_HexString(nwg_uid)+' for Ad '+obj.UID_String);
       continue;
      end;
      writeln(obj.DumpToString());
      writeln(nwg.DumpToString());
      for j:=0 to nwg.Field('networks').ValueCount-1 do begin
       nw_uid:=nwg.Field('networks').AsObjectLinkItem[j];
       if conn.Fetch(nw_uid,nw)<>edb_OK then begin
        LogError('Could not find Network '+GFRE_BT.GUID_2_HexString(nw_uid)+' for Networkgroup '+nwg.UID_String);
        continue;
       end;
       nn:=GetNetworkID(nw);
       intnw:=result_obj.Field(nn).AsObject;
       // AddGuids
       fn:='IP'+inttostr(insertpoint);
       ad_uid:=obj.UID;
       found:=false;
       for k:=0 to intnw.Field(fn).ValueCount-1 do begin
        if FREDB_Guids_Same(ad_uid,intnw.Field(fn).AsGUIDItem[k]) then begin
         found:=true;
         break;
        end;
       end;
       if found=false then begin
        intnw.Field(fn).AddGuid(ad_uid);
        // AddExceptions
        for c:=0 to obj.Field('urlexceptions').ValueCount-1 do begin
         found:=false;
         for k:=0 to intnw.Field('urlexceptions').ValueCount-1 do begin
          if obj.Field('urlexceptions').AsStringItem[c]=intnw.Field('urlexceptions').AsStringItem[k] then begin
            found:=true;
            break;
          end;
         end;
         if found=false then begin
          intnw.Field('urlexceptions').AddString(obj.Field('urlexceptions').AsStringItem[c]);
         end;
        end;
       end;
      end;
     end;
     ads.Field(obj.UID_String).AddObject(ao);
    end;
   end;
  end;
 end;

begin
 cmsob:=nil;

 coll:=Conn.Collection('service');
 hlt:=false;
 coll.ForAllBreak(@_findcms,hlt);

 if not Assigned(cmsob) then begin
  LogError('Could not find CMS for ServiceGroup '+GFRE_BT.GUID_2_HexString(serviceobj.uid));
  exit;
 end;

 //writeln(ob.DumpToString());


 baseurl:=cmsob.Field('baseurl').AsString;
 _getbasedata;

 coll:=Conn.Collection('sitecaptiveextension');
 coll.ForAll(@_findsitecap);

 ads:=GFRE_DBI.NewObject;
 result_obj.Field('ads').AsObject:=ads;
 coll:=Conn.Collection('cmspage');
 coll.ForAll(@_getcmspage);

 writeln(result_obj.DumpToString());
// writeln(ads.DumpToString());

end;


end.

