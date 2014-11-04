unit fos_captiveportalhal;

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
{$interfaces corba}

interface

uses
  Classes, SysUtils,dateutils,dos, FOS_CAPTIVEPORTAL_INTERFACE,FRE_HAL_UTILS,FRE_DB_INTERFACE,FOS_TOOL_INTERFACES,process,
  fre_system;

  const tmpext  ='.allow';
  const tmptask ='.task';
  const cpfaccesstable = 'access';

  type

  TModifyPFTable= (pfadd,pfdelete,pfkill);
  TPFTask       = (taskenable,taskdisable);


  TRedirectionRecord = record
   state        : integer;
   url          : string;
   statetext    : string;
   logouturl    : string;
   laststateurl : string;
   laststate    : integer;
   laststatetext: string;
   timeout_s    : integer;
   count_ad     : boolean;
  end;

  { TFOSHALCaptiveportalControl }

  TFOSHALCaptiveportalControl    = class (TObject,IFOS_CAPTIVEPORTAL_CONTROL)
  public
   function EnableInternetAccessForIP         (const ip:string)                                            :TFOSCaptiveportalResult;
   function EnableInternetAccessForIPbyUser   (const ip:string;const username:string;const password:string):TFOSCaptiveportalResult;
   function EnableInternetAccessForIPbyOTP    (const ip:string;const otp:string)                           :TFOSCaptiveportalResult;
   function EnableInternetAccessForIPRedirect (const ip:string;const state:string;out url:string)       :TFOSCaptiveportalResult;
   function DisableInternetAccessForIP        (const ip:string)                                            :TFOSCaptiveportalResult;
   function DisableInternetAccessForUser      (const username :string)                                     :TFOSCaptiveportalResult;
   function DisableInternetAccessForIPRedirect(const ip:string;out url:string)                             :TFOSCaptiveportalResult;
   function GetLoginStateForIP                (const ip:string;const stateurl:string;out state:integer;out statetext:string)     :TFOSCaptiveportalResult;

  end;

  { TFOSHALCaptiveportalRedirector }

  TFOSHALCaptiveportalRedirector = class (TObject,IFOS_CAPTIVEPORTAL_REDIREKTOR)
  private
   FInitialized  : boolean;
   fredir_object : IFRE_DB_Object;
   procedure Init;
  public
   function IsInternetAccessAllowedforIP    (const ip:string;var access:RFOSCaptiveAccess)            :TFOSCaptiveportalResult;
  end;

  { TFOSHALCaptivPortalPFUpdater }

  TFOSHALCaptivPortalPFUpdater = class
  public
   procedure Process;
   procedure CheckTasks;
   procedure UpdateAccess;
  end;


implementation


function cfre_tmpdir : string;
begin
  result := cFRE_HAL_CFG_DIR +'/redirector';
end;

function cfre_addir : string;
begin
  result := cFRE_HAL_CFG_DIR +'/ad';
end;


{ TFOSHALCaptiveportalRedirector }

procedure AddTask(const ip:string;const task:TPFTask; const table:string);
var o :IFRE_DB_OBject;
    fn:string;
begin
  fn := cfre_tmpdir+'/'+ip+tmptask;
  o := GFRE_DBI.NewObject;
  case task of
    taskenable  : o.Field('task').AsString:='ENABLE';
    taskdisable : o.Field('task').AsString:='DISABLE';
  end;
  o.Field('table').AsString:=table;
  o.SaveToFile(fn);
  o.Finalize;
end;


procedure ModifyPFTable(const ip:string;const mode:TModifyPFTable;const table:string);
var aprocess : TProcess;
    cmd      : string;
    msg      : string;
begin
  AProcess := TProcess.Create(nil);
  try
   case mode of
    pfadd   : begin
     cmd:='/sbin/pfctl -t '+table+' -T add '+ip;
     msg:='ENABLED: '+ip;
    end;
    pfdelete: begin
     cmd:='/sbin/pfctl -t '+table+' -T del '+ip;
     msg:='DISABLED: '+ip;
    end;
    pfkill  : begin
     cmd:='/sbin/pfctl -k '+ip;
     msg:='STATES REMOVED: '+ip;
    end;
   end;
   AProcess.CommandLine := cmd;
   AProcess.Options := [poWaitOnExit];
   AProcess.Execute;
   GFRE_LOG.Log (msg,'',fll_Info,ccaptivelogtarget,false);
   if Aprocess.ExitStatus<>0 then begin
    GFRE_LOG.Log ('pfctl '+cmd+' exited with exitstatus '+inttostr(Aprocess.ExitStatus),'',fll_Error,ccaptivelogtarget,false);
   end;
  finally
   AProcess.Free;
  end;
end;


function LoadAccess(const ip:string;var ao:IFRE_DB_Object):boolean;
var fn:string;
begin
 fn:=cfre_tmpdir+'/'+ip+tmpext;
 if FileExists(fn) then begin
  ao:=GFRE_DBI.CreateFromFile(fn);
  result:=true;
 end else begin
  ao:=nil;
  result:=false;
 end;
end;

procedure SaveAccess(const ip:string;const ao:IFRE_DB_Object);
var fn:string;
begin
 try
  fn:=cfre_tmpdir+'/'+ip+tmpext;
  ao.SaveToFile(fn);
 except on E:Exception do begin
  writeln('Error on saving access '+fn+' :'+E.Message);
 end; end;
end;

function DeleteAccess(const ip:string):boolean;
var fn:string;
begin
 fn:=cfre_tmpdir+'/'+ip+tmpext;
 if FileExists(fn) then begin
  DeleteFile(fn);
  result:=true;
 end else begin
  result:=false;
 end;
end;

function GetAdCount(const ad_id:string):longword;
var fn:string;
    ao:IFRE_DB_Object;
begin
  fn:=cfre_addir+'/'+ad_id+'.count';
  if FileExists(fn) then begin
   ao:=GFRE_DBI.CreateFromFile(fn);
   result:=ao.Field('count').AsUInt32;
  end else begin
   result:=0;
  end;
end;

procedure SetAdCount(const ad_id:string;const newcount:longword;const ip:string);
var fn:string;
    ao:IFRE_DB_Object;
    dt:TFRE_DB_DateTime64;
begin
  fn:=cfre_addir+'/'+ad_id;
  ForceDirectories(fn);
  ao:=GFRE_DBI.NewObject;
  ao.Field('count').AsUInt32:=newcount;
  ao.SaveToFile(fn+'.count');
  dt:=GFRE_DT.Now_UTC;
  ao:=GFRE_DBI.NewObject;
  ao.Field('ip').Asstring:=ip;
  ao.Field('ts').AsDateTimeUTC:=dt;
  ao.SaveToFile(fn+'/'+inttostr(dt));
end;


function GetRedirection(const redir_object:IFRE_DB_Object;const ip:string;const requesturl:string;const nextstate:boolean;var redir_rec:TRedirectionRecord):TFOSCaptiveportalResult;




 function _get(const ip_extended:string):TFOSCaptiveportalResult;
 var i  :integer;
     nw :IFRE_DB_Object;
     found:boolean;
     search_next:boolean;

   function _doAd(const insertpoint:string):boolean;
   var ad_guid   : TFRE_DB_GUID;
       ad_select : integer;
       ads       : IFRE_DB_Object;
       ad        : IFRE_DB_Object;
       ad_try    : integer;
       nowd      : TFRE_DB_DateTime64;
       year,month,day,hour,minute,second,millisecond:longint;
       lhour     : integer;
       lminute   : integer;
       ad_max     : longword;
       ad_show_hal: longword;
       ad_show_cur: longword;

       procedure _convertdaily(const s:string);
       begin
        try
         lhour:=strtoint(Copy(s,1,Pos(':',s)-1));
         lminute:=strtoint(Copy(s,Pos(':',s)+1,maxint));
        except
         lhour:=0;lminute:=0;
        end;
       end;

   begin
    ads       := redir_object.Field('ads').AsObject;
    ad_try    := nw.Field(insertpoint).valuecount;
    result    := false;
    nowd      := GFRE_DT.Now_UTC;
    ad_select := random (nw.Field(insertpoint).valuecount);
    while ((result=false) and (ad_try>0)) do begin
//     writeln('ad_select:',ad_select);
     ad_guid   := nw.Field(insertpoint).AsGUIDItem[ad_select];
     ad        := ads.Field(ad_guid.AsHexString).asobject;
     // get ad count
     ad_max      := ad.Field('MAX_INSERTS').AsUInt32;
     ad_show_hal := ad.Field('SHOWN_INSERTS').AsUInt32;
     ad_show_cur:=GetAdCount (ad_guid.AsHexString);
//     writeln('ad:',GFRE_BT.GUID_2_HexString(ad_guid),' Max:',ad_max,' Show hal:',ad_show_hal,' Show cur:',ad_show_cur);
     if (ad_show_hal+ad_show_cur)<ad_max then begin
      if ((ad.Field('start_time').AsDateTimeUTC<nowd) and (ad.Field('end_time').AsDateTimeUTC>nowd)) then begin
       GFRE_DT.DecodeTime(GFRE_DBI.UTCToLocalTimeDB64(nowd),year,month,day,hour,minute,second,millisecond);
       _convertdaily(ad.Field('START_DAILY').AsString);
 //      writeln('start:',lhour,':',lminute,' now:',hour,':',minute);
       if (lhour<hour) or ((lhour=hour) and (lminute<=minute)) then begin
         _convertdaily(ad.Field('END_DAILY').AsString);
 //       if ad_select=1 then begin
 //         _convertdaily('11:30');
 //        end;
 //        writeln('end:',lhour,':',lminute,' now:',hour,':',minute);
         if (lhour>hour) or ((lhour=hour) and (lminute>=minute)) then begin
          result:=true;
          redir_rec.url:=ad.Field('url').AsString;
          inc(ad_show_cur);
          if nextstate=true then begin      // count only on nextstate requests for forwards
           SetAdCount(ad_guid.AsHexString,ad_show_cur,ip);
          end;
         end;
       end;
      end;
     end;
     dec(ad_try);
     inc(ad_select);
     if (ad_select=nw.Field(insertpoint).valuecount) then ad_select:=0;
    end;
   end;

 begin
  result:=cprNOTDEFINED;
  // global exceptions
  if nextstate=false then begin
   for i:=0 to redir_object.Field('urlexceptions').ValueCount-1 do begin
    if Pos(redir_object.Field('urlexceptions').AsStringItem[i],requesturl)=1 then begin
     result:=cprOK;
     exit;
    end;
   end;
  end;
  if redir_object.FieldExists(ip_extended) then begin
   nw:=redir_object.Field(ip_extended).AsObject;
   redir_rec.timeout_s:=nw.Field('sessiontimeout').AsUInt32;
   if nw.FieldExists('redirection_start') then begin
    redir_rec.logouturl:=nw.Field('redirection_start').AsString;
   end else if nw.FieldExists('redirection_customer') then begin
    redir_rec.logouturl:=nw.Field('redirection_customer').AsString;
   end else if nw.FieldExists('redirection_agb') then begin
    redir_rec.logouturl:=nw.Field('redirection_agb').AsString;
   end;
   result:=cprDENIED;
   found:=false;
//   writeln(nw.DumpToString());
   if nextstate=false then begin
    for i:=0 to nw.Field('urlexceptions').ValueCount-1 do begin
     if Pos(nw.Field('urlexceptions').AsStringItem[i],requesturl)=1 then begin
      result:=cprOK;
      exit;
     end;
    end;
    search_next:=false;
   end else begin
    search_next:=true;
   end;
   while not found do begin
    case redir_rec.state of
     0 : begin
          if nw.FieldExists('redirection_start') then begin
            redir_rec.url:=nw.Field('redirection_start').AsString;
            found:=true;
          end;
         end;
     1 : begin
          if nw.FieldExists('IP1') then begin
           found:=_doAd('IP1');
          end;
         end;
     2 : begin
          if nw.FieldExists('redirection_customer') then begin
            redir_rec.url:=nw.Field('redirection_customer').AsString;
            found:=true;
          end;
         end;
     3 : begin
          if nw.FieldExists('IP2') then begin
           found:=_doAd('IP2');
          end;
         end;
     4 : begin
          if nw.FieldExists('redirection_agb') then begin
           redir_rec.url:=nw.Field('redirection_agb').AsString;
           found:=true;
          end;
         end;
     5 : begin
          if nw.FieldExists('IP3') then begin
           found:=_doAd('IP3');
          end;
          if found=false then begin
           found:=true;
           if nw.FieldExists('redirection_end') then begin
            redir_rec.url:=nw.Field('redirection_end').AsString;
           end else begin
            redir_rec.url:=nw.Field('redirection_customer').AsString;
           end;
//           writeln('TEST:',redir_rec.url);
          end;
          result:=cprOK;
         end;
    end;
    if found then begin
     redir_rec.statetext:=redir_object.Field('statetxt').AsStringItem[redir_rec.state];
     if search_next then begin
      redir_rec.laststateurl:=redir_rec.url;
      redir_rec.laststate:=redir_rec.state;
      redir_rec.laststatetext:=redir_object.Field('statetxt').AsStringItem[redir_rec.state];
      inc(redir_rec.state);     // get the next state after the current
      search_next:=false;
      found:=false;
     end;
    end else begin
     inc(redir_rec.state);
    end;
   end;
  end;
 end;

begin
 // check ip ranges
// if debugmode then writeln('DEBUG:GET REDIRECTION ',ip);
 GFRE_LOG.Log ('GET REDIRECTION :'+ip,'',fll_Debug,ccaptivelogtarget);

 result:=_get(GetNetworkIDfromString(ip+'/24'));
 if result=cprNOTDEFINED then begin
  result:=_get(GetNetworkIDfromString(ip+'/16'));
  if result=cprNOTDEFINED then begin
   result:=_get(GetNetworkIDfromString(ip+'/8'));
   if result=cprNOTDEFINED then begin
    GFRE_LOG.Log ('GET REDIRECTION DEFAULT 0.0.0.0','',fll_Debug,ccaptivelogtarget);
    result:=_get(GetNetworkIDfromString('0.0.0.0/24'));
   end;
  end;
 end;
end;

{ TFOSHALCaptivPortalPFUpdater }

procedure TFOSHALCaptivPortalPFUpdater.Process;
var i:integer;
begin
 i:=0;
 repeat
  inc(i);
  CheckTasks;
  if i=60 then begin
    UpdateAccess;
    i:=0;
  end;
  sleep(1000);
 until false;
end;

procedure TFOSHALCaptivPortalPFUpdater.CheckTasks;
var DSR  : TSearchRec;
     st   : integer;
     ao   : IFRE_DB_Object;
     fn   : string;
     ip   : string;
     aprocess : TProcess;
     task : string;
     table: string;
begin
   st := Sysutils.FindFirst(cfre_tmpdir +'/*'+tmptask, faAnyFile, DSR);
   try
    while St = 0 do begin
     if (DSR.Name <> '.') and (DSR.Name <> '..') then begin
      fn:=cfre_tmpdir+'/'+DSR.name;
      try
        ao:=GFRE_DBI.CreateFromFile(fn);
        try
          ip:=Copy(DSR.name,1,Pos(tmptask,dsr.name)-1);
          task :=ao.Field('task').AsString;
          table:=ao.Field('table').AsString;
          if task='ENABLE' then begin
            ModifyPFTable(ip,pfadd,table);
          end;
          if task='DISABLE' then begin
            ModifyPFTable(ip,pfdelete,table);
            ModifyPFTable(ip,pfkill,table);
          end;
        finally
          ao.Finalize;
        end;
      except on E:Exception do begin
       GFRE_LOG.Log ('Exception on reading '+fn+' :'+E.Message,'',fll_Error,ccaptivelogtarget,false);
      end; end;
      try
        DeleteFile(fn);
      except
      end;
     end;
     St := sysutils.FindNext(DSR);
    end;
   finally
    sysutils.FindClose(DSR);
   end;
end;

procedure TFOSHALCaptivPortalPFUpdater.UpdateAccess;
var DSR  : TSearchRec;
    st   : integer;
    ao   : IFRE_DB_Object;
    nowd : TFRE_DB_DateTime64;
    fn   : string;
    state: integer;
    ip   : string;
    validl,accessl,deletel: TStringList;
    aprocess : TProcess;
    i    : integer;


begin
 validl := TstringList.Create;
 try
//  writeln('nowfind');
  st := Sysutils.FindFirst(cfre_tmpdir +'/*.allow', faAnyFile, DSR);
  try
   while St = 0 do begin
    if (DSR.Name <> '.') and (DSR.Name <> '..') then begin
     fn:=cfre_tmpdir+'/'+DSR.name;
//     writeln(fn);
//     try
      ao:=GFRE_DBI.CreateFromFile(fn);
//      writeln (ao.Dumptostring);
      ip:=Copy(DSR.name,1,Pos(tmpext,dsr.name)-1);
//      writeln('ip check',ip);
      state:=ao.Field('state').asint16;
//      writeln('ip ',ip,' state ',state);
      if state=5 then begin
       nowd:=GFRE_DT.Now_UTC;
       if nowd>ao.Field('valid').AsDateTimeUTC then begin
        writeln('ip '+ip+' timed out');
        GFOS_CAPTIVEPORTAL_CONTROL.DisableInternetAccessForIP(ip);
       end else begin
        validl.add(ip);
       end;
      end;
      ao.Finalize;
//     except
//     end;
    end;
    St := sysutils.FindNext(DSR);
   end;
  finally
   sysutils.FindClose(DSR);
  end;

  accessl:=TStringList.Create;
  deletel:=TStringList.Create;
  try
   AProcess := TProcess.Create(nil);
   try
    AProcess.CommandLine := '/sbin/pfctl -t '+cpfaccesstable+' -T show';
    AProcess.Options := [poUsePipes,poWaitOnExit];
    AProcess.Execute;
    accessl.LoadFromStream(aprocess.Output);
   finally
    AProcess.Free;
   end;
   for i:=0 to accessl.Count-1 do begin
    accessl[i]:=trim(accessl[i]);
    ip:=accessl[i];
    if validl.IndexOf(ip)<0 then begin
     deletel.add(ip);
    end;
   end;
   for i:=0 to deletel.Count-1 do begin
    ip:=deletel[i];
    writeln('del '+ip);
    ModifyPFTable(ip,pfdelete,cpfaccesstable);
    ModifyPFTable(ip,pfkill,cpfaccesstable);
   end;
  finally
   deletel.Free;
   accessl.Free;
  end;
 finally
  validl.free;
 end;

end;

procedure TFOSHALCaptiveportalRedirector.Init;
begin
 if assigned(fredir_object) then begin
  fredir_object.Finalize;
 end;
 fredir_object:=LoadRedirect;
 ForceDirectories(cfre_tmpdir);
 ForceDirectories(cfre_addir);
 FInitialized:=true;
end;

function TFOSHALCaptiveportalRedirector.IsInternetAccessAllowedforIP(const ip: string; var access: RFOSCaptiveAccess): TFOSCaptiveportalResult;
var ipr            :string;
    redir_rec      :TRedirectionRecord;
    ao             :IFRE_DB_Object;
    nowd           :TFRE_DB_DateTime64;
begin
 if not FInitialized then Init;
 Result:=cprNOTDEFINED;
 if ip='' then exit;
 access.redirecturl:='';
 if LoadAccess(ip,ao) then begin
  redir_rec.state:=ao.Field('state').asint16;
  if redir_rec.state=5 then begin
//  No checking for TimeOuts, only pfupdater checks timeouts
//   nowd:=GFRE_DT.Now_UTC;
//   if nowd>ao.Field('valid').AsDateTimeUTC then begin
//    GFOS_CAPTIVEPORTAL_CONTROL.DisableInternetAccessForIP(ip);
//    redir_rec.state:=0;
//   end else begin
    result:=cprOK;
    exit;
//   end;
  end;
  ao.Finalize;
 end else begin
  redir_rec.state:=0;
 end;
 redir_rec.count_ad:=true;
 result:=GetRedirection(fredir_object,ip,lowercase(access.requesturl),false,redir_rec);
 access.redirecturl:=redir_rec.url;
end;


{ TFOSHALCaptiveportalControl }

function TFOSHALCaptiveportalControl.EnableInternetAccessForIP(const ip: string): TFOSCaptiveportalResult;
var url:string;
begin
  result:=EnableInternetAccessForIPRedirect(ip,'',url);
end;

function TFOSHALCaptiveportalControl.EnableInternetAccessForIPbyUser(const ip: string; const username: string; const password: string): TFOSCaptiveportalResult;
begin
  result:=cprINTERNALERROR;  // not implemented
end;

function TFOSHALCaptiveportalControl.EnableInternetAccessForIPbyOTP(const ip: string; const otp: string): TFOSCaptiveportalResult;
begin
  result:=cprINTERNALERROR;  // not implemented;
end;

function TFOSHALCaptiveportalControl.EnableInternetAccessForIPRedirect(const ip: string; const state:string;out url: string): TFOSCaptiveportalResult;
var access_o     :IFRE_DB_Object;
    redir_object :IFRE_DB_Object;
    redir_rec    :TRedirectionRecord;
    dt           :TFRE_DB_DateTime64;
    lstate       :integer;

    procedure _savestateurl(const i:integer;lurl:string);
    begin
     lurl:=Copy(lurl,Pos('/',lurl)+2,maxint);
     lurl:=Copy(lurl,Pos('/',lurl),maxint);
     access_o.Field('stateurl'+inttostr(i)).AsString:=lurl;
    end;
begin
 access_o:=nil;
 result:=cprNOTDEFINED;

 if LoadAccess(ip,access_o) then begin
//  writeln(access_o.DumpToString());
//  writeln('state:',state);
  redir_rec.state:=access_o.Field('state').asint16;
  try
   lstate:=strtoint(state);
  except
   lstate:=0;
  end;
//  writeln(lstate);
  if lstate=redir_rec.state then begin
   // expected state
  end else begin
   if redir_rec.state=5 then begin
    //was logged in
    AddTask(ip,taskenable,cpfaccesstable);
   end;
   if lstate<redir_rec.state then begin
    redir_rec.state:=lstate;
   end else if lstate>redir_rec.state then begin
    redir_rec.state:=0;
   end;
  end;
  GFRE_LOG.Log ('NEW REDIRSTATE :'+inttostr(redir_rec.state),'',fll_Debug,ccaptivelogtarget);
  if redir_rec.state=5 then begin
   url:=access_o.Field('logout_url').AsString;
   access_o.Finalize;
   result:=cprOK;
   exit;
  end;
 end else begin
  access_o:=GFRE_DBI.NewObject;
  redir_rec.state:=0;
 end;
 redir_object:=LoadRedirect;
// redir_rec.count_ad:=true;
 result:=GetRedirection(redir_object,ip,'invalid',true,redir_rec);
 if result=cprNOTDEFINED then begin
  url:='';
  access_o.Finalize;
  exit;
 end else begin
//  writeln('new entry');
  dt:=GFRE_DT.Now_UTC;
  dt:=dt+(1000*redir_rec.timeout_s);
  access_o.Field('valid').AsDateTimeUTC:=dt;
  access_o.Field('state').AsUInt16:=redir_rec.state;
  access_o.Field('logout_url').AsString:=redir_rec.logouturl;
  access_o.Field('statetext'+inttostr(redir_rec.state)).AsString:=redir_rec.statetext;
  access_o.Field('statetext'+inttostr(redir_rec.laststate)).AsString:=redir_rec.laststatetext;
  _savestateurl(redir_rec.laststate,redir_rec.laststateurl);
  _savestateurl(redir_rec.state,redir_rec.url);
//  writeln(access_o.DumpToString());   //DEBUG
  SaveAccess(ip,access_o);
  access_o.Finalize;
  url:=redir_rec.url;
 end;
 if result=cprOK then begin
  AddTask(ip,taskenable,cpfaccesstable);
 end;
end;

function TFOSHALCaptiveportalControl.DisableInternetAccessForIP(const ip: string): TFOSCaptiveportalResult;
var url:string;
begin
 result:=DisableInternetAccessForIPRedirect(ip,url);
end;

function TFOSHALCaptiveportalControl.DisableInternetAccessForUser(const username: string): TFOSCaptiveportalResult;
begin
 result:=cprINTERNALERROR;  // not implemented;
end;

function TFOSHALCaptiveportalControl.DisableInternetAccessForIPRedirect(const ip: string; out url: string): TFOSCaptiveportalResult;
var  ao           :IFRE_DB_Object;
     redir_object :IFRE_DB_Object;
     redir_rec    :TRedirectionRecord;
begin
 result:=cprNOTDEFINED;
 if LoadAccess(ip,ao) then begin
  url:=ao.Field('logout_url').asstring;
  DeleteAccess(ip);
  AddTask(ip,taskdisable,cpfaccesstable);
  result:=cprDENIED;
 end else begin
  redir_object:=LoadRedirect;
  try
   redir_rec.state:=0;
   redir_rec.count_ad:=false;
   result:=GetRedirection(redir_object,ip,'invalid',false,redir_rec);
   url:=redir_rec.url;
  finally
   redir_object.Finalize;
  end;
 end;
end;


function TFOSHALCaptiveportalControl.GetLoginStateForIP(const ip: string; const stateurl:string;out state: integer; out statetext: string): TFOSCaptiveportalResult;
var access_o     :IFRE_DB_Object;
    redir_object :IFRE_DB_Object;
    redir_rec    :TRedirectionRecord;
    lstateurl    :string;
    li           :integer;
begin
 result:=cprNOTDEFINED;
 if LoadAccess(ip,access_o) then begin
  state    :=access_o.Field('state').AsUInt16;
  lstateurl:=access_o.Field('stateurl'+inttostr(state)).AsString;
  if lowercase(lstateurl)=lowercase(stateurl) then begin
   statetext:=access_o.Field('statetext'+inttostr(state)).AsString;
   result:=cprOK;
  end else begin
   for li:=4 downto 0 do begin
    lstateurl:=access_o.Field('stateurl'+inttostr(li)).AsString;
    if lowercase(lstateurl)=lowercase(stateurl) then begin
     access_o.Field('state').AsUInt16:=li;
     state:=li;
     statetext:=access_o.Field('statetext'+inttostr(state)).AsString;
     SaveAccess(ip,access_o);
     result:=cprOK;
     exit;
    end;
   end;
   if state=5 then begin
    statetext:=access_o.Field('statetext'+inttostr(state)).AsString;
    result:=cprOK;
   end else begin
    state:=0;
    access_o.Field('state').AsUInt16:=state;
    statetext:=access_o.Field('statetext'+inttostr(state)).AsString;
    DeleteAccess(ip);
//    SaveAccess(ip,access_o);
    result:=cprDENIED;
   end;
  end;
 end else begin
   redir_rec.state:=0;
   redir_rec.count_ad:=true;
   redir_object:=LoadRedirect;
   try
    result:=GetRedirection(redir_object,ip,'invalid',false,redir_rec);
    state     :=redir_rec.state;
    statetext :=redir_rec.statetext;
   finally
    redir_object.Finalize;
   end;
 end;
end;

initialization
 GFOS_CAPTIVEPORTAL_CONTROL    := TFOSHALCaptiveportalControl.Create;
 GFOS_CAPTIVEPORTAL_REDIRECTOR := TFOSHALCaptiveportalRedirector.Create;
end.

