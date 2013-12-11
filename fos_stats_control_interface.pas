unit fos_stats_control_interface;

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
{$interfaces corba}
//{$codepage UTF8}

interface


uses
  Classes, SysUtils,FRE_DB_INTERFACE,FOS_TOOL_INTERFACES,FRE_PROCESS,FRE_SYSTEM,fre_base_parser;

const    cDEBUG_ZPOOL_NAME = 'zones';

var     c_GET_ZFS_DATA_ONCE : string = 'zfs list -Hp -o name,referenced,available,used '+cDEBUG_ZPOOL_NAME;


const
        cIOSTATFILEONCE         = 'sh -c /zones/firmos/onceiostat.sh';

        c_GET_CPU_DATA     = 'mpstat -q 1';
        c_GET_CPU_DATA_LOC = 'mpstat -q 1';

        c_GET_RAM_DATA     = 'vmstat 1';
        c_GET_RAM_DATA_LOC = 'vmstat 1';

        c_GET_NETWORK_DATA     = 'dlstat show-phys -o link,type,pkts,bytes -p -i 1';
        c_GET_NETWORK_DATA_LOC = 'dlstat show-phys -o link,type,pkts,bytes -p -i 1';

        c_GET_CACHE_DATA     = 'kstat -p zfs:0:arcstats:size zfs:0:arcstats:misses zfs:0:arcstats:hits zfs:0:arcstats:c zfs:0:arcstats:c_min zfs:0:arcstats:c_max 1';
        c_GET_CACHE_DATA_LOC = 'kstat -p zfs:0:arcstats:size zfs:0:arcstats:misses zfs:0:arcstats:hits zfs:0:arcstats:c zfs:0:arcstats:c_min zfs:0:arcstats:c_max 1';


type

  { IFOS_STATS_CONTROL }

  IFOS_STATS_CONTROL=interface
    procedure StartCPUParser                   (const enable:boolean);
    procedure StartRAMParser                   (const enable:boolean);
    procedure StartNetworkParser               (const enable:boolean);
    procedure StartCacheParser                 (const enable:boolean);
    procedure StartZFSParser                   (const enable:boolean);
    function  Get_CPU_Data        : IFRE_DB_Object;
    function  Get_CacheData       : IFRE_DB_Object;
    function  Get_Ram_Data        : IFRE_DB_Object;
    function  Get_ZFS_Data_Once   : IFRE_DB_Object;
    function  Get_Network_Data    : IFRE_DB_Object;
    procedure Finalize;
  end;

  function Get_Stats_Control(const user,host:string): IFOS_STATS_CONTROL;


implementation

type

  { TFOS_CPU_PARSER }

  TFOS_CPU_PARSER=class(TFOS_PARSER_PROC)
  protected
     procedure   MySetup ; override;
     procedure   MyOutStreamCallBack (const stream:TStream); override;
  public
  end;

  { TFOS_RAM_PARSER }

  TFOS_RAM_PARSER=class(TFOS_PARSER_PROC)
  protected
     procedure   MySetup ; override;
     procedure   MyOutStreamCallBack (const stream:TStream); override;
  public
  end;

  { TFOS_NETWORK_PARSER }

  TFOS_NETWORK_PARSER=class(TFOS_PARSER_PROC)
  protected
     procedure   MySetup ; override;
     procedure   MyOutStreamCallBack (const stream:TStream); override;
  public
  end;

  { TFOS_CACHE_PARSER }

  TFOS_CACHE_PARSER=class(TFOS_PARSER_PROC)
  protected
     procedure   MySetup ; override;
     procedure   MyOutStreamCallBack (const stream:TStream); override;
  public
  end;



  { TFOS_ZFS_PARSER }

  TFOS_ZFS_PARSER=class(TFOS_PARSER_PROC)
  private
  protected
     procedure   MySetup ; override;
     procedure   MyOutStreamCallBack (const stream:TStream); override;
  public
  end;



  { TFOS_STATS_CONTROL }

  TFOS_STATS_CONTROL=class(TObject,IFOS_STATS_CONTROL)
  private
     cremoteuser               : string;
     cremotehost               : string;
     cremotekeyfilename        : string;

     FCPUMon                   : TFOS_CPU_PARSER;
     FRAMMon                   : TFOS_RAM_PARSER;
     FNetworkMon               : TFOS_NETWORK_PARSER;
     FCacheMon                 : TFOS_CACHE_PARSER;
     FZFSMon                   : TFOS_ZFS_PARSER;

     procedure   Finalize;
  public
     constructor Create           (const user,host,dir : string);
     destructor  Destroy          ; override ;
     procedure   StartCPUParser                   (const enable:boolean);
     procedure   StartRAMParser                   (const enable:boolean);
     procedure   StartNetworkParser               (const enable:boolean);
     procedure   StartCacheParser                 (const enable:boolean);
     procedure   StartZFSParser                   (const enable:boolean);
     function    Get_CPU_Data        : IFRE_DB_Object;
     function    Get_CacheData       : IFRE_DB_Object;
     function    Get_Ram_Data        : IFRE_DB_Object;
     function    Get_ZFS_Data_Once   : IFRE_DB_Object;
     function    Get_Network_Data    : IFRE_DB_Object;
  end;


function Get_Stats_Control(const user, host: string): IFOS_STATS_CONTROL;
var res : TFOS_STATS_CONTROL;
    dir : string;
begin
  dir := SetDirSeparators(cFRE_SERVER_DEFAULT_DIR+'/ssl/user/id_rsa');
  res := TFOS_STATS_CONTROL.Create(user,host,dir);
  Result := res;
end;

procedure TFOS_ZFS_PARSER.MySetup;
begin
  FLine.Delimiter:=' ';
end;

procedure TFOS_ZFS_PARSER.MyOutStreamCallBack(const stream: TStream);
var
  st: TStringStream;
  lc: Integer;
  i: Integer;
  s: String;
  SA: TFOSStringArray;
  j: Integer;

  procedure _UpdateZFS;
  var
    poolid : string;
  begin
    poolid := Fline[0];
    FData.Field(poolid).AsObject.Field('referenced').AsInt64    := StrToInt64(Fline[1]);
    FData.Field(poolid).AsObject.Field('available').AsInt64    := StrToInt64(Fline[2]);
    FData.Field(poolid).AsObject.Field('used').AsInt64   := StrToInt64(Fline[3]);
  end;

begin
  stream.Position:=0;
  st := TStringStream.Create('');
  try
    st.CopyFrom(stream,stream.Size);
    stream.Size:=0;
    FLines.DelimitedText := st.DataString;
    if Flines.count>0 then begin
      lc := FLines.Count;
      for i := 0 to lc-1 do begin
        s := FLines[i];
        if s='' then
          continue;
        GFRE_BT.SeperateString(s,#9,SA);
        fline.Clear;
        for j := 0 to high(sa) do
          begin
            if sa[j]<>'' then
              FLine.Add(sa[j]);
          end;
        if FLine.count<>4 then
          raise EFRE_Exception.Create('zfs parser error, unexpected val count ' + IntToStr(fline.Count));
        FLock.Acquire;
        try
          try
            _UpdateZFS;
          except on E:Exception do begin
            writeln(ClassName,'>>>Mickey Parser Error---');
            s:= Fline.DelimitedText;
            writeln(s);
            writeln(Classname,'<<<Mickey Parser Error---');
          end;end;
        finally
          FLock.Release;
        end;
      end;
    end else begin
      writeln(ClassName,'*IGNORING JUNK : ',st.Size,': [',st.DataString,']');
    end;
  finally
    st.Free;
  end;
end;

procedure TFOS_CACHE_PARSER.MySetup;
begin
  FLine.Delimiter:=' ';
end;


procedure TFOS_CACHE_PARSER.MyOutStreamCallBack(const stream: TStream);
var
  st: TStringStream;
  lc: Integer;
  i: Integer;
  s: String;
  SA: TFOSStringArray;
  j: Integer;

  procedure _UpdateCache;
  var
    s             : string;
    TA: TFOSStringArray;
  begin
    //s := FLine.CommaText;
    //writeln(Fline.Count,' ',s);
    GFRE_BT.SeperateString(Fline[0],':',TA);
    case TA[3] of
      'hits': begin
                if FData.FieldExists('hits') then begin
                  FData.Field('relHits').AsInt16:=StrToInt64(Fline[1]) - FData.Field('hits').AsInt64;
                end else begin
                   FData.Field('relHits').AsInt16:=0;
                end;
              end;
      'misses': begin
                  if FData.FieldExists('misses') then begin
                     FData.Field('relMisses').AsInt16:=StrToInt64(Fline[1]) - FData.Field('misses').AsInt64;
                  end else begin
                     FData.Field('relMisses').AsInt16:=0;
                  end;
                end;
    end;
    FData.Field(TA[3]).AsInt64   := StrToInt64(Fline[1]);
  end;


begin
  stream.Position:=0;
  st := TStringStream.Create('');
  try
    st.CopyFrom(stream,stream.Size);
    stream.Size:=0;
    FLines.DelimitedText := st.DataString;
    if Flines.count>0 then begin
      lc := FLines.Count;
      for i:= 0 to lc-1 do
        begin
          s := FLines[i];
          if s='' then continue;
          GFRE_BT.SeperateString(s,#9,SA);
          fline.Clear;
          for j := 0 to high(sa) do
            begin
              if sa[j]<>'' then
                FLine.Add(sa[j]);
            end;
          if FLine.count<>2 then
            raise EFRE_Exception.Create('cache parser error, unexpected val count');
          FLock.Acquire;
          try
            try
              _UpdateCache;
            except on E:Exception do begin
              writeln(ClassName,'>>>Mickey Parser Error---');
              s:= Fline.DelimitedText;
              writeln(s);
              writeln(Classname,'<<<Mickey Parser Error---');
            end;end;
          finally
            FLock.Release;
          end;
      end;
    end else begin
      writeln(ClassName,'*IGNORING JUNK : ',st.Size,': [',st.DataString,']');
    end;
  finally
    st.Free;
  end;
end;

procedure TFOS_NETWORK_PARSER.MySetup;
begin
  FLine.Delimiter:=' ';
end;


procedure TFOS_NETWORK_PARSER.MyOutStreamCallBack(const stream: TStream);
var
  st: TStringStream;
  lc: Integer;
  i: Integer;
  s: String;
  SA: TFOSStringArray;
  j: Integer;

  //  link,type,pkts,bytes
  procedure _UpdateNetwork;
  var
    s             : string;
    netId,netType : String;
    pkts,bytes    : string;
  begin
    //s := FLine.CommaText;
    //writeln(Fline.Count,' ',s);
    netId   := Fline[0];
    netType := FLine[1];
    pkts    := Fline[2];
    bytes   := Fline[3];
    FData.Field(netId).AsObject.Field(netType).AsObject.Field('pkts').AsInt64    := StrToInt64(pkts);
    FData.Field(netId).AsObject.Field(netType).AsObject.Field('bytes').AsInt64   := StrToInt64(bytes);
  end;


begin
  stream.Position:=0;
  st := TStringStream.Create('');
  try
    st.CopyFrom(stream,stream.Size);
    stream.Size:=0;
    FLines.DelimitedText := st.DataString;
    if Flines.count>0 then begin
      lc := FLines.Count;
      for i:= 0 to lc-1 do
        begin
          s := FLines[i];
          if s='' then continue;
          GFRE_BT.SeperateString(s,':',SA);
          fline.Clear;
          for j := 0 to high(sa) do
            begin
              if sa[j]<>'' then
                FLine.Add(sa[j]);
            end;
          if FLine.count<>4 then
            raise EFRE_Exception.Create('network parser error, unexpected val count');
          FLock.Acquire;
          try
            try
              _UpdateNetwork;
            except on E:Exception do begin
              writeln(ClassName,'>>>Mickey Parser Error---');
              s:= Fline.DelimitedText;
              writeln(s);
              writeln(Classname,'<<<Mickey Parser Error---');
            end;end;
          finally
            FLock.Release;
          end;
      end;
    end else begin
      writeln(ClassName,'*IGNORING JUNK : ',st.Size,': [',st.DataString,']');
    end;
  finally
    st.Free;
  end;
end;

procedure TFOS_RAM_PARSER.MySetup;
begin
  FLine.Delimiter:=' ';
end;


procedure TFOS_RAM_PARSER.MyOutStreamCallBack(const stream: TStream);
var
  st: TStringStream;
  lc: Integer;
  i: Integer;
  s: String;
  SA: TFOSStringArray;
  j: Integer;

  //  r b w   swap  free  re  mf pi po fr de sr lf rm s0 s1   in   sy   cs us sy id
  procedure _UpdateRAM;
  var
    s     : string;
  begin
    //s := FLine.CommaText;
    //writeln(Fline.Count,' ',s);
    FData.Field('kthr_r').AsInt64   := StrToInt64(Fline[0]);
    FData.Field('kthr_b').AsInt64   := StrToInt64(Fline[1]);
    FData.Field('kthr_w').AsInt64   := StrToInt64(Fline[2]);
    FData.Field('memory_swap').AsInt64   := StrToInt64(Fline[3]);
    FData.Field('memory_free').AsInt64   := StrToInt64(Fline[4]);
    FData.Field('page_re').AsInt64   := StrToInt64(Fline[5]);
    FData.Field('page_mf').AsInt64   := StrToInt64(Fline[6]);
    FData.Field('page_pi').AsInt64   := StrToInt64(Fline[7]);
    FData.Field('page_po').AsInt64   := StrToInt64(Fline[8]);
    FData.Field('page_fr').AsInt64   := StrToInt64(Fline[9]);
    FData.Field('page_de').AsInt64   := StrToInt64(Fline[10]);
    FData.Field('page_sr').AsInt64   := StrToInt64(Fline[10]);
    //ignore disks (4)
    //FData.Field('').AsInt64   := StrToInt64(Fline[12]);
    //FData.Field('').AsInt64   := StrToInt64(Fline[13]);
    //FData.Field('').AsInt64   := StrToInt64(Fline[14]);
    //FData.Field('').AsInt64   := StrToInt64(Fline[15]);
    FData.Field('faults_in').AsInt64   := StrToInt64(Fline[16]);
    FData.Field('faults_sy').AsInt64   := StrToInt64(Fline[17]);
    FData.Field('faults_cs').AsInt64   := StrToInt64(Fline[18]);
    FData.Field('cpu_us').AsInt64   := StrToInt64(Fline[19]);
    FData.Field('cpu_sy').AsInt64   := StrToInt64(Fline[20]);
    FData.Field('cpu_id').AsInt64   := StrToInt64(Fline[21]);
  end;


begin
  stream.Position:=0;
  st := TStringStream.Create('');
  try
    st.CopyFrom(stream,stream.Size);
    stream.Size:=0;
    FLines.DelimitedText := st.DataString;
    if Flines.count>0 then begin
      lc := FLines.Count;
      for i:= 0 to lc-1 do
        begin
          if pos('kthr',Flines[i])>1 then continue;
          if pos('r b w',Flines[i])>1 then continue;
          s := FLines[i];
          if s='' then continue;
          GFRE_BT.SeperateString(s,' ',SA);
          fline.Clear;
          for j := 0 to high(sa) do
            begin
              if sa[j]<>'' then
                FLine.Add(sa[j]);
            end;
          if FLine.count<>22 then
            raise EFRE_Exception.Create('ram parser error, unexpected val count');
          FLock.Acquire;
          try
            try
              _UpdateRAM;
            except on E:Exception do begin
              writeln(ClassName,'>>>Mickey Parser Error---');
              s:= Fline.DelimitedText;
              writeln(s);
              writeln(Classname,'<<<Mickey Parser Error---');
            end;end;
          finally
            FLock.Release;
          end;
      end;
    end else begin
      writeln(ClassName,'*IGNORING JUNK : ',st.Size,': [',st.DataString,']');
    end;
  finally
    st.Free;
  end;
end;

{ TFOS_DISK_PARSER }


{ TFOS_CPU_PARSER }

procedure TFOS_CPU_PARSER.MySetup;
begin
  FLine.Delimiter:=' ';
end;


procedure TFOS_CPU_PARSER.MyOutStreamCallBack(const stream: TStream);
var st  : TStringStream;
    i,j : integer;
    s   : string;
    lc  : integer;

  //  CPU minf mjf xcal  intr ithr  csw icsw migr smtx  srw syscl  usr sys  wt idl
  procedure _UpdateCPU;
  var CPUID : string[30];
      s     : string;
  begin
    CPUID := 'CPU'+Fline[0];
    //s := FLine.CommaText;
    //writeln(Fline.Count,' ',s);
    FData.Field(CPUID).AsObject.Field('minf').AsInt64   := StrToInt64(Fline[1]);
    FData.Field(CPUID).AsObject.Field('mjf').AsInt64    := StrToInt64(Fline[2]);
    FData.Field(CPUID).AsObject.Field('xcal').AsInt64   := StrToInt64(Fline[3]);
    FData.Field(CPUID).AsObject.Field('intr').AsInt64   := StrToInt64(Fline[4]);
    FData.Field(CPUID).AsObject.Field('ithr').AsInt64   := StrToInt64(Fline[5]);
    FData.Field(CPUID).AsObject.Field('csw').AsInt64    := StrToInt64(Fline[6]);
    FData.Field(CPUID).AsObject.Field('icsw').AsInt64   := StrToInt64(Fline[7]);
    FData.Field(CPUID).AsObject.Field('migr').AsInt64   := StrToInt64(Fline[8]);
    FData.Field(CPUID).AsObject.Field('smtx').AsInt64   := StrToInt64(Fline[9]);
    FData.Field(CPUID).AsObject.Field('srw').AsInt64    := StrToInt64(Fline[10]);
    FData.Field(CPUID).AsObject.Field('syscl').AsInt64  := StrToInt64(Fline[11]);
    FData.Field(CPUID).AsObject.Field('usr').AsInt64    := StrToInt64(Fline[12]);
    FData.Field(CPUID).AsObject.Field('sys').AsInt64    := StrToInt64(Fline[13]);
    //FData.Field(CPUID).AsObject.Field('wt').AsInt64     := StrToInt64(Fline[14]); // always zero
    FData.Field(CPUID).AsObject.Field('idl').AsInt64    := StrToInt64(Fline[15]);
  end;

  var sa:TFOSStringArray;

begin
  stream.Position:=0;
  st := TStringStream.Create('');
  try
    st.CopyFrom(stream,stream.Size);
    stream.Size:=0;
    FLines.DelimitedText := st.DataString;
    if Flines.count>0 then begin
      lc := FLines.Count;
      for i:= 0 to lc-1 do
        begin
          if pos('CPU',Flines[i])=1 then
            continue;
          s := FLines[i];
          if s='' then
            continue;
          GFRE_BT.SeperateString(s,' ',SA);
          fline.Clear;
          for j := 0 to high(sa) do
            begin
              if sa[j]<>'' then
                FLine.Add(sa[j]);
            end;
          if FLine.count<>16 then
            raise EFRE_Exception.Create('cpu parser error, unexpected val count');
          FLock.Acquire;
          try
            try
              _UpdateCPU;
            except on E:Exception do begin
              writeln(ClassName,'>>>Mickey Parser Error---');
              s:= Fline.DelimitedText;
              writeln(s);
              writeln(Classname,'<<<Mickey Parser Error---');
            end;end;
          finally
            FLock.Release;
          end;
      end;
    end else begin
      writeln(ClassName,'*IGNORING JUNK : ',st.Size,': [',st.DataString,']');
    end;
  finally
    st.Free;
  end;
end;

{ TFOS_DISK_CONTROL }

procedure TFOS_STATS_CONTROL.Finalize;
begin

  Free;
end;

constructor TFOS_STATS_CONTROL.Create(const user, host, dir: string);
begin
  if user<>'' then
    begin
      FCPUMon     := TFOS_CPU_PARSER.Create(user,dir,host,c_GET_CPU_DATA);
      FRAMMon     := TFOS_RAM_PARSER.Create(user,dir,host,c_GET_RAM_DATA);
      FNetworkMon := TFOS_NETWORK_PARSER.Create(user,dir,host,c_GET_NETWORK_DATA);
      FCacheMon   := TFOS_CACHE_PARSER.Create(user,dir,host,c_GET_CACHE_DATA);
    end
  else
    begin
      FCPUMon     := TFOS_CPU_PARSER.Create(user,dir,host,c_GET_CPU_DATA_LOC);
      FRAMMon     := TFOS_RAM_PARSER.Create(user,dir,host,c_GET_RAM_DATA_LOC);
      FNetworkMon := TFOS_NETWORK_PARSER.Create(user,dir,host,c_GET_NETWORK_DATA_LOC);
      FCacheMon   := TFOS_CACHE_PARSER.Create(user,dir,host,c_GET_CACHE_DATA_LOC);
    end;
  cremoteuser        := user;
  cremotehost        := host;
  cremotekeyfilename := dir;
end;

destructor TFOS_STATS_CONTROL.Destroy;
begin
  FNetworkMon.Disable;
  FNetworkMon.Free;
  FCacheMon.Disable;
  FCacheMon.Free;
  FRAMMon.Disable;
  FRAMMon.FRee;
  FCPUMon.Disable;
  FCPUMon.Free;
  inherited Destroy;
end;



procedure TFOS_STATS_CONTROL.StartCPUParser(const enable: boolean);
begin
  if enable then
    FCPUMon.Enable
  else
    FCPUMon.Disable;
end;

procedure TFOS_STATS_CONTROL.StartRAMParser(const enable: boolean);
begin
  if enable then
    FRAMMon.Enable
  else
    FRAMMon.Disable;
end;

procedure TFOS_STATS_CONTROL.StartNetworkParser(const enable: boolean);
begin
  if enable then
    FNetworkMon.Enable
  else
    FNetworkMon.Disable;
end;

procedure TFOS_STATS_CONTROL.StartCacheParser(const enable: boolean);
begin
  if enable then
    FCacheMon.Enable
  else
    FCacheMon.Disable;
end;

procedure TFOS_STATS_CONTROL.StartZFSParser(const enable: boolean);
begin
  if enable then
    FCPUMon.Enable
  else
    FCPUMon.Disable;
end;

function TFOS_STATS_CONTROL.Get_CPU_Data: IFRE_DB_Object;
var
  CPUAGGR : IFRE_DB_Object;
  count   : Integer;

  procedure _addCpu(const field: IFRE_DB_Field);

    procedure _addValue(const field: IFRE_DB_Field);
    begin
      if field.FieldType=fdbft_GUID then exit;
      CPUAGGR.Field(field.FieldName).AsReal32:=CPUAGGR.Field(field.FieldName).AsReal32+field.AsInt64;
    end;

    procedure _setValue(const field: IFRE_DB_Field);
    begin
      if field.FieldType=fdbft_GUID then exit;
      CPUAGGR.Field(field.FieldName).AsReal32:=field.AsInt64;
    end;

    begin
    if field.FieldType=fdbft_GUID then exit;
    if count=0 then begin
      field.AsObject.ForAllFields(@_setValue);
    end else begin
       field.AsObject.ForAllFields(@_addValue);
    end;
    count:=count+1;
  end;

  procedure _divideValue(const field: IFRE_DB_Field);
  begin
    if field.FieldType=fdbft_GUID then exit;
    field.AsReal32:=field.AsReal32 / count;
  end;

begin
  result    := FCPUMon.Get_Data_Object;
  count     := 0;
  CPUAGGR:=GFRE_DBI.NewObject;
  result.ForAllFields(@_addCpu);
  CPUAGGR.ForAllFields(@_divideValue);

  result.Field('CPU_AGGR').AsObject := CPUAGGR;
end;

function TFOS_STATS_CONTROL.Get_CacheData: IFRE_DB_Object;
begin
  result    := FCacheMon.Get_Data_Object;
end;

function TFOS_STATS_CONTROL.Get_Ram_Data: IFRE_DB_Object;
begin
  result    := FRAMMon.Get_Data_Object;
end;

function TFOS_STATS_CONTROL.Get_ZFS_Data_Once: IFRE_DB_Object;
var
  dimon : TFOS_ZFS_PARSER;
begin
  dimon  := TFOS_ZFS_PARSER.Create(cremoteuser,cremotekeyfilename,cremotehost,c_GET_ZFS_DATA_ONCE);
  try
    dimon.Once;
    result := dimon.Get_Data_Object;
  finally
    dimon.Free;
  end;
end;

function TFOS_STATS_CONTROL.Get_Network_Data: IFRE_DB_Object;
var
  NETAGGR : IFRE_DB_Object;
  count   : Integer;

  procedure _addNet(const field: IFRE_DB_Field);

    procedure _setObjs(const field: IFRE_DB_Field);
    begin
      if field.FieldType=fdbft_GUID then exit;
      NETAGGR.Field(field.FieldName).AsObject:=field.AsObject.CloneToNewObject();
    end;

    procedure _addObjs(const field: IFRE_DB_Field);
    var
      objId: String;

      procedure _addValues(const field: IFRE_DB_Field);
      begin
        if field.FieldType=fdbft_GUID then exit;
        NETAGGR.Field(objId).AsObject.Field(field.FieldName).AsInt64:=NETAGGR.Field(objId).AsObject.Field(field.FieldName).AsInt64+field.AsInt64;
      end;

    begin
      if field.FieldType=fdbft_GUID then exit;
      objId:=field.FieldName;
      field.AsObject.ForAllFields(@_addValues);
    end;

    begin
    if field.FieldType=fdbft_GUID then exit;
    if count=0 then begin
      field.AsObject.ForAllFields(@_setObjs);
    end else begin
      field.AsObject.ForAllFields(@_addObjs);
    end;
    count:=count+1;
  end;

begin
  result    := FNetworkMon.Get_Data_Object;
  NETAGGR:=GFRE_DBI.NewObject;
  count:=0;
  result.ForAllFields(@_addNet);

  result.Field('NET_AGGR').AsObject := NETAGGR;
end;

end.

