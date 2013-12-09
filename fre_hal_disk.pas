unit fre_hal_disk;

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
{$interfaces corba}

interface

uses
  Classes, SysUtils,FRE_DB_INTERFACE, FRE_DB_COMMON, FRE_PROCESS, FOS_BASIS_TOOLS,
  FOS_TOOL_INTERFACES,FRE_ZFS,fre_scsi,fre_base_parser;


const

  cIOSTATFILEHACKMIST     = '/zones/firmos/myiostat.sh';
  cIOSTATFILEHACKMIST_LOC = 'sh -c /zones/firmos/myiostat.sh';

type


  TFRE_HAL_DISK = class;

  { TFOS_IOSTAT_PARSER }

  { TFRE_IOSTAT_PARSER }

  TFRE_IOSTAT_PARSER=class(TFOS_PARSER_PROC)
  private
    fhal_disk  : TFRE_HAL_DISK;
  protected
    procedure   MyOutStreamCallBack (const stream:TStream); override;
  public
    constructor Create (const remoteuser,remotekeyfile,remotehost,cmd : string;const hal_disk:TFRE_HAL_DISK);
  end;

  { TFRE_HAL_DISK }

  TFRE_HAL_DISK = class (TFRE_DB_Base)
  private
    FDiskIoStatMon        : TFRE_IOSTAT_PARSER;

    disk_lock                          : IFOS_LOCK;
    disk_information                   : IFRE_DB_Object;

  public
    constructor Create                 ; override;
    destructor Destroy                 ; override;

    procedure InitializeDiskandEnclosureInformation (const remoteuser:string='';const remotehost:string='';const remotekey:string='');
    procedure InitializePoolInformation             (const remoteuser:string='';const remotehost:string='';const remotekey:string='');
    procedure StartIostatMonitor                    (const remoteuser:string='';const remotehost:string='';const remotekey:string='');

    function  IsInformationAvailable                :boolean;

    function  GetInformation                        : IFRE_DB_Object;

    function  FetchDiskAndEnclosureInformation      (const remoteuser:string='';const remotehost:string='';const remotekey:string=''): IFRE_DB_OBJECT;
    function  FetchPoolConfiguration                (const zfs_pool_name:string; const remoteuser:string='';const remotehost:string='';const remotekey:string=''): IFRE_DB_OBJECT;

    procedure UpdateDiskIoStatInformation           (const devicename:string; const iostat_information: TFRE_DB_BLOCKDEVICE_IOSTAT);

    function  GetPools                  (const remoteuser:string='';const remotehost:string='';const remotekey:string=''): IFRE_DB_OBJECT;
    function  CreateDiskpool            (const input:IFRE_DB_Object; const remoteuser:string='';const remotehost:string='';const remotekey:string=''): IFRE_DB_OBJECT;

  published
    procedure REM_GetDiskInformation    (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
    procedure REM_GetPools              (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
    procedure REM_GetPoolConfiguration  (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
    procedure REM_CreateDiskpool        (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
  end;

implementation

{ TFRE_IOSTAT_PARSER }

procedure TFRE_IOSTAT_PARSER.MyOutStreamCallBack(const stream: TStream);
var st : TStringStream;
    sl : TStringlist;
    i  : integer;
    s  : string;
    lc : integer;

  //  r/s,w/s,kr/s,kw/s,wait,actv,wsvc_t,asvc_t,%w,%b,device
  procedure _UpdateDisk;
  var devicename : string[30];
      diskiostat : TFRE_DB_BLOCKDEVICE_IOSTAT;
  begin
    devicename := Fline[10];
    diskiostat := TFRE_DB_BLOCKDEVICE_IOSTAT.Create;
    try
      diskiostat.Field('rps').AsReal32    := StrToFloat(Fline[0]);
      diskiostat.Field('wps').AsReal32    := StrToFloat(Fline[1]);
      diskiostat.Field('krps').AsReal32   := StrToFloat(Fline[2]);
      diskiostat.Field('kwps').AsReal32   := StrToFloat(Fline[3]);
      diskiostat.Field('wait').AsReal32   := StrToFloat(Fline[4]);
      diskiostat.Field('actv').AsReal32   := StrToFloat(Fline[5]);
      diskiostat.Field('wsvc_t').AsReal32 := StrToFloat(Fline[6]);
      diskiostat.Field('actv_t').AsReal32 := StrToFloat(Fline[7]);
      diskiostat.Field('perc_w').AsReal32 := StrToFloat(Fline[8]);
      diskiostat.Field('perc_b').AsReal32 := StrToFloat(Fline[9]);
    except on E:Exception do begin
      writeln(ClassName,'>>>Mickey Parser Error---');
      s:= Fline.DelimitedText;
      writeln(s);
      writeln(Classname,'<<<Mickey Parser Error---');
    end;end;
    fhal_disk.UpdateDiskIoStatInformation(devicename,diskiostat);
  end;

begin
  stream.Position:=0;
  st := TStringStream.Create('');
  try
    st.CopyFrom(stream,stream.Size);
    stream.Size:=0;
    FLines.DelimitedText := st.DataString;
    if Flines.count>0 then begin
      if pos('extended',Flines[0])=1 then begin
        lc := FLines.Count;
        for i := 2 to lc-2 do begin
          Fline.DelimitedText := Flines[i];
          if pos('extended',Fline[0])=1 then begin
            Continue;
          end;
          if pos('r/s',Fline[0])=1 then begin
            continue;
          end;
          _UpdateDisk;
        end;
      end;
    end else begin
      writeln(ClassName,'*IGNORING JUNK : ',st.Size,': [',st.DataString,']');
    end;
  finally
    st.Free;
  end;
end;

constructor TFRE_IOSTAT_PARSER.Create(const remoteuser, remotekeyfile, remotehost, cmd: string; const hal_disk: TFRE_HAL_DISK);
begin
  inherited Create(remoteuser,remotekeyfile,remotehost,cmd);
  fhal_disk := hal_disk;
end;


{ TFRE_HAL_DISK }

constructor TFRE_HAL_DISK.Create;
begin
  inherited;
  GFRE_TF.Get_Lock(disk_lock);
end;

destructor TFRE_HAL_DISK.Destroy;
begin
  if Assigned(FDiskIostatMon) then
    FDiskIOstatMon.Free;

  disk_lock.Finalize;
  disk_information.Finalize;

  inherited Destroy;
end;

procedure TFRE_HAL_DISK.InitializeDiskandEnclosureInformation(const remoteuser: string; const remotehost: string; const remotekey: string);
var
    disks    : IFRE_DB_Object;
begin
  disks := FetchDiskAndEnclosureInformation(remoteuser,remotehost,remotekey);
  if disks.Field('resultcode').AsInt32<>0 then
    begin
      GFRE_DBI.LogError(dblc_APPLICATION,'COULD NOT GET DISK INFORMATION %d %s',[disks.Field('resultcode').AsInt32,disks.Field('error').AsString]);
      disks.Finalize;
    end
  else
    begin
      disk_lock.Acquire;
      try
        if Assigned(disk_information) then
          disk_information.Finalize;
        disk_information := disks.Field('data').AsObject.CloneToNewObject(false);
        disks.Finalize;
      finally
        disk_lock.Release;
      end;
    end;
end;

procedure TFRE_HAL_DISK.InitializePoolInformation(const remoteuser: string; const remotehost: string; const remotekey: string);
var
    pools    : IFRE_DB_Object;
    pool     : IFRE_DB_Object;
    i        : NativeInt;
    poolname : TFRE_DB_String;

begin

  // send all pool data once
  pools := GetPools(remoteuser,remotehost,remotekey);
  if pools.Field('resultcode').AsInt32<>0 then
    begin
      GFRE_DBI.LogError(dblc_APPLICATION,'COULD NOT GET POOL CONFIGURATION %d %s',[pools.Field('resultcode').AsInt32,pools.Field('error').AsString]);
      pools.Finalize;
    end
  else
    begin
      for i := 0 to pools.Field('data').ValueCount-1 do
        begin
          poolname := pools.Field('data').AsObjectItem[i].Field('name').asstring;
          pool     := FetchPoolConfiguration(poolname,remoteuser,remotehost,remotekey);
          (pool.Field('data').asobject.Implementor_HC as TFRE_DB_ZFS_POOL).setZFSGuid(pools.Field('data').AsObjectItem[i].Field('zpool_guid').asstring);

          disk_lock.Acquire;
          try
            if disk_information.FieldExists('pools')=false then
              disk_information.Field('pools').AddObject(GFRE_DBI.NewObject);
            disk_information.Field('pools').asobject.Field(poolname).asObject:=pool.Field('data').asobject;
          finally
            disk_lock.Release;
          end;
        end;
    end;

end;

procedure TFRE_HAL_DISK.StartIostatMonitor(const remoteuser: string; const remotehost: string; const remotekey: string);
begin
  if remotehost<>'' then
    FDiskIoStatMon := TFRE_IOSTAT_PARSER.Create(remoteuser,remotekey,remotehost,cIOSTATFILEHACKMIST_LOC,self)
  else
    FDiskIoStatMon := TFRE_IOSTAT_PARSER.Create(remoteuser,remotekey,remotehost,cIOSTATFILEHACKMIST,self);

  FDiskIoStatMon.Enable;
end;

function TFRE_HAL_DISK.IsInformationAvailable: boolean;
begin
  result := Assigned(disk_information);
end;

function TFRE_HAL_DISK.GetInformation: IFRE_DB_Object;
begin
  if IsInformationAvailable then
    begin
      disk_lock.Acquire;
      try
        result := disk_information.CloneToNewObject;
      finally
        disk_lock.Release;
      end;
    end
  else
    result := nil;
end;


function TFRE_HAL_DISK.FetchDiskAndEnclosureInformation(const remoteuser: string; const remotehost: string; const remotekey: string): IFRE_DB_OBJECT;
var so    : TFRE_DB_SCSI;
    obj   : IFRE_DB_Object;
    error : string;
    res   : integer;
begin
  so     := TFRE_DB_SCSI.create;
  try
    so.SetRemoteSSH(remoteuser, remotehost, remotekey);
    res    := so.GetSG3DiskAndEnclosureInformation(error,obj);
//    res    := so.GetDiskInformation(error,obj);
    result := GFRE_DBI.NewObject;
    result.Field('resultcode').AsInt32 := res;
    result.Field('error').asstring     := error;
    result.Field('data').AsObject      := obj;
  finally
    so.Free;
  end;
end;

function TFRE_HAL_DISK.GetPools(const remoteuser: string; const remotehost: string; const remotekey: string): IFRE_DB_OBJECT;
var zo    : TFRE_DB_ZFS;
    obj   : IFRE_DB_Object;
    error : string;
    res   : integer;
begin
  zo     := TFRE_DB_ZFS.create;
  try
    zo.SetRemoteSSH(remoteuser, remotehost, remotekey);
    res    := zo.GetPools(error,obj);
    result := GFRE_DBI.NewObject;
    result.Field('resultcode').AsInt32 := res;
    result.Field('error').asstring     := error;
    result.Field('data').AsObject      := obj;
  finally
    zo.Free;
  end;
end;

function TFRE_HAL_DISK.FetchPoolConfiguration(const zfs_pool_name: string; const remoteuser: string; const remotehost: string; const remotekey: string): IFRE_DB_OBJECT;
var zo    : TFRE_DB_ZFS;
    obj   : IFRE_DB_Object;
    error : string;
    res   : integer;
begin
  zo     := TFRE_DB_ZFS.create;
  try
    zo.SetRemoteSSH(remoteuser, remotehost, remotekey);
    res    := zo.GetPoolStatus(zfs_pool_name,error,obj);
    result := GFRE_DBI.NewObject;
    result.Field('resultcode').AsInt32 := res;
    result.Field('error').asstring     := error;
    result.Field('data').AsObject      := obj;
  finally
    zo.Free;
  end;
end;

procedure TFRE_HAL_DISK.UpdateDiskIoStatInformation(const devicename: string; const iostat_information: TFRE_DB_BLOCKDEVICE_IOSTAT);

  function _FindDiskbyDevicename (const fld:IFRE_DB_FIELD):boolean;
  var
    disk : TFRE_DB_ZFS_BLOCKDEVICE;
  begin
    result := false;
    if fld.FieldType=fdbft_Object then
      begin
        disk := (fld.AsObject.Implementor_HC as TFRE_DB_ZFS_BLOCKDEVICE);
        if disk.DeviceName=devicename then
          begin
            disk.IoStat:=iostat_information;
            result :=true;
          end;
      end;
  end;
begin
 disk_lock.Acquire;
 try
   disk_information.Field('disks').AsObject.ForAllFieldsBreak(@_FindDiskbyDevicename);
 finally
   disk_lock.Release;
 end;
end;

function TFRE_HAL_DISK.CreateDiskpool(const input: IFRE_DB_Object; const remoteuser: string; const remotehost: string; const remotekey: string): IFRE_DB_OBJECT;
var zo    : TFRE_DB_ZFS;
    obj   : IFRE_DB_Object;
    error : string;
    res   : integer;
begin
  zo     := TFRE_DB_ZFS.create;
  try
    zo.SetRemoteSSH(remoteuser, remotehost, remotekey);
    res    := zo.CreateDiskpool(input,error,obj);
    result := GFRE_DBI.NewObject;
    result.Field('resultcode').AsInt32 := res;
    result.Field('error').asstring     := error;
    result.Field('data').AsObject      := obj;
  finally
    zo.Free;
  end;
end;

procedure TFRE_HAL_DISK.REM_GetDiskInformation(const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
begin
  // AnswerSyncCommand(command_id,GetDiskInformation(input.Field('remoteuser').asstring,input.Field('remotehost').asstring,input.Field('remotekey').asstring));
  input.Finalize;
end;

procedure TFRE_HAL_DISK.REM_GetPools(const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
begin
  // AnswerSyncCommand(command_id,GetPools(input.Field('remoteuser').asstring,input.Field('remotehost').asstring,input.Field('remotekey').asstring));
  input.Finalize;
end;

procedure TFRE_HAL_DISK.REM_GetPoolConfiguration(const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
begin
  // AnswerSyncCommand(command_id,GetPoolConfiguration(input.Field('poolname').asstring,input.Field('remoteuser').asstring,input.Field('remotehost').asstring,input.Field('remotekey').asstring));
  input.Finalize;
end;

procedure TFRE_HAL_DISK.REM_CreateDiskpool(const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
begin
  // AnswerSyncCommand(command_id,CreateDiskpool(input,input.Field('remoteuser').asstring,input.Field('remotehost').asstring,input.Field('remotekey').asstring));
  input.Finalize;
end;

//function TFOS_STATS_CONTROL.Get_Disk_Data: IFRE_DB_Object;
//var
//  DISKAGGR: IFRE_DB_Object;
//
//  procedure _addDisk(const field: IFRE_DB_Field);
//    begin
//    if pos('C',field.FieldName)<>1 then exit; //fixxme - hack to check for disks
//    DISKAGGR.Field('rps').AsInt64:=DISKAGGR.Field('rps').AsInt64 + field.AsObject.Field('rps').AsInt64;
//    DISKAGGR.Field('wps').AsInt64:=DISKAGGR.Field('wps').AsInt64 + field.AsObject.Field('wps').AsInt64;
//  end;
//
//begin
//  result := FDiskMon.Get_Data_Object;
//  DISKAGGR:=GFRE_DBI.NewObject;
//  DISKAGGR.Field('rps').AsInt64:=0;
//  DISKAGGR.Field('wps').AsInt64:=0;
//  result.ForAllFields(@_addDisk);
//
//  result.Field('disk_aggr').AsObject := DISKAGGR;
//end;


end.

