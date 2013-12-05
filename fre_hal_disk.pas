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

interface

uses
  Classes, SysUtils,FRE_DB_INTERFACE, FRE_DB_COMMON, FRE_PROCESS, FOS_BASIS_TOOLS,
  FOS_TOOL_INTERFACES,FRE_ZFS,fre_scsi;


type

  { TFRE_HAL_DISK }

  TFRE_HAL_DISK = class (TFRE_DB_Base)
  private
    diskenclosure_lock     : IFOS_LOCK;
    pools_lock             : IFOS_LOCK;

    diskenclosure_information          : IFRE_DB_Object;
    pools_information                  : IFRE_DB_Object;

  public
    constructor Create                 ; override;
    destructor Destroy                 ; override;

    procedure InitializeDiskandEnclosureInformation (const remoteuser:string='';const remotehost:string='';const remotekey:string='');
    procedure InitializePoolInformation             (const remoteuser:string='';const remotehost:string='';const remotekey:string='');

    function  IsDiskandEnclosureInformationAvailable:boolean;
    function  IsPoolInformationAvailable            :boolean;

    function  GetPoolsInformation                   : IFRE_DB_Object;
    function  GetDiskandEnclosureInformation        : IFRE_DB_Object;

    function  FetchDiskAndEnclosureInformation      (const remoteuser:string='';const remotehost:string='';const remotekey:string=''): IFRE_DB_OBJECT;
    function  FetchPoolConfiguration                (const zfs_pool_name:string; const remoteuser:string='';const remotehost:string='';const remotekey:string=''): IFRE_DB_OBJECT;

    function  GetPools                  (const remoteuser:string='';const remotehost:string='';const remotekey:string=''): IFRE_DB_OBJECT;
    function  CreateDiskpool            (const input:IFRE_DB_Object; const remoteuser:string='';const remotehost:string='';const remotekey:string=''): IFRE_DB_OBJECT;

  published
    procedure REM_GetDiskInformation    (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
    procedure REM_GetPools              (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
    procedure REM_GetPoolConfiguration  (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
    procedure REM_CreateDiskpool        (const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE);
  end;

implementation




{ TFRE_HAL_DISK }

constructor TFRE_HAL_DISK.Create;
begin
  inherited;
  GFRE_TF.Get_Lock(diskenclosure_lock);
  GFRE_TF.Get_Lock(pools_lock);
end;

destructor TFRE_HAL_DISK.Destroy;
begin
  pools_lock.Finalize;
  diskenclosure_lock.Finalize;
  pools_information.Finalize;
  diskenclosure_information.Finalize;

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
      diskenclosure_lock.Acquire;
      try
        if Assigned(diskenclosure_information) then
          diskenclosure_information.Finalize;
        diskenclosure_information := disks;
      finally
        diskenclosure_lock.Release;
      end;
    end;
end;

procedure TFRE_HAL_DISK.InitializePoolInformation(const remoteuser: string; const remotehost: string; const remotekey: string);
var
    pools    : IFRE_DB_Object;
    pool     : IFRE_DB_Object;
    i        : NativeInt;
    poolname : TFRE_DB_String;
    pool_res : IFRE_DB_Object;

begin
  pool_res   := GFRE_DBI.NewObject;

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
          pool_res.Field('data').AddObject(pool.Field('data').asobject);
        end;
      pools_lock.Acquire;
      try
        if Assigned(pools_information) then
          pools_information.Finalize;
        pools_information := pool_res;
      finally
        pools_lock.Release;
      end;
    end;

end;

function TFRE_HAL_DISK.IsDiskandEnclosureInformationAvailable: boolean;
begin
  result := Assigned(diskenclosure_information);
end;

function TFRE_HAL_DISK.IsPoolInformationAvailable: boolean;
begin
  result := Assigned(pools_information);
end;

function TFRE_HAL_DISK.GetPoolsInformation: IFRE_DB_Object;
begin
  if IsPoolInformationAvailable then
    begin
      pools_lock.Acquire;
      try
        result := pools_information.CloneToNewObject;
      finally
        pools_lock.Release;
      end;
    end
  else
    result := nil;
end;

function TFRE_HAL_DISK.GetDiskandEnclosureInformation: IFRE_DB_Object;
begin
  if IsDiskandEnclosureInformationAvailable then
    begin
      diskenclosure_lock.Acquire;
      try
        result := diskenclosure_information.CloneToNewObject;
      finally
        diskenclosure_lock.Release;
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
//      res    := so.GetSG3DiskAndEnclosureInformation(error,obj);
    res    := so.GetDiskInformation(error,obj);
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

end.

