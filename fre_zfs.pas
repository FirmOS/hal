unit fre_zfs;

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
  FOS_TOOL_INTERFACES,fre_testcase;

type

  TFRE_DB_ZFS_RAID_LEVEL      = (zfs_rl_stripe,zfs_rl_mirror,zfs_rl_z1,zfs_rl_z2,zfs_rl_z3,zfs_rl_undefined);

const

  CFRE_DB_ZFS_RAID_LEVEL      : array [TFRE_DB_ZFS_RAID_LEVEL] of string        = ('rl_stripe','rl_mirror','rl_z1','rl_z2','rl_z3','rl_undefined');
  CFRE_DB_ZFS_POOL_COLLECTION        = 'pool';
  CFRE_DB_ZFS_VDEV_COLLECTION        = 'vdev';
  CFRE_DB_ZFS_BLOCKDEVICE_COLLECTION = 'blockdevice';


type
  EFOS_ZFS_Exception=class(Exception);

  TFRE_DB_ZFS_SendMode = (zfsSend,zfsSendReplicated,zfsSendRecursive,zfsSendRecursiveProperties);

  TFRE_DB_ZFS_ROOTOBJ=class;

  { TFRE_DB_ZFS_OBJ }

  TFRE_DB_ZFS_OBJ=class(TFRE_DB_ObjectEx)
  private
    function  GetIopsRead      : TFRE_DB_String;
    function  GetIopsWrite     : TFRE_DB_String;
    function  GetParentInZFS   : TGuid;
    function  GetPoolId        : TGuid;
    function  GetTransferRead  : TFRE_DB_String;
    function  GetTransferWrite : TFRE_DB_String;
    procedure SetIopsRead      (AValue: TFRE_DB_String);
    procedure SetIopsWrite     (AValue: TFRE_DB_String);
    function  getCaption       : TFRE_DB_String; virtual;
    procedure setCaption       (avalue: TFRE_DB_String); virtual;
    procedure SetParentInZFSId (AValue: TGuid);
    procedure SetPoolId        (AValue: TGuid);
    procedure SetTransferRead  (AValue: TFRE_DB_String);
    procedure SetTransferWrite (AValue: TFRE_DB_String);
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    procedure removeFromPool          ;
    function  getPool                 (const conn: IFRE_DB_CONNECTION): TFRE_DB_ZFS_ROOTOBJ; virtual;
    function  getZFSParent            (const conn: IFRE_DB_CONNECTION): TFRE_DB_ZFS_OBJ;
    function  getId                   : TFRE_DB_String; virtual;
    function  getChildren             (const conn: IFRE_DB_CONNECTION): IFRE_DB_ObjectArray; virtual;
    function  mayHaveChildren         : Boolean; virtual;
    function  acceptsNewChildren      (const conn: IFRE_DB_CONNECTION): Boolean; virtual;
    function  getIsModified           : Boolean;
    function  getIsNew                : Boolean;
    procedure setIsModified           (const avalue:Boolean=true);
    procedure setIsNew                (const avalue:Boolean=true);
    property  caption                 : TFRE_DB_String read GetCaption       write SetCaption;
    property  iopsR                   : TFRE_DB_String read GetIopsRead      write SetIopsRead;
    property  iopsW                   : TFRE_DB_String read GetIopsWrite     write SetIopsWrite;
    property  transferR               : TFRE_DB_String read GetTransferRead  write SetTransferRead;
    property  transferW               : TFRE_DB_String read GetTransferWrite write SetTransferWrite;
    property  poolId                  : TGuid          read GetPoolId        write SetPoolId;
    property  parentInZFSId           : TGuid          read GetParentInZFS   write SetParentInZFSId;
  end;

  { TFRE_DB_DISK_MPATH }

  TFRE_DB_DISK_MPATH=class(TFRE_DB_ObjectEx)
  private
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
  end;

  TFRE_DB_ZFS_BLOCKDEVICE=class(TFRE_DB_ZFS_OBJ)
  private
    function  getIsOffline    : Boolean;
    procedure setIsOffline    (AValue: Boolean);
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    function  mayHaveChildren : Boolean; override;
    property  isOffline       : Boolean read getIsOffline write setIsOffline;
  end;

  { TFRE_DB_ZFS_DISKCONTAINER }

  TFRE_DB_ZFS_DISKCONTAINER=class(TFRE_DB_ZFS_OBJ)
  private
    function  GetRaidLevel       : TFRE_DB_ZFS_RAID_LEVEL; virtual;
    procedure SetRaidLevel       (AValue: TFRE_DB_ZFS_RAID_LEVEL); virtual;
  public
    function  addBlockdevice            (const blockdevice: TFRE_DB_ZFS_BLOCKDEVICE): TFRE_DB_ZFS_BLOCKDEVICE; virtual;
    function  addBlockdeviceEmbedded    (const blockdevice: TFRE_DB_ZFS_BLOCKDEVICE): TFRE_DB_ZFS_BLOCKDEVICE; virtual;
    function  createBlockdeviceEmbedded : TFRE_DB_ZFS_BLOCKDEVICE; virtual;
    function  mayHaveChildren           : Boolean; override;
    function  acceptsNewChildren        (const conn: IFRE_DB_CONNECTION): Boolean; override;
    function  getLastChildId            (const conn: IFRE_DB_CONNECTION): String; //FIXXME - remove store update
    property  raidLevel                 : TFRE_DB_ZFS_RAID_LEVEL read GetRaidLevel write SetRaidLevel;
  end;

  { TFRE_DB_ZFS_VDEV }

  TFRE_DB_ZFS_VDEV=class(TFRE_DB_ZFS_DISKCONTAINER)
  private
    function  GetRaidLevel       : TFRE_DB_ZFS_RAID_LEVEL; override;
    procedure SetRaidLevel       (AValue: TFRE_DB_ZFS_RAID_LEVEL); override;
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    function acceptsNewChildren  (const conn: IFRE_DB_CONNECTION): Boolean; override;
  end;

  { TFRE_DB_ZFS_VDEVCONTAINER }

  TFRE_DB_ZFS_VDEVCONTAINER=class(TFRE_DB_ZFS_DISKCONTAINER)
  private
    function  GetRaidLevel       : TFRE_DB_ZFS_RAID_LEVEL; override;
    procedure SetRaidLevel       (AValue: TFRE_DB_ZFS_RAID_LEVEL); override;
  public
    function  createVdev         : TFRE_DB_ZFS_VDEV; virtual;
    function  createVdevEmbedded : TFRE_DB_ZFS_VDEV; virtual;
  end;

  { TFRE_DB_ZFS_DATASTORAGE }

  TFRE_DB_ZFS_DATASTORAGE=class(TFRE_DB_ZFS_VDEVCONTAINER)
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
  end;

  { TFRE_DB_ZFS_SPARE }

  TFRE_DB_ZFS_SPARE=class(TFRE_DB_ZFS_DISKCONTAINER)
  private
    function  GetRaidLevel       : TFRE_DB_ZFS_RAID_LEVEL; override;
    procedure SetRaidLevel       (AValue: TFRE_DB_ZFS_RAID_LEVEL); override;
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
  end;

  { TFRE_DB_ZFS_LOG }

  TFRE_DB_ZFS_LOG=class(TFRE_DB_ZFS_VDEVCONTAINER)
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
  end;

  { TFRE_DB_ZFS_CACHE }

  TFRE_DB_ZFS_CACHE=class(TFRE_DB_ZFS_DISKCONTAINER)
  private
    function  GetRaidLevel       : TFRE_DB_ZFS_RAID_LEVEL; override;
    procedure SetRaidLevel       (AValue: TFRE_DB_ZFS_RAID_LEVEL); override;
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
  end;

  { TFRE_DB_ZFS_DATASET }

  TFRE_DB_ZFS_PARSE_DATASET=class(TFRE_DB_ZFS_OBJ)
  private
  public
  end;

  { TFRE_DB_ZFS_ROOTOBJ }

  TFRE_DB_ZFS_ROOTOBJ=class(TFRE_DB_ZFS_OBJ)
  public
    function mayHaveChildren    : Boolean; override;
    function acceptsNewChildren (const conn: IFRE_DB_CONNECTION): Boolean; override;
    function getPool            (const conn: IFRE_DB_CONNECTION): TFRE_DB_ZFS_ROOTOBJ; override;
    function getPoolName        : TFRE_DB_String;
    function getLastChildId     (const conn: IFRE_DB_CONNECTION): String; //FIXXME - remove store update
  end;

  { TFRE_DB_ZFS_POOL }

  TFRE_DB_ZFS_POOL=class(TFRE_DB_ZFS_ROOTOBJ)
  private
    function  getCaption       : TFRE_DB_String; override;
    procedure setCaption       (avalue: TFRE_DB_String); override;
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    function  GetDatastorage            (const conn: IFRE_DB_CONNECTION): TFRE_DB_ZFS_DATASTORAGE;
    function  GetDatastorageEmbedded    : TFRE_DB_ZFS_DATASTORAGE;
    function  GetSpare                  (const conn: IFRE_DB_CONNECTION): TFRE_DB_ZFS_SPARE;
    function  GetCache                  (const conn: IFRE_DB_CONNECTION): TFRE_DB_ZFS_CACHE;
    function  GetLog                    (const conn: IFRE_DB_CONNECTION): TFRE_DB_ZFS_LOG;
    function  createDatastorage         : TFRE_DB_ZFS_DATASTORAGE;
    function  createDatastorageEmbedded : TFRE_DB_ZFS_DATASTORAGE;
    function  createCache               : TFRE_DB_ZFS_CACHE;
    function  createCacheEmbedded       : TFRE_DB_ZFS_CACHE;
    function  createLog                 : TFRE_DB_ZFS_LOG;
    function  createLogEmbedded         : TFRE_DB_ZFS_LOG;
    function  createSpare               : TFRE_DB_ZFS_SPARE;
    function  createSpareEmbedded       : TFRE_DB_ZFS_SPARE;
  end;

  { TFRE_DB_ZFS_UNASSIGNED }

  TFRE_DB_ZFS_UNASSIGNED=class(TFRE_DB_ZFS_ROOTOBJ)
    function  addBlockdevice            (const blockdevice: TFRE_DB_ZFS_BLOCKDEVICE): TFRE_DB_ZFS_BLOCKDEVICE;
    function  addBlockdeviceEmbedded    (const blockdevice: TFRE_DB_ZFS_BLOCKDEVICE): TFRE_DB_ZFS_BLOCKDEVICE;
  end;

  { TFRE_DB_ZFS }

  TFRE_DB_ZFS = class (TFRE_DB_Multiprocess)
  private
    procedure       AddParameter                (const parameter : string; var  params  : TFRE_DB_StringArray);
    procedure       AnalyzeSnapshots            (const proc   : TFRE_DB_Process);
    procedure       AnalyzePoolStatus           (const proc   : TFRE_DB_Process);
    procedure       AnalyzeDataSetSpace         (const proc   : TFRE_DB_Process);
    procedure       AnalyzePoolList             (const proc   : TFRE_DB_Process);
    procedure       AnalyzePrtConfDisks         (const proc   : TFRE_DB_Process;const disks:IFRE_DB_Object);
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    function        CreateSnapShot              (const dataset: string; const snapshotname : string; out error : string ) : integer;
    procedure       GetSnapshots                (const dataset: string; const foscmdmode: boolean);
    function        DestroySnapshot             (const dataset: string; const snapshotname : string; out error : string) : integer;
    function        SnapShotExists              (const dataset: string; const snapshotname : string; out creation_ts: TFRE_DB_DateTime64; out error : string ; out exists : boolean;const foscmdmode: boolean=false) : integer;
    function        GetLastSnapShot             (const dataset: string; const snapshotkey : string; out snapshotname : string; out creation_ts: TFRE_DB_DateTime64; out error : string;const foscmdmode: boolean=false) : integer;

    function        CreateDataset               (const dataset: string; out error: string) : integer;
    function        DestroyDataset              (const dataset: string; out error: string; const recursive: boolean=false; const dependents: boolean=false ; const force : boolean=false) : integer;

    function        DataSetExists               (const dataset: string; out error : string; out exists : boolean; const foscmdmode :boolean=false) : integer;
    function        SendSnapshot                (const dataset: string; const snapshotname : string; const destinationhost : string; const destinationuser: string; const destinationdataset : string; const destinationkeyfilename :string; out error :string; const incrementalsnapshot: string=''; const sendmode : TFRE_DB_ZFS_Sendmode=zfsSend; const compressed : boolean=true; const destinationport: integer = 22;const foscmdmode: boolean=false) : integer;

    function        Scrub                       (const poolname: string; out error : string) : integer;
    function        GetPoolStatus               (const poolname: string; out error : string; out pool: IFRE_DB_Object) : integer;
    function        GetDataset                  (const dataset: string; out error : string; out datasetobject: IFRE_DB_Object) : integer;
    function        GetPools                    (out error: string; out pools:IFRE_DB_Object) : integer;
    function        GetDiskInformation          (out error: string; out disks:IFRE_DB_Object) : integer;
    function        CreateDiskPool              (const pool_definition:IFRE_DB_Object; out error: string; out pool_result:IFRE_DB_Object) : integer;
  end;

  { TFRE_DB_ZFSJob }

  TFRE_DB_ZFSJob = class (TFRE_DB_Testcase)
  private
    procedure       _SnapShotReplicate          (const do_replicate: boolean);
    procedure       _SnapShotCheck              ;
    procedure       _PoolStatus                 ;
    procedure       _DataSetSpace               ;
    procedure       _AnalyzePool                (const proc   : IFRE_DB_Object);
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    procedure       InternalSetup               ; override;
  public
    procedure       ExecuteCMD                  ;
    procedure       SetScrub                    (const poolname: string);
    procedure       SetPoolStatus               (const poolname: string; const scrub_warning_days: integer; const scrub_error_days: integer);
    procedure       SetDatasetspace             (const dataset: string; const warning_percent: integer; const error_percent: integer);
    procedure       SetReplicate                (const sourcedataset: string; const destinationdataset:string; const snapshotkey : string; const destinationhost : string; const destinationuser : string; const destinationkeyfilename : string; const replicationkeyfilename : string; const destinationport: integer=22; const replicationport: integer=22);
    procedure       SetSnapshot                 (const dataset: string; const snapshotkey : string);
    procedure       SetSnapshotCheck            (const dataset:string; const snapshotkey : string; const warning_seconds: integer; const error_seconds: integer);
  published
    function        IMI_Do_the_Job              (const input: IFRE_DB_Object): IFRE_DB_Object;
  end;


procedure Register_DB_Extensions;

function String2DBZFSRaidLevelType(const fts: string): TFRE_DB_ZFS_RAID_LEVEL;

implementation

{ TFRE_DB_ZFS_UNASSIGNED }

function TFRE_DB_ZFS_UNASSIGNED.addBlockdevice(const blockdevice: TFRE_DB_ZFS_BLOCKDEVICE): TFRE_DB_ZFS_BLOCKDEVICE;
begin
  blockdevice.parentInZFSId:=UID;
  blockdevice.poolId:=poolId;
  Result:=blockdevice;
end;

function TFRE_DB_ZFS_UNASSIGNED.addBlockdeviceEmbedded(const blockdevice: TFRE_DB_ZFS_BLOCKDEVICE): TFRE_DB_ZFS_BLOCKDEVICE;
begin
  Field('vdev').AddObject(blockdevice);
end;

{ TFRE_DB_DISK_MPATH }

class procedure TFRE_DB_DISK_MPATH.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

{ TFRE_DB_ZFS_ROOTOBJ }

function TFRE_DB_ZFS_ROOTOBJ.mayHaveChildren: Boolean;
begin
  Result:=true;
end;

function TFRE_DB_ZFS_ROOTOBJ.acceptsNewChildren(const conn: IFRE_DB_CONNECTION): Boolean;
begin
  Result:=true;
end;

function TFRE_DB_ZFS_ROOTOBJ.getPool(const conn: IFRE_DB_CONNECTION): TFRE_DB_ZFS_ROOTOBJ;
begin
  Result:=Self;
end;

function TFRE_DB_ZFS_ROOTOBJ.getPoolName: TFRE_DB_String;
begin
  Result:=getCaption;
end;

function TFRE_DB_ZFS_ROOTOBJ.getLastChildId(const conn: IFRE_DB_CONNECTION): String;
var
  children: IFRE_DB_ObjectArray;
begin
  children:=getChildren(conn);
  if Length(children)>0 then begin
    Result:=(children[Length(children)-1].Implementor_HC as TFRE_DB_ZFS_OBJ).getId;
  end else begin
    Result:='';
  end;
end;

{ TFRE_DB_ZFS_DISKCONTAINER }

function TFRE_DB_ZFS_DISKCONTAINER.GetRaidLevel: TFRE_DB_ZFS_RAID_LEVEL;
begin
  if FieldExists('raidLevel') then begin
    Result:=String2DBZFSRaidLevelType(Field('raidLevel').AsString);
  end else begin
    Result:=zfs_rl_stripe;
  end;
end;

procedure TFRE_DB_ZFS_DISKCONTAINER.SetRaidLevel(AValue: TFRE_DB_ZFS_RAID_LEVEL);
begin
  Field('raidLevel').AsString:=CFRE_DB_ZFS_RAID_LEVEL[AValue];
end;

function TFRE_DB_ZFS_DISKCONTAINER.addBlockdevice(const blockdevice: TFRE_DB_ZFS_BLOCKDEVICE): TFRE_DB_ZFS_BLOCKDEVICE;
begin
  blockdevice.parentInZFSId:=UID;
  blockdevice.poolId:=poolId;
  Result:=blockdevice;
end;

function TFRE_DB_ZFS_DISKCONTAINER.addBlockdeviceEmbedded(const blockdevice: TFRE_DB_ZFS_BLOCKDEVICE): TFRE_DB_ZFS_BLOCKDEVICE;
begin
  Field('vdev').AddObject(blockdevice);
end;

function TFRE_DB_ZFS_DISKCONTAINER.createBlockdeviceEmbedded: TFRE_DB_ZFS_BLOCKDEVICE;
begin
  Result:=TFRE_DB_ZFS_BLOCKDEVICE.create;
  Field('vdev').AddObject(Result);
end;

function TFRE_DB_ZFS_DISKCONTAINER.mayHaveChildren: Boolean;
begin
  Result:=true;
end;

function TFRE_DB_ZFS_DISKCONTAINER.acceptsNewChildren(const conn: IFRE_DB_CONNECTION): Boolean;
begin
  Result:=true;
end;

function TFRE_DB_ZFS_DISKCONTAINER.getLastChildId(const conn: IFRE_DB_CONNECTION): String;
var
  children: IFRE_DB_ObjectArray;
begin
  children:=getChildren(conn);
  if Length(children)>0 then begin
    Result:=(children[Length(children)-1].Implementor_HC as TFRE_DB_ZFS_OBJ).getId;
  end else begin
    Result:='';
  end;
end;

{ TFRE_DB_ZFS_RAIDCONTAINER }

function TFRE_DB_ZFS_VDEVCONTAINER.GetRaidLevel: TFRE_DB_ZFS_RAID_LEVEL;
begin
  Result:=zfs_rl_undefined;
end;

procedure TFRE_DB_ZFS_VDEVCONTAINER.SetRaidLevel(AValue: TFRE_DB_ZFS_RAID_LEVEL);
begin
  if AValue<>zfs_rl_stripe then raise EFOS_ZFS_Exception.Create('A Vdev container has to be striped.');
  inherited SetRaidLevel(AValue);
end;

function TFRE_DB_ZFS_VDEVCONTAINER.createVdev: TFRE_DB_ZFS_VDEV;
begin
  Result:=TFRE_DB_ZFS_VDEV.create;
  Result.parentInZFSId:=UID;
  Result.poolId:=poolId;
end;

function TFRE_DB_ZFS_VDEVCONTAINER.createVdevEmbedded: TFRE_DB_ZFS_VDEV;
begin
  Result:=TFRE_DB_ZFS_VDEV.create;
  Field('vdev').AddObject(Result);
end;

{ TFRE_DB_ZFS_DATASTORAGE }

class procedure TFRE_DB_ZFS_DATASTORAGE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

{ TFRE_DB_ZFS_OBJ }

function TFRE_DB_ZFS_OBJ.getIsNew: Boolean;
begin
  Result:=(FieldExists('isNew') and Field('isNew').AsBoolean);
end;

function TFRE_DB_ZFS_OBJ.GetParentInZFS: TGuid;
begin
  Field('parent_in_zfs_uid').AsObjectLink;
end;

function TFRE_DB_ZFS_OBJ.GetPoolId: TGuid;
begin
  Result:=Field('pool_uid').AsGUID;
end;

function TFRE_DB_ZFS_OBJ.GetTransferRead: TFRE_DB_String;
begin
  Result:=Field('transfer_r').AsString;
end;

function TFRE_DB_ZFS_OBJ.GetTransferWrite: TFRE_DB_String;
begin
 Result:=Field('transfer_w').AsString;
end;

procedure TFRE_DB_ZFS_OBJ.SetIopsRead(AValue: TFRE_DB_String);
begin
  Field('iops_r').AsString:=AValue;
end;

procedure TFRE_DB_ZFS_OBJ.SetIopsWrite(AValue: TFRE_DB_String);
begin
  Field('iops_w').AsString:=AValue;
end;

function TFRE_DB_ZFS_OBJ.GetIopsRead: TFRE_DB_String;
begin
  Result:=Field('iops_r').AsString;
end;

function TFRE_DB_ZFS_OBJ.GetIopsWrite: TFRE_DB_String;
begin
 Result:=Field('iops_w').AsString;
end;

function TFRE_DB_ZFS_OBJ.getIsModified: Boolean;
begin
  Result:=(FieldExists('isModified') and Field('isModified').AsBoolean);
end;

class procedure TFRE_DB_ZFS_OBJ.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

procedure TFRE_DB_ZFS_OBJ.removeFromPool;
begin
   DeleteField('parent_in_zfs_uid');
   DeleteField('pool_uid');
end;

function TFRE_DB_ZFS_OBJ.getPool(const conn: IFRE_DB_CONNECTION): TFRE_DB_ZFS_ROOTOBJ;
var
  obj: IFRE_DB_Object;
begin
  CheckDbResult(conn.Fetch(poolId,obj),'TFRE_DB_ZFS_OBJ.getPool');
  Result:=obj.Implementor_HC as TFRE_DB_ZFS_ROOTOBJ;
end;

function TFRE_DB_ZFS_OBJ.getZFSParent(const conn: IFRE_DB_CONNECTION): TFRE_DB_ZFS_OBJ;
var
   zfsParent: IFRE_DB_Object;
begin
   conn.Fetch(parentInZFSId,zfsParent);
   if Assigned(zfsParent) then begin
     Result:=zfsParent.Implementor_HC as TFRE_DB_ZFS_OBJ;
   end else begin
     Result:=Nil;
   end;
end;

function TFRE_DB_ZFS_OBJ.getCaption: TFRE_DB_String;
begin
  Result:=Field('name').AsString;
end;

procedure TFRE_DB_ZFS_OBJ.setCaption(avalue: TFRE_DB_String);
begin
  Field('name').AsString:=avalue;
end;

procedure TFRE_DB_ZFS_OBJ.SetParentInZFSId(AValue: TGuid);
begin
  Field('parent_in_zfs_uid').AsObjectLink:=AValue;
end;

procedure TFRE_DB_ZFS_OBJ.SetPoolId(AValue: TGuid);
begin
  Field('pool_uid').AsGUID:=AValue;
end;

procedure TFRE_DB_ZFS_OBJ.SetTransferRead(AValue: TFRE_DB_String);
begin
  Field('transfer_r').AsString:=AValue;
end;

procedure TFRE_DB_ZFS_OBJ.SetTransferWrite(AValue: TFRE_DB_String);
begin
  Field('transfer_w').AsString:=AValue;
end;

function TFRE_DB_ZFS_OBJ.getId: TFRE_DB_String;
begin
  Result:=UID_String;
end;

function TFRE_DB_ZFS_OBJ.getChildren(const conn: IFRE_DB_CONNECTION): IFRE_DB_ObjectArray;
var
  refs: TFRE_DB_GUIDArray;
  obj : IFRE_DB_Object;
  i   : Integer;
begin
  refs:=conn.GetReferences(Self.UID,false,'');
  SetLength(Result,Length(refs));
  for i := 0 to Length(refs) - 1 do begin
    CheckDbResult(conn.Fetch(refs[i],obj),'TFRE_DB_ZFS_OBJ.getChildren');
    Result[i]:=obj;
  end;
end;

function TFRE_DB_ZFS_OBJ.mayHaveChildren: Boolean;
begin
  Result:=false;
end;

function TFRE_DB_ZFS_OBJ.acceptsNewChildren(const conn: IFRE_DB_CONNECTION): Boolean;
begin
  Result:=false;
end;

procedure TFRE_DB_ZFS_OBJ.setIsModified(const avalue:Boolean);
begin
  Field('isModified').AsBoolean:=avalue;
end;

procedure TFRE_DB_ZFS_OBJ.setIsNew(const avalue:Boolean);
begin
  Field('isNew').AsBoolean:=avalue;
  setIsModified;
end;

{ TFRE_DB_ZFS_CACHE }

function TFRE_DB_ZFS_CACHE.GetRaidLevel: TFRE_DB_ZFS_RAID_LEVEL;
begin
  Result:=zfs_rl_stripe;
end;

procedure TFRE_DB_ZFS_CACHE.SetRaidLevel(AValue: TFRE_DB_ZFS_RAID_LEVEL);
begin
  if AValue<>zfs_rl_stripe then raise EFOS_ZFS_Exception.Create('A cache container has to be striped.');
  inherited SetRaidLevel(AValue);
end;

class procedure TFRE_DB_ZFS_CACHE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

{ TFRE_DB_ZFS_LOG }

class procedure TFRE_DB_ZFS_LOG.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

{ TFRE_DB_ZFS_SPARE }

function TFRE_DB_ZFS_SPARE.GetRaidLevel: TFRE_DB_ZFS_RAID_LEVEL;
begin
  Result:=zfs_rl_undefined;
end;

procedure TFRE_DB_ZFS_SPARE.SetRaidLevel(AValue: TFRE_DB_ZFS_RAID_LEVEL);
begin
  if AValue<>zfs_rl_undefined then raise EFOS_ZFS_Exception.Create('A spare container does not support a raid level.');
  inherited SetRaidLevel(AValue);
end;

class procedure TFRE_DB_ZFS_SPARE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

{ TFRE_DB_ZFS_VDEV }

function TFRE_DB_ZFS_VDEV.GetRaidLevel: TFRE_DB_ZFS_RAID_LEVEL;
begin
  if FieldExists('raidLevel') then begin
    Result:=String2DBZFSRaidLevelType(Field('raidLevel').AsString);
  end else begin
    Result:=zfs_rl_undefined;
  end;
end;

procedure TFRE_DB_ZFS_VDEV.SetRaidLevel(AValue: TFRE_DB_ZFS_RAID_LEVEL);
begin
  if AValue=zfs_rl_stripe then raise EFOS_ZFS_Exception.Create('Raid level stripe is not allowd for a Vdev.');
  inherited SetRaidLevel(AValue);
end;

class procedure TFRE_DB_ZFS_VDEV.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

function TFRE_DB_ZFS_VDEV.acceptsNewChildren(const conn: IFRE_DB_CONNECTION): Boolean;
var
  zfsParent: TFRE_DB_ZFS_OBJ;
begin
  zfsParent:=getZFSParent(conn);
  if getIsNew or (Assigned(zfsParent) and (zfsParent.Implementor_HC is TFRE_DB_ZFS_LOG)) then begin
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

{ TFRE_DB_ZFS_BLOCKDEVICE }

function TFRE_DB_ZFS_BLOCKDEVICE.getIsOffline: Boolean;
begin
   Result:=(FieldExists('isOffline') and Field('isOffline').AsBoolean);
end;

procedure TFRE_DB_ZFS_BLOCKDEVICE.setIsOffline(AValue: Boolean);
begin
  Field('isOffline').AsBoolean:=AValue;
end;

class procedure TFRE_DB_ZFS_BLOCKDEVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

function TFRE_DB_ZFS_BLOCKDEVICE.mayHaveChildren: Boolean;
begin
  Result:=false;
end;

{ TFRE_DB_ZFS_POOL }

function TFRE_DB_ZFS_POOL.GetDatastorage(const conn: IFRE_DB_CONNECTION): TFRE_DB_ZFS_DATASTORAGE;
var
  i       : Integer;
  children: IFRE_DB_ObjectArray;
begin
  Result:=nil;
  children:=getChildren(conn);
  for i := 0 to Length(children) - 1 do begin
    if children[i].Implementor_HC is TFRE_DB_ZFS_DATASTORAGE then begin
      Result:=children[i].Implementor_HC as TFRE_DB_ZFS_DATASTORAGE;
      break;
    end;
  end;
end;

function TFRE_DB_ZFS_POOL.GetDatastorageEmbedded: TFRE_DB_ZFS_DATASTORAGE;
var
  i: Integer;
begin
  for i := 0 to Field('vdev').ValueCount - 1 do begin
    if Field('vdev').AsObjectItem[i].Implementor_HC is TFRE_DB_ZFS_DATASTORAGE then begin
      Result:=Field('vdev').AsObjectItem[i].Implementor_HC as TFRE_DB_ZFS_DATASTORAGE;
      break;
    end;
  end;
end;

function TFRE_DB_ZFS_POOL.GetSpare(const conn: IFRE_DB_CONNECTION): TFRE_DB_ZFS_SPARE;
var
  i       : Integer;
  children: IFRE_DB_ObjectArray;
begin
  Result:=nil;
  children:=getChildren(conn);
  for i := 0 to Length(children) - 1 do begin
    if children[i].Implementor_HC is TFRE_DB_ZFS_SPARE then begin
      Result:=children[i].Implementor_HC as TFRE_DB_ZFS_SPARE;
      break;
    end;
  end;
end;

function TFRE_DB_ZFS_POOL.GetCache(const conn: IFRE_DB_CONNECTION): TFRE_DB_ZFS_CACHE;
var
  i       : Integer;
  children: IFRE_DB_ObjectArray;
begin
  Result:=nil;
  children:=getChildren(conn);
  for i := 0 to Length(children) - 1 do begin
    if children[i].Implementor_HC is TFRE_DB_ZFS_CACHE then begin
      Result:=children[i].Implementor_HC as TFRE_DB_ZFS_CACHE;
      break;
    end;
  end;
end;

function TFRE_DB_ZFS_POOL.GetLog(const conn: IFRE_DB_CONNECTION): TFRE_DB_ZFS_LOG;
var
  i       : Integer;
  children: IFRE_DB_ObjectArray;
begin
  Result:=nil;
  children:=getChildren(conn);
  for i := 0 to Length(children) - 1 do begin
    if children[i].Implementor_HC is TFRE_DB_ZFS_LOG then begin
      Result:=children[i].Implementor_HC as TFRE_DB_ZFS_LOG;
      break;
    end;
  end;
end;

class procedure TFRE_DB_ZFS_POOL.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

function TFRE_DB_ZFS_POOL.getCaption: TFRE_DB_String;
begin
  Result:=Field('pool').AsString;
end;

procedure TFRE_DB_ZFS_POOL.setCaption(avalue: TFRE_DB_String);
begin
  Field('pool').AsString:=avalue;
end;

function TFRE_DB_ZFS_POOL.createDatastorage: TFRE_DB_ZFS_DATASTORAGE;
var
  coll: IFRE_DB_COLLECTION;
begin
  Result:=TFRE_DB_ZFS_DATASTORAGE.create;
  Result.poolId:=UID;
  Result.parentInZFSId:=UID;
end;

function TFRE_DB_ZFS_POOL.createDatastorageEmbedded: TFRE_DB_ZFS_DATASTORAGE;
begin
  Result:=TFRE_DB_ZFS_DATASTORAGE.create;
  Field('vdev').AddObject(Result);
end;

function TFRE_DB_ZFS_POOL.createCache: TFRE_DB_ZFS_CACHE;
begin
  Result:=TFRE_DB_ZFS_CACHE.create;
  Result.poolId:=UID;
  Result.parentInZFSId:=UID;
end;

function TFRE_DB_ZFS_POOL.createCacheEmbedded: TFRE_DB_ZFS_CACHE;
begin
  Result:=TFRE_DB_ZFS_CACHE.create;
  Field('vdev').AddObject(Result);
end;

function TFRE_DB_ZFS_POOL.createLog: TFRE_DB_ZFS_LOG;
begin
  Result:=TFRE_DB_ZFS_LOG.create;
  Result.poolId:=UID;
  Result.parentInZFSId:=UID;
end;

function TFRE_DB_ZFS_POOL.createLogEmbedded: TFRE_DB_ZFS_LOG;
begin
  Result:=TFRE_DB_ZFS_LOG.create;
  Field('vdev').AddObject(Result);
end;

function TFRE_DB_ZFS_POOL.createSpare: TFRE_DB_ZFS_SPARE;
var
  coll: IFRE_DB_COLLECTION;
begin
  Result:=TFRE_DB_ZFS_SPARE.create;
  Result.poolId:=UID;
  Result.parentInZFSId:=UID;
end;

function TFRE_DB_ZFS_POOL.createSpareEmbedded: TFRE_DB_ZFS_SPARE;
begin
  Result:=TFRE_DB_ZFS_SPARE.create;
  Field('vdev').AddObject(Result);
end;

{ TFRE_DB_ZFSJob }

procedure TFRE_DB_ZFSJob._SnapShotReplicate(const do_replicate: boolean);
var res                     : integer;
    error                   : string;
    exists                  : boolean;
    snapshotname            : string;
    snapshotkey             : string;
    incrementalsnapshotname : string;
    source_ts               : TFRE_DB_DateTime64;
    destination_ts          : TFRE_DB_DateTime64;

  function SourceZFS : TFRE_DB_ZFS;
  begin
    result   := TFRE_DB_ZFS.create;
    if Field('remotehost').AsString<>'' then begin
      result.SetRemoteSSH (Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);
    end;
  end;

  function DestinationZFS : TFRE_DB_ZFS;
  begin
    result   := TFRE_DB_ZFS.create;
    result.SetRemoteSSH(config.Field('destinationuser').AsString,config.Field('destinationhost').AsString, config.Field('destinationkeyfilename').AsString, config.Field('destinationport').AsInt32);
  end;

  function ResultCheck(const msg: string):boolean;
  begin
    result := false;
    if res<>0 then begin
      SetStatus(statusFailure,msg+':'+error);
      result := true;
    end;
  end;

begin
  snapshotkey   := config.Field('snapshotkey').AsString;

  res           := SourceZFS.GetLastSnapShot(config.Field('sourcedataset').AsString, snapshotkey, snapshotname,source_ts,error);
  if res = 0 then begin
    snapshotname := GFRE_BT.SepRight(snapshotname,'-');
    snapshotname := snapshotkey+'-'+inttostr(strtoint(snapshotname)+1);
  end else begin
    snapshotname := snapshotkey+'-'+'1';
  end;
  res           := SourceZFS.CreateSnapShot(config.Field('sourcedataset').AsString,snapshotname,error);
  if ResultCheck('CAN NOT CREATE SOURCE SNAPSHOT') then exit;
  res           := SourceZFS.SnapShotExists(config.Field('sourcedataset').AsString,snapshotname,source_ts,error,exists);           // Check if source snapshot exists, get source_ts
  if ResultCheck('CAN NOT GET SOURCE SNAPSHOT CREATION TIME') then exit;

  if do_replicate then begin
    res           := DestinationZFS.DataSetExists(config.Field('destinationdataset').AsString,error,exists,true);                          // Check if destination dataset exists
    if ResultCheck('CAN NOT CHECK IF DESTINATION DATASET EXISTS') then exit;
    if not exists then begin
      incrementalsnapshotname := '';
    end else begin
      //    ICECREMENTAL :-)
      writeln('NOW INCREMENTAL');
      res         := DestinationZFS.GetLastSnapShot(config.Field('destinationdataset').AsString,'',incrementalsnapshotname,destination_ts,error,true);
      if ResultCheck('CAN NOT GET LAST SNAPSHOT FOR DESTINATION DATASET') then exit;
      writeln(incrementalsnapshotname);
    end;

    res         := SourceZFS.SendSnapshot(config.Field('sourcedataset').AsString,snapshotname,config.Field('destinationhost').AsString,config.Field('destinationuser').AsString,config.Field('destinationdataset').AsString,config.Field('replicationkeyfilename').AsString, error, incrementalsnapshotname, zfsSendReplicated,true,config.Field('replicationport').asint32,true);   // Send Snapshotname to Destination
    if ResultCheck('ERROR ON SENDING SNAPSHOT') then exit;
    writeln(error);
    if error<>'' then begin                                                                                                                // Result was OK, write Warning
      SetStatus(statusWarning,'WARNING ON ZFS SEND/RECEIVE');
      report.Field('STATUSDETAIL').asstring := error;
    end;

    res           := DestinationZFS.SnapShotExists(config.Field('destinationdataset').AsString,snapshotname,destination_ts,error,exists,true);            // Check if destination snapshot exists
    if ResultCheck('CAN NOT CHECK IF DESTINATION SNAPSHOT EXISTS AFTER RECEIVE') then exit;
    if exists then begin
      if source_ts=destination_ts then begin                                                                                                             // Check matching timestamp
        SetStatus(statusOK, 'REPLICATION OF SNAPSHOT '+snapshotname+ ' SUCCESSFUL');
      end else begin
        SetStatus(statusFailure, 'CREATION TIME OF SNAPSHOTS '+snapshotname+' NOT IDENTICAL SOURCE:'+ GFRE_DT.ToStrFOS(source_ts)+' DESTINATION:'+GFRE_DT.ToStrFOS(destination_ts));
      end;
    end;
  end else begin
    SetStatus(statusOK, 'CREATION OF SNAPSHOT '+snapshotname+ ' SUCCESSFUL');
  end;
end;


procedure TFRE_DB_ZFSJob._SnapShotCheck;
var
  zfs          : TFRE_DB_ZFS;
  res          : integer;
  snapshotname : string;
  creation_ts  : TFRE_DB_DateTime64;
  error        : string;
  dt           : TFRE_DB_DateTime64;
  diffs        : integer;

begin
 zfs   := TFRE_DB_ZFS.Create;
 zfs.SetRemoteSSH (Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);
 res   := zfs.GetLastSnapShot(config.Field('dataset').AsString,config.Field('snapshotkey').AsString,snapshotname,creation_ts,error);
 if res<>0 then begin
   SetStatus(statusFailure,'NO SNAPSHOT FOUND FOR DATASET '+config.Field('dataset').AsString);
   report.Field('STATUSDETAIL').asstring := error;
 end else begin
   report.Field('SNAPSHOT').asstring      := snapshotname;
   report.Field('CREATION').AsDateTimeUTC := creation_ts;
   dt  := GFRE_DT.Now_UTC;
   diffs    := (dt - creation_ts) div 1000;
   if diffs < config.Field('warning_seconds').AsInt32 then begin
     SetStatus(statusOK,'SNAPSHOT CHECK OK SECONDS:'+inttostr(diffs));
   end else begin
     if diffs < config.Field('error_seconds').AsInt32  then begin
       SetStatus(statusWarning,'SNAPSHOT TOO OLD SECONDS:'+inttostr(diffs));
     end else begin
       SetStatus(statusFailure,'SNAPSHOT TOO OLD SECONDS:'+inttostr(diffs));
     end;
   end;
 end;
end;

procedure TFRE_DB_ZFSJob._PoolStatus;
var zfs    : TFRE_DB_ZFS;
    res    : integer;
    error  : string;
    pool   : IFRE_DB_Object;
begin
  zfs      := TFRE_DB_ZFS.create;
  zfs.SetRemoteSSH (Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);
  res      := zfs.GetPoolStatus(config.Field('poolname').AsString,error,pool);
  if res<>0 then begin
    SetStatus(statusFailure,'FAILURE ON CHECKING POOL '+config.Field('poolname').AsString);
    report.Field('STATUSDETAIL').asstring := error;
  end else begin
    _AnalyzePool(zfs.GetProcess(0));
  end;
  writeln(report.DumpToString());
end;

procedure TFRE_DB_ZFSJob._DataSetSpace;
var zfs             : TFRE_DB_ZFS;
    res             : integer;
    error           : string;
    datasetobject   : IFRE_DB_Object;
    total           : UInt64;
    percent         : double;
    percent_b       : byte;
begin
  zfs      := TFRE_DB_ZFS.create;
  zfs.SetRemoteSSH (Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);
  res      := zfs.GetDataset(config.Field('dataset').AsString,error,datasetobject);
//  writeln(datasetobject.DumpToString());
  if res<>0 then begin
    SetStatus(statusFailure,'FAILURE ON GETTING DATASET SPACE '+config.Field('dataset').AsString);
    report.Field('STATUSDETAIL').asstring := error;
  end else begin
    total     := datasetobject.Field('avail').AsUInt64+datasetobject.Field('used').AsUInt64;
    percent   := (datasetobject.Field('used').AsUInt64 / total);
    percent_b := round(percent*100);
    writeln(percent_b);
    if percent_b>config.Field('error_percent').AsByte then begin
      SetStatus(statusFailure,'DATASET SPACE LIMIT '+inttostr(config.Field('error_percent').AsByte)+'% EXCEEDED :'+inttostr(percent_b)+'%');
    end else if percent_b>config.Field('warning_percent').AsByte then begin
      SetStatus(statusWarning,'DATASET SPACE LIMIT '+inttostr(config.Field('warning_percent').AsByte)+'% WARNING :'+inttostr(percent_b)+'%');
    end else begin
      SetStatus(statusOK,'DATASET SPACE LIMIT OK :'+inttostr(percent_b)+'%');
    end;
    report.Field('STATUSDETAIL').asstring := 'TOTAL:'+inttostr((total div (1024*1024*1024)))+'GB AVAIL:'+inttostr((datasetobject.Field('avail').AsUInt64 div (1024*1024*1024)))+'GB USED:'+inttostr((datasetobject.Field('used').AsUInt64 div (1024*1024*1024)))+'GB';
  end;
end;

procedure TFRE_DB_ZFSJob._AnalyzePool(const proc: IFRE_DB_Object);
var dates   : string;
    scans   : string;
    ts      : TFRE_DB_DateTime64;
    nowt    : int64;
    daydiff : int64;
    i       : integer;
    pool    : IFRE_DB_Object;
begin
  report.CopyField(proc,'exitstatus');
  report.CopyField(proc,'errorstring');
  pool                              := proc.Field('pool').asobject;
  report.Field('pool').asobject     := pool;
  report.Field('poolname').asstring := pool.Field('pool').asstring;
  if pool.Field('state').asstring<>'ONLINE' then begin
    SetStatus(statusFailure, 'POOL '+report.Field('poolname').asstring+' IN STATE '+pool.Field('state').asstring);
  end;
  if pool.FieldExists('scan') then begin
    scans    := pool.Field('scan').AsString;
    if pos(' on ',scans)>0 then begin
      dates  := Copy(scans,Pos(' on ',scans)+4,maxint);
//      writeln('DATES:',dates);
      try
        ts   := GFRE_DT.FromHttp(dates);
      except
        ts   := 0;
      end;
      report.Field('scan_ts').AsDateTimeUTC := ts;
      nowt   := GFRE_DT.Now_UTC;
      daydiff:= (((nowt-ts) div 1000) div 86400);
      if daydiff < config.Field('scrub_warning_days').AsInt32 then begin
        SetStatus(statusOK, 'LAST SCAN FOR POOL '+report.Field('poolname').asstring+' '+inttostr(daydiff)+' DAYS AGO.');
      end else begin
        if daydiff < config.Field('scrub_error_days').AsInt32 then begin
          SetStatus(statusWarning, 'LAST SCAN FOR POOL '+report.Field('poolname').asstring+' '+inttostr(daydiff)+' DAYS AGO.');
        end else begin
          SetStatus(statusFailure, 'LAST SCAN FOR POOL '+report.Field('poolname').asstring+' '+inttostr(daydiff)+' DAYS AGO.');
        end;
      end;
    end else begin
     if Pos(' in progress ',scans)>0 then begin
      dates  := Copy(scans,Pos('since',scans)+6,24);
//      writeln('DATES:',dates,'-');
      try
        ts   := GFRE_DT.FromHttp(dates);
      except
        ts   := 0;
      end;
      report.Field('scan_ts').AsDateTimeUTC := ts;
      SetStatus(statusOK, 'SCAN FOR POOL '+report.Field('poolname').asstring+' IN PROGRESS.');
     end else begin
      SetStatus(statusFailure, 'UNKNOWN SCAN INFORMATION FOR POOL '+report.Field('poolname').asstring);
     end;
    end;
  end else begin
    SetStatus(statusFailure, 'NO SCAN INFORMATION FOR POOL '+report.Field('poolname').asstring);
  end;
end;

class procedure TFRE_DB_ZFSJob.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
//  scheme.SetParentSchemeByName  ('TFRE_DB_TESTCASE');
end;

procedure TFRE_DB_ZFSJob.InternalSetup;
begin
  inherited InternalSetup;
  Field('MAX_ALLOWED_TIME').AsInt32 := 86400;
end;

procedure TFRE_DB_ZFSJob.ExecuteCMD;
var
  cmd     : string;
  zfs     : TFRE_DB_ZFS;
  errors  : string;

  procedure SetupZFS;
  begin
    zfs   := TFRE_DB_ZFS.Create;
    zfs.SetRemoteSSH (Field('remoteuser').asstring, Field('remotehost').asstring, Field('remotekeyfilename').asstring);
  end;

begin
  cmd := config.Field('cmd').asstring;
  case cmd of
    'scrub' : begin
      SetupZFS;
      Field('exitstatus').asint32   := zfs.Scrub(config.Field('poolname').asstring,errors);
      Field('errorstring').asstring := errors;
    end;
    'replicate': begin
      _SnapShotReplicate(true);
    end;
    'snapshot': begin
      _SnapShotReplicate(false);
    end;
    'snapshotcheck': begin
      _Snapshotcheck;
    end;
    'poolstatus': begin
      _PoolStatus;
    end;
    'datasetspace': begin
      _DataSetspace;
    end
  else
    raise EFOS_TESTCASE_Exception.Create('UNKNOWN ZFS CMD '+cmd);
  end;
end;

function TFRE_DB_ZFSJob.IMI_Do_the_Job(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  writeln(DumpToString);
  Field('starttime').AsDateTimeUTC:=GFRE_DT.Now_UTC;
  ExecuteCMD;
  Field('endtime').AsDateTimeUTC:=GFRE_DT.Now_UTC;
end;


{ TFRE_DB_ZFS }

procedure TFRE_DB_ZFS.AddParameter(const parameter: string; var params: TFRE_DB_StringArray);
begin
 SetLength(params,Length(params)+1);
 params[high(params)] := parameter;
end;

procedure TFRE_DB_ZFS.AnalyzeSnapshots(const proc: TFRE_DB_Process);
var
  slist    : TStringList;
  props    : TFRE_DB_StringArray;
  snapo    : IFRE_DB_Object;
  i        : integer;
  snapname : string;
begin
  if proc.ExitStatus=0 then begin
    slist := TStringList.Create;
    try
      slist.text  := proc.OutputToString;
      for  i  := 0 to slist.count -1 do begin
        GFRE_BT.SeperateString(slist[i],#9,props);
        snapo := GFRE_DBI.NewObject;
        snapname                               := GFRE_BT.SepRight(props[0],'@');
        snapo.Field('name').AsString           := snapname;
        snapo.Field('creation').AsDateTimeUTC  := StrToInt64(props[1])*1000;
        snapo.Field('used').AsInt64            := StrToInt64(props[2]);
        Field('snapshots').AddObject(snapo);
      end;
    finally
      slist.Free;
    end;
  end;
end;

procedure TFRE_DB_ZFS.AnalyzePoolStatus(const proc: TFRE_DB_Process);
var  slist   : TStringList;
     i       : integer;
     cmd     : string;
     namep   : integer;
     statep  : integer;
     readp   : integer;
     writep  : integer;
     cksump  : integer;
     pool    : TFRE_DB_ZFS_POOL;

    //  pool: zones
    // state: ONLINE
    //status: One or more devices has experienced an unrecoverable error.  An
    //	attempt was made to correct the error.  Applications are unaffected.
    //action: Determine if the device needs to be replaced, and clear the errors
    //	using 'zpool clear' or replace the device with 'zpool replace'.
    //   see: http://illumos.org/msg/ZFS-8000-9P
    //  scan: scrub repaired 0 in 4h57m with 0 errors on Tue Sep 11 15:06:55 2012
    //config:
    //
    //	NAME        STATE     READ WRITE CKSUM
    //	zones       ONLINE       0     0     0
    //	  raidz2-0  ONLINE       0     0     0
    //	    c3t3d0  ONLINE       0     0     2
    //	    c3t5d0  ONLINE       0     0     0
    //	    c3t1d0  ONLINE       0     0     1
    //	    c3t0d0  ONLINE       0     0     0
    //	    c3t4d0  ONLINE       0     0     0
    //	    c3t2d0  ONLINE       0     0     0
    //	logs
    //	  c1t1d0    ONLINE       0     0     0
    //
    //errors: No known data errors

    //      pool: testpool
    // state: ONLINE
    //  scan: none requested
    //config:
    //
    //        NAME                     STATE     READ WRITE CKSUM
    //        testpool                 ONLINE       0     0     0
    //          raidz1-0               ONLINE       0     0     0
    //            /zones/testfiles/11  ONLINE       0     0     0
    //            /zones/testfiles/12  ONLINE       0     0     0
    //            /zones/testfiles/13  ONLINE       0     0     0
    //          raidz1-1               ONLINE       0     0     0
    //            /zones/testfiles/21  ONLINE       0     0     0
    //            /zones/testfiles/22  ONLINE       0     0     0
    //            /zones/testfiles/23  ONLINE       0     0     0
    //
  procedure Parseline (line : string);
  var newcmd    : string;
      cmdline   : boolean;
      devname   : string;
      dev       : IFRE_DB_Object;
      parentdev : Array [0 .. 10 ] of IFRE_DB_Object;
      devlvl    : integer;

      function DevLevel(name : string) : integer;
      begin
       result := 0;
       while (Pos('  ',name)=1) do begin
        name := Copy(name, 3, maxint);
        inc(result);
       end;
      end;

  begin
    newcmd    := trim(Copy ( line, 1, 8));
    cmdline   := false;
    if Pos    (':',newcmd)>0 then begin
      cmd     := Copy(newcmd,1,Pos(':',newcmd)-1);
      cmdline := true;
    end;
    if cmd <> 'config' then begin
      writeln(line );
      if not cmdline then begin
        line   := trim( line );
        pool.Field(cmd).AsString  := pool.Field(cmd).AsString + ' '+ line;
      end else begin
        line   := trim ( Copy ( line, Pos(':',line)+1, maxint ));
        pool.Field(cmd).AsString  := line;
      end;
    end else begin
      if (length (line) > 0) and (not cmdline)  then begin
        if Pos('NAME',line) > 0  then begin
          namep  := Pos('NAME' , line);
          statep := Pos('STATE' , line);
          readp  := Pos('READ', line);
          writep := Pos('WRITE', line);
          cksump := Pos('CKSUM', line);
        end else begin
          devname                       := Copy(line,namep,statep-namep-2);
          devlvl                        := DevLevel(devname);
          if (Pos('raid',trim(lowercase(devname)))=1) then begin
            dev := (parentdev[devlvl-1].Implementor_HC as TFRE_DB_ZFS_VDEVCONTAINER).createVdevEmbedded;
            if (Pos('raidz1',trim(lowercase(devname)))=1) then (dev.Implementor_HC as TFRE_DB_ZFS_VDEV).SetRaidLevel(zfs_rl_z1)
              else if (Pos('raidz2',trim(lowercase(devname)))=1) then (dev.Implementor_HC as TFRE_DB_ZFS_VDEV).SetRaidLevel(zfs_rl_z2)
                else if (Pos('raidz3',trim(lowercase(devname)))=1) then (dev.Implementor_HC as TFRE_DB_ZFS_VDEV).SetRaidLevel(zfs_rl_z3)
                  else (dev.Implementor_HC as TFRE_DB_ZFS_VDEV).SetRaidLevel(zfs_rl_undefined);
          end else if (Pos('mirror',trim(lowercase(devname)))=1) then begin
            dev := (parentdev[devlvl-1].Implementor_HC as TFRE_DB_ZFS_VDEVCONTAINER).createVdevEmbedded;
            (dev.Implementor_HC as TFRE_DB_ZFS_VDEV).SetRaidLevel(zfs_rl_mirror);
          end else if Pos('spare',trim(lowercase(devname)))=1 then begin
            dev := pool.createSpareEmbedded;
          end else if Pos('log',trim(lowercase(devname)))=1 then begin
            dev := pool.createLogEmbedded;
          end else if Pos('cache',trim(lowercase(devname)))=1 then begin
            dev := pool.createCacheEmbedded;
          end else begin
            if devlvl = 0 then begin
              dev := pool.createDatastorageEmbedded;
            end else begin
              dev := (parentdev[devlvl-1].Implementor_HC as TFRE_DB_ZFS_DISKCONTAINER).createBlockdeviceEmbedded;
            end;
          end;
          dev.Field('name').AsString    := trim(devname);
          dev.Field('state').AsString   := trim(Copy (line,statep,readp-statep-1));
          dev.Field('read').AsString    := trim(Copy (line,readp,writep-readp-1));
          dev.Field('write').AsString   := trim(Copy (line,writep,cksump-writep-1));
          dev.Field('cksum').AsString   := trim(Copy (line,cksump,maxint));
          parentdev [devlvl ]           := dev;
        end;
      end;
    end;
  end;

begin
  slist := TStringList.Create;
  try
     slist.text                          := TFRE_DB_Process (proc.Implementor_HC).OutputToString;
     pool                                := TFRE_DB_ZFS_POOL.Create;
     proc.Field('pool').asobject         := pool;
     for i  := 0 to slist.Count -1 do begin
       ParseLine(slist[i]);
     end;
  finally
    slist.Free;
  end;
end;

procedure TFRE_DB_ZFS.AnalyzeDataSetSpace(const proc: TFRE_DB_Process);
var  slist         : TStringList;
     llist         : TStringList;
     datasetobject : TFRE_DB_ZFS_PARSE_DATASET;
//     NAME    USED  AVAIL  REFER
//     zones  3741499296256  178121831936  4795709424


begin
  slist := TStringList.Create;
  llist := TStringList.Create;
  try
     slist.text                          := TFRE_DB_Process (proc.Implementor_HC).OutputToString;
     llist.CommaText:= slist[1];
     datasetobject                        := TFRE_DB_ZFS_PARSE_DATASET.Create;
     datasetobject.Field('used').AsUInt64 := StrToInt64(trim(llist[1]));
     datasetobject.Field('avail').AsUInt64:= StrToInt64(trim(llist[2]));
     datasetobject.Field('refer').AsUInt64:= StrToInt64(trim(llist[3]));
     proc.Field('datasetobject').asobject := datasetobject;
  finally
    llist.free;
    slist.Free;
  end;
end;

procedure TFRE_DB_ZFS.AnalyzePoolList(const proc: TFRE_DB_Process);
var  slist         : TStringList;
     llist         : TStringList;
     pool          : IFRE_DB_Object;
     i             : NativeInt;
begin
  slist := TStringList.Create;
  llist := TStringList.Create;
  try
     slist.text                          := TFRE_DB_Process (proc.Implementor_HC).OutputToString;
     for i:= 1 to slist.count-1 do
       begin
         llist.CommaText:= slist[i];
         pool           := GFRE_DBI.NewObject;
         pool.Field('name').AsString       := trim(llist[0]);
         pool.Field('zpool_guid').AsString := trim(llist[1]);
         proc.Field('pools').addobject(pool);
       end;
  finally
    llist.free;
    slist.Free;
  end;
end;

procedure TFRE_DB_ZFS.AnalyzePrtConfDisks(const proc: TFRE_DB_Process;const disks:IFRE_DB_Object);
type
     TPrtConf_Parsestate = (psInvalid,psNoDisk,psDiskInfo,psDiskMPT,psDeviceName);

var  slist         : TStringList;
     disk          : IFRE_DB_Object;
     mpath         : IFRE_DB_Object;
     i             : NativeInt;
     s             : TFRE_DB_String;
     devicename    : TFRE_DB_String;
     pstate        : TPrtConf_Parsestate;


     procedure _newMpath;
     begin
        mpath  :=GFRE_DBI.NewObjectScheme(TFRE_DB_DISK_MPATH);
        if Pos('online',s)>0 then
          mpath.Field('online').asboolean:=true
        else
          mpath.Field('online').asboolean:=false
     end;

     function GetValueS(const valuestring: TFRE_DB_String): TFRE_DB_String;
     begin
       result := GFRE_BT.SepRight(valuestring,'''');
       result := GFRE_BT.SepLeft(result,'''');
     end;

     function GetValueHexUInt32(const valuestring: TFRE_DB_String): UInt32;
     begin
       result := strtoint('$'+GFRE_BT.SepRight(valuestring,'='));
     end;

     function GetDirectS(const valuestring: TFRE_DB_String): TFRE_DB_String;
     begin
       result := GFRE_BT.SepRight(valuestring,'=');
     end;

begin
  slist := TStringList.Create;
  try
     slist.text              := TFRE_DB_Process (proc.Implementor_HC).OutputToString;
     pstate                  := psNoDisk;

     for i:= 1 to slist.count-1 do
       begin
         s := slist[i];

         case pstate of
           psNoDisk:           begin
                                 if Pos('disk, instance',s)>0 then
                                   begin
                                     pstate := psDiskInfo;
//                                     writeln('{899838B2-08CF-97CE-4C85-67FEDCF05C16}','disk found',s);
                                     disk   := GFRE_DBI.NewObjectScheme(TFRE_DB_ZFS_BLOCKDEVICE);
                                   end;
                               end;
           psDiskInfo:        begin
//                                writeln('DISKINFO:',s);
                                 if Pos('name=''inquiry-serial-no''',s)>0 then
                                   disk.Field('serial').AsString:=GetValueS(slist[i+1]);
                                 if Pos('name=''inquiry-revision-id''',s)>0 then
                                   disk.Field('revision').AsString:=GetValueS(slist[i+1]);
                                 if Pos('name=''inquiry-product-id''',s)>0 then
                                   disk.Field('product').AsString:=GetValueS(slist[i+1]);
                                 if Pos('name=''inquiry-vendor-id''',s)>0 then
                                   disk.Field('vendor').AsString:=GetValueS(slist[i+1]);

                                 if Pos('mpt_sas',s)>0 then
                                   begin
                                     _newMpath;
                                     pstate :=psDiskMPT;
                                   end;

                                 if Pos('Device Minor Nodes',s)>0 then
                                   pstate :=psDeviceName;
                               end;
           psDiskMPT:          begin
                                 if Pos('name=''wwn''',s)>0 then
                                   mpath.Field('wwn').AsString:=GetValueS(slist[i+1]);
                                 if Pos('name=''target-port''',s)>0 then
                                   mpath.Field('target-port').AsString:=GetValueS(slist[i+1]);
                                 if Pos('name=''attached-port''',s)>0 then
                                   mpath.Field('attached-port').AsString:=GetValueS(slist[i+1]);
                                 if Pos('name=''path-class''',s)>0 then
                                   mpath.Field('path-class').AsString:=GetValueS(slist[i+1]);
                                 if Pos('name=''phy-num''',s)>0 then
                                   mpath.Field('phy-num').AsUInt32:=GetValueHexUInt32(slist[i+1]);


                                 if Pos('mpt_sas',s)>0 then                                      // next mpath
                                   begin
                                     disk.Field('mpath').AddObject(mpath);
                                     _newmpath;
                                   end;

                                 if Pos('Device Minor Nodes',s)>0 then                          // end of mpath
                                   begin
//                                     writeln('add mpath');
                                     disk.Field('mpath').AddObject(mpath);
                                     pstate :=psDeviceName;
                                   end;

                                 if Pos('disk, instance',s)>0 then                              // no devicename, ignore
                                   begin
                                     disk.Finalize;
                                     pstate := psDiskInfo;
                                     disk   := GFRE_DBI.NewObjectScheme(TFRE_DB_ZFS_BLOCKDEVICE);
                                   end;

                               end;
           psDeviceName:       begin
                                 if Pos('dev_link',s)>0 then
                                   begin
                                     devicename := GetDirectS(s);
                                     devicename := Copy(devicename,1,length(devicename)-2);
                                     disk.Field('device').AsString:=devicename;
                                     disks.Field('disks').addobject(disk);
                                     pstate := psNoDisk;
                                   end;
                               end
         else
           raise Exception.Create('INVALID PRTCONF PARSE STATE:'+inttostr(ord(pstate)));
         end;
       end;
  finally
    slist.Free;
  end;
end;



class procedure TFRE_DB_ZFS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

function TFRE_DB_ZFS.CreateSnapShot(const dataset: string; const snapshotname: string; out error: string): integer;
var
  proc  : TFRE_DB_Process;
begin
  ClearProcess;
  proc := TFRE_DB_Process.create;
  proc.SetupInput('zfs',TFRE_DB_StringArray.Create('snapshot','-r',dataset+'@'+snapshotname));
  AddProcess(proc);
  ExecuteMulti;
  result := proc.Field('exitstatus').AsInt32;
  error  := proc.Field('errorstring').AsString;
end;

function TFRE_DB_ZFS.DataSetExists(const dataset: string; out error: string; out exists: boolean; const foscmdmode: boolean): integer;
var
  proc  : TFRE_DB_Process;
begin
  ClearProcess;
  exists := false;
  proc   := TFRE_DB_Process.create;
  if foscmdmode=false then begin
    proc.SetupInput('zfs',TFRE_DB_StringArray.Create('list','-H','-o','name',dataset));
  end else begin
    proc.SetupInput('foscmd',TFRE_DB_StringArray.Create('DSEXISTS',dataset));
  end;
  AddProcess(proc);
  ExecuteMulti;
  result := proc.Field('exitstatus').AsInt32;
  if result = 0 then begin
    if Pos(dataset,proc.OutputToString)=1 then begin
      exists := true;
    end;
  end else begin
    if Pos('does not exist',proc.ErrorToString)>0 then begin
      result := 0;
    end;
  end;
  error  := proc.Field('errorstring').AsString;
end;

function TFRE_DB_ZFS.SnapShotExists(const dataset: string; const snapshotname: string; out creation_ts: TFRE_DB_DateTime64; out error: string; out exists: boolean; const foscmdmode: boolean): integer;
var
  isnap      : integer;
  proc       : TFRE_DB_Process;
  snapo      : IFRE_DB_Object;
begin
  exists       := false;
  creation_ts  := 0;
  GetSnapshots(dataset,foscmdmode);
  proc         := GetProcess(0);
  if FieldExists('snapshots') then begin
    for isnap  := 0 to Field('snapshots').ValueCount-1 do begin
      snapo    := Field('snapshots').AsObjectItem[isnap];
      if snapo.Field('name').asstring = snapshotname then begin
        creation_ts  := snapo.Field('creation').AsDateTimeUTC;
        exists       := true;
        break;
      end;
    end;
  end;
//  writeln('Exitstatus:',proc.ExitStatus);
  result       := proc.ExitStatus;
  error        := proc.ErrorToString;
end;

function TFRE_DB_ZFS.GetLastSnapShot(const dataset: string; const snapshotkey: string; out snapshotname: string; out creation_ts: TFRE_DB_DateTime64; out error: string; const foscmdmode: boolean): integer;
var
  isnap      : integer;
  proc       : TFRE_DB_Process;
  snapo      : IFRE_DB_Object;
begin
  GetSnapshots(dataset,foscmdmode);
  creation_ts  := 0;
  snapshotname := '';
  proc         := GetProcess(0);
  result       := proc.ExitStatus;
  error        := proc.ErrorToString;
  if FieldExists('snapshots') then begin
    for isnap    := Field('snapshots').ValueCount-1 downto 0 do begin
      snapo      := Field('snapshots').AsObjectItem[isnap];
      if (snapshotkey='') or (Pos(snapshotkey,snapo.Field('name').asstring)=1) then begin
        snapshotname := snapo.Field('name').asstring;
        creation_ts  := snapo.Field('creation').AsDateTimeUTC;
        exit;
      end;
    end;
  end;
  // not found
  result       := -1;
end;

function TFRE_DB_ZFS.CreateDataset(const dataset: string; out error: string): integer;
var
  proc  : TFRE_DB_Process;
begin
  ClearProcess;
  proc := TFRE_DB_Process.create;
  proc.SetupInput('zfs',TFRE_DB_StringArray.Create('create',dataset));
  AddProcess(proc);
  ExecuteMulti;
  result := proc.Field('exitstatus').AsInt32;
  error  := proc.Field('errorstring').AsString;
end;

function TFRE_DB_ZFS.DestroyDataset(const dataset: string; out error: string; const recursive: boolean; const dependents: boolean; const force: boolean): integer;
var
  proc   : TFRE_DB_Process;
  params : TFRE_DB_StringArray;
  pcount : integer;


begin
  ClearProcess;
  proc      := TFRE_DB_Process.create;
  SetLength(params,1);
  params[0] := 'destroy';
  if recursive then AddParameter('-r',params);
  if dependents then AddParameter('-R',params);
  if force then AddParameter('-f',params);
  AddParameter('-p',params);
  AddParameter(dataset,params);
  proc.SetupInput('zfs',params);
  AddProcess(proc);
  ExecuteMulti;
  result := proc.Field('exitstatus').AsInt32;
  error  := proc.Field('errorstring').AsString;
//  writeln(proc.DumpToString);
//  writeln('OUT:',proc.OutputToString);
end;

procedure TFRE_DB_ZFS.GetSnapshots(const dataset: string; const foscmdmode: boolean);
var
  proc  : TFRE_DB_Process;
begin
  ClearProcess;
  proc := TFRE_DB_Process.create;
  if foscmdmode=false then begin
    proc.SetupInput('zfs',TFRE_DB_StringArray.Create('list','-r','-H','-p','-t','snapshot','-o','name,creation,used',dataset));
  end else begin
    proc.SetupInput('foscmd',TFRE_DB_StringArray.Create('GETSNAPSHOTS',dataset));
  end;
  proc.Field('dataset').AsString:=dataset;
  AddProcess(proc);
  ExecuteMulti;
  AnalyzeSnapshots(proc);
end;

function TFRE_DB_ZFS.DestroySnapshot(const dataset: string; const snapshotname: string; out error: string): integer;
var
  proc  : TFRE_DB_Process;
begin
  ClearProcess;
  proc := TFRE_DB_Process.create;
  proc.SetupInput('zfs',TFRE_DB_StringArray.Create('destroy','-r',dataset+'@'+snapshotname));
  AddProcess(proc);
  ExecuteMulti;
  result := proc.Field('exitstatus').AsInt32;
  error  := proc.Field('errorstring').AsString;
end;

function TFRE_DB_ZFS.SendSnapshot(const dataset: string; const snapshotname: string; const destinationhost: string; const destinationuser: string; const destinationdataset: string; const destinationkeyfilename: string; out error: string; const incrementalsnapshot: string; const sendmode: TFRE_DB_ZFS_Sendmode; const compressed: boolean; const destinationport: integer; const foscmdmode: boolean): integer;
var
  proc     : TFRE_DB_Process;
  zcommand : string;
begin
  ClearProcess;
  proc := TFRE_DB_Process.create;
//  ssh -i ~/.firmos/fre/ssl/user/id_rsa root@10.1.0.116 zfs "send testpool/ds1@20121016 | ssh -i /zones/rootkey/id_rsa 10.1.0.130 zfs receive zones/testpool/ds1"
  zcommand := '"'+ 'zfs send ';
  case sendmode of
    zfsSend                     : ; // no additional options
    zfsSendReplicated           : zcommand := zcommand + '-R ';
    zfsSendRecursive            : zcommand := zcommand + '-r ';
    zfsSendRecursiveProperties  : zcommand := zcommand + '-r -p ';
   else
    raise EFOS_ZFS_Exception.Create('INVALID ZFS SEND MODE');
  end;
  if length(incrementalsnapshot)>0 then begin
    zcommand := zcommand+'-I '+dataset+'@'+incrementalsnapshot+' ';
  end;
  zcommand := zcommand +dataset+'@'+snapshotname;
  if compressed then begin
    zcommand := zcommand +' | bzip2 -c';
  end;
  zcommand := zcommand +' | ssh -i '+destinationkeyfilename+' -p '+ inttostr(destinationport)+' '+destinationuser+'@'+destinationhost+' ''';
  if foscmdmode=false then begin
    if compressed then begin
      zcommand := zcommand +'bzcat | ';
    end;
    zcommand := zcommand + 'zfs receive -u -F '+ destinationdataset+''' "';
  end else begin
    if compressed then begin
      zcommand := zcommand + 'foscmd RECEIVEBZ '+ destinationdataset+''' "';
    end else begin
      zcommand := zcommand + 'foscmd RECEIVE '+ destinationdataset+''' "';
    end;
  end;
//  writeln(zcommand);
  proc.SetupInput(zcommand,nil);
  AddProcess(proc);
  ExecuteMulti;
  result := proc.Field('exitstatus').AsInt32;
  error  := proc.Field('errorstring').AsString;
end;

function TFRE_DB_ZFS.Scrub(const poolname: string; out error: string): integer;
var
  proc  : TFRE_DB_Process;
begin
  Clearprocess;
  proc := TFRE_DB_Process.create;
  proc.SetupInput('zpool',TFRE_DB_StringArray.Create('scrub',poolname));
  AddProcess(proc);
  ExecuteMulti;
  result := proc.Field('exitstatus').AsInt32;
  error  := proc.Field('errorstring').AsString;
end;

function TFRE_DB_ZFS.GetPoolStatus(const poolname: string; out error: string; out pool: IFRE_DB_Object): integer;
var
  proc  : TFRE_DB_Process;
begin
  ClearProcess;
  proc := TFRE_DB_Process.create;
  proc.SetupInput('zpool',TFRE_DB_StringArray.Create ('status',poolname));
  AddProcess(proc);
  ExecuteMulti;
  AnalyzePoolStatus(proc);
  pool   := proc.Field('pool').AsObject.CloneToNewObject();
  result := proc.Field('exitstatus').AsInt32;
  error  := proc.Field('errorstring').AsString;
end;

function TFRE_DB_ZFS.GetDataset(const dataset: string; out error: string; out datasetobject: IFRE_DB_Object): integer;
var
  proc  : TFRE_DB_Process;
begin
  ClearProcess;
  proc := TFRE_DB_Process.create;
  proc.SetupInput('zfs',TFRE_DB_StringArray.Create ('list','-p','-o','name,used,avail,refer',dataset));
  AddProcess(proc);
  ExecuteMulti;
  AnalyzeDataSetSpace(proc);
  datasetobject := proc.Field('datasetobject').AsObject;
  result        := proc.Field('exitstatus').AsInt32;
  error         := proc.Field('errorstring').AsString;
end;

function TFRE_DB_ZFS.GetPools(out error: string; out pools: IFRE_DB_Object): integer;
var
  proc  : TFRE_DB_Process;
begin
  ClearProcess;
  proc := TFRE_DB_Process.create;
  proc.SetupInput('zpool',TFRE_DB_StringArray.Create ('list','-p','-o','name,guid'));
  AddProcess(proc);
  ExecuteMulti;
  AnalyzePoolList(proc);
  pools         := proc.Field('pools').AsObject.CloneToNewObject;
  result        := proc.Field('exitstatus').AsInt32;
  error         := proc.Field('errorstring').AsString;
end;

function TFRE_DB_ZFS.GetDiskInformation(out error: string; out disks: IFRE_DB_Object): integer;
var
  proc  : TFRE_DB_Process;
begin
  ClearProcess;
  proc := TFRE_DB_Process.create;
  proc.SetupInput('prtconf',TFRE_DB_StringArray.Create ('-v'));
  AddProcess(proc);
  ExecuteMulti;
  disks         := GFRE_DBI.NewObject;
  AnalyzePrtConfDisks(proc,disks);
  result        := proc.Field('exitstatus').AsInt32;
  error         := proc.Field('errorstring').AsString;
end;

function TFRE_DB_ZFS.CreateDiskPool(const pool_definition: IFRE_DB_Object; out error: string; out pool_result: IFRE_DB_Object): integer;
var zfs_cmd   : TFRE_DB_String;
    vdevcont  : IFRE_DB_Object;
    vdev      : IFRE_DB_Object;
    disk      : IFRE_DB_Object;
    proc      : TFRE_DB_Process;

    i         : NativeInt;
    j         : NativeInt;



    procedure _AddDisks(const vdev:IFRE_DB_Object);
    var i     : NativeInt;
    begin
      for i:= 0 to vdev.Field('vdev').ValueCount-1 do
        begin
          disk := vdev.Field('vdev').AsObjectItem[i];
          zfs_cmd := zfs_cmd+' '+disk.Field('name').asstring;
        end;
    end;

    procedure _AddDisk(const disk:IFRE_DB_Object);
    begin
      zfs_cmd := zfs_cmd+' '+disk.Field('name').asstring;
    end;

begin
  zfs_cmd  := 'xpool create '+pool_definition.Field('pool').asstring;
  for i    := 0 to pool_definition.Field('vdev').ValueCount-1 do
    begin
      vdevcont := pool_definition.Field('vdev').AsObjectItem[i];
      if vdevcont.SchemeClass='TFRE_DB_ZFS_DATASTORAGE' then
        begin
          for j:=0 to vdevcont.Field('vdev').ValueCount-1 do
            begin
              vdev := vdevcont.Field('vdev').AsObjectItem[j];
              if vdev.SchemeClass='TFRE_DB_ZFS_VDEV' then
                begin
                  case (vdev.Implementor_HC as TFRE_DB_ZFS_VDEV).GetRaidLevel of
                    zfs_rl_mirror: zfs_cmd:=zfs_cmd+' mirror';
                    zfs_rl_z1    : zfs_cmd:=zfs_cmd+' raidz1';
                    zfs_rl_z2    : zfs_cmd:=zfs_cmd+' raidz2';
                    zfs_rl_z3    : zfs_cmd:=zfs_cmd+' raidz3';
                  else
                    raise EFRE_DB_Exception.Create('unsupported raiadlevel in CreateDiskpool for datastorage'+CFRE_DB_ZFS_RAID_LEVEL[(vdev.Implementor_HC as TFRE_DB_ZFS_VDEV).GetRaidLevel]);
                  end;
                  _AddDisks(vdev);
                end else begin
                  _AddDisk(vdev);
                end;
            end;
        end;
      if vdevcont.SchemeClass='TFRE_DB_ZFS_LOG' then
        begin
          zfs_cmd:=zfs_cmd+' log';
          for j:=0 to vdevcont.Field('vdev').ValueCount-1 do
            begin
              vdev := vdevcont.Field('vdev').AsObjectItem[j];
              if vdev.SchemeClass='TFRE_DB_ZFS_VDEV' then
                begin
                  case (vdev.Implementor_HC as TFRE_DB_ZFS_VDEV).GetRaidLevel of
                    zfs_rl_mirror: zfs_cmd:=zfs_cmd+' mirror';
                  else
                    raise EFRE_DB_Exception.Create('unsupported raiadlevel in CreateDiskpool for log'+CFRE_DB_ZFS_RAID_LEVEL[(vdev.Implementor_HC as TFRE_DB_ZFS_VDEV).GetRaidLevel]);
                  end;
                  _AddDisks(vdev);
                end else begin
                  _AddDisk(vdev);
                end;
            end;
        end;
      if vdevcont.SchemeClass='TFRE_DB_ZFS_SPARE' then
        begin
          zfs_cmd:=zfs_cmd+' spare';
          for j:=0 to vdevcont.Field('vdev').ValueCount-1 do
            begin
              vdev := vdevcont.Field('vdev').AsObjectItem[j];
              _AddDisk(vdev);
            end;
        end;
      if vdevcont.SchemeClass='TFRE_DB_ZFS_CACHE' then
        begin
          zfs_cmd:=zfs_cmd+' cache';
          for j:=0 to vdevcont.Field('vdev').ValueCount-1 do
            begin
              vdev := vdevcont.Field('vdev').AsObjectItem[j];
              _AddDisk(vdev);
            end;
        end;
      writeln(vdev.SchemeClass);
    end;

  ClearProcess;
  proc := TFRE_DB_Process.create;
  proc.SetupInput(zfs_cmd,nil);
  AddProcess(proc);
  ExecuteMulti;
  pool_result := GFRE_DBI.NewObject;
  pool_result.Field('zfs_cmd').asstring := zfs_cmd;

  result := proc.Field('exitstatus').AsInt32;
  error  := proc.Field('errorstring').AsString;
end;

procedure TFRE_DB_ZFSJob.SetScrub(const poolname: string);
begin
  config.Field('cmd').AsString      :='scrub';
  config.Field('poolname').AsString :=poolname;
end;

procedure TFRE_DB_ZFSJob.SetPoolStatus(const poolname: string; const scrub_warning_days: integer; const scrub_error_days: integer);
begin
  config.Field('cmd').AsString      :='poolstatus';
  config.Field('poolname').AsString            := poolname;
  config.Field('scrub_warning_days').AsInt32   := scrub_warning_days;
  config.Field('scrub_error_days').AsInt32     := scrub_error_days;
end;

procedure TFRE_DB_ZFSJob.SetDatasetSpace(const dataset: string; const warning_percent: integer; const error_percent: integer);
begin
 config.Field('cmd').AsString             :='datasetspace';
 config.Field('dataset').AsString         := dataset;
 config.Field('warning_percent').AsByte   := warning_percent;
 config.Field('error_percent').AsByte     := error_percent;
end;

procedure TFRE_DB_ZFSJob.SetReplicate(const sourcedataset: string; const destinationdataset: string; const snapshotkey: string; const destinationhost: string; const destinationuser: string; const destinationkeyfilename: string; const replicationkeyfilename: string; const destinationport: integer; const replicationport: integer);
begin
  Field('MAX_ALLOWED_TIME').AsInt32 := 86400*14;
  config.Field('cmd').AsString                    :='replicate';
  config.Field('sourcedataset').AsString          :=sourcedataset;
  config.Field('destinationdataset').AsString     :=destinationdataset;
  config.Field('snapshotkey').AsString            :=snapshotkey;
  config.Field('destinationhost').AsString        :=destinationhost;
  config.Field('destinationuser').AsString        :=destinationuser;
  config.Field('destinationkeyfilename').AsString :=destinationkeyfilename;
  config.Field('replicationkeyfilename').AsString :=replicationkeyfilename;
  config.Field('destinationport').AsInt32         :=destinationport;
  config.Field('replicationport').AsInt32        :=replicationport;
end;

procedure TFRE_DB_ZFSJob.SetSnapshot(const dataset: string; const snapshotkey: string);
begin
 config.Field('cmd').AsString                    := 'snapshot';
 config.Field('sourcedataset').AsString          := dataset;
 config.Field('snapshotkey').AsString            := snapshotkey;
end;

procedure TFRE_DB_ZFSJob.SetSnapshotCheck(const dataset: string; const snapshotkey: string; const warning_seconds: integer; const error_seconds: integer);
begin
  config.Field('cmd').AsString                    := 'snapshotcheck';
  config.Field('dataset').AsString                := dataset;
  config.Field('snapshotkey').AsString            := snapshotkey;
  config.Field('warning_seconds').AsInt32         := warning_seconds;
  config.Field('error_seconds').AsInt32           := error_seconds;
end;

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZFSJob);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZFS_OBJ);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZFS_POOL);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZFS_UNASSIGNED);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZFS_DATASTORAGE);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZFS_VDEV);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZFS_BLOCKDEVICE);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZFS_DISKCONTAINER);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZFS_VDEVCONTAINER);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZFS_SPARE);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZFS_LOG);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZFS_CACHE);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZFS_PARSE_DATASET);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DISK_MPATH);
  GFRE_DBI.Initialize_Extension_Objects;
end;

function String2DBZFSRaidLevelType(const fts: string): TFRE_DB_ZFS_RAID_LEVEL;
begin
  for result in TFRE_DB_ZFS_RAID_LEVEL do begin
     if CFRE_DB_ZFS_RAID_LEVEL[result]=fts then exit;
  end;
  raise Exception.Create('invalid short DBZFSRaidLevel specifier : ['+fts+']');
end;


initialization
end.

