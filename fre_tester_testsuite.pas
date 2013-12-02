unit fre_tester_testsuite;

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
  Classes, SysUtils,fpcunit,testregistry,testdecorator,FRE_DB_INTERFACE,
  FOS_TOOL_INTERFACES,FRE_DBBASE,FRE_PROCESS, fre_testmethod, FRE_ZFS,
  fre_testcase, fre_do_safejob,FRE_SYSTEM,  FRE_DBMONITORING,
  FRE_OPENVPN,fre_alert,fre_scsi;

const

  cremoteuser               = 'root';
  cremotehost               = '10.220.251.10';
  cremotehosttester         = '10.220.252.130';
  cwinrmurl                 = 'https://10.4.0.234:5986/wsman';
  cwinrmuser                = 'winrmi';
  cwinrmpassword            = 'mgo54rb5A';

 // createtestpool.sh for pool preparation on cremotehost

 // zfs allow -d zfsback create,mount,quota,receive,snapshot zones/backup

// zfsback:x:100:100:zfsback:/zones/firmos/zfsback:/usr/bin/bash

type


  { TFRE_Tester_Tests }

  TFRE_Tester_Tests = class (TTestCase)
  private
    procedure _intCreateDS (dsname: string);
    procedure _intDestroyDS (dsname : string; const recursive:boolean; const dependents:boolean; const force :boolean);
  published
  public
    procedure CreateSafeJobs;
    procedure SeparatorTest;
    procedure SimplePing;
    procedure VMState;
    procedure InternetTest;
    procedure SafeJobInternet;
    procedure WinServerStatus;
    procedure SafeJobMailSBS;
    procedure FetchMails;
    procedure SMTPMethod;
    procedure SafeJobZpoolStatus;
    procedure SafeJobLiveScrub;
    procedure SafeJobCheckMailSBS;
    procedure WinRMMethod;
    procedure SafeJobWinRMSBS;
    procedure SMBTestMethod;
    procedure VPNTest;
    procedure SNMPTest;
    procedure SafeJobSMB;
    procedure ZPoolPrepare;
    procedure ZCreateSnapShot;
    procedure ZSnapShotExists;
    procedure ZGetLastSnapShot;
    procedure ZGetSnapShots;
    procedure ZGetLastSnapShotOnWrongDS;
    procedure ZDestroySnapShot;
    procedure ZSnapShotExistsFail;
    procedure ZDestroyDataSet;
    procedure ZCreateDataSet;
    procedure ZCreateDataSetHierarchy;
    procedure ZDestroyDataSetHierarchy;
    procedure ZCreateSnapshotJob;
    procedure ZReplicateDataSet;
    procedure ZReplicateDataSetDok;
    procedure ZReplicateDataSetKSMLinux;
    procedure ZSnapShotCheck;
    procedure ZSendSnapshot;
    procedure ZCheckDataSetExists;
 //   procedure ZPoolStatus;
    procedure SafeJobLiveZpoolStatus;
  public
    procedure MultiPing;
    procedure AlertConfig;
    procedure changeToggle;
    procedure ZPoolStatus;
    procedure CreateDiskshelf;
    procedure ZFSDiskspace;
    procedure ProcessCheck;
    procedure ProcessFreebsdCheck;
    procedure MountDiskspace;
    procedure HTTPRequest;
  published
    procedure CPULoad;

  end;



implementation

function GetRemoteKeyFilename: string;
begin
  result := SetDirSeparators(cFRE_SERVER_DEFAULT_DIR+'/ssl/user/id_rsa');         // must be authorized for remoteuser on remotehost
end;

function GetBackupKeyFilename: string;
begin
  result := SetDirSeparators(cFRE_SERVER_DEFAULT_DIR+'/ssl/user/zfsbackup_rsa');
end;


procedure TFRE_Tester_Tests.SeparatorTest;
begin
 AssertEquals('Wrong Decimal Separator','123.45',FloatToStr(123.45));
end;

procedure TFRE_Tester_Tests.SimplePing;
var ping    : TFRE_DB_Testmethod_Ping;
begin
  ping              := TFRE_DB_Testmethod_ping.Create;
  ping.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
  ping.Ping(TFRE_DB_StringArray.Create ('10.1.0.1','8.8.8.8','www.orf.at'),2);
end;

procedure TFRE_Tester_Tests.VMState;
var vm    : TFRE_DB_Testmethod_VirtualMachines;
begin
  vm             := TFRE_DB_Testmethod_VirtualMachines.Create;
  vm.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
  vm.GetMachineStates;
  writeln(vm.DumpToString());
end;

procedure TFRE_Tester_Tests.ZPoolStatus;
var po     : TFRE_DB_ZFS;
    error  : string;
    res    : integer;
    obj    : IFRE_DB_Object;
begin
  po     := TFRE_DB_ZFS.create;
  po.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
  res    := po.GetPoolStatus('zones',error,obj);
  writeln(obj.DumpToString());
end;

procedure TFRE_Tester_Tests.CreateDiskshelf;
var po     : TFRE_DB_DISKSHELF;
    ex2,ex3: TFRE_DB_SAS_EXPANDER;
    error  : string;
    res    : integer;
    obj    : IFRE_DB_Object;
    bay    : TFRE_DB_DISKBAY;
    disk   : TFRE_DB_DISK;
    i      : integer;
begin
  po     := TFRE_DB_DISKSHELF_SC836.create;
//  po.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
//  res    := po.GetPoolStatus('zones',error,obj);
  po.ChassisID:='5003048001abcc7f';
  ex2    := po.AddSASExpander(2,'5003048001abcc7d','LSI CORP','SAS2X28','0717','x36557230');
  ex3    := po.AddSASExpander(3,'5003048001abccfd','LSI CORP','SAS2X28','0717','x36557230');
  for i  := 0 to 15 do begin
    bay  := po.AddDiskBay(i);
    if i = 0 then begin
      bay.InsertSATADisk('5003048001abcc6c',122104,250069679,'ATA','Samsung SSD 840','4B0Q','S12PNEAD204359F','50025385501cb30c');
    end else if i=1 then begin
      bay.InsertSATADisk('5003048001abcc6d',244198,500118191,'ATA','Samsung SSD 840','4B0Q','S12RNEACC94641Y','500253855015d44d');
      ex2.AddSASTarget('5003048001abcc6d');
    end else if i>3 then begin
      bay.InsertSASDisk('5000c500562c2495','5000c500562c2496',953869,1953525167,'SEAGATE','ST1000NM0001','0002','Z1N3RMMG','5000c500562c2497');
      ex2.AddSASTarget('5000c500562c2495');
      ex3.AddSASTarget('5000c500562c2496');
    end;
  end;
  writeln(po.DumpToString());
end;

procedure TFRE_Tester_Tests.ZFSDiskspace;
var po     : TFRE_DB_ZFSJob;
    res    : integer;
    error  : string;
begin
  po     := TFRE_DB_ZFSJob.create;
  po.SetRemoteSSH('root', cremotehost, GetRemoteKeyFilename);
  po.SetDatasetspace('zones',80,90);
  po.ExecuteCMD;
  writeln(po.DumpToString());
end;

procedure TFRE_Tester_Tests.ProcessCheck;
var po     : TFRE_DB_ProcessTestcase;
begin
  po     := TFRE_DB_ProcessTestcase.create;
  po.SetRemoteSSH('root', '10.1.0.132', GetRemoteKeyFilename);
  po.AddProcessTest('smbd',1,1,'Samba Fileservice');
  po.AddProcessTest('afpd',1,1,'Apple Fileservice');
  po.AddProcessTest('netatalk',1,1,'Apple Netatalk');
  po.AddProcessTest('httpd',4,1,'Webserver');
  po.ExecuteTest;
  writeln(po.DumpToString);
end;

procedure TFRE_Tester_Tests.ProcessFreebsdCheck;
var po     : TFRE_DB_ProcessTestcase;
begin
  po     := TFRE_DB_ProcessTestcase.create;
  po.SetRemoteSSH('root', '10.1.0.1', GetRemoteKeyFilename);
  po.AddProcessTest('netatalk',1,1,'Apple Netatalk');
  po.AddProcessTest('openvpn',4,4,'OpenVPN Connections');
  po.ExecuteTest;
  writeln(po.DumpToString);
end;

procedure TFRE_Tester_Tests.MountDiskspace;
var po     : TFRE_DB_DiskspaceTestcase;
    res    : integer;
    error  : string;
begin
  po     := TFRE_DB_DiskspaceTestcase.create;
  po.SetRemoteSSH('root', '10.220.252.21', GetRemoteKeyFilename);
  po.SetMountpoint('/',50,60);
  po.ExecuteTest;
  writeln(po.DumpToString());
end;

procedure TFRE_Tester_Tests.HTTPRequest;
var po     : TFRE_DB_HTTPTestcase;
    res    : integer;
    error  : string;
begin
  po     := TFRE_DB_HTTPTestcase.create;
//  po.SetRemoteSSH('root', '10.1.0.116', GetRemoteKeyFilename);
  po.SetURL('http://10.220.252.147','Host: www.firmos.at','FirmOS Business Solutions');
//  po.SetURL('http://10.220.252.147','Host: www.ksm.at','KSM');
//  po.SetURL('http://10.220.252.81','','forumtitle');
//  po.SetURL('http://fosbuild.firmos.at','','Jenkins');
  po.ExecuteTest;
  writeln(po.DumpToString());
end;

procedure TFRE_Tester_Tests.CPULoad;
var po     : TFRE_DB_CPULoadTestcase;
    res    : integer;
    error  : string;
begin
  po     := TFRE_DB_CPULoadTestcase.create;
  po.SetRemoteSSH('root', '10.220.251.10', GetRemoteKeyFilename);
  po.SetLimits(10,12);
  po.ExecuteTest;
  writeln(po.DumpToString());
end;

procedure TFRE_Tester_Tests.ZCreateSnapShot;
var po     : TFRE_DB_ZFS;
    dt     : TFRE_DB_DateTime64;
    res    : integer;
    error  : string;

begin
    po     := TFRE_DB_ZFS.Create;
    po.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
    dt:=GFRE_DT.Now_UTC;
    res    := po.CreateSnapShot('testpool/ds1', 'TEST', error);
//    writeln(po.DumpToString());
    AssertEquals('Result not 0',0,res);
    AssertEquals('Error not empty','',error);
end;

procedure TFRE_Tester_Tests.ZGetSnapShots;
var po     : TFRE_DB_ZFS;
begin
  po     := TFRE_DB_ZFS.create;
  po.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
  po.GetSnapshots('testpool/ds1',false);
  writeln(po.DumpToString());
end;

procedure TFRE_Tester_Tests.ZDestroySnapShot;
var po     : TFRE_DB_ZFS;
    dt     : TFRE_DB_DateTime64;
    res    : integer;
    error  : string;

begin
  po     := TFRE_DB_ZFS.Create;
  po.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
  dt:=GFRE_DT.Now_UTC;
  res    := po.DestroySnapShot('testpool/ds1', 'TEST', error);
//  writeln(po.DumpToString());
  AssertEquals('Result not 0',0,res);
  AssertEquals('Error not empty','',error);
end;

procedure TFRE_Tester_Tests.ZSnapShotExists;
var po     : TFRE_DB_ZFS;
    dt     : TFRE_DB_DateTime64;
    res    : integer;
    error  : string;
    exists : boolean;
    cts    : TFRE_DB_DateTime64;

begin
  po     := TFRE_DB_ZFS.create;
  po.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
  dt:=GFRE_DT.Now_UTC;
  res    := po.SnapShotExists('testpool/ds1', 'TEST', cts, error, exists);
//  writeln(po.DumpToString());
  AssertEquals('Result not 0',0,res);
  AssertEquals('Error not empty','',error);
  AssertEquals('Exists',true,exists);
end;

procedure TFRE_Tester_Tests.ZGetLastSnapShot;
var po           : TFRE_DB_ZFS;
    dt           : TFRE_DB_DateTime64;
    res          : integer;
    error        : string;
    snapshotname : string;
    creation_ts  : TFRE_DB_DateTime64;
begin
  po     := TFRE_DB_ZFS.create;
  po.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
  res    := po.GetLastSnapShot('testpool/ds1', 'TEST',snapshotname, creation_ts, error);
  writeln('Last Snapshot :',snapshotname);
  writeln('res:',res);
  AssertEquals('Result is 0',0,res);
  AssertEquals('Last Snapshot ist wrong!',snapshotname,'TEST');
end;

procedure TFRE_Tester_Tests.ZGetLastSnapShotOnWrongDS;
var po           : TFRE_DB_ZFS;
    dt           : TFRE_DB_DateTime64;
    res          : integer;
    error        : string;
    snapshotname : string;
    creation_ts  : TFRE_DB_DateTime64;
begin
  po     := TFRE_DB_ZFS.create;
  po.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
  dt:=GFRE_DT.Now_UTC;
  res    := po.GetLastSnapShot('testpool/dsfail', '',snapshotname, creation_ts,error);
  writeln('Last Snapshot :',snapshotname);
  writeln('res:',res);
  AssertTrue('Result is not 0',(res<>0));
  AssertTrue('Error not empty',(length(error)>0));
end;

procedure TFRE_Tester_Tests.ZCreateDataSet;
begin
  _intCreateDS('testpool/dscreate');
end;

procedure TFRE_Tester_Tests.ZCreateDataSetHierarchy;
begin
 _intCreateDS('testpool/dshier1');
 _intCreateDS('testpool/dshier1/dshier1.1');
end;

procedure TFRE_Tester_Tests.ZDestroyDataSetHierarchy;
begin
  _intDestroyDS('testpool/dshier1',true,false,false);
end;

procedure TFRE_Tester_Tests.ZDestroyDataSet;
begin
  _intDestroyDS('testpool/dscreate',false,false,false);
end;

procedure TFRE_Tester_Tests.ZSnapShotExistsFail;
var po     : TFRE_DB_ZFS;
    dt     : TFRE_DB_DateTime64;
    res    : integer;
    error  : string;
    exists : boolean;
    cts    : TFRE_DB_DateTime64;
begin
  po     := TFRE_DB_ZFS.create;
  po.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
  res    := po.SnapShotExists('testpool/ds2', 'TEST', cts, error, exists);
//    writeln(po.DumpToString());
  Assert(res<>0,'Result is 0');
  AssertEquals('Error not empty','',error);
  AssertEquals('Exists',false,exists);
end;

procedure TFRE_Tester_Tests._intCreateDS(dsname: string);
var po     : TFRE_DB_ZFS;
    res    : integer;
    error  : string;
begin
  po     := TFRE_DB_ZFS.create;
  po.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
  res    := po.CreateDataset(dsname,error);
  writeln(po.DumpToString());
  AssertEquals('Result not 0',0,res);
  AssertEquals('Error not empty','',error);
end;

procedure TFRE_Tester_Tests._intDestroyDS(dsname: string; const recursive: boolean; const dependents: boolean; const force: boolean);
var po     : TFRE_DB_ZFS;
    res    : integer;
    error  : string;
begin
  po     := TFRE_DB_ZFS.create;
  po.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
  res    := po.DestroyDataset(dsname,error,recursive,dependents,force);
  //  writeln(po.DumpToString());
  AssertEquals('Result not 0, '+error,0,res);
  AssertEquals('Error not empty','',error);
end;

procedure TFRE_Tester_Tests.ZSendSnapshot;
var po     : TFRE_DB_ZFS;
    dt     : TFRE_DB_DateTime64;
    res    : integer;
    error  : string;
begin
  po     := TFRE_DB_ZFS.create;
  po.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);

  dt:=GFRE_DT.Now_UTC;
  res    := po.SendSnapshot('testpool/ds1', 'TEST', '10.1.0.130','root', 'zones/testpool/ds1', '/zones/rootkey/id_rsa',error,'',zfsSend);
  writeln(po.DumpToString());
  AssertEquals('Result not 0,'+error,0,res);
  AssertEquals('Error not empty','',error);
end;


procedure TFRE_Tester_Tests.ZReplicateDataSet;
var po     : TFRE_DB_ZFSJob;
    res    : integer;
    error  : string;
begin
  po     := TFRE_DB_ZFSJob.create;
  po.SetRemoteSSH(cremoteuser, '10.1.0.116', GetRemoteKeyFilename);
  po.SetReplicate('zones/FirmOS_Data/FirmOS_Library','zones/backup/firmos/FirmOSData/FirmOS_Library','AUTO','smartstore.firmos.at','zfsback',GetBackupKeyFilename,'/zones/firmos/zfsback/.ssh/id_rsa',22,22);
  po.ExecuteCMD;
  writeln(po.DumpToString());
end;


procedure TFRE_Tester_Tests.ZReplicateDataSetDok;
var po     : TFRE_DB_ZFSJob;
    res    : integer;
    error  : string;
begin
  po     := TFRE_DB_ZFSJob.create;
  po.SetRemoteSSH(cremoteuser, '10.1.0.116', GetRemoteKeyFilename);
  po.SetReplicate('zones/FirmOS_Data/FirmOS_Dokumente','zones/backup/firmos/FirmOSData/FirmOS_Dokumente','AUTO','smartstore.firmos.at','zfsback',GetBackupKeyFilename,'/zones/firmos/zfsback/.ssh/id_rsa',22,22);
  po.ExecuteCMD;
  writeln(po.DumpToString());
end;

procedure TFRE_Tester_Tests.ZReplicateDataSetKSMLinux;
var po     : TFRE_DB_ZFSJob;
    res    : integer;
    error  : string;
begin
  po     := TFRE_DB_ZFSJob.create;
  po.SetRemoteSSH(cremoteuser, '10.1.0.129', GetRemoteKeyFilename);
  po.SetReplicate('zones/2cd2e934-f21e-4fea-b46e-f3098f4e3be3-disk0','zones/backup/ksm/2cd2e934-f21e-4fea-b46e-f3098f4e3be3-disk0','AUTO','smartstore.firmos.at','zfsback',GetBackupKeyFilename,'/zones/firmos/zfsback/.ssh/id_rsa',22,22);
  po.ExecuteCMD;
  writeln(po.DumpToString());
end;

procedure TFRE_Tester_Tests.MultiPing;
var po     : TFRE_DB_MultiPingTestcase;
begin
  po     := TFRE_DB_MultiPingTestcase.create;
  po.SetRemoteSSH(cremoteuser, '10.1.0.24', GetRemoteKeyFilename);
  po.setRTTTimeout_ms(10);
  po.setpingcount(5);
  po.addPingTest('10.1.0.1','');
  po.AddPingTest('10.1.0.116','');
  po.addPingTest('10.1.0.108','');
//  po.addPingTest('10.200.1.11');
//  po.AddPingTest('10.200.3.11');

  po.ExecuteTest;
  writeln(po.DumpToString);
end;

procedure TFRE_Tester_Tests.AlertConfig;
var alert: TFRE_Alert;
begin
  alert := TFRE_Alert.Create;
  try
    alert.SetSMTPHost('mail.firmos.at','25','mailer','45mfj345l2094pc1');
    alert.LoadAlertStatus;
    alert.SetAlertModeCommon(almo_EveryError);
    alert.SetAlertModeForKeys(almo_NoReaction,TFRE_DB_StringArray.Create('NOREACTION1','NOREACTION2'));
    alert.AddAlertingEMailCommon(TFRE_DB_StringArray.Create('franz.schober@firmos.at','EMAIL2','noc@firmos.at'));
    alert.AddAlertingEMailForKeys(TFRE_DB_StringArray.Create('EMAIL4','EMAIL5','EMAIL6'),TFRE_DB_StringArray.Create('KEY1'));
    alert.RemoveAlertingEMailCommon(TFRE_DB_StringArray.Create('EMAIL2'));
    alert.SetAlertTypeCommon(alty_Mail);
    alert.AddAlertTypeCommon(alty_Mail);
    alert.AddAlertTypeCommon(alty_SMS);
    alert.AddAlertTypeForKeys(alty_SNMPTrap,TFRE_DB_StringArray.Create('NOREACTION1','NOREACTION2'));
    alert.SaveAlertConfig;
    alert.ClearAlertTypeForKeys(TFRE_DB_StringArray.Create('NOREACTION2'));
    alert.ClearAlertModeForKeys(TFRE_DB_StringArray.Create('NOREACTION2'));
    alert.AddAlertKeys(TFRE_DB_StringArray.Create('EMPTYKEY1','EMPTYKEY2'));
    alert.RemoveAlertKeys(TFRE_DB_StringArray.Create('EMPTYKEY2'));
    alert.SaveAlertConfig;
    alert.UpdateStatus('NOREACTION1',statusFailure,'TEST FAILURE');
    alert.UpdateStatus('NOREACTION2',statusFailure,'TEST FAILURE');
    alert.SetAlertModeCommon(almo_Change);
    alert.UpdateStatus('ANY1',statusFailure,'TEST FAILURE');
    alert.UpdateStatus('ANY1',statusFailure,'TEST FAILURE');
    alert.UpdateStatus('ANY1',statusOK,'TEST OK');
    alert.UpdateStatus('ANY1',statusFailure,'TEST FAILURE');
    alert.UpdateStatus('ANY1',statusFailure,'TEST FAILURE');
//    alert.SendAlerts;
    alert.ClearAlerts;
    alert.CheckAlerting(GFRE_DBI.NewObject);
    sleep(2000);
    alert.UpdateStatus('EMPTYKEY1',statusOK,'Emptykey1 Detail');
    alert.UpdateStatus('KEY1',statusFailure,'Key1 Detail');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    alert.SetAlertModeForKeys(almo_ChangeTimeout,TFRE_DB_StringArray.Create('KEY1'));
    alert.SetChangeTimeoutCommon(1);
    sleep(2000);
    alert.UpdateStatus('EMPTYKEY1',statusFailure,'Emptykey1 Detail');
    alert.UpdateStatus('KEY1',statusOK,'Key1 Detail');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    writeln('Begin Change1');
    alert.UpdateStatus('CHANGE1',statusOK,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    alert.UpdateStatus('CHANGE1',statusFailure,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    alert.UpdateStatus('CHANGE1',statusOK,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    alert.UpdateStatus('CHANGE1',statusFailure,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    writeln('wait');
    sleep(2000);
    alert.UpdateStatus('CHANGE1',statusOK,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
  finally
    alert.Free;
  end;

end;

procedure TFRE_Tester_Tests.changeToggle;
var alert: TFRE_Alert;
begin
  alert := TFRE_Alert.Create;
  try
    alert.ClearStatus;
    alert.ClearConfig;
    alert.SetSMTPHost('mail.firmos.at','25','mailer','45mfj345l2094pc1');
    alert.SetMailFrom('alert@firmos.at');
    alert.SetMailSubject('Alert from Cityaccess Accesspoint Controller');
    alert.AddAlertingEMailCommon(TFRE_DB_StringArray.Create('franz.schober@firmos.at'));
    alert.AddAlertTypeCommon(alty_Mail);
    alert.SetAlertModeCommon(almo_ChangeTimeout);
    alert.SetChangeTimeoutCommon(1);
    writeln('Begin Change1');
    alert.UpdateStatus('CHANGE1',statusOK,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    alert.UpdateStatus('CHANGE1',statusFailure,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    alert.UpdateStatus('CHANGE1',statusOK,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    alert.UpdateStatus('CHANGE1',statusFailure,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    writeln('wait');
    sleep(2000);
    alert.UpdateStatus('CHANGE1',statusOK,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    writeln('Begin Change2 -------------------');
    alert.UpdateStatus('CHANGE2',statusFailure,'This is the alert Text for this key!');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    alert.UpdateStatus('CHANGE2',statusOK,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    alert.UpdateStatus('CHANGE2',statusFailure,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    writeln('wait');
    sleep(2000);
    alert.UpdateStatus('CHANGE2',statusOK,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    writeln('wait');
    sleep(2000);
    alert.UpdateStatus('CHANGE2',statusOK,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    writeln('wait');
    sleep(2000);
    alert.UpdateStatus('CHANGE2',statusOK,'');
    alert.CheckAlerting(GFRE_DBI.NewObject);
    alert.SendAlerts;
  finally
    alert.Free;
  end;
end;

procedure TFRE_Tester_Tests.ZSnapShotCheck;
var po     : TFRE_DB_ZFSJob;
    res    : integer;
    error  : string;
begin
  po     := TFRE_DB_ZFSJob.create;
  po.SetRemoteSSH('root', 'smartstore.firmos.at', GetRemoteKeyFilename);
  po.SetSnapshotCheck('zones/backup/firmos/FirmOSData/FirmOS_Dokumente','AUTO',300,1000);
  po.ExecuteCMD;
  writeln(po.DumpToString());
end;

procedure TFRE_Tester_Tests.ZCreateSnapshotJob;
var po     : TFRE_DB_ZFSJob;
    res    : integer;
    error  : string;
begin
  po     := TFRE_DB_ZFSJob.create;
  po.SetRemoteSSH('root', cremotehost, GetRemoteKeyFilename);
  po.SetSnapshot('testpool/ds1','NEXT');
  po.ExecuteCMD;
  writeln(po.DumpToString());
end;


procedure TFRE_Tester_Tests.InternetTest;
var po     : TFRE_DB_InternetTestcase;
begin
  po     := TFRE_DB_InternetTestcase.create;
  po.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
  po.PrepareTest('10.1.0.1','91.114.28.41',TFRE_DB_StringArray.Create ('8.8.8.8','www.orf.at','www.google.com'));
  po.ExecuteTest;
  writeln(po.DumpToString);
  po.SaveToFile('internet.dbo',false);
end;

procedure TFRE_Tester_Tests.CreateSafeJobs;
var job    : TFRE_DB_InternetTestcase;
    config : IFRE_DB_Object;
    jobdbo : IFRE_DB_Object;
    scpjob : TFRE_DB_SCP_JOB;
    wm     : TFRE_DB_WinRMTestcase;
    wmt    : TFRE_DB_WinRMTarget;
    sm     : TFRE_DB_MailSendTestcase;
    fm     : TFRE_DB_MailCheckTestcase;
    z      : TFRE_DB_ZFSJob;
    smb    : TFRE_DB_SMBTestcase;
begin
  job      := TFRE_DB_InternetTestcase.Create;
  job.SetJobkeyDescription('INTERNETTESTCASE','Desc Internet');
  job.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
  job.PrepareTest('10.1.0.1','91.114.28.41',TFRE_DB_StringArray.Create ('8.8.8.8','www.orf.at','www.google.com'));

  jobdbo   := GFRE_DBI.NewObject;
  jobdbo.Field(job.JobKey).asobject := job;
//  DO_SaveJob(dbi);

  scpjob   := TFRE_DB_SCP_JOB.create;
  jobdbo.Field('SCPMONITORING').asobject := scpjob;

  wm       := TFRE_DB_WinRMTestcase.create;
  wm.Field('MAX_ALLOWED_TIME').AsInt32 := 60;
  wm.SetJobkeyDescription('WINRM_KSM','Desc RM KSM');
  wm.SetRemoteSSH(cremoteuser, cremotehosttester, GetRemoteKeyFilename);
  wmt      := wm.AddWinRMTarget(cwinrmurl, cwinrmuser, cwinrmpassword);
  wmt.AddTestDiskSpace('E422D5D9',40,90);
  wmt.AddTestDiskSpace('xxx',50,70);
  wmt.AddTestServiceRunning('MSExchangeServiceHost');
  wmt.AddTestServiceRunning('DHCPServer');
  wmt.AddTestServiceRunning('defragsvc');
  wmt.AddTestServiceRunning('Sepplservice');
  wmt.SetTestCPUUsage(90);
  jobdbo.Field(wm.jobkey).asobject := wm;

  sm       := TFRE_DB_MailSendTestcase.create;
  sm.SetJobkeyDescription('MAILSEND_KSM','Desc Mailsend KSM');
  sm.SetRemoteSSH(cremoteuser, cremotehosttester, GetRemoteKeyFilename);
  sm.AddMailserver('10.4.0.234',cwinrmuser,cwinrmpassword,'tester@ksm.at','monsys@monsys.firmos.at');
  jobdbo.Field(sm.jobkey).asobject := sm;

  fm       := TFRE_DB_MailCheckTestcase.create;
  fm.SetJobkeyDescription('MAILCHECK_KSM','Desc Mailcheck KSM');
  fm.Field('MAX_ALLOWED_TIME').AsInt32 := 60;
  fm.SetRemoteSSH(cremoteuser, cremotehosttester, GetRemoteKeyFilename);
  fm.SetMailserver('10.4.0.234','MAILSEND_KSM',300,600);
  jobdbo.Field(fm.jobkey).asobject := fm;

  z       := TFRE_DB_ZFSJob.create;
  z.SetJobkeyDescription('ZPOOL_TESTPOOL','Desc Zpool Testpool');
  z.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
  z.SetPoolStatus('testpool',7,14);
  jobdbo.Field(z.jobkey).asobject := z;

  z       := TFRE_DB_ZFSJob.create;
  z.SetJobkeyDescription('ZPOOL_LIVEPOOL','Desc Zpool Livepool');
  z.SetRemoteSSH(cremoteuser,'10.220.251.10', GetRemoteKeyFilename);
  z.SetPoolStatus('zones',7,14);
  jobdbo.Field(z.jobkey).asobject := z;

  z        := TFRE_DB_ZFSJob.Create;
  z.SetJobkeyDescription('ZPOOL_SCRUB_LIVEPOOL', 'Desc Scrub Livepool');
  z.SetRemoteSSH(cremoteuser, '10.1.0.116', GetRemoteKeyFilename);
  z.SetScrub('zones');
  jobdbo.Field(z.jobkey).asobject := z;

  smb        := TFRE_DB_SMBTestcase.Create;
  smb.SetJobkeyDescription('SMB_MOUNT', 'Desc SMB MOUNT');
  smb.SetRemoteSSH(cremoteuser, cremotehosttester, GetRemoteKeyFilename);
  smb.SetFileshare('10.1.0.132','schramml','o1n1e1','Library','/usr/local/testsmb');
  jobdbo.Field(smb.jobkey).asobject := smb;


  jobdbo.SaveToFile(cFRE_HAL_CFG_DIR+'jobs.dbo');
//  writeln(jobdbo.DumpToString);
  writeln('Filename :',cFRE_HAL_CFG_DIR+'jobs.dbo');
  jobdbo := nil;
end;

procedure TFRE_Tester_Tests.SafeJobZpoolStatus;
begin
  DO_SaveJob('ZPOOL_TESTPOOL',cFRE_HAL_CFG_DIR+'jobs.dbo');
end;

procedure TFRE_Tester_Tests.SafeJobLiveZpoolStatus;
begin
  DO_SaveJob('ZPOOL_LIVEPOOL',cFRE_HAL_CFG_DIR+'jobs.dbo');
end;

procedure TFRE_Tester_Tests.SMBTestMethod;
var
  po      : TFRE_DB_Testmethod_SMB;
  res     : integer;
  error   : System.Ansistring;
begin
 po     := TFRE_DB_Testmethod_SMB.create;
 po.SetRemoteSSH(cremoteuser, cremotehosttester, GetRemoteKeyFilename);
 res := po.MountSMBandTest('10.1.0.132','schramml','o1n1e1','Library','/usr/local/testsmb',error);
 writeln(po.DumpToString());
 Assert(res=0,'Resultcode not 0:'+inttostr(res));
 AssertEquals('Error not empty','',error);
end;

procedure TFRE_Tester_Tests.SafeJobSMB;
begin
  DO_SaveJob('SMB_MOUNT',cFRE_HAL_CFG_DIR+'jobs.dbo');
end;

procedure TFRE_Tester_Tests.ZPoolPrepare;
begin
  try
   writeln('ZPool Prepare Destroy ------');
   _intDestroyDS('testpool/ds1',true,true,true);
  except
  end;
  try
   _intDestroyDS('testpool/ds2',true,true,true);
  except
  end;
  writeln('ZPool Prepare Create ------');
  _intCreateDS('testpool/ds1');
  _intCreateDS('testpool/ds2');
end;

procedure TFRE_Tester_Tests.VPNTest;
var
  po     : TFRE_DB_OPENVPN;
  res    : integer;
  error  : string;
  pid    : integer;
  cmdobject : IFRE_DB_Object;

  //client
  //dev tun
  //proto tcp
  //remote portal.cityaccess.at 1194
  //resolv-retry infinite
  //nobind
  //
  //# Downgrade privileges after initialization (non-Windows only)
  //;user nobody
  //;group nobody
  //persist-key
  //persist-tun
  //log-append /var/log/openvpn.log
  //ca /opt/local/etc/openvpn/service_cityaccess/cacert.pem
  //cert /opt/local/etc/openvpn/service_cityaccess/FirmOSMonitoring1.crt
  //key /opt/local/etc/openvpn/service_cityaccess/FirmOSMonitoring1.key
  //comp-lzo
  //verb 3

begin
  cmdobject := GFRE_DBI.NewObject;
  cmdobject.Field('dev').asstring           := 'tun';
  cmdobject.Field('proto').asstring         := 'tcp';
  cmdobject.Field('remote').asstring        := 'portal.cityaccess.at 1194';
  cmdobject.Field('resolv-retry').asstring  := 'infinte';
  cmdobject.Field('nobind').asboolean       := true;
  cmdobject.Field('persist-key').asboolean  := true;
  cmdobject.Field('persist-tun').asboolean  := true;
  cmdobject.Field('log-append').asstring    := '/var/log/openvpn.log';
  cmdobject.Field('ca').asstring            := '/opt/local/etc/openvpn/service_cityaccess/cacert.pem';
  cmdobject.Field('cert').asstring          := '/opt/local/etc/openvpn/service_cityaccess/FirmOSMonitoring1.crt';
  cmdobject.Field('key').asstring           := '/opt/local/etc/openvpn/service_cityaccess/FirmOSMonitoring1.key';
  cmdobject.Field('comp-lzo').asboolean     := true;
  cmdobject.Field('verb').asstring          := '3';

  po     := TFRE_DB_OPENVPN.Create;
  po.SetRemoteSSH(cremoteuser, cremotehosttester, GetRemoteKeyFilename);
  res    := po.Connect(cmdobject,error,pid);
  writeln(po.DumpToString());
  AssertEquals('Result not 0',0,res);
  AssertEquals('Error not empty','',error);
  sleep(10000);
  res    := po.Disconnect(error,pid);
  AssertEquals('Result not 0',0,res);
  AssertEquals('Error not empty','',error);
  writeln(po.DumpToString());
end;

procedure TFRE_Tester_Tests.SNMPTest;
var po      : TFRE_DB_Testmethod_SNMP;
begin
  po      := TFRE_DB_Testmethod_SNMP.Create;
  po.SetRemoteSSH(cremoteuser, cremotehosttester, GetRemoteKeyFilename);
  po.AddSNMPRequest('HOST-RESOURCES-MIB::hrProcessorLoad.2','10.4.0.234',1,'public');
  po.AddSNMPRequest('HOST-RESOURCES-MIB::hrStorageSize.1','10.4.0.234',1,'public');
  po.SendRequests;
  writeln(po.DumpToString);
end;

procedure TFRE_Tester_Tests.SafeJobLiveScrub;
begin
  DO_SaveJob('ZPOOL_SCRUB_LIVEPOOL',cFRE_HAL_CFG_DIR+'jobs.dbo');
end;


procedure TFRE_Tester_Tests.SafeJobCheckMailSBS;
begin
  DO_SaveJob('MAILCHECK_KSM',cFRE_HAL_CFG_DIR+'jobs.dbo');
end;

procedure TFRE_Tester_Tests.FetchMails;
var po   : TFRE_DB_Testmethod_FetchMailDir;
begin
  po     := TFRE_DB_Testmethod_FetchMailDir.create;
  po.SetRemoteSSH(cremoteuser, cremotehosttester, GetRemoteKeyFilename);
  po.FetchMailDirectory ('10.4.0.234','SMTPMETHODTEST');
end;

procedure TFRE_Tester_Tests.SafeJobMailSBS;
begin
  DO_SaveJob('MAILSEND_KSM',cFRE_HAL_CFG_DIR+'jobs.dbo');
end;

procedure TFRE_Tester_Tests.SafeJobWinRMSBS;
begin
  DO_SaveJob('WINRM_KSM',cFRE_HAL_CFG_DIR+'jobs.dbo');
end;

procedure TFRE_Tester_Tests.ZCheckDataSetExists;
var po     : TFRE_DB_ZFS;
    dt     : TFRE_DB_DateTime64;
    res    : integer;
    error  : string;
    exists : boolean;

begin
    po     := TFRE_DB_ZFS.Create;
    po.SetRemoteSSH(cremoteuser, cremotehost, GetRemoteKeyFilename);
    dt:=GFRE_DT.Now_UTC;
    res    := po.DataSetExists('testpool/ds1', error, exists);
    writeln(po.DumpToString());
    AssertEquals('Result not 0',0,res);
    AssertEquals('Error not empty','',error);
    AssertEquals('Exists',true,exists);
end;

procedure TFRE_Tester_Tests.SMTPMethod;
var po     : TFRE_DB_Testmethod_SMTPSend;
    res    : integer;
    body   : IFRE_DB_Object;
    btext  : string;
    subject: string;
    jobkey : string;
    host   : string;
begin
  writeln ('------- REMOTE SMTP--------');

  jobkey := 'MAILSEND_KSM';
  host   := '10.4.0.234';


  subject:= GFRE_BT.HashString_MD5_HEX(uppercase(host)+' '+uppercase(jobkey))+' '+GFRE_DT.ToStrFOS(GFRE_DT.Now_UTC);
  writeln(subject);

  po     := TFRE_DB_Testmethod_SMTPSend.create;
  po.SetRemoteSSH(cremoteuser, cremotehosttester, GetRemoteKeyFilename);
  po.AddSMTPSend(host,cwinrmuser,cwinrmpassword,'tester@ksm.at','monsys@monsys.firmos.at',subject,btext);
  po.Send;
  writeln(po.DumpToString());
  res := po.GetProcess(0).ExitStatus;
  Assert(res=0,'SMTP Resultcode not 0:'+inttostr(res));
end;

procedure TFRE_Tester_Tests.SafeJobInternet;
begin
  DO_SaveJob('INTERNETTESTCASE',cFRE_HAL_CFG_DIR+'jobs.dbo');
end;

//procedure TFRE_Tester_Tests.SafeJobSCP;
//begin
//  DO_SaveJob('SCPMONITORING',cFRE_HAL_CFG_DIR+'jobs.dbo');
//end;

procedure TFRE_Tester_Tests.WinRMMethod;
var po     : TFRE_DB_Testmethod_WinRM;
begin
  writeln ('------- REMOTE WINRM--------');
  po     := TFRE_DB_Testmethod_WinRM.create;
  po.SetRemoteSSH(cremoteuser, cremotehosttester, GetRemoteKeyFilename);
  po.AddWinRMTarget(cwinrmurl, cwinrmuser, cwinrmpassword);
  po.GetWinRM;
//  writeln(po.DumpToString());
end;

procedure TFRE_Tester_Tests.WinServerStatus;
var po     : TFRE_DB_WinRMTestcase;
    pt     : TFRE_DB_WinRMTarget;
begin
  po     := TFRE_DB_WinRMTestcase.create;
  po.SetJobkeyDescription('WINRM_KSM','Desc WindRM_KSM');
  po.SetRemoteSSH(cremoteuser, cremotehosttester, GetRemoteKeyFilename);
  pt     := po.AddWinRMTarget(cwinrmurl, cwinrmuser, cwinrmpassword);
  pt.AddTestDiskSpace('E422D5D9',40,90);
  pt.AddTestDiskSpace('xxx',50,70);
  pt.AddTestServiceRunning('MSExchangeServiceHost');
  pt.AddTestServiceRunning('DHCPServer');
  pt.AddTestServiceRunning('defragsvc');
  pt.AddTestServiceRunning('Sepplservice');
  pt.SetTestCPUUsage(90);

  po.ExecuteTest;
//  writeln(po.DumpToString);
//  po.SaveToFile('internet.dbo',false);

end;




initialization
  RegisterTest(TFRE_Tester_Tests);
end.

