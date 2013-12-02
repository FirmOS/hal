unit fre_scsi;

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
  FOS_TOOL_INTERFACES,fre_zfs;


// ./sas2ircu 0 DISPLAY

//Device is a Hard disk
//  Enclosure #                             : 2
//  Slot #                                  : 0
//  SAS Address                             : 5003048-0-01ab-cc6c
//  State                                   : Ready (RDY)
//  Size (in MB)/(in sectors)               : 122104/250069679
//  Manufacturer                            : ATA
//  Model Number                            : Samsung SSD 840
//  Firmware Revision                       : 4B0Q
//  Serial No                               : S12PNEAD204359F
//  GUID                                    : 50025385501cb30c
//  Protocol                                : SATA
//  Drive Type                              : SATA_SSD
//
//Device is a Enclosure services device
//  Enclosure #                             : 2
//  Slot #                                  : 0
//  SAS Address                             : 5003048-0-01ab-cc7d
//  State                                   : Standby (SBY)
//  Manufacturer                            : LSI CORP
//  Model Number                            : SAS2X28
//  Firmware Revision                       : 0717
//  Serial No                               : x36557230
//  GUID                                    : N/A
//  Protocol                                : SAS
//  Device Type                             : Enclosure services device
//
//Device is a Hard disk
//  Enclosure #                             : 2
//  Slot #                                  : 1
//  SAS Address                             : 5003048-0-01ab-cc6d
//  State                                   : Ready (RDY)
//  Size (in MB)/(in sectors)               : 244198/500118191
//  Manufacturer                            : ATA
//  Model Number                            : Samsung SSD 840
//  Firmware Revision                       : 4B0Q
//  Serial No                               : S12RNEACC94641Y
//  GUID                                    : 500253855015d44d
//  Protocol                                : SATA
//  Drive Type                              : SATA_SSD
//
//Device is a Hard disk
//  Enclosure #                             : 2
//  Slot #                                  : 4
//  SAS Address                             : 5000c50-0-562c-2495
//  State                                   : Ready (RDY)
//  Size (in MB)/(in sectors)               : 953869/1953525167
//  Manufacturer                            : SEAGATE
//  Model Number                            : ST1000NM0001
//  Firmware Revision                       : 0002
//  Serial No                               : Z1N3RMMG
//  GUID                                    : 5000c500562c2497
//  Protocol                                : SAS
//  Drive Type                              : SAS_HDD

//fmtopo -V
//hc://:product-id=LSI-CORP-SAS2X28:server-id=:chassis-id=5003048001abcc7f:serial=Z1N3RMMG0000C3310LYW:part=SEAGATE-ST1000NM0001:revision=0002/ses-enclosure=0/bay=4/disk=0
//  group: protocol                       version: 1   stability: Private/Private
//    resource          fmri      hc://:product-id=LSI-CORP-SAS2X28:server-id=:chassis-id=5003048001abcc7f:serial=Z1N3RMMG0000C3310LYW:part=SEAGATE-ST1000NM0001:revision=0002/ses-enclosure=0/bay=4/disk=0
//    label             string    Slot 05
//    FRU               fmri      hc://:product-id=LSI-CORP-SAS2X28:server-id=:chassis-id=5003048001abcc7f:serial=Z1N3RMMG0000C3310LYW:part=SEAGATE-ST1000NM0001:revision=0002/ses-enclosure=0/bay=4/disk=0
//    ASRU              fmri      dev:///:devid=id1,sd@n5000c500562c2497//scsi_vhci/disk@g5000c500562c2497
//  group: authority                      version: 1   stability: Private/Private
//    product-id        string    LSI-CORP-SAS2X28
//    chassis-id        string    5003048001abcc7f
//    server-id         string
//  group: storage                        version: 1   stability: Private/Private
//    logical-disk      string    c0t5000C500562C2497d0
//    manufacturer      string    SEAGATE
//    model             string    ST1000NM0001
//    serial-number     string    Z1N3RMMG0000C3310LYW
//    firmware-revision string    0002
//    capacity-in-bytes string    1000204886016
//    target-port-l0ids string[]  [ "w5000c500562c2495" "w5000c500562c2496" ]
//  group: io                             version: 1   stability: Private/Private
//    devfs-path        string    /scsi_vhci/disk@g5000c500562c2497
//    devid             string    id1,sd@n5000c500562c2497
//    phys-path         string[]  [ "/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f/disk@w5000c500562c2495,0" "/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f0/disk@w5000c500562c2496,0" ]
//
//

// cfgadm -alv
//c2                             connected    configured   unknown
//unavailable  scsi-sas     n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi
//c2::es/ses0                    connected    configured   unknown    LSI CORP SAS2X28
//unavailable  ESI          n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::es/ses0
//c2::w50025385501cb30c,0        connected    configured   unknown    Client Device: /dev/dsk/c0t50025385501CB30Cd0s0(sd12)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::w50025385501cb30c,0
//c2::w5000c500562a02e1,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A02E3d0s0(sd1)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::w5000c500562a02e1,0
//c2::w5000c500562a02e9,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A02EBd0s0(sd10)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::w5000c500562a02e9,0
//c2::w5000c500562a15f9,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A15FBd0s0(sd7)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::w5000c500562a15f9,0
//c2::w5000c500562a015d,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A015Fd0s0(sd13)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::w5000c500562a015d,0
//c2::w5000c500562a0235,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A0237d0s0(sd4)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::w5000c500562a0235,0
//c2::w5000c500562a0299,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A029Bd0s0(sd8)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::w5000c500562a0299,0
//c2::w5000c500562a0329,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A032Bd0s0(sd11)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::w5000c500562a0329,0
//c2::w5000c500562a0641,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A0643d0s0(sd3)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::w5000c500562a0641,0
//c2::w5000c500562c1b09,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562C1B0Bd0s0(sd9)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::w5000c500562c1b09,0
//c2::w5000c500562c17d1,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562C17D3d0s0(sd2)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::w5000c500562c17d1,0
//c2::w5000c500562c1825,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562C1827d0s0(sd5)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::w5000c500562c1825,0
//c2::w5000c500562c2495,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562C2497d0s0(sd6)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::w5000c500562c2495,0
//c2::w500253855015d44d,0        connected    configured   unknown    Client Device: /dev/dsk/c0t500253855015D44Dd0s0(sd14)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f:scsi::w500253855015d44d,0
//c3                             connected    configured   unknown
//unavailable  scsi-sas     n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f0:scsi
//c3::es/ses1                    connected    configured   unknown    LSI CORP SAS2X28
//unavailable  ESI          n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f0:scsi::es/ses1
//c3::w5000c500562a02e2,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A02E3d0s0(sd1)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f0:scsi::w5000c500562a02e2,0
//c3::w5000c500562a02ea,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A02EBd0s0(sd10)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f0:scsi::w5000c500562a02ea,0
//c3::w5000c500562a015e,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A015Fd0s0(sd13)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f0:scsi::w5000c500562a015e,0
//c3::w5000c500562a15fa,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A15FBd0s0(sd7)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f0:scsi::w5000c500562a15fa,0
//c3::w5000c500562a029a,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A029Bd0s0(sd8)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f0:scsi::w5000c500562a029a,0
//c3::w5000c500562a032a,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A032Bd0s0(sd11)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f0:scsi::w5000c500562a032a,0
//c3::w5000c500562a0236,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A0237d0s0(sd4)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f0:scsi::w5000c500562a0236,0
//c3::w5000c500562a0642,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562A0643d0s0(sd3)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f0:scsi::w5000c500562a0642,0
//c3::w5000c500562c1b0a,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562C1B0Bd0s0(sd9)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f0:scsi::w5000c500562c1b0a,0
//c3::w5000c500562c17d2,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562C17D3d0s0(sd2)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f0:scsi::w5000c500562c17d2,0
//c3::w5000c500562c1826,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562C1827d0s0(sd5)
//unavailable  disk-path    n        /devices/pci@0,0/pci8086,3c0a@3,2/pci1000,3020@0/iport@f0:scsi::w5000c500562c1826,0
//c3::w5000c500562c2496,0        connected    configured   unknown    Client Device: /dev/dsk/c0t5000C500562C2497d0s0(sd6)
//

//mpathadm show lu /dev/rdsk/c0t5000C500562A02E3d0s2
//Logical Unit:  /dev/rdsk/c0t5000C500562A02E3d0s2
//        mpath-support:  libmpscsi_vhci.so
//        Vendor:  SEAGATE
//        Product:  ST1000NM0001
//        Revision:  0002
//        Name Type:  unknown type
//        Name:  5000c500562a02e3
//        Asymmetric:  no
//        Current Load Balance:  none
//        Logical Unit Group ID:  NA
//        Auto Failback:  on
//        Auto Probing:  NA
//
//        Paths:
//                Initiator Port Name:  w500605b0055961c0
//                Target Port Name:  w5000c500562a02e1
//                Override Path:  NA
//                Path State:  OK
//                Disabled:  no
//
//                Initiator Port Name:  w500605b0055961c0
//                Target Port Name:  w5000c500562a02e2
//                Override Path:  NA
//                Path State:  OK
//                Disabled:  no
//
//        Target Ports:
//                Name:  w5000c500562a02e1
//                Relative ID:  0
//
//                Name:  w5000c500562a02e2
//                Relative ID:  0

const

   cSG3Inq = '/opt/local/sg3utils/bin/sg_inq';


type


  { TFRE_DB_SCSI_DEVICE }

  TFRE_DB_SCSI_DEVICE=class(TFRE_DB_ZFS_BLOCKDEVICE)
  private
    function Getfw_revision: string;
    function Getmanufacturer: string;
    function Getmodel_number: string;
    function GetWWN: string;
    function Getserial_number: string;
    function GetSize_MB: UInt32;
    function GetSize_Sectors: UInt32;
    procedure Setfw_revision(AValue: string);
    procedure Setmanufacturer(AValue: string);
    procedure Setmodel_number(AValue: string);
    procedure SetWWN(AValue: string);
    procedure Setserial_number(AValue: string);
    procedure SetSize_MB(AValue: UInt32);
    procedure SetSize_Sectors(AValue: UInt32);
  public
    property WWN: string read GetWWN write SetWWN;
    property Manufacturer: string read Getmanufacturer write Setmanufacturer;
    property Model_number: string read Getmodel_number write Setmodel_number;
    property Fw_revision: string read Getfw_revision write Setfw_revision;
    property Serial_number: string read Getserial_number write Setserial_number;
    property Size_MB: UInt32 read GetSize_MB write SetSize_MB;
    property Size_Sectors: UInt32 read GetSize_Sectors write SetSize_Sectors;
  end;

  { TFRE_DB_SATA_DISK }

  TFRE_DB_SATA_DISK=class(TFRE_DB_SCSI_DEVICE)
  public
  end;

  { TFRE_DB_SAS_DISK }

  TFRE_DB_SAS_DISK=class(TFRE_DB_SCSI_DEVICE)
  public
    procedure SetTargetPorts                    (const targetport_1:string; const targetport_2:string);
  end;

  { TFRE_DB_SAS_EXPANDER }

  TFRE_DB_SAS_EXPANDER=class(TFRE_DB_SCSI_DEVICE)
  private
    function GetExpanderNr: integer;
    procedure SetExpanderNr(AValue: integer);
  public
    procedure AddSASTarget                      (const sas_target : string);

    property  ExpanderNr: integer read GetExpanderNr write SetExpanderNr;
  end;

  { TFRE_DB_DISKBAY }

  TFRE_DB_DISKBAY=class(TFRE_DB_ObjectEx)
  private
    function GetBayNr: integer;
    procedure SetBayNr(AValue: integer);
  public

    property BayNr: integer read GetBayNr write SetBayNr;
  end;

  { TFRE_DB_DISKSHELF }

  TFRE_DB_DISKSHELF=class(TFRE_DB_ObjectEx)
  private
    function GetChassisID: string;
    procedure SetChassisID(AValue: string);
  public
    function AddSASExpander                     (const nr : integer; const wwn: string; const manufacturer: string; const model_number:string; const fw_revision: string; const serialnr:string) : TFRE_DB_SAS_EXPANDER;
    function AddDiskBay                         (const nr : integer) : TFRE_DB_DISKBAY;

    property ChassisID:string read GetChassisID write SetChassisID;
  end;

  TFRE_DB_SCSI = class (TFRE_DB_Multiprocess)
  private
    procedure       AnalyzePrtConfDisks         (const proc   : TFRE_DB_Process;const disks:IFRE_DB_Object);
    function        getrdskDevices               : IFRE_DB_Object;
    function        SG3DiskInquiry              (const devicepath : TFRE_DB_String; out disk: IFRE_DB_Object) : integer;
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    function        GetDiskInformation          (out error: string; out disks:IFRE_DB_Object) : integer;
    function        GetSG3DiskInformation       (out error: string; out disks:IFRE_DB_Object) : integer;

  end;

procedure Register_DB_Extensions;


implementation


procedure TFRE_DB_SCSI.AnalyzePrtConfDisks(const proc: TFRE_DB_Process; const disks: IFRE_DB_Object);
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
                                     disk   := GFRE_DBI.NewObjectScheme(TFRE_DB_SCSI_DEVICE);
                                   end;
                               end;
           psDiskInfo:        begin
//                                writeln('DISKINFO:',s);
                                 if Pos('name=''inquiry-serial-no''',s)>0 then
                                   (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).Setserial_number(GetValueS(slist[i+1]));
                                 if Pos('name=''inquiry-revision-id''',s)>0 then
                                   (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).Setfw_revision(GetValueS(slist[i+1]));
                                 if Pos('name=''inquiry-product-id''',s)>0 then
                                   (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).Setmodel_number(GetValueS(slist[i+1]));
                                 if Pos('name=''inquiry-vendor-id''',s)>0 then
                                   (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).Setmanufacturer(GetValueS(slist[i+1]));
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
                                   (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).SetWWN(GetValueS(slist[i+1]));
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
                                     disk   := GFRE_DBI.NewObjectScheme(TFRE_DB_SCSI_DEVICE);
                                   end;

                               end;
           psDeviceName:       begin
                                 if Pos('dev_link',s)>0 then
                                   begin
                                     devicename := GetDirectS(s);
                                     devicename := Copy(devicename,1,length(devicename)-2);
                                     devicename := Copy(devicename,10,length(devicename));
                                     (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).DeviceName := devicename;

                                     if (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).DeviceIdentifier='' then          // no WWN
                                       (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).DeviceIdentifier:=devicename;

                                     if Pos('c',devicename)=1 then
                                       disks.Field('disks').addobject(disk)
                                     else
                                       disk.Finalize;

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

function TFRE_DB_SCSI.getrdskDevices: IFRE_DB_Object;
var sl : TStringList;
    i  : NativeInt;
    proc  : TFRE_DB_Process;
begin
  if FieldExists('remotehost') then
    begin
      ClearProcess;
      proc := TFRE_DB_Process.create;
      proc.SetupInput('find',TFRE_DB_StringArray.Create ('/dev/rdsk/*d0'));
      AddProcess(proc);
      ExecuteMulti;
      result := GFRE_DBI.NewObject;
      sl := TStringList.Create;
      try
        sl.Text:=TFRE_DB_Process (proc.Implementor_HC).OutputToString;
        for i:= 0 to sl.count-1 do
          begin
            result.Field('device').AddString(sl[i]);
          end;
      finally
        sl.Free;
      end;
    end
  else
    abort; //Implement local Find for devices
end;

function TFRE_DB_SCSI.SG3DiskInquiry(const devicepath: TFRE_DB_String; out disk: IFRE_DB_Object): integer;
var
  proc        : TFRE_DB_Process;
  sl          : TStringList;
  i           : NativeInt;
  s           : string;
  porti       : NativeInt;
  ports       : Array[1..2] of string;
  address_loc : boolean;
  devicename  : string;

begin
  writeln(devicepath);
  // determine sas/sata, get target ports

  ClearProcess;
  proc := TFRE_DB_Process.create;
  proc.SetupInput(cSG3Inq,TFRE_DB_StringArray.Create ('-p','0x88',devicepath));
  AddProcess(proc);
  ExecuteMulti;
  if proc.Field('exitstatus').AsInt32=0 then //SAS DISK
    begin
      disk  := GFRE_DBI.NewObjectScheme(TFRE_DB_SAS_DISK);
      porti := 0;
      sl := TStringList.Create;
      try
        sl.Text := proc.OutputToString;
        for i:=0 to sl.count-1 do
         begin
           s:= sl[i];
           if Pos('Relative port=1',s)>0 then
             porti := 1;
           if Pos('Relative port=2',s)>0 then
             porti := 2;
           if Pos('Vendor Specific Identifier:',s)>0 then
             begin
               if porti>0 then
                 begin
                   s := sl[i+1];
                   ports[porti] := GFRE_BT.SepLeft(GFRE_BT.SepRight(s,'['),']');
                   porti :=0;
                 end;
             end;
         end;
      finally
        sl.Free;
      end;
      (disk.Implementor_HC as TFRE_DB_SAS_DISK).SetTargetPorts(ports[1],ports[2]);
    end
  else
    begin
      if proc.Field('exitstatus').AsInt32=5 then
        disk  := GFRE_DBI.NewObjectScheme(TFRE_DB_SATA_DISK)
      else
        begin
          disk   := nil;
          exit(1);
        end;
    end;

  // get wwn
  writeln('wwn');
  ClearProcess;
  proc := TFRE_DB_Process.create;
  proc.SetupInput(cSG3Inq,TFRE_DB_StringArray.Create ('-p','di',devicepath));
  AddProcess(proc);
  ExecuteMulti;
  if proc.Field('exitstatus').AsInt32=0 then
    begin
      address_loc := false;
      sl := TStringList.Create;
      try
        sl.Text := proc.OutputToString;
        for i:=0 to sl.count-1 do
         begin
           s:= sl[i];
           if Pos('associated with the addressed logical unit',s)>0 then
             address_loc:=true;
           if Pos('Vendor Specific Identifier:',s)>0 then
             begin
               if address_loc then
                 begin
                   s := sl[i+1];
                   (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).SetWWN(GFRE_BT.SepLeft(GFRE_BT.SepRight(s,'['),']'));
                   address_loc:= false;
                 end;
             end;
         end;
      finally
        sl.Free;
      end;
    end;

  devicename := Copy(devicepath,11,length(devicepath));
  (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).DeviceName := devicename;

  if (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).DeviceIdentifier='' then          // no WWN
    (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).DeviceIdentifier:=devicename;

  //get serial, vendor
  writeln('serial');

  ClearProcess;
  proc := TFRE_DB_Process.create;
  proc.SetupInput(cSG3Inq,TFRE_DB_StringArray.Create (devicepath));
  AddProcess(proc);
  ExecuteMulti;
  if proc.Field('exitstatus').AsInt32=0 then
    begin
      sl := TStringList.Create;
      try
        sl.Text := proc.OutputToString;
        for i:=0 to sl.count-1 do
         begin
           s:= sl[i];
           if Pos('Vendor identification',s)>0 then
             (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).Setmanufacturer(trim(GFRE_BT.SepRight(s,':')));
           if Pos('Product identification',s)>0 then
             (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).Setmodel_number(trim(GFRE_BT.SepRight(s,':')));
           if Pos('Product revision level',s)>0 then
             (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).Setfw_revision(trim(GFRE_BT.SepRight(s,':')));
           if Pos('Unit serial number',s)>0 then
             (disk.Implementor_HC as TFRE_DB_SCSI_DEVICE).Setserial_number(trim(GFRE_BT.SepRight(s,':')));
         end;
      finally
        sl.Free;
      end;
    end
  else
    begin
      disk   := nil;
      exit(proc.Field('exitstatus').AsInt32);
    end;

  exit(0);
end;

class procedure TFRE_DB_SCSI.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

function TFRE_DB_SCSI.GetDiskInformation(out error: string; out disks: IFRE_DB_Object): integer;
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

function TFRE_DB_SCSI.GetSG3DiskInformation(out error: string; out disks: IFRE_DB_Object): integer;
var devices : IFRE_DB_Object;
    disk    : IFRE_DB_Object;
    i       : NativeInt;
begin
  disks   := GFRE_DBI.NewObject;
  devices := getrdskDevices;
  for i := 0 to devices.Field('device').ValueCount-1 do
    begin
      if SG3DiskInquiry(devices.Field('device').AsStringItem[i],disk)=0 then
        begin
          disks.Field('disks').addobject(disk);
          writeln(disk.DumpToString);
        end;
    end;
  writeln(disks.DumpToString);
end;

{ TFRE_DB_SCSI_DEVICE }


function TFRE_DB_SCSI_DEVICE.Getfw_revision: string;
begin
  result:=field('fw_revision').asstring;
end;

function TFRE_DB_SCSI_DEVICE.Getmanufacturer: string;
begin
  result:=field('manufacturer').asstring;
end;

function TFRE_DB_SCSI_DEVICE.Getmodel_number: string;
begin
  result:=field('model_number').asstring;
end;

function TFRE_DB_SCSI_DEVICE.GetWWN: string;
begin
  result:=getDeviceIdentifier;
end;

function TFRE_DB_SCSI_DEVICE.Getserial_number: string;
begin
  result:=field('serial_number').asstring;
end;

function TFRE_DB_SCSI_DEVICE.GetSize_MB: UInt32;
begin
  result := field('size_mb').AsUInt32;
end;

function TFRE_DB_SCSI_DEVICE.GetSize_Sectors: UInt32;
begin
  result := field('size_sectors').AsUInt32;
end;


procedure TFRE_DB_SCSI_DEVICE.Setfw_revision(AValue: string);
begin
  field('fw_revision').asstring := avalue;
end;

procedure TFRE_DB_SCSI_DEVICE.Setmanufacturer(AValue: string);
begin
  field('manufacturer').asstring := avalue;
end;

procedure TFRE_DB_SCSI_DEVICE.Setmodel_number(AValue: string);
begin
  field('model_number').asstring := avalue;
end;


procedure TFRE_DB_SCSI_DEVICE.SetWWN(AValue: string);
begin
  setDeviceIdentifier(AValue);
end;

procedure TFRE_DB_SCSI_DEVICE.Setserial_number(AValue: string);
begin
  field('serial_number').asstring := avalue;
end;

procedure TFRE_DB_SCSI_DEVICE.SetSize_MB(AValue: UInt32);
begin
  field('size_mb').AsUInt32:=AValue;
end;

procedure TFRE_DB_SCSI_DEVICE.SetSize_Sectors(AValue: UInt32);
begin
  field('size_sectors').AsUInt32 := AValue;
end;


{ TFRE_DB_SAS_DISK }

procedure TFRE_DB_SAS_DISK.SetTargetPorts(const targetport_1: string; const targetport_2: string);
begin
  Field('target_port').AsString:=targetport_1;
  Field('target_port').AddString(targetport_2);
end;

{ TFRE_DB_DISKBAY }

function TFRE_DB_DISKBAY.GetBayNr: integer;
begin
  result := field('baynr').AsInt32;
end;

procedure TFRE_DB_DISKBAY.SetBayNr(AValue: integer);
begin
  field('baynr').AsInt32 := AValue;
end;


{ TFRE_DB_SAS_EXPANDER }

function TFRE_DB_SAS_EXPANDER.GetExpanderNr: integer;
begin
  result := field('expandernr').AsInt32;
end;

procedure TFRE_DB_SAS_EXPANDER.SetExpanderNr(AValue: integer);
begin
  field('expandernr').AsInt32 := AValue;
end;

procedure TFRE_DB_SAS_EXPANDER.AddSASTarget(const sas_target: string);
begin
  field('target').AddString(sas_target);
end;


{ TFRE_DB_DISKSHELF }

function TFRE_DB_DISKSHELF.GetChassisID: string;
begin
  result := Field('chassisid').AsString;
end;

procedure TFRE_DB_DISKSHELF.SetChassisID(AValue: string);
begin
  Field('ChassisID').Asstring := AValue;
end;

function TFRE_DB_DISKSHELF.AddSASExpander(const nr: integer; const wwn: string; const manufacturer: string; const model_number: string; const fw_revision: string; const serialnr: string): TFRE_DB_SAS_EXPANDER;
begin
  result   := TFRE_DB_SAS_EXPANDER.create;
  result.ExpanderNr    := nr;
  result.wwn           := wwn;
  result.Manufacturer  := manufacturer;
  result.Model_number  := model_number;
  result.Fw_revision   := fw_revision;
  result.Serial_number := serialnr;
  field('expander').AddObject(result);
end;

function TFRE_DB_DISKSHELF.AddDiskBay(const nr: integer): TFRE_DB_DISKBAY;
begin
  result   := TFRE_DB_DISKBAY.create;
  result.BayNr    := nr;
  field('bay').AddObject(result);
end;

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SCSI_DEVICE);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SAS_DISK);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SATA_DISK);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SAS_EXPANDER);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DISKBAY);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DISKSHELF);
  GFRE_DBI.Initialize_Extension_Objects;
end;


end.

