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
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils,FRE_DB_INTERFACE, FRE_DB_COMMON, FRE_PROCESS, FOS_BASIS_TOOLS,
  FOS_TOOL_INTERFACES,fre_zfs,fre_system,fre_monitoring;

var

   cSG3Inq : string = '/opt/local/sg3utils/bin/sg_inq';
   cSG3Ses : string = '/opt/local/sg3utils/bin/sg_ses';
   cSG3cap : string = '/opt/local/sg3utils/bin/sg_readcap';
   cSG3logs: string = '/opt/local/sg3utils/bin/sg_logs';
   cMpathListLu : string = 'mpathadm';
   cSG3LogPages : Array [0..5] of String = ('0x2','0x3','0x5','0x6','0xd','0x18');

type

  TFRE_DB_UNDEFINED_BLOCKDEVICE = class(TFRE_DB_OS_BLOCKDEVICE)   // devices from iostat without /dev/rdsk/*s2 entry
  end;

  { TFRE_DB_SG_LOGS }

  TFRE_DB_SG_LOGS=class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects            (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function        WEB_GetDefaultCollection    (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_PHYS_DISK }

  TFRE_DB_PHYS_DISK=class(TFRE_DB_OS_BLOCKDEVICE)
  private
    function GetBlockSize: Uint16;
    function GetEnclosureUID: TFRE_DB_GUID;
    function GetEnclosureNr: Uint16;
    function Getfw_revision: string;
    function Getmanufacturer: string;
    function Getmodel_number: string;
    function GetOperationalPathCount: Uint16;
    function GetParentInEnclosureUID: TFRE_DB_GUID;
    function GetSlotNr: Uint16;
    function GetTotalPathCount: Uint16;
    function GetWWN: string;
    function Getserial_number: string;
    function GetSize_Sectors: UInt32;
    function GetDiskLog: TFRE_DB_SG_LOGS;
    procedure SetBlockSize(AValue: Uint16);
    procedure SetEnclosureNr(AValue: Uint16);
    procedure SetEnclosureUID(AValue: TFRE_DB_GUID);
    procedure Setfw_revision(AValue: string);
    procedure Setmanufacturer(AValue: string);
    procedure Setmodel_number(AValue: string);
    procedure SetOperationalPathCount(AValue: Uint16);
    procedure SetParentInEnclosureUID(AValue: TFRE_DB_GUID);
    procedure SetSlotNr(AValue: Uint16);
    procedure SetTotalPathCount(AValue: Uint16);
    procedure SetWWN(AValue: string);
    procedure Setserial_number(AValue: string);
    procedure SetSize_Sectors(AValue: UInt32);
    procedure SetDiskLog(Avalue:TFRE_DB_SG_LOGS);
  protected
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects            (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function  GetTargetPorts: TFRE_DB_StringArray;
    function  hasParentinEnclosure               : boolean;
    procedure ClearSlotandEnclosureInformation   ;
    property WWN                                 : string read GetWWN write SetWWN;
    property Manufacturer                        : string read Getmanufacturer write Setmanufacturer;
    property Model_number                        : string read Getmodel_number write Setmodel_number;
    property Fw_revision                         : string read Getfw_revision write Setfw_revision;
    property Serial_number                       : string read Getserial_number write Setserial_number;
    property Size_Sectors                        : UInt32 read GetSize_Sectors write SetSize_Sectors;
    property ParentInEnclosureUID                : TFRE_DB_GUID read GetParentInEnclosureUID write SetParentInEnclosureUID;
    property SlotNr                              : Uint16 read GetSlotNr write SetSlotNr;
    property EnclosureNr                         : Uint16 read GetEnclosureNr write SetEnclosureNr;
    property EnclosureUID                        : TFRE_DB_GUID  read GetEnclosureUID write SetEnclosureUID;
    property BlockSize                           : Uint16 read GetBlockSize write SetBlockSize;
    property TotalPathCount                      : Uint16 read GetTotalPathCount write SetTotalPathCount;
    property OperationalPathCount                : Uint16 read GetOperationalPathCount write SetOperationalPathCount;
    property DiskLog                             : TFRE_DB_SG_LOGS read GetDiskLog write SetDiskLog;
  published
    procedure _getLayoutCaption                 (const calcfieldsetter: IFRE_DB_CALCFIELD_SETTER);
    procedure _getLayoutSubCaption              (const calcfieldsetter: IFRE_DB_CALCFIELD_SETTER);
    procedure _getLayoutIcon                    (const calcfieldsetter: IFRE_DB_CALCFIELD_SETTER);
    procedure _getSizeMB                        (const calcfieldsetter : IFRE_DB_CALCFIELD_SETTER); virtual;
    procedure _getCaption                       (const calcfieldsetter : IFRE_DB_CALCFIELD_SETTER); override;
    procedure _getMOSCaption                    (const calcfieldsetter : IFRE_DB_CALCFIELD_SETTER); override;
    function  WEB_MOSContent             (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_SATA_DISK }

  TFRE_DB_SATA_DISK=class(TFRE_DB_PHYS_DISK)
  public
    procedure SetTargetPort                     (const targetport_1:string);
    class procedure InstallDBObjects            (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_SAS_DISK }

  TFRE_DB_SAS_DISK=class(TFRE_DB_PHYS_DISK)
  public
    procedure SetTargetPorts                    (const targetport_1:string; const targetport_2:string);
    class procedure InstallDBObjects            (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;

  end;


  { TFRE_DB_SAS_EXPANDER }

  TFRE_DB_SAS_EXPANDER=class(TFRE_DB_ObjectEx)
  private
    function  Getfw_revision: string;
    function  Getmanufacturer: string;
    function  Getmodel_number: string;
    function  GetDeviceIdentifier: TFRE_DB_String;
    function  GetParentInEnclosureUID: TFRE_DB_GUID;
    procedure Setfw_revision(AValue: string);
    procedure Setmanufacturer(AValue: string);
    procedure Setmodel_number(AValue: string);
    procedure SetDeviceIdentifier(AValue: TFRE_DB_String);
    procedure SetParentInEnclosureUID(AValue: TFRE_DB_GUID);
  public
    class procedure InstallDBObjects            (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    procedure AddMosParentID                    (const avalue : TFRE_DB_GUID);
    procedure RemoveMosParentID                 (const avalue : TFRE_DB_GUID);
    property  Model_number         : string read Getmodel_number write Setmodel_number;
    property  Manufacturer         : string read Getmanufacturer write Setmanufacturer;
    property  Fw_revision          : string read Getfw_revision write Setfw_revision;
    property  DeviceIdentifier     : TFRE_DB_String read getDeviceIdentifier write setDeviceIdentifier;
    property  ParentInEnclosureUID : TFRE_DB_GUID read GetParentInEnclosureUID write SetParentInEnclosureUID;
  published
    function  WEB_GetDefaultCollection    (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_DRIVESLOT }

  TFRE_DB_DRIVESLOT=class(TFRE_DB_ObjectEx)
  private
    function  GetDeviceIdentifier: TFRE_DB_String;
    function  GetEnclosureNr: Uint16;
    function  GetPortType: string;
    function  GetSlotNr    : UInt16;
    function  GetParentInEnclosureUID: TFRE_DB_GUID;
    procedure setDeviceIdentifier(AValue: TFRE_DB_String);
    procedure SetEnclosureNr(AValue: Uint16);
    procedure SetParentInEnclosureUID(AValue: TFRE_DB_GUID);
    procedure SetPortType(AValue: string);
    procedure SetSlotNr   (AValue: UInt16);
  public
    class procedure InstallDBObjects            (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    procedure AddMosParentID                    (const avalue : TFRE_DB_GUID);
    procedure RemoveMosParentID                 (const avalue : TFRE_DB_GUID);
    procedure SetAttachedTo  (const port_nr:Byte; const AValue: string);
    procedure SetTargetPort  (const port_nr:Byte; const AValue: string);
    function  GetTargetPort  (const port_nr:Byte) : TFRE_DB_String;
    procedure RemoveLinkToMe             (const conn:IFRE_DB_CONNECTION);
    property  SlotNr: Uint16 read GetSlotNr write SetSlotNr;
    property  PortType             : string read GetPortType write SetPortType;
    property  DeviceIdentifier     : TFRE_DB_String read getDeviceIdentifier write setDeviceIdentifier;
    property  ParentInEnclosureUID : TFRE_DB_GUID read GetParentInEnclosureUID write SetParentInEnclosureUID;
    property  EnclosureNr          : Uint16 read GetEnclosureNr write SetEnclosureNr;
  published
    procedure CALC_GetDisplayName         (const setter : IFRE_DB_CALCFIELD_SETTER);
    function  WEB_GetDefaultCollection    (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;


  { TFRE_DB_ENCLOSURE }

  TFRE_DB_ENCLOSURE=class(TFRE_DB_ObjectEx)
  private
    function  getDeviceIdentifier               : TFRE_DB_String;
    function  GetDriveSlots                     : UInt16;
    function  GetEnclosureNr                    : Uint16;
    function  GetMachineID                      : TFRE_DB_GUID;
    function  GetParentInEnclosureUID           : TFRE_DB_GUID;
    procedure setDeviceIdentifier               (AValue: TFRE_DB_String);
    procedure SetDriveSlots                     (AValue: UInt16);
    procedure SetEnclosureNr                    (AValue: Uint16);
    procedure SetMachineID                      (AValue: TFRE_DB_GUID);
    procedure SetParentInEnclosureUID           (AValue: TFRE_DB_GUID);
  public
    class procedure InstallDBObjects            (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure RegisterSystemScheme        (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    procedure AddExpanderEmbedded               (const expander: TFRE_DB_SAS_Expander; const devicename: string='');
    function  GetDriveSlotEmbedded              (const slotnr:UInt16; out driveslot: TFRE_DB_DRIVESLOT):boolean;
    procedure AddDriveSlotEmbedded              (const slotnr:UInt16; const driveslot: TFRE_DB_DRIVESLOT);
    procedure DeleteReferencingToMe             (const conn:IFRE_DB_CONNECTION);
    procedure EmbedSlotsAndExpanders            (const conn:IFRE_DB_CONNECTION);
    procedure AddMosParentID                    (const avalue : TFRE_DB_GUID);
    procedure RemoveMosParentID                 (const avalue : TFRE_DB_GUID);


    procedure SetMOSStatus                      (const status: TFRE_DB_MOS_STATUS_TYPE; const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION);
    function  GetMOSStatus                      : TFRE_DB_MOS_STATUS_TYPE;

    property  DeviceIdentifier : TFRE_DB_String read getDeviceIdentifier write setDeviceIdentifier;
    property  DriveSlots       : UInt16 read GetDriveSlots write SetDriveSlots;
    property  EnclosureNr      : Uint16 read GetEnclosureNr write SetEnclosureNr;
    property  MachineID        : TFRE_DB_GUID read GetMachineID write SetMachineID;
    property  ParentInEnclosureUID : TFRE_DB_GUID read GetParentInEnclosureUID write SetParentInEnclosureUID;

  published
    procedure _getChildrenString                (const calcfieldsetter : IFRE_DB_CALCFIELD_SETTER);
    procedure _getLayoutCaption                 (const calcfieldsetter : IFRE_DB_CALCFIELD_SETTER);
    procedure _getLayoutSubcaption              (const calcfieldsetter : IFRE_DB_CALCFIELD_SETTER);
    procedure _getLayoutIcon                    (const calcfieldsetter : IFRE_DB_CALCFIELD_SETTER);
    procedure _getStatusIcon                    (const calc: IFRE_DB_CALCFIELD_SETTER);
    function  WEB_MOSContent             (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_MOSChildStatusChanged  (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_MOSStatus              (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_GetDefaultCollection   (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_SCSI }

  TFRE_DB_SCSI = class (TFRE_DB_Multiprocess)
  private
    function        _SG3GetPage                        (const cmd:string;  out output:string;out error:string; const params : TFRE_DB_StringArray=nil):integer;
    function        _FileExistsLocalorRemote           (const filename:string) : boolean;

    function        getrdskDevices                     (const iostat_devices: IFRE_DB_Object): IFRE_DB_Object;
    function        getexpanderDevices                 : IFRE_DB_Object;
    function        SG3DiskInquiry                     (const devicepath : TFRE_DB_String; out disk: IFRE_DB_Object; const read_logs:boolean=false) : integer;
    function        SG3SESInquiry                      (const devicepath : TFRE_DB_String; var scsi_structure: IFRE_DB_Object) : integer;
  protected
    class procedure RegisterSystemScheme               (const scheme : IFRE_DB_SCHEMEOBJECT); override;
  public
    function        GetSG3DiskandEnclosureInformation  (const iostat_devices: IFRE_DB_Object; out error: string; out scsi_structure:IFRE_DB_Object;const read_logs:boolean=false) : integer;
    function        GetMpathAdmLUInformation           (out error: string; out mpathinfo:IFRE_DB_Object) : integer;

  end;

procedure Register_DB_Extensions;


implementation

{ TFRE_DB_SG_LOGS }

class procedure TFRE_DB_SG_LOGS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.Classname);

  scheme.AddSchemeField('EC_READ_EC_POSSIBLE_DELAYS',fdbft_Int64);
  scheme.AddSchemeField('EC_READ_EC_SUBSTANTIAL_DELAY',fdbft_Int64);
  scheme.AddSchemeField('EC_READ_TOTAL_BYTES_PROCESSED',fdbft_Int64);
  scheme.AddSchemeField('EC_READ_TOTAL_ERRORS_CORRECTED',fdbft_Int64);
  scheme.AddSchemeField('EC_READ_TOTAL_REWRITES_OR_REREADS',fdbft_Int64);
  scheme.AddSchemeField('EC_READ_TOTAL_TIMES_CORR_ALGO_PROCESSED',fdbft_Int64);
  scheme.AddSchemeField('EC_READ_TOTAL_UNCORRECTED_ERRORS',fdbft_Int64);

  scheme.AddSchemeField('EC_VERIFY_EC_POSSIBLE_DELAYS',fdbft_Int64);
  scheme.AddSchemeField('EC_VERIFY_EC_SUBSTANTIAL_DELAY',fdbft_Int64);
  scheme.AddSchemeField('EC_VERIFY_TOTAL_BYTES_PROCESSED',fdbft_Int64);
  scheme.AddSchemeField('EC_VERIFY_TOTAL_ERRORS_CORRECTED',fdbft_Int64);
  scheme.AddSchemeField('EC_VERIFY_TOTAL_REWRITES_OR_REREADS',fdbft_Int64);
  scheme.AddSchemeField('EC_VERIFY_TOTAL_TIMES_CORR_ALGO_PROCESSED',fdbft_Int64);
  scheme.AddSchemeField('EC_VERIFY_TOTAL_UNCORRECTED_ERRORS',fdbft_Int64);

  scheme.AddSchemeField('EC_WRITE_EC_POSSIBLE_DELAYS',fdbft_Int64);
  scheme.AddSchemeField('EC_WRITE_TOTAL_BYTES_PROCESSED',fdbft_Int64);
  scheme.AddSchemeField('EC_WRITE_TOTAL_ERRORS_CORRECTED',fdbft_Int64);
  scheme.AddSchemeField('EC_WRITE_TOTAL_REWRITES_OR_REREADS',fdbft_Int64);
  scheme.AddSchemeField('EC_WRITE_TOTAL_TIMES_CORR_ALGO_PROCESSED',fdbft_Int64);
  scheme.AddSchemeField('EC_WRITE_TOTAL_UNCORRECTED_ERRORS',fdbft_Int64);

  scheme.AddSchemeField('NON_MEDIUM_ERROR_COUNT',fdbft_Int64);
  scheme.AddSchemeField('TEMP_REFERENCE_TEMP',fdbft_Int16);
  scheme.AddSchemeField('TEMP_CURRENT_TEMP',fdbft_Int16);

  scheme.AddSchemeField('PROTOCOL_TP1_ATTACHED_DEVICE_TYPE',fdbft_String);
  scheme.AddSchemeField('PROTOCOL_TP1_ATTACHED_INITIATOR_PORT',fdbft_String);
  scheme.AddSchemeField('PROTOCOL_TP1_ATTACHED_PHY_IDENTIFIER',fdbft_Int16);
  scheme.AddSchemeField('PROTOCOL_TP1_ATTACHED_REASON',fdbft_String);
  scheme.AddSchemeField('PROTOCOL_TP1_ATTACHED_SAS_ADDRESS',fdbft_String);
  scheme.AddSchemeField('PROTOCOL_TP1_ATTACHED_TARGET_PORT',fdbft_String);
  scheme.AddSchemeField('PROTOCOL_TP1_GENERATION_CODE',fdbft_Int16);
  scheme.AddSchemeField('PROTOCOL_TP1_INVALID_DWORD_COUNT',fdbft_Int64);
  scheme.AddSchemeField('PROTOCOL_TP1_LOSS_DWORD_SYNC',fdbft_Int64);
  scheme.AddSchemeField('PROTOCOL_TP1_NEG_LINK_RATE',fdbft_String);
  scheme.AddSchemeField('PROTOCOL_TP1_NUMBER_PHYS',fdbft_int16);
  scheme.AddSchemeField('PROTOCOL_TP1_PHY_IDENTIFIER',fdbft_int16);
  scheme.AddSchemeField('PROTOCOL_TP1_PHY_INVALID_WORD_COUNT',fdbft_int64);
  scheme.AddSchemeField('PROTOCOL_TP1_PHY_LOSS_DWORD_SYNC_COUNT',fdbft_int64);
  scheme.AddSchemeField('PROTOCOL_TP1_PHY_RESET_PROBLEM',fdbft_int64);
  scheme.AddSchemeField('PROTOCOL_TP1_PHY_RESET_PROBLEM_COUNT',fdbft_int64);
  scheme.AddSchemeField('PROTOCOL_TP1_REASON',fdbft_String);
  scheme.AddSchemeField('PROTOCOL_TP1_RUNNING_DISPARITY_EC',fdbft_int64);
  scheme.AddSchemeField('PROTOCOL_TP1_SAS_ADDRESS',fdbft_string);

  scheme.AddSchemeField('PROTOCOL_TP2_ATTACHED_DEVICE_TYPE',fdbft_String);
  scheme.AddSchemeField('PROTOCOL_TP2_ATTACHED_INITIATOR_PORT',fdbft_String);
  scheme.AddSchemeField('PROTOCOL_TP2_ATTACHED_PHY_IDENTIFIER',fdbft_Int16);
  scheme.AddSchemeField('PROTOCOL_TP2_ATTACHED_REASON',fdbft_String);
  scheme.AddSchemeField('PROTOCOL_TP2_ATTACHED_SAS_ADDRESS',fdbft_String);
  scheme.AddSchemeField('PROTOCOL_TP2_ATTACHED_TARGET_PORT',fdbft_String);
  scheme.AddSchemeField('PROTOCOL_TP2_GENERATION_CODE',fdbft_Int16);
  scheme.AddSchemeField('PROTOCOL_TP2_INVALID_DWORD_COUNT',fdbft_Int64);
  scheme.AddSchemeField('PROTOCOL_TP2_LOSS_DWORD_SYNC',fdbft_Int64);
  scheme.AddSchemeField('PROTOCOL_TP2_NEG_LINK_RATE',fdbft_String);
  scheme.AddSchemeField('PROTOCOL_TP2_NUMBER_PHYS',fdbft_int16);
  scheme.AddSchemeField('PROTOCOL_TP2_PHY_IDENTIFIER',fdbft_int16);
  scheme.AddSchemeField('PROTOCOL_TP2_PHY_INVALID_WORD_COUNT',fdbft_int64);
  scheme.AddSchemeField('PROTOCOL_TP2_PHY_LOSS_DWORD_SYNC_COUNT',fdbft_int64);
  scheme.AddSchemeField('PROTOCOL_TP2_PHY_RESET_PROBLEM',fdbft_int64);
  scheme.AddSchemeField('PROTOCOL_TP2_PHY_RESET_PROBLEM_COUNT',fdbft_int64);
  scheme.AddSchemeField('PROTOCOL_TP2_REASON',fdbft_String);
  scheme.AddSchemeField('PROTOCOL_TP2_RUNNING_DISPARITY_EC',fdbft_int64);
  scheme.AddSchemeField('PROTOCOL_TP2_SAS_ADDRESS',fdbft_string);

  group:=scheme.AddInputGroup('log_common').Setup('scheme_common');
  group.AddInput('NON_MEDIUM_ERROR_COUNT',GetTranslateableTextKey('scheme_NON_MEDIUM_ERROR_COUNT'),true);
  group.AddInput('TEMP_REFERENCE_TEMP',GetTranslateableTextKey('scheme_TEMP_REFERENCE_TEMP'),true);
  group.AddInput('TEMP_CURRENT_TEMP',GetTranslateableTextKey('scheme_TEMP_CURRENT_TEMP'),true);

  group:=scheme.AddInputGroup('log_ec').Setup('scheme_ec');
  group.AddInput('EC_READ_EC_POSSIBLE_DELAYS',GetTranslateableTextKey('scheme_EC_READ_EC_POSSIBLE_DELAYS'),true);
  group.AddInput('EC_READ_EC_SUBSTANTIAL_DELAY',GetTranslateableTextKey('scheme_EC_READ_EC_SUBSTANTIAL_DELAY'),true);
  group.AddInput('EC_READ_TOTAL_BYTES_PROCESSED',GetTranslateableTextKey('scheme_EC_READ_TOTAL_BYTES_PROCESSED'),true);
  group.AddInput('EC_READ_TOTAL_ERRORS_CORRECTED',GetTranslateableTextKey('scheme_EC_READ_TOTAL_ERRORS_CORRECTED'),true);
  group.AddInput('EC_READ_TOTAL_REWRITES_OR_REREADS',GetTranslateableTextKey('scheme_EC_READ_TOTAL_REWRITES_OR_REREADS'),true);
  group.AddInput('EC_READ_TOTAL_TIMES_CORR_ALGO_PROCESSED',GetTranslateableTextKey('scheme_EC_READ_TOTAL_TIMES_CORR_ALGO_PROC'),true);
  group.AddInput('EC_READ_TOTAL_UNCORRECTED_ERRORS',GetTranslateableTextKey('scheme_EC_READ_TOTAL_UNCORRECTED_ERRORS'),true);

  group.AddInput('EC_VERIFY_EC_POSSIBLE_DELAYS',GetTranslateableTextKey('scheme_EC_VERIFY_EC_POSSIBLE_DELAYS'),true);
  group.AddInput('EC_VERIFY_EC_SUBSTANTIAL_DELAY',GetTranslateableTextKey('scheme_EC_VERIFY_EC_SUBSTANTIAL_DELAY'),true);
  group.AddInput('EC_VERIFY_TOTAL_BYTES_PROCESSED',GetTranslateableTextKey('scheme_EC_VERIFY_TOTAL_BYTES_PROCESSED'),true);
  group.AddInput('EC_VERIFY_TOTAL_ERRORS_CORRECTED',GetTranslateableTextKey('scheme_EC_VERIFY_TOTAL_ERRORS_CORRECTED'),true);
  group.AddInput('EC_VERIFY_TOTAL_REWRITES_OR_REREADS',GetTranslateableTextKey('scheme_EC_VERIFY_TOTAL_REWRITES_OR_REREADS'),true);
  group.AddInput('EC_VERIFY_TOTAL_TIMES_CORR_ALGO_PROCESSED',GetTranslateableTextKey('scheme_EC_VERIFY_TOTAL_TIMES_CORR_ALGO_PROC'),true);
  group.AddInput('EC_VERIFY_TOTAL_UNCORRECTED_ERRORS',GetTranslateableTextKey('scheme_EC_VERIFY_TOTAL_UNCORRECTED_ERRORS'),true);

  group.AddInput('EC_WRITE_EC_POSSIBLE_DELAYS',GetTranslateableTextKey('scheme_EC_WRITE_EC_POSSIBLE_DELAYS'),true);
  group.AddInput('EC_WRITE_TOTAL_BYTES_PROCESSED',GetTranslateableTextKey('scheme_EC_WRITE_TOTAL_BYTES_PROCESSED'),true);
  group.AddInput('EC_WRITE_TOTAL_ERRORS_CORRECTED',GetTranslateableTextKey('scheme_EC_WRITE_TOTAL_ERRORS_CORRECTED'),true);
  group.AddInput('EC_WRITE_TOTAL_REWRITES_OR_REREADS',GetTranslateableTextKey('scheme_EC_WRITE_TOTAL_REWRITES_OR_REREADS'),true);
  group.AddInput('EC_WRITE_TOTAL_TIMES_CORR_ALGO_PROCESSED',GetTranslateableTextKey('scheme_EC_WRITE_TOTAL_TIMES_CORR_ALGO_PROC'),true);
  group.AddInput('EC_WRITE_TOTAL_UNCORRECTED_ERRORS',GetTranslateableTextKey('scheme_EC_WRITE_TOTAL_UNCORRECTED_ERRORS'),true);


  group:=scheme.AddInputGroup('log_tp1').Setup('scheme_tp1');

  group.AddInput('PROTOCOL_TP1_SAS_ADDRESS',GetTranslateableTextKey('scheme_PROTOCOL_TP1_SAS_ADDRESS'),true);
  group.AddInput('PROTOCOL_TP1_ATTACHED_DEVICE_TYPE',GetTranslateableTextKey('scheme_PROTOCOL_TP1_ATTACHED_DEVICE_TYPE'),true);
  group.AddInput('PROTOCOL_TP1_ATTACHED_INITIATOR_PORT',GetTranslateableTextKey('scheme_PROTOCOL_TP1_ATTACHED_INITIATOR_PORT'),true);
  group.AddInput('PROTOCOL_TP1_ATTACHED_PHY_IDENTIFIER',GetTranslateableTextKey('scheme_PROTOCOL_TP1_ATTACHED_PHY_IDENTIFIER'),true);
  group.AddInput('PROTOCOL_TP1_ATTACHED_REASON',GetTranslateableTextKey('scheme_PROTOCOL_TP1_ATTACHED_REASON'),true);
  group.AddInput('PROTOCOL_TP1_ATTACHED_SAS_ADDRESS',GetTranslateableTextKey('scheme_PROTOCOL_TP1_ATTACHED_SAS_ADDRESS'),true);
  group.AddInput('PROTOCOL_TP1_ATTACHED_TARGET_PORT',GetTranslateableTextKey('scheme_PROTOCOL_TP1_ATTACHED_TARGET_PORT'),true);
  group.AddInput('PROTOCOL_TP1_GENERATION_CODE',GetTranslateableTextKey('scheme_PROTOCOL_TP1_GENERATION_CODE'),true);
  group.AddInput('PROTOCOL_TP1_INVALID_DWORD_COUNT',GetTranslateableTextKey('scheme_PROTOCOL_TP1_INVALID_DWORD_COUNT'),true);
  group.AddInput('PROTOCOL_TP1_LOSS_DWORD_SYNC',GetTranslateableTextKey('scheme_PROTOCOL_TP1_LOSS_DWORD_SYNC'),true);
  group.AddInput('PROTOCOL_TP1_NEG_LINK_RATE',GetTranslateableTextKey('scheme_PROTOCOL_TP1_NEG_LINK_RATE'),true);
  group.AddInput('PROTOCOL_TP1_NUMBER_PHYS',GetTranslateableTextKey('scheme_PROTOCOL_TP1_NUMBER_PHYS'),true);
  group.AddInput('PROTOCOL_TP1_PHY_IDENTIFIER',GetTranslateableTextKey('scheme_PROTOCOL_TP1_PHY_IDENTIFIER'),true);
  group.AddInput('PROTOCOL_TP1_PHY_INVALID_WORD_COUNT',GetTranslateableTextKey('scheme_PROTOCOL_TP1_PHY_INVALID_WORD_COUNT'),true);
  group.AddInput('PROTOCOL_TP1_PHY_LOSS_DWORD_SYNC_COUNT',GetTranslateableTextKey('scheme_PROTOCOL_TP1_PHY_LOSS_DWORD_SYNC_COUNT'),true);
  group.AddInput('PROTOCOL_TP1_PHY_RESET_PROBLEM',GetTranslateableTextKey('scheme_PROTOCOL_TP1_PHY_RESET_PROBLEM'),true);
  group.AddInput('PROTOCOL_TP1_PHY_RESET_PROBLEM_COUNT',GetTranslateableTextKey('scheme_PROTOCOL_TP1_PHY_RESET_PROBLEM_COUNT'),true);
  group.AddInput('PROTOCOL_TP1_REASON',GetTranslateableTextKey('scheme_PROTOCOL_TP1_REASON'),true);
  group.AddInput('PROTOCOL_TP1_RUNNING_DISPARITY_EC',GetTranslateableTextKey('scheme_PROTOCOL_TP1_RUNNING_DISPARITY_EC'),true);

  group:=scheme.AddInputGroup('log_tp2').Setup('scheme_tp2');

  group.AddInput('PROTOCOL_TP2_SAS_ADDRESS',GetTranslateableTextKey('scheme_PROTOCOL_TP2_SAS_ADDRESS'),true);
  group.AddInput('PROTOCOL_TP2_ATTACHED_DEVICE_TYPE',GetTranslateableTextKey('scheme_PROTOCOL_TP2_ATTACHED_DEVICE_TYPE'),true);
  group.AddInput('PROTOCOL_TP2_ATTACHED_INITIATOR_PORT',GetTranslateableTextKey('scheme_PROTOCOL_TP2_ATTACHED_INITIATOR_PORT'),true);
  group.AddInput('PROTOCOL_TP2_ATTACHED_PHY_IDENTIFIER',GetTranslateableTextKey('scheme_PROTOCOL_TP2_ATTACHED_PHY_IDENTIFIER'),true);
  group.AddInput('PROTOCOL_TP2_ATTACHED_REASON',GetTranslateableTextKey('scheme_PROTOCOL_TP2_ATTACHED_REASON'),true);
  group.AddInput('PROTOCOL_TP2_ATTACHED_SAS_ADDRESS',GetTranslateableTextKey('scheme_PROTOCOL_TP2_ATTACHED_SAS_ADDRESS'),true);
  group.AddInput('PROTOCOL_TP2_ATTACHED_TARGET_PORT',GetTranslateableTextKey('scheme_PROTOCOL_TP2_ATTACHED_TARGET_PORT'),true);
  group.AddInput('PROTOCOL_TP2_GENERATION_CODE',GetTranslateableTextKey('scheme_PROTOCOL_TP2_GENERATION_CODE'),true);
  group.AddInput('PROTOCOL_TP2_INVALID_DWORD_COUNT',GetTranslateableTextKey('scheme_PROTOCOL_TP2_INVALID_DWORD_COUNT'),true);
  group.AddInput('PROTOCOL_TP2_LOSS_DWORD_SYNC',GetTranslateableTextKey('scheme_PROTOCOL_TP2_LOSS_DWORD_SYNC'),true);
  group.AddInput('PROTOCOL_TP2_NEG_LINK_RATE',GetTranslateableTextKey('scheme_PROTOCOL_TP2_NEG_LINK_RATE'),true);
  group.AddInput('PROTOCOL_TP2_NUMBER_PHYS',GetTranslateableTextKey('scheme_PROTOCOL_TP2_NUMBER_PHYS'),true);
  group.AddInput('PROTOCOL_TP2_PHY_IDENTIFIER',GetTranslateableTextKey('scheme_PROTOCOL_TP2_PHY_IDENTIFIER'),true);
  group.AddInput('PROTOCOL_TP2_PHY_INVALID_WORD_COUNT',GetTranslateableTextKey('scheme_PROTOCOL_TP2_PHY_INVALID_WORD_COUNT'),true);
  group.AddInput('PROTOCOL_TP2_PHY_LOSS_DWORD_SYNC_COUNT',GetTranslateableTextKey('scheme_PROTOCOL_TP2_PHY_LOSS_DWORD_SYNC_COUNT'),true);
  group.AddInput('PROTOCOL_TP2_PHY_RESET_PROBLEM',GetTranslateableTextKey('scheme_PROTOCOL_TP2_PHY_RESET_PROBLEM'),true);
  group.AddInput('PROTOCOL_TP2_PHY_RESET_PROBLEM_COUNT',GetTranslateableTextKey('scheme_PROTOCOL_TP2_PHY_RESET_PROBLEM_COUNT'),true);
  group.AddInput('PROTOCOL_TP2_REASON',GetTranslateableTextKey('scheme_PROTOCOL_TP2_REASON'),true);
  group.AddInput('PROTOCOL_TP2_RUNNING_DISPARITY_EC',GetTranslateableTextKey('scheme_PROTOCOL_TP2_RUNNING_DISPARITY_EC'),true);

end;

class procedure TFRE_DB_SG_LOGS.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';


    StoreTranslateableText(conn,'scheme_common','General Information');
    StoreTranslateableText(conn,'scheme_NON_MEDIUM_ERROR_COUNT','Non Medium Error Count');
    StoreTranslateableText(conn,'scheme_TEMP_REFERENCE_TEMP','Reference Temperature [C]');
    StoreTranslateableText(conn,'scheme_TEMP_CURRENT_TEMP','Current Temperature [C]');

    StoreTranslateableText(conn,'scheme_ec','Error Correction Information');
    StoreTranslateableText(conn,'scheme_EC_READ_EC_POSSIBLE_DELAYS','Read Errors corrected with possible delays');
    StoreTranslateableText(conn,'scheme_EC_READ_EC_SUBSTANTIAL_DELAY','Read Errors corrected without substantial delay');
    StoreTranslateableText(conn,'scheme_EC_READ_TOTAL_BYTES_PROCESSED','Read Total bytes processed');
    StoreTranslateableText(conn,'scheme_EC_READ_TOTAL_ERRORS_CORRECTED','Read Total errors corrected');
    StoreTranslateableText(conn,'scheme_EC_READ_TOTAL_REWRITES_OR_REREADS','Read Total rewrites or rereads');
    StoreTranslateableText(conn,'scheme_EC_READ_TOTAL_TIMES_CORR_ALGO_PROC','Read Total times correction algorithm processed');
    StoreTranslateableText(conn,'scheme_EC_READ_TOTAL_UNCORRECTED_ERRORS','Read Total uncorrected errors');

    StoreTranslateableText(conn,'scheme_EC_WRITE_EC_POSSIBLE_DELAYS','Write Errors corrected with possible delays');
    StoreTranslateableText(conn,'scheme_EC_WRITE_EC_SUBSTANTIAL_DELAY','Write Errors corrected without substantial delay');
    StoreTranslateableText(conn,'scheme_EC_WRITE_TOTAL_BYTES_PROCESSED','Write Total bytes processed');
    StoreTranslateableText(conn,'scheme_EC_WRITE_TOTAL_ERRORS_CORRECTED','Write Total errors corrected');
    StoreTranslateableText(conn,'scheme_EC_WRITE_TOTAL_REWRITES_OR_REREADS','Write Total rewrites or rereads');
    StoreTranslateableText(conn,'scheme_EC_WRITE_TOTAL_TIMES_CORR_ALGO_PROC','Write Total times correction algorithm processed');
    StoreTranslateableText(conn,'scheme_EC_WRITE_TOTAL_UNCORRECTED_ERRORS','Write Total uncorrected errors');

    StoreTranslateableText(conn,'scheme_EC_VERIFY_EC_POSSIBLE_DELAYS','Verify Errors corrected with possible delays');
    StoreTranslateableText(conn,'scheme_EC_VERIFY_EC_SUBSTANTIAL_DELAY','Verify Errors corrected without substantial delay');
    StoreTranslateableText(conn,'scheme_EC_VERIFY_TOTAL_BYTES_PROCESSED','Verify Total bytes processed');
    StoreTranslateableText(conn,'scheme_EC_VERIFY_TOTAL_ERRORS_CORRECTED','VerifyTotal errors corrected');
    StoreTranslateableText(conn,'scheme_EC_VERIFY_TOTAL_REWRITES_OR_REREADS','Verify Total rewrites or rereads');
    StoreTranslateableText(conn,'scheme_EC_VERIFY_TOTAL_TIMES_CORR_ALGO_PROC','Verify Total times correction algorithm processed');
    StoreTranslateableText(conn,'scheme_EC_VERIFY_TOTAL_UNCORRECTED_ERRORS','Verify Total uncorrected errors');

    StoreTranslateableText(conn,'scheme_tp1','Target Port 1 Information');

    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_SAS_ADDRESS','SAS address');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_ATTACHED_DEVICE_TYPE','Attached device type');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_ATTACHED_INITIATOR_PORT','Attached initiator port');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_ATTACHED_PHY_IDENTIFIER','Attached phy identifier');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_ATTACHED_REASON','Attached reason');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_ATTACHED_SAS_ADDRESS','Attached SAS address');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_ATTACHED_TARGET_PORT','Attached Target Port');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_GENERATION_CODE','Generation Code');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_INVALID_DWORD_COUNT','Invalid DWORD count');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_LOSS_DWORD_SYNC','Loss of DWORD synchronization');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_NEG_LINK_RATE','Negotiated logical link rate');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_NUMBER_PHYS','Number of phys');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_PHY_IDENTIFIER','Phy identifier');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_PHY_INVALID_WORD_COUNT','Phy Invalid word count');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_PHY_LOSS_DWORD_SYNC_COUNT','Phy Loss of dword synchronization count');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_PHY_RESET_PROBLEM','Phy reset problem');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_PHY_RESET_PROBLEM_COUNT','Phy reset problem count');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_REASON','Reason');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP1_RUNNING_DISPARITY_EC','Running disparity error count');

    StoreTranslateableText(conn,'scheme_tp2','Target Port 2 Information');

    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_SAS_ADDRESS','SAS address');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_ATTACHED_DEVICE_TYPE','Attached device type');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_ATTACHED_INITIATOR_PORT','Attached initiator port');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_ATTACHED_PHY_IDENTIFIER','Attached phy identifier');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_ATTACHED_REASON','Attached reason');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_ATTACHED_SAS_ADDRESS','Attached SAS address');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_ATTACHED_TARGET_PORT','Attached Target Port');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_GENERATION_CODE','Generation Code');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_INVALID_DWORD_COUNT','Invalid DWORD count');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_LOSS_DWORD_SYNC','Loss of DWORD synchronization');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_NEG_LINK_RATE','Negotiated logical link rate');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_NUMBER_PHYS','Number of phys');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_PHY_IDENTIFIER','Phy identifier');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_PHY_INVALID_WORD_COUNT','Phy Invalid word count');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_PHY_LOSS_DWORD_SYNC_COUNT','Phy Loss of dword synchronization count');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_PHY_RESET_PROBLEM','Phy reset problem');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_PHY_RESET_PROBLEM_COUNT','Phy reset problem count');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_REASON','Reason');
    StoreTranslateableText(conn,'scheme_PROTOCOL_TP2_RUNNING_DISPARITY_EC','Running disparity error count');

  end;

end;

function TFRE_DB_SG_LOGS.WEB_GetDefaultCollection(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  result:=GFRE_DBI.NewObject;
  result.Field('collection').asstring:=CFRE_DB_SG_LOGS_COLLECTION;
end;

{ TFRE_DB_SATA_DISK }

procedure TFRE_DB_SATA_DISK.SetTargetPort(const targetport_1: string);
begin
  Field('target_port').AsString:=targetport_1;
end;

class procedure TFRE_DB_SATA_DISK.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';

  if (currentVersionId='') then begin
    currentVersionId := '1.0';

    StoreTranslateableText(conn,'scheme_main','General Information');
    StoreTranslateableText(conn,'scheme_deviceidentifier','Device Identifier');
    StoreTranslateableText(conn,'scheme_manufacturer','Manufacturer');
    StoreTranslateableText(conn,'scheme_model_number','Model Number');
    StoreTranslateableText(conn,'scheme_serial_number','Serial Number');
    StoreTranslateableText(conn,'scheme_fw_revision','Firmware');
    StoreTranslateableText(conn,'scheme_devicename','Devicename');
    StoreTranslateableText(conn,'scheme_log_common','General Log Information');
    StoreTranslateableText(conn,'scheme_log_ec','Error Correction Information');
    StoreTranslateableText(conn,'scheme_log_tp1','Target Port 1 Information');
    StoreTranslateableText(conn,'scheme_log_tp2','Target Port 2 Information');

  end;
  if (currentVersionId='1.0') then begin
  //next update code
  end;


end;

{ TFRE_DB_ENCLOSURE }

function TFRE_DB_ENCLOSURE.getDeviceIdentifier: TFRE_DB_String;
begin
  result := Field('deviceIdentifier').AsString;
end;

function TFRE_DB_ENCLOSURE.GetDriveSlots: UInt16;
begin
  if FieldExists('drive_slots') then
    Result:=Field('drive_slots').AsUInt16
  else
    result:=0;
end;

function TFRE_DB_ENCLOSURE.GetEnclosureNr: Uint16;
begin
  if FieldExists('enclosurenr') then
    Result:=Field('enclosurenr').AsUInt16
  else
    result:=0;
end;

function TFRE_DB_ENCLOSURE.GetMachineID: TFRE_DB_GUID;
begin
  result:=Field('machineid').AsObjectLink;
end;

function TFRE_DB_ENCLOSURE.GetParentInEnclosureUID: TFRE_DB_GUID;
begin
  Result:=Field('parent_in_enclosure_uid').AsObjectLink;
end;

procedure TFRE_DB_ENCLOSURE.setDeviceIdentifier(AValue: TFRE_DB_String);
begin
  Field('deviceIdentifier').AsString := AValue;
end;

procedure TFRE_DB_ENCLOSURE.SetDriveSlots(AValue: UInt16);
begin
  field('drive_slots').AsUInt16 := avalue;
end;

procedure TFRE_DB_ENCLOSURE.SetEnclosureNr(AValue: Uint16);
begin
  Field('enclosurenr').AsUInt16 := AValue;
end;

procedure TFRE_DB_ENCLOSURE.SetMachineID(AValue: TFRE_DB_GUID);
begin
  Field('machineid').AsObjectLink :=Avalue;
end;

procedure TFRE_DB_ENCLOSURE.SetParentInEnclosureUID(AValue: TFRE_DB_GUID);
begin
  Field('parent_in_enclosure_uid').AsObjectLink:=Avalue;
end;

procedure TFRE_DB_ENCLOSURE.SetMOSStatus(const status: TFRE_DB_MOS_STATUS_TYPE; const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION);
begin
  GFRE_MOS_SetMOSStatusandUpdate(self,status,input,ses,app,conn);
end;

function TFRE_DB_ENCLOSURE.GetMOSStatus: TFRE_DB_MOS_STATUS_TYPE;
begin
  Result:=String2DBMOSStatus(Field('status_mos').AsString);
end;

class procedure TFRE_DB_ENCLOSURE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';

  if (currentVersionId='') then begin
    currentVersionId := '1.0';

    StoreTranslateableText(conn,'scheme_enclosure','General Information');
    StoreTranslateableText(conn,'scheme__deviceidentifier','Enclosure Identifier');
    StoreTranslateableText(conn,'scheme_enclosure_nr','Enclosure Nr');
    StoreTranslateableText(conn,'scheme_drive_slots','Drive Slots');

  end;
  if (currentVersionId='1.0') then begin
  //next update code
  end;


end;

class procedure TFRE_DB_ENCLOSURE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.Classname);

  scheme.AddSchemeField('deviceIdentifier',fdbft_String);
  scheme.AddSchemeField('drive_slots',fdbft_UInt16);
  scheme.AddSchemeField('enclosurenr',fdbft_UInt16);

  scheme.AddCalcSchemeField('children',fdbft_String,@_getChildrenString);
  scheme.AddCalcSchemeField('caption_layout',fdbft_String,@_getLayoutCaption);
  scheme.AddCalcSchemeField('subcaption_layout',fdbft_String,@_getLayoutSubcaption);
  scheme.AddCalcSchemeField('icon_layout',fdbft_String,@_getLayoutIcon);
  scheme.AddCalcSchemeField('caption_mos',fdbft_String,@_getLayoutCaption);
  scheme.AddCalcSchemeField('status_icon_mos',fdbft_String,@_getStatusIcon);

  group:=scheme.AddInputGroup('enclosure').Setup(GetTranslateableTextKey('scheme_enclosure'));
  group.AddInput('deviceidentifier',GetTranslateableTextKey('scheme__deviceidentifier'),true);
  group.AddInput('enclosurenr',GetTranslateableTextKey('scheme_enclosure_nr'),true);
  group.AddInput('drive_slots',GetTranslateableTextKey('scheme_drive_slots'),true);
end;

procedure TFRE_DB_ENCLOSURE._getChildrenString(const calcfieldsetter: IFRE_DB_CALCFIELD_SETTER);
begin
  calcfieldsetter.SetAsString('UNCHECKED');
end;

procedure TFRE_DB_ENCLOSURE._getLayoutCaption(const calcfieldsetter: IFRE_DB_CALCFIELD_SETTER);
begin
  calcfieldsetter.SetAsString('Enclosure '+inttostr(EnclosureNr)+' '+Field('subcaption_layout').asstring);
end;

procedure TFRE_DB_ENCLOSURE._getLayoutSubcaption(const calcfieldsetter: IFRE_DB_CALCFIELD_SETTER);
begin
  calcfieldsetter.SetAsString('ID:'+DeviceIdentifier+' Slots:'+inttostr(DriveSlots));
end;

procedure TFRE_DB_ENCLOSURE._getLayoutIcon(const calcfieldsetter: IFRE_DB_CALCFIELD_SETTER);
begin
  calcfieldsetter.SetAsString(FREDB_getThemedResource('images_apps/firmbox_storage/'+ClassName+'.png'));
end;

procedure TFRE_DB_ENCLOSURE._getStatusIcon(const calc: IFRE_DB_CALCFIELD_SETTER);
begin
  GFRE_MOS_GetStatusIcon(GetMOSStatus,calc);
end;

procedure TFRE_DB_ENCLOSURE.AddExpanderEmbedded(const expander: TFRE_DB_SAS_Expander;const devicename:string);
begin
  if devicename<>'' then
    expander.DeviceIdentifier:=DeviceIdentifier+'_'+devicename;
  if not FieldExists('expanders') then
    Field('expanders').AsObject:= GFRE_DBI.NewObject;
  Field('expanders').asObject.Field(expander.DeviceIdentifier).AsObject:=expander;
end;

function TFRE_DB_ENCLOSURE.GetDriveSlotEmbedded(const slotnr: UInt16; out driveslot: TFRE_DB_DRIVESLOT): boolean;
begin
  if not FieldExists('slots') then
    begin
      driveslot:=nil;
      exit(false);
    end;
  result:=Field('slots').asObject.FieldExists('slot'+inttostr(slotnr));
  if result then
    driveslot := Field('slots').AsObject.Field('slot'+inttostr(slotnr)).AsObject.Implementor_HC as TFRE_DB_DRIVESLOT
  else
    driveslot := nil;
end;

procedure TFRE_DB_ENCLOSURE.AddDriveSlotEmbedded(const slotnr: UInt16; const driveslot: TFRE_DB_DRIVESLOT);
begin
  driveslot.DeviceIdentifier:=DeviceIdentifier+'_slot'+inttostr(slotnr);
  if not FieldExists('slots') then
    Field('slots').AsObject:= GFRE_DBI.NewObject;
  driveslot.SlotNr       := slotnr;
  driveslot.EnclosureNr  := EnclosureNr;
  Field('slots').AsObject.Field('slot'+inttostr(slotnr)).AsObject:=driveslot;
end;

procedure TFRE_DB_ENCLOSURE.DeleteReferencingToMe(const conn: IFRE_DB_CONNECTION);
var refs: TFRE_DB_ObjectReferences;
       i: NativeInt;
    obj : IFRE_DB_Object;
begin
  refs := conn.GetReferencesDetailed(Uid,false);
  for i:=0 to high(refs) do
    begin
      writeln('SWL DB ENCL REF:',refs[i].schemename);
      if refs[i].schemename=TFRE_DB_DRIVESLOT.Classname then
        begin
          CheckDbResult(conn.Fetch(refs[i].linked_uid,obj),'could not fetch driveslot for delete referencing ['+FREDB_G2H(refs[i].linked_uid));
          (obj.Implementor_HC as TFRE_DB_DRIVESLOT).RemoveLinkToMe(conn);
          obj.Finalize;
        end;
      CheckDbResult(conn.Delete(refs[i].linked_uid),'could not delete enclosure refs uid ['+FREDB_G2H(refs[i].linked_uid)+'] scheme ['+refs[i].schemename+']');
    end;
end;

procedure TFRE_DB_ENCLOSURE.EmbedSlotsAndExpanders(const conn: IFRE_DB_CONNECTION);
var refs      : TFRE_DB_ObjectReferences;
    i         : NativeInt;
    obj       : IFRE_DB_Object;
    lexpander : TFRE_DB_SAS_EXPANDER;
    lslot     : TFRE_DB_DRIVESLOT;
begin
  refs := conn.GetReferencesDetailed(UID,false);
  for i:=0 to high(refs) do
    begin
      CheckDbResult(conn.Fetch(refs[i].linked_uid,obj),' could not fetch referencing object '+FREDB_G2H(refs[i].linked_uid));
      if obj.IsA(TFRE_DB_SAS_EXPANDER,lexpander) then
        begin
          AddExpanderEmbedded(lexpander);
        end
      else if obj.IsA(TFRE_DB_DRIVESLOT,lslot) then
        begin
          AddDriveSlotEmbedded(lslot.SlotNr,lslot);
        end
      else
        obj.Finalize;
    end;
end;

procedure TFRE_DB_ENCLOSURE.AddMosParentID(const avalue: TFRE_DB_GUID);
begin
  if FREDB_GuidInArray(AValue,Field('mosparentIds').AsObjectLinkArray)=-1 then
    begin
      Field('mosparentIds').AddObjectLink(AValue);
    end;
  if FREDB_GuidInArray(AValue,Field('serviceparent').AsObjectLinkArray)=-1 then
    begin
      Field('serviceparent').AddObjectLink(AValue);
    end;
end;

procedure TFRE_DB_ENCLOSURE.RemoveMosParentID(const avalue: TFRE_DB_GUID);
var lp:NativeInt;
begin
  lp := FREDB_GuidInArray(AValue,Field('mosparentIds').AsObjectLinkArray);
  if lp>=0 then
    begin
      Field('mosparentIds').RemoveObjectLink(lp);
    end;
  lp := FREDB_GuidInArray(AValue,Field('serviceparent').AsObjectLinkArray);
  if lp>=0 then
    begin
      Field('serviceparent').RemoveObjectLink(lp);
    end;
end;

function TFRE_DB_ENCLOSURE.WEB_MOSContent(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  panel         : TFRE_DB_FORM_PANEL_DESC;
  scheme        : IFRE_DB_SchemeObject;
begin
  GFRE_DBI.GetSystemSchemeByName(SchemeClass,scheme);
  panel :=TFRE_DB_FORM_PANEL_DESC.Create.Describe(app.FetchAppTextShort(ses,'enclosure_content_header'));
  panel.AddSchemeFormGroup(scheme.GetInputGroup('enclosure'),GetSession(input));
  panel.FillWithObjectValues(self,GetSession(input));
  Result:=panel;
end;

function TFRE_DB_ENCLOSURE.WEB_MOSChildStatusChanged(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  SetMOSStatus(GFRE_MOS_MOSChildStatusChanged(UID,input,ses,app,conn),input,ses,app,conn);
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_ENCLOSURE.WEB_MOSStatus(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=GFRE_DBI.NewObject;
  Result.Field('status_mos').AsString:=Field('status_mos').AsString;
end;

function TFRE_DB_ENCLOSURE.WEB_GetDefaultCollection(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  result:=GFRE_DBI.NewObject;
  result.Field('collection').asstring:=CFRE_DB_ENCLOSURE_COLLECTION;
end;

function TFRE_DB_SCSI._SG3GetPage(const cmd: string; out output: string; out error: string; const params: TFRE_DB_StringArray): integer;
var   proc        : TFRE_DB_Process;
begin
  ClearProcess;
  proc := TFRE_DB_Process.create;
  proc.SetupInput(cmd,params);
  AddProcess(proc);
  ExecuteMulti;
  output := proc.OutputToString;
  error  := proc.ErrorToString;
  result := proc.Field('exitstatus').AsInt32;
  ClearProcess;
end;

function TFRE_DB_SCSI._FileExistsLocalorRemote(const filename: string): boolean;
var   proc        : TFRE_DB_Process;
begin
  if FieldExists('remotehost') then
    begin
      ClearProcess;
      proc := TFRE_DB_Process.create;
      proc.SetupInput('test',TFRE_DB_StringArray.Create ('-e',filename));
      AddProcess(proc);
      ExecuteMulti;
      result := proc.Field('exitstatus').AsInt32=0;
   end
  else
    result := FileExists(filename);
end;


function TFRE_DB_SCSI.getrdskDevices(const iostat_devices: IFRE_DB_Object): IFRE_DB_Object;

  procedure _getDevices(const obj:IFRE_DB_Object);
  begin
    result.Field('device').AddString('/dev/rdsk/'+obj.Field('iodevicename').asstring+'s2');
  end;

begin
  result := GFRE_DBI.NewObject;
  iostat_devices.ForAllObjects(@_getdevices);
end;

function TFRE_DB_SCSI.getexpanderDevices: IFRE_DB_Object;
var sl : TStringList;
    i  : NativeInt;
    proc  : TFRE_DB_Process;
begin
//  if FieldExists('remotehost') then
//    begin
      result := GFRE_DBI.NewObject;
      ClearProcess;
      proc := TFRE_DB_Process.create;
      try
        proc.SetupInput('find',TFRE_DB_StringArray.Create ('/dev/es'));
        AddProcess(proc);
        ExecuteMulti;
        sl := TStringList.Create;
        try
          sl.Text:=TFRE_DB_Process (proc.Implementor_HC).OutputToString;
          for i:= 1 to sl.count-1 do
            begin
              result.Field('device').AddString(sl[i]);
            end;
        finally
          sl.Free;
        end;
      finally
        ClearProcess;
      end;
//    end
//  else
//    abort; //Implement local Find for devices
end;

function TFRE_DB_SCSI.SG3DiskInquiry(const devicepath: TFRE_DB_String; out disk: IFRE_DB_Object; const read_logs: boolean): integer;

type
  TFRE_DB_SCSI_DEVICEIDENTIFICATION_PARSE = (di_none,di_address_loc,di_target);
var
  sl          : TStringList;
  i           : NativeInt;
  s           : string;
  porti       : NativeInt;
  ports       : Array[1..2] of string;
  di_parse    : TFRE_DB_SCSI_DEVICEIDENTIFICATION_PARSE;
  devicename  : string;
  res         : integer;
  outstring   : string;
  errstring   : string;
  undefineddisk : TFRE_DB_UNDEFINED_BLOCKDEVICE;
  blocks_s    : string;
  blocksize_s : string;
  vendor      : string;
  model_number: string;
  fw_revision : string;
  serialnr    : string;
  devicetype  : string;


  procedure _ParseSG_Logs(const disk: IFRE_DB_Object; const parsestring:string);
  type

      TSG3LogPage = (sglNone, sglECwrite,sglECRead,sglECVerify,sglNME,sglTemp,sglStartStop,sglAppClient,sglSelftest,sglBGScan,sglProtocol,sglGeneral,sglPowerCond,sglInfoExceptions,sglCacheMisc,sgl0x38,sglFactory);

  const
      CSG3LogPageHex         : Array[TSG3LogPage] of String = ('','0x2','0x3','0x5','0x6','0xd','0xe','0xf','0x10','0x15','0x18','0x19','0x1a','0x2f','0x37','0x38','0x3e');
      CSG3LogPageFieldPrefix : Array[TSG3LogPage] of String = ('','ec_write','ec_read','ec_verify','non_medium','temp','startstop','appclient','selftest','bgscan','protocol','general','powertr','informalex','cachemisc','x38','factory');


      //0x00        Supported log pages        //SEAGATE
      //0x02        Error counters (write)
      //0x03        Error counters (read)
      //0x05        Error counters (verify)
      //0x06        Non-medium errors
      //0x0d        Temperature
      //0x0e        Start-stop cycle counter
      //0x0f        Application client
      //0x10        Self-test results
      //0x15        Background scan results (sbc-3)
      //0x18        Protocol specific port
      //0x1a        Power condition transition
      //0x2f        Informational exceptions (SMART)
      //0x37        Cache (Seagate), Miscellaneous (Hitachi)
      //0x38        [unknown vendor specific page code]
      //0x3e        Factory (Seagate/Hitachi)

      //0x00        Supported log pages       //ZEOS RAM
      //0x02        Error counters (write)
      //0x03        Error counters (read)
      //0x05        Error counters (verify)
      //0x06        Non-medium errors
      //0x0d        Temperature
      //0x10        Self-test results
      //0x18        Protocol specific port
      //0x19        General statistics and performance
      //0x2f        Informational exceptions (SMART)
      //0x30        Performance counters (Hitachi)
      //0x31        [unknown vendor specific page code]
      //0x32        [unknown vendor specific page code]
      //0x33        [unknown vendor specific page code]
      //0x34        [unknown vendor specific page code]
      //0x37        Cache (Seagate), Miscellaneous (Hitachi)

  var sl        : TStringList;
      i         : NativeInt;
      logdbo    : IFRE_DB_Object;
      parsepage : TSG3LogPage;
      line      : string;
      targetport: NativeInt;

  procedure _CheckParsemode;
  var
      ipm       : TSG3LogPage;
  begin
    for ipm := low(TSG3LogPage) to high(TSG3LogPage) do begin
      if Pos('['+CSG3LogPageHex[ipm]+']',line)>0 then
        begin
          parsepage := ipm;
          break;
        end;
    end;
  end;

  function _getValue(const idstring:string; const fieldname:string; const fieldtype:TFRE_DB_FIELDTYPE; const split_value_unit:boolean=false; const separator:string='='):boolean;
  var svalue     : string;
      ffieldname : string;
  begin
    result := false;
    if Pos(idstring,line)>0 then
      begin

        svalue := trim(GFRE_BT.SepRight(line,separator));
        if split_value_unit then
          svalue := GFRE_BT.SepLeft(svalue,' ');
        if parsepage=sglProtocol then
          ffieldname := CSG3LogPageFieldPrefix[parsepage]+'_TP'+inttostr(targetport)+'_'+fieldname
        else
          ffieldname := CSG3LogPageFieldPrefix[parsepage]+'_'+fieldname;
        case fieldtype of
          fdbft_String : logdbo.Field(ffieldname).AsString := svalue;
          fdbft_Int16  : logdbo.Field(ffieldname).AsInt16  := StrToIntDef(svalue,-1);
          fdbft_Int64  : logdbo.Field(ffieldname).AsInt64  := StrToInt64Def(svalue,-1);
          fdbft_Real32 : logdbo.Field(ffieldname).AsReal32 := StrToFloatDef(svalue,-1,DefaultFormatSettings);
        else
          GFRE_DBI.LogError(dblc_APPLICATION,'SG3 sg_logs undefined getValue for %s',[CFRE_DB_FIELDTYPE [fieldtype]]);
        end;
        result := true;
      end;
  end;

  begin
    if not disk.FieldExists('log') then
      begin
       logdbo := TFRE_DB_SG_LOGS.CreateForDB;
       disk.Field('log').AsObject := logdbo;
      end
    else
      logdbo:=disk.Field('log').AsObject;

    sl  := TStringList.Create;
    try
      sl.Text   := outstring;
      parsepage := sglNone;
      for i:=0 to sl.count-1 do
        begin
          line := sl[i];
          _CheckParseMode;
          case parsepage of
            sglNone : continue;
            sglECwrite :
              begin
                if _GetValue('Errors corrected without substantial delay','ec_substantial_delay',fdbft_Int64) then continue;      //ZEOS
                if _GetValue('Errors corrected with possible delays','ec_possible_delays',fdbft_Int64) then continue;
                if _GetValue('Total rewrites or rereads','total_rewrites_or_rereads',fdbft_Int64) then continue;
                if _GetValue('Total errors corrected','total_errors_corrected',fdbft_Int64) then continue;
                if _GetValue('Total times correction algorithm processed','total_times_corr_algo_processed',fdbft_Int64) then continue;
                if _GetValue('Total bytes processed','total_bytes_processed',fdbft_Int64) then continue;
                if _GetValue('Total uncorrected errors','total_uncorrected_errors',fdbft_Int64) then continue;
                if _GetValue('Reserved or vendor specific [0x8000]','vendor_0x8000',fdbft_Int64) then continue;    //ZEOS
                if _GetValue('Reserved or vendor specific [0x8001]','vendor_0x8001',fdbft_Int64) then continue;    //ZEOS
              end;
            sglECRead :
              begin
                if _GetValue('Errors corrected without substantial delay','ec_substantial_delay',fdbft_Int64) then continue;
                if _GetValue('Errors corrected with possible delays','ec_possible_delays',fdbft_Int64) then continue;
                if _GetValue('Total rewrites or rereads','total_rewrites_or_rereads',fdbft_Int64) then continue;
                if _GetValue('Total errors corrected','total_errors_corrected',fdbft_Int64) then continue;
                if _GetValue('Total times correction algorithm processed','total_times_corr_algo_processed',fdbft_Int64) then continue;
                if _GetValue('Total bytes processed','total_bytes_processed',fdbft_Int64) then continue;
                if _GetValue('Total uncorrected errors','total_uncorrected_errors',fdbft_Int64) then continue;
                if _GetValue('Reserved or vendor specific [0x8000]','vendor_0x8000',fdbft_Int64) then continue;    //ZEOS
              end;
            sglECVerify :
              begin
                if _GetValue('Errors corrected without substantial delay','ec_substantial_delay',fdbft_Int64) then continue;
                if _GetValue('Errors corrected with possible delays','ec_possible_delays',fdbft_Int64) then continue;
                if _GetValue('Total rewrites or rereads','total_rewrites_or_rereads',fdbft_Int64) then continue;
                if _GetValue('Total errors corrected','total_errors_corrected',fdbft_Int64) then continue;
                if _GetValue('Total times correction algorithm processed','total_times_corr_algo_processed',fdbft_Int64) then continue;
                if _GetValue('Total bytes processed','total_bytes_processed',fdbft_Int64) then continue;
                if _GetValue('Total uncorrected errors','total_uncorrected_errors',fdbft_Int64) then continue;
              end;
            sglNME :
              begin
                if _GetValue('Non-medium error count','error_count',fdbft_Int64) then continue;
              end;
            sglTemp :
              begin
                if _GetValue('Current temperature','current_temp',fdbft_Int16,true) then continue;
                if _GetValue('Reference temperature','reference_temp',fdbft_Int16,true) then continue;
              end;
            sglStartStop :
              begin
                if _GetValue('Date of manufacture','manufacture_year_week',fdbft_String,false,',') then continue;
                if _GetValue('Accounting date','accounting_year_week',fdbft_String,false,',') then continue;
                if _GetValue('Specified cycle count over device lifetime','cycle_count_lifetime',fdbft_Int64) then continue;
                if _GetValue('Accumulated start-stop cycles','accumulated_start_stop',fdbft_Int64) then continue;
                if _GetValue('Specified load-unload count over device lifetime','load_unload_lifetime',fdbft_Int64) then continue;
                if _GetValue('Accumulated load-unload cycles','accumulated_load_unload',fdbft_Int64) then continue;
              end;
            sglAppClient : continue; // no information
            sglSelftest : continue; // no information
            sglBGScan :
              begin
                if _GetValue('Accumulated power on minutes','accumulated_power_on_min',fdbft_Int64,true,':') then continue;
                if _GetValue('Status','status',fdbft_String,false,':') then continue;
                if _GetValue('Number of background scans performed','number_of_bg_scans',fdbft_Int64,false,':') then continue;
                if _GetValue('Background medium scan progress','progress',fdbft_String,false,':') then continue;
                if _GetValue('Number of background medium scans performed','number_of_bg_medium_scans',fdbft_Int64,false,':') then continue;
              end;
            sglProtocol :
              begin
                if Pos('relative target port id',line)>0 then
                  begin
                    targetport:=StrToIntDef(trim(GFRE_BT.SepRight(line,'=')),-1);
                    continue;
                  end;
                if _GetValue('generation code','generation_code',fdbft_Int16) then continue;
                if _GetValue('number of phys','number_phys',fdbft_Int16) then continue;
                if _GetValue('  phy identifier','phy_identifier',fdbft_Int16) then continue;    // space intentionally
                if _GetValue('attached device type','attached_device_type',fdbft_String,false,':') then continue;
                if _GetValue('attached reason','attached_reason',fdbft_String,false,':') then continue;
                if _GetValue('reason','reason',fdbft_String,false,':') then continue;
                if _GetValue('negotiated logical link rate','neg_link_rate',fdbft_String,false,':') then continue;
                if _GetValue('attached initiator port','attached_initiator_port',fdbft_String,false,':') then continue;
                if _GetValue('attached target port','attached_target_port',fdbft_String,false,':') then continue;
                if _GetValue('  SAS address','sas_address',fdbft_String) then continue;        // space intentionally
                if _GetValue('attached SAS address','attached_sas_address',fdbft_String) then continue;
                if _GetValue('attached phy identifier','attached_phy_identifier',fdbft_Int16) then continue;
                if _GetValue('Invalid DWORD count','invalid_dword_count',fdbft_Int64) then continue;
                if _GetValue('Running disparity error count =','running_disparity_ec',fdbft_Int64) then continue;  // = intenionally
                if _GetValue('Loss of DWORD synchronization','loss_dword_sync',fdbft_Int64) then continue;
                if _GetValue('Phy reset problem =','phy_reset_problem',fdbft_Int64) then continue;
                if _GetValue('Invalid word count','phy_invalid_word_count',fdbft_Int64,false,':') then continue;
                if _GetValue('Running disparity error count:','phy_invalid_word_count',fdbft_Int64,false,':') then continue; // : intentionally
                if _GetValue('Loss of dword synchronization count','phy_loss_dword_sync_count',fdbft_Int64,false,':') then continue;
                if _GetValue('Phy reset problem count','phy_reset_problem_count',fdbft_Int64,false,':') then continue;
              end;
            sglGeneral:
              begin
                if _GetValue('number of read commands =','read_commands',fdbft_Int64) then continue;
                if _GetValue('number of write commands =','write_commands',fdbft_Int64) then continue;
                if _GetValue('number of logical blocks received','logical_blocks_received',fdbft_Int64) then continue;
                if _GetValue('number of logical blocks transmitted','logical_blocks_transmitted',fdbft_Int64) then continue;
                if _GetValue('read command processing intervals','read_cmd_processing_intervals',fdbft_Int64) then continue;
                if _GetValue('write command processing intervals','write_cmd_processing_intervals',fdbft_Int64) then continue;
                if _GetValue('weighted number of read commands plus write commands','weighted_nr_reads_plus_write_cmds',fdbft_Int64) then continue;
                if _GetValue('weighted read command processing plus write command processing','weighted_reads_plus_write_cmd_proc',fdbft_Int64) then continue;
                if _GetValue('idle time intervals','idle_time_intervals',fdbft_Int64) then continue;
                if _GetValue('time interval negative exponent','time_interval_negative_exponent',fdbft_Int64) then continue;
                if _GetValue('time interval integer','time_interval_integer',fdbft_Int64) then continue;
              end;
            sglPowerCond :
              begin
                if _GetValue('Accumulated transitions to idle_a','accumulated_tr_idle_a',fdbft_Int64) then continue;
                if _GetValue('Accumulated transitions to idle_b','accumulated_tr_idle_b',fdbft_Int64) then continue;
                if _GetValue('Accumulated transitions to idle_c','accumulated_tr_idle_c',fdbft_Int64) then continue;
                if _GetValue('Accumulated transitions to standby_z','accumulated_tr_standby_z',fdbft_Int64) then continue;
                if _GetValue('Accumulated transitions to standby_y','accumulated_tr_standby_y',fdbft_Int64) then continue;
              end;
            sglInfoExceptions :
              begin
                if _GetValue('IE asc','ie',fdbft_string,false,'IE') then continue;
                if _GetValue('Current temperature','current_temp',fdbft_string,true) then continue;
                if _GetValue('Threshold temperature','threshold_temp',fdbft_string,true) then continue;
              end;
            sglCacheMisc :
              begin
                if _GetValue('Blocks sent to initiator','blocks_sent_initiator',fdbft_Int64) then continue;
                if _GetValue('Blocks received from initiator','blocks_received_initiator',fdbft_Int64) then continue;
                if _GetValue('Blocks read from cache and sent to initiator','blocks_read_cache_sent',fdbft_Int64) then continue;
                if _GetValue('Number of read and write commands whose size <= segment size','rw_commands_leq_segsize',fdbft_Int64,false,' =') then continue;
                if _GetValue('Number of read and write commands whose size > segment size','rw_commands_gr_segsize',fdbft_Int64) then continue;
              end;
            sgl0x38: continue; // hex, not decoded by sg_logs
            sglFactory :
              begin
                if _GetValue('number of hours powered up','number_hours_powered_up',fdbft_Real32) then continue;
                if _GetValue('number of minutes until next internal SMART test','min_next_smart_test',fdbft_Int64) then continue;
              end;
            else
              GFRE_DBI.LogError(dblc_APPLICATION,'SG3 sg_logs undefined parsing page for %s %s',[devicepath,CSG3LogPageHex[parsepage]]);
          end
        end;
    finally
      sl.Free;
    end;
  end;

begin
//  writeln(devicepath);
  devicename := Copy(devicepath,11,length(devicepath));
  devicename := Copy(devicename,1,length(devicename)-2);
//  writeln(devicename);

  GFRE_DBI.LogDebug(dblc_APPLICATION,'SG3DiskInquiry [%s]',[devicepath]);
  if not _FileExistsLocalorRemote(devicepath) then                      // devices from iostat without /dev/rdsk/*s2 entry
    begin
      GFRE_DBI.LogInfo(dblc_APPLICATION,'SG3DiskInquiry Undefined Blockdevice [%s]',[devicepath]);
      undefineddisk  := TFRE_DB_UNDEFINED_BLOCKDEVICE.CreateForDB;
      undefineddisk.DeviceName       := devicename;
      undefineddisk.DeviceIdentifier := devicename;
      disk := undefineddisk;
      exit(0);
    end;

  GFRE_DBI.LogDebug(dblc_APPLICATION,'SG3GetPage: <default> [%s]',[devicepath]);

  res := _SG3GetPage(cSG3Inq,outstring,errstring,TFRE_DB_StringArray.Create (devicepath));
  if res=0 then
    begin
      sl := TStringList.Create;
      try
        sl.Text := outstring;
        for i:=0 to sl.count-1 do
         begin
           s:= sl[i];
           if Pos('Vendor identification',s)>0 then
             vendor       := trim(GFRE_BT.SepRight(s,':'));
           if Pos('Product identification',s)>0 then
             model_number := trim(GFRE_BT.SepRight(s,':'));
           if Pos('Product revision level',s)>0 then
             fw_revision  := trim(GFRE_BT.SepRight(s,':'));
           if Pos('Unit serial number',s)>0 then
             serialnr     := trim(GFRE_BT.SepRight(s,':'));
           if Pos('Peripheral device type',s)>0 then
             devicetype   := trim(GFRE_BT.SepRight(s,':'));
         end;
      finally
        sl.Free;
      end;
    end
  else
    begin
      disk   := nil;
      GFRE_DBI.LogError(dblc_APPLICATION,'SG3 Inquire for %s failed with resultcode %d, %s',[devicepath,res,errstring]);
      exit(res);
    end;

   if devicetype<>'disk' then
     begin
       GFRE_DBI.LogInfo(dblc_APPLICATION,'SG3DiskInquiry Type of device [%s] is not disk [%s]',[devicetype,devicepath]);
       undefineddisk  := TFRE_DB_UNDEFINED_BLOCKDEVICE.CreateForDB;
       undefineddisk.DeviceName       := devicename;
       undefineddisk.DeviceIdentifier := devicename;
       disk := undefineddisk;
       exit(0);
     end;

  if vendor='ATA' then
    begin
      GFRE_DBI.LogDebug(dblc_APPLICATION,'Vendor of device is %s [%s]',[vendor, devicepath]);
      disk  := GFRE_DBI.NewObjectScheme(TFRE_DB_SATA_DISK)
    end
  else
    begin
      GFRE_DBI.LogDebug(dblc_APPLICATION,'SG3GetPage: 0x88 [%s]',[devicepath]);
      res := _SG3GetPage(cSG3Inq,outstring,errstring,TFRE_DB_StringArray.Create ('-p','0x88',devicepath));
      if res=0 then //SAS DISK
        begin
          disk  := GFRE_DBI.NewObjectScheme(TFRE_DB_SAS_DISK);
          porti := 0;
          sl := TStringList.Create;
          try
            sl.Text := outstring;
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
          // eg. vmware
          disk  := GFRE_DBI.NewObjectScheme(TFRE_DB_SAS_DISK);
          GFRE_DBI.LogInfo(dblc_APPLICATION,'SG3 Inquire for %s failed with resultcode %d, %s',[devicepath,res,errstring]);
          //exit(res);
        end;
    end;

  (disk.Implementor_HC as TFRE_DB_PHYS_DISK).Setmanufacturer(vendor);
  (disk.Implementor_HC as TFRE_DB_PHYS_DISK).Setmodel_number(model_number);
  (disk.Implementor_HC as TFRE_DB_PHYS_DISK).Setfw_revision(fw_revision);
  (disk.Implementor_HC as TFRE_DB_PHYS_DISK).Setserial_number(serialnr);

  // get wwn
  GFRE_DBI.LogDebug(dblc_APPLICATION,'SG3GetPage: di [%s]',[devicepath]);
  res := _SG3GetPage(cSG3Inq,outstring,errstring,TFRE_DB_StringArray.Create ('-p','di',devicepath));
  if res=0 then
    begin
      di_parse:=di_none;
      sl := TStringList.Create;
      try
        sl.Text := outstring;
        for i:=0 to sl.count-1 do
         begin
           s:= sl[i];
           if Pos('associated with the addressed logical unit',s)>0 then
             di_parse:=di_address_loc;
           if Pos('associated with the target port',s)>0 then
             di_parse:=di_target;
           if Pos('Vendor Specific Identifier:',s)>0 then
             begin
               case di_parse of
                 di_address_loc:
                   begin
                     s := sl[i+1];
                     (disk.Implementor_HC as TFRE_DB_PHYS_DISK).SetWWN(GFRE_BT.SepLeft(GFRE_BT.SepRight(s,'['),']'));
                   end;
                 di_target:
                   begin
                     if (disk.Implementor_HC is TFRE_DB_SATA_DISK) then
                       begin
                         s := sl[i+1];
                         (disk.Implementor_HC as TFRE_DB_SATA_DISK).SetTargetPort(GFRE_BT.SepLeft(GFRE_BT.SepRight(s,'['),']'));
                       end;
                   end;
               end;  //ignore others
               di_parse:=di_none;
             end;
         end;
      finally
        sl.Free;
      end;
    end
  else
    GFRE_DBI.LogError(dblc_APPLICATION,'SG3 Inquire for %s failed with resultcode %d, %s',[devicepath,res,errstring]);



  (disk.Implementor_HC as TFRE_DB_PHYS_DISK).DeviceName := devicename;

  if (disk.Implementor_HC as TFRE_DB_PHYS_DISK).DeviceIdentifier='' then          // no WWN
    (disk.Implementor_HC as TFRE_DB_PHYS_DISK).DeviceIdentifier:=devicename;

  GFRE_DBI.LogDebug(dblc_APPLICATION,'SG3GetPage: cap [%s]',[devicepath]);

  res := _SG3GetPage(cSG3cap,outstring,errstring,TFRE_DB_StringArray.Create ('-l','-b',devicepath));
  if res=0 then
    begin
      s:=outstring;
      blocks_s    := trim(GFRE_BT.SepLeft(s,' '));
      blocksize_s := trim(GFRE_BT.SepRight(s,' '));
      (disk.Implementor_HC as TFRE_DB_PHYS_DISK).SetSize_Sectors(StrToInt(blocks_s));
      (disk.Implementor_HC as TFRE_DB_PHYS_DISK).SetBlockSize(StrToInt(blocksize_s));
    end
  else
    begin
      if Assigned(disk) then
        disk.Finalize;
      disk   := nil;
      GFRE_DBI.LogError(dblc_APPLICATION,'SG3 Readcap for %s failed with resultcode %d, %s',[devicepath,res,errstring]);
      exit(res);
    end;

  if read_logs then begin
    GFRE_DBI.LogDebug(dblc_APPLICATION,'SG3GetPage: sg_logs [%s]',[devicepath]);
    for i := low(cSG3LogPages) to high(cSG3LogPages) do
      begin
//        res := _SG3GetPage(cSG3logs,outstring,errstring,TFRE_DB_StringArray.Create ('-a',devicepath));    // all pages
        res := _SG3GetPage(cSG3logs,outstring,errstring,TFRE_DB_StringArray.Create ('-p',cSG3LogPages[i],devicepath));
        if res=0 then
          begin
            _ParseSG_Logs(disk,outstring);
          end
        else
          begin
            GFRE_DBI.LogError(dblc_APPLICATION,'SG3 sg_logs for %s page %s failed with resultcode %d, %s',[devicepath,cSG3LogPages[i],res,errstring]);
          end;
      end;
  end;
  exit(0);
end;

function TFRE_DB_SCSI.SG3SESInquiry(const devicepath: TFRE_DB_String; var scsi_structure: IFRE_DB_Object): integer;
var
  sl             : TStringList;
  i              : NativeInt;
  s              : string;
  res            : integer;
  outstring      : string;
  errstring      : string;
  expander       : TFRE_DB_SAS_EXPANDER;
  enclosure      : TFRE_DB_ENCLOSURE;
  enclosure_id   : string;

  function _getOrCreateEnclosure(const enclosure_id:string):TFRE_DB_ENCLOSURE;
  var enclosures : IFRE_DB_Object;
      encnr      : UInt16;
  begin
    result     := nil;
    enclosures := scsi_structure.Field('enclosures').AsObject;
    if enclosures.FieldExists(enclosure_id) then
      exit(enclosures.Field(enclosure_id).AsObject.Implementor_HC as TFRE_DB_ENCLOSURE)
    else
      begin
        result := TFRE_DB_ENCLOSURE.Create;
        result.DeviceIdentifier:= enclosure_id;
        result.EnclosureNr     := enclosures.FieldCount(true,true);
//        writeln('ENCLOSURE NR:',result.EnclosureNr);
        enclosures.Field(enclosure_id).AsObject:=result;
      end;
  end;

  procedure _AnalyzeDriveSlotInformation(const proc:TFRE_DB_Process);
  var i            : NativeInt;
      outstring    : string;
      errstring    : string;
      driveslot    : TFRE_DB_DRIVESLOT;
      port_nr      : integer;
      slotnr       : integer;
  begin
    outstring   := proc.OutputToString;
    errstring   := proc.ErrorToString;
    slotnr      := proc.Field('slotnr').AsUInt16;
    res         := proc.Field('exitstatus').AsInt32;
    if res=0 then
      begin
        if enclosure.GetDriveSlotEmbedded(slotnr,driveslot) then
          port_nr  := 2
        else
          begin
            driveslot        := TFRE_DB_DRIVESLOT.Create;
            enclosure.AddDriveSlotEmbedded(slotnr,driveslot);
            port_nr := 1;
          end;

        sl := TStringList.Create;
        try
          sl.Text := outstring;
          for i:=1 to sl.count-1 do
           begin
             s:= sl[i];
             if Pos('target port for',s)>0 then
                 driveslot.PortType:=trim(GFRE_BT.SepRight(s,':'));
             if Pos('attached SAS address',s)>0 then
               begin
                 driveslot.SetAttachedTo(port_nr,trim(GFRE_BT.SepRight(s,':')));
                 s:=sl[i+1];
                 driveslot.SetTargetPort(port_nr,trim(GFRE_BT.SepRight(s,':')));
               end;
           end;
//          writeln(driveslot.DumpToString());
        finally
          sl.Free;
        end;
      end
    else
      begin
        GFRE_DBI.LogError(dblc_APPLICATION,'SG3 Ses for %s, Slot %d failed with resultcode %d, %s',[devicepath,slotnr,res,errstring]);
        exit;
      end;
  end;

  procedure _GetDriveSlotInformation(const slotnr:NativeInt);
  var
    proc         : TFRE_DB_Process;
  begin
    GFRE_DBI.LogDebug(dblc_APPLICATION,'SG3Ses: -I 0 [%d] -p aes [%s]',[slotnr,devicepath]);

    proc := TFRE_DB_Process.create;
    proc.SetupInput(cSG3Ses,TFRE_DB_StringArray.Create ('-I','0,'+inttostr(slotnr),'-p','aes',devicepath));
    proc.Field('slotnr').AsUInt16:=slotnr;
//    writeln('SWL:slot ',slotnr,' ',devicepath);
    AddProcess(proc);
  end;


begin

  // determine sas/sata, get target ports
//  writeln('SWL:',devicepath);
  GFRE_DBI.LogDebug(dblc_APPLICATION,'SG3GetPage: cf [%s]',[devicepath]);

  res := _SG3GetPage(cSG3Ses,outstring,errstring,TFRE_DB_StringArray.Create ('-p','cf',devicepath));
  if res=0 then
    begin
      sl := TStringList.Create;
      try
        sl.Text := outstring;
        expander := TFRE_DB_SAS_EXPANDER.create;
        for i:=1 to sl.count-1 do
         begin
           s:= sl[i];
           if Pos('enclosure logical identifier',s)>0 then
             begin
               enclosure_id := trim(GFRE_BT.SepRight(s,':'));
               enclosure:=_getOrCreateEnclosure(enclosure_id);
               enclosure.AddExpanderEmbedded(expander,Copy(devicepath,Pos('ses',devicepath),maxint));
             end;
           if Pos('enclosure vendor',s)>0 then
             begin
               s := GFRE_BT.SepRight(s,':');
               expander.Manufacturer:=trim(GFRE_BT.SepLeft(s,'product'));
               s := GFRE_BT.SepRight(s,':');
               expander.Model_number:=trim(GFRE_BT.SepLeft(s,'rev'));
               s := GFRE_BT.SepRight(s,':');
               expander.Fw_revision:=trim(s);
             end;
           if Pos('Array device slot',s)>0 then
             begin
               s:= sl[i+1];
               enclosure.DriveSlots:=strtoint(trim(GFRE_BT.SepRight(s,':')));
             end;
         end;
      finally
        sl.Free;
      end;
    end
  else
    begin
      GFRE_DBI.LogError(dblc_APPLICATION,'SG3 Ses for %s failed with resultcode %d, %s',[devicepath,res,errstring]);
      exit;
    end;
//  writeln('SWL:ENCLO',enclosure.DumpToString());
  if assigned(enclosure) then
    begin
      for i:=0 to enclosure.DriveSlots-1 do
        begin
          ClearProcess;
          _GetDriveSlotInformation(i);
          ExecuteMulti;
          _AnalyzeDriveSlotInformation(GetProcess(0));
          ClearProcess;
        end;
    end;
end;

class procedure TFRE_DB_SCSI.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;


function TFRE_DB_SCSI.GetSG3DiskandEnclosureInformation(const iostat_devices: IFRE_DB_Object; out error: string; out scsi_structure: IFRE_DB_Object; const read_logs: boolean): integer;
var devices : IFRE_DB_Object;
    disk    : IFRE_DB_Object;
    i       : NativeInt;
begin
//  writeln('IOStat Devices:',iostat_devices.DumpToString());
  try
    cSG3Inq := cFRE_ToolsPath+'/sg3utils/bin/sg_inq';
    cSG3Ses := cFRE_ToolsPath+'/sg3utils/bin/sg_ses';
    cSG3cap := cFRE_ToolsPath+'/sg3utils/bin/sg_readcap';
    cSG3logs:= cFRE_ToolsPath+'/sg3utils/bin/sg_logs';

    scsi_structure   := GFRE_DBI.NewObject;
    scsi_structure.Field('enclosures').asObject := GFRE_DBI.NewObject;
    scsi_structure.Field('disks').asObject      := GFRE_DBI.NewObject;
    devices := getexpanderDevices;
  //  writeln('ES :',devices.DumpToString());
    try
      for i := 0 to devices.Field('device').ValueCount-1 do
        begin
          SG3SESInquiry(devices.Field('device').AsStringItem[i],scsi_structure);
        end;
    finally
      devices.Finalize;
    end;
    devices := getrdskDevices(iostat_devices);
    try
      for i := 0 to devices.Field('device').ValueCount-1 do
        begin
          if SG3DiskInquiry(devices.Field('device').AsStringItem[i],disk,read_logs)=0 then
            scsi_structure.Field('disks').asObject.Field(disk.Field('DEVICEIDENTIFIER').asstring).asobject:=disk;
        end;
    finally
      devices.Finalize;
    end;
  finally
    iostat_devices.Finalize;
  end;
  GFRE_DBI.LogDebug(dblc_APPLICATION,'SG3DiskandEnclosureInformation Done');
end;

function TFRE_DB_SCSI.GetMpathAdmLUInformation(out error: string; out mpathinfo: IFRE_DB_Object): integer;
var   proc        : TFRE_DB_Process;
      sl          : TStringList;
      s           : string;
      device      : string;
      i           : NativeInt;
      p           : NativeInt;
      tpathcount  : NativeInt;
      opathcount  : NativeInt;
      devobj      : IFRE_DB_Object;
begin
  ClearProcess;
  proc := TFRE_DB_Process.create;
  proc.SetupInput(cMpathListLu,TFRE_DB_StringArray.Create ('list','lu'));
  AddProcess(proc);
  ExecuteMulti;

  error  := proc.ErrorToString;
  result := proc.Field('exitstatus').AsInt32;
  mpathinfo := GFRE_DBI.NewObject;
  if result=0 then
    begin
      sl:=TStringList.Create;
      try
        sl.Text := proc.OutputToString;
        for i:=0 to sl.count-1 do
          begin
            s := sl[i];
            p := Pos('/dev/rdsk/',s);
            if p>0 then
              begin
                device := Copy(s,p+10,length(s));
                device := Copy(device,1,length(device)-2);
//                writeln('SWL: DEVICE:',device);
                if (i+1<sl.count) then
                  tpathcount:= strtoint(trim(GFRE_BT.SepRight(sl[i+1],':')))
                else
                  tpathcount:= 0;
                if (i+2<sl.count) then
                  opathcount:= strtoint(trim(GFRE_BT.SepRight(sl[i+2],':')))
                else
                  opathcount:= 0;
                devobj:=GFRE_DBI.NewObject;
                devobj.Field('device').asstring     := device;
                devobj.Field('tpathcount').AsUInt16 := tpathcount;
                devobj.Field('opathcount').AsUInt16 := opathcount;
                mpathinfo.Field(device).AsObject    := devobj;
              end;
          end;
      finally
        sl.Free;
      end;
  end;
end;

{ TFRE_DB_PHYS_DISK }

function TFRE_DB_PHYS_DISK.GetBlockSize: Uint16;
begin
  Result:=Field('blocksize').AsUint16;
end;

function TFRE_DB_PHYS_DISK.GetEnclosureUID: TFRE_DB_GUID;
begin
  Result:=Field('enclosure_uid').AsObjectLink;
end;

function TFRE_DB_PHYS_DISK.GetEnclosureNr: Uint16;
begin
  Result:=Field('enclosurenr').AsUInt16;
end;

function TFRE_DB_PHYS_DISK.Getfw_revision: string;
begin
  result:=field('fw_revision').asstring;
end;

function TFRE_DB_PHYS_DISK.Getmanufacturer: string;
begin
  result:=field('manufacturer').asstring;
end;

function TFRE_DB_PHYS_DISK.Getmodel_number: string;
begin
  result:=field('model_number').asstring;
end;

function TFRE_DB_PHYS_DISK.GetOperationalPathCount: Uint16;
begin
  if FieldExists('operationalpathcount') then
    Result:=Field('operationalpathcount').AsUInt16
  else
    Result:=0;
end;

function TFRE_DB_PHYS_DISK.GetParentInEnclosureUID: TFRE_DB_GUID;
begin
  Result:=Field('parent_in_enclosure_uid').AsObjectLink;
end;

function TFRE_DB_PHYS_DISK.GetSlotNr: Uint16;
begin
  if FieldExists('slotnr') then begin
    Result:=Field('slotnr').AsUInt16;
  end else begin
    Result:=0;
  end;
end;

function TFRE_DB_PHYS_DISK.GetTotalPathCount: Uint16;
begin
  if FieldExists('totalpathcount') then
    Result:=Field('totalpathcount').AsUInt16
  else
    Result:=0;
end;

function TFRE_DB_PHYS_DISK.GetWWN: string;
begin
  result:=getDeviceIdentifier;
end;

function TFRE_DB_PHYS_DISK.Getserial_number: string;
begin
  result:=field('serial_number').asstring;
end;

function TFRE_DB_PHYS_DISK.GetSize_Sectors: UInt32;
begin
  result := field('size_sectors').AsUInt32;
end;

function TFRE_DB_PHYS_DISK.GetDiskLog: TFRE_DB_SG_LOGS;
begin
  if FieldExists('log') then
    result :=(Field('log').AsObject.Implementor_HC as TFRE_DB_SG_LOGS)
  else
    result := nil;
end;

procedure TFRE_DB_PHYS_DISK.SetBlockSize(AValue: Uint16);
begin
  Field('blocksize').AsUInt16:=AValue;
end;

procedure TFRE_DB_PHYS_DISK.SetEnclosureNr(AValue: Uint16);
begin
  Field('enclosurenr').AsUInt16:=AValue;
end;

procedure TFRE_DB_PHYS_DISK.SetEnclosureUID(AValue: TFRE_DB_GUID);
begin
   Field('enclosure_uid').AsObjectLink:=AValue;
end;


procedure TFRE_DB_PHYS_DISK.Setfw_revision(AValue: string);
begin
  field('fw_revision').asstring := avalue;
end;

procedure TFRE_DB_PHYS_DISK.Setmanufacturer(AValue: string);
begin
  field('manufacturer').asstring := avalue;
end;

procedure TFRE_DB_PHYS_DISK.Setmodel_number(AValue: string);
begin
  field('model_number').asstring := avalue;
end;

procedure TFRE_DB_PHYS_DISK.SetOperationalPathCount(AValue: Uint16);
begin
  Field('operationalpathcount').AsUInt16:=AValue;
end;

procedure TFRE_DB_PHYS_DISK.SetParentInEnclosureUID(AValue: TFRE_DB_GUID);
begin
  if FieldExists('parent_in_enclosure_uid') then
    if AValue<>Field('parent_in_enclosure_uid').AsObjectLink then
      RemoveMosParentID(Field('parent_in_enclosure_uid').AsObjectLink);
  Field('parent_in_enclosure_uid').AsObjectLink:=AValue;
  AddMosParentID(AValue);
end;

procedure TFRE_DB_PHYS_DISK.SetSlotNr(AValue: Uint16);
begin
  Field('slotnr').AsUInt16 := AValue;
end;

procedure TFRE_DB_PHYS_DISK.SetTotalPathCount(AValue: Uint16);
begin
  Field('totalpathcount').AsUInt16:=AValue;
end;


procedure TFRE_DB_PHYS_DISK.SetWWN(AValue: string);
begin
  setDeviceIdentifier(AValue);
end;

procedure TFRE_DB_PHYS_DISK.Setserial_number(AValue: string);
begin
  field('serial_number').asstring := avalue;
end;

procedure TFRE_DB_PHYS_DISK.SetSize_Sectors(AValue: UInt32);
begin
  field('size_sectors').AsUInt32 := AValue;
end;

procedure TFRE_DB_PHYS_DISK.SetDiskLog(Avalue: TFRE_DB_SG_LOGS);
begin
  Field('log').AsObject := Avalue;
end;

procedure TFRE_DB_PHYS_DISK._getSizeMB(const calcfieldsetter: IFRE_DB_CALCFIELD_SETTER);
begin
  if FieldExists('blocksize') and FieldExists('size_sectors') then
    calcfieldsetter.SetAsUInt32(Uint64(Field('blocksize').AsUInt16*Field('size_sectors').AsUInt32) div 1000000)
  else
    calcfieldsetter.SetAsUInt32(0);
end;

procedure TFRE_DB_PHYS_DISK._getCaption(const calcfieldsetter: IFRE_DB_CALCFIELD_SETTER);
begin
   calcfieldsetter.SetAsString('(' + IntToStr(SlotNr) + ')'+' '+WWN);
end;

procedure TFRE_DB_PHYS_DISK._getMOSCaption(const calcfieldsetter: IFRE_DB_CALCFIELD_SETTER);
begin
  calcfieldsetter.SetAsString('(' + IntToStr(SlotNr) + ')'+' '+Model_number+' ('+inttostr(OperationalPathCount)+'/'+inttostr(TotalPathCount)+')')
end;

class procedure TFRE_DB_PHYS_DISK.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddCalcSchemeField('size_mb',fdbft_UInt32,@_getSizeMb);
  scheme.AddCalcSchemeField('caption_layout',fdbft_String,@_getLayoutCaption);
  scheme.AddCalcSchemeField('subcaption_layout',fdbft_String,@_getLayoutSubcaption);
  scheme.AddCalcSchemeField('icon_layout',fdbft_String,@_getLayoutIcon);

  scheme.AddSchemeField('manufacturer',fdbft_String);
  scheme.AddSchemeField('model_number',fdbft_String);
  scheme.AddSchemeField('serial_number',fdbft_String);
  scheme.AddSchemeField('fw_revision',fdbft_String);

  scheme.AddSchemeFieldSubscheme('log',TFRE_DB_SG_LOGS.Classname);

  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main'));
  group.AddInput('deviceidentifier',GetTranslateableTextKey('scheme_deviceidentifier'),true);
  group.AddInput('manufacturer',GetTranslateableTextKey('scheme_manufacturer'),true);
  group.AddInput('model_number',GetTranslateableTextKey('scheme_model_number'),true);
  group.AddInput('serial_number',GetTranslateableTextKey('scheme_serial_number'),true);
  group.AddInput('fw_revision',GetTranslateableTextKey('scheme_fw_revision'),true);
  group.AddInput('devicename',GetTranslateableTextKey('scheme_devicename'),true);

  group:=scheme.AddInputGroup('log_common').Setup(GetTranslateableTextKey('scheme_log_common'));
  group.UseInputGroup(TFRE_DB_SG_LOGS.ClassName,'log_common','log');

  group:=scheme.AddInputGroup('log_ec').Setup(GetTranslateableTextKey('scheme_log_ec'));
  group.UseInputGroup(TFRE_DB_SG_LOGS.ClassName,'log_ec','log');

  group:=scheme.AddInputGroup('log_tp1').Setup(GetTranslateableTextKey('scheme_log_tp1'));
  group.UseInputGroup(TFRE_DB_SG_LOGS.ClassName,'log_tp1','log');

  group:=scheme.AddInputGroup('log_tp2').Setup(GetTranslateableTextKey('scheme_log_tp2'));
  group.UseInputGroup(TFRE_DB_SG_LOGS.ClassName,'log_tp2','log');

end;

class procedure TFRE_DB_PHYS_DISK.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newversionID:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

procedure TFRE_DB_PHYS_DISK._getLayoutCaption(const calcfieldsetter: IFRE_DB_CALCFIELD_SETTER);
begin
  calcfieldsetter.SetAsString('(' + IntToStr(SlotNr) + ')'+inttostr(Field('size_mb').AsUint32)+' '+Field('subcaption_layout').asstring);
end;

procedure TFRE_DB_PHYS_DISK._getLayoutSubCaption(const calcfieldsetter: IFRE_DB_CALCFIELD_SETTER);
begin
  calcfieldsetter.SetAsString(Manufacturer+' '+Model_number+' '+Serial_number+' '+WWN+' ('+inttostr(OperationalPathCount)+'/'+inttostr(TotalPathCount)+')');
end;

procedure TFRE_DB_PHYS_DISK._getLayoutIcon(const calcfieldsetter: IFRE_DB_CALCFIELD_SETTER);
begin
  if getIsNew then begin
    calcfieldsetter.SetAsString(FREDB_getThemedResource('images_apps/firmbox_storage/'+ClassName+'_new.png'));
  end else begin
    if IsUnassigned then begin
      calcfieldsetter.SetAsString(FREDB_getThemedResource('images_apps/firmbox_storage/'+ClassName+'_unassigned.png'));
    end else begin
      calcfieldsetter.SetAsString(FREDB_getThemedResource('images_apps/firmbox_storage/'+ClassName+'.png'));
    end;
  end;
end;

function TFRE_DB_PHYS_DISK.GetTargetPorts: TFRE_DB_StringArray;
begin
  if fieldExists('target_port') then
    result := field('target_port').AsStringArr
  else
    result := TFRE_DB_StringArray.Create;
end;

function TFRE_DB_PHYS_DISK.hasParentinEnclosure: boolean;
begin
  result := (FieldExists('parent_in_enclosure_uid')) and (not Field('parent_in_enclosure_uid').IsEmptyArray);
end;

procedure TFRE_DB_PHYS_DISK.ClearSlotandEnclosureInformation;
begin
  if FieldExists('enclosure_uid') then
    begin
      Field('mosparentIds').RemoveObjectLinkByUID(Field('enclosure_uid').AsGUID);
      Field('serviceParent').RemoveObjectLinkByUID(Field('enclosure_uid').AsGUID);
    end;
  Field('parent_in_enclosure_uid').Clear;
  Field('enclosure_uid').Clear;
  EnclosureNr         := 0;
  SlotNr              := 0;
end;

function TFRE_DB_PHYS_DISK.WEB_MOSContent(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  panel         : TFRE_DB_FORM_PANEL_DESC;
  scheme        : IFRE_DB_SchemeObject;
begin
  GFRE_DBI.GetSystemSchemeByName(SchemeClass,scheme);
  panel :=TFRE_DB_FORM_PANEL_DESC.Create.Describe(app.FetchAppTextShort(ses,'phys_disk_content_header'));
  panel.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  panel.AddSchemeFormGroup(scheme.GetInputGroup('log_common'),GetSession(input)).SetCollapseState(false,true);
  panel.AddSchemeFormGroup(scheme.GetInputGroup('log_ec'),GetSession(input)).SetCollapseState(true);
  panel.AddSchemeFormGroup(scheme.GetInputGroup('log_tp1'),GetSession(input)).SetCollapseState(true);
  panel.AddSchemeFormGroup(scheme.GetInputGroup('log_tp2'),GetSession(input)).SetCollapseState(true);

  panel.FillWithObjectValues(self,GetSession(input));
  Result:=panel;
end;

{ TFRE_DB_SAS_DISK }

procedure TFRE_DB_SAS_DISK.SetTargetPorts(const targetport_1: string; const targetport_2: string);
begin
  Field('target_port').AsString:=targetport_1;
  Field('target_port').AddString(targetport_2);
end;

class procedure TFRE_DB_SAS_DISK.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';

  if (currentVersionId='') then begin
    currentVersionId := '1.0';

    StoreTranslateableText(conn,'scheme_main','General Information');
    StoreTranslateableText(conn,'scheme_deviceidentifier','Device Identifier');
    StoreTranslateableText(conn,'scheme_manufacturer','Manufacturer');
    StoreTranslateableText(conn,'scheme_model_number','Model Number');
    StoreTranslateableText(conn,'scheme_serial_number','Serial Number');
    StoreTranslateableText(conn,'scheme_fw_revision','Firmware');
    StoreTranslateableText(conn,'scheme_devicename','Devicename');
    StoreTranslateableText(conn,'scheme_log_common','General Log Information');
    StoreTranslateableText(conn,'scheme_log_ec','Error Correction Information');
    StoreTranslateableText(conn,'scheme_log_tp1','Target Port 1 Information');
    StoreTranslateableText(conn,'scheme_log_tp2','Target Port 2 Information');

  end;
  if (currentVersionId='1.0') then begin
  //next update code
  end;


end;

{ TFRE_DB_DRIVESLOT }

function TFRE_DB_DRIVESLOT.GetDeviceIdentifier: TFRE_DB_String;
begin
  result := Field('deviceIdentifier').AsString;
end;

function TFRE_DB_DRIVESLOT.GetEnclosureNr: Uint16;
begin
  Result:=Field('enclosurenr').AsUInt16;
end;

function TFRE_DB_DRIVESLOT.GetPortType: string;
begin
  result:=Field('porttype').asstring;
end;

function TFRE_DB_DRIVESLOT.GetSlotNr: UInt16;
begin
  if FieldExists('slotnr') then
    result := Field('slotnr').AsUInt16
  else
    result := 0;
end;

function TFRE_DB_DRIVESLOT.GetParentInEnclosureUID: TFRE_DB_GUID;
begin
  Result:=Field('parent_in_enclosure_uid').AsObjectLink;
end;

procedure TFRE_DB_DRIVESLOT.setDeviceIdentifier(AValue: TFRE_DB_String);
begin
  Field('deviceIdentifier').AsString:=AValue;
end;

procedure TFRE_DB_DRIVESLOT.SetEnclosureNr(AValue: Uint16);
begin
  Field('enclosurenr').AsUInt16:=AValue;
end;

procedure TFRE_DB_DRIVESLOT.SetParentInEnclosureUID(AValue: TFRE_DB_GUID);
begin
  if FieldExists('parent_in_enclosure_uid') then
    if AValue<>Field('parent_in_enclosure_uid').AsObjectLink then
      RemoveMosParentID(Field('parent_in_enclosure_uid').AsObjectLink);
  Field('parent_in_enclosure_uid').AsObjectLink:=AValue;
  AddMosParentID(AValue);
end;

procedure TFRE_DB_DRIVESLOT.SetPortType(AValue: string);
begin
  Field('porttype').asstring:=Avalue;
end;

procedure TFRE_DB_DRIVESLOT.SetSlotNr(AValue: UInt16);
begin
  Field('slotnr').AsUInt16:=Avalue;
end;

class procedure TFRE_DB_DRIVESLOT.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

class procedure TFRE_DB_DRIVESLOT.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.Classname);
  scheme.AddCalcSchemeField ('displayname',fdbft_String,@CALC_GetDisplayName);
end;

procedure TFRE_DB_DRIVESLOT.AddMosParentID(const avalue: TFRE_DB_GUID);
begin
  if FREDB_GuidInArray(AValue,Field('mosparentIds').AsObjectLinkArray)=-1 then
    begin
      Field('mosparentIds').AddObjectLink(AValue);
    end;
  if FREDB_GuidInArray(AValue,Field('serviceparent').AsObjectLinkArray)=-1 then
    begin
      Field('serviceParent').AddObjectLink(avalue);
    end;
end;

procedure TFRE_DB_DRIVESLOT.RemoveMosParentID(const avalue: TFRE_DB_GUID);
var lp:NativeInt;
begin
  lp := FREDB_GuidInArray(AValue,Field('mosparentIds').AsObjectLinkArray);
  if lp>=0 then
    begin
      Field('serviceParent').RemoveObjectLink(lp);
    end;
  lp := FREDB_GuidInArray(AValue,Field('serviceparent').AsObjectLinkArray);
  if lp>=0 then
    begin
      Field('serviceParent').RemoveObjectLink(lp);
    end;
end;

procedure TFRE_DB_DRIVESLOT.SetAttachedTo(const port_nr: Byte; const AValue: string);
begin
  Field('attached_to_'+inttostr(port_nr)).AsString := AValue;
end;

procedure TFRE_DB_DRIVESLOT.SetTargetPort(const port_nr: Byte; const AValue: string);
begin
  Field('targetport_'+inttostr(port_nr)).AsString := AValue;
end;

function TFRE_DB_DRIVESLOT.GetTargetPort(const port_nr: Byte): TFRE_DB_String;
begin
  result := Field('targetport_'+inttostr(port_nr)).AsString;
end;

procedure TFRE_DB_DRIVESLOT.RemoveLinkToMe(const conn: IFRE_DB_CONNECTION);
var refs: TFRE_DB_ObjectReferences;
       i: NativeInt;
    obj : IFRE_DB_Object;
begin
  refs := conn.GetReferencesDetailed(Uid,false);
  for i:=0 to high(refs) do
    begin
      CheckDbResult(conn.Fetch(refs[i].linked_uid,obj),'could not fetch driveslot for update referencing ['+FREDB_G2H(refs[i].linked_uid)+']');
      if (obj.Implementor_HC is TFRE_DB_PHYS_DISK) then
        begin
          (obj.Implementor_HC as TFRE_DB_PHYS_DISK).ClearSlotandEnclosureInformation;
          CheckDbResult(conn.Update(obj),'could not update disk after clearing driveslot ['+FREDB_G2H(refs[i].linked_uid)+']');
        end
      else
        obj.Finalize;
    end;
end;

procedure TFRE_DB_DRIVESLOT.CALC_GetDisplayName(const setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString('(' + IntToStr(SlotNr) + ')' + DeviceIdentifier);
end;

function TFRE_DB_DRIVESLOT.WEB_GetDefaultCollection(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  result:=GFRE_DBI.NewObject;
  result.Field('collection').asstring:=CFRE_DB_DRIVESLOT_COLLECTION;
end;

{ TFRE_DB_SAS_EXPANDER }

function TFRE_DB_SAS_EXPANDER.Getfw_revision: string;
begin
  result:=field('fw_revision').asstring;
end;

function TFRE_DB_SAS_EXPANDER.Getmanufacturer: string;
begin
  result:=field('manufacturer').asstring;
end;

function TFRE_DB_SAS_EXPANDER.Getmodel_number: string;
begin
  result:=field('model_number').asstring;
end;

function TFRE_DB_SAS_EXPANDER.GetDeviceIdentifier: TFRE_DB_String;
begin
  result:=Field('deviceIdentifier').AsString;
end;

function TFRE_DB_SAS_EXPANDER.GetParentInEnclosureUID: TFRE_DB_GUID;
begin
  Result:=Field('parent_in_enclosure_uid').AsObjectLink;
end;


procedure TFRE_DB_SAS_EXPANDER.Setfw_revision(AValue: string);
begin
  field('fw_revision').asstring := avalue;
end;

procedure TFRE_DB_SAS_EXPANDER.Setmanufacturer(AValue: string);
begin
  field('manufacturer').asstring := avalue;
end;

procedure TFRE_DB_SAS_EXPANDER.Setmodel_number(AValue: string);
begin
  field('model_number').asstring := avalue;
end;

procedure TFRE_DB_SAS_EXPANDER.SetDeviceIdentifier(AValue: TFRE_DB_String);
begin
  Field('deviceIdentifier').AsString:=AValue;
end;

procedure TFRE_DB_SAS_EXPANDER.SetParentInEnclosureUID(AValue: TFRE_DB_GUID);
begin
  if FieldExists('parent_in_enclosure_uid') then
    if AValue<>Field('parent_in_enclosure_uid').AsObjectLink then
      RemoveMosParentID(Field('parent_in_enclosure_uid').AsObjectLink);
  Field('parent_in_enclosure_uid').AsObjectLink:=AValue;
  AddMosParentID(AValue);
end;

class procedure TFRE_DB_SAS_EXPANDER.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

procedure TFRE_DB_SAS_EXPANDER.AddMosParentID(const avalue: TFRE_DB_GUID);
begin
  if FREDB_GuidInArray(AValue,Field('mosparentIds').AsObjectLinkArray)=-1 then
    Field('mosparentIds').AddObjectLink(AValue);
  if FREDB_GuidInArray(AValue,Field('serviceParent').AsObjectLinkArray)=-1 then
    Field('serviceparent').AddObjectLink(AValue);
end;

procedure TFRE_DB_SAS_EXPANDER.RemoveMosParentID(const avalue: TFRE_DB_GUID);
var lp:NativeInt;
begin
  lp := FREDB_GuidInArray(AValue,Field('mosparentIds').AsObjectLinkArray);
  if lp>=0 then
    Field('mosparentIds').RemoveObjectLink(lp);
  lp := FREDB_GuidInArray(AValue,Field('serviceparent').AsObjectLinkArray);
  if lp>=0 then
    Field('serviceparent').RemoveObjectLink(lp);
end;

function TFRE_DB_SAS_EXPANDER.WEB_GetDefaultCollection(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  result:=GFRE_DBI.NewObject;
  result.Field('collection').asstring:=CFRE_DB_SAS_EXPANDER_COLLECTION;
end;

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_UNDEFINED_BLOCKDEVICE);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SG_LOGS);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_PHYS_DISK);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SAS_DISK);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SATA_DISK);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SAS_EXPANDER);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DRIVESLOT);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ENCLOSURE);
  //GFRE_DBI.Initialize_Extension_Objects;
end;


end.

