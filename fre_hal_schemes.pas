unit fre_hal_schemes;

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
  Classes, SysUtils,FOS_TOOL_INTERFACES,
  Process, unixutil,

  FRE_HAL_UTILS,
  FRE_DB_COMMON,
  FRE_DBBUSINESS,
  FRE_DB_INTERFACE,
  fre_system,
  fre_dbbase,
  fre_testcase,
  fre_alert,
  fre_zfs,
  fre_scsi,
  fre_openssl_interface,
  fre_monitoring,
  {$IFDEF SOLARIS}
  fosillu_hal_dbo_common,
  fosillu_hal_zonectrl,
  fosillu_hal_svcctrl,
  fosillu_hal_dbo_zfs_dataset,
  fosillu_dladm,
  fosillu_ipadm,
  {$ENDIF}
  fre_process;

const

  CFRE_DB_CA_COLLECTION                = 'ca';
  CFRE_DB_CERTIFICATE_COLLECTION       = 'certificate';

  CFOS_DB_SERVICES_COLLECTION          = 'services';
  CFOS_DB_ZONES_COLLECTION             = 'zones';
  CFRE_DB_ASSET_COLLECTION             = 'assets';
  CFRE_DB_DATACENTER_COLLECTION        = 'datacenter';
  CFRE_DB_TEMPLATE_COLLECTION          = 'templates';
  CFRE_DB_IP_COLLECTION                = 'ip';
  CFRE_DB_ROUTING_COLLECTION           = 'routing';
  CFRE_DB_VM_COMPONENTS_COLLECTION     = 'vmcomponents';
  CFRE_DB_IMAGEFILE_COLLECTION         = 'imagefiles';

type



   { TFRE_DB_HALCONFIG }

    TFRE_DB_HALCONFIG=class(TFRE_DB_ObjectEx)
    protected
      class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
      class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    end;

    { TFRE_DB_SERVICEDOMAIN }

    TFRE_DB_SERVICE_DOMAIN=class(TFRE_DB_ObjectEx) { TODO: Think about link with original(system) domain }
    protected
      class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
      class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    published
      procedure       CALC_GetDisplayName (const setter:IFRE_DB_CALCFIELD_SETTER);
    end;

    { TFRE_DB_SERVICE_BASE }

   TFRE_DB_SERVICE_BASE=class(TFRE_DB_VIRTUALMOSOBJECT)
   public
     class function  OnlyOneServicePerZone  : Boolean; virtual;
     class function  GetCaption             (const conn:IFRE_DB_CONNECTION): String; virtual;
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   published
     procedure       CALC_GetDisplayName    (const setter:IFRE_DB_CALCFIELD_SETTER); virtual;
     class function  WBC_GetConfig          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object; virtual;
   end;

    { TFRE_DB_SERVICE }

   TFRE_DB_SERVICE=class(TFRE_DB_SERVICE_BASE)
   protected
     procedure       ClearErrors;
     function        ExecuteCMD(const cmd:string;out outstring:string;const ignore_errors:boolean=false):integer;

   public
     class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
     class function  GetMachineUIDForService(const conn : IFRE_DB_CONNECTION ; service_uid : TFRE_DB_GUID):TFRE_DB_GUID;
     procedure       Embed                  (const conn: IFRE_DB_CONNECTION); virtual;
     procedure       SetSvcNameandType      (const service_name:string; const common_name : string; const duration:string; const ignore_error:string; const enabled:boolean=false);
     procedure       SetSvcEnvironment      (const working_directory:string; const user,group:string; const environment:string; const privileges:string='');
     procedure       SetSvcStart            (const execname:string; const timeout: Uint64);
     procedure       SetSvcStop             (const execname:string; const timeout: Uint64);
     procedure       SetSvcRestart          (const execname:string; const timeout: Uint64);
     procedure       AddSvcDependency       (const name:string; const fmri:string; const grouping:string; const restart_on:string);
     procedure       AddSvcDependent        (const name:string; const fmri:string; const grouping:string; const restart_on:string);
     function        GetFMRI                : TFRE_DB_STRING; virtual;
   published
     function        RIF_CreateOrUpdateService : IFRE_DB_Object; virtual;
     function        StartService          : IFRE_DB_Object; virtual; abstract;
     function        StopService           : IFRE_DB_Object; virtual; abstract;
     function        RIF_EnableService     (const runnning_ctx : TObject) : IFRE_DB_Object; virtual;
     function        RIF_DisableService    (const runnning_ctx : TObject) : IFRE_DB_Object; virtual;

   end;

   { TFRE_DB_SUBSERVICE }

   TFRE_DB_SUBSERVICE=class(TFRE_DB_SERVICE_BASE)
   protected
     class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   published
     procedure       CALC_GetDisplayName (const setter:IFRE_DB_CALCFIELD_SETTER);
   end;

   { TFRE_DB_SERVICE_INSTANCE }

   TFRE_DB_SERVICE_INSTANCE=class(TFRE_DB_ObjectEx)
   private
     function  GetFMRI: TFRE_DB_String;
     function  GetLogfileName: TFRE_DB_String;
     function  GetServiceDescription: TFRE_DB_String;
     function  GetState: TFRE_DB_String;
     function  GetStateTime: TFRE_DB_DateTime64;
     procedure SetFMRI(AValue: TFRE_DB_String);
     procedure SetLogfileName(AValue: TFRE_DB_String);
     procedure SetServiceDescription(AValue: TFRE_DB_String);
     procedure SetState(AValue: TFRE_DB_String);
     procedure SetStateTime(AValue: TFRE_DB_DateTime64);
   protected
     class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     property FMRI               : TFRE_DB_String read GetFMRI write SetFMRI;
     property State              : TFRE_DB_String read GetState write SetState;
     property ServiceDescription : TFRE_DB_String read GetServiceDescription write SetServiceDescription;
     property StateTime          : TFRE_DB_DateTime64 read GetStateTime write SetStateTime;
     property LogfileName        : TFRE_DB_String read GetLogfileName write SetLogfileName;
   published
   end;

   { TFRE_DB_DATACENTER }

   TFRE_DB_DATACENTER  = class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme       (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects           (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_FBZ_TEMPLATE }

   TFRE_DB_FBZ_TEMPLATE = class (TFRE_DB_ObjectEx)
   private
     function  getDeprecated : Boolean;
     function  getGlobal     : Boolean;
     procedure setDeprecated (AValue: Boolean);
     procedure setGlobal     (AValue: Boolean);
   protected
     class procedure RegisterSystemScheme       (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects           (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     property deprecated : Boolean read getDeprecated write setDeprecated;
     property global     : Boolean read getGlobal write setGlobal;
   end;

   { TFRE_DB_ASSET }

   TFRE_DB_ASSET  =class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme       (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects           (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   end;




   { TFRE_DB_MACHINE }

   TFRE_DB_MACHINE=class(TFRE_DB_ASSET)
   protected
     class procedure RegisterSystemScheme       (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects           (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     procedure       DeleteReferencingToMe      (const conn: IFRE_DB_CONNECTION);
     procedure       SetMOSStatus               (const status: TFRE_DB_MOS_STATUS_TYPE; const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION);
     function        GetMOSStatus               : TFRE_DB_MOS_STATUS_TYPE;
   published
     procedure       _getMOSCaption             (const calcfieldsetter : IFRE_DB_CALCFIELD_SETTER); virtual;
     procedure       _getStatusIcon             (const calc: IFRE_DB_CALCFIELD_SETTER);
     procedure       CALC_GetDisplayAddress     (const setter:IFRE_DB_CALCFIELD_SETTER);
     procedure       CALC_GetDisplayName        (const setter:IFRE_DB_CALCFIELD_SETTER);
     function        WEB_MOSContent             (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
     function        WEB_MOSChildStatusChanged  (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
     function        WEB_MOSStatus              (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
     function        WEB_GetDefaultCollection   (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
     function        WEB_REQUEST_DISK_ENC_POOL_DATA   (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
     function        WEB_REQUEST_SERVICE_STRUCTURE    (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
   end;

   { TFRE_DB_MACHINE_SETTING }

   TFRE_DB_MACHINE_SETTING=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_MACHINE_SETTING_POWER }

   TFRE_DB_MACHINE_SETTING_POWER=class(TFRE_DB_MACHINE_SETTING)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   published
     function        IMI_Shutdown           (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        IMI_Reboot             (const input:IFRE_DB_Object): IFRE_DB_Object;
   end;

   { TFRE_DB_MACHINE_SETTING_HOSTNAME }

   TFRE_DB_MACHINE_SETTING_HOSTNAME=class(TFRE_DB_MACHINE_SETTING)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_MACHINE_SETTING_MAIL }

   TFRE_DB_MACHINE_SETTING_MAIL=class(TFRE_DB_MACHINE_SETTING)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_MACHINE_SETTING_TIME }

   TFRE_DB_MACHINE_SETTING_TIME=class(TFRE_DB_MACHINE_SETTING)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_FC_PORT }

   TFRE_DB_FC_PORT=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_DATALINK }

   TFRE_DB_DATALINK=class(TFRE_DB_SERVICE)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     class function getAllDataLinkClasses   : TFRE_DB_StringArray;
     procedure      Embed                   (const conn: IFRE_DB_CONNECTION); override;
   published
     function       IMI_Menu                (const input:IFRE_DB_Object): IFRE_DB_Object;
     function       RIF_CreateOrUpdateServices                          : IFRE_DB_Object;
     class function WBC_GetConfig           (const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object; override;
   end;

   { TFRE_DB_DATALINK_PHYS }

   TFRE_DB_DATALINK_PHYS=class(TFRE_DB_DATALINK)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   published
     function        IMI_Menu               (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        IMI_AddVNIC            (const input:IFRE_DB_Object): IFRE_DB_Object;
   end;

   { TFRE_DB_DATALINK_VNIC }

   TFRE_DB_DATALINK_VNIC=class(TFRE_DB_DATALINK)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   published
     function        IMI_Menu               (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        IMI_Delete             (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        RIF_CreateVNIC         : IFRE_DB_Object;
     class function  GetCaption             (const conn: IFRE_DB_CONNECTION): String; override;
   end;

   { TFRE_DB_DATALINK_IPTUN }

   TFRE_DB_DATALINK_IPTUN=class(TFRE_DB_DATALINK)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   published
     class function  GetCaption             (const conn: IFRE_DB_CONNECTION): String; override;
   end;

   { TFRE_DB_IP_HOSTNET }

   TFRE_DB_IP_HOSTNET=class(TFRE_DB_SERVICE)
   protected
     procedure       InternalSetIPCIDR      (const value:TFRE_DB_String); virtual; abstract;
     function        GetAddrObjAlias        : string; virtual;

     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     class function  getAllHostnetClasses   : TFRE_DB_StringArray;
     function        GetFMRI                : TFRE_DB_String; override;
     procedure       SetIPCIDR              (const value:TFRE_DB_String);
   published
     function        RIF_CreateOrUpdateService : IFRE_DB_Object; override;
   end;



   { TFRE_DB_IPV4_HOSTNET }

   TFRE_DB_IPV4_HOSTNET=class(TFRE_DB_IP_HOSTNET)
   protected
     procedure       InternalSetIPCIDR      (const value:TFRE_DB_String); override;

     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     procedure       SetGatewayIP           (const value:TFRE_DB_String);
   published
     function        StartService           : IFRE_DB_Object; override;
     function        StopService            : IFRE_DB_Object; override;
   end;


   { TFRE_DB_IPV6_HOSTNET }

   TFRE_DB_IPV6_HOSTNET=class(TFRE_DB_IP_HOSTNET)
   protected
     procedure       InternalSetIPCIDR      (const value:TFRE_DB_String); override;

     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     procedure       SetGatewayIP           (const value:TFRE_DB_String);
     function        StartService           : IFRE_DB_Object; override;
     function        StopService            : IFRE_DB_Object; override;
   end;


   { TFRE_DB_DATALINK_STUB }

   TFRE_DB_DATALINK_STUB=class(TFRE_DB_DATALINK)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   published
     function        IMI_Menu               (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        IMI_AddVNIC            (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        IMI_Delete             (const input:IFRE_DB_Object): IFRE_DB_Object;
     class function  GetCaption             (const conn: IFRE_DB_CONNECTION): String; override;
   end;

   { TFRE_DB_DATALINK_AGGR }

   TFRE_DB_DATALINK_AGGR=class(TFRE_DB_DATALINK)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   published
     function        IMI_Menu               (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        IMI_AddVNIC            (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        IMI_Delete             (const input:IFRE_DB_Object): IFRE_DB_Object;
     class function  GetCaption             (const conn: IFRE_DB_CONNECTION): String; override;
   end;

   { TFRE_DB_DATALINK_IPMP }

   TFRE_DB_DATALINK_IPMP=class(TFRE_DB_DATALINK)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   published
     class function  GetCaption             (const conn: IFRE_DB_CONNECTION): String; override;
   end;

   { TFRE_DB_DATALINK_BRIDGE }

   TFRE_DB_DATALINK_BRIDGE=class(TFRE_DB_DATALINK)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   published
     class function  GetCaption             (const conn: IFRE_DB_CONNECTION): String; override;
   end;

   { TFRE_DB_DATALINK_SIMNET }

   TFRE_DB_DATALINK_SIMNET=class(TFRE_DB_DATALINK)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   published
     class function  GetCaption             (const conn: IFRE_DB_CONNECTION): String; override;
   end;

   { TFRE_DB_ZIP_STATUS }

   TFRE_ZIP_STATUS=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   end;


   { TFRE_DB_TESTER }

   TFRE_DB_TESTER=class(TFRE_DB_MACHINE)
   protected
     class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   end;


   { TFRE_DB_IMAGE_FILE }

   TFRE_DB_IMAGE_FILE=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_VHOST }

   TFRE_DB_VHOST=class(TFRE_DB_SERVICE)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     procedure       Embed                     (const conn: IFRE_DB_CONNECTION); override;
   end;

   { TFRE_DB_VROOTSERVER }

   TFRE_DB_VROOTSERVER=class(TFRE_DB_VHOST)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   end;


   { TFRE_DB_VMACHINE_DISK }

   TFRE_DB_VMACHINE_DISK=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     procedure       Embed                  (const conn: IFRE_DB_CONNECTION); virtual;
   end;

   { TFRE_DB_VMACHINE_NIC }

   TFRE_DB_VMACHINE_NIC=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
     procedure       EmbedDatalink          (const conn: IFRE_DB_CONNECTION);
   public
     procedure       Embed                  (const conn: IFRE_DB_CONNECTION); virtual;
   end;

   { TFRE_DB_VMACHINE }

   TFRE_DB_VMACHINE=class(TFRE_DB_VHOST)
   private
     function  getVNCHost: TFRE_DB_String;
     function  getVNCPort: UInt32;
     procedure setVNCHost(AValue: TFRE_DB_String);
     procedure setVNCPort(AValue: UInt32);
   published
     procedure       CALC_GetDisplayName       (const setter: IFRE_DB_CALCFIELD_SETTER); override;
     function        RIF_CreateOrUpdateService : IFRE_DB_Object; override;
     class function  GetCaption                (const conn: IFRE_DB_CONNECTION): String; override;
   public
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
     procedure       Embed                     (const conn: IFRE_DB_CONNECTION); override;
     function        GetFMRI                   : TFRE_DB_STRING; override;

     property  vncHost    : TFRE_DB_String read getVNCHost write setVNCHost;
     property  vncPort    : UInt32         read getVNCPort write setVNCPort;
   end;


   { TFRE_DB_DNS }

   TFRE_DB_DNS=class(TFRE_DB_SERVICE)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     function        GetFMRI                : TFRE_DB_STRING; override;
   published
     procedure       CALC_GetDisplayName       (const setter: IFRE_DB_CALCFIELD_SETTER); override;
     class function  GetCaption                (const conn: IFRE_DB_CONNECTION): String; override;
     function        RIF_CreateOrUpdateService : IFRE_DB_Object; override;
   end;

   { TFRE_DB_LDAP_SERVICE }

   TFRE_DB_LDAP_SERVICE=class(TFRE_DB_SERVICE)
   protected
     class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects          (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     function        GetFMRI                   : TFRE_DB_STRING; override;
   published
     function        RIF_CreateOrUpdateService : IFRE_DB_Object; override;
     class function  GetCaption                (const conn: IFRE_DB_CONNECTION): String; override;
   end;

   { TFRE_DB_NAS }

   TFRE_DB_NAS=class(TFRE_DB_SERVICE)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   published
     procedure       CALC_GetDisplayName          (const setter: IFRE_DB_CALCFIELD_SETTER); override;
   end;


   { TFRE_DB_ZONESTATUS_PLUGIN }

   TFRE_DB_ZONESTATUS_PLUGIN=class(TFRE_DB_STATUS_PLUGIN)
   public
     class procedure RegisterSystemScheme                (const scheme : IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects                    (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;

     procedure       SetZoneState         (const zonestatename:string; const zonestate_num:Int32);
     procedure       SetZoneID            (const zid:int64);
   end;

   { TFRE_DB_ZONE }

   TFRE_DB_ZONE=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme  (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects      (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   public
     procedure       Embed(const conn: IFRE_DB_CONNECTION);
     procedure       BootingHookConfigure  ;
   published
     procedure       CALC_GetDisplayName   (const setter: IFRE_DB_CALCFIELD_SETTER);
     class function  WBC_NewOperation      (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;override;
     function        WEB_SaveOperation     (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;override;
     function        hasNAS                (const conn: IFRE_DB_CONNECTION):Boolean;
     function        hasDNS                (const conn: IFRE_DB_CONNECTION):Boolean;
     function        MachineID             : TFRE_DB_GUID;
     function        RIF_Boot              (const runnning_ctx : TObject) : IFRE_DB_Object;
     function        RIF_Halt              (const runnning_ctx : TObject) : IFRE_DB_Object;
   end;

   { TFRE_DB_GLOBAL_ZONE }

   TFRE_DB_GLOBAL_ZONE=class(TFRE_DB_ZONE)
   protected
     class procedure RegisterSystemScheme  (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects      (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_ZONECREATION_JOB }

   TFRE_DB_ZONECREATION_JOB=class(TFRE_DB_JOB)
   public
     procedure       SetZoneObject               (const zonedbo:IFRE_DB_Object);
     procedure       ExecuteJob                  ; override;
   end;

   { TFRE_DB_ZONEDESTROY_JOB }

   TFRE_DB_ZONEDESTROY_JOB=class(TFRE_DB_JOB)
   public
     procedure       SetZoneObject               (const zonedbo:IFRE_DB_Object);
     procedure       ExecuteJob                  ; override;
   end;


   { TFRE_DB_DEVICE }

  TFRE_DB_DEVICE=class(TFRE_DB_ASSET)
  private
    function getProvisioningMac: TFRE_DB_String;
    procedure setProvisioningMac(AValue: TFRE_DB_String);
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    property  provisioningmac      : TFRE_DB_String read getProvisioningMac     write setProvisioningMac;
  end;

  { TFRE_DB_CRYPTOCPE }

  TFRE_DB_CRYPTOCPE=class(TFRE_DB_DEVICE)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_NETWORK_GROUP }

  TFRE_DB_NETWORK_GROUP=class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_CMS }

  TFRE_DB_CMS=class(TFRE_DB_SERVICE)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_REDIRECTION_FLOW }

  TFRE_DB_REDIRECTION_FLOW=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   end;

  { TFRE_DB_Site_Captive_Extension }

  TFRE_DB_Site_Captive_Extension = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_Endpoint }

  TFRE_DB_Endpoint = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function  IMI_Content         (const input:IFRE_DB_Object): IFRE_DB_Object;
    function  IMI_Configuration   (const input:IFRE_DB_Object): IFRE_DB_Object;
    function  IMI_Monitoring      (const input:IFRE_DB_Object): IFRE_DB_Object;
    function  IMI_Monitoring_Con  (const input:IFRE_DB_Object): IFRE_DB_Object;
    function  IMI_Monitoring_All  (const input:IFRE_DB_Object): IFRE_DB_Object;
    function  IMI_Monitoring_Data (const input:IFRE_DB_Object): IFRE_DB_Object;
    function  IMI_Provision       (const input:IFRE_DB_Object): IFRE_DB_Object;
    function  IMI_addOpenWifiNetwork  (const input:IFRE_DB_Object): IFRE_DB_Object;
    function  IMI_addWPA2Network  (const input:IFRE_DB_Object): IFRE_DB_Object;
    function  IMI_ChildrenData    (const input:IFRE_DB_Object): IFRE_DB_Object;
    procedure CALC_GetDisplayName (const Setter : IFRE_DB_CALCFIELD_SETTER);virtual;
  end;

  { TFRE_DB_Accesspoint }

  TFRE_DB_Accesspoint = class(TFRE_DB_Endpoint)
  private
    function  HasAnotherAP        (const site_id:TFRE_DB_GUID ; const conn : IFRE_DB_CONNECTION)  : boolean;
  protected
    class procedure RegisterSystemScheme  (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects      (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure AccessPointOnChange   (const conn: IFRE_DB_CONNECTION; const is_dhcp:boolean ; const dhcp_id : TFRE_DB_GUID; const mac : TFRE_DB_String); virtual;
  published
   class function  WBC_NewOperation       (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object; override;
   function        WEB_Menu               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
   function        WEB_SaveOperation      (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
  end;

  { TFRE_DB_AP_Linksys }

  TFRE_DB_AP_Linksys = class(TFRE_DB_Accesspoint)
  private
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Configuration           (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_AP_Linksys_E1000 }

  TFRE_DB_AP_Linksys_E1000 = class(TFRE_DB_AP_Linksys)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    procedure CALC_GetDisplayName        (const Setter : IFRE_DB_CALCFIELD_SETTER);
  end;

  { TFRE_DB_AP_Linksys_E1200 }

  TFRE_DB_AP_Linksys_E1200 = class(TFRE_DB_AP_Linksys)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    procedure CALC_GetDisplayName        (const Setter : IFRE_DB_CALCFIELD_SETTER);
  end;

  { TFRE_DB_AP_Linksys_E1200V2 }

  TFRE_DB_AP_Linksys_E1200V2 = class(TFRE_DB_AP_Linksys_E1200)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
     procedure CALC_GetDisplayName       (const Setter : IFRE_DB_CALCFIELD_SETTER);
  end;

  { TFRE_DB_AP_Lancom }

  TFRE_DB_AP_Lancom = class(TFRE_DB_Accesspoint)
  protected
   class procedure InstallDBObjects      (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
   class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
  end;

  { TFRE_DB_AP_Lancom_IAP321 }

  TFRE_DB_AP_Lancom_IAP321 = class(TFRE_DB_AP_Lancom)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    procedure CALC_GetDisplayName        (const Setter : IFRE_DB_CALCFIELD_SETTER);
  end;

  { TFRE_DB_AP_Lancom_OAP321 }

  TFRE_DB_AP_Lancom_OAP321 = class(TFRE_DB_AP_Lancom)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    procedure CALC_GetDisplayName        (const Setter : IFRE_DB_CALCFIELD_SETTER);
  end;

  { TFRE_DB_Monitoring_Status }

  TFRE_DB_Monitoring_Status = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    procedure CALC_GetStatusIcon         (const setter: IFRE_DB_CALCFIELD_SETTER);
  end;

  { TFRE_DB_CMS_PAGE }

  TFRE_DB_CMS_PAGE = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_Menu                    (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_CMS_ADPAGE }

  TFRE_DB_CMS_ADPAGE = class(TFRE_DB_CMS_PAGE)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
  end;

  { TFRE_DB_MobileDevice }

  TFRE_DB_MobileDevice = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_Menu                    (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_unassign                (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_Network }

  TFRE_DB_Network = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure NetworkOnChange      (const dbc : IFRE_DB_Connection; const is_dhcp:boolean; const subnet : string; const ep_id: TFRE_DB_GUID; const dns:string; const range_start, range_end : integer ); virtual;
  published
    class function  WBC_NewOperation     (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;override;
    function IMI_Content                 (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_Menu                    (const input:IFRE_DB_Object):IFRE_DB_Object;
    function WEB_SaveOperation           (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
  end;

  { TFRE_DB_WifiNetwork }

  TFRE_DB_WifiNetwork = class(TFRE_DB_Network)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_OpenWifiNetwork }

  TFRE_DB_OpenWifiNetwork = class(TFRE_DB_WifiNetwork)
  protected
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
  end;

  { TFRE_DB_WPA2Network }

  TFRE_DB_WPA2Network = class(TFRE_DB_WifiNetwork)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_RadiusNetwork }

  TFRE_DB_RadiusNetwork = class(TFRE_DB_WifiNetwork)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_CA }

  TFRE_DB_CA = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme     (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects         (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    class procedure RestoreCA                (const conn:IFRE_DB_CONNECTION; const filename:string; const domainName: string='');
  published
    function        Create_SSL_CA            : boolean;
    function        Import_SSL_CA            (const ca_crt_file,serial_file,ca_key_file,random_file,index_file,crl_number_file:TFRE_DB_String;out import_error: TFRE_DB_String) : boolean;
    function        Import_SSL_Certificates  (const conn: IFRE_DB_CONNECTION; const crt_dir,key_dir:TFRE_DB_String;out import_error: TFRE_DB_String) : boolean;
    procedure       BackupCA                 (const conn: IFRE_DB_CONNECTION; const filename:string);
  end;

  { TFRE_DB_Certificate }

  TFRE_DB_CERTIFICATE = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme     (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects         (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function        WEB_Revoke               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
    function        Create_SSL_Certificate   (const conn:IFRE_DB_CONNECTION): boolean;
    function        Import_SSL_Certificate   (const crt_file,key_file:string;out import_error: TFRE_DB_String): boolean;
  end;

  { TFRE_DB_DHCP }

  TFRE_DB_DHCP = class(TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        GetFMRI              : TFRE_DB_STRING; override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object) : IFRE_DB_Object;
    function IMI_Menu                    (const input:IFRE_DB_Object) : IFRE_DB_Object;
    function WEB_ChildrenData            (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
    function IMI_addSubnet               (const input:IFRE_DB_Object) : IFRE_DB_Object;
    function IMI_addFixedHost            (const input:IFRE_DB_Object) : IFRE_DB_Object;
    class function  GetCaption           (const conn: IFRE_DB_CONNECTION): String; override;
    function RIF_CreateOrUpdateService   : IFRE_DB_Object; override;
  end;

  { TFRE_DB_DHCP_Subnet }

  TFRE_DB_DHCP_Subnet = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object) : IFRE_DB_Object;
    function IMI_Menu                    (const input:IFRE_DB_Object) : IFRE_DB_Object;
  end;

  { TFRE_DB_DHCP_Fixed }

  TFRE_DB_DHCP_Fixed = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object) : IFRE_DB_Object;
    function IMI_Menu                    (const input:IFRE_DB_Object) : IFRE_DB_Object;
  end;

  { TFRE_DB_VPN }

  TFRE_DB_VPN = class(TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        GetFMRI                : TFRE_DB_STRING; override;
  published
    class function  GetCaption                (const conn: IFRE_DB_CONNECTION): String; override;
    function        RIF_CreateOrUpdateService : IFRE_DB_Object; override;
  end;

  { TFRE_DB_Radius }

  TFRE_DB_Radius = class(TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
  end;

  { TFRE_DB_Captiveportal }

  TFRE_DB_Captiveportal = class(TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Menu                    (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_Content                 (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_Routing }

  TFRE_DB_Routing = class(TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class function  OnlyOneServicePerZone : boolean; override;
    class function  GetCaption            (const conn: IFRE_DB_CONNECTION): String; override;
  published
  end;

  { TFRE_DB_FS_ENTRY }

  TFRE_DB_FS_ENTRY=class(TFRE_DB_ObjectEx)
  protected
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure InternalSetup ; override;
    procedure SetIsFile     (const isfile:boolean);
  public
    function  GetIsFile     : Boolean;
    procedure SetProperties (const name : TFRE_DB_String ; const is_file : boolean; const size : NativeInt ; const mode : Cardinal; const time : Longint);
    function  FileDirName   : String;
  end;

  { TFRE_DB_FILESERVER }

  TFRE_DB_FILESERVER=class(TFRE_DB_SERVICE)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_GLOBAL_FILESERVER }

  TFRE_DB_GLOBAL_FILESERVER=class(TFRE_DB_FILESERVER)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    class function  GetCaption           (const conn:IFRE_DB_CONNECTION): String; override;
  end;

  { TFRE_DB_VIRTUAL_FILESERVER }

  TFRE_DB_VIRTUAL_FILESERVER=class(TFRE_DB_FILESERVER)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        GetFMRI                : TFRE_DB_STRING; override;
  published
    function        RIF_CreateOrUpdateService : IFRE_DB_Object; override;
    class function  GetCaption                (const conn: IFRE_DB_CONNECTION): String; override;
  end;

  { TFRE_DB_CRYPTO_FILESERVER }

  TFRE_DB_CRYPTO_FILESERVER=class(TFRE_DB_FILESERVER)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        GetFMRI                : TFRE_DB_STRING; override;
  published
    function        RIF_CreateOrUpdateService : IFRE_DB_Object; override;
  end;

  { TFRE_DB_SSH_SERVICE }

  TFRE_DB_SSH_SERVICE=class(TFRE_DB_SERVICE)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        GetFMRI                : TFRE_DB_STRING; override;
  published
    class function  GetCaption                (const conn: IFRE_DB_CONNECTION): String; override;
    function        RIF_CreateOrUpdateService : IFRE_DB_Object; override;
  end;

  { TFRE_DB_IMAP_SERVICE }

  TFRE_DB_IMAP_SERVICE=class(TFRE_DB_SERVICE)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        GetFMRI                : TFRE_DB_STRING; override;
  published
    class function  GetCaption                (const conn: IFRE_DB_CONNECTION): String; override;
    function        RIF_CreateOrUpdateService : IFRE_DB_Object; override;
  end;

  { TFRE_DB_MTA_SERVICE }

  TFRE_DB_MTA_SERVICE=class(TFRE_DB_SERVICE)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        GetFMRI                : TFRE_DB_STRING; override;
  published
    class function  GetCaption                (const conn: IFRE_DB_CONNECTION): String; override;
    function        RIF_CreateOrUpdateService : IFRE_DB_Object; override;
  end;

  { TFRE_DB_POSTGRES_SERVICE }

  TFRE_DB_POSTGRES_SERVICE=class(TFRE_DB_SERVICE)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        GetFMRI                : TFRE_DB_STRING; override;
  published
    class function  GetCaption                (const conn: IFRE_DB_CONNECTION): String; override;
    function        RIF_CreateOrUpdateService : IFRE_DB_Object; override;
  end;

  { TFRE_DB_MYSQL_SERVICE }

  TFRE_DB_MYSQL_SERVICE=class(TFRE_DB_SERVICE)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        GetFMRI              : TFRE_DB_STRING; override;
  published
    class function  GetCaption                (const conn: IFRE_DB_CONNECTION): String; override;
    function        RIF_CreateOrUpdateService : IFRE_DB_Object; override;
  end;

  { TFRE_DB_HTTP_SERVICE }

  TFRE_DB_HTTP_SERVICE=class(TFRE_DB_SERVICE)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        GetFMRI              : TFRE_DB_STRING; override;
  published
    class function  GetCaption                (const conn: IFRE_DB_CONNECTION): String; override;
    function        RIF_CreateOrUpdateService : IFRE_DB_Object; override;
  end;

  { TFRE_DB_PHPFPM_SERVICE }

  TFRE_DB_PHPFPM_SERVICE=class(TFRE_DB_SERVICE)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        GetFMRI              : TFRE_DB_STRING; override;
  published
    class function  GetCaption                (const conn: IFRE_DB_CONNECTION): String; override;
    function        RIF_CreateOrUpdateService : IFRE_DB_Object; override;
  end;


  { TFRE_DB_CPE_NETWORK_SERVICE }

  TFRE_DB_CPE_NETWORK_SERVICE=class(TFRE_DB_SERVICE)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        ConfigureHAL:integer;
  end;

  { TFRE_DB_CPE_OPENVPN_SERVICE }

  TFRE_DB_CPE_OPENVPN_SERVICE=class(TFRE_DB_VPN)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        ConfigureHAL:integer;
  end;

  { TFRE_DB_CPE_DHCP_SERVICE }

  TFRE_DB_CPE_DHCP_SERVICE=class(TFRE_DB_DHCP)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        ConfigureHAL:integer;
  end;

  { TFRE_DB_CPE_VIRTUAL_FILESERVER }

  TFRE_DB_CPE_VIRTUAL_FILESERVER=class(TFRE_DB_CRYPTO_FILESERVER)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    function        ConfigureHAL:integer;
  end;



procedure Register_DB_Extensions;

procedure CreateServicesCollections(const conn: IFRE_DB_COnnection);

implementation

 procedure CA_BaseInformationtoDBO(const cao:IFRE_DB_OBJECT; const ca_base_information:RFRE_CA_BASEINFORMATION;const update:boolean=false);
 begin
   cao.Field('index').AsString       := ca_base_information.index;
   cao.Field('index_attr').AsString  := ca_base_information.index_attr;
   cao.Field('serial').AsString      := ca_base_information.serial;
   cao.Field('crlnumber').AsString   := ca_base_information.crlnumber;
   cao.Field('crl_stream').AsStream.SetFromRawByteString(ca_base_information.crl);
   if update=false then begin
     cao.Field('crt_stream').AsStream.SetFromRawByteString(ca_base_information.crt);
     cao.Field('key_stream').AsStream.SetFromRawByteString(ca_base_information.key);
     cao.Field('issued').AsDateTimeUTC := GFRE_DT.Now_UTC;
   end;
//   writeln(cao.DumpToString());
 end;

 procedure DBOtoCA_BaseInformation(const cao:IFRE_DB_OBJECT; out ca_base_information:RFRE_CA_BASEINFORMATION);
 begin
   ca_base_information.index         := cao.Field('index').AsString;
   ca_base_information.index_attr    := cao.Field('index_attr').AsString;
   ca_base_information.serial        := cao.Field('serial').AsString;
   ca_base_information.crlnumber     := cao.Field('crlnumber').AsString;
   ca_base_information.crl           := cao.Field('crl_stream').asstream.AsRawByteString;
   ca_base_information.crt           := cao.Field('crt_stream').asstream.AsRawByteString;
   ca_base_information.key           := cao.Field('key_stream').asstream.AsRawByteString;
 end;

 procedure SetReprovision (const dbc: IFRE_DB_Connection; const id:TFRE_DB_GUID);
 var
     obj       :    IFRE_DB_Object;
 begin
   writeln  ('set reprovision :'+FREDB_G2H(id));
   CheckDbResult(dbc.Fetch(id,obj),'NO OBJ FOUND FOR REPROVISION '+FREDB_G2H(id));
   obj.Field('reprovision').asboolean:=true;
   writeln  (obj.DumpToString());
   CheckDbResult(dbc.Update(obj),'failure on cloned/update');
 end;

 function GetService     (const dbc: IFRE_DB_Connection; const serviceclass:string): TFRE_DB_GUID;
 var
     coll    : IFRE_DB_COLLECTION;
     id      : TFRE_DB_GUID;
     hlt     : boolean;

     procedure _get(const obj:IFRE_DB_Object ; var halt : boolean);
     begin
       writeln('SERVICE '+obj.UID_String);
       if obj.IsA(serviceclass) then begin
         writeln('FOUND '+serviceclass+' '+obj.UID_String);
         id:=obj.uid;
         halt := true;
       end;
     end;


 begin
   writeln('GET SERVICE');
   coll   := dbc.GetCollection('service');
   hlt    := false;
   coll.ForAllBreak(@_get,hlt);
   result := id;
 end;


 procedure HasNets(const ep_id : IFRE_DB_Object; out has_open, has_wpa2: boolean);
 var
     childs              : TFRE_DB_GUIDArray;

 begin
   abort;
   //TODO FIX

   //writeln('HAS NETS');
   //has_open:=false; has_wpa2:=false;
   //childs:=ep_id.ReferencedByList('TFRE_DB_OPENWIFINETWORK');
   //has_open:=length(childs)>0;
   //writeln ('WIFI :',length(childs));
   //childs:=ep_id.ReferencedByList('TFRE_DB_WPA2NETWORK');
   //writeln ('WPA2 :',length(childs));
   //has_wpa2:=length(childs)>0;
 end;


 function GetNextNet (const dbc: IFRE_DB_CONNECTION; const ep_id:TFRE_DB_GUID) : string;
 var
     ep_obj     : IFRE_DB_Object;
     cap_id     : TFRE_DB_GUID;
     cap_obj    : IFRE_DB_Object;
     net        : string;
     colln      : IFRE_DB_COLLECTION;
     highest    : integer;
     cp_net     : string;
     cp_ip      : TFRE_HAL_IP4;
     result_ip  : TFRE_HAL_IP4;
     mask       : string;

     procedure _getnets(const obj:IFRE_DB_Object);
     var
         lcurrent     : string;
         lnet         : string;
         lsplit       : string;
         lmask        : string;
         lip          : TFRE_HAL_IP4;

     begin
       lnet       := obj.Field('ip_net').AsString;
       SplitCIDR(lnet,lcurrent,lmask);
       lip        := StringtoIP4 (lcurrent);
       if (cp_ip._bytes[0]=lip._bytes[0]) and (cp_ip._bytes[1]=lip._bytes[1]) then begin
        writeln (lcurrent);
        if lip._bytes[2]>highest then begin
         highest  := lip._bytes[2];
        end;
       end;
     end;

 begin
   CheckDbResult(dbc.Fetch(ep_id,ep_obj),'NO EP FOUND IN GET NEXT NET '+FREDB_G2H(ep_id));
   cap_id:=GetService(dbc,'TFRE_DB_CAPTIVEPORTAL');
   CheckDbResult(dbc.Fetch(cap_id,cap_obj),'NO CAPSERVICE FOUND IN GET NEXT NET '+FREDB_G2H(cap_id));

   if ep_obj.IsA('TFRE_DB_AP_Lancom') then begin
     net := cap_obj.Field('lancom_net').asstring;
     writeln('lancom ');

   end else begin
     writeln('linksys');
     if ep_obj.FieldExists('vpn_crtid') then begin
      net := cap_obj.Field('vpn_net').asstring;
      writeln('vpn');
     end else begin
      net := cap_obj.Field('linksys_net').asstring;
      writeln('no vpn');
     end;
   end;
  writeln(net);

  SplitCIDR(net,cp_net ,mask);


  cp_ip   := StringtoIP4(cp_net );
  highest := cp_ip._bytes[2];
  writeln (highest);

  colln   :=dbc.GetCollection('network');
  colln.ForAll(@_getnets);

  writeln (highest);

  result_ip._long     := cp_ip._long;
  result_ip._bytes[2] := highest + 1;
  result_ip._bytes[3] := 1;
  result              := IP4toString(result_ip)+'/24';
  writeln (result);
 end;

 function CheckClass(const new_net:string) : boolean;
 var
     mask       : string;
 begin
  result := true;
  writeln('CHECK CLASS :'+new_net+':');
  if new_net='' then exit;                     // accept empty nets

  mask:= Copy(new_net,Pos('/',new_net)+1,maxint);
  if mask<>'24' then begin
   result:=false;
  end;
 end;

 function UniqueNet (const dbc: IFRE_DB_CONNECTION; const network_id:TFRE_DB_GUID; const new_net: string) : boolean;
  var
      colln      : IFRE_DB_COLLECTION;
      check_net  : TFRE_HAL_IP4;
      ip         : string;
      mask       : string;
      gresult    : boolean;

      procedure _checknets(const obj:IFRE_DB_Object);
      var
          lcurrent     : string;
          lnet         : string;
          lsplit       : string;
          lmask        : string;
          lip          : TFRE_HAL_IP4;

      begin
        lnet       := obj.Field('ip_net').AsString;
        SplitCIDR(lnet,lcurrent,lmask);
        lip        := StringtoIP4 (lcurrent);
        if FREDB_Guids_Same(obj.UID,network_id)=false then begin    // check all other nets
         if (check_net._bytes[0]=lip._bytes[0]) and (check_net._bytes[1]=lip._bytes[1]) and (check_net._bytes[2]=lip._bytes[2]) then begin        // TODO FIXXME  Other Than /24 Networks
          writeln (lcurrent);
          gresult := false;
         end;
        end;
      end;

  begin

   gresult  := true;

   if new_net='' then exit;                     // accept empty nets

   SplitCIDR (new_net,ip,mask);
   check_net := StringtoIP4(ip);

   colln   := dbc.GetCollection('network');
   colln.ForAll(@_checknets);

   result   := gresult;
  end;

{ TFRE_DB_IMAGE_FILE }

class procedure TFRE_DB_IMAGE_FILE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.ClassName);

  scheme.AddSchemeField('filename',fdbft_String).required:=true;
end;

class procedure TFRE_DB_IMAGE_FILE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='0.1';
  if (currentVersionId='') then begin
    currentVersionId:='0.1';
  end;
end;

{ TFRE_DB_VMACHINE_NIC }

class procedure TFRE_DB_VMACHINE_NIC.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.ClassName);

  scheme.AddSchemeField('model',fdbft_String).required:=true;     // enum
  scheme.AddSchemeField('vnic',fdbft_ObjLink).required:=true;
  scheme.AddSchemeField('vm_vlan',fdbft_Int16).required:=true;

  //  https://wiki.firmos.at/display/FBX/Qemu+Parameters
  //-net
  //model (enum)
  //
  //Auswahl VNIC (reflink)
  //
  //vlan (vm_vlan)
  //  (default every vmachine nic in an other vm_vlan)
  //-ip assignment (ja/nein) (aufklappbar, extra configuration)
  //ip= IPV4HOSTNET
  //gateway_ip= IPV4HOSTNET
  //hostname=
  //dns_ip0=  IPV4HOSTNET
  //dns_ip1=  IPV4HOSTNET
end;

class procedure TFRE_DB_VMACHINE_NIC.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='0.1';
  if (currentVersionId='') then begin
    currentVersionId:='0.1';
  end;
end;

procedure TFRE_DB_VMACHINE_NIC.EmbedDatalink(const conn: IFRE_DB_CONNECTION);
var obj  : IFRE_DB_Object;
    vnic : TFRE_DB_DATALINK;
begin
  if FieldExists('nic') then
    begin
      CheckDbResult(conn.Fetch(Field('nic').AsGUID,obj));
      if obj.IsA(TFRE_DB_DATALINK,vnic) then
        begin
          Field('NIC_EMBED').asobject:=vnic;
        end
      else
        raise EFRE_DB_Exception.Create(edb_ERROR,'TFRE_DB_VMACHINE_NIC %s LINKS A NON DATALINK OBJ %s',[self.UID_String,vnic.UID_String]);
    end;
end;

procedure TFRE_DB_VMACHINE_NIC.Embed(const conn: IFRE_DB_CONNECTION);
begin
  EmbedDatalink(conn);
end;


{ TFRE_DB_VMACHINE_DISK }

class procedure TFRE_DB_VMACHINE_DISK.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.ClassName);

  scheme.AddSchemeField('drive_if',fdbft_String).required:=true;  // enum
  scheme.AddSchemeField('index',fdbft_Int16).required:=true;
  scheme.AddSchemeField('media',fdbft_String).required:=true;     // enum
  scheme.AddSchemeField('file',fdbft_ObjLink).required:=true;      // zvol or file

  //  https://wiki.firmos.at/display/FBX/Qemu+Parameters
  //  Parameters to configure
  //  -drive
  //  if (enum)
  //     ide, scsi, sd, mtd, floppy, virtio.
  //  index=i (select order of devices)
  //  file (objlink)
  //    select zvol (ide, scsi, virtio) or uploaded file (all if options)
  //  media (enum)
  //    set if to ide if media cdrom

  // Expert:
  //cache=            (enum)
  //S writethrough
  //S writeback
  //S none
  //S directsync (only 1.1.2)
  //S unsafe
  //
  //snapshot=on|off
  //readonly=on|off
  //copy-on-read=on|off

end;

class procedure TFRE_DB_VMACHINE_DISK.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='0.1';
  if (currentVersionId='') then begin
    currentVersionId:='0.1';
  end;
end;

procedure TFRE_DB_VMACHINE_DISK.Embed(const conn: IFRE_DB_CONNECTION);
var obj  : IFRE_DB_Object;
    zvol : TFRE_DB_ZFS_DATASET_ZVOL;
    img  : TFRE_DB_IMAGE_FILE;
begin
  if FieldExists('file') then
    begin
      CheckDbResult(conn.Fetch(Field('file').AsGUID,obj));
      if obj.IsA(TFRE_DB_ZFS_DATASET_ZVOL,zvol) then
        begin
          zvol.Field('FULLDATASETNAME').AsString := zvol.GetFullDatasetname(conn);
          Field('ZVOL_EMBED').asobject:=zvol;
        end
      else
        if obj.IsA(TFRE_DB_IMAGE_FILE,img) then
          begin
            img.Field('FULLFILENAME').AsString := '/shared/'+img.Field('filename').asstring;
            Field('IMGFILE').asobject:=img;
          end
        else
          raise EFRE_DB_Exception.Create(edb_ERROR,'TFRE_DB_VMACHINE_DISK %s LINKS A NON ZVOL OR IMAGEFILE OBJ %s',[self.UID_String,zvol.UID_String]);
    end;
end;

{ TFRE_DB_VHOST }

class procedure TFRE_DB_VHOST.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_SERVICE.ClassName);
end;

class procedure TFRE_DB_VHOST.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='0.1';
  if (currentVersionId='') then begin
    currentVersionId:='0.1';
  end;
end;

procedure TFRE_DB_VHOST.Embed(const conn: IFRE_DB_CONNECTION);
begin
end;

{ TFRE_DB_VROOTSERVER }

class procedure TFRE_DB_VROOTSERVER.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_VHOST.ClassName);
end;

class procedure TFRE_DB_VROOTSERVER.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='0.1';
  if (currentVersionId='') then begin
    currentVersionId:='0.1';
  end;
end;

{ TFRE_DB_ZONESTATUS_PLUGIN }

class procedure TFRE_DB_ZONESTATUS_PLUGIN.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_STATUS_PLUGIN.Classname);
end;

class procedure TFRE_DB_ZONESTATUS_PLUGIN.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='0.1';
  if (currentVersionId='') then begin
    currentVersionId:='0.1';
  end;
end;

procedure TFRE_DB_ZONESTATUS_PLUGIN.SetZoneState(const zonestatename: string;const zonestate_num: Int32);
begin
  Field('zstate').asstring     := zonestatename;
  Field('zstate_num').AsInt32  := zonestate_num;
end;

procedure TFRE_DB_ZONESTATUS_PLUGIN.SetZoneID(const zid: int64);
begin
  Field('zid').AsInt64:=zid;
end;

{ TFRE_DB_GLOBAL_ZONE }

class procedure TFRE_DB_GLOBAL_ZONE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var
  group: IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  group:=scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('scheme_main'));
  group.AddInput('objname',GetTranslateableTextKey('scheme_name'),true);
end;

class procedure TFRE_DB_GLOBAL_ZONE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  inherited InstallDBObjects(conn, currentVersionId, newVersionId);
end;

{ TFRE_DB_DATALINK_SIMNET }

class procedure TFRE_DB_DATALINK_SIMNET.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_DATALINK.ClassName);
end;

class procedure TFRE_DB_DATALINK_SIMNET.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'caption','Simnet');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_name','Name');
    StoreTranslateableText(conn,'scheme_description','Description');
    StoreTranslateableText(conn,'scheme_mtu','MTU');
  end;
end;

class function TFRE_DB_DATALINK_SIMNET.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

{ TFRE_DB_DATALINK_BRIDGE }

class procedure TFRE_DB_DATALINK_BRIDGE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_DATALINK.ClassName);
end;

class procedure TFRE_DB_DATALINK_BRIDGE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'caption','Bridge');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_name','Name');
    StoreTranslateableText(conn,'scheme_description','Description');
    StoreTranslateableText(conn,'scheme_mtu','MTU');
  end;
end;

class function TFRE_DB_DATALINK_BRIDGE.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

{ TFRE_DB_SERVICE_BASE }

class function TFRE_DB_SERVICE_BASE.OnlyOneServicePerZone: Boolean;
begin
  Result := false;
end;

class function TFRE_DB_SERVICE_BASE.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=ClassName;
end;

class procedure TFRE_DB_SERVICE_BASE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var
  group: IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  ('TFRE_DB_VIRTUALMOSOBJECT');
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('serviceParent',fdbft_ObjLink);
  scheme.AddCalcSchemeField('displayname',fdbft_String,@CALC_GetDisplayName);

  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('objname',GetTranslateableTextKey('scheme_objname'));
end;

class procedure TFRE_DB_SERVICE_BASE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Servicename');
  end;
end;

procedure TFRE_DB_SERVICE_BASE.CALC_GetDisplayName(const setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString(Field('objname').AsString);
end;

class function TFRE_DB_SERVICE_BASE.WBC_GetConfig(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=GFRE_DBI.NewObject;
  Result.Field('OnlyOneServicePerZone').AsBoolean:=OnlyOneServicePerZone;
  Result.Field('Caption').AsString:=GetCaption(conn);
  Result.Field('Type').AsString:='service';
end;

{ TFRE_DB_ZONEDESTROY_JOB }

procedure TFRE_DB_ZONEDESTROY_JOB.SetZoneObject(const zonedbo: IFRE_DB_Object);
begin
  Field('zone').asobject       :=zonedbo;
  Field('linkid').AsObjectLink :=zonedbo.UID;
  SetJobkeyDescription(uppercase('ZONEDESTROY_'+zonedbo.UID.AsHexString),'Destruction of Zone '+zonedbo.UID.AsHexString);
end;

procedure TFRE_DB_ZONEDESTROY_JOB.ExecuteJob;
var zobj : IFRE_DB_Object;
begin
  {$IFDEF SOLARIS}
//  writeln('SWL EXEC ZONEDESTROY');
  InitIllumosLibraryHandles;

  zobj := Field('zone').asobject;

  AddProgressLog('Zone destruction started',10);
  fre_halt_zone(zobj,self);
  AddProgressLog('Zone halt done',50);
  fre_destroy_zone(zobj,self);
  AddProgressLog('Zone destruction done',100);
  {$ELSE}
  AddProgressLog('Zone uninstallation not implemented on this system',100);
  {$ENDIF SOLARIS}
end;

{ TFRE_DB_ZONECREATION_JOB }

procedure TFRE_DB_ZONECREATION_JOB.SetZoneObject(const zonedbo: IFRE_DB_Object);
begin
  Field('zone').asobject       :=zonedbo;
  Field('linkid').AsObjectLink :=zonedbo.UID;
  SetJobkeyDescription(uppercase('ZONECREATE_'+zonedbo.UID.AsHexString),'Creation of Zone '+zonedbo.UID.AsHexString);
end;

procedure TFRE_DB_ZONECREATION_JOB.ExecuteJob;
var zobj : IFRE_DB_Object;
begin
  {$IFDEF SOLARIS}
  InitIllumosLibraryHandles;

  zobj := Field('zone').asobject;

  AddProgressLog('Zone creation started',10);
  fre_create_zonecfg(zobj,self);
  AddProgressLog('Zone configuration done',50);
//  readln;
  fre_install_zone(zobj,self);
  AddProgressLog('Zone installation done',100);
//  readln;
//  fre_set_zonestate(obj.UID.AsHexString,ZONE_STATE_INSTALLED);
//  writeln('zone set to installed');
//  readln;
//  fre_boot_zone(zobj);
//  writeln('zone booting');
  {$ELSE}
  AddProgressLog('Zone installation not implemented on this system',100);
  {$ENDIF SOLARIS}
end;


{ TFRE_DB_SUBSERVICE }

class procedure TFRE_DB_SUBSERVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  (TFRE_DB_SERVICE_BASE.ClassName);
end;

class procedure TFRE_DB_SUBSERVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Servicename');
  end;
end;

procedure TFRE_DB_SUBSERVICE.CALC_GetDisplayName(const setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString(Field('objname').AsString);
end;

{ TFRE_DB_PHPFPM_SERVICE }

class procedure TFRE_DB_PHPFPM_SERVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  scheme.SetParentSchemeByName(TFRE_DB_SERVICE.ClassName);
  inherited RegisterSystemScheme(scheme);
end;

class procedure TFRE_DB_PHPFPM_SERVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'caption','PHP');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Name');
  end;
end;

function TFRE_DB_PHPFPM_SERVICE.GetFMRI: TFRE_DB_STRING;
begin
  result := 'svc:/fos/fos_phpfpm';
end;

class function TFRE_DB_PHPFPM_SERVICE.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

function TFRE_DB_PHPFPM_SERVICE.RIF_CreateOrUpdateService: IFRE_DB_Object;
var servicename : string;
begin
  {$IFDEF SOLARIS}
    result := GFRE_DBI.NewObject;
    servicename := Copy(GetFMRI,6,maxint);
    SetSvcNameandType(servicename,'FirmOS PHPFPM Service ','contract','core,signal',true);
    SetSvcEnvironment('/','root','root','LANG=C');
    SetSvcStart('/opt/local/sbin/php-fpm -y /opt/local/etc/php-fpm.conf',60);
    SetSvcStop(':kill',60);

    AddSvcDependency('network','svc:/milestone/network:default','require_all','error');
    AddSvcDependency('filesystem-local','svc:/system/filesystem/local:default','require_all','none');

    fre_create_service(self);
    result.Field('fmri').asstring:=servicename;

  {$ENDIF}
end;

{ TFRE_DB_LDAP }

class procedure TFRE_DB_LDAP_SERVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_SERVICE.ClassName);
end;

class procedure TFRE_DB_LDAP_SERVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'caption','LDAP');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Name');
  end;
end;

function TFRE_DB_LDAP_SERVICE.GetFMRI: TFRE_DB_STRING;
begin
  result := 'svc:/fos/fos_ldap';
end;

function TFRE_DB_LDAP_SERVICE.RIF_CreateOrUpdateService: IFRE_DB_Object;
var servicename :string;
begin
 {$IFDEF SOLARIS}
   result := GFRE_DBI.NewObject;
   servicename := Copy(GetFMRI,6,maxint);
   SetSvcNameandType(servicename,'FirmOS LDAP Service ','contract','core,signal',true);
   SetSvcEnvironment('/var/openldap','root','root','PATH=/opt/local/bin:/opt/local/sbin:/usr/xpg4/bin:/usr/bin:/usr/sbin:/usr/sfw/bin:/usr/openwin/bin:/opt/SUNWspro/bin:/usr/ccs/bin');
   SetSvcStart('/opt/local/libexec/slapd -u slapd -g ldap -h ''ldap://0.0.0.0:44005 ldaps://0.0.0.0:44006'' -F /opt/local/etc/openldap/slapd.d',60);
   SetSvcStop(':kill',60);
   AddSvcDependency('network','svc:/milestone/network','require_all','none');
   AddSvcDependency('filesystem-local','svc:/system/filesystem/local:default','require_all','none');
   AddSvcDependency('autofs','svc:/system/filesystem/autofs:default','optional_all','error');
   AddSvcDependency('milestone','svc:/milestone/sysconfig','require_all','none');

   fre_create_service(self);
   result.Field('fmri').asstring:=servicename;

 {$ENDIF}
end;

class function TFRE_DB_LDAP_SERVICE.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

{ TFRE_DB_CRYPTO_FILESERVER }

class procedure TFRE_DB_CRYPTO_FILESERVER.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_FILESERVER.Classname);
  scheme.AddSchemeField('nfsshare',fdbft_String);
  scheme.AddSchemeField('nfsmountoptions',fdbft_String);
  scheme.AddSchemeField('ecryptfs_fnek_sig',fdbft_String);
  scheme.AddSchemeField('ecryptfs_sig',fdbft_String);
  scheme.AddSchemeField('ecryptfs_key',fdbft_String);
  scheme.AddSchemeField('cryptofile',fdbft_String);
  group:=scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('cfs_scheme_main_group'));
  group.AddInput('objname',GetTranslateableTextKey('scheme_fileservername'),false);
  group.AddInput('desc.txt',GetTranslateableTextKey('scheme_description'));
end;

class procedure TFRE_DB_CRYPTO_FILESERVER.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'cfs_scheme_main_group','Crypto Fileserver Properties');
    StoreTranslateableText(conn,'scheme_fileservername','Servername');
    StoreTranslateableText(conn,'scheme_description','Description');
  end;
end;

function TFRE_DB_CRYPTO_FILESERVER.GetFMRI: TFRE_DB_STRING;
begin
  Result:='no fmri';
end;

function TFRE_DB_CRYPTO_FILESERVER.RIF_CreateOrUpdateService: IFRE_DB_Object;
begin
 {$IFDEF SOLARIS}
   result := GFRE_DBI.NewObject;
 {$ENDIF}
end;


{ TFRE_DB_DATALINK_IPMP }

class procedure TFRE_DB_DATALINK_IPMP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_DATALINK.ClassName);
end;

class procedure TFRE_DB_DATALINK_IPMP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.1';
  if currentVersionId='' then begin
    currentVersionId := '1.0';

    StoreTranslateableText(conn,'scheme_main_group','Link Properties');
    StoreTranslateableText(conn,'scheme_name','Link Name');
    StoreTranslateableText(conn,'scheme_description','Description');
    StoreTranslateableText(conn,'scheme_mtu','MTU');
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';
    StoreTranslateableText(conn,'caption','IPMP');

    DeleteTranslateableText(conn,'scheme_main_group');
    DeleteTranslateableText(conn,'scheme_name');
    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_name','Name');
  end;
end;

class function TFRE_DB_DATALINK_IPMP.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

{ TFRE_DB_SSH_SERVICE }

class procedure TFRE_DB_SSH_SERVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  scheme.SetParentSchemeByName(TFRE_DB_SERVICE.ClassName);
  inherited RegisterSystemScheme(scheme);
end;

class procedure TFRE_DB_SSH_SERVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'caption','SSH');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Name');
  end;
end;

function TFRE_DB_SSH_SERVICE.GetFMRI: TFRE_DB_STRING;
begin
  result := 'svc:/fos/fos_ssh';
end;

class function TFRE_DB_SSH_SERVICE.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

function TFRE_DB_SSH_SERVICE.RIF_CreateOrUpdateService: IFRE_DB_Object;
var servicename :string;
begin
 {$IFDEF SOLARIS}
   result := GFRE_DBI.NewObject;
   servicename := Copy(GetFMRI,6,maxint);
   SetSvcNameandType(servicename,'FirmOS SSH Service ','contract','core,signal',true);
   SetSvcEnvironment('/','root','root','LANG=C');
   SetSvcStart('/lib/svc/method/sshd start',60);
   SetSvcStop(':kill',60);
   SetSvcRestart('/lib/svc/method/sshd restart',60);
   AddSvcDependency('network','svc:/milestone/network','require_all','none');
   AddSvcDependency('fs-local','svc:/system/filesystem/local','require_all','none');
   AddSvcDependency('fs-autofs','svc:/system/filesystem/autofs','optional_all','error');
   AddSvcDependency('net-loopback','svc:/network/loopback','require_all','none');
   AddSvcDependency('net-physical','svc:/network/physical','require_all','none');
   AddSvcDependency('cryptosvc','svc:/system/cryptosvc','require_all','none');
   AddSvcDependency('utmp','svc:/system/utmp','require_all','none');
   AddSvcDependency('network_ipfilter','svc:/network/ipfilter:default','optional_all','error');

   AddSvcDependent ('fos_ssh_multi-user-server','svc:/milestone/multi-user-server','optional_all','none');

//   <dependency name='config_data' grouping='require_all' restart_on='restart' type='path'>
//     <service_fmri value='file://localhost/etc/ssh/sshd_config'/>
//   </dependency>

   //<property_group name='firewall_config' type='com.sun,fw_configuration'>
   //  <propval name='apply_to' type='astring' value=''/>
   //  <propval name='exceptions' type='astring' value=''/>
   //  <propval name='policy' type='astring' value='use_global'/>
   //  <propval name='value_authorization' type='astring' value='solaris.smf.value.firewall.config'/>
   //</property_group>
   //<property_group name='firewall_context' type='com.sun,fw_definition'>
   //  <propval name='ipf_method' type='astring' value='/lib/svc/method/sshd ipfilter'/>
   //  <propval name='name' type='astring' value='ssh'/>
   //</property_group>
   //<property_group name='general' type='framework'>
   //  <propval name='action_authorization' type='astring' value='solaris.smf.manage.ssh'/>
   //</property_group>
   //<property_group name='startd' type='framework'>
   //  <propval name='ignore_error' type='astring' value='core,signal'/>
   //</property_group>

   fre_create_service(self);
   result.Field('fmri').asstring:=servicename;

 {$ENDIF}
end;

{ TFRE_DB_HTTP_SERVICE }

class procedure TFRE_DB_HTTP_SERVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  scheme.SetParentSchemeByName(TFRE_DB_SERVICE.ClassName);
  inherited RegisterSystemScheme(scheme);
end;

class procedure TFRE_DB_HTTP_SERVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'caption','Webserver');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Name');
  end;
end;

function TFRE_DB_HTTP_SERVICE.GetFMRI: TFRE_DB_STRING;
begin
  result := 'svc:/fos/fos_httpd';
end;

class function TFRE_DB_HTTP_SERVICE.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

function TFRE_DB_HTTP_SERVICE.RIF_CreateOrUpdateService: IFRE_DB_Object;
var servicename : string;
begin
  {$IFDEF SOLARIS}
    result := GFRE_DBI.NewObject;
    servicename := Copy(GetFMRI,6,maxint);
    SetSvcNameandType(servicename,'FirmOS HTTPD Service ','contract','core,signal',true);
    SetSvcEnvironment('/','root','root','LD_PRELOAD_32=/usr/lib/extendedFILE.so.1');
    SetSvcStart('/opt/local/sbin/httpd -k start',300);
    SetSvcStop('/opt/local/sbin/httpd -k stop',300);
    SetSvcRestart('/opt/local/sbin/httpd -k graceful',300);

    AddSvcDependency('network','svc:/milestone/network:default','require_all','error');
    AddSvcDependency('filesystem-local','svc:/system/filesystem/local:default','require_all','none');
    AddSvcDependency('php-fpm','svc:/fos/fos_phpfpm','require_all','none');

//        <dependency name='config-file' grouping='require_all' restart_on='refresh' type='path'>
//          <service_fmri value='file://localhost/opt/local/etc/httpd/httpd.conf'/>
//        </dependency>

    fre_create_service(self);
    result.Field('fmri').asstring:=servicename;

  {$ENDIF}
end;

{ TFRE_DB_MYSQL_SERVICE }

class procedure TFRE_DB_MYSQL_SERVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  scheme.SetParentSchemeByName(TFRE_DB_SERVICE.ClassName);
  inherited RegisterSystemScheme(scheme);
end;

class procedure TFRE_DB_MYSQL_SERVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'caption','MySQL');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Name');
  end;
end;

function TFRE_DB_MYSQL_SERVICE.GetFMRI: TFRE_DB_STRING;
begin
  result := 'svc:/fos/fos_mysql';
end;

class function TFRE_DB_MYSQL_SERVICE.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

function TFRE_DB_MYSQL_SERVICE.RIF_CreateOrUpdateService: IFRE_DB_Object;
var servicename : string;
begin
  {$IFDEF SOLARIS}
    result := GFRE_DBI.NewObject;
    servicename := Copy(GetFMRI,6,maxint);
    SetSvcNameandType(servicename,'FirmOS MySQL Service ','contract','core,signal',true);
    SetSvcEnvironment('/var/mysql','mysql','mysql','LD_PRELOAD_32=/usr/lib/extendedFILE.so.1');
    SetSvcStart('/opt/local/lib/svc/method/mysqld start',18446744073709551615);
    SetSvcStop('/opt/local/lib/svc/method/mysqld stop',18446744073709551615);

    AddSvcDependency('network','svc:/milestone/network:default','require_all','none');
    AddSvcDependency('filesystem-local','svc:/system/filesystem/local','require_all','none');
    AddSvcDependency('ldap','svc:/fos/fos_ldap','require_all','none');


//        <dependency name='net' grouping='require_all' restart_on='none' type='service'>
//          <service_fmri value='svc:/network/loopback'/>
//        </dependency>

    fre_create_service(self);
    result.Field('fmri').asstring:=servicename;

  {$ENDIF}
end;

{ TFRE_DB_POSTGRES_SERVICE }

class procedure TFRE_DB_POSTGRES_SERVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  scheme.SetParentSchemeByName(TFRE_DB_SERVICE.ClassName);
  inherited RegisterSystemScheme(scheme);
end;

class procedure TFRE_DB_POSTGRES_SERVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'caption','Postgres');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Name');
  end;
end;

function TFRE_DB_POSTGRES_SERVICE.GetFMRI: TFRE_DB_STRING;
begin
  result := 'svc:/fos/fos_postgresql';
end;

class function TFRE_DB_POSTGRES_SERVICE.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

function TFRE_DB_POSTGRES_SERVICE.RIF_CreateOrUpdateService: IFRE_DB_Object;
var servicename : string;
begin
  {$IFDEF SOLARIS}
    result := GFRE_DBI.NewObject;
    servicename := Copy(GetFMRI,6,maxint);
    SetSvcNameandType(servicename,'FirmOS PostgreSQL Service ','contract','core,signal',true);
    SetSvcEnvironment('/','postgres','postgres','LD_PRELOAD_32=/usr/lib/extendedFILE.so.1');
    SetSvcStart('/opt/local/lib/svc/method/postgresql start',300);
    SetSvcStop('/opt/local/lib/svc/method/postgresql stop',300);
    SetSvcRestart('/opt/local/lib/svc/method/postgresql refresh',60);

    AddSvcDependency('network','svc:/milestone/network:default','require_all','none');
    AddSvcDependency('filesystem-local','svc:/system/filesystem/local','require_all','error');
    AddSvcDependency('ldap','svc:/fos/fos_ldap','require_all','none');

    fre_create_service(self);
    result.Field('fmri').asstring:=servicename;

  {$ENDIF}
end;

{ TFRE_DB_MTA_SERVICE }

class procedure TFRE_DB_MTA_SERVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_SERVICE.ClassName);
end;

class procedure TFRE_DB_MTA_SERVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'caption','MTA');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Name');
  end;
end;

function TFRE_DB_MTA_SERVICE.GetFMRI: TFRE_DB_STRING;
begin
  result := 'svc:/fos/fos_mta';
end;

class function TFRE_DB_MTA_SERVICE.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

function TFRE_DB_MTA_SERVICE.RIF_CreateOrUpdateService: IFRE_DB_Object;
var servicename : string;
begin
  {$IFDEF SOLARIS}
    result := GFRE_DBI.NewObject;
    servicename := Copy(GetFMRI,6,maxint);
    SetSvcNameandType(servicename,'FirmOS MTA Service ','child','core,signal',true);
    SetSvcEnvironment('/','mail','mail','LANG=C');
    SetSvcStart('/opt/local/sbin/exim -C /opt/local/etc/exim/configure -bdf',60);
    SetSvcStop(':kill',60);
    AddSvcDependency('network','svc:/milestone/network:default','require_all','none');
    AddSvcDependency('filesystem-local','svc:/system/filesystem/local','require_all','error');
    AddSvcDependency('ldap','svc:/fos/fos_ldap','require_all','none');

    fre_create_service(self);
    result.Field('fmri').asstring:=servicename;

  {$ENDIF}
end;

{ TFRE_DB_IMAP_SERVICE }

class procedure TFRE_DB_IMAP_SERVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_SERVICE.ClassName);
end;

class procedure TFRE_DB_IMAP_SERVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'caption','IMAP');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Name');
  end;
end;

function TFRE_DB_IMAP_SERVICE.GetFMRI: TFRE_DB_STRING;
begin
  result := 'svc:/fos/fos_imap';
end;

class function TFRE_DB_IMAP_SERVICE.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

function TFRE_DB_IMAP_SERVICE.RIF_CreateOrUpdateService: IFRE_DB_Object;
var servicename : string;
begin
  {$IFDEF SOLARIS}
    result := GFRE_DBI.NewObject;
    servicename := Copy(GetFMRI,6,maxint);
    SetSvcNameandType(servicename,'FirmOS IMAP Service ','contract','core,signal',true);
    SetSvcEnvironment('/','root','root','MASTER_IS_PARENT_ENV=1');
    SetSvcStart('/opt/local/sbin/dovecot',60);
    SetSvcStop(':kill',60);
    AddSvcDependency('network','svc:/milestone/network:default','require_all','none');
    AddSvcDependency('filesystem-local','svc:/system/filesystem/local','require_all','error');
    AddSvcDependency('ldap','svc:/fos/fos_ldap','require_all','none');


    //<property_group name='application' type='application'>
    //    <propval name='config_file' type='astring' value='/opt/local/etc/dovecot/dovecot.conf'/>
    //</property_group>

    fre_create_service(self);
    result.Field('fmri').asstring:=servicename;

  {$ENDIF}
end;

{ TFRE_DB_FBZ_TEMPLATE }

function TFRE_DB_FBZ_TEMPLATE.getDeprecated: Boolean;
var
  f : IFRE_DB_Field;
begin
  if FieldOnlyExisting('deprecated',f) then
    Result:=f.AsBoolean
  else
    Result:=false;
end;

function TFRE_DB_FBZ_TEMPLATE.getGlobal: Boolean;
var
  f : IFRE_DB_Field;
begin
  if FieldOnlyExisting('global',f) then
    Result:=f.AsBoolean
  else
    Result:=false;
end;

procedure TFRE_DB_FBZ_TEMPLATE.setDeprecated(AValue: Boolean);
begin
  Field('deprecated').AsBoolean:=AValue;
end;

procedure TFRE_DB_FBZ_TEMPLATE.setGlobal(AValue: Boolean);
begin
  Field('global').AsBoolean:=AValue;
end;

class procedure TFRE_DB_FBZ_TEMPLATE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.ClassName);
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('serviceclasses',fdbft_String).SetupFieldDef(true,true);
  scheme.AddSchemeField('deprecated',fdbft_Boolean);
  scheme.AddSchemeField('global',fdbft_Boolean);
end;

class procedure TFRE_DB_FBZ_TEMPLATE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

{ TFRE_DB_DATACENTER }

class procedure TFRE_DB_DATACENTER.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var
  group: IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.ClassName);
  scheme.GetSchemeField('objname').required:=true;

  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('objname',GetTranslateableTextKey('scheme_objname'));
end;

class procedure TFRE_DB_DATACENTER.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Datacenter Name');
  end;
end;

{ TFRE_DB_DATALINK_IPTUN }

class procedure TFRE_DB_DATALINK_IPTUN.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
    enum  : IFRE_DB_Enum;

begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_DATALINK.ClassName);

  enum:=GFRE_DBI.NewEnum('ip_tun_mode').Setup(GFRE_DBI.CreateText('$enum_ip_tun_mode','ip tunnel mode Enum'));
  enum.addEntry('ip6ip6',GetTranslateableTextKey('enum_ip_tun_mode_ip6ip6'));
  enum.addEntry('sit',GetTranslateableTextKey('enum_ip_tun_mode_sit'));
  GFRE_DBI.RegisterSysEnum(enum);

  scheme.AddSchemeField('mode',fdbft_String).SetupFieldDef(true,false,'ip_tun_mode');
  scheme.AddSchemeField('remote_ip_net_ipv6',fdbft_String);
  scheme.AddSchemeField('local_ip_net_ipv6',fdbft_String);
  scheme.AddSchemeField('remote_ip_net_ipv4',fdbft_String);
  scheme.AddSchemeField('local_ip_net_ipv4',fdbft_String);
  scheme.AddSchemeField('device',fdbft_String).required:=true;
  group:=scheme.AddInputGroup('tunnel').Setup(GetTranslateableTextKey('scheme_tunnel_group'));
  group.AddInput('mode',GetTranslateableTextKey('scheme_tunnel_mode'));
  group.AddInput('remote_ip_net_ipv6',GetTranslateableTextKey('scheme_remote_ip_net_ipv6'));
  group.AddInput('local_ip_net_ipv6',GetTranslateableTextKey('scheme_local_ip_net_ipv6'));
  group.AddInput('remote_ip_net_ipv4',GetTranslateableTextKey('scheme_remote_ip_net_ipv4'));
  group.AddInput('local_ip_net_ipv4',GetTranslateableTextKey('scheme_local_ip_net_ipv4'));
  group.AddInput('device',GetTranslateableTextKey('scheme_device'));
end;

class procedure TFRE_DB_DATALINK_IPTUN.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.1';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'enum_ip_tun_mode_ip6ip6','IPv6 in IPv6');
    StoreTranslateableText(conn,'scheme_tunnel_group','IP Tunnel');
    StoreTranslateableText(conn,'scheme_tunnel_mode','IP Tunnel Mode');
    StoreTranslateableText(conn,'scheme_remote_ip_net_ipv6','IPv6 Remote Address');
    StoreTranslateableText(conn,'scheme_local_ip_net_ipv6','IPv6 Local Address');
    StoreTranslateableText(conn,'scheme_remote_ip_net_ipv4','IPv4 Remote Address');
    StoreTranslateableText(conn,'scheme_local_ip_net_ipv4','IPv4 Local Address');
    StoreTranslateableText(conn,'scheme_device','Device');
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';
    StoreTranslateableText(conn,'caption','IPTUN');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_name','Name');
    StoreTranslateableText(conn,'scheme_description','Description');
    StoreTranslateableText(conn,'scheme_mtu','MTU');
  end;
end;

class function TFRE_DB_DATALINK_IPTUN.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

{ TFRE_DB_IP_ADDRESS }

function TFRE_DB_IP_HOSTNET.GetAddrObjAlias: string;
begin
  result :='v'+Copy(UID.AsHexString,1,24); // 30 works for ipv4, 28 for ipv6
end;

class procedure TFRE_DB_IP_HOSTNET.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_SERVICE.Classname);
  scheme.AddSchemeField('datalinkParent',fdbft_ObjLink).multiValues:=false;
end;

class procedure TFRE_DB_IP_HOSTNET.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

class function TFRE_DB_IP_HOSTNET.getAllHostnetClasses: TFRE_DB_StringArray;
begin
  Result:=TFRE_DB_StringArray.create(TFRE_DB_IPV4_HOSTNET.ClassName,TFRE_DB_IPV6_HOSTNET.ClassName);
end;

function TFRE_DB_IP_HOSTNET.GetFMRI: TFRE_DB_String;
begin
  if FieldExists('gateway') then
    result := 'svc:/fos/fos_routing_'+UID.AsHexString
  else
    result := 'svc:/fos/fos_ip_'+Field('datalinkname').asstring+'_'+UID.AsHexString;
end;

procedure TFRE_DB_IP_HOSTNET.SetIPCIDR(const value: TFRE_DB_String);
begin
  InternalSetIPCIDR(value);
end;

function TFRE_DB_IP_HOSTNET.RIF_CreateOrUpdateService: IFRE_DB_Object;
var servicename : string;
begin
  {$IFDEF SOLARIS}
  result := GFRE_DBI.NewObject;
  if FieldExists('gateway') then
    begin
      servicename := Copy(GetFMRI,6,maxint);
      writeln('SWL: CREATE SVC ROUTING ',ObjectName,' ',servicename);
      SetSvcNameandType(servicename,'FirmOS Routing Service '+Field('IP_NET').asstring+' '+Field('gateway').asstring,'transient','core,signal',true);
      SetSvcEnvironment('/opt/local/fre','root','root','LANG=C');
      SetSvcStart('/opt/local/fre/bin/fossvc --start --routing='+UID.AsHexString,60);
      SetSvcStop('/opt/local/fre/bin/fossvc --stop --routing='+UID.AsHexString,60);
      AddSvcDependency('fosip','svc:/fos/fosip','require_all','none');
      AddSvcDependent (StringReplace(servicename,'/','',[rfReplaceAll]),'svc:/milestone/network','require_all','none');
    end
  else
    begin
      servicename := Copy(GetFMRI,6,maxint);
      writeln('SWL: CREATE SVC IP HOSTNET ',ObjectName,' ',servicename);

      SetSvcNameandType(servicename,'FirmOS IP Service '+Field('IP_NET').asstring,'transient','core,signal',true);
      SetSvcEnvironment('/opt/local/fre','root','root','LANG=C');
      SetSvcStart('/opt/local/fre/bin/fossvc --start --ip='+UID.AsHexString,60);
      SetSvcStop('/opt/local/fre/bin/fossvc --stop --ip='+UID.AsHexString,60);
      AddSvcDependency('foscfg','svc:/fos/foscfg','require_all','none');
      AddSvcDependent (StringReplace(servicename,'/','',[rfReplaceAll]),'svc:/fos/fosip','require_all','none');
    end;
  fre_create_service(self);

  result.Field('fmri').asstring:=servicename;
  {$ENDIF}
end;


{ TFRE_DB_IPV6_ADDRESS }

procedure TFRE_DB_IPV6_HOSTNET.InternalSetIPCIDR(const value: TFRE_DB_String);
begin
  Field('ip_net').asstring:=value;
end;

class procedure TFRE_DB_IPV6_HOSTNET.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
    inherited RegisterSystemScheme(scheme);
    scheme.SetParentSchemeByName(TFRE_DB_IP_HOSTNET.Classname);
    scheme.AddSchemeField('ip_net',fdbft_String).SetupFieldDef(true,false,'','ipv6_subnet');
    scheme.AddSchemeField('slaac',fdbft_Boolean).addDepField('ip_net');
    scheme.AddSchemeField('gateway',fdbft_String).SetupFieldDef(false,false,'','ipv6');
    scheme.AddSchemeField('zoneid',fdbft_ObjLink).multiValues:=false;
    group:=scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
    group.AddInput('slaac',GetTranslateableTextKey('scheme_slaac'));
    group.AddInput('ip_net',GetTranslateableTextKey('scheme_ip_net'));
    group:=scheme.AddInputGroup('route').Setup(GetTranslateableTextKey('scheme_route_group'));
    group.AddInput('ip_net',GetTranslateableTextKey('scheme_ip_net'));
    group.AddInput('gateway',GetTranslateableTextKey('scheme_gateway'));
end;

class procedure TFRE_DB_IPV6_HOSTNET.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','IPv6 Properties');
    StoreTranslateableText(conn,'scheme_slaac','Stateless Adress Autoconfiguration');
    StoreTranslateableText(conn,'scheme_ip_net','IPv6/Subnet');
    StoreTranslateableText(conn,'scheme_gateway','Gateway');
    StoreTranslateableText(conn,'scheme_route_group','IPv6 Route Properties');
  end;
end;

procedure TFRE_DB_IPV6_HOSTNET.SetGatewayIP(const value: TFRE_DB_String);
begin
  Field('gateway').asstring:=value;
end;

function TFRE_DB_IPV6_HOSTNET.StartService: IFRE_DB_Object;
var linkname    : string;
    aliasname   : string;
    ip_hostnet  : string;
    errorstring : string;
    gateway     : string;
begin
  {$IFDEF SOLARIS}
  result := GFRE_DBI.NewObject;
  writeln('SWL: IPV6 START');
  if FieldExists('gateway') then
    begin
      ip_hostnet := Field('ip_net').asstring;
      gateway    := Field('gateway').asstring;
      writeln('SWL: ADD ROUTING ',ip_hostnet,' GW ',gateway);
      if add_ipv6routing(gateway,ip_hostnet,errorstring) then
        begin
          result.Field('STARTED').asboolean:=true;
        end
      else
        begin
          result.Field('STARTED').asboolean:= false;
          result.Field('ERROR').asstring   := errorstring;
        end;
    end
  else
    begin
      linkname   := Field('datalinkname').asstring;
      aliasname  := GetAddrObjAlias;
      if FieldExists('slaac') and Field('slaac').AsBoolean=true then
        begin
         if create_ipv6slaac(linkname,'addrconf',errorstring) then
           begin
             result.Field('STARTED').asboolean:=true;
           end
         else
           begin
             result.Field('STARTED').asboolean:= false;
             result.Field('ERROR').asstring   := errorstring;
           end
        end
      else
        begin
          create_ipv6slaac(linkname,'addrconf',errorstring);  // ignore result, has to be activated before fixed ipv6 addressess
          ip_hostnet := Field('ip_net').asstring;
          if create_ipv6address(linkname,aliasname,ip_hostnet,errorstring) then
            begin
              result.Field('STARTED').asboolean:=true;
            end
          else
            begin
              result.Field('STARTED').asboolean:= false;
              result.Field('ERROR').asstring   := errorstring;
            end
        end;
     end;
  {$ENDIF}
end;

function TFRE_DB_IPV6_HOSTNET.StopService: IFRE_DB_Object;
var linkname    : string;
    aliasname   : string;
    errorstring : string;
    ip_hostnet  : string;
    gateway     : string;
begin
  {$IFDEF SOLARIS}
  result := GFRE_DBI.NewObject;
  writeln('SWL: IPV6 STOP');
  if FieldExists('gateway') then
    begin
      ip_hostnet := Field('ip_net').asstring;
      gateway    := Field('gateway').asstring;
      writeln('SWL: DELETE ROUTING ',ip_hostnet,' GW ',gateway);
      if delete_ipv6routing(gateway,ip_hostnet,errorstring) then
        begin
          result.Field('STOPPED').asboolean:=true;
        end
      else
        begin
          result.Field('STOPPED').asboolean:= false;
          result.Field('ERROR').asstring   := errorstring;
        end;
    end
  else
    begin
      linkname   := Field('datalinkname').asstring;
      aliasname  := GetAddrObjAlias;
      if FieldExists('slaac') and Field('slaac').AsBoolean=true then
        begin
          result.Field('STOPPED').asboolean:=true;  // dont remove addrconf on link, no further add or remove of ipv6 possible after that
          exit;
        end;
      if delete_ipaddress(linkname,aliasname,errorstring) then
        begin
          result.Field('STOPPED').asboolean:=true;
        end
      else
        begin
          result.Field('STOPPED').asboolean:= false;
          result.Field('ERROR').asstring   := errorstring;
        end;
    end;
  {$ENDIF}
end;

{ TFRE_DB_IPV4 }

procedure TFRE_DB_IPV4_HOSTNET.InternalSetIPCIDR(const value: TFRE_DB_String);
begin
  Field('ip_net').asstring:=value;
end;

class procedure TFRE_DB_IPV4_HOSTNET.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
    inherited RegisterSystemScheme(scheme);
    scheme.SetParentSchemeByName(TFRE_DB_IP_HOSTNET.Classname);
    scheme.AddSchemeField('ip_net',fdbft_String).SetupFieldDef(true,false,'','ip_subnet');
    scheme.AddSchemeField('dhcp',fdbft_Boolean).addDepField('ip_net');
    scheme.AddSchemeField('gateway',fdbft_String).SetupFieldDef(false,false,'','ip');
    scheme.AddSchemeField('zoneid',fdbft_ObjLink).multiValues:=false;
    group:=scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
    group.AddInput('dhcp',GetTranslateableTextKey('scheme_dhcp'));
    group.AddInput('ip_net',GetTranslateableTextKey('scheme_ip_net'));
    group:=scheme.AddInputGroup('route').Setup(GetTranslateableTextKey('scheme_route_group'));
    group.AddInput('ip_net',GetTranslateableTextKey('scheme_ip_net'));
    group.AddInput('gateway',GetTranslateableTextKey('scheme_gateway'));
end;

class procedure TFRE_DB_IPV4_HOSTNET.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','IPv4 Properties');
    StoreTranslateableText(conn,'scheme_ip_net','IP/Subnet');
    StoreTranslateableText(conn,'scheme_dhcp','DHCP');
    StoreTranslateableText(conn,'scheme_gateway','Gateway');
  end;
end;

procedure TFRE_DB_IPV4_HOSTNET.SetGatewayIP(const value: TFRE_DB_String);
begin
  Field('gateway').asstring:=value;
end;

function TFRE_DB_IPV4_HOSTNET.StartService: IFRE_DB_Object;
var linkname    : string;
    aliasname   : string;
    ip_hostnet  : string;
    errorstring : string;
    gateway     : string;
    res         : boolean;
begin
  {$IFDEF SOLARIS}
  result := GFRE_DBI.NewObject;
  if FieldExists('gateway') then
    begin
      ip_hostnet := Field('ip_net').asstring;
      gateway    := Field('gateway').asstring;
      writeln('SWL: ADD ROUTING ',ip_hostnet,' GW ',gateway);
      if add_ipv4routing(gateway,ip_hostnet,errorstring) then
        begin
          result.Field('STARTED').asboolean:=true;
        end
      else
        begin
          result.Field('STARTED').asboolean:= false;
          result.Field('ERROR').asstring   := errorstring;
        end;
    end
  else
    begin
      writeln('SWL: IPV4 START');
      linkname   := Field('datalinkname').asstring;
      aliasname  := GetAddrObjAlias;
      ip_hostnet := Field('ip_net').asstring;
      if FieldExists('dhcp') and Field('dhcp').AsBoolean=true then
        res := create_ipv4dhcp(linkname,aliasname,errorstring)
      else
        res := create_ipv4address(linkname,aliasname,ip_hostnet,errorstring);
      if res then
        begin
          result.Field('STARTED').asboolean:=true;
        end
      else
        begin
          result.Field('STARTED').asboolean:= false;
          result.Field('ERROR').asstring   := errorstring;
        end;
     end;
  {$ENDIF}
end;

function TFRE_DB_IPV4_HOSTNET.StopService: IFRE_DB_Object;
var linkname    : string;
    aliasname   : string;
    ip_hostnet  : string;
    errorstring : string;
    gateway     : string;
begin
  {$IFDEF SOLARIS}
  result := GFRE_DBI.NewObject;
  if FieldExists('gateway') then
    begin
      ip_hostnet := Field('ip_net').asstring;
      gateway    := Field('gateway').asstring;
      writeln('SWL: DELETE ROUTING ',ip_hostnet,' GW ',gateway);
      if delete_ipv4routing(gateway,ip_hostnet,errorstring) then
        begin
          result.Field('STOPPED').asboolean:=true;
        end
      else
        begin
          result.Field('STOPPED').asboolean:= false;
          result.Field('ERROR').asstring   := errorstring;
        end;
    end
  else
    begin
      writeln('SWL: IPV4 STOP');
      linkname   := Field('datalinkname').asstring;
      aliasname  := GetAddrObjAlias;
      if delete_ipaddress(linkname,aliasname,errorstring) then
        begin
          result.Field('STOPPED').asboolean:=true;
        end
      else
        begin
          result.Field('STOPPED').asboolean:= false;
          result.Field('ERROR').asstring   := errorstring;
        end
     end;
  {$ENDIF}
end;

{ TFRE_DB_CPE_NETWORK_SERVICE }

class procedure TFRE_DB_CPE_NETWORK_SERVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_SERVICE.ClassName);
  scheme.AddSchemeField('ipv4_forward',fdbft_Boolean);
end;

class procedure TFRE_DB_CPE_NETWORK_SERVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

function TFRE_DB_CPE_NETWORK_SERVICE.ConfigureHAL: integer;
var icount     : integer;
    outstring  : string;


  procedure _NetIterator(const device:IFRE_DB_Object);
  var phys       : TFRE_DB_DATALINK_PHYS;
      iptun      : TFRE_DB_DATALINK_IPTUN;
      ipv4       : TFRE_DB_IPV4_HOSTNET;
      ipv6       : TFRE_DB_IPV6_HOSTNET;
      resultcode : integer;
      param      : string;

    procedure _IPIterator (const ip:IFRE_DB_Object);
    var devname:string;
    begin
      if ip.IsA(TFRE_DB_IPV4_HOSTNET,ipv4) then
        begin
          if icount=0 then
            devname := phys.ObjectName
          else
            begin
              devname := phys.ObjectName+':'+inttostr(icount);
              ExecuteCMD('ifconfig '+devname+' down',outstring,true);
            end;
          inc(icount);
          if (ipv4.FieldExists('dhcp')) and (ipv4.Field('dhcp').asboolean=true) then
            begin
              param :='-v -pf /run/dhclient.'+devname+'.pid -lf /var/lib/dhcp/dhclient.'+devname+'.leases '+devname;
              writeln('DHCLIENT:','dhclient ',param);
              ExecuteCMD('ifconfig '+devname+' up',outstring);
              ExecuteProcess('/sbin/dhclient',param,[]);
            end
          else
            if ipv4.Field('ip_net').asstring<>'' then
              ExecuteCMD('ifconfig '+devname+' '+ipv4.Field('ip_net').asstring+' up',outstring);
        end;
      if ip.IsA(TFRE_DB_IPV6_HOSTNET,ipv6) then
        begin
          devname := phys.ObjectName;
          if (ipv6.FieldExists('slaac')) and (ipv6.Field('slaac').asboolean=true) then
            ExecuteCMD('ifconfig '+devname+' inet6 up',outstring)
          else
            if ipv6.Field('ip_net').asstring<>'' then
              ExecuteCMD('ifconfig '+devname+' add '+ipv6.Field('ip_net').asstring+' up',outstring);
        end;
    end;

    procedure _IPTunIterator (const ip:IFRE_DB_Object);
    var devname:string;
    begin
      devname := iptun.ObjectName;
      if ip.IsA(TFRE_DB_IPV4_HOSTNET,ipv4) then
        begin
          if ipv4.Field('ip_net').asstring<>'' then
            ExecuteCMD('ip addr add '+ipv4.Field('ip_net').asstring+' dev '+devname,outstring);
        end;
      if ip.IsA(TFRE_DB_IPV6_HOSTNET,ipv6) then
        begin
          if ipv6.Field('ip_net').asstring<>'' then
            ExecuteCMD('ip -6 addr add '+ipv6.Field('ip_net').asstring+' dev '+devname,outstring);
        end;
    end;

  begin
    if device.IsA(TFRE_DB_DATALINK_PHYS,phys) then
      begin
        writeln('PHYS:',phys.ObjectName);
        ExecuteCMD('/sbin/dhclient -r '+phys.ObjectName,outstring,true);
        ExecuteCMD('ifconfig '+phys.Objectname+' down',outstring,true);
        icount := 0;
        device.ForAllObjects(@_IPIterator);
        ExecuteCMD('ifconfig '+phys.Objectname+' up',outstring,true);
      end;
    if device.IsA(TFRE_DB_DATALINK_IPTUN,iptun) then
      begin
        writeln('IPTUN:',iptun.ObjectName);
        if iptun.Field('mode').asstring='ip6ip6' then
          begin
            ExecuteCMD('ip -6 tunnel del '+iptun.Objectname,outstring,true);
            ExecuteCMD('ip -6 tunnel add '+iptun.Objectname+' mode '+iptun.Field('mode').asstring+' remote '+iptun.Field('REMOTE_IP_NET_IPV6').asstring+' local '+iptun.Field('LOCAL_IP_NET_IPV6').asstring+' dev '+iptun.Field('device').asstring,outstring);
          end
        else
          begin
            ExecuteCMD('ip tunnel del '+iptun.Objectname,outstring,true);
            ExecuteCMD('ip tunnel add '+iptun.Objectname+' mode '+iptun.Field('mode').asstring+' remote '+iptun.Field('REMOTE_IP_NET_IPV4').asstring+' local '+iptun.Field('LOCAL_IP_NET_IPV4').asstring+' dev '+iptun.Field('device').asstring,outstring);
          end;
        device.ForAllObjects(@_IPTunIterator);
        ExecuteCMD('ip link set '+iptun.Objectname+' up',outstring,true);
      end;
    if device.IsA(TFRE_DB_IPV6_HOSTNET,ipv6) then
      begin
        if ipv6.FieldExists('gateway') then
          begin
            writeln('ROUTE6:',ipv6.ObjectName);
            ExecuteCMD('ip -6 route add '+ ipv6.Field('ip_net').asstring+' via '+ipv6.Field('gateway').asstring,outstring,false);
          end;
      end;
    if device.IsA(TFRE_DB_IPV4_HOSTNET,ipv4) then
      begin
        if ipv4.FieldExists('gateway') then
          begin
            writeln('ROUTE4:',ipv4.ObjectName);
            ExecuteCMD('ip route add '+ ipv4.Field('ip_net').asstring+' via '+ipv4.Field('gateway').asstring,outstring,false);
          end;
      end;

  end;

begin
  writeln('SWL:CONFIGUREHAL');
  ClearErrors;
  ForAllObjects(@_NetIterator);
  if field('ipv4_forward').asboolean=true then
    begin
      ExecuteCMD('sysctl -w net.ipv4.ip_forward=1',outstring);
    end
  else
    begin
      ExecuteCMD('sysctl -w net.ipv4.ip_forward=0',outstring);
    end;
  writeln('ERRORS:',Field('errors').asobject.DumpToString());
end;

{ TFRE_DB_CPE_VPN_SERVICE }

class procedure TFRE_DB_CPE_OPENVPN_SERVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_VPN.ClassName);
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('server',fdbft_Boolean).required:=true;
  scheme.AddSchemeField('device',fdbft_String).required:=true;
  scheme.AddSchemeField('protocol',fdbft_String).required:=true;
  scheme.AddSchemeField('remote',fdbft_String).multiValues:=true;
  scheme.AddSchemeField('ca',fdbft_Stream).required:=true;
  scheme.AddSchemeField('crt',fdbft_Stream).required:=true;
  scheme.AddSchemeField('key',fdbft_Stream).required:=true;
  //TODO: OPENVPN Configuration
end;

class procedure TFRE_DB_CPE_OPENVPN_SERVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

function TFRE_DB_CPE_OPENVPN_SERVICE.ConfigureHAL: integer;
var sl        :TStringList;
    i         :integer;
    outstring :string;
    cmd       :string;
begin
  writeln('SWL:CONFIGUREHAL OPENVPN');
  ClearErrors;
  sl:=TStringList.Create;
  try
    sl.Add('client');
    sl.Add('dev '+Field('device').asstring);
    sl.Add('proto '+Field('protocol').asstring);
    for i:=0 to Field('remote').ValueCount-1 do
      begin
        sl.add('remote '+Field('remote').AsStringItem[i]);
      end;
    sl.add('remote-random');
    sl.add('script-security 2');
    if Pos('tap',Field('device').asstring)>0 then
      begin
        sl.add('up /etc/openvpn/'+lowercase(ObjectName)+'_tap_up.sh');
        sl.add('down /etc/openvpn/'+lowercase(ObjectName)+'_tap_down.sh');
      end;
    sl.add('nobind');
    sl.add('persist-key');
    sl.add('persist-tun');
    sl.add('ca /etc/openvpn/'+lowercase(ObjectName)+'_ca.crt');
    sl.add('cert /etc/openvpn/'+lowercase(ObjectName)+'_crt.crt');
    sl.add('key /etc/openvpn/'+lowercase(ObjectName)+'_crt.key');
    sl.add('log /var/log/openvpn_'+lowercase(ObjectName)+'.log');
    sl.add('verb 3');
    for i:=0 to Field('extra').ValueCount-1 do
      begin
        sl.add(Field('extra').AsStringItem[i]);
      end;
    sl.SaveToFile('/etc/openvpn/'+lowercase(ObjectName)+'.conf');

    Field('ca').AsStream.SaveToFile('/etc/openvpn/'+lowercase(ObjectName)+'_ca.crt');
    Field('crt').AsStream.SaveToFile('/etc/openvpn/'+lowercase(ObjectName)+'_crt.crt');
    Field('key').AsStream.SaveToFile('/etc/openvpn/'+lowercase(ObjectName)+'_crt.key');

    if Pos('tap',Field('device').asstring)>0 then
      begin
        sl.Clear;
        sl.add('#!/bin/sh');
        sl.add('/sbin/ifconfig $dev up');
        sl.add('/sbin/brctl addbr br0');
        sl.add('/sbin/brctl addif br0 $dev eth1');
        sl.add('/sbin/ifconfig br0 '+Field('bridgeip').asstring+' up');
        sl.SaveToFile('/etc/openvpn/'+lowercase(ObjectName)+'_tap_up.sh');
        ExecuteCMD('chmod 755 /etc/openvpn/'+lowercase(ObjectName)+'_tap_up.sh',outstring,false);
        sl.Clear;
        sl.add('#!/bin/sh');
        sl.add('/sbin/ifconfig br0 down');
        sl.add('/sbin/brctl delbr br0');
        sl.SaveToFile('/etc/openvpn/'+lowercase(ObjectName)+'_tap_down.sh');
        ExecuteCMD('chmod 755 /etc/openvpn/'+lowercase(ObjectName)+'_tap_down.sh',outstring,false);
      end;
  finally
    sl.Free;
  end;
  ExecuteCMD('modprobe cryptodev',outstring);
  cmd := '--writepid /var/run/openvpn.'+lowercase(ObjectName)+'.pid --daemon ovpn-'+lowercase(ObjectName)+' --status /var/run/openvpn.'+lowercase(ObjectName)+'.status 10 --cd /etc/openvpn --config /etc/openvpn/'+lowercase(ObjectName)+'.conf';
  writeln('OPENVPN CMD:','/usr/sbin/openvpn ',cmd);
  ExecuteProcess('/usr/sbin/openvpn',cmd,[]);
//  ExecuteCMD('/usr/sbin/openvpn --writepid /var/run/openvpn.'+lowercase(ObjectName)+'.pid --daemon ovpn-'+lowercase(ObjectName)+' --status /var/run/openvpn.'+lowercase(ObjectName)+'.status 10 --cd /etc/openvpn --config /etc/openvpn/'+lowercase(ObjectName)+'.conf',outstring);
  writeln('OPENVPN STARTED');
  writeln('ERRORS:',Field('errors').asobject.DumpToString());
end;

{ TFRE_DB_CPE_DHCP_SERVICE }

class procedure TFRE_DB_CPE_DHCP_SERVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
end;

class procedure TFRE_DB_CPE_DHCP_SERVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

function TFRE_DB_CPE_DHCP_SERVICE.ConfigureHAL: integer;
var sl        : TStringList;
    outstring : string;
    cmd,param : string;

  procedure _SubnetIterator(const obj:IFRE_DB_Object);
  var resultcode : integer;
      outstring  : string;
      subnet     : TFRE_DB_DHCP_Subnet;
      ip,mask    : string;

    procedure _FixedIterator(const fobj:IFRE_DB_Object);
    var fixed    : TFRE_DB_DHCP_Fixed;
    begin
      if fobj.IsA(TFRE_DB_DHCP_Fixed,fixed) then
        begin
          sl.add('');
          sl.add('  host '+fixed.ObjectName+' {');
          sl.add('    hardware ethernet '+fixed.Field('mac').asstring+';');
          sl.add('    fixed-address '+fixed.Field('ip').asstring+';');
          sl.add('  }');
        end;
    end;

  begin
    if obj.IsA(TFRE_DB_DHCP_Subnet,subnet) then
      begin
        sl.add('');
        SplitCIDR(subnet.Field('subnet').asstring,ip,mask);
        sl.add('subnet '+ip+' netmask '+mask+' {');
        if (subnet.Field('range_start').asstring<>'') and (subnet.Field('range_end').asstring<>'') then
          sl.add(' range '+subnet.Field('range_start').asstring+' '+subnet.Field('range_end').asstring+';');
        sl.add(' option routers '+subnet.Field('router').asstring+';');
        sl.add(' option domain-name-servers '+subnet.Field('dns').asstring+';');
        sl.add(' option tftp66  "'+subnet.Field('option_tftp66').asstring+'";');

        subnet.ForAllObjects(@_FixedIterator);

        sl.Add('}');
      end;
  end;


begin
  sl:=TStringList.Create;
  try
    writeln('SWL:CONFIGUREHAL DHCP');
    ClearErrors;
    ExecuteCMD('pkill dhcpd',outstring);
    writeln('killed');

    sl.add('default-lease-time 600;');
    sl.add('max-lease-time 7200;');
    sl.add('ddns-update-style none;');

    sl.add('authoritative;');
    sl.add('log-facility local7;');
    sl.add('option tftp66 code 66 = string;');

    ForAllObjects(@_SubnetIterator);

    sl.SaveToFile('/etc/dhcp/dhcpd.conf');

    cmd   := '/usr/sbin/dhcpd';
    param := '-q -cf /etc/dhcp/dhcpd.conf -pf /var/run/dhcpd.pid';
    writeln('DHCP CMD:',cmd,param);
    ExecuteProcess(cmd,param,[]);

    writeln('ERRORS:',Field('errors').asobject.DumpToString());
  finally
    sl.Free;
  end;
end;

{ TFRE_DB_CPE_VIRTUAL_FILESERVER }

class procedure TFRE_DB_CPE_VIRTUAL_FILESERVER.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_CRYPTO_FILESERVER.ClassName);
end;

class procedure TFRE_DB_CPE_VIRTUAL_FILESERVER.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

function TFRE_DB_CPE_VIRTUAL_FILESERVER.ConfigureHAL: integer;
var sl        : TStringList;
    outstring : string;
    cmd,param : string;

  procedure _FileshareIterator(const obj:IFRE_DB_Object);
  var resultcode : integer;
      fs         : TFRE_DB_VIRTUAL_FILESHARE;
  begin
    if obj.IsA(TFRE_DB_VIRTUAL_FILESHARE,fs) then
      begin
        sl.add('');
        sl.add('['+fs.ObjectName+']');
        sl.add('path = /cfiler/'+lowercase(fs.ObjectName));
        sl.add('public = yes');
        sl.add('writable = yes');
        sl.add('printable = no');
        sl.add('guest ok = yes');
      end;
  end;


begin
  sl:=TStringList.Create;
  try
    writeln('SWL:CONFIGUREHAL SAMBA');
    writeln(DumpToString());
    ClearErrors;


    sl.add('[global]');  // TODO
    sl.add('workgroup = secure');
    sl.add('netbios name = '+ObjectName);
    sl.add('log level = 3');
    sl.add('log file = /var/log/samba.log');
    sl.add('guest account = nobody');
    sl.add('map to guest = bad user');
    sl.add('security = user');

    ForAllObjects(@_FileshareIterator);

    sl.SaveToFile('/etc/samba/smb.conf');

    ExecuteCMD('mkdir /secfiler',outstring);
    ExecuteCMD('mkdir /cfiler',outstring);

    cmd:='mount -t nfs4 ';
    if field('nfsmountoptions').asstring<>'' then
      cmd := cmd+field('nfsmountoptions').asstring+' ';
    cmd := cmd + field('nfsshare').asstring+' /secfiler';

    ExecuteCMD(cmd,outstring);
    ExecuteCMD('losetup /dev/loop0 /secfiler/'+Field('cryptofile').asstring,outstring);
    ExecuteCMD('mount /dev/loop0 /cfiler',outstring);
    ExecuteCMD('mount -t ecryptfs -o ecryptfs_fnek_sig='+Field('ecryptfs_fnek_sig').asstring+' -o ecryptfs_sig='+ Field('ecryptfs_fnek_sig').asstring+
               ' -o ecryptfs_cipher=aes -o ecryptfs_key_bytes=16 -o no_sig_cache -o ecryptfs_enable_filename_crypto=y -o ecryptfs_passthrough=n '+
               '-o key=passphrase:passphrase_passwd='+ Field('ecryptfs_key').asstring+' /cfiler /cfiler',outstring);

    cmd   := '/usr/sbin/service';
    param := 'samba restart';
    writeln('SAMBA CMD:',cmd,param);
    ExecuteProcess(cmd,param,[]);

    writeln('ERRORS:',Field('errors').asobject.DumpToString());
  finally
    sl.Free;
  end;
end;

{ TFRE_DB_CRYPTOCPE }

class procedure TFRE_DB_CRYPTOCPE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_DEVICE.ClassName);
end;

class procedure TFRE_DB_CRYPTOCPE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Name');
    StoreTranslateableText(conn,'scheme_pmac','Mac');
  end;
end;

{ TFRE_DB_ASSET }

class procedure TFRE_DB_ASSET.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.ClassName);
  scheme.GetSchemeField('objname').required:=true;
end;

class procedure TFRE_DB_ASSET.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

{ TFRE_DB_NAS }

class procedure TFRE_DB_NAS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_SERVICE.ClassName);
end;

class procedure TFRE_DB_NAS.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

procedure TFRE_DB_NAS.CALC_GetDisplayName(const setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString('NAS: '+Field('objname').AsString);
end;

{ TFRE_DB_DNS }

class procedure TFRE_DB_DNS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_SERVICE.ClassName);
end;

class procedure TFRE_DB_DNS.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.1';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';
    StoreTranslateableText(conn,'caption','DNS');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Name');
  end;
end;

function TFRE_DB_DNS.GetFMRI: TFRE_DB_STRING;
begin
  result := 'svc:/fos/fos_dns';
end;

procedure TFRE_DB_DNS.CALC_GetDisplayName(const setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString('DNS: '+Field('objname').AsString);
end;

class function TFRE_DB_DNS.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

function TFRE_DB_DNS.RIF_CreateOrUpdateService: IFRE_DB_Object;
var servicename : string;
begin
  {$IFDEF SOLARIS}
    result := GFRE_DBI.NewObject;
    servicename := Copy(GetFMRI,6,maxint);
    SetSvcNameandType(servicename,'FirmOS DNS Service ','contract','core,signal',true);
    SetSvcEnvironment('/','root','root','LANG=C','basic,!proc_session,!proc_info,!file_link_any,net_privaddr,file_dac_read,file_dac_search,sys_resource,proc_chroot');
    SetSvcStart('/opt/local/lib/svc/method/named',60);
    SetSvcStop(':kill',60);
    SetSvcRestart(':kill -HUP',60);

    AddSvcDependency('loopback','svc:/network/loopback','require_any','error');
    AddSvcDependency('network','svc:/milestone/network','optional_all','error');
    AddSvcDependency('ldap','svc:/fos/fos_ldap','require_all','none');

        //<dependency name='config-files' grouping='require_any' restart_on='refresh' type='path'>
        //  <service_fmri value='file://localhost/opt/local/etc/named.conf'/>
        //</dependency>

        //  <exec_method name='start' type='method' exec='/opt/local/lib/svc/method/named %m %i' timeout_seconds='60'>
//
        //<property_group name='options' type='application'>
          //  <propval name='chroot_dir' type='astring' value=''/>
          //  <propval name='configuration_file' type='astring' value=''/>
          //  <propval name='debug_level' type='integer' value='0'/>
          //  <propval name='ip_interfaces' type='astring' value='all'/>
          //  <propval name='listen_on_port' type='integer' value='0'/>
          //  <propval name='server' type='astring' value=''/>
          //  <propval name='threads' type='integer' value='0'/>
          //</property_group>
          //<property_group name='general' type='framework'>
          //  <propval name='action_authorization' type='astring' value='solaris.smf.manage.bind'/>
          //  <propval name='value_authorization' type='astring' value='solaris.smf.manage.bind'/>
          //</property_group>

    fre_create_service(self);
    result.Field('fmri').asstring:=servicename;

  {$ENDIF}
end;

{ TFRE_DB_SERVICE_INSTANCE }

function TFRE_DB_SERVICE_INSTANCE.GetFMRI: TFRE_DB_String;
begin

end;

function TFRE_DB_SERVICE_INSTANCE.GetLogfileName: TFRE_DB_String;
begin

end;

function TFRE_DB_SERVICE_INSTANCE.GetServiceDescription: TFRE_DB_String;
begin

end;

function TFRE_DB_SERVICE_INSTANCE.GetState: TFRE_DB_String;
begin

end;

function TFRE_DB_SERVICE_INSTANCE.GetStateTime: TFRE_DB_DateTime64;
begin

end;

procedure TFRE_DB_SERVICE_INSTANCE.SetFMRI(AValue: TFRE_DB_String);
begin

end;

procedure TFRE_DB_SERVICE_INSTANCE.SetLogfileName(AValue: TFRE_DB_String);
begin

end;

procedure TFRE_DB_SERVICE_INSTANCE.SetServiceDescription(AValue: TFRE_DB_String);
begin

end;

procedure TFRE_DB_SERVICE_INSTANCE.SetState(AValue: TFRE_DB_String);
begin

end;

procedure TFRE_DB_SERVICE_INSTANCE.SetStateTime(AValue: TFRE_DB_DateTime64);
begin

end;

class procedure TFRE_DB_SERVICE_INSTANCE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

class procedure TFRE_DB_SERVICE_INSTANCE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

{ TFRE_DB_SERVICE_DOMAIN }

class procedure TFRE_DB_SERVICE_DOMAIN.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  ('TFRE_DB_OBJECTEX');
  scheme.AddSchemeField('serviceParent',fdbft_ObjLink);
  scheme.AddCalcSchemeField('displayname',fdbft_String,@CALC_GetDisplayName);
  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
end;

class procedure TFRE_DB_SERVICE_DOMAIN.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','General Information');
  end;
end;

procedure TFRE_DB_SERVICE_DOMAIN.CALC_GetDisplayName(const setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString('Domain: ' + Field('objname').AsString);
end;


{ TFRE_DB_ZIP_STATUS }

class procedure TFRE_ZIP_STATUS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  ('TFRE_DB_OBJECTEX');
  scheme.GetSchemeField('objname').required:=true;
end;

class procedure TFRE_ZIP_STATUS.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

{ TFRE_DB_HALCONFIG }

class procedure TFRE_DB_HALCONFIG.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  ('TFRE_DB_OBJECTEX');
end;

class procedure TFRE_DB_HALCONFIG.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;


{ TFRE_DB_OpenWifiNetwork }

class procedure TFRE_DB_OpenWifiNetwork.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

class procedure TFRE_DB_OpenWifiNetwork.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_WIFINETWORK');
end;

{ TFRE_DB_Monitoring_Status }

class procedure TFRE_DB_Monitoring_Status.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  ('TFRE_DB_OBJECTEX');
  scheme.AddSchemeField         ('provisioned_time',fdbft_DateTimeUTC);
  scheme.AddSchemeField         ('online_time',fdbft_DateTimeUTC);
  scheme.AddCalcSchemeField     ('status_icon',fdbft_String,@CALC_GetStatusIcon);
end;

class procedure TFRE_DB_Monitoring_Status.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
   if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

procedure TFRE_DB_Monitoring_Status.CALC_GetStatusIcon(const setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString(GetStatusIconURI(Field('status').asstring));
end;

{ TFRE_DB_AP_Lancom_OAP321 }

class procedure TFRE_DB_AP_Lancom_OAP321.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_AP_LANCOM');
end;

class procedure TFRE_DB_AP_Lancom_OAP321.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

procedure TFRE_DB_AP_Lancom_OAP321.CALC_GetDisplayName(const Setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString('Accesspoint Lancom OAP321 ('+Field('provisioningmac').AsString+')');
end;

{ TFRE_DB_AP_Lancom_IAP321 }

class procedure TFRE_DB_AP_Lancom_IAP321.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_AP_LANCOM');
end;

class procedure TFRE_DB_AP_Lancom_IAP321.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

procedure TFRE_DB_AP_Lancom_IAP321.CALC_GetDisplayName(const Setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString('Accesspoint Lancom IAP321 ('+Field('provisioningmac').AsString+')');
end;

{ TFRE_DB_AP_Lancom }

class procedure TFRE_DB_AP_Lancom.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

class procedure TFRE_DB_AP_Lancom.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_ACCESSPOINT');
end;

{ TFRE_DB_AP_Linksys_E1200V2 }

class procedure TFRE_DB_AP_Linksys_E1200V2.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_AP_LINKSYS_E1200');
end;

class procedure TFRE_DB_AP_Linksys_E1200V2.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

procedure TFRE_DB_AP_Linksys_E1200V2.CALC_GetDisplayName(const Setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString('Accesspoint Linksys E1200V2 ('+Field('provisioningmac').AsString+')');
end;

{ TFRE_DB_AP_Linksys_E1200 }

class procedure TFRE_DB_AP_Linksys_E1200.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_AP_LINKSYS');
end;

class procedure TFRE_DB_AP_Linksys_E1200.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

procedure TFRE_DB_AP_Linksys_E1200.CALC_GetDisplayName(const Setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString('Accesspoint Linksys E1200 ('+Field('provisioningmac').AsString+')');
end;

{ TFRE_DB_AP_Linksys_E1000 }

class procedure TFRE_DB_AP_Linksys_E1000.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_AP_LINKSYS');
end;

class procedure TFRE_DB_AP_Linksys_E1000.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

procedure TFRE_DB_AP_Linksys_E1000.CALC_GetDisplayName(const Setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString('Accesspoint Linksys E1000 ('+Field('reprovision').AsString+') ('+Field('provisioningmac').AsString+')');
end;

{ TFRE_DB_AP_Linksys }


class procedure TFRE_DB_AP_Linksys.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var
  group : IFRE_DB_InputGroupSchemeDefinition;
  enum: IFRE_DB_Enum;
begin
  inherited RegisterSystemScheme(scheme);

  enum:=GFRE_DBI.NewEnum('tcr_signal_status').Setup(GFRE_DBI.CreateText('$enum_tcr_signal_status','signal status Enum'));
  enum.addEntry('ok',GetTranslateableTextKey('enum_tcr_signal_status_ok'));
  enum.addEntry('warning',GetTranslateableTextKey('enum_tcr_signal_status_warning'));
  enum.addEntry('failure',GetTranslateableTextKey('enum_tcr_signal_status_failure'));
  enum.addEntry('unknown',GetTranslateableTextKey('enum_tcr_signal_status_unknown'));
  GFRE_DBI.RegisterSysEnum(enum);

  scheme.SetParentSchemeByName('TFRE_DB_ACCESSPOINT');

//  scheme.AddSchemeField('routing',fdbft_String).SetupFieldDef(true,false,'routing');
  scheme.AddSchemeField('vpn_crtid',fdbft_ObjLink);

  group:=scheme.AddInputGroup('options').Setup(GetTranslateableTextKey('scheme_options_group'));
//  group.AddInput('routing',GetTranslateableTextKey('scheme_routing'));
  group.AddInput('vpn_crtid',GetTranslateableTextKey('scheme_vpn_cert'),false,false,'certificate');
end;

class procedure TFRE_DB_AP_Linksys.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_options_group','Device Options');
    StoreTranslateableText(conn,'scheme_routing','Routing');
    StoreTranslateableText(conn,'scheme_vpn_cert','VPN Certificate');

    StoreTranslateableText(conn,'enum_routing_enabled','Enabled');
    StoreTranslateableText(conn,'enum_routing_disabled','Disabled');
    StoreTranslateableText(conn,'enum_routing_nat','NAT');
  end;
end;

function TFRE_DB_AP_Linksys.IMI_Configuration(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
  scheme: IFRE_DB_SchemeObject;
begin
  result:=inherited;

  scheme := GetScheme;
  res:=result.Implementor_HC as TFRE_DB_FORM_PANEL_DESC;
  res.AddSchemeFormGroup(scheme.GetInputGroup('options'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));

end;

{ TFRE_DB_Accesspoint }



class procedure TFRE_DB_Accesspoint.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_ENDPOINT');

  scheme.AddSchemeField('external_ip',fdbft_String).SetupFieldDef(False,False,'','ip');
  scheme.AddSchemeField('dhcp',fdbft_Boolean).addDepField('external_ip');
  scheme.AddSchemeField('channel',fdbft_UInt16).required:=true;
  scheme.AddSchemeField('password',fdbft_String);
  scheme.AddSchemeField('serialnumber',fdbft_String);
  scheme.AddSchemeField('mountingdetail',fdbft_String);

  group:=scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.UseInputGroup('TFRE_DB_ENDPOINT','main');
  group.AddInput('serialnumber',GetTranslateableTextKey('scheme_serial'));
  group.AddInput('external_ip',GetTranslateableTextKey('scheme_exip'));
  group.AddInput('dhcp',GetTranslateableTextKey('scheme_dhcp'));
  group.AddInput('channel',GetTranslateableTextKey('scheme_channel'));
  group.AddInput('password',GetTranslateableTextKey('scheme_pw'));
end;

class procedure TFRE_DB_Accesspoint.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','Accesspoint Configuration');
    StoreTranslateableText(conn,'scheme_serial','Serialnumber');
    StoreTranslateableText(conn,'scheme_exip','IP');
    StoreTranslateableText(conn,'scheme_dhcp','DHCP');
    StoreTranslateableText(conn,'scheme_channel','Channel');
    StoreTranslateableText(conn,'scheme_pw','Password');
  end;
end;



class function TFRE_DB_Accesspoint.WBC_NewOperation(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var dbc        :   IFRE_DB_CONNECTION;
    raw_object :   IFRE_DB_Object;
    dhcp       :   boolean;
    mac        :   string;
    dhcp_id    :   TFRE_DB_GUID;


begin
  writeln ('AP AddOperation');
  writeln(input.DumpToString());
  dbc          := input.GetReference as IFRE_DB_CONNECTION;
  raw_object   := input.Field('data').AsObject;

  dhcp    := raw_object.Field('dhcp').asboolean;
  mac     := lowercase(raw_object.Field('provisioningmac').AsString);
  writeln('now get dhcp');
  dhcp_id := GetService(dbc,'TFRE_DB_DHCP');
  raw_object.Field('reprovision').AsString:='true';

  Result:=inherited WBC_NewOperation(input,ses,app,conn);
  writeln ('After new AP');
  AccesspointOnChange (dbc, dhcp, dhcp_id, mac);
end;

function TFRE_DB_Accesspoint.WEB_Menu(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res       :   TFRE_DB_MENU_DESC;
  has_open  :   boolean;
  has_wpa2  :   boolean;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  if HasAnotherAP(Field('site').asguid,conn)=false then begin
   HasNets(self, has_open,has_wpa2);
   if not has_open then res.AddEntry.Describe('Add Open Wifi Network','images_apps/cloudcontrol/add_open_wifi.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'addopenwifinetwork'));
   if not has_wpa2 then res.AddEntry.Describe('Add WPA2 Wifi Network','images_apps/cloudcontrol/add_wpa2_wifi.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'addwpa2network'));
  end;
  res.AddEntry.Describe('Update Provisioning','images_apps/cloudcontrol/update_provisioning.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'provision'));
  res.AddEntry.Describe('Delete','images_apps/cloudcontrol/delete_accesspoint.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'deleteOperation'));
  Result:=res;
end;


function TFRE_DB_Accesspoint.HasAnotherAP(const site_id: TFRE_DB_GUID; const conn: IFRE_DB_CONNECTION): boolean;
var site_object       : IFRE_DB_Object;
    childs            : TFRE_DB_GUIDArray;
    i                 : integer;
    has_open          : boolean;
    has_wpa2          : boolean;
    ep_obj            : IFRE_DB_Object;
begin
  result:=false;
  conn.Fetch(site_id,site_object);
  if assigned(site_object) then begin
    writeln (site_object.DumpToString());
    //TODO Fix
    abort;
//    childs:=site_object.ReferencedByList('TFRE_DB_ACCESSPOINT');
    for i := 0 to Length(childs) - 1 do begin
      if FREDB_Guids_Same(UID,childs[i])=false then begin
        conn.Fetch(childs[i],ep_obj);
        HasNets(ep_obj,has_open,has_wpa2);
        if (has_open or has_wpa2) then begin
         result := true;
         break;
        end;
      end;
    end;
  end;
end;


class procedure TFRE_DB_Accesspoint.AccessPointOnChange(const conn: IFRE_DB_CONNECTION; const is_dhcp: boolean; const dhcp_id: TFRE_DB_GUID; const mac: TFRE_DB_String);
var dhcp_obj        : IFRE_DB_Object;
    childs          : TFRE_DB_GUIDArray;
    dhcp_fixed_obj  : IFRE_DB_Object;
    i               : integer;
    collf           : IFRE_DB_Collection;
    current_ip      : string;
    highest         : integer;
    sub_ip          : integer;
begin
  writeln('PostSave');
  if is_dhcp then begin
    writeln('DHCP');
    CheckDbResult(conn.Fetch(dhcp_id,dhcp_obj),'NO DHCP SERVICE FOUND IN AP AFTER SAVE');
    writeln('ChiLDS');
    writeln(dhcp_obj.DumpToString);
    current_ip := dhcp_obj.Field('fixed_start').AsString;
    writeln(current_ip);
    highest    := StringtoIP4(current_ip)._bytes[3];                    // TODO implement for other classes than /24
    abort;
    //TODO FIX
    //childs     := dhcp_obj.ReferencedByList('TFRE_DB_DHCP_FIXED');
    for i := 0 to Length(childs) - 1 do begin
      writeln('FETCH ChiLDS');
      conn.Fetch(childs[i],dhcp_fixed_obj);
      if lowercase(dhcp_fixed_obj.Field('mac').AsString)=mac then begin
        writeln('ALREADY IN DHCP !');
        exit;
      end else begin
        current_ip := dhcp_fixed_obj.Field('ip').AsString;
        sub_ip     := StringtoIP4(current_ip)._bytes[3];
        writeln(sub_ip);
        if sub_ip>highest then highest:=sub_ip;
      end;
    end;
    writeln('HIGHEST :', highest);
    inc(highest);
    // create new dhcp_fixed
    writeln('NOW ADD DHCP');
    collf          := conn .GetCollection('dhcp_fixed');
    dhcp_fixed_obj := GFRE_DBI.NewObjectScheme(TFRE_DB_DHCP_Fixed);
    dhcp_fixed_obj.Field('ip').AsString      := GetIPDots(dhcp_obj.Field('fixed_start').AsString,3)+inttostr(highest);
    dhcp_fixed_obj.Field('mac').AsString     := lowercase (mac);
    dhcp_fixed_obj.Field('objname').AsString := 'Automatic'+StringReplace(lowercase(mac),':','',[rfReplaceAll]);
    dhcp_fixed_obj.Field('dhcp').AsObjectLink:= dhcp_id;
    writeln('NOW STORE');
    CheckDBResult(COLLF.Store(dhcp_fixed_obj),'Add DHCP Fixed');                            // TODO Update DHCP Tree
    writeln ('DHCP FIXED CREATED !!');
  end;
end;


function TFRE_DB_Accesspoint.WEB_SaveOperation(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var scheme            : IFRE_DB_SCHEMEOBJECT;
    update_object_uid : TFRE_DB_GUID;
    raw_object        : IFRE_DB_Object;
    dhcp              : boolean;
    dhcp_id           : TFRE_DB_GUID;
    mac               : TFRE_DB_String;


begin
  if assigned(Parent) then begin
    result := TFRE_DB_MESSAGE_DESC(result.Implementor).Describe('SAVE','Error on saving! Saving of Subobject not supported!',fdbmt_error);
    exit;
  end;
  result            := nil;
  scheme            := GetScheme;
  update_object_uid := UID;
  raw_object        := input.Field('data').AsObject;

  scheme.SetObjectFieldsWithScheme(raw_object,self,false,conn);

  dhcp    := Field('dhcp').asboolean;
  mac     := lowercase(Field('provisioningmac').AsString);
  dhcp_id := GetService(conn,'TFRE_DB_DHCP');
  Field('reprovision').AsBoolean:=true;

  CheckDbResult(conn.Update(self),'failure on cloned/update');  // This instance is freed by now, so rely on the stackframe only (self) pointer is garbage(!!)
  result := GFRE_DB_NIL_DESC;

  AccesspointOnChange (conn, dhcp, dhcp_id, mac);
end;

{ TFRE_DB_Site_Captive_Extension }

class procedure TFRE_DB_Site_Captive_Extension.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('site',fdbft_ObjLink);
  scheme.AddSchemeField('captiveportal',fdbft_ObjLink);
end;

class procedure TFRE_DB_Site_Captive_Extension.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

{ TFRE_DB_Captiveportal }

class procedure TFRE_DB_Captiveportal.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_SERVICE');
  scheme.AddSchemeField('vpn_caid',fdbft_ObjLink);
end;

class procedure TFRE_DB_Captiveportal.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

function TFRE_DB_Captiveportal.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

function TFRE_DB_Captiveportal.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

{ TFRE_DB_REDIRECTION_FLOW }

class procedure TFRE_DB_REDIRECTION_FLOW.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('redirection_start',fdbft_ObjLink);
  scheme.AddSchemeField('redirection_customer',fdbft_ObjLink);
  scheme.AddSchemeField('redirection_agb_ipad',fdbft_ObjLink);
  scheme.AddSchemeField('redirection_agb_open',fdbft_ObjLink);
  scheme.AddSchemeField('redirection_end',fdbft_ObjLink);
end;

class procedure TFRE_DB_REDIRECTION_FLOW.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;


class procedure TFRE_DB_NETWORK_GROUP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('networks',fdbft_ObjLink).multiValues:=true;
  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('objname',GetTranslateableTextKey('scheme_name'));
  group.AddInput('networks',GetTranslateableTextKey('scheme_networks'));
end;

class procedure TFRE_DB_NETWORK_GROUP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','Network Group');
    StoreTranslateableText(conn,'scheme_name','Name');
    StoreTranslateableText(conn,'scheme_networks','Networks');
  end;
end;

class procedure TFRE_DB_CMS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('baseurl',fdbft_String).required:=true;
  scheme.AddSchemeField('urlexceptions',fdbft_String).multiValues:=true;
  group := scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.UseInputGroup('TFRE_DB_SERVICE','main');
  group.AddInput('baseurl',GetTranslateableTextKey('scheme_baseurl'));
  group.AddInput('urlexceptions',GetTranslateableTextKey('scheme_execeptions'));
end;

class procedure TFRE_DB_CMS.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','CMS');
    StoreTranslateableText(conn,'scheme_baseurl','Base URL');
    StoreTranslateableText(conn,'scheme_execeptions','Url Exceptions');
  end;
end;

function TFRE_DB_DEVICE.getProvisioningMac: TFRE_DB_String;
begin
  result:=Field('provisioningmac').AsString;
end;

procedure TFRE_DB_DEVICE.setProvisioningMac(AValue: TFRE_DB_String);
begin
  Field('provisioningmac').AsString:=AValue;
end;

class procedure TFRE_DB_DEVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_ASSET.ClassName);
  scheme.AddSchemeField('provisioningmac',fdbft_String).SetupFieldDef(true,false,'','mac');
  scheme.AddSchemeField('provisioning_serial',fdbft_Int32);
  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('objname',GetTranslateableTextKey('scheme_objname'));
  group.AddInput('provisioningmac',GetTranslateableTextKey('scheme_pmac'));
end;

class procedure TFRE_DB_DEVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.1';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_pmac','Mac');
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';

    StoreTranslateableText(conn,'scheme_objname','Name');
  end;
end;

class procedure TFRE_DB_CMS_ADPAGE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_CMS_PAGE');
  scheme.AddSchemeField('start_time',fdbft_DateTimeUTC).required:=true;
  scheme.AddSchemeField('end_time',fdbft_DateTimeUTC).required:=true;
  scheme.AddSchemeField('start_daily',fdbft_String).required:=true;
  scheme.AddSchemeField('end_daily',fdbft_String).required:=true;
  scheme.AddSchemeField('insertpoint',fdbft_Int16).required:=true;
  scheme.AddSchemeField('max_inserts',fdbft_UInt32).required:=true;
  scheme.AddSchemeField('shown_inserts',fdbft_UInt32);
  scheme.AddSchemeField('networkgroups',fdbft_ObjLink).multiValues:=true;

  group:=scheme.ReplaceInputGroup('main').Setup('Page');
  group.UseInputGroup('TFRE_DB_CMS_PAGE','main');
  group.AddInput('start_time',GetTranslateableTextKey('scheme_starttime'));
  group.AddInput('end_time',GetTranslateableTextKey('scheme_endtime'));
  group.AddInput('start_daily',GetTranslateableTextKey('scheme_startdaily'));
  group.AddInput('end_daily',GetTranslateableTextKey('scheme_enddaily'));
  group.AddInput('insertpoint',GetTranslateableTextKey('scheme_insertpoint'));
  group.AddInput('max_inserts',GetTranslateableTextKey('scheme_max_inserts'));
  group.AddInput('shown_inserts',GetTranslateableTextKey('scheme_show_inserts'),true);
  group.AddInput('networkgroups',GetTranslateableTextKey('scheme_networkgroups'));
end;

class procedure TFRE_DB_CMS_ADPAGE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_starttime','Start Time');
    StoreTranslateableText(conn,'scheme_endtime','End Time');
    StoreTranslateableText(conn,'scheme_startdaily','Start Daily');
    StoreTranslateableText(conn,'scheme_enddaily','End Daily');
    StoreTranslateableText(conn,'scheme_insertpoint','Insertion Point');
    StoreTranslateableText(conn,'scheme_max_inserts','Maximum Insertion Count');
    StoreTranslateableText(conn,'scheme_show_inserts','Already Shown Inserts');
    StoreTranslateableText(conn,'scheme_networkgroups','Network Groups');
  end;
end;

class procedure TFRE_DB_WPA2Network.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_WIFINETWORK');
  scheme.AddSchemeField('wpa2psk',fdbft_String).required:=true;
  group:=scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.UseInputGroup('TFRE_DB_WIFINETWORK','main');
  group.AddInput('wpa2psk',GetTranslateableTextKey('scheme_wpa2psk'));
end;

class procedure TFRE_DB_WPA2Network.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','WPA2 Wifi Network');
    StoreTranslateableText(conn,'scheme_wpa2psk','WPA2PSK');
  end;
end;


{ TFRE_DB_CMS_PAGE }

class procedure TFRE_DB_CMS_PAGE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('cms',fdbft_ObjLink).required:=true;
  scheme.AddSchemeField('active',fdbft_Boolean).required:=true;
  scheme.AddSchemeField('relativeurl',fdbft_Boolean).required:=true;
  scheme.AddSchemeField('url',fdbft_String).required:=true;
  scheme.AddSchemeField('urlexceptions',fdbft_String).multiValues:=true;
  scheme.SetSysDisplayField(TFRE_DB_NameTypeArray.create('url'),'%s');

  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('cms','',false,true);
  group.AddInput('active',GetTranslateableTextKey('scheme_active'));
  group.AddInput('relativeurl',GetTranslateableTextKey('scheme_relurl'));
  group.AddInput('url',GetTranslateableTextKey('scheme_url'));
  group.AddInput('urlexceptions',GetTranslateableTextKey('scheme_exceptions'));
end;

class procedure TFRE_DB_CMS_PAGE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','Page');
    StoreTranslateableText(conn,'scheme_active','Active');
    StoreTranslateableText(conn,'scheme_relurl','Relative Url');
    StoreTranslateableText(conn,'scheme_url','Url');
    StoreTranslateableText(conn,'scheme_exceptions','Url Exceptions');
  end;
end;

function TFRE_DB_CMS_PAGE.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
  scheme: IFRE_DB_SchemeObject;
begin
  scheme := GetScheme;

  res:=TFRE_DB_FORM_PANEL_DESC.Create.Describe('CMS Page');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_CMS_PAGE.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Delete','images_apps/cloudcontrol/delete_cms_page.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'deleteOperation'));
  Result:=res;
end;


{ TFRE_DB_Routing }

class procedure TFRE_DB_Routing.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_SERVICE');
  scheme.AddSchemeField('default',fdbft_String).required:=true;
  //scheme.AddSchemeFieldSubscheme('static','TFRE_DB_ROUTE').multiValues:=true; { FIXXME:HH -> TFRE_DB_ROUTE }
  group:=scheme.GetInputGroup('main');
  group.AddInput('default',GetTranslateableTextKey('scheme_default'));
end;

class procedure TFRE_DB_Routing.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.1';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','Routing');
    StoreTranslateableText(conn,'scheme_default','Default Routing');
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';
    StoreTranslateableText(conn,'caption','Routing');
    StoreTranslateableText(conn,'scheme_objname','Name');
  end;
end;

class function TFRE_DB_Routing.OnlyOneServicePerZone: boolean;
begin
  result := true;
end;

class function TFRE_DB_Routing.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

{ TFRE_DB_Radius }

class procedure TFRE_DB_Radius.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_SERVICE');
end;

class procedure TFRE_DB_Radius.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

{ TFRE_DB_VPN }

class procedure TFRE_DB_VPN.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.SetParentSchemeByName(TFRE_DB_SERVICE.ClassName);
end;

class procedure TFRE_DB_VPN.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.1';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';
    StoreTranslateableText(conn,'caption','VPN');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Name');
  end;
end;

function TFRE_DB_VPN.GetFMRI: TFRE_DB_STRING;
begin
  result := 'svc:/fos/fos_vpn_'+UID.AsHexString;
end;

class function TFRE_DB_VPN.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

function TFRE_DB_VPN.RIF_CreateOrUpdateService: IFRE_DB_Object;
var servicename : string;
begin
  {$IFDEF SOLARIS}
    result := GFRE_DBI.NewObject;
    servicename := Copy(GetFMRI,6,maxint);
    SetSvcNameandType(servicename,'FirmOS OpenVPN Service ','contract','core,signal',true);
    SetSvcEnvironment('/var/log','root','root','PATH=/usr/sbin:/usr/bin:/bin:/opt/local/sbin');
    SetSvcStart('/opt/local/sbin/openvpn --config --/opt/local/etc/openvpn/'+UID.AsHexString+'.conf --daemon',60);
    SetSvcStop(':kill',60);
    AddSvcDependency('net-physical','svc:/network/physical','require_all','none');
    AddSvcDependency('filesystem-local','svc:/system/filesystem/local','require_all','error');

    fre_create_service(self);
    result.Field('fmri').asstring:=servicename;

  {$ENDIF}
end;

{ TFRE_DB_DHCP_Fixed }

class procedure TFRE_DB_DHCP_Fixed.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('dhcp',fdbft_ObjLink).required:=true;
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('mac',fdbft_String).required:=true;
  scheme.AddSchemeField('ip',fdbft_String).required:=true;
  scheme.AddSchemeField('router',fdbft_String).multiValues:=true;
  scheme.AddSchemeField('dns',fdbft_String).multiValues:=true;

  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('dhcp','',false,true);
  group.AddInput('objname',GetTranslateableTextKey('scheme_objname'));
  group.AddInput('mac',GetTranslateableTextKey('scheme_mac'));
  group.AddInput('ip',GetTranslateableTextKey('scheme_ip'));
  group.AddInput('router',GetTranslateableTextKey('scheme_router'));
  group.AddInput('dns',GetTranslateableTextKey('scheme_dns'));
end;

class procedure TFRE_DB_DHCP_Fixed.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','DHCP Fixed');
    StoreTranslateableText(conn,'scheme_objname','Name');
    StoreTranslateableText(conn,'scheme_mac','Mac');
    StoreTranslateableText(conn,'scheme_ip','Ip');
    StoreTranslateableText(conn,'scheme_router','Router');
    StoreTranslateableText(conn,'scheme_dns','DNS');
  end;
end;

function TFRE_DB_DHCP_Fixed.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.Create.Describe('Fixed Host');
  res.AddSchemeFormGroup(getscheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_DHCP_Fixed.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Delete','images_apps/cloudcontrol/delete_fixed_dhcp.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'deleteOperation'));
  Result:=res;
end;

{ TFRE_DB_DHCP_Subnet }

class procedure TFRE_DB_DHCP_Subnet.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('dhcp',fdbft_ObjLink).required:=true;
  scheme.AddSchemeField('subnet',fdbft_String).required:=true;
  scheme.AddSchemeField('range_start',fdbft_String).required:=true;
  scheme.AddSchemeField('range_end',fdbft_String).required:=true;
  scheme.AddSchemeField('router',fdbft_String).multiValues:=true;
  scheme.AddSchemeField('dns',fdbft_String).multiValues:=true;

  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('dhcp','',false,true);
  group.AddInput('subnet',GetTranslateableTextKey('scheme_subnet'));
  group.AddInput('range_start',GetTranslateableTextKey('scheme_range_start'));
  group.AddInput('range_end',GetTranslateableTextKey('scheme_range_end'));
  group.AddInput('router',GetTranslateableTextKey('scheme_router'));
  group.AddInput('dns',GetTranslateableTextKey('scheme_dns'));
end;

class procedure TFRE_DB_DHCP_Subnet.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','DHCP Subnet');
    StoreTranslateableText(conn,'scheme_subnet','Subnet');
    StoreTranslateableText(conn,'scheme_range_start','Range Start');
    StoreTranslateableText(conn,'scheme_range_end','Range End');
    StoreTranslateableText(conn,'scheme_router','Router');
    StoreTranslateableText(conn,'scheme_dns','DNS');
  end;
end;

function TFRE_DB_DHCP_Subnet.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.Create.Describe('Subnet');
  res.AddSchemeFormGroup(getscheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_DHCP_Subnet.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Delete','images_apps/cloudcontrol/delete_subnet.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'deleteOperation'));
  Result:=res;
end;

{ TFRE_DB_DHCP }

class procedure TFRE_DB_DHCP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_SERVICE');
  scheme.AddSchemeField('default_domainname',fdbft_String).required:=true;
  scheme.AddSchemeField('default_dns',fdbft_String).required:=true;
  scheme.AddSchemeField('default_leasetime',fdbft_Int16).required:=true;
  scheme.AddSchemeField('fixed_start',fdbft_String).required:=true;
  scheme.AddSchemeField('fixed_end',fdbft_String).required:=true;


  group:=scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.UseInputGroup('TFRE_DB_SERVICE','main');
  group.AddInput('default_domainname',GetTranslateableTextKey('scheme_default_domainname'));
  group.AddInput('default_dns',GetTranslateableTextKey('scheme_default_domainname'));
  group.AddInput('default_leasetime',GetTranslateableTextKey('scheme_default_leasetime'));
  group.AddInput('fixed_start',GetTranslateableTextKey('scheme_fixed_start'));
  group.AddInput('fixed_end',GetTranslateableTextKey('scheme_fixed_end'));
end;

class procedure TFRE_DB_DHCP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.1';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_default_domainname','Default Domainname');
    StoreTranslateableText(conn,'scheme_default_dns','Default DNS');
    StoreTranslateableText(conn,'scheme_default_leasetime','Default Leasetime');
    StoreTranslateableText(conn,'scheme_fixed_start','Begin of fixed addresses');
    StoreTranslateableText(conn,'scheme_fixed_end','End of fixed addresses');
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';
    StoreTranslateableText(conn,'caption','DHCP');
  end;
end;

function TFRE_DB_DHCP.GetFMRI: TFRE_DB_STRING;
begin
  result := 'svc:/fos/fos_dhcpd';
end;

function TFRE_DB_DHCP.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.Create.Describe('DHCP Service');
  res.AddSchemeFormGroup(getscheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_DHCP.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Add Subnet','images_apps/cloudcontrol/add_subnet.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'addSubnet'));
  res.AddEntry.Describe('Add Fixed Host','images_apps/cloudcontrol/add_fixed_host.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'addFixedHost'));
  Result:=res;
end;

function TFRE_DB_DHCP.WEB_ChildrenData(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res   : TFRE_DB_STORE_DATA_DESC;
  entry : IFRE_DB_Object;
  childs: TFRE_DB_GUIDArray;
  i     : Integer;
  dbo   : IFRE_DB_Object;
  txt   : String;

begin
  res := TFRE_DB_STORE_DATA_DESC.create;
  //FIXME
  //childs:=ReferencedByList;
  abort;
  for i := 0 to Length(childs) - 1 do begin
    conn.Fetch(childs[i],dbo);
    if dbo.IsA('TFRE_DB_DHCP_SUBNET') or dbo.IsA('TFRE_DB_DHCP_FIXED') then begin
      if dbo.IsA('TFRE_DB_DHCP_SUBNET') then begin
        txt:=dbo.field('subnet').AsString;
      end else begin
        txt:=dbo.field('objname').AsString;
      end;

      entry:=GFRE_DBI.NewObject;
      entry.Field('text').AsString:=txt;
      entry.Field('uid').AsGUID:=dbo.UID;
      entry.Field('uidpath').AsStringArr:=dbo.GetUIDPath;
      entry.Field('_funcclassname_').AsString:=dbo.SchemeClass;
      entry.Field('_childrenfunc_').AsString:='ChildrenData';
      entry.Field('_menufunc_').AsString:='Menu';
      entry.Field('_contentfunc_').AsString:='Content';
      res.addEntry(entry);

    end;
  end;
  Result:=res;
end;

function TFRE_DB_DHCP.IMI_addSubnet(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res       :TFRE_DB_FORM_DIALOG_DESC;
  scheme    : IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  GFRE_DBI.GetSystemScheme(TFRE_DB_DHCP_SUBNET,scheme);
  res:=TFRE_DB_FORM_DIALOG_DESC.Create.Describe('Add Subnet');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.SetElementValue('dhcp',UID_String);
  serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_DHCP_SUBNET','newOperation');
  serverFunc.AddParam.Describe('collection','dhcp_subnet');
  res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_DHCP.IMI_addFixedHost(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res       : TFRE_DB_FORM_DIALOG_DESC;
  scheme    : IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  GFRE_DBI.GetSystemScheme(TFRE_DB_DHCP_FIXED,scheme);
  res:=TFRE_DB_FORM_DIALOG_DESC.Create.Describe('Add Subnet');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.SetElementValue('dhcp',UID_String);
  serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_DHCP_FIXED','newOperation');
  serverFunc.AddParam.Describe('collection','dhcp_fixed');
  res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  Result:=res;
end;

class function TFRE_DB_DHCP.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

function TFRE_DB_DHCP.RIF_CreateOrUpdateService: IFRE_DB_Object;
var servicename : string;
begin
  {$IFDEF SOLARIS}
    result := GFRE_DBI.NewObject;
    servicename := Copy(GetFMRI,6,maxint);
    SetSvcNameandType(servicename,'FirmOS DHCPD Service ','contract','core,signal',true);
    SetSvcEnvironment('/','root','root','LANG=C','');
    SetSvcStart('/opt/local/lib/svc/method/isc-dhcpd start',30);
    SetSvcStop('/opt/local/lib/svc/method/isc-dhcpd stop',30);
    SetSvcRestart('/opt/local/lib/svc/method/isc-dhcpd refresh',30);

    AddSvcDependency('filesystem-local','svc:/system/filesystem/local:default','require_all','none');
    AddSvcDependency('network','svc:/milestone/network','require_all','error');
//    AddSvcDependency('ldap','svc:/fos/fos_ldap','require_all','none');

    fre_create_service(self);
    result.Field('fmri').asstring:=servicename;

  {$ENDIF}
end;



{ TFRE_DB_Certificate }

class procedure TFRE_DB_Certificate.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('ca',fdbft_ObjLink).required:=true;
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('c',fdbft_String);
  scheme.AddSchemeField('email',fdbft_String);
  scheme.AddSchemeField('st',fdbft_String);
  scheme.AddSchemeField('l',fdbft_String);
  scheme.AddSchemeField('ou',fdbft_String);
  scheme.AddSchemeField('crt_stream',fdbft_Stream);
  scheme.AddSchemeField('key_stream',fdbft_Stream);
  scheme.AddSchemeField('issued',fdbft_DateTimeUTC);
  scheme.AddSchemeField('revoked',fdbft_DateTimeUTC);
  scheme.AddSchemeField('valid',fdbft_DateTimeUTC);
  scheme.AddSchemeField('server',fdbft_Boolean);
  scheme.SetSysDisplayField(TFRE_DB_NameTypeArray.Create('cn'),'%s');

  group:=scheme.AddInputGroup('main_create').Setup('scheme_main_group');
  group.AddInput('ca','',false,true);
  group.AddInput('objname','scheme_cn');
  group.AddInput('email','scheme_email');
  group.AddInput('l','scheme_l');
  group.AddInput('ou','scheme_ou');
  group.AddInput('server','scheme_server');

  group:=scheme.AddInputGroup('main_edit').Setup('scheme_main_group');
  group.AddInput('ca','',false,true);
  group.AddInput('objname','scheme_main_group',true);
  group.AddInput('c','scheme_cn',true);
  group.AddInput('email','scheme_email',true);
  group.AddInput('st','scheme_st',true);
  group.AddInput('l','scheme_l',true);
  group.AddInput('ou','scheme_ou',true);
  group.AddInput('issued','scheme_issued',true);
  group.AddInput('revoked','scheme_revoked',true);
  group.AddInput('valid','scheme_valid',true);
  group.AddInput('server','scheme_server',true);
end;

class procedure TFRE_DB_Certificate.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','Certificate');
    StoreTranslateableText(conn,'scheme_cn','Common Name');
    StoreTranslateableText(conn,'scheme_c','Country');
    StoreTranslateableText(conn,'scheme_email','EMail');
    StoreTranslateableText(conn,'scheme_st','State');
    StoreTranslateableText(conn,'scheme_l','Location');
    StoreTranslateableText(conn,'scheme_ou','Organization Unit');
    StoreTranslateableText(conn,'scheme_issued','Issued');
    StoreTranslateableText(conn,'scheme_revoked','Revoked');
    StoreTranslateableText(conn,'scheme_valid','Valid');
    StoreTranslateableText(conn,'scheme_server','Server Certificate');
  end;

end;


function TFRE_DB_Certificate.WEB_Revoke(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var cao       : IFRE_DB_Object;
    ca_base   : RFRE_CA_BASEINFORMATION;
    crt_id    : TFRE_DB_GUID;

begin
  try
   CheckDbResult(conn.Fetch(Field('ca').AsGUID,cao),'can not fetch ca object from database!');
    DBOtoCA_BaseInformation(cao,ca_base);
    if GET_SSL_IF.RevokeCrt(Field('objname').asstring, cao.Field('pass').asstring,Field('crt_stream').asstream.AsRawByteString,ca_base)=sslOK then begin
      CA_BaseInformationtoDBO(cao,ca_base,true);
      if conn.Update(cao)<>edb_OK then begin
        raise EFRE_Exception.Create('Error on updating CA object');
      end;
      Field('revoked').AsDateTimeUTC := GFRE_DT.Now_UTC;
      writeln(self.DumpToString());
    end else begin
      raise EFRE_Exception.Create('Error on revoking crt');
    end;
    result  := GFRE_DB_NIL_DESC;
  except
   on E:Exception do begin
     result := TFRE_DB_MESSAGE_DESC.Create.Describe('NEW','Error on revoking crt ['+e.Message+']',fdbmt_error);
   end;
  end;
end;

function TFRE_DB_CERTIFICATE.Create_SSL_Certificate(const conn: IFRE_DB_CONNECTION): boolean;
var cao       : IFRE_DB_Object;
    cao_id    : TFRE_DB_GUID;
    ca_base   : RFRE_CA_BASEINFORMATION;
    crt_base  : RFRE_CRT_BASEINFORMATION;

begin
  CheckDbResult(conn.Fetch(Field('ca').AsGUID,cao),'can not fetch ca object from database!');

  DBOtoCA_BaseInformation(cao,ca_base);
  if GET_SSL_IF.CreateCrt(Field('objname').asstring,cao.Field('c').asstring,cao.Field('st').asstring,Field('l').asstring,cao.Field('o').asstring,cao.Field('ou').asstring,Field('email').asstring, cao.Field('pass').asstring,ca_base,Field('server').asboolean,crt_base)=sslOK then begin
    CA_BaseInformationtoDBO(cao,ca_base,true);
    Field('c').asstring           := cao.Field('c').asstring;
    Field('st').asstring          := cao.Field('st').asstring;
    if conn.Update(cao)<>edb_OK then begin
      raise EFRE_Exception.Create('Error on updating CA object');
    end;
//    writeln(crt_base.crt);
    Field('crt_stream').asStream.SetFromRawByteString(crt_base.crt);
    Field('key_stream').asStream.SetFromRawByteString(crt_base.key);
    Field('issued').AsDateTimeUTC := GFRE_DT.Now_UTC;
    exit(true);
  end else begin
    exit(false);
  end;
end;

function TFRE_DB_CERTIFICATE.Import_SSL_Certificate(const crt_file, key_file: string; out import_error: TFRE_DB_String): boolean;
var
  crt,cn,c,st,l,o,ou,email : string;
  issued_date,end_date     : TFRE_DB_DateTime64;
begin
  try
    Field('crt').asstring  := GFRE_BT.StringFromFile(crt_file);
    Field('key').asstring  := GFRE_BT.StringFromFile(key_file);

    if GET_SSL_IF.ReadCrtInformation(Field('crt').asstring,cn,c,st,l,o,ou,email,issued_date,end_date)=sslOK then
      begin
        Field('objname').asstring        := cn;
        Field('c').asstring              := c;
        Field('st').asstring             := st;
        Field('l').asstring              := l;
        Field('o').asstring              := o;
        Field('ou').asstring             := ou;
        Field('email').asstring          := email;
        Field('issued').AsDateTimeUTC    := issued_date;
        Field('valid').AsDateTimeUTC     := end_date;
        exit(true);
      end
    else
      begin
        exit(false);
      end;
  except on E:Exception do begin
    import_error      := E.Message;
    exit(false);
  end; end;
end;

{ TFRE_DB_CA }

class procedure TFRE_DB_CA.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.ClassName);
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('c',fdbft_String).required:=true;
  scheme.AddSchemeField('email',fdbft_String).required:=true;
  scheme.AddSchemeField('st',fdbft_String).required:=true;
  scheme.AddSchemeField('l',fdbft_String).required:=true;
  scheme.AddSchemeField('o',fdbft_String).required:=true;
  scheme.AddSchemeField('ou',fdbft_String).required:=true;
  scheme.AddSchemeField('crt_stream',fdbft_Stream);
  scheme.AddSchemeField('key_stream',fdbft_Stream);
  scheme.AddSchemeField('pass',fdbft_String).SetupFieldDef(true,false,'','',true,false);
  scheme.AddSchemeField('issued',fdbft_DateTimeUTC);
  scheme.AddSchemeField('valid',fdbft_DateTimeUTC);
  scheme.AddSchemeField('directory',fdbft_String);

  group:=scheme.AddInputGroup('main_create').Setup('$scheme_TFRE_DB_CA_main_group');
  group.AddInput('objname','$scheme_TFRE_DB_CA_cn');
  group.AddInput('c','$scheme_TFRE_DB_CA_c');
  group.AddInput('email','$scheme_TFRE_DB_CA_email');
  group.AddInput('st','$scheme_TFRE_DB_CA_st');
  group.AddInput('l','$scheme_TFRE_DB_CA_l');
  group.AddInput('o','$scheme_TFRE_DB_CA_o');
  group.AddInput('ou','$scheme_TFRE_DB_CA_ou');
  group.AddInput('pass','$scheme_TFRE_DB_CA_pass');

  group:=scheme.AddInputGroup('main_edit').Setup('$scheme_TFRE_DB_CA_main_group');
  group.AddInput('objname','$scheme_TFRE_DB_CA_cn',true);
  group.AddInput('c','$scheme_TFRE_DB_CA_c',true);
  group.AddInput('email','$scheme_TFRE_DB_CA_email',true);
  group.AddInput('st','$scheme_TFRE_DB_CA_st',true);
  group.AddInput('l','$scheme_TFRE_DB_CA_l',true);
  group.AddInput('o','$scheme_TFRE_DB_CA_o',true);
  group.AddInput('ou','$scheme_TFRE_DB_CA_ou',true);
  group.AddInput('issued','$scheme_TFRE_DB_CA_issued',true);
  group.AddInput('valid','$scheme_TFRE_DB_CA_valid',true);

  group:=scheme.AddInputGroup('main_import').Setup('$scheme_TFRE_DB_CA_main_group');
  group.AddInput('directory','$scheme_TFRE_DB_CA_directory');
  group.AddInput('pass','$scheme_TFRE_DB_CA_pass');
end;

class procedure TFRE_DB_CA.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_main_group','Certificate Authority'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_cn','Common Name'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_c','Country'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_email','EMail'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_st','State'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_l','Location'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_o','Organization'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_ou','Organization Unit'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_pass','Password'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_issued','Issued'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_valid','Valid'));
    conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_directory','Basisverzeichnis'));
  end;

end;

class procedure TFRE_DB_CA.RestoreCA(const conn: IFRE_DB_CONNECTION; const filename: string; const domainName: string);
var
  coll  : IFRE_DB_COLLECTION;
  collc : IFRE_DB_COLLECTION;
  halo  : IFRE_DB_Object;
  caobj : IFRE_DB_Object;
  crtobj: IFRE_DB_Object;
  ldomainid : TFRE_DB_GUID;

  procedure _allCA(const fld:IFRE_DB_Field);
  var ca: IFRE_DB_Object;
  begin
    if (fld.FieldType=fdbft_Object) then
      begin
        ca := fld.AsObject.CloneToNewObject;
        if ca.FieldExists('crt') then
          begin
            ca.Field('crt_stream').AsStream.SetFromRawByteString(ca.Field('crt').asstring);
            ca.DeleteField('crt');
          end;
        if ca.FieldExists('key') then
          begin
            ca.Field('key_stream').AsStream.SetFromRawByteString(ca.Field('key').asstring);
            ca.DeleteField('key');
          end;
        ca.Field('DomainID').AsGUID:= ldomainid;
        CheckDbResult(coll.Store(ca),'could not store ca');
      end;
  end;

  procedure _allCrt(const fld:IFRE_DB_Field);
  var crt      : IFRE_DB_Object;
  begin
    if (fld.FieldType=fdbft_Object) then
      begin
        crt := fld.AsObject.CloneToNewObject;
        if crt.FieldExists('crt') then
          begin
            crt.Field('crt_stream').AsStream.SetFromRawByteString(crt.Field('crt').asstring);
            crt.DeleteField('crt');
          end;
        if crt.FieldExists('key') then
          begin
            crt.Field('key_stream').AsStream.SetFromRawByteString(crt.Field('key').asstring);
            crt.DeleteField('key');
          end;
//        writeln(crt.DumpToString);
        crt.Field('DomainID').AsGUID:= ldomainid;
        CheckDbResult(collc.Store(crt),'could not store crt');
      end;
  end;

begin
  ldomainid := conn.SYS.DomainID(domainName);
  COLL  := CONN.GetCollection(CFRE_DB_CA_COLLECTION);
  COLLC := CONN.GetCollection(CFRE_DB_CERTIFICATE_COLLECTION);
  halo   := GFRE_DBI.CreateFromFile(filename);
  caobj  := halo.Field('ca').AsObject;
  caobj.ForAllFields(@_allCA);
  crtobj := halo.Field('crt').AsObject;
  crtobj.ForAllFields(@_allCrt);
end;


function TFRE_DB_CA.Create_SSL_CA : boolean;
var ca_base : RFRE_CA_BASEINFORMATION;
begin
  if GET_SSL_IF.CreateCA(Field('objname').asstring,Field('c').asstring,Field('st').asstring,Field('l').asstring,Field('o').asstring,Field('ou').asstring,Field('email').asstring, Field('pass').asstring,ca_base)=sslOK then
    begin
      CA_BaseInformationtoDBO(self,ca_base);
      exit(true);
    end
  else
    exit(false);
end;

function TFRE_DB_CA.Import_SSL_CA(const ca_crt_file, serial_file, ca_key_file, random_file, index_file, crl_number_file: TFRE_DB_String; out import_error: TFRE_DB_String): boolean;
var ca_base                  : RFRE_CA_BASEINFORMATION;
    crt,cn,c,st,l,o,ou,email : string;
    issued_date,end_date     : TFRE_DB_DateTime64;
begin
  try
    ca_base.index     := GFRE_BT.StringFromFile(index_file);
    ca_base.serial    := GFRE_BT.StringFromFile(serial_file);
    ca_base.crlnumber := GFRE_BT.StringFromFile(crl_number_file);
    ca_base.crt       := GFRE_BT.StringFromFile(ca_crt_file);
    ca_base.key       := GFRE_BT.StringFromFile(ca_key_file);
    ca_base.random    := GFRE_BT.StringFromFile(random_file);
    CA_BaseInformationtoDBO(self,ca_base);
    if GET_SSL_IF.ReadCrtInformation(ca_base.crt,cn,c,st,l,o,ou,email,issued_date,end_date)=sslOK then
      begin
        Field('objname').asstring        := cn;
        Field('c').asstring              := c;
        Field('st').asstring             := st;
        Field('l').asstring              := l;
        Field('o').asstring              := o;
        Field('ou').asstring             := ou;
        Field('email').asstring          := email;
        Field('issued').AsDateTimeUTC    := issued_date;
        Field('valid').AsDateTimeUTC     := end_date;
        exit(true);
      end
    else
      begin
        exit(false);
      end;
  except on E:Exception do begin
    import_error      := E.Message;
    exit(false);
  end; end;
end;

function TFRE_DB_CA.Import_SSL_Certificates(const conn: IFRE_DB_CONNECTION; const crt_dir, key_dir: TFRE_DB_String; out import_error: TFRE_DB_String): boolean;
var
  info       : TSearchRec;
  crt        : IFRE_DB_Object;
  crt_error  : TFRE_DB_String;
begin
  result       :=true;
  import_error :='';
  try
    If FindFirst (crt_dir + '/*.crt',faAnyFile,info)=0 then
      repeat
        if (info.Name='.') or (info.Name='..') then Continue;
        crt := GFRE_DBI.NewObjectScheme(TFRE_DB_CERTIFICATE);
        crt.Field('ca').AsObjectLink := UID;
        if (crt.Implementor_HC as TFRE_DB_CERTIFICATE).Import_SSL_Certificate(crt_dir+DirectorySeparator+info.Name,key_dir+DirectorySeparator+ChangeFileExt(info.Name,'.key'),crt_error)=false then
          begin
            import_error := import_error+#13+#10+crt_error;
            result       := false;
          end;
        CheckDbResult(conn.GetCollection(CFRE_DB_CERTIFICATE_COLLECTION).Store(crt),'could not store certificate');
      until FindNext(info)<>0;
    FindClose(info);
  except on E:Exception do begin
    import_error := E.Message;
    result       := false;
  end; end;
end;

procedure TFRE_DB_CA.BackupCA(const conn: IFRE_DB_CONNECTION; const filename: string);
var
  crtobj     : IFRE_DB_Object;
  hal_caobj  : IFRE_DB_Object;
  hal_crtobj : IFRE_DB_Object;
  cobj       : IFRE_DB_Object;
  crt_array  : TFRE_DB_GUIDArray;
  i          : NativeInt;

begin
  cobj   := GFRE_DBI.NewObjectScheme(TFRE_DB_HALCONFIG);
  try
    hal_caobj  := GFRE_DBI.NewObject;
    hal_crtobj := GFRE_DBI.NewObject;
    cobj.Field('ca').asobject  := hal_caobj;
    cobj.Field('crt').asobject := hal_crtobj;

    hal_caobj.Field(Field('objname').asstring).asobject := CloneToNewObject();
    crt_array      := conn.GetReferences(UID,false,'TFRE_DB_CERTIFICATE');
    for i:= 0 to High(crt_array) do
      begin
        CheckDbResult(conn.Fetch(crt_array[i],crtobj),'could not fetch certificate');
        hal_crtobj.Field(crtobj.UID_String).asobject := crtobj;
      end;
    cobj.SaveToFile(filename);
  finally
    cobj.Finalize;
  end;
end;



{ TFRE_DB_WifiNetwork }

class procedure TFRE_DB_WifiNetwork.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_NETWORK');
  scheme.AddSchemeField('ssid',fdbft_String).required:=true;
  scheme.AddSchemeField('hidden',fdbft_Boolean);
  group:=scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('ssid',GetTranslateableTextKey('scheme_ssid'));
  group.AddInput('hidden',GetTranslateableTextKey('scheme_hidden'));
  group.UseInputGroup('TFRE_DB_NETWORK','main');
end;

class procedure TFRE_DB_WifiNetwork.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','Wifi Network');
    StoreTranslateableText(conn,'scheme_ssid','SSID');
    StoreTranslateableText(conn,'scheme_hidden','Hidden Network');
  end;
end;

function TFRE_DB_WifiNetwork.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=inherited IMI_Content(input);
end;

class procedure TFRE_DB_RadiusNetwork.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_WIFINETWORK');
  scheme.AddSchemeField('caid',fdbft_ObjLink).required:=false; //TODO FRANZ
  group:=scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.UseInputGroup('TFRE_DB_WIFINETWORK','main');
  group.AddInput('caid',GetTranslateableTextKey('scheme_caid'));
end;

class procedure TFRE_DB_RadiusNetwork.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','Radius Wifi Network');
    StoreTranslateableText(conn,'scheme_caid','CAID');
  end;
end;

function TFRE_DB_RadiusNetwork.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=inherited IMI_Content(input);
end;

class procedure TFRE_DB_Network.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('endpoint',fdbft_ObjLink).required:=true;
  scheme.AddSchemeField('ip_net',fdbft_String).SetupFieldDef(true,false,'','ip');
  scheme.AddSchemeField('dns',fdbft_String);
  scheme.AddSchemeField('dhcp',fdbft_Boolean);
  scheme.AddSchemeField('dhcp_range_start',fdbft_UInt16);
  scheme.AddSchemeField('dhcp_range_end',fdbft_UInt16);
  scheme.AddSchemeField('dhcp_leasetime',fdbft_UInt16);
  scheme.AddSchemeField('dhcp_parameters',fdbft_String).multiValues:=true;
  scheme.AddSchemeField('urlexceptions',fdbft_String).multiValues:=true;
  scheme.AddSchemeField('redirection_start',fdbft_ObjLink);
  scheme.AddSchemeField('redirection_customer',fdbft_ObjLink);
  scheme.AddSchemeField('redirection_agb',fdbft_ObjLink);
  scheme.AddSchemeField('redirection_end',fdbft_ObjLink);
  scheme.AddSchemeField('sessiontimeout',fdbft_UInt32);
  scheme.AddSchemeField('vlan_id',fdbft_Uint16);

  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('endpoint','',false,true);
  group.AddInput('ip_net',GetTranslateableTextKey('scheme_ip_net'));
  group.AddInput('dns',GetTranslateableTextKey('scheme_dns'));
  group.AddInput('dhcp',GetTranslateableTextKey('scheme_dhcp'));
  group.AddInput('dhcp_range_start',GetTranslateableTextKey('scheme_dhcp_range_start'));
  group.AddInput('dhcp_range_end',GetTranslateableTextKey('scheme_dhcp_range_end'));
  group.AddInput('dhcp_leasetime',GetTranslateableTextKey('scheme_dhcp_leasetime'));
  group.AddInput('dhcp_parameters',GetTranslateableTextKey('scheme_dhcp_parameters'));
  group.AddInput('urlexceptions',GetTranslateableTextKey('scheme_urlexceptions'));
  group.AddInput('redirection_start',GetTranslateableTextKey('scheme_redirection_start'),false,false,'cmspage');
  group.AddInput('redirection_customer',GetTranslateableTextKey('scheme_redirection_customer'),false,false,'cmspage');
  group.AddInput('redirection_agb',GetTranslateableTextKey('scheme_redirection_agb'),false,false,'cmspage');
  group.AddInput('redirection_end',GetTranslateableTextKey('scheme_redirection_end'),false,false,'cmspage');
  group.AddInput('sessiontimeout',GetTranslateableTextKey('scheme_sessiontimeout'));
  group.AddInput('vlan_id',GetTranslateableTextKey('scheme_vlan_id'));
end;

class procedure TFRE_DB_Network.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_ip_net','Subnet');
    StoreTranslateableText(conn,'scheme_dns','DNS');
    StoreTranslateableText(conn,'scheme_dhcp','DHCP');
    StoreTranslateableText(conn,'scheme_dhcp_range_start','DHCP Range Start');
    StoreTranslateableText(conn,'scheme_dhcp_range_end','DHCP Range End');
    StoreTranslateableText(conn,'scheme_dhcp_leasetime','DHCP Leasetime');
    StoreTranslateableText(conn,'scheme_dhcp_parameters','DHCP Parameters');
    StoreTranslateableText(conn,'scheme_urlexceptions','Url Exceptions');
    StoreTranslateableText(conn,'scheme_redirection_start','Redirection Start');
    StoreTranslateableText(conn,'scheme_redirection_customer','Redirection Customer');
    StoreTranslateableText(conn,'scheme_redirection_agb','Redirection AGB');
    StoreTranslateableText(conn,'scheme_redirection_end','Redirection End');
    StoreTranslateableText(conn,'scheme_sessiontimeout','Sessiontimeout');
    StoreTranslateableText(conn,'scheme_vlan_id','VLAN Identifier');
  end;
end;

class procedure TFRE_DB_Network.NetworkOnChange(const dbc: IFRE_DB_Connection; const is_dhcp: boolean; const subnet: string; const ep_id: TFRE_DB_GUID; const dns: string; const range_start, range_end: integer);
  var dhcp_obj        : IFRE_DB_Object;
      dhcp_id         : TFRE_DB_GUID;
      childs          : TFRE_DB_GUIDArray;
      dhcp_subnet_obj : IFRE_DB_Object;
      i               : integer;
      colls           : IFRE_DB_Collection;
      current_ip      : string;
      highest         : integer;
      sub_ip          : integer;
      do_update       : boolean;
      routing_id      : TFRE_DB_GUID;
      routing_obj     : IFRE_DB_Object;
      route_obj       : IFRE_DB_Object;
      ep_obj          : IFRE_DB_Object;
      gw              : string;

      procedure _setfields;
      begin
        dhcp_subnet_obj.Field('subnet').AsString:=subnet;
        dhcp_subnet_obj.Field('range_start').AsString:=GetIPDots(subnet,3)+inttostr(range_start);
        dhcp_subnet_obj.Field('range_end').AsString:=GetIPDots(subnet,3)+inttostr(range_end);
        dhcp_subnet_obj.Field('router').AsString:=GetIPDots(subnet,3)+'1';
        dhcp_subnet_obj.Field('dns').AsString:=dns;
      end;

begin
    writeln('NetworkOnChange');
    if is_dhcp then begin
      writeln('DHCP');
      dhcp_id        := GetService(dbc,'TFRE_DB_DHCP');
      CheckDbResult(dbc.Fetch(dhcp_id,dhcp_obj),'NO DHCP SERVICE FOUND IN NETWORK ON CHANGE');
      //fixme
      //childs     := dhcp_obj.ReferencedByList('TFRE_DB_DHCP_SUBNET');
      abort;
      writeln('ChiLDS');

      do_update  := false;

      for i := 0 to Length(childs) - 1 do begin
        dbc.Fetch(childs[i],dhcp_subnet_obj);
        if dhcp_subnet_obj.Field('subnet').AsString=subnet then begin
          writeln('ALREADY IN DHCP, UPDATING !');
          do_update  := true;
          break;
        end;
      end;

      if do_update then begin
        _setfields;
        CheckDbResult(dbc.Update(dhcp_subnet_obj),'failure on cloned/update');
      end else begin
        colls          := dbc.GetCollection('dhcp_subnet');
        dhcp_subnet_obj:= GFRE_DBI.NewObjectScheme(TFRE_DB_DHCP_Subnet);
        _setfields;
        dhcp_subnet_obj.Field('dhcp').AsObjectLink:=dhcp_id;
        CheckDbResult(COLLS.Store(dhcp_subnet_obj),'Add DHCP Subnet');
      end;
    end;

    // check routing
    CheckDbResult(dbc.Fetch(ep_id,ep_obj),'NO EP FOUND IN NETWORK ON CHANGE');
    if   ep_obj.IsA('TFRE_DB_AP_Lancom') then begin
      // no routing
      gw := '';
    end else begin
      if ep_obj.FieldExists('vpn_crtid') then begin
        gw := '';
      end else begin
        gw := '1.2.3.4';    // get from mac / dhcp   // TODO XXXX
      end;
    end;

    if gw <>'' then begin
      do_update  := false;

      routing_id := GetService  (dbc,'TFRE_DB_ROUTING');
      CheckDbResult(dbc.Fetch    (routing_id, routing_obj),'NO ROUTING SERVICE FOUND IN NETWORK ON CHANGE');
      for i := 0 to routing_obj.Field('static').ValueCount-1 do begin
        route_obj   :=    routing_obj.Field('static').AsObjectItem[i];
        if route_obj.Field('subnet').AsString=subnet then begin
          writeln('ALREADY IN ROUTING, UPDATING !');
          do_update  := true;
          break;
        end;
      end;

      if do_update then begin
       //route_obj.Field('gateway').AsString := '1.1.1.1';
      end else begin
       abort;
       //route_obj:=GFRE_DBI.NewObjectScheme(TFRE_DB_Route);
       //route_obj.Field('subnet').AsString:=subnet;
       //route_obj.Field('gateway').AsString:='2.2.2.2';
       //routing_obj.Field('static').AddObject(route_obj);
      end;
      routing_obj.Field('reprovision').AsBoolean := true;
      writeln(routing_obj.DumpToString);
      CheckDbResult (dbc.Update(routing_obj),'failure on cloned/update');
    end;

    SetReprovision(dbc,dhcp_id);
end;


class function TFRE_DB_Network.WBC_NewOperation(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
 var
      dbc        :   IFRE_DB_CONNECTION;
      raw_object :   IFRE_DB_Object;
      new_net    :   string;
      ep_id      :   TFRE_DB_GUID;
      dhcp       :   boolean;
      range_start:   integer;
      range_end  :   integer;
      dns        :   string;
      s          :   string;

begin
  //writeln ('NETWORK NewOperation');
  //writeln(input.DumpToString());
  //dbc          := input.GetReference as IFRE_DB_CONNECTION;
  //raw_object   := input.Field('data').AsObject;
  //new_net      := raw_object.Field('ip_net').asstring;
  //dhcp         := raw_object.Field('dhcp').asboolean;
  //ep_id        := raw_object.Field('endpoint').AsGUID;
  //
  //s            := raw_object.Field('dhcp_range_start').Asstring;
  //if s<>'' then range_start:=strtoint(s) else range_start:=20;
  //s            := raw_object.Field('dhcp_range_end').Asstring;
  //if s<>'' then range_start:=strtoint(s) else range_end:=20;
  //dns          := raw_object.Field('dns').Asstring;
  //
  //if CheckClass(new_net)=false then begin
  //  result := TFRE_DB_MESSAGE_DESC.Create.Describe('SAVE','Error on creating! Only Class C networks are currently allowed !',fdbmt_error);
  //  exit;
  //end;
  //
  //if UniqueNet(dbc,GUID_NULL,new_net) then begin
  //  writeln('UNIQUE');
  //end else begin
  //  writeln('NOT UNIQUE');
  //  result := TFRE_DB_MESSAGE_DESC.Create.Describe('SAVE','Error on creating! The network is not unique !',fdbmt_error);
  //  exit;
  //end;
  //
  //Result:=inherited IMC_NewOperation(input);
  //
  //// set reprovision on endpoint
  //SetReprovision(dbc,ep_id);
  //NetworkOnChange(dbc,dhcp,new_net,ep_id,dns,range_start,range_end);
end;

function TFRE_DB_Network.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res    :TFRE_DB_FORM_PANEL_DESC;
  scheme :IFRE_DB_SchemeObject;
begin
  scheme := GetScheme;
  res:=TFRE_DB_FORM_PANEL_DESC.Create.Describe('Network');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_Network.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Delete','images_apps/cloudcontrol/delete_network.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'deleteOperation'));
  Result:=res;
end;


function TFRE_DB_Network.WEB_SaveOperation(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var scheme            : IFRE_DB_SCHEMEOBJECT;
    update_object_uid : TFRE_DB_GUID;
    raw_object        : IFRE_DB_Object;
    dbc               : IFRE_DB_CONNECTION;
    ep_id             : TFRE_DB_GUID;
    dhcp              : boolean;
    subnet            : string;
    dns               : string;
    range_start       : integer;
    range_end         : integer;

begin
  writeln('NETWORK SAVE');
  if assigned(Parent) then begin
    result := TFRE_DB_MESSAGE_DESC.Create.Describe('SAVE','Error on saving! Saving of Subobject not supported!',fdbmt_error);
    exit;
  end;
  result            := nil;
  scheme            := GetScheme;
  update_object_uid := UID;
  raw_object        := input.Field('data').AsObject;

  if CheckClass(raw_object.Field('ip_net').AsString)=false then begin
    result := TFRE_DB_MESSAGE_DESC.Create.Describe('SAVE','Error on saving! Only Class C networks are currently allowed !',fdbmt_error);
    exit;
  end;

  if UniqueNet(dbc,UID,raw_object.Field('ip_net').AsString)=false then begin
   result := TFRE_DB_MESSAGE_DESC.Create.Describe('SAVE','Error on saving! The network is not unique !',fdbmt_error);
   exit;
  end;

  scheme.SetObjectFieldsWithScheme(raw_object,self,false,conn);

  ep_id             := Field('endpoint').AsGUID;
  dhcp              := Field('dhcp').AsBoolean;
  subnet            := Field('ip_net').AsString;
  range_start       := Field('dhcp_range_start').AsUInt16;
  range_end         := Field('dhcp_range_end').AsUInt16;
  dns               := Field('dns').AsString;

  CheckDbResult     (dbc.Update(self),'failure on cloned/update');  // This instance is freed by now, so rely on the stackframe only (self) pointer is garbage(!!)

  SetReprovision    (dbc,ep_id);
  NetworkOnChange   (dbc,dhcp,subnet,ep_id,dns,range_start,range_end);

  result := GFRE_DB_NIL_DESC;

end;


{ TFRE_DB_MobileDevice }

class procedure TFRE_DB_MobileDevice.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_DEVICE');
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('site',fdbft_ObjLink);
  scheme.AddSchemeField('crtid',fdbft_ObjLink);
  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('objname',GetTranslateableTextKey('scheme_name'));
  group.UseInputGroup('TFRE_DB_DEVICE','main');
  group.AddInput('site','',false,true);
  group.AddInput('crtid',GetTranslateableTextKey('scheme_certificate'));
end;

class procedure TFRE_DB_MobileDevice.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','Mobile Device');
    StoreTranslateableText(conn,'scheme_name','Device Name');
    StoreTranslateableText(conn,'scheme_certificate','Certificate');
  end;
end;

function TFRE_DB_MobileDevice.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
  scheme: IFRE_DB_SchemeObject;
begin
  scheme := Getscheme;
  res:=TFRE_DB_FORM_PANEL_DESC.Create.Describe('Mobile Device');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_MobileDevice.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  //TODO - check if mobiledevice is assigned to a site?
  res.AddEntry.Describe('Unassign','images_apps/cloudcontrol/unassign_mobile_device.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'unassign'));
  res.AddEntry.Describe('Delete','images_apps/cloudcontrol/delete_mobile_device.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'deleteOperation'));
  Result:=res;
end;

function TFRE_DB_MobileDevice.IMI_unassign(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result := TFRE_DB_MESSAGE_DESC.Create.Describe('Unassign Mobile Device','Not implemented yet!',fdbmt_error);
end;

{ TFRE_DB_Endpoint }

class procedure TFRE_DB_Endpoint.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_DEVICE');

  scheme.AddSchemeField     ('site',fdbft_ObjLink).required:=true;
  scheme.AddSchemeField     ('status_uid',fdbft_ObjLink);
  scheme.AddSchemeField     ('reprovision',fdbft_Boolean);
  scheme.AddCalcSchemeField ('displayname',fdbft_String,@CALC_GetDisplayName);

  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.UseInputGroup('TFRE_DB_DEVICE','main');
  group.AddInput('site','',false,true);
end;

class procedure TFRE_DB_Endpoint.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'scheme_main_group','End Point Configuration');
  end;
end;

function TFRE_DB_Endpoint.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_SUBSECTIONS_DESC;
  sec   : TFRE_DB_SECTION_DESC;
begin
  res:=TFRE_DB_SUBSECTIONS_DESC.create.Describe;
  sec:=res.AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'Configuration'),'Configuration',1);
  res.AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'Monitoring'),'Monitoring',2);

  Result:=res;
end;

function TFRE_DB_Endpoint.IMI_Configuration(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
  scheme: IFRE_DB_SchemeObject;
begin
  scheme := GetScheme;
  res:=TFRE_DB_FORM_PANEL_DESC.Create.Describe('Endpoint');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_Endpoint.IMI_Monitoring(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res  : TFRE_DB_SUBSECTIONS_DESC;
  sub  : TFRE_DB_SECTION_DESC;
begin
  res:=TFRE_DB_SUBSECTIONS_DESC.create.Describe(sec_dt_vertical);

  sub:=res.AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'Monitoring_Con'),'Connected',1);
//  sub.SetContentDesc(IMI_Statistics_Con(nil).Implementor_HC as TFRE_DB_CONTENT_DESC);

  sub:=res.AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'Monitoring_All'),'All',2,'',2);
//  sub.SetContentDesc(IMI_Statistics_All(nil).Implementor_HC as TFRE_DB_CONTENT_DESC);

  sub:=res.AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'Monitoring_All'),'All',0);
//  sub.SetContentDesc(IMI_Statistics_All(nil).Implementor_HC as TFRE_DB_CONTENT_DESC);

  Result:=res;
end;

function TFRE_DB_Endpoint.IMI_Monitoring_Con(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_VIEW_LIST_DESC;
  layout: TFRE_DB_VIEW_LIST_LAYOUT_DESC;
  store : TFRE_DB_STORE_DESC;
begin

  layout:=TFRE_DB_VIEW_LIST_LAYOUT_DESC.create.Describe();
  layout.AddDataElement.Describe('customernumber','Number');
  layout.AddDataElement.Describe('company','Company');
  layout.AddDataElement.Describe('firstname','Firstname');
  layout.AddDataElement.Describe('lastname','Lastname');

  store:=TFRE_DB_STORE_DESC.create.Describe('Statistics',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'Monitoring_Data'));

  res:=TFRE_DB_VIEW_LIST_DESC.create.Describe(store, layout, nil, 'Monitoring Con',[]);
  Result:=res;
end;

function TFRE_DB_Endpoint.IMI_Monitoring_All(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_VIEW_LIST_DESC;
  layout: TFRE_DB_VIEW_LIST_LAYOUT_DESC;
  store : TFRE_DB_STORE_DESC;
begin

  layout:=TFRE_DB_VIEW_LIST_LAYOUT_DESC.create.Describe();
  layout.AddDataElement.Describe('customernumber','Number');
  layout.AddDataElement.Describe('company','Company');
  layout.AddDataElement.Describe('firstname','Firstname');
  layout.AddDataElement.Describe('lastname','Lastname');

  store:=TFRE_DB_STORE_DESC.create.Describe('Statistics',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'Monitoring_Data'));

  res:=TFRE_DB_VIEW_LIST_DESC.create.Describe(store, layout, nil, 'Monitoring All',[]);
  Result:=res;
end;

function TFRE_DB_Endpoint.IMI_Monitoring_Data(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_STORE_DATA_DESC;
  entry : IFRE_DB_Object;
begin
  res:=TFRE_DB_STORE_DATA_DESC.create.Describe(3);
  entry:=GFRE_DBI.NewObject;
  entry.Field('customernumber').AsInt16:=1;
  entry.Field('company').AsString:='A';
  entry.Field('firstname').AsString:='AF';
  entry.Field('lastname').AsString:='AL';
  res.addEntry(entry);
  entry:=GFRE_DBI.NewObject;
  entry.Field('customernumber').AsInt16:=2;
  entry.Field('company').AsString:='B';
  entry.Field('firstname').AsString:='BF';
  entry.Field('lastname').AsString:='BL';
  res.addEntry(entry);
  entry:=GFRE_DBI.NewObject;
  entry.Field('customernumber').AsInt16:=3;
  entry.Field('company').AsString:='C';
  entry.Field('firstname').AsString:='CF';
  entry.Field('lastname').AsString:='CL';
  res.addEntry(entry);
  Result:=res;
end;


function TFRE_DB_Endpoint.IMI_Provision(const input: IFRE_DB_Object): IFRE_DB_Object;
var pnr:integer;
    AProcess: TProcess;
    res: TFRE_DB_MESSAGE_DESC;
begin
  abort; //FIXME
  //if FieldExists('provisioning_serial') then begin
  // pnr:=Field('provisioning_serial').asint32;
  //end else begin
  // pnr:=0;
  //end;
  //inc(pnr);
  //writeln('new frehash:',pnr);
  //Field('provisioning_serial').asint32:=pnr;
  //writeln(DumpToString());
  //Field('reprovision').asboolean := false;
  //
  //CheckDbResult(conn.Update(self),'failure on cloned/update');  // This instance is freed by now, so rely on the stackframe only (self) pointer is garbage(!!)
  //
  //res := TFRE_DB_MESSAGE_DESC.Create.Describe('PROVISIONING','Provisioning OK',fdbmt_info); //TODO - add nil message (nothing to do response)
  //Result:=res;
end;

function TFRE_DB_Endpoint.IMI_addOpenWifiNetwork(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res       : TFRE_DB_FORM_DIALOG_DESC;
  scheme    : IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  abort;//FIXME
  //GFRE_DBI.GetSystemScheme(TFRE_DB_OpenWifiNetwork,scheme);
  //res:=TFRE_DB_FORM_DIALOG_DESC.Create.Describe('Add Open Wifi Network',0,0,true,true,false);
  //res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  //res.SetElementValue('endpoint',GFRE_BT.GUID_2_HexString(UID));
  //res.SetElementValue('hidden','false');
  //res.SetElementValue('ip_net',GetNextNet(GetDBConnection,UID));
  //res.SetElementValue('dns','172.17.0.1');
  //res.SetElementValue('dhcp','true');
  //res.SetElementValue('dhcp_range_start','10');
  //res.SetElementValue('dhcp_range_end','250');
  //res.SetElementValue('dhcp_leasetime','600');
  //res.SetElementValue('dhcp_leasetime','600');
  //res.SetElementValue('sessiontimeout','1800');
  //serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_OPENWIFINETWORK','newOperation');
  //serverFunc.AddParam.Describe('collection','network');
  //res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  //Result:=res;
end;

function TFRE_DB_Endpoint.IMI_addWPA2Network(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res       : TFRE_DB_FORM_DIALOG_DESC;
  scheme    : IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  abort;//FIXME
  //GFRE_DBI.GetSystemScheme(TFRE_DB_WPA2NETWORK,scheme);
  //res:=TFRE_DB_FORM_DIALOG_DESC.Create.Describe('Add WPA2 Network',0,0,true,true,false);
  //res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  //res.SetElementValue('endpoint',GFRE_BT.GUID_2_HexString(UID));
  //res.SetElementValue('hidden','true');
  //res.SetElementValue('ip_net',GetNextNet(GetDBConnection,UID));
  //res.SetElementValue('dns','172.17.0.1');
  //res.SetElementValue('dhcp','true');
  //res.SetElementValue('dhcp_range_start','10');
  //res.SetElementValue('dhcp_range_end','250');
  //res.SetElementValue('dhcp_leasetime','600');
  //res.SetElementValue('dhcp_leasetime','600');
  //res.SetElementValue('sessiontimeout','1800');
  //serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_WPA2NETWORK','newOperation');
  //serverFunc.AddParam.Describe('collection','network');
  //res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  //Result:=res;
end;


function TFRE_DB_Endpoint.IMI_ChildrenData(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_STORE_DATA_DESC;
  entry : IFRE_DB_Object;
  childs: TFRE_DB_GUIDArray;
  i     : Integer;
  dbo   : IFRE_DB_Object;

begin
  res := TFRE_DB_STORE_DATA_DESC.create;
  //Fixme
  //childs:=ReferencedByList;
  abort;
  //for i := 0 to Length(childs) - 1 do begin
  //  GetDBConnection.Fetch(childs[i],dbo);
  //  if dbo.IsA('TFRE_DB_NETWORK') then begin
  //    entry:=GFRE_DBI.NewObject;
  //    entry.Field('text').AsString:=dbo.field('ssid').AsString;
  //    entry.Field('uid').AsGUID:=dbo.UID;
  //    entry.Field('uidpath').AsStringArr:=dbo.GetUIDPath;
  //    entry.Field('_funcclassname_').AsString:=dbo.SchemeClass;
  //    entry.Field('_childrenfunc_').AsString:='ChildrenData';
  //    entry.Field('_menufunc_').AsString:='Menu';
  //    entry.Field('_contentfunc_').AsString:='Content';
  //    res.addEntry(entry);
  //  end;
  //end;
  Result:=res;
end;

procedure TFRE_DB_Endpoint.CALC_GetDisplayName(const Setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString('Endpoint('+Field('provisioningmac').AsString+')');
end;



 { TFRE_DB_ZONE }

 class procedure TFRE_DB_ZONE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName('TFRE_DB_OBJECTEX');
   scheme.GetSchemeField('objname').required:=true;
   scheme.AddSchemeField('templateid',fdbft_ObjLink).Required:=true;
   scheme.AddSchemeField('disabledSCs',fdbft_String).MultiValues:=true;

   scheme.AddSchemeField('zonepath',fdbft_String);
   scheme.AddSchemeField('hostid',fdbft_ObjLink).required:=true;
   scheme.AddSchemeField('serviceParent',fdbft_ObjLink);
   scheme.AddCalcSchemeField('displayname',fdbft_String,@CALC_GetDisplayName);

   //embedding
   scheme.AddSchemeField('masterdataset',fdbft_String);
   scheme.AddSchemeField('masterdatasetpath',fdbft_String);
   scheme.AddSchemeField('zonedataset',fdbft_String);
   scheme.AddSchemeField('zonedbodataset',fdbft_String);
   scheme.AddSchemeField('templatedataset',fdbft_String);

   group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main'));
   group.AddInput('objname',GetTranslateableTextKey('scheme_name'));
end;

 class procedure TFRE_DB_ZONE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.1';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
     StoreTranslateableText(conn,'scheme_main','General Information');
     StoreTranslateableText(conn,'scheme_name','Name');
     StoreTranslateableText(conn,'scheme_description','Description');
   end;
   if currentVersionId='1.0' then begin
     currentVersionId := '1.1';
     DeleteTranslateableText(conn,'scheme_name');
     StoreTranslateableText(conn,'scheme_name','Zone Name');
     DeleteTranslateableText(conn,'scheme_description');
   end;
 end;

 procedure TFRE_DB_ZONE.Embed(const conn: IFRE_DB_CONNECTION);
 var refs      : TFRE_DB_ObjectReferences;
     obj       : IFRE_DB_Object;
     i         : integer;
     svc       : TFRE_DB_SERVICE;
     ds        : TFRE_DB_ZFS_DATASET;
     pds       : TFRE_DB_ZFS_DATASET_PARENT;
     tmpl      : TFRE_DB_FBZ_TEMPLATE;
     svcobj    : IFRE_DB_OBject;
     sp_id     : TFRE_DB_GUID;
     whole_dsname : string;
     master_dsname: string;
 begin
   whole_dsname := '';
   master_dsname:= '';
   if FieldExists('serviceparent') then
     begin
       writeln('SWL: SERVICEPARENT');
       sp_id := Field('serviceparent').AsGUID;
       repeat
         CheckDbResult(conn.Fetch(sp_id,obj),'could not fetch serviceparent '+Field('serviceparent').AsGUID.AsHexString);
         if obj.FieldExists('serviceparent') then
           sp_id := obj.Field('serviceparent').AsGUID
         else
           break;
         if obj.IsA(TFRE_DB_ZFS_DATASET,ds) then
           begin
             writeln('SWL DS:',obj.DumpToString);
             if whole_dsname='' then   // first, zonedataset
               begin
                 Field('zonepath').asstring := ds.Field('mountpoint').asstring;
                 whole_dsname := ds.ObjectName;
               end
             else
               begin
                 whole_dsname := ds.ObjectName+'/'+whole_dsname;
               end;
             if master_dsname<>'' then
               begin
                 master_dsname := ds.ObjectName+'/'+master_dsname;
               end;
             if obj.isA(TFRE_DB_ZFS_DATASET_PARENT,pds) then
               begin
                 master_dsname := ds.ObjectName;
                 Field('masterdatasetpath').AsString := pds.Field('mountpoint').asstring;
               end;
           end;
         obj.finalize;
       until false;
       Field('masterdataset').AsString       := master_dsname;
       Field('zonedataset').asstring         := whole_dsname;
       Field('zonedbodataset').asstring      := Field('masterdataset').asstring+'/zones/'+UID.AsHexstring;

       writeln('SWL: SERVICEPARENT DONE');
//       writeln('SWL: ZONEDBO: ',DumpToString());
//       abort;
     end;
   if FieldExists('templateid') then
     begin
       writeln('SWL: TEMPLATE');
       CheckDbResult(conn.Fetch(Field('templateid').AsGUID,obj),'could not fetch template '+Field('templateid').AsGUID.AsHexString);
       if obj.IsA(TFRE_DB_FBZ_TEMPLATE,tmpl) then
         begin
           Field('templatedataset').asstring   := Field('masterdataset').AsString+'/template/'+Lowercase(tmpl.ObjectName);
           if tmpl.FieldExists('serviceclasses') then  //TODO : GET CONFIGURED SERVICES, NOT FAKE FROM TEMPLATE
             begin
               for i:=0 to tmpl.Field('serviceclasses').valuecount-1 do
                 begin
                   if tmpl.Field('serviceclasses').AsStringItem[i]='TFOS_DB_CITYCOM_VOIP_SERVICE' then
                     continue;
                   if tmpl.Field('serviceclasses').AsStringItem[i]=TFRE_DB_VIRTUAL_FILESERVER.ClassName then
                     continue;
                   if tmpl.Field('serviceclasses').AsStringItem[i]=TFRE_DB_VMACHINE.ClassName then
                     continue;
                   if Pos('TFRE_DB_DATALINK',tmpl.Field('serviceclasses').AsStringItem[i])>0 then
                     continue;

                   writeln('SWL : SERVICECLASSES ',tmpl.Field('serviceclasses').AsStringItem[i]);
                   svcobj := GFRE_DBI.NewObjectSchemeByName(tmpl.Field('serviceclasses').AsStringItem[i]);
                   self.Field(svcobj.UID.AsHexString).AsObject:=svcobj;
                 end;
             end;
         end;
       obj.Finalize;
       writeln('SWL: TEMPLATEDONE');
//       writeln('SWL: ZONEDBO: ',DumpToString());
//       abort;
     end;

   refs := conn.GetReferencesDetailed(UID,false);
   for i:=0 to high(refs) do
     begin
       CheckDbResult(conn.Fetch(refs[i].linked_uid,obj),' could not fetch referencing object '+refs[i].linked_uid.AsHexString);
       if obj.IsA(TFRE_DB_SERVICE,svc) then
         begin
           svc.Embed(conn);
           self.Field(obj.UID.AsHexString).AsObject:=svc;
         end
       else
         obj.Finalize;
     end;

 //  writeln('SWL: ZONEDBO: ',DumpToString());
 end;


 procedure TFRE_DB_ZONE.BootingHookConfigure;

   procedure _objIterator(const obj:IFRE_DB_Object);
   var dl_vnic : TFRE_DB_DATALINK_VNIC;
   begin
     if obj.IsA(TFRE_DB_DATALINK_VNIC,dl_vnic) then
       begin
         dl_vnic.Field('zonename').asstring := UID.AsHexString;
         writeln('SWL ZONENAME:',UID.AsHexString);
         dl_vnic.RIF_CreateVNIC;
       end;
   end;

 begin
   ForAllObjects(@_objIterator,true);
 end;

 procedure TFRE_DB_ZONE.CALC_GetDisplayName(const setter: IFRE_DB_CALCFIELD_SETTER);
 begin
   setter.SetAsString('Zone: '+Field('objname').AsString);
 end;

 class function TFRE_DB_ZONE.WBC_NewOperation(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=inherited WBC_NewOperation(input, ses, app, conn);
end;

 function TFRE_DB_ZONE.WEB_SaveOperation(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=inherited WEB_SaveOperation(input, ses, app, conn);
end;

 function TFRE_DB_ZONE.hasNAS(const conn: IFRE_DB_CONNECTION): Boolean;
 begin
   Result:=conn.IsReferenced(UID,TFRE_DB_NAS.ClassName,'serviceParent');
 end;

 function TFRE_DB_ZONE.hasDNS(const conn: IFRE_DB_CONNECTION): Boolean;
 begin
   Result:=conn.IsReferenced(UID,TFRE_DB_DNS.ClassName,'serviceParent');
 end;

 function TFRE_DB_ZONE.MachineID: TFRE_DB_GUID;
 begin
   result := Field('hostid').AsObjectLink;
 end;

 function TFRE_DB_ZONE.RIF_Boot(const runnning_ctx: TObject): IFRE_DB_Object;
 begin
   {$IFDEF SOLARIS}
   result := fre_boot_zone (self,nil);
   {$ELSE}
   writeln('zone boot not implemented on this system');
   {$ENDIF SOLARIS}
 end;

 function TFRE_DB_ZONE.RIF_Halt(const runnning_ctx: TObject): IFRE_DB_Object;
 begin
   {$IFDEF SOLARIS}
   result := fre_halt_zone (self,nil);
   {$ELSE}
   writeln('zone halt not implemented on this system');
   {$ENDIF SOLARIS}
 end;

 { TFRE_DB_MACHINE_SETTING_TIME }

 class procedure TFRE_DB_MACHINE_SETTING_TIME.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_MACHINE_SETTING.Classname);
   scheme.AddSchemeField('region',fdbft_String).required:=true;
   scheme.AddSchemeField('timezone',fdbft_String).required:=true;
   scheme.AddSchemeField('ntpserver',fdbft_String);
   group:=scheme.AddInputGroup('setting').Setup(GetTranslateableTextKey('scheme_setting'));
   group.AddInput('region',GetTranslateableTextKey('scheme_region'));
   group.AddInput('timezone',GetTranslateableTextKey('scheme_timezone'));
   group.AddInput('ntpserver',GetTranslateableTextKey('scheme_ntpserver'));
 end;

 class procedure TFRE_DB_MACHINE_SETTING_TIME.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
     StoreTranslateableText(conn,'scheme_setting','Setting');
     StoreTranslateableText(conn,'scheme_region','Region');
     StoreTranslateableText(conn,'scheme_timezone','Timezone');
     StoreTranslateableText(conn,'scheme_ntpserver','NTP Server');
   end;
 end;

 { TFRE_DB_MACHINE_SETTING_MAIL }

 class procedure TFRE_DB_MACHINE_SETTING_MAIL.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_MACHINE_SETTING.Classname);
   scheme.AddSchemeField('smtpserver',fdbft_String).required:=true;
   scheme.AddSchemeField('smtpuser',fdbft_String);
   scheme.AddSchemeField('smtppassword',fdbft_String);
   scheme.AddSchemeField('mailfrom',fdbft_String).required:=true;
   scheme.AddSchemeField('mailto',fdbft_String).required:=true;
   group:=scheme.AddInputGroup('setting').Setup(GetTranslateableTextKey('scheme_setting'));
   group.AddInput('smtpserver',GetTranslateableTextKey('scheme_smtpserver'));
   group.AddInput('smtpuser',GetTranslateableTextKey('scheme_smtpuser'));
   group.AddInput('smtppassword',GetTranslateableTextKey('scheme_smtppassword'));
   group.AddInput('mailfrom',GetTranslateableTextKey('scheme_mailfrom'));
   group.AddInput('mailto',GetTranslateableTextKey('scheme_mailto'));
 end;

 class procedure TFRE_DB_MACHINE_SETTING_MAIL.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
     StoreTranslateableText(conn,'scheme_setting','Mail Parameters');
     StoreTranslateableText(conn,'scheme_smtpserver','SMTP Server');
     StoreTranslateableText(conn,'scheme_smtpuser','SMTP User');
     StoreTranslateableText(conn,'scheme_smtppassword','SMTP Password');
     StoreTranslateableText(conn,'scheme_mailfrom','Mail from');
     StoreTranslateableText(conn,'scheme_mailto','Mail to');
   end;
 end;

 { TFRE_DB_MACHINE_SETTING_HOSTNAME }

 class procedure TFRE_DB_MACHINE_SETTING_HOSTNAME.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_MACHINE_SETTING.Classname);
   scheme.AddSchemeField('hostname',fdbft_String);
   scheme.AddSchemeField('domainname',fdbft_String);
   group:=scheme.AddInputGroup('setting').Setup(GetTranslateableTextKey('scheme_setting'));
   group.AddInput('hostname',GetTranslateableTextKey('scheme_hostname'));
   group.AddInput('domainname',GetTranslateableTextKey('scheme_domainname'));
 end;

 class procedure TFRE_DB_MACHINE_SETTING_HOSTNAME.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
     StoreTranslateableText(conn,'scheme_setting','Setting');
     StoreTranslateableText(conn,'scheme_hostname','Hostname');
     StoreTranslateableText(conn,'scheme_domainname','Domainname');
   end;
 end;

 { TFRE_DB_MACHINE_SETTING_POWER }

 class procedure TFRE_DB_MACHINE_SETTING_POWER.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_MACHINE_SETTING.Classname);
   scheme.AddSchemeField('uptime',fdbft_String);
   group:=scheme.AddInputGroup('setting').Setup(GetTranslateableTextKey('scheme_setting'));
   group.AddInput('uptime',GetTranslateableTextKey('scheme_uptime'),true);
 end;

 class procedure TFRE_DB_MACHINE_SETTING_POWER.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
     StoreTranslateableText(conn,'scheme_setting','Setting');
     StoreTranslateableText(conn,'scheme_uptime','Uptime');
   end;
 end;

 function TFRE_DB_MACHINE_SETTING_POWER.IMI_Shutdown(const input: IFRE_DB_Object): IFRE_DB_Object;
 begin
   Result:=TFRE_DB_MESSAGE_DESC.create.Describe('Shutdown','Shutdown disabled in Demo Mode',fdbmt_info,nil);
 end;

 function TFRE_DB_MACHINE_SETTING_POWER.IMI_Reboot(const input: IFRE_DB_Object): IFRE_DB_Object;
 begin
   Result:=TFRE_DB_MESSAGE_DESC.create.Describe('Reboot','Reboot disabled in Demo Mode',fdbmt_info,nil);
 end;

 { TFRE_DB_MACHINE_SETTING }

 class procedure TFRE_DB_MACHINE_SETTING.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.Classname);
   scheme.GetSchemeField('objname').required:=true;
   group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main'));
   group.AddInput('objname',GetTranslateableTextKey('scheme_name'),true);
   group.AddInput('desc.txt',GetTranslateableTextKey('scheme_description'),true);
 end;

 class procedure TFRE_DB_MACHINE_SETTING.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
     StoreTranslateableText(conn,'scheme_main','Properties');
     StoreTranslateableText(conn,'scheme_name','Name');
     StoreTranslateableText(conn,'scheme_description','Description');
   end;
 end;

 { TFRE_DB_FC_PORT }

 class procedure TFRE_DB_FC_PORT.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.Classname);
   scheme.GetSchemeField('objname').required:=true;
   scheme.AddSchemeField('targetmode',fdbft_boolean);
   scheme.AddSchemeField('portnr',fdbft_UInt16);
   scheme.AddSchemeField('manufacturer',fdbft_String);
   scheme.AddSchemeField('model',fdbft_String);
   scheme.AddSchemeField('firmware',fdbft_String);
   scheme.AddSchemeField('biosversion',fdbft_String);
   scheme.AddSchemeField('serial',fdbft_String);
   scheme.AddSchemeField('driver',fdbft_String);
   scheme.AddSchemeField('driverversion',fdbft_String);
   scheme.AddSchemeField('porttype',fdbft_String);
   scheme.AddSchemeField('state',fdbft_String);
   scheme.AddSchemeField('supportedspeeds',fdbft_String);
   scheme.AddSchemeField('currentspeed',fdbft_String);
   scheme.AddSchemeField('nodewwn',fdbft_String);

   group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main'));
   group.AddInput('objname',GetTranslateableTextKey('scheme_wwn'),true);
   group.AddInput('desc.txt',GetTranslateableTextKey('scheme_description'));
   group.AddInput('targetmode',GetTranslateableTextKey('scheme_targetmode'));
   group.AddInput('portnr',GetTranslateableTextKey('scheme_portnr'),true);
   group.AddInput('manufacturer',GetTranslateableTextKey('scheme_manufacturer'),true);
   group.AddInput('model',GetTranslateableTextKey('scheme_model'),true);
   group.AddInput('firmware',GetTranslateableTextKey('scheme_firmware'),true);
   group.AddInput('biosversion',GetTranslateableTextKey('scheme_biosversion'),true);
   group.AddInput('serial',GetTranslateableTextKey('scheme_serial'),true);
   group.AddInput('driver',GetTranslateableTextKey('scheme_driver'),true);
   group.AddInput('driverversion',GetTranslateableTextKey('scheme_driverversion'),true);
   group.AddInput('porttype',GetTranslateableTextKey('scheme_porttype'),true);
   group.AddInput('state',GetTranslateableTextKey('scheme_state'),true);
   group.AddInput('supportedspeeds',GetTranslateableTextKey('scheme_supportedspeeds'),true);
   group.AddInput('currentspeed',GetTranslateableTextKey('scheme_currentspeed'),true);
   group.AddInput('nodewwn',GetTranslateableTextKey('scheme_nodewwn'),true);
 end;

 class procedure TFRE_DB_FC_PORT.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
     StoreTranslateableText(conn,'scheme_main','FC Adapter Port');
     StoreTranslateableText(conn,'scheme_wwn','Port WWN');
     StoreTranslateableText(conn,'scheme_description','Description');
     StoreTranslateableText(conn,'scheme_targetmode','Targetmode');
     StoreTranslateableText(conn,'scheme_portnr','Port ID');
     StoreTranslateableText(conn,'scheme_manufacturer','Manufacturer');
     StoreTranslateableText(conn,'scheme_model','Model');
     StoreTranslateableText(conn,'scheme_firmware','Firmware');
     StoreTranslateableText(conn,'scheme_biosversion','Bios Version');
     StoreTranslateableText(conn,'scheme_serial','Serial Number');
     StoreTranslateableText(conn,'scheme_driver','Driver');
     StoreTranslateableText(conn,'scheme_driverversion','Driver Version');
     StoreTranslateableText(conn,'scheme_porttype','Port Type');
     StoreTranslateableText(conn,'scheme_state','State');
     StoreTranslateableText(conn,'scheme_supportedspeeds','Supported Speeds');
     StoreTranslateableText(conn,'scheme_currentspeed','Current Speed');
     StoreTranslateableText(conn,'scheme_nodewwn','Node WWN');
   end;
 end;

 { TFRE_DB_DATALINK_STUB }

 class procedure TFRE_DB_DATALINK_STUB.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_DATALINK.ClassName);
 end;

 class procedure TFRE_DB_DATALINK_STUB.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.1';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
   end;
   if currentVersionId='1.0' then begin
     currentVersionId := '1.1';
     StoreTranslateableText(conn,'caption','Stub');

     StoreTranslateableText(conn,'scheme_main_group','General Information');
     StoreTranslateableText(conn,'scheme_name','Name');
     StoreTranslateableText(conn,'scheme_description','Description');
     StoreTranslateableText(conn,'scheme_mtu','MTU');
   end;
 end;

 function TFRE_DB_DATALINK_STUB.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
 var res       : TFRE_DB_MENU_DESC;
     func      : TFRE_DB_SERVER_FUNC_DESC;
 begin
   res:=TFRE_DB_MENU_DESC.create.Describe;
   func:=CSF(@IMI_Delete);
   res.AddEntry.Describe(input.Field('delete_stub').asstring,'images_apps/hal/delete_stub.png',func);
   func:=CSF(@IMI_AddVNIC);
   res.AddEntry.Describe(input.Field('add_vnic').asstring,'images_apps/hal/add_vnic.png',func);
   Result:=res;
 end;

 function TFRE_DB_DATALINK_STUB.IMI_AddVNIC(const input: IFRE_DB_Object): IFRE_DB_Object;
 begin
   result :=  TFRE_DB_MESSAGE_DESC.create.Describe('','Feature disabled in Demo Mode',fdbmt_info,nil);
 end;

 function TFRE_DB_DATALINK_STUB.IMI_Delete(const input: IFRE_DB_Object): IFRE_DB_Object;
 begin
   result :=  TFRE_DB_MESSAGE_DESC.create.Describe('','Feature disabled in Demo Mode',fdbmt_info,nil);
 end;

class function TFRE_DB_DATALINK_STUB.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

 { TFRE_DB_DATALINK_AGGR }

 class procedure TFRE_DB_DATALINK_AGGR.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_DATALINK.ClassName);
 end;

 class procedure TFRE_DB_DATALINK_AGGR.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.1';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
   end;
   if currentVersionId='1.0' then begin
     currentVersionId := '1.1';
     StoreTranslateableText(conn,'caption','Aggregation');

     StoreTranslateableText(conn,'scheme_main_group','General Information');
     StoreTranslateableText(conn,'scheme_name','Name');
     StoreTranslateableText(conn,'scheme_description','Description');
     StoreTranslateableText(conn,'scheme_mtu','MTU');
   end;
 end;

 function TFRE_DB_DATALINK_AGGR.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
 var res       : TFRE_DB_MENU_DESC;
     func      : TFRE_DB_SERVER_FUNC_DESC;
 begin
   res:=TFRE_DB_MENU_DESC.create.Describe;
   func:=CSF(@IMI_Delete);
   res.AddEntry.Describe(input.Field('delete_aggr').asstring,'images_apps/hal/delete_aggr.png',func);
   func:=CSF(@IMI_AddVNIC);
   res.AddEntry.Describe(input.Field('add_vnic').asstring,'images_apps/hal/add_vnic.png',func);
   Result:=res;
 end;

 function TFRE_DB_DATALINK_AGGR.IMI_AddVNIC(const input: IFRE_DB_Object): IFRE_DB_Object;
 begin
   result :=  TFRE_DB_MESSAGE_DESC.create.Describe('','Feature disabled in Demo Mode',fdbmt_info,nil);
 end;

 function TFRE_DB_DATALINK_AGGR.IMI_Delete(const input: IFRE_DB_Object): IFRE_DB_Object;
 begin
   result :=  TFRE_DB_MESSAGE_DESC.create.Describe('','Feature disabled in Demo Mode',fdbmt_info,nil);
 end;

class function TFRE_DB_DATALINK_AGGR.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

 { TFRE_DB_DATALINK_VNIC }

 class procedure TFRE_DB_DATALINK_VNIC.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_DATALINK.ClassName);
   scheme.AddSchemeField('vlan',fdbft_Uint16);
   group:=scheme.AddInputGroup('vlan').Setup(GetTranslateableTextKey('scheme_vlan_group'));
   group.AddInput('vlan',GetTranslateableTextKey('scheme_vlan'));
 end;

 class procedure TFRE_DB_DATALINK_VNIC.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.1';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
   end;
   if currentVersionId='1.0' then begin
     currentVersionId := '1.1';
     StoreTranslateableText(conn,'scheme_vlan_group','Vlan Properties');
     StoreTranslateableText(conn,'scheme_vlan','Vlan');

     StoreTranslateableText(conn,'caption','VNIC');

     StoreTranslateableText(conn,'scheme_main_group','General Information');
     StoreTranslateableText(conn,'scheme_name','Name');
     StoreTranslateableText(conn,'scheme_description','Description');
     StoreTranslateableText(conn,'scheme_mtu','MTU');
   end;
 end;

 function TFRE_DB_DATALINK_VNIC.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
 var res       : TFRE_DB_MENU_DESC;
     func      : TFRE_DB_SERVER_FUNC_DESC;
 begin
   res:=TFRE_DB_MENU_DESC.create.Describe;
   func:=CSF(@IMI_Delete);
   res.AddEntry.Describe(input.Field('delete_vnic').asstring,'images_apps/hal/delete_vnic.png',func);
   Result:=res;
 end;

 function TFRE_DB_DATALINK_VNIC.IMI_Delete(const input: IFRE_DB_Object): IFRE_DB_Object;
 begin
   result :=  TFRE_DB_MESSAGE_DESC.create.Describe('','Feature disabled in Demo Mode',fdbmt_info,nil);
 end;

 function TFRE_DB_DATALINK_VNIC.RIF_CreateVNIC: IFRE_DB_Object;
 var parent_if  : string;
     mac        : TFOS_MAC_ADDR;
     vlan       : Uint16;
     zonename   : string;
     err        : string;
 begin
  {$IFDEF SOLARIS}
   writeln('CREATE VNIC START');
  //writeln('SWL DUMP:',self.DumpToString());
   parent_if := Field('parent').AsObject.Field('objname').AsString;
   writeln('SWL: PARENT IF:',parent_if);
   if FieldExists('zonename') then
     zonename  := Field('zonename').asstring
   else
     zonename  := '';
   if FieldExists('UNIQUEPHYSICALID') then
     mac.SetFromString(Field('UNIQUEPHYSICALID').asstring)
   else
     mac.GenerateRandom;
   writeln('SWL: MAC ',mac.GetAsString);

   if FieldExists('vlan') then
     vlan := Field('vlan').AsUInt16
   else
     vlan := 0;

   writeln('SWL: VLAN ',vlan);

   if zonename<>'' then
     begin
       writeln('CREATE VNIC ZONED ',mac.GetAsString,' ',create_vnic(ObjectName,parent_if,mac,err,zonename,vlan),' ',err);
      end
    else
      begin
        writeln('CREATE VNIC ',mac.GetAsString,' ',create_vnic(ObjectName,parent_if,mac,err,'',vlan),' ',err);
      end;
   writeln('CREATE VNIC DONE');
   {$ENDIF}
 end;

class function TFRE_DB_DATALINK_VNIC.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

 { TFRE_DB_DATALINK_PHYS }

 class procedure TFRE_DB_DATALINK_PHYS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_DATALINK.ClassName);
 end;

 class procedure TFRE_DB_DATALINK_PHYS.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.1';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
   end;
   if currentVersionId='1.0' then begin
     currentVersionId := '1.1';

     StoreTranslateableText(conn,'scheme_main_group','Link Properties');
     StoreTranslateableText(conn,'scheme_name','Link Name');
     StoreTranslateableText(conn,'scheme_description','Description');
     StoreTranslateableText(conn,'scheme_mtu','MTU');
   end;
 end;

 function TFRE_DB_DATALINK_PHYS.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
 var res       : TFRE_DB_MENU_DESC;
     func      : TFRE_DB_SERVER_FUNC_DESC;
 begin
   if Field('parentid').ValueCount=0 then
     begin
       res:=TFRE_DB_MENU_DESC.create.Describe;
       func:=CSF(@IMI_AddVNIC);
       res.AddEntry.Describe(input.Field('add_vnic').asstring,'images_apps/hal/add_vnic.png',func);
       Result:=res;
     end
   else
     result := GFRE_DB_NIL_DESC;
 end;

 function TFRE_DB_DATALINK_PHYS.IMI_AddVNIC(const input: IFRE_DB_Object): IFRE_DB_Object;
 begin
   result :=  TFRE_DB_MESSAGE_DESC.create.Describe('','Feature disabled in Demo Mode',fdbmt_info,nil);
 end;

 { TFRE_DB_DATALINK }

 class procedure TFRE_DB_DATALINK.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
     enum  : IFRE_DB_Enum;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_SERVICE.Classname);

   enum:=GFRE_DBI.NewEnum('datalink_network_type').Setup(GFRE_DBI.CreateText('$datalink_network_type','Datalink Network Type'));
   enum.addEntry('generic',GetTranslateableTextKey('datalink_network_type_generic'));
   enum.addEntry('internet',GetTranslateableTextKey('datalink_network_type_internet'));
   enum.addEntry('lan',GetTranslateableTextKey('datalink_network_type_lan'));
   enum.addEntry('mgmt',GetTranslateableTextKey('datalink_network_type_mgmt'));
   enum.addEntry('cpe',GetTranslateableTextKey('datalink_network_type_cpe'));
   enum.addEntry('vm',GetTranslateableTextKey('datalink_network_type_vm'));
   enum.addEntry('link',GetTranslateableTextKey('datalink_network_type_link'));
   GFRE_DBI.RegisterSysEnum(enum);

   scheme.GetSchemeField('objname').required:=true;
   scheme.AddSchemeField('datalinkParent',fdbft_ObjLink);
   scheme.AddSchemeField('zoneId',fdbft_ObjLink);
   scheme.AddSchemeField('mtu',fdbft_Uint16);
   scheme.AddSchemeField('type',fdbft_String).SetupFieldDef(true,false,'datalink_network_type');

   group:=scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
   group.AddInput('objname',GetTranslateableTextKey('scheme_name'));
   group.AddInput('desc.txt',GetTranslateableTextKey('scheme_description'));
   group.AddInput('mtu',GetTranslateableTextKey('scheme_mtu'));
 end;

 class procedure TFRE_DB_DATALINK.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.2';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
     StoreTranslateableText(conn,'scheme_main_group','Link Properties');
     StoreTranslateableText(conn,'scheme_name','Link Name');
     StoreTranslateableText(conn,'scheme_description','Description');
     StoreTranslateableText(conn,'scheme_ip_net','IP/Subnet');
     StoreTranslateableText(conn,'scheme_mtu','MTU');
     StoreTranslateableText(conn,'scheme_vlan','Vlan');
   end;
   if currentVersionId='1.0' then begin
     currentVersionId := '1.1';
     DeleteTranslateableText(conn,'scheme_ip_net');
     DeleteTranslateableText(conn,'scheme_vlan');
   end;
   if currentVersionId='1.1' then begin
     currentVersionId := '1.2';
     StoreTranslateableText(conn,'datalink_network_type_generic','Generic Interface');
     StoreTranslateableText(conn,'datalink_network_type_internet','Internet');
     StoreTranslateableText(conn,'datalink_network_type_lan','Lokal Network');
     StoreTranslateableText(conn,'datalink_network_type_mgmt','Mgmt Network');
     StoreTranslateableText(conn,'datalink_network_type_cpe','CPE Network');
     StoreTranslateableText(conn,'datalink_network_type_vm','VM Interface');
     StoreTranslateableText(conn,'datalink_network_type_link','Zone Interlink');
   end;
 end;

 class function TFRE_DB_DATALINK.getAllDataLinkClasses: TFRE_DB_StringArray;
 begin
   Result:=TFRE_DB_StringArray.create(TFRE_DB_DATALINK_PHYS.ClassName,TFRE_DB_DATALINK_AGGR.ClassName,TFRE_DB_DATALINK_IPMP.ClassName,TFRE_DB_DATALINK_IPTUN.ClassName,TFRE_DB_DATALINK_STUB.ClassName,TFRE_DB_DATALINK_BRIDGE.ClassName,TFRE_DB_DATALINK_SIMNET.ClassName,TFRE_DB_DATALINK_VNIC.ClassName);
 end;

 procedure TFRE_DB_DATALINK.Embed(const conn: IFRE_DB_CONNECTION);
 var refs      : TFRE_DB_ObjectReferences;
    obj       : IFRE_DB_Object;
    i         : integer;
    ip        : TFRE_DB_IP_HOSTNET;
    dl        : TFRE_DB_DATALINK;
 begin
   inherited;
   refs := conn.GetReferencesDetailed(UID,false);
   for i:=0 to high(refs) do
     begin
       CheckDbResult(conn.Fetch(refs[i].linked_uid,obj),' could not fetch referencing object '+refs[i].linked_uid.AsHexString);
//         writeln(obj.DumpToString());
       if obj.IsA(TFRE_DB_IP_HOSTNET,ip) then
         self.Field(obj.UID.AsHexString).AsObject:=ip
       else
         obj.Finalize;
     end;
   // embed parent (datalink)
   if FieldExists('datalinkparent') then
     begin
       for i:=0 to field('datalinkparent').ValueCount-1 do
         begin
           CheckDbResult(conn.Fetch(field('datalinkparent').AsObjectLinkItem[i],obj),' could not fetch parent object '+field('datalinkparent').AsObjectLinkItem[i].AsHexString);
           if obj.IsA(TFRE_DB_DATALINK,dl) then
             self.Field('PARENT').AsObject:=dl
           else
             obj.Finalize;
         end;
     end;
 end;

 function TFRE_DB_DATALINK.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
 begin
   writeln('DATALINK MENU');
   result := GFRE_DB_NIL_DESC;
 end;

  function TFRE_DB_DATALINK.RIF_CreateOrUpdateServices: IFRE_DB_Object;
 var
   oldsvclist : IFRE_DB_Object;
   resultdatalink : IFRE_DB_Object;

   {$IFDEF SOLARIS}
   procedure _CreateorUpdateIPHOSTNET(const obj:IFRE_DB_Object);
   var iphostnet   : TFRE_DB_IP_HOSTNET;
       resdbo      : IFRE_DB_Object;
       foundobj    : IFRE_DB_Object;
       servicename : string;
       fmri        : string;
   begin
     if obj.IsA(TFRE_DB_IP_HOSTNET,iphostnet) then
       begin
         iphostnet.Field('datalinkname').asstring := ObjectName;
         fmri        := iphostnet.GetFMRI;
//         writeln('SWL:',fmri);
         if oldsvclist.FetchObjWithStringFieldValue('fmri',fmri,foundobj,'') then
           begin
//             writeln('SWL: SVC ALREADY CREATED');
           end
         else
           begin
             resdbo := iphostnet.RIF_CreateOrUpdateService;
             resdbo.Field('UID').AsGUID:=iphostnet.UID;
//             writeln('SWL:',resdbo.DumpToString);
           end;
         resultdatalink.Field(iphostnet.UID_String).AsString := fmri;
       end;
   end;

   {$ENDIF}
 begin
   {$IFDEF SOLARIS}
   resultdatalink := GFRE_DBI.NewObject;
   result         := resultdatalink;
   if ObjectName='' then exit;  // TODO FS: REMOVE AFTER ALL SERVICES ARE SET IN ZONE

   oldsvclist := fre_get_servicelist('fos_ip_'+Objectname);
//   writeln('SWL NEW LIST:',oldsvclist.DumpToString);
   try
     ForAllObjects(@_CreateorUpdateIPHOSTNET);
//     writeln('SWL LIST AFTER:',oldsvclist.DumpToString);
   finally
     oldsvclist.Finalize;
   end;
   {$ENDIF}
 end;

 class function TFRE_DB_DATALINK.WBC_GetConfig(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=inherited WBC_GetConfig(input, ses, app, conn);
  Result.Field('Type').AsString:='datalink';
end;

 { TFRE_DB_TESTER }

 class procedure TFRE_DB_TESTER.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 begin
   inherited RegisterSystemScheme(scheme);
 end;

 class procedure TFRE_DB_TESTER.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
   end;
 end;

function TFRE_DB_VMACHINE.getVNCHost: TFRE_DB_String;
begin
  Result:=Field('vnc_host').AsString;
end;

function TFRE_DB_VMACHINE.getVNCPort: UInt32;
begin
  Result:=Field('vnc_port').AsUInt32;
end;

procedure TFRE_DB_VMACHINE.setVNCHost(AValue: TFRE_DB_String);
begin
  Field('vnc_host').AsString:=AValue;
end;

procedure TFRE_DB_VMACHINE.setVNCPort(AValue: UInt32);
begin
  Field('vnc_port').AsUInt32:=AValue;
end;

 class procedure TFRE_DB_VMACHINE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var
   enum : IFRE_DB_Enum;
   group: IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_VHOST.ClassName);

   enum:=GFRE_DBI.NewEnum('qemu_cpu').Setup(GFRE_DBI.CreateText('$enum_qemu_cpu','CPU'));
   enum.addEntry('qemu64',GetTranslateableTextKey('enum_cpu_qemu64'));
   enum.addEntry('Opteron_G3',GetTranslateableTextKey('enum_cpu_Opteron_G3'));
   enum.addEntry('Opteron_G2',GetTranslateableTextKey('enum_cpu_Opteron_G2'));
   enum.addEntry('Opteron_G1',GetTranslateableTextKey('enum_cpu_Opteron_G1'));
   enum.addEntry('SandyBridge',GetTranslateableTextKey('enum_cpu_SandyBridge'));
   enum.addEntry('Westmere',GetTranslateableTextKey('enum_cpu_Westmere'));
   enum.addEntry('Nehalem',GetTranslateableTextKey('enum_cpu_Nehalem'));
   enum.addEntry('Penryn',GetTranslateableTextKey('enum_cpu_Penryn'));
   enum.addEntry('Conroe',GetTranslateableTextKey('enum_cpu_Conroe'));
   enum.addEntry('n270',GetTranslateableTextKey('enum_cpu_n270'));
   enum.addEntry('athlon',GetTranslateableTextKey('enum_cpu_athlon'));
   enum.addEntry('pentium3',GetTranslateableTextKey('enum_cpu_pentium3'));
   enum.addEntry('pentium2',GetTranslateableTextKey('enum_cpu_pentium2'));
   enum.addEntry('pentium',GetTranslateableTextKey('enum_cpu_pentium'));
   enum.addEntry('coreduo',GetTranslateableTextKey('enum_cpu_coreduo'));
   enum.addEntry('kvm32',GetTranslateableTextKey('enum_cpu_kvm32'));
   enum.addEntry('qemu32',GetTranslateableTextKey('enum_cpu_qemu32'));
   enum.addEntry('kvm64',GetTranslateableTextKey('enum_cpu_kvm64'));
   enum.addEntry('core2duo',GetTranslateableTextKey('enum_cpu_core2duo'));
   enum.addEntry('phenom',GetTranslateableTextKey('enum_cpu_phenom'));
   enum.addEntry('host',GetTranslateableTextKey('enum_cpu_host'));

   GFRE_DBI.RegisterSysEnum(enum);
   enum:=GFRE_DBI.NewEnum('qemu_language').Setup(GFRE_DBI.CreateText('$enum_qemu_language','Language'));
   GFRE_DBI.RegisterSysEnum(enum);
   enum.addEntry('de',GetTranslateableTextKey('enum_language_de'));
   enum.addEntry('de-ch',GetTranslateableTextKey('enum_language_de-ch'));
   enum.addEntry('en-gb',GetTranslateableTextKey('enum_language_en-gb'));
   enum.addEntry('en-us',GetTranslateableTextKey('enum_language_en-us'));
   enum.addEntry('it',GetTranslateableTextKey('enum_language_it'));
   enum.addEntry('es',GetTranslateableTextKey('enum_language_es'));
   enum.addEntry('fr',GetTranslateableTextKey('enum_language_fr'));
   enum.addEntry('fr-ch',GetTranslateableTextKey('enum_language_fr-ch'));
   enum.addEntry('fr-ca',GetTranslateableTextKey('enum_language_fr-ca'));
   enum.addEntry('ar',GetTranslateableTextKey('enum_language_ar'));
   enum.addEntry('fo',GetTranslateableTextKey('enum_language_fo'));
   enum.addEntry('hu',GetTranslateableTextKey('enum_language_hu'));
   enum.addEntry('ja',GetTranslateableTextKey('enum_language_ja'));
   enum.addEntry('mk',GetTranslateableTextKey('enum_language_mk'));
   enum.addEntry('no',GetTranslateableTextKey('enum_language_no'));
   enum.addEntry('pt-br',GetTranslateableTextKey('enum_language_pt-br'));
   enum.addEntry('sv',GetTranslateableTextKey('enum_language_sv'));
   enum.addEntry('da',GetTranslateableTextKey('enum_language_da'));
   enum.addEntry('et',GetTranslateableTextKey('enum_language_et'));
   enum.addEntry('is',GetTranslateableTextKey('enum_language_is'));
   enum.addEntry('lt',GetTranslateableTextKey('enum_language_lt'));
   enum.addEntry('nl',GetTranslateableTextKey('enum_language_nl'));
   enum.addEntry('pl',GetTranslateableTextKey('enum_language_pl'));
   enum.addEntry('ru',GetTranslateableTextKey('enum_language_ru'));
   enum.addEntry('th',GetTranslateableTextKey('enum_language_th'));
   enum.addEntry('fi',GetTranslateableTextKey('enum_language_fi'));
   enum.addEntry('fr-be',GetTranslateableTextKey('enum_language_fr-be'));
   enum.addEntry('hr',GetTranslateableTextKey('enum_language_hr'));
   enum.addEntry('lv',GetTranslateableTextKey('enum_language_lv'));
   enum.addEntry('nl-be',GetTranslateableTextKey('enum_language_nl-be'));
   enum.addEntry('pt',GetTranslateableTextKey('enum_language_pt'));
   enum.addEntry('sl',GetTranslateableTextKey('enum_language_sl'));
   enum.addEntry('tr',GetTranslateableTextKey('enum_language_tr'));
   enum:=GFRE_DBI.NewEnum('qemu_vga').Setup(GFRE_DBI.CreateText('$enum_qemu_vga','VGA'));
   enum.addEntry('cirrus',GetTranslateableTextKey('enum_vga_cirrus'));
   enum.addEntry('qxl',GetTranslateableTextKey('enum_vga_qxl'));
   GFRE_DBI.RegisterSysEnum(enum);
   enum:=GFRE_DBI.NewEnum('qemu_usbdevice').Setup(GFRE_DBI.CreateText('$enum_qemu_usbdevice','USB-Device'));
   enum.addEntry('mouse',GetTranslateableTextKey('enum_usbdevice_mouse'));
   enum.addEntry('tablet',GetTranslateableTextKey('enum_usbdevice_tablet'));
   enum.addEntry('disk-file',GetTranslateableTextKey('enum_usbdevice_disk-file'));
   enum.addEntry('wacom-tablet',GetTranslateableTextKey('enum_usbdevice_wacom-tablet'));
   enum.addEntry('keyboard',GetTranslateableTextKey('enum_usbdevice_keyboard'));
   GFRE_DBI.RegisterSysEnum(enum);
   enum:=GFRE_DBI.NewEnum('qemu_boot').Setup(GFRE_DBI.CreateText('$enum_qemu_boot','Boot'));
   enum.addEntry('cd',GetTranslateableTextKey('enum_boot_cd'));
   enum.addEntry('dc',GetTranslateableTextKey('enum_boot_dc'));
   enum.addEntry('acd',GetTranslateableTextKey('enum_boot_acd'));
   GFRE_DBI.RegisterSysEnum(enum);

   scheme.AddSchemeField('cpu',fdbft_String).SetupFieldDef(true,false,'qemu_cpu');
   scheme.AddSchemeField('cores',fdbft_Int16).SetupFieldDefNum(true,1,64);
   scheme.AddSchemeField('threads',fdbft_Int16).SetupFieldDefNum(true,1,64);
   scheme.AddSchemeField('sockets',fdbft_Int16).SetupFieldDefNum(true,1,64);
   scheme.AddSchemeField('ram',fdbft_Int32).SetupFieldDef(true);
   scheme.AddSchemeField('language',fdbft_String).SetupFieldDef(true,false,'qemu_language');
   scheme.AddSchemeField('vga',fdbft_String).SetupFieldDef(true,false,'qemu_vga');
   scheme.AddSchemeField('usbdevice',fdbft_String).SetupFieldDef(false,true,'qemu_usbdevice');
   scheme.AddSchemeField('boot',fdbft_String).SetupFieldDef(true,false,'qemu_boot');
   scheme.AddSchemeField('snapshot',fdbft_Boolean);

   group:=scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
   group.AddInput('objname',GetTranslateableTextKey('scheme_objname'));
   group.AddInput('cpu',GetTranslateableTextKey('scheme_cpu'));
   group.AddInput('cores',GetTranslateableTextKey('scheme_cores'));
   group.AddInput('threads',GetTranslateableTextKey('scheme_threads'));
   group.AddInput('sockets',GetTranslateableTextKey('scheme_sockets'));
   group.AddInput('ram',GetTranslateableTextKey('scheme_ram'));
   group.AddInput('language',GetTranslateableTextKey('scheme_language'));
   group.AddInput('vga',GetTranslateableTextKey('scheme_vga'));
   group.AddInput('usbdevice',GetTranslateableTextKey('scheme_usbdevice'),false,false,'','',false,dh_chooser_check);
   group.AddInput('boot',GetTranslateableTextKey('scheme_boot'));
   group.AddInput('snapshot',GetTranslateableTextKey('scheme_snapshot'));

//   https://wiki.firmos.at/display/FBX/Qemu+Parameters
//  Parameters to configure
// -name (set default to servicename)
// -cpu  (enum)
// -smp   (3 Felder)
// -m     (Ram in MB)
// -k     (enum)
// -vga   (enum)
// -usbdevice, possible to add one ore more
// -boot  (enum)
// -snapshot   (boolean)

// Expert Configuration:
//-no-acpi        disable ACPI
//-no-hpet        disable HPET
//-no-kvm         disable KVM hardware virtualization
//-no-kvm-irqchip disable KVM kernel mode PIC/IOAPIC/LAPIC
//-no-kvm-pit     disable KVM kernel mode PIT
//-no-kvm-pit-reinjection
// -balloon (enum)
//(all default false)
//emulator (enum)
//0.14.1
//1.1.2  (default

 end;

 class procedure TFRE_DB_VMACHINE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.1';
   if currentVersionId='' then begin
     currentVersionId := '1.0';

     StoreTranslateableText(conn,'enum_state_stopped','Stopped');
     StoreTranslateableText(conn,'enum_state_running','Running');
     StoreTranslateableText(conn,'enum_state_stopping','Stopping');

     StoreTranslateableText(conn,'enum_type_kvm','KVM');
     StoreTranslateableText(conn,'enum_type_os','OS');
   end;
   if currentVersionId='1.0' then begin
     currentVersionId := '1.1';
     DeleteTranslateableText(conn,'enum_state_stopped');
     DeleteTranslateableText(conn,'enum_state_running');
     DeleteTranslateableText(conn,'enum_state_stopping');

     DeleteTranslateableText(conn,'enum_type_kvm');
     DeleteTranslateableText(conn,'enum_type_os');

     StoreTranslateableText(conn,'caption','Virtual Machine');

     StoreTranslateableText(conn,'scheme_main_group','General Information');
     StoreTranslateableText(conn,'scheme_objname','Name');
     StoreTranslateableText(conn,'scheme_cpu','CPU');
     StoreTranslateableText(conn,'scheme_cores','Cores');
     StoreTranslateableText(conn,'scheme_threads','Threads');
     StoreTranslateableText(conn,'scheme_sockets','Sockets');
     StoreTranslateableText(conn,'scheme_ram','RAM (MB)');
     StoreTranslateableText(conn,'scheme_language','Language');
     StoreTranslateableText(conn,'scheme_vga','VGA');
     StoreTranslateableText(conn,'scheme_usbdevice','USB Device');
     StoreTranslateableText(conn,'scheme_boot','Boot Order');
     StoreTranslateableText(conn,'scheme_snapshot','Snapshot');

     StoreTranslateableText(conn,'enum_cpu_qemu64','qemu64');
     StoreTranslateableText(conn,'enum_cpu_Opteron_G3','Opteron_G3');
     StoreTranslateableText(conn,'enum_cpu_Opteron_G2','Opteron_G2');
     StoreTranslateableText(conn,'enum_cpu_Opteron_G1','Opteron_G1');
     StoreTranslateableText(conn,'enum_cpu_SandyBridge','SandyBridge');
     StoreTranslateableText(conn,'enum_cpu_Westmere','Westmere');
     StoreTranslateableText(conn,'enum_cpu_Nehalem','Nehalem');
     StoreTranslateableText(conn,'enum_cpu_Penryn','Penryn');
     StoreTranslateableText(conn,'enum_cpu_Conroe','Conroe');
     StoreTranslateableText(conn,'enum_cpu_n270','n270');
     StoreTranslateableText(conn,'enum_cpu_athlon','athlon');
     StoreTranslateableText(conn,'enum_cpu_pentium3','pentium3');
     StoreTranslateableText(conn,'enum_cpu_pentium2','pentium2');
     StoreTranslateableText(conn,'enum_cpu_pentium','pentium');
     StoreTranslateableText(conn,'enum_cpu_coreduo','coreduo');
     StoreTranslateableText(conn,'enum_cpu_kvm32','kvm32');
     StoreTranslateableText(conn,'enum_cpu_qemu32','qemu32');
     StoreTranslateableText(conn,'enum_cpu_kvm64','kvm64');
     StoreTranslateableText(conn,'enum_cpu_core2duo','core2duo');
     StoreTranslateableText(conn,'enum_cpu_phenom','phenom');
     StoreTranslateableText(conn,'enum_cpu_host','host');

     StoreTranslateableText(conn,'enum_language_ar','ar');
     StoreTranslateableText(conn,'enum_language_de-ch','de-ch');
     StoreTranslateableText(conn,'enum_language_es','es');
     StoreTranslateableText(conn,'enum_language_fo','fo');
     StoreTranslateableText(conn,'enum_language_fr','fr');
     StoreTranslateableText(conn,'enum_language_fr-ca','fr-ca');
     StoreTranslateableText(conn,'enum_language_hu','hu');
     StoreTranslateableText(conn,'enum_language_ja','ja');
     StoreTranslateableText(conn,'enum_language_mk','mk');
     StoreTranslateableText(conn,'enum_language_no','no');
     StoreTranslateableText(conn,'enum_language_pt-br','pt-br');
     StoreTranslateableText(conn,'enum_language_sv','sv');
     StoreTranslateableText(conn,'enum_language_da','da');
     StoreTranslateableText(conn,'enum_language_en-gb','en-gb');
     StoreTranslateableText(conn,'enum_language_et','et');
     StoreTranslateableText(conn,'enum_language_fr-ch','fr-ch');
     StoreTranslateableText(conn,'enum_language_is','is');
     StoreTranslateableText(conn,'enum_language_lt','lt');
     StoreTranslateableText(conn,'enum_language_nl','nl');
     StoreTranslateableText(conn,'enum_language_pl','pl');
     StoreTranslateableText(conn,'enum_language_ru','ru');
     StoreTranslateableText(conn,'enum_language_th','th');
     StoreTranslateableText(conn,'enum_language_de','de');
     StoreTranslateableText(conn,'enum_language_en-us','en-us');
     StoreTranslateableText(conn,'enum_language_fi','fi');
     StoreTranslateableText(conn,'enum_language_fr-be','fr-be');
     StoreTranslateableText(conn,'enum_language_hr','hr');
     StoreTranslateableText(conn,'enum_language_it','it');
     StoreTranslateableText(conn,'enum_language_lv','lv');
     StoreTranslateableText(conn,'enum_language_nl-be','nl-be');
     StoreTranslateableText(conn,'enum_language_pt','pt');
     StoreTranslateableText(conn,'enum_language_sl','sl');
     StoreTranslateableText(conn,'enum_language_tr','tr');

     StoreTranslateableText(conn,'enum_vga_cirrus','cirrus');
     StoreTranslateableText(conn,'enum_vga_qxl','qxl');

     StoreTranslateableText(conn,'enum_boot_acd','Floppy First');
     StoreTranslateableText(conn,'enum_boot_cd','HD First');
     StoreTranslateableText(conn,'enum_boot_dc','CD First');

     StoreTranslateableText(conn,'enum_usbdevice_mouse','Mouse');
     StoreTranslateableText(conn,'enum_usbdevice_tablet','Tablet');
     StoreTranslateableText(conn,'enum_usbdevice_disk-file','Disk:File');
     StoreTranslateableText(conn,'enum_usbdevice_wacom-tablet','Wacom-Tablet');
     StoreTranslateableText(conn,'enum_usbdevice_keyboard','Keyboard');
   end;
 end;

 procedure TFRE_DB_VMACHINE.CALC_GetDisplayName(const setter: IFRE_DB_CALCFIELD_SETTER);
 begin
   setter.SetAsString('VM: '+Field('objname').AsString);
 end;

 function TFRE_DB_VMACHINE.RIF_CreateOrUpdateService: IFRE_DB_Object;
 var servicename : string;
     pidfile     : string;
     qemubin     : string;
     qmbsocket   : string;

     sl          : TStringList;

     procedure ConfigureComponents(const obj:IFRE_DB_Object);
     var disk        : TFRE_DB_VMACHINE_DISK;
         nic         : TFRE_DB_VMACHINE_NIC;
         nicvlan     : string;
         nicname     : string;
         zvol        : TFRE_DB_ZFS_DATASET_ZVOL;

         procedure   CreateZVol(const zvol:TFRE_DB_ZFS_DATASET);
         var
           errs      : string;
         begin
          writeln('SWL CREATE ZVOL');
          {$IFDEF SOLARIS}
          fosillu_zfs_create_zvol(zvol.Field('fulldatasetname').asstring,zvol.Field('size_mb').AsUInt32*1024*1024,8192,false,errs);
          if errs<>'' then
            begin
              writeln('Error on creating zvol ',zvol.Field('fulldatasetname').asstring);
//                  raise Exception.Create('error on creating '+dsname+' dataset for zone :'+errs);
            end;
          {$ENDIF}
         end;

     begin
       if obj.IsA(TFRE_DB_VMACHINE_DISK,disk) then
         begin
           writeln('SWL VM DISK');
           if disk.Field('zvol_embed').AsObject.IsA(TFRE_DB_ZFS_DATASET_ZVOL,zvol) then
             begin
               CreateZVol(zvol);
               sl.add('-drive file=/dev/zvol/rdsk/'+zvol.Field('fulldatasetname').asstring+',if=ide,index='+disk.Field('index').asstring+' \');
             end;
         end;
       //if obj.IsA(TFRE_DB_VMACHINE_DISK_ISO,disk_iso) then
       //  begin
       //    writeln('SWL VM ISO');
       //    sl.add('-drive file='+disk_iso.Field('filename').asstring+',media=cdrom,if=ide,index='+disk_iso.Field('index').asstring+' \');
       //  end;
       if obj.IsA(TFRE_DB_VMACHINE_NIC,nic) then
         begin
           writeln('SWL VM NIC');
           nicvlan :=nic.Field('vm_vlan').asstring;
           nicname :=nic.Field('nic_embed').asobject.Field('objname').asstring;
           sl.add('-device virtio-net-pci,mac='+nic.Field('nic_embed').asobject.Field('uniquephysicalid').asstring+',tx=timer,x-txtimer=200000,x-txburst=128,vlan='+nicvlan+' \');
           sl.add('-net vnic,name='+nicname+',vlan='+nicvlan+',ifname='+nicname+' \');
         end;
     end;

 begin
   {$IFDEF SOLARIS}
     result := GFRE_DBI.NewObject;
     servicename := Copy(GetFMRI,6,maxint);
     SetSvcNameandType(servicename,'FirmOS VM Service ','transient','core,signal',true);
     SetSvcEnvironment('/','root','root','LANG=C');
     SetSvcStart('sh /opt/local/etc/kvm/start_qemu_'+UID.AsHexString,60);
     SetSvcStop('sh /opt/local/etc/kvm/stop_qemu_'+UID.AsHexString,180);

     AddSvcDependency('network','svc:/milestone/network:default','require_all','none');
     AddSvcDependency('filesystem-local','svc:/system/filesystem/local','require_all','error');

     fre_create_service(self);
     result.Field('fmri').asstring:=servicename;

     // Start Script

     ForceDirectories('/opt/local/etc/kvm');
     qemubin   :='/opt/local/qemu112/bin/qemu-system-x86_64';
     qmbsocket :='/var/run/qmp-'+UID_String;
     pidfile   :='/var/run/qemu-'+UID_String+'.pid';
     sl:=TStringList.Create;
     try
       sl.Add(qemubin+' \');
       sl.Add('-vnc '+vncHost+':'+inttostr(vncPort-5900)+' \');
       sl.Add('-boot cd \');      //FIXXME
       sl.Add('-enable-kvm \');
       sl.Add('-smp '+inttostr(CpuCores*CpuSockets)+',cores='+inttostr(CpuCores)+',threads=1,sockets='+inttostr(CpuSockets)+' \');
       sl.add('-m '+inttostr(MemoryMB)+' \');
       sl.add('-cpu qemu64 \');
       sl.add('-usb -usbdevice tablet -k de \');
       sl.add('-qmp unix:'+qmbsocket+',server,nowait \');
       sl.add('-daemonize \');
       sl.add('-pidfile '+pidfile+' \');
       // #-no-acpi

       ForAllObjects(@ConfigureComponents,true);
       writeln('SWL VM COMPONENTS DONE');

       sl.savetofile('/opt/local/etc/kvm/start_qemu_'+UID.AsHexString);

     finally
       sl.Free;
     end;

     // Stop Script

     sl:=TStringList.Create;
     try
       sl.add('PIDFILE='+pidfile);
       sl.add('QMP_SOCKET='+qmbsocket);
       sl.add('QMP_SHUTDOWN=''{ "execute": "qmp_capabilities" }{ "execute": "system_powerdown" }''');
       sl.add('TIMEOUT_SHUTDOWN=60');
       sl.add('TIMEOUT_TERMINATE=30');
       sl.add('PID=`cat "$PIDFILE"`');
       sl.add('if [ $PID -eq "" ] ; then');
       sl.add('  echo "no pid"');
       sl.add('  exit 0');
       sl.add('fi');
       sl.add('ps -p $PID >/dev/null 2>&1');
       sl.add('result=$?');
       sl.add('if [ $result -ne 0 ] ; then');
       sl.add('  echo "no process"');
       sl.add('  exit 0');
       sl.add('fi');
       sl.add('echo "$QMP_SHUTDOWN" | nc -U $QMP_SOCKET');
       sl.add('COUNTER=0');
       sl.add('result=0');
       sl.add('until [ $result -ne 0 ]; do');
       sl.add('  ps -p $PID >/dev/null 2>&1');
       sl.add('  result=$?');
       sl.add('  sleep 1');
       sl.add('  echo "waiting for shutdown $COUNTER $pid"');
       sl.add('  COUNTER=`expr $COUNTER + 1`');
       sl.add('  if [ "$COUNTER" -gt "$TIMEOUT_SHUTDOWN" ]; then');
       sl.add('    break');
       sl.add('  fi');
       sl.add('done');
       sl.add('ps -p $PID >/dev/null 2>&1');
       sl.add('result=$?');
       sl.add('if [ $result -ne 0 ]; then');
       sl.add('  exit 0');
       sl.add('fi');
       sl.add('echo "terminating"');
       sl.add('kill -TERM $PID');
       sl.add('COUNTER=0');
       sl.add('result=0');
       sl.add('until [ $result -ne 0 ]; do');
       sl.add('  ps -p $PID >/dev/null 2>&1');
       sl.add('  result=$?');
       sl.add('  sleep 1');
       sl.add('  echo "waiting for termination $COUNTER $pid"');
       sl.add('  COUNTER=`expr $COUNTER + 1`');
       sl.add('  if [ "$COUNTER" -gt "$TIMEOUT_TERMINATE" ]; then');
       sl.add('    break');
       sl.add('  fi');
       sl.add('done');
       sl.add('ps -p $PID >/dev/null 2>&1');
       sl.add(' result=$?');
       sl.add(' if [ $result -ne 0 ]; then');
       sl.add('   exit 0');
       sl.add(' fi');
       sl.add('echo "killing"');
       sl.add('kill -KILL $PID');
       sl.add('rm $PIDFILE');
       sl.add('exit 0');

       sl.savetoFile('/opt/local/etc/kvm/stop_qemu_'+UID.AsHexString);
     finally
       sl.Free;
     end;

   {$ENDIF}
 end;

class function TFRE_DB_VMACHINE.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

procedure TFRE_DB_VMACHINE.Embed(const conn: IFRE_DB_CONNECTION);
var
  refs      : TFRE_DB_ObjectReferences;
  obj       : IFRE_DB_Object;
  i         : integer;
  disk      : TFRE_DB_VMACHINE_DISK;
  nic       : TFRE_DB_VMACHINE_NIC;

begin
  refs := conn.GetReferencesDetailed(UID,false);
  for i:=0 to high(refs) do
    begin
      CheckDbResult(conn.Fetch(refs[i].linked_uid,obj),' could not fetch referencing object '+refs[i].linked_uid.AsHexString);
      if obj.IsA(TFRE_DB_VMACHINE_DISK,disk) then
        begin
          disk.Embed(conn);
          self.Field(obj.UID.AsHexString).AsObject:=disk;
        end
      else
        if obj.IsA(TFRE_DB_VMACHINE_NIC,nic) then
          begin
            nic.Embed(conn);
            self.Field(obj.UID.AsHexString).AsObject:=nic
          end
      else
        obj.Finalize;
    end;
end;

 function TFRE_DB_VMACHINE.GetFMRI: TFRE_DB_STRING;
 begin
   result := 'svc:/fos/fos_vm_'+UID.AsHexString;
 end;

 class procedure TFRE_DB_MACHINE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_ASSET.ClassName);
   scheme.AddSchemeField('provisioningmac',fdbft_String).SetupFieldDef(true,false,'','mac');
   scheme.AddSchemeFieldSubscheme('position','TFRE_DB_GEOPOSITION').required:=false;
   scheme.AddSchemeFieldSubscheme('address','TFRE_DB_ADDRESS').required:=false;
   scheme.AddCalcSchemeField('displayaddress',fdbft_String,@CALC_GetDisplayAddress);
   scheme.AddCalcSchemeField('displayname',fdbft_String,@CALC_GetDisplayName);

   scheme.AddCalcSchemeField('caption_mos',fdbft_String,@_getMOSCaption);
   scheme.AddSchemeField('status_mos',fdbft_String);
   scheme.AddCalcSchemeField('status_icon_mos',fdbft_String,@_getStatusIcon);

   group:=scheme.AddInputGroup('address').Setup(GetTranslateableTextKey('scheme_address_group'));
   group.UseInputGroup('TFRE_DB_ADDRESS','main','address');
   group.UseInputGroup('TFRE_DB_GEOPOSITION','main','position');

   group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main'));
   group.AddInput('objname',GetTranslateableTextKey('scheme_name'));
   group.AddInput('provisioningmac',GetTranslateableTextKey('scheme_provisioningmac'));
end;

 class procedure TFRE_DB_MACHINE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.1';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
     StoreTranslateableText(conn,'scheme_address_group','Site Address');
     StoreTranslateableText(conn,'scheme_main','General');
     StoreTranslateableText(conn,'scheme_name','Machine Name');
     StoreTranslateableText(conn,'machine_content_header_short','Machine Information');
   end;
   if currentVersionId='1.0' then begin
     currentVersionId := '1.1';
     DeleteTranslateableText(conn,'machine_content_header_short');
     StoreTranslateableText(conn,'machine_content_header','Machine Information');
     DeleteTranslateableText(conn,'scheme_main');
     StoreTranslateableText(conn,'scheme_main','General Information');
     StoreTranslateableText(conn,'scheme_provisioningmac','Mac');
   end;
 end;

procedure TFRE_DB_MACHINE._getMOSCaption(const calcfieldsetter: IFRE_DB_CALCFIELD_SETTER);
begin
  calcfieldsetter.SetAsString('Machine '+ObjectName);
end;

procedure TFRE_DB_MACHINE._getStatusIcon(const calc: IFRE_DB_CALCFIELD_SETTER);
begin
  case GetMOSStatus of
    fdbstat_ok     : calc.SetAsString('images_apps/citycom_monitoring/status_ok.png');
    fdbstat_warning: calc.SetAsString('images_apps/citycom_monitoring/status_warning.png');
    fdbstat_error  : calc.SetAsString('images_apps/citycom_monitoring/status_error.png');
    fdbstat_unknown: calc.SetAsString('images_apps/citycom_monitoring/status_unknown.png');
  else begin
    calc.SetAsString('images_apps/citycom_monitoring/status_unknown.png');
  end; end;
end;

 procedure TFRE_DB_MACHINE.DeleteReferencingToMe(const conn: IFRE_DB_CONNECTION);
 var refs: TFRE_DB_ObjectReferences;
        i: NativeInt;
     obj : IFRE_DB_Object;
     res : TFRE_DB_Errortype;
 begin
   refs := conn.GetReferencesDetailed(Uid,false);
   for i:=0 to high(refs) do
     begin
       res := conn.Fetch(refs[i].linked_uid,obj);
       if (res=edb_NOT_FOUND) then   // already deleted
         continue;
       if not (res=edb_NOT_FOUND) then
         CheckDbResult(res,'could not fetch referencing for delete ['+FREDB_G2H(refs[i].linked_uid));
       if (obj.Implementor_HC is TFRE_DB_ENCLOSURE) then
         begin
           (obj.Implementor_HC as TFRE_DB_ENCLOSURE).DeleteReferencingToMe(conn);
         end;
       if (obj.Implementor_HC is TFRE_DB_ZFS_POOL) then
         begin
           (obj.Implementor_HC as TFRE_DB_ZFS_POOL).DeleteReferencingVdevToMe(conn);
         end;
       if (obj.Implementor_HC is TFRE_DB_ZFS_DISKCONTAINER) then
         begin
           (obj.Implementor_HC as TFRE_DB_ZFS_DISKCONTAINER).DeleteReferencingVdevToMe(conn);
         end;
       if (obj.Implementor_HC is TFRE_DB_ZFS_BLOCKDEVICE) then
         begin
           (obj.Implementor_HC as TFRE_DB_ZFS_BLOCKDEVICE).UnassignReferencingDisksToMe(conn);
         end;
       obj.Finalize;
       res := conn.Delete(refs[i].linked_uid);
       if not ((res=edb_OK) or (res=edb_NOT_FOUND)) then
         CheckDbResult(res,'could not delete machine refs uid ['+FREDB_G2H(refs[i].linked_uid)+'] scheme ['+refs[i].schemename+']');
     end;
 end;

procedure TFRE_DB_MACHINE.SetMOSStatus(const status: TFRE_DB_MOS_STATUS_TYPE; const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION);
begin
  GFRE_MOS_SetMOSStatusandUpdate(self,status,input,ses,app,conn);
end;

function TFRE_DB_MACHINE.GetMOSStatus: TFRE_DB_MOS_STATUS_TYPE;
begin
  Result:=String2DBMOSStatus(Field('status_mos').AsString);
end;

 procedure TFRE_DB_MACHINE.CALC_GetDisplayAddress(const setter: IFRE_DB_CALCFIELD_SETTER);
 var s : String;
 begin
   s      := '';
   if FieldExists('address') then begin
     s := trim(Field('address').AsObject.Field('co').AsString);
     if length(s)>0 then begin
       s:= s + ', ';
     end;
     s := s+Field('address').AsObject.Field('zip').asstring+' '+Field('address').AsObject.Field('city').asstring+', '+Field('address').AsObject.Field('street').asstring+' '+Field('address').AsObject.Field('nr').asstring;
   end else begin
     s := '';
   end;
   setter.SetAsString(s);
 end;

 procedure TFRE_DB_MACHINE.CALC_GetDisplayName(const setter: IFRE_DB_CALCFIELD_SETTER);
 begin
   setter.SetAsString('Machine: '+Field('objname').AsString);
 end;

function TFRE_DB_MACHINE.WEB_MOSContent(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  panel         : TFRE_DB_FORM_PANEL_DESC;
  scheme        : IFRE_DB_SchemeObject;
begin
  GFRE_DBI.GetSystemSchemeByName(SchemeClass,scheme);
  panel :=TFRE_DB_FORM_PANEL_DESC.Create.Describe(GetTranslateableTextShort(conn,'machine_content_header'),true,false);
  panel.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  panel.FillWithObjectValues(self,GetSession(input));
  Result:=panel;
end;

function TFRE_DB_MACHINE.WEB_MOSChildStatusChanged(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  SetMOSStatus(GFRE_MOS_MOSChildStatusChanged(UID,input,ses,app,conn),input,ses,app,conn);
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_MACHINE.WEB_MOSStatus(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=GFRE_DBI.NewObject;
  Result.Field('status_mos').AsString:=Field('status_mos').AsString;
end;

function TFRE_DB_MACHINE.WEB_GetDefaultCollection(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  result:=GFRE_DBI.NewObject;
  result.Field('collection').asstring:=conn.GetMachinesCollection.CollectionName()
end;

function TFRE_DB_MACHINE.WEB_REQUEST_DISK_ENC_POOL_DATA(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var mo          : IFRE_DB_Object;
    refs        : TFRE_DB_ObjectReferences;
    pool        : TFRE_DB_ZFS_POOL;
    i           : NativeInt;
    obj         : IFRE_DB_Object;
    pools       : IFRE_DB_Object;
    disks       : IFRE_DB_Object;
    enclosures  : IFRE_DB_Object;
    disk        : TFRE_DB_OS_BLOCKDEVICE;
    enclosure   : TFRE_DB_ENCLOSURE;
    ua          : TFRE_DB_ZFS_UNASSIGNED;
    foundua     : boolean;
    ua_uid      : TFRE_DB_GUID;
    ua_name     : TFRE_DB_String;

begin
  result := GFRE_DBI.NewObject;

  mo     := CloneToNewObject;
  result.Field(field('objname').asstring).AsObject := mo;

  pools      := TFRE_DB_EMBEDDING_GROUP.CreateForDB;
  disks      := TFRE_DB_EMBEDDING_GROUP.CreateForDB;
  enclosures := TFRE_DB_EMBEDDING_GROUP.CreateForDB;

  mo.Field('POOLS').asObject := pools;
  mo.Field('DISKS').asObject := disks;
  mo.Field('ENCLOSURES').asObject := enclosures;

  foundua    := false;

  refs := conn.GetReferencesDetailed(UID,false);
  for i:=0 to high(refs) do
    begin
      CheckDbResult(conn.Fetch(refs[i].linked_uid,obj),' could not fetch referencing object '+FREDB_G2H(refs[i].linked_uid));
      if obj.IsA(TFRE_DB_ZFS_UNASSIGNED,ua) then
        begin
          foundua := true;
          pools.Field(ua.GetName).AsObject:=ua;
        end
      else if obj.IsA(TFRE_DB_ZFS_POOL,pool) then
        begin
          pools.Field(pool.GetName).AsObject:=TFRE_DB_ZFS_POOL.CreateEmbeddedPoolObjectfromDB(conn,refs[i].linked_uid,False);
          pool.Finalize;
        end
      else if obj.IsA(TFRE_DB_OS_BLOCKDEVICE,disk) then
        begin
          disk.embedIostat(conn);
          disks.Field(disk.DeviceIdentifier).AsObject := disk;
        end
      else if obj.IsA(TFRE_DB_ENCLOSURE,enclosure) then
        begin
          enclosure.embedSlotsandExpanders(conn);
          enclosures.Field(enclosure.DeviceIdentifier).AsObject := enclosure;
        end
      else
        obj.Finalize;
    end;

  if not foundua then
    begin
      ua := TFRE_DB_ZFS_UNASSIGNED.CreateForDB;
      ua.SetDomainID(DomainID);
      ua.InitforMachine(UID);
      ua_uid       := ua.UID;
      ua_name      := ua.GetName;
      CheckDbResult(conn.GetCollection(CFRE_DB_ZFS_POOL_COLLECTION).Store(ua),'could not store pool for unassigned disks');
      CheckDbResult(conn.Fetch(ua_uid,obj),' could not fetch unassigned disk object '+FREDB_G2H(ua_uid));
      pools.Field(ua_name).AsObject:=obj;
    end;

//  writeln('SWL REQUEST_DISC_ENC_POOL: ',result.DumpToString);

end;

function TFRE_DB_MACHINE.WEB_REQUEST_SERVICE_STRUCTURE(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var mo          : IFRE_DB_Object;

    procedure _getsubreferences(const obj:IFRE_DB_Object; const level:integer);
    var  refs        : TFRE_DB_ObjectReferences;
         sobj        : IFRE_DB_Object;
         clonedobj   : IFRE_DB_Object;
         svc         : TFRE_DB_SERVICE;
         i           : integer;
    begin
      refs := conn.GetReferencesDetailed(obj.uid,false);
      for i:=0 to high(refs) do
        begin
          if (refs[i].fieldname='SERVICEPARENT') or (refs[i].fieldname='ZONEID') then
            begin
              CheckDbResult(conn.Fetch(refs[i].linked_uid,sobj),' could not fetch referencing object '+refs[i].linked_uid.AsHexString);
//              writeln('SWL SET FIELD LEVEL',level,' ',obj.UID_String,' ',sobj.UID_String,' ',sobj.SchemeClass);
              clonedobj := sobj.CloneToNewObject;
              obj.Field(sobj.UID_String).AsObject:=clonedobj;
              if clonedobj.IsA(TFRE_DB_SERVICE,svc) then
                begin
                  svc.Embed(conn);
                end
              else
                _getsubreferences(clonedobj,level+1);
            end;
        end;
    end;

begin
  result := GFRE_DBI.NewObject;

  mo     := CloneToNewObject;
  result.Field(field('objname').asstring).AsObject := mo;

  _getsubreferences(mo,0);

 //writeln('SWL SERVICE STRUCTURE: ',result.DumpToString);
end;

 procedure TFRE_DB_SERVICE.ClearErrors;
 begin
   DeleteField('errors');
 end;

  function TFRE_DB_SERVICE.ExecuteCMD(const cmd: string; out outstring: string; const ignore_errors: boolean): integer;
 var errorobj    : IFRE_DB_Object;
     resultcode  : integer;
     errorstring : string;
 begin
   resultcode    := FRE_ProcessCMD(cmd,outstring,errorstring);
   if resultcode<>0 then
     begin
       if ignore_errors=false then
         begin
           errorobj  := GFRE_DBI.NewObject;
           errorobj.Field('cmd').asstring       := cmd;
           errorobj.Field('resultcode').AsInt32 := resultcode;
           errorobj.Field('error').asstring     := errorstring;
           errorobj.Field('output').asstring    := outstring;
           Field('errors').AddObject(errorobj);
         end;
     end;
 end;

 class procedure TFRE_DB_SERVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName  ('TFRE_DB_SERVICE_BASE');
 end;

  class procedure TFRE_DB_SERVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   if currentVersionId='' then begin
     currentVersionId := '1.0';
     StoreTranslateableText(conn,'scheme_main_group','General Information');
     StoreTranslateableText(conn,'scheme_objname','Servicename');
   end;
 end;

 class function TFRE_DB_SERVICE.GetMachineUIDForService(const conn: IFRE_DB_CONNECTION; service_uid: TFRE_DB_GUID): TFRE_DB_GUID;
 var parentObj : IFRE_DB_Object;
 begin
   CheckDbResult(conn.Fetch(service_uid,parentObj));
   while (parentObj.FieldExists('serviceParent') and not parentObj.IsA('TFRE_DB_MACHINE')) do
     begin
       service_uid := parentObj.Field('serviceParent').AsGUID;
       parentObj.Finalize;
       CheckDbResult(conn.Fetch(service_uid,parentObj));
     end;
   result := parentObj.UID;
   try
     if not parentObj.IsA('TFRE_DB_MACHINE') then
       raise EFRE_DB_Exception.Create(edb_ERROR,'No Machine found for service [%s]',[service_uid.AsHexString]);
   finally
     parentObj.Finalize;
   end;
 end;

 procedure TFRE_DB_SERVICE.Embed(const conn: IFRE_DB_CONNECTION);
 begin
   // no generic service embedding right now
 end;

  procedure TFRE_DB_SERVICE.SetSvcNameandType(const service_name: string; const common_name: string; const duration: string; const ignore_error: string; const enabled: boolean);
 begin
   Field('svc_name').asstring             :=service_name;
   Field('svc_duration').asstring         :=duration;
   Field('svc_ignore_error').asstring     :=ignore_error;
   Field('svc_common_name').asstring      :=common_name;
   Field('svc_enabled').asboolean          :=enabled;
 end;

  procedure TFRE_DB_SERVICE.SetSvcEnvironment(const working_directory: string; const user, group: string; const environment: string; const privileges: string);
 begin
   Field('svc_environment').asstring      := environment;
   Field('svc_working_directory').asstring:= working_directory;
   Field('svc_user').asstring             := user;
   Field('svc_group').asstring            := group;
   Field('svc_privileges').asstring       := privileges;
 end;

 procedure TFRE_DB_SERVICE.SetSvcStart(const execname: string; const timeout: Uint64);
 begin
   Field('svc_start_exec').asstring    := execname;
   Field('svc_start_timeout').AsUInt64 := timeout;
 end;

 procedure TFRE_DB_SERVICE.SetSvcStop(const execname: string; const timeout: Uint64);
 begin
   Field('svc_stop_exec').asstring    := execname;
   Field('svc_stop_timeout').AsUInt64 := timeout;
 end;

 procedure TFRE_DB_SERVICE.SetSvcRestart(const execname: string; const timeout: Uint64);
 begin
   Field('svc_restart_exec').asstring    := execname;
   Field('svc_restart_timeout').AsUInt64 := timeout;
 end;

procedure TFRE_DB_SERVICE.AddSvcDependency(const name: string;const fmri: string; const grouping: string; const restart_on: string);
var
  deb           : IFRE_DB_Object;
begin
  deb := GFRE_DBI.NewObject;
  deb.Field('fmri').asstring      := fmri;
  deb.Field('grouping').asstring  := grouping;
  deb.Field('restart_on').asstring:= restart_on;
  deb.Field('name').asstring      := name;
  Field('svc_dependency').addObject(deb);
end;

procedure TFRE_DB_SERVICE.AddSvcDependent(const name: string;const fmri: string; const grouping: string; const restart_on: string);
var
  deb           : IFRE_DB_Object;
begin
  deb := GFRE_DBI.NewObject;
  deb.Field('fmri').asstring      := fmri;
  deb.Field('grouping').asstring  := grouping;
  deb.Field('restart_on').asstring:= restart_on;
  deb.Field('name').asstring      := name;
  Field('svc_dependent').addObject(deb);
end;

function TFRE_DB_SERVICE.GetFMRI: TFRE_DB_STRING;
begin
  result := 'svc:/fos/fos_'+ClassName;
end;

 function TFRE_DB_SERVICE.RIF_CreateOrUpdateService: IFRE_DB_Object;
 var servicename :string;
 begin
  {$IFDEF SOLARIS}
    result := GFRE_DBI.NewObject;
    writeln('SWL: SERVICE CREATE',ClassName);

    servicename := Copy(GetFMRI,6,maxint);
    writeln('SWL: CREATE SVC GENERIC ',ObjectName,' ',servicename);
    SetSvcNameandType(servicename,'FirmOS GENERIC Service ','transient','core,signal',true);
    SetSvcEnvironment('/opt/local/fre','root','root','LANG=C');
    SetSvcStart('/opt/local/fre/bin/fossvc --enable  --generic='+UID.AsHexString,60);
    SetSvcStop('/opt/local/fre/bin/fossvc --disable --generic='+UID.AsHexString,60);
    AddSvcDependency('network','svc:/milestone/network','require_all','none');

    fre_create_service(self);
    result.Field('fmri').asstring:=servicename;

  {$ENDIF}
 end;

 function TFRE_DB_SERVICE.RIF_EnableService(const runnning_ctx: TObject): IFRE_DB_Object;
 var servicename:string;
 begin
   writeln('RIF ENABLE SERVICE ',Field('zone').asstring,' ',GetFMRI);
   {$IFDEF SOLARIS}
   servicename := Copy(GetFMRI,6,maxint);
   fre_enable_or_disable_service(servicename,true,Field('zone').asstring);
   result := GFRE_DBI.NewObject;
   {$ENDIF}
 end;

 function TFRE_DB_SERVICE.RIF_DisableService(const runnning_ctx: TObject): IFRE_DB_Object;
 var servicename:string;
 begin
   writeln('RIF DISABLE SERVICE ',Field('zone').asstring,' ',GetFMRI);
   {$IFDEF SOLARIS}
   servicename := Copy(GetFMRI,6,maxint);
   fre_enable_or_disable_service(servicename,false,Field('zone').asstring);
   result := GFRE_DBI.NewObject;
   {$ENDIF}
 end;

 { TFRE_DB_VIRTUAL_FILESERVER }

class procedure TFRE_DB_VIRTUAL_FILESERVER.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_FILESERVER.Classname);
  scheme.AddSchemeField('customer',fdbft_ObjLink).SetupFieldDef(true);

  group:=scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('vfs_scheme_main_group'));
  group.AddInput('objname',GetTranslateableTextKey('scheme_fileservername'),false);
  group.AddInput('desc.txt',GetTranslateableTextKey('scheme_description'));
end;

class procedure TFRE_DB_VIRTUAL_FILESERVER.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.1';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
    StoreTranslateableText(conn,'vfs_scheme_main_group','Virtual Fileserver Properties');
    StoreTranslateableText(conn,'scheme_fileservername','Servername');
    //StoreTranslateableText(conn,'scheme_pool','Diskpool');
    StoreTranslateableText(conn,'scheme_description','Description');
    //StoreTranslateableText(conn,'scheme_ip','IP');
    //StoreTranslateableText(conn,'scheme_interface','Interface');
    //StoreTranslateableText(conn,'scheme_vlan','Vlan');
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';
    StoreTranslateableText(conn,'caption','Virtual Fileserver');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Name');
  end;
end;

function TFRE_DB_VIRTUAL_FILESERVER.GetFMRI: TFRE_DB_STRING;
begin
  result := 'svc:/fos/fos_samba';
end;

function TFRE_DB_VIRTUAL_FILESERVER.RIF_CreateOrUpdateService: IFRE_DB_Object;
var servicename : string;
begin
  {$IFDEF SOLARIS}
    result := GFRE_DBI.NewObject;
    servicename := Copy(GetFMRI,6,maxint);
    SetSvcNameandType(servicename,'FirmOS Samba Service ','transient','core,signal',true);
    SetSvcEnvironment('/','root','root','LANG=C');
    SetSvcStart('/opt/local/samba4/sbin/samba --daemon',180);
    SetSvcStop(':kill',60);
    AddSvcDependency('network','svc:/milestone/network:default','require_all','none');
    AddSvcDependency('filesystem-local','svc:/system/filesystem/local','require_all','error');
//    AddSvcDependency('ldap','svc:/fos/fos_ldap','require_all','none');

    fre_create_service(self);
    result.Field('fmri').asstring:=servicename;

  {$ENDIF}
end;

class function TFRE_DB_VIRTUAL_FILESERVER.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;


{ TFRE_DB_GLOBAL_FILESERVER }

class procedure TFRE_DB_GLOBAL_FILESERVER.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_FILESERVER.Classname);
end;

class procedure TFRE_DB_GLOBAL_FILESERVER.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.1';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';
    StoreTranslateableText(conn,'caption','Global Fileserver');

    StoreTranslateableText(conn,'scheme_main_group','General Information');
    StoreTranslateableText(conn,'scheme_objname','Name');
  end;
end;

class function TFRE_DB_GLOBAL_FILESERVER.GetCaption(const conn: IFRE_DB_CONNECTION): String;
begin
  Result:=GetTranslateableTextShort(conn,'caption');
end;

{ TFRE_DB_FS_ENTRY }

class procedure TFRE_DB_FS_ENTRY.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId := '1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;

class procedure TFRE_DB_FS_ENTRY.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

procedure TFRE_DB_FS_ENTRY.InternalSetup;
begin
  inherited InternalSetup;
  Field('isfile').AsBoolean:=true;
  Field('mypath').AsString :='/';
end;

procedure TFRE_DB_FS_ENTRY.SetIsFile(const isfile: boolean);
begin
  Field('isfile').AsBoolean := isfile;
  if isfile then begin
    Field('children').AsString:='';
  end else begin
    Field('children').AsString:='UNCHECKED';
  end;
end;

function TFRE_DB_FS_ENTRY.GetIsFile: Boolean;
begin
  result := Field('isfile').AsBoolean;
end;

procedure TFRE_DB_FS_ENTRY.SetProperties(const name: TFRE_DB_String; const is_file: boolean; const size: NativeInt; const mode: Cardinal; const time: Longint);
var
    y, mon, d, h, min, s: word;
    fosdt : TFRE_DB_DateTime64;
    icon  : String;
    hrtype: String;

   procedure mimeTypeToIconAndHRType(const mt: String; var icon: String; var hrtype: String);
   var
     mtp: TFRE_DB_StringArray;
   begin
     GFRE_BT.SeperateString(LowerCase(mt),'/',mtp);
     icon:='images_apps/test/file.png';
     hrtype:='Unknown';
     case mtp[0] of
       'audio': begin
                  icon:='images_apps/test/audio-basic.png';
                  hrtype:='Audio';
                end;
       'video': begin
                  icon:='images_apps/test/video-x-generic-mplayer.png';
                  hrtype:='Video';
                end;
       'image': begin
                  hrtype:='Image';
                  case mtp[1] of
                    'bmp': icon:='images_apps/test/image-bmp.png';
                    'jpeg': icon:='images_apps/test/image-jpeg.png';
                    'tiff': icon:='images_apps/test/image-tiff.png';
                    'gif': icon:='images_apps/test/image-gif.png';
                    'png': icon:='images_apps/test/image-png.png';
                  end;
                end;
       'application': begin
                        hrtype:='Application file';
                        case mtp[1] of
                          'zip': icon:='images_apps/test/application-zip.png';
                          'pdf': icon:='images_apps/test/application-pdf.png';
                          'msword': icon:='images_apps/test/page-word.png';
                          'postscript': icon:='images_apps/test/application-postscript-2.png';
                          'rtf': icon:='images_apps/test/application-rtf.png';
                          'wordperfect5.1': icon:='images_apps/test/application-vnd.wordperfect-abiword.png';
                          'octet-stream': hrtype:='Unknown';
                        end;
                      end;
     end;
   end;

begin
  EpochToLocal(time,y,mon,d,h,min,s);
  Field('date').AsDateTime := GFRE_DT.EncodeTime(y,mon,d,h,min,s,0);
  Field('name').AsString   := name;
  Field('size').AsUInt64   := size;
  if is_file then begin
    Field('sizeHR').AsString := GFRE_BT.ByteToString(size);
    mimeTypeToIconAndHRType(FREDB_Filename2MimeType(name),icon,hrtype);
    Field('typeHR').AsString   := hrtype;
    Field('icon').AsString:=FREDB_getThemedResource(icon);
  end else begin
    Field('typeHR').AsString   := 'Folder';
    Field('sizeHR').AsString := '';
    Field('icon').AsString:=FREDB_getThemedResource('images_apps/test/folder.png');
    Field('icon_open').AsString:=FREDB_getThemedResource('images_apps/test/folder-open.png');
  end;
  Field('mode').AsUInt32   := mode;
  SetIsFile(is_file);
end;

function TFRE_DB_FS_ENTRY.FileDirName: String;
begin
  result := Field('name').AsString;
end;

{ TFRE_DB_FILESERVER }

class procedure TFRE_DB_FILESERVER.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName(TFRE_DB_SERVICE.Classname);
end;

class procedure TFRE_DB_FILESERVER.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';
  end;
end;


procedure Register_DB_Extensions;
begin
   fre_monitoring.Register_DB_Extensions;

   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MACHINE_SETTING);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MACHINE_SETTING_POWER);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MACHINE_SETTING_MAIL);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MACHINE_SETTING_TIME);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MACHINE_SETTING_HOSTNAME);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_FC_PORT);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SERVICE_BASE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SERVICE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SUBSERVICE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATALINK);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATALINK_PHYS);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATALINK_VNIC);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATALINK_IPTUN);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATALINK_AGGR);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATALINK_STUB);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATALINK_IPMP);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATALINK_BRIDGE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATALINK_SIMNET);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SERVICE_DOMAIN);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SERVICE_INSTANCE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATACENTER);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_FBZ_TEMPLATE);

   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ASSET);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Machine);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_VHOST);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_VROOTSERVER);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_IMAGE_FILE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_VMACHINE_DISK);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_VMACHINE_NIC);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_VMACHINE);

   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DNS);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_NAS);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZONE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_GLOBAL_ZONE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Tester);

   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DEVICE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_NETWORK_GROUP);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CMS);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Captiveportal);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Site_Captive_Extension);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Endpoint);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Accesspoint);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_AP_Linksys);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_AP_Linksys_E1000);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_AP_Linksys_E1200);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_AP_Linksys_E1200V2);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_AP_Lancom);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_AP_Lancom_IAP321);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_AP_Lancom_OAP321);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MobileDevice);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Network);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_WifiNetwork);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_OpenWifiNetwork);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_WPA2Network);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_RadiusNetwork);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Monitoring_Status);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CA);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CERTIFICATE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DHCP);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DHCP_Subnet);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DHCP_Fixed);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_VPN);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_RADIUS);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Routing);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CMS_PAGE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CMS_ADPAGE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_REDIRECTION_FLOW);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_HALCONFIG);
   GFRE_DBI.RegisterObjectClassEx(TFRE_ZIP_STATUS);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_FILESERVER);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_GLOBAL_FILESERVER);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_VIRTUAL_FILESERVER);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CRYPTO_FILESERVER);

   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SSH_SERVICE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_IMAP_SERVICE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MTA_SERVICE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_POSTGRES_SERVICE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MYSQL_SERVICE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_HTTP_SERVICE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_LDAP_SERVICE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_PHPFPM_SERVICE);

   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CRYPTOCPE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CPE_NETWORK_SERVICE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CPE_OPENVPN_SERVICE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CPE_DHCP_SERVICE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CPE_VIRTUAL_FILESERVER);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_IP_HOSTNET);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_IPV4_HOSTNET);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_IPV6_HOSTNET);

   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_FS_ENTRY);

   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZONECREATION_JOB);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZONEDESTROY_JOB);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZONESTATUS_PLUGIN);

   //GFRE_DBI.Initialize_Extension_Objects;
 end;

procedure CreateServicesCollections(const conn: IFRE_DB_COnnection);
var
  collection: IFRE_DB_COLLECTION;
  ix_def    : TFRE_DB_INDEX_DEF;
begin

  if not conn.CollectionExists(CFRE_DB_DATACENTER_COLLECTION) then
    begin
     collection:=conn.CreateCollection(CFRE_DB_DATACENTER_COLLECTION);
     collection.DefineIndexOnField('objname',fdbft_String,true);
    end;

  if not conn.CollectionExists(CFRE_DB_TEMPLATE_COLLECTION) then
    begin
     collection:=conn.CreateCollection(CFRE_DB_TEMPLATE_COLLECTION);
     collection.DefineIndexOnField('objname',fdbft_String,true);
    end;

  if not conn.CollectionExists(CFOS_DB_SERVICES_COLLECTION) then begin
    collection  := conn.CreateCollection(CFOS_DB_SERVICES_COLLECTION);
    collection.DefineIndexOnField('uniquephysicalid',fdbft_String,true,true,'def',false);
  end else
  begin
    collection  := conn.GetCollection(CFOS_DB_SERVICES_COLLECTION);
    ix_def      := collection.GetIndexDefinition('def');
    if lowercase(ix_def.FieldName)='objname' then
      begin
        collection.DropIndex('def');
        CheckDbResult(collection.DefineIndexOnField('uniquephysicalid',fdbft_String,true,true,'def',false));
      end;
  end;

  if not conn.CollectionExists(CFRE_DB_IP_COLLECTION) then begin
    collection  := conn.CreateCollection(CFRE_DB_IP_COLLECTION);
  end else begin
    collection  := conn.GetCollection(CFRE_DB_IP_COLLECTION);
  end;
  if not collection.IndexExists('def') then begin
    collection.DefineIndexOnField('uniquephysicalid',fdbft_String,true,true,'def',false);
  end;

  if not conn.CollectionExists(CFOS_DB_ZONES_COLLECTION) then begin
    collection  := conn.CreateCollection(CFOS_DB_ZONES_COLLECTION);
  end else begin
    collection  := conn.GetCollection(CFOS_DB_ZONES_COLLECTION);
  end;
  if not collection.IndexExists('upid') then begin
    collection.DefineIndexOnField('uniquephysicalid',fdbft_String,true,true,'upid',false);
  end;

  if not conn.CollectionExists(CFRE_DB_VM_COMPONENTS_COLLECTION) then begin
    collection  := conn.CreateCollection(CFRE_DB_VM_COMPONENTS_COLLECTION);
    collection.DefineIndexOnField('uniquephysicalid',fdbft_String,true,true,'def',false);
  end;

  if not conn.CollectionExists(CFRE_DB_IMAGEFILE_COLLECTION) then begin
    collection  := conn.CreateCollection(CFRE_DB_IMAGEFILE_COLLECTION);
  end;

end;




end.

