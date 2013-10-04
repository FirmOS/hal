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
  Process,

  FRE_HAL_UTILS,
  FRE_DB_COMMON,
  FRE_DBBUSINESS,
  FRE_DB_INTERFACE,
  FRE_HAL_TRANSPORT,
  FRE_DB_SYSRIGHT_CONSTANTS,
  fre_system,
  fre_testcase,
  fre_alert,
  fre_zfs;

type

   { TFRE_DB_ServiceGroup }

   TFRE_DB_ServiceGroup=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   end;

   TFRE_DB_Service=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_MACHINE }

   TFRE_DB_MACHINE=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   published
     procedure       CALC_GetDisplayAddress  (const setter:IFRE_DB_CALCFIELD_SETTER);
     procedure       CALC_GetDisplayName     (const setter:IFRE_DB_CALCFIELD_SETTER);
   end;

   { TFRE_DB_MACHINE_SETTING }

   TFRE_DB_MACHINE_SETTING=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_MACHINE_SETTING_POWER }

   TFRE_DB_MACHINE_SETTING_POWER=class(TFRE_DB_MACHINE_SETTING)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   published
     function        IMI_Shutdown           (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        IMI_Reboot             (const input:IFRE_DB_Object): IFRE_DB_Object;
   end;

   { TFRE_DB_MACHINE_SETTING_HOSTNAME }

   TFRE_DB_MACHINE_SETTING_HOSTNAME=class(TFRE_DB_MACHINE_SETTING)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_MACHINE_SETTING_MAIL }

   TFRE_DB_MACHINE_SETTING_MAIL=class(TFRE_DB_MACHINE_SETTING)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_MACHINE_SETTING_TIME }

   TFRE_DB_MACHINE_SETTING_TIME=class(TFRE_DB_MACHINE_SETTING)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_FC_PORT }

   TFRE_DB_FC_PORT=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_DATALINK }

   TFRE_DB_DATALINK=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   published
     function        IMI_Menu               (const input:IFRE_DB_Object): IFRE_DB_Object;
   end;

   { TFRE_DB_DATALINK_PHYS }

   TFRE_DB_DATALINK_PHYS=class(TFRE_DB_DATALINK)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   published
     function        IMI_Menu               (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        IMI_AddVNIC            (const input:IFRE_DB_Object): IFRE_DB_Object;
   end;

   { TFRE_DB_DATALINK_VNIC }

   TFRE_DB_DATALINK_VNIC=class(TFRE_DB_DATALINK)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   published
     function        IMI_Menu               (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        IMI_Delete             (const input:IFRE_DB_Object): IFRE_DB_Object;
   end;

   { TFRE_DB_DATALINK_STUB }

   TFRE_DB_DATALINK_STUB=class(TFRE_DB_DATALINK)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   published
     function        IMI_Menu               (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        IMI_AddVNIC            (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        IMI_Delete             (const input:IFRE_DB_Object): IFRE_DB_Object;
   end;

   { TFRE_DB_DATALINK_AGGR }

   TFRE_DB_DATALINK_AGGR=class(TFRE_DB_DATALINK)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   published
     function        IMI_Menu               (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        IMI_AddVNIC            (const input:IFRE_DB_Object): IFRE_DB_Object;
     function        IMI_Delete             (const input:IFRE_DB_Object): IFRE_DB_Object;
   end;

   { TFRE_DB_TESTER }

   TFRE_DB_TESTER=class(TFRE_DB_MACHINE)
   protected
     class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   end;

   { TFRE_DB_VMACHINE }

   TFRE_DB_VMACHINE=class(TFRE_DB_MACHINE)
   protected
     class procedure RegisterSystemScheme   (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   published
     procedure CALC_GetDisplayName          (const setter: IFRE_DB_CALCFIELD_SETTER);
   end;

   { TFRE_DB_ZONE }

   TFRE_DB_ZONE=class(TFRE_DB_MACHINE)
   protected
     class procedure RegisterSystemScheme  (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects      (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   published
     procedure CALC_GetDisplayName         (const setter: IFRE_DB_CALCFIELD_SETTER);
   end;

  { TFRE_DB_DEVICE }

  TFRE_DB_DEVICE=class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_NETWORK_GROUP }

  TFRE_DB_NETWORK_GROUP=class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_CMS }

  TFRE_DB_CMS=class(TFRE_DB_SERVICE)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_REDIRECTION_FLOW }

  TFRE_DB_REDIRECTION_FLOW=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
     class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   end;

  { TFRE_DB_ROUTE }

  TFRE_DB_ROUTE=class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_Site_Captive_Extension }

  TFRE_DB_Site_Captive_Extension = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_Endpoint }

  TFRE_DB_Endpoint = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
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
    function  HasAnotherAP        (const site_id:TGUID ; const conn : IFRE_DB_CONNECTION)  : boolean;
  protected
    class procedure RegisterSystemScheme  (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects      (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
    class procedure AccessPointOnChange   (const conn: IFRE_DB_CONNECTION; const is_dhcp:boolean ; const dhcp_id : TGUID; const mac : TFRE_DB_String); virtual;
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
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Configuration           (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_AP_Linksys_E1000 }

  TFRE_DB_AP_Linksys_E1000 = class(TFRE_DB_AP_Linksys)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    procedure CALC_GetDisplayName        (const Setter : IFRE_DB_CALCFIELD_SETTER);
  end;

  { TFRE_DB_AP_Linksys_E1200 }

  TFRE_DB_AP_Linksys_E1200 = class(TFRE_DB_AP_Linksys)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    procedure CALC_GetDisplayName        (const Setter : IFRE_DB_CALCFIELD_SETTER);
  end;

  { TFRE_DB_AP_Linksys_E1200V2 }

  TFRE_DB_AP_Linksys_E1200V2 = class(TFRE_DB_AP_Linksys_E1200)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
     procedure CALC_GetDisplayName       (const Setter : IFRE_DB_CALCFIELD_SETTER);
  end;

  { TFRE_DB_AP_Lancom }

  TFRE_DB_AP_Lancom = class(TFRE_DB_Accesspoint)
  protected
   class procedure InstallDBObjects      (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
   class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
  end;

  { TFRE_DB_AP_Lancom_IAP321 }

  TFRE_DB_AP_Lancom_IAP321 = class(TFRE_DB_AP_Lancom)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    procedure CALC_GetDisplayName        (const Setter : IFRE_DB_CALCFIELD_SETTER);
  end;

  { TFRE_DB_AP_Lancom_OAP321 }

  TFRE_DB_AP_Lancom_OAP321 = class(TFRE_DB_AP_Lancom)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    procedure CALC_GetDisplayName        (const Setter : IFRE_DB_CALCFIELD_SETTER);
  end;

  { TFRE_DB_Monitoring_Status }

  TFRE_DB_Monitoring_Status = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    procedure CALC_GetStatusIcon         (const setter: IFRE_DB_CALCFIELD_SETTER);
  end;

  { TFRE_DB_CMS_PAGE }

  TFRE_DB_CMS_PAGE = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_Menu                    (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_CMS_ADPAGE }

  TFRE_DB_CMS_ADPAGE = class(TFRE_DB_CMS_PAGE)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
  end;

  { TFRE_DB_MobileDevice }

  TFRE_DB_MobileDevice = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_Menu                    (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_unassign                (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_Network }

  TFRE_DB_Network = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
    class procedure NetworkOnChange      (const dbc : IFRE_DB_Connection; const is_dhcp:boolean; const subnet : string; const ep_id: TGUID; const dns:string; const range_start, range_end : integer ); virtual;
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
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_OpenWifiNetwork }

  TFRE_DB_OpenWifiNetwork = class(TFRE_DB_WifiNetwork)
  protected
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
  end;

  { TFRE_DB_WPA2Network }

  TFRE_DB_WPA2Network = class(TFRE_DB_WifiNetwork)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_RadiusNetwork }

  TFRE_DB_RadiusNetwork = class(TFRE_DB_WifiNetwork)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_CA }

  TFRE_DB_CA = class(TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Menu                    (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_Content                 (const input:IFRE_DB_Object):IFRE_DB_Object;
    function WEB_ChildrenData            (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
    function IMI_AddCertificate          (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_Certificate }

  TFRE_DB_Certificate = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Menu                    (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_Content                 (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_DHCP }

  TFRE_DB_DHCP = class(TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object) : IFRE_DB_Object;
    function IMI_Menu                    (const input:IFRE_DB_Object) : IFRE_DB_Object;
    function WEB_ChildrenData            (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
    function IMI_addSubnet               (const input:IFRE_DB_Object) : IFRE_DB_Object;
    function IMI_addFixedHost            (const input:IFRE_DB_Object) : IFRE_DB_Object;
  end;

  { TFRE_DB_DHCP_Subnet }

  TFRE_DB_DHCP_Subnet = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object) : IFRE_DB_Object;
    function IMI_Menu                    (const input:IFRE_DB_Object) : IFRE_DB_Object;
  end;

  { TFRE_DB_DHCP_Fixed }

  TFRE_DB_DHCP_Fixed = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object) : IFRE_DB_Object;
    function IMI_Menu                    (const input:IFRE_DB_Object) : IFRE_DB_Object;
  end;

  { TFRE_DB_VPN }

  TFRE_DB_VPN = class(TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
  end;

  { TFRE_DB_Radius }

  TFRE_DB_Radius = class(TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
  end;

  { TFRE_DB_Captiveportal }

  TFRE_DB_Captiveportal = class(TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Menu                    (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_Content                 (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_Routing }

  TFRE_DB_Routing = class(TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType); override;
  published
  end;

procedure Register_DB_Extensions;

implementation



 procedure SetReprovision (const dbc: IFRE_DB_Connection; const id:TGUID);
 var
     obj       :    IFRE_DB_Object;
 begin
   writeln  ('set reprovision :'+GUIDToString(id));
   if  not  dbc.Fetch(id,obj)  then raise EFRE_DB_Exception.Create(edb_ERROR,'NO OBJ FOUND FOR REPROVISION '+GUIDToString(id));
   obj.Field('reprovision').asboolean:=true;
   writeln  (obj.DumpToString());
   CheckDbResult(dbc.Update(obj),'failure on cloned/update');
 end;

 function GetService     (const dbc: IFRE_DB_Connection; const serviceclass:string): TGUID;
 var
     coll    : IFRE_DB_COLLECTION;
     id      : TGUID;

     function _get(const obj:IFRE_DB_Object):boolean;
     begin
       result := false;
       writeln('SERVICE '+obj.UID_String);
       if obj.IsA(serviceclass) then begin
         writeln('FOUND '+serviceclass+' '+obj.UID_String);
         id:=obj.uid;
         Result := true;
       end;
     end;


 begin
   writeln('GET SERVICE');
   coll   := dbc.Collection('service');
   coll.ForAllBreak(@_get);
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


 function GetNextNet (const dbc: IFRE_DB_CONNECTION; const ep_id:TGUID) : string;
 var
     ep_obj     : IFRE_DB_Object;
     cap_id     : TGUID;
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
   if not dbc.Fetch(ep_id,ep_obj) then raise EFRE_DB_Exception.Create(edb_ERROR,'NO EP FOUND IN GET NEXT NET '+GUIDToString(ep_id));
   cap_id:=GetService(dbc,'TFRE_DB_CAPTIVEPORTAL');
   if not dbc.Fetch(cap_id,cap_obj) then raise EFRE_DB_Exception.Create(edb_ERROR,'NO CAPSERVICE FOUND IN GET NEXT NET '+GUIDToString(cap_id));

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

  colln   :=dbc.Collection('network');
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

 function UniqueNet (const dbc: IFRE_DB_CONNECTION; const network_id:TGUID; const new_net: string) : boolean;
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

   colln   := dbc.Collection('network');
   colln.ForAll(@_checknets);

   result   := gresult;
  end;


{ TFRE_DB_OpenWifiNetwork }

class procedure TFRE_DB_OpenWifiNetwork.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
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
  scheme.AddSchemeField         ('status',fdbft_String).SetupFieldDef(true,false,'signal_status');
  scheme.AddSchemeField         ('provisioned_time',fdbft_DateTimeUTC);
  scheme.AddSchemeField         ('online_time',fdbft_DateTimeUTC);
  scheme.AddCalcSchemeField     ('status_icon',fdbft_String,@CALC_GetStatusIcon);
end;

class procedure TFRE_DB_Monitoring_Status.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
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

class procedure TFRE_DB_AP_Lancom_OAP321.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
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

class procedure TFRE_DB_AP_Lancom_IAP321.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
end;

procedure TFRE_DB_AP_Lancom_IAP321.CALC_GetDisplayName(const Setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString('Accesspoint Lancom IAP321 ('+Field('provisioningmac').AsString+')');
end;

{ TFRE_DB_AP_Lancom }

class procedure TFRE_DB_AP_Lancom.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
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

class procedure TFRE_DB_AP_Linksys_E1200V2.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
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

class procedure TFRE_DB_AP_Linksys_E1200.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
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

class procedure TFRE_DB_AP_Linksys_E1000.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
end;

procedure TFRE_DB_AP_Linksys_E1000.CALC_GetDisplayName(const Setter: IFRE_DB_CALCFIELD_SETTER);
begin
  setter.SetAsString('Accesspoint Linksys E1000 ('+Field('reprovision').AsString+') ('+Field('provisioningmac').AsString+')');
end;

{ TFRE_DB_AP_Linksys }


class procedure TFRE_DB_AP_Linksys.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_ACCESSPOINT');

  scheme.AddSchemeField('routing',fdbft_String).SetupFieldDef(true,false,'routing');
  scheme.AddSchemeField('vpn_crtid',fdbft_ObjLink);

  group:=scheme.AddInputGroup('options').Setup(GetTranslateableTextKey('scheme_options_group'));
  group.AddInput('routing',GetTranslateableTextKey('scheme_routing'));
  group.AddInput('vpn_crtid',GetTranslateableTextKey('scheme_vpn_cert'),false,false,'certificate');

end;

class procedure TFRE_DB_AP_Linksys.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_options_group','Device Options');
  StoreTranslateableText(conn,'scheme_routing','Routing');
  StoreTranslateableText(conn,'scheme_vpn_cert','VPN Certificate');
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

class procedure TFRE_DB_Accesspoint.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','Accesspoint Configuration');
  StoreTranslateableText(conn,'scheme_serial','Serialnumber');
  StoreTranslateableText(conn,'scheme_exip','IP');
  StoreTranslateableText(conn,'scheme_dhcp','DHCP');
  StoreTranslateableText(conn,'scheme_channel','Channel');
  StoreTranslateableText(conn,'scheme_pw','Password');
end;



class function TFRE_DB_Accesspoint.WBC_NewOperation(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var dbc        :   IFRE_DB_CONNECTION;
    raw_object :   IFRE_DB_Object;
    dhcp       :   boolean;
    mac        :   string;
    dhcp_id    :   TGUID;


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


function TFRE_DB_Accesspoint.HasAnotherAP(const site_id: TGUID; const conn: IFRE_DB_CONNECTION): boolean;
var site_object       : IFRE_DB_Object;
    site_fnd          : boolean;
    childs            : TFRE_DB_GUIDArray;
    i                 : integer;
    has_open          : boolean;
    has_wpa2          : boolean;
    ep_obj            : IFRE_DB_Object;
begin
  result:=false;
  site_fnd          :=conn.Fetch(site_id,site_object);
  if site_fnd        then begin
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


class procedure TFRE_DB_Accesspoint.AccessPointOnChange(const conn: IFRE_DB_CONNECTION; const is_dhcp: boolean; const dhcp_id: TGUID; const mac: TFRE_DB_String);
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
    if       not conn.Fetch(dhcp_id,dhcp_obj)          then raise EFRE_DB_Exception.Create(edb_ERROR,'NO DHCP SERVICE FOUND IN AP AFTER SAVE');
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
    collf          := conn .Collection('dhcp_fixed');
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
    update_object_uid : TGUid;
    raw_object        : IFRE_DB_Object;
    dhcp              : boolean;
    dhcp_id           : TGUID;
    mac               : TFRE_DB_String;


begin
  if assigned(Parent) then begin
    result := TFRE_DB_MESSAGE_DESC(result).Describe('SAVE','Error on saving! Saving of Subobject not supported!',fdbmt_error);
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

class procedure TFRE_DB_Site_Captive_Extension.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
end;

{ TFRE_DB_Captiveportal }

class procedure TFRE_DB_Captiveportal.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_SERVICE');
  scheme.AddSchemeField('vpn_caid',fdbft_ObjLink);
end;

class procedure TFRE_DB_Captiveportal.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
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

class procedure TFRE_DB_REDIRECTION_FLOW.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
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

class procedure TFRE_DB_NETWORK_GROUP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','Network Group');
  StoreTranslateableText(conn,'scheme_name','Name');
  StoreTranslateableText(conn,'scheme_networks','Networks');
end;

class procedure TFRE_DB_ROUTE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('subnet',fdbft_String).required:=true;
  scheme.AddSchemeField('gateway',fdbft_String).required:=true;
  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('subnet',GetTranslateableTextKey('scheme_subnet'));
  group.AddInput('gateway',GetTranslateableTextKey('scheme_gateway'));
end;

class procedure TFRE_DB_ROUTE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','General Information');
  StoreTranslateableText(conn,'scheme_subnet','Subnet');
  StoreTranslateableText(conn,'scheme_gateway','Gateway');
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

class procedure TFRE_DB_CMS.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','CMS');
  StoreTranslateableText(conn,'scheme_baseurl','Base URL');
  StoreTranslateableText(conn,'scheme_execeptions','Url Exceptions');
end;

class procedure TFRE_DB_DEVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_OBJECTEX');
  scheme.AddSchemeField('provisioningmac',fdbft_String).SetupFieldDef(true,false,'','mac');
  scheme.AddSchemeField('provisioning_serial',fdbft_Int32);
  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('provisioningmac',GetTranslateableTextKey('scheme_pmac'));
end;

class procedure TFRE_DB_DEVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','General Information');
  StoreTranslateableText(conn,'scheme_pmac','Mac');
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

class procedure TFRE_DB_CMS_ADPAGE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_starttime','Start Time');
  StoreTranslateableText(conn,'scheme_endtime','End Time');
  StoreTranslateableText(conn,'scheme_startdaily','Start Daily');
  StoreTranslateableText(conn,'scheme_enddaily','End Daily');
  StoreTranslateableText(conn,'scheme_insertpoint','Insertion Point');
  StoreTranslateableText(conn,'scheme_max_inserts','Maximum Insertion Count');
  StoreTranslateableText(conn,'scheme_show_inserts','Already Shown Inserts');
  StoreTranslateableText(conn,'scheme_networkgroups','Network Groups');
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

class procedure TFRE_DB_WPA2Network.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','WPA2 Wifi Network');
  StoreTranslateableText(conn,'scheme_wpa2psk','WPA2PSK');
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

class procedure TFRE_DB_CMS_PAGE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','Page');
  StoreTranslateableText(conn,'scheme_active','Active');
  StoreTranslateableText(conn,'scheme_relurl','Relative Url');
  StoreTranslateableText(conn,'scheme_url','Url');
  StoreTranslateableText(conn,'scheme_exceptions','Url Exceptions');
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
  scheme.AddSchemeFieldSubscheme('static','TFRE_DB_ROUTE').multiValues:=true;

  group:=scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.UseInputGroup('TFRE_DB_SERVICE','main');
  group.AddInput('default',GetTranslateableTextKey('scheme_default'));
end;

class procedure TFRE_DB_Routing.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','Routing');
  StoreTranslateableText(conn,'scheme_default','Default Routing');
end;

{ TFRE_DB_Radius }

class procedure TFRE_DB_Radius.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_SERVICE');
end;

class procedure TFRE_DB_Radius.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
end;

{ TFRE_DB_VPN }

class procedure TFRE_DB_VPN.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.SetParentSchemeByName('TFRE_DB_SERVICE');
end;

class procedure TFRE_DB_VPN.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
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

class procedure TFRE_DB_DHCP_Fixed.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','DHCP Fixed');
  StoreTranslateableText(conn,'scheme_objname','Name');
  StoreTranslateableText(conn,'scheme_mac','Mac');
  StoreTranslateableText(conn,'scheme_ip','Ip');
  StoreTranslateableText(conn,'scheme_router','Router');
  StoreTranslateableText(conn,'scheme_dns','DNS');
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

class procedure TFRE_DB_DHCP_Subnet.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','DHCP Subnet');
  StoreTranslateableText(conn,'scheme_subnet','Subnet');
  StoreTranslateableText(conn,'scheme_range_start','Range Start');
  StoreTranslateableText(conn,'scheme_range_end','Range End');
  StoreTranslateableText(conn,'scheme_router','Router');
  StoreTranslateableText(conn,'scheme_dns','DNS');
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

class procedure TFRE_DB_DHCP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','General Information');
  StoreTranslateableText(conn,'scheme_default_domainname','Default Domainname');
  StoreTranslateableText(conn,'scheme_default_dns','Default DNS');
  StoreTranslateableText(conn,'scheme_default_leasetime','Default Leasetime');
  StoreTranslateableText(conn,'scheme_fixed_start','Begin of fixed addresses');
  StoreTranslateableText(conn,'scheme_fixed_end','End of fixed addresses');
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
  res       :TFRE_DB_DIALOG_DESC;
  scheme    : IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  GFRE_DBI.GetSystemScheme(TFRE_DB_DHCP_SUBNET,scheme);
  res:=TFRE_DB_DIALOG_DESC.Create.Describe('Add Subnet');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.SetElementValue('dhcp',UID_String);
  serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_DHCP_SUBNET','newOperation');
  serverFunc.AddParam.Describe('collection','dhcp_subnet');
  res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_DHCP.IMI_addFixedHost(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res       : TFRE_DB_DIALOG_DESC;
  scheme    : IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  GFRE_DBI.GetSystemScheme(TFRE_DB_DHCP_FIXED,scheme);
  res:=TFRE_DB_DIALOG_DESC.Create.Describe('Add Subnet');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.SetElementValue('dhcp',UID_String);
  serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_DHCP_FIXED','newOperation');
  serverFunc.AddParam.Describe('collection','dhcp_fixed');
  res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  Result:=res;
end;



{ TFRE_DB_Certificate }

class procedure TFRE_DB_Certificate.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('ca',fdbft_ObjLink).required:=true;
  scheme.AddSchemeField('cn',fdbft_String).required:=true;
  scheme.AddSchemeField('c',fdbft_String);
  scheme.AddSchemeField('email',fdbft_String);
  scheme.AddSchemeField('st',fdbft_String);
  scheme.AddSchemeField('l',fdbft_String);
  scheme.AddSchemeField('ou',fdbft_String);
  scheme.AddSchemeField('crt',fdbft_String);
  scheme.AddSchemeField('key',fdbft_String);
  scheme.AddSchemeField('issued',fdbft_DateTimeUTC);
  scheme.AddSchemeField('revoked',fdbft_DateTimeUTC);
  scheme.AddSchemeField('valid',fdbft_DateTimeUTC);
  scheme.AddSchemeField('revoke',fdbft_Boolean);
  scheme.SetSysDisplayField(TFRE_DB_NameTypeArray.Create('cn'),'%s');

  group:=scheme.AddInputGroup('main_create').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('ca','',false,true);
  group.AddInput('cn',GetTranslateableTextKey('scheme_cn'));
  group.AddInput('c',GetTranslateableTextKey('scheme_c'));
  group.AddInput('email',GetTranslateableTextKey('scheme_email'));
  group.AddInput('st',GetTranslateableTextKey('scheme_st'));
  group.AddInput('l',GetTranslateableTextKey('scheme_l'));
  group.AddInput('ou',GetTranslateableTextKey('scheme_ou'));

  group:=scheme.AddInputGroup('main_edit').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.UseInputGroup(scheme.DefinedSchemeName,'main_create');
  group.AddInput('revoke',GetTranslateableTextKey('scheme_revoke'),true);
  group.AddInput('issued',GetTranslateableTextKey('scheme_issued'),true);
  group.AddInput('revoked',GetTranslateableTextKey('scheme_revoked'),true);
  group.AddInput('valid',GetTranslateableTextKey('scheme_valid'),true);
  group.AddInput('crt',GetTranslateableTextKey('scheme_crt'));
  group.AddInput('key',GetTranslateableTextKey('scheme_key'));
end;

class procedure TFRE_DB_Certificate.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','Certificate');
  StoreTranslateableText(conn,'scheme_cn','Common Name');
  StoreTranslateableText(conn,'scheme_c','Country');
  StoreTranslateableText(conn,'scheme_email','EMail');
  StoreTranslateableText(conn,'scheme_st','State');
  StoreTranslateableText(conn,'scheme_l','Location');
  StoreTranslateableText(conn,'scheme_ou','Organization Unit');
  StoreTranslateableText(conn,'scheme_revoke','Revoke');
  StoreTranslateableText(conn,'scheme_issued','Issued');
  StoreTranslateableText(conn,'scheme_revoked','Revoked');
  StoreTranslateableText(conn,'scheme_valid','Valid');
  StoreTranslateableText(conn,'scheme_crt','Certificate');
  StoreTranslateableText(conn,'scheme_key','Key');
end;

function TFRE_DB_Certificate.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Revoke Certificate','images_apps/cloudcontrol/revoke_certificate.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'revoke'));
  Result:=res;
end;

function TFRE_DB_Certificate.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res    :TFRE_DB_FORM_PANEL_DESC;
  scheme :IFRE_DB_SchemeObject;
begin
  scheme := GetScheme;
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('Certificate Authority');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main_edit'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

{ TFRE_DB_CA }

class procedure TFRE_DB_CA.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_SERVICE');
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('cn',fdbft_String).required:=true;
  scheme.AddSchemeField('c',fdbft_String);
  scheme.AddSchemeField('email',fdbft_String);
  scheme.AddSchemeField('st',fdbft_String);
  scheme.AddSchemeField('l',fdbft_String);
  scheme.AddSchemeField('o',fdbft_String);
  scheme.AddSchemeField('ou',fdbft_String);
  scheme.AddSchemeField('crt',fdbft_String);
  scheme.AddSchemeField('key',fdbft_String);
  scheme.AddSchemeField('pass',fdbft_String);
  scheme.AddSchemeField('issued',fdbft_DateTimeUTC);

  group:=scheme.ReplaceInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('objname',GetTranslateableTextKey('scheme_objname'));
  group.AddInput('cn',GetTranslateableTextKey('scheme_cn'));
  group.AddInput('c',GetTranslateableTextKey('scheme_c'));
  group.AddInput('email',GetTranslateableTextKey('scheme_email'));
  group.AddInput('st',GetTranslateableTextKey('scheme_st'));
  group.AddInput('l',GetTranslateableTextKey('scheme_l'));
  group.AddInput('o',GetTranslateableTextKey('scheme_o'));
  group.AddInput('ou',GetTranslateableTextKey('scheme_ou'));
  group.AddInput('crt',GetTranslateableTextKey('scheme_crt'));
  group.AddInput('key',GetTranslateableTextKey('scheme_key'));
  group.AddInput('pass',GetTranslateableTextKey('scheme_pass'));
  group.AddInput('issued',GetTranslateableTextKey('scheme_issued'),true);
end;

class procedure TFRE_DB_CA.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','Certificate Authority');
  StoreTranslateableText(conn,'scheme_objname','Name');
  StoreTranslateableText(conn,'scheme_cn','Common Name');
  StoreTranslateableText(conn,'scheme_c','Country');
  StoreTranslateableText(conn,'scheme_email','EMail');
  StoreTranslateableText(conn,'scheme_st','State');
  StoreTranslateableText(conn,'scheme_l','Location');
  StoreTranslateableText(conn,'scheme_o','Organization Unit');
  StoreTranslateableText(conn,'scheme_ou','Organization Unit');
  StoreTranslateableText(conn,'scheme_crt','Certificate');
  StoreTranslateableText(conn,'scheme_key','Key');
  StoreTranslateableText(conn,'scheme_pass','Password');
  StoreTranslateableText(conn,'scheme_issued','Issued');
end;

function TFRE_DB_CA.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Add Certificate','images_apps/cloudcontrol/add_certificate.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'addCertificate'));
  res.AddEntry.Describe('Delete','images_apps/cloudcontrol/delete_ca.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'deleteOperation'));
  Result:=res;
end;

function TFRE_DB_CA.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
  scheme: IFRE_DB_SchemeObject;
begin
  scheme := GetScheme;
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('Certificate Authority');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_CA.WEB_ChildrenData(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res   : TFRE_DB_STORE_DATA_DESC;
  entry : IFRE_DB_Object;
  childs: TFRE_DB_GUIDArray;
  i     : Integer;
  dbo   : IFRE_DB_Object;

begin
  res := TFRE_DB_STORE_DATA_DESC.create;
  //fixme
  abort;
  //childs:=ReferencedByList;
  for i := 0 to Length(childs) - 1 do begin
    conn.Fetch(childs[i],dbo);
    if dbo.IsA('TFRE_DB_CERTIFICATE') then begin
      entry:=GFRE_DBI.NewObject;
      entry.Field('text').AsString:=dbo.field('cn').AsString;
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

function TFRE_DB_CA.IMI_AddCertificate(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  scheme    : IFRE_DB_SchemeObject;
  res       : TFRE_DB_DIALOG_DESC;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  GFRE_DBI.GetSystemScheme(TFRE_DB_Certificate,scheme);
  res:=TFRE_DB_DIALOG_DESC.Create.Describe('Add Certificate');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main_create'),GetSession(input));
  res.SetElementValue('ca',UID_String);
  serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_Certificate','newOperation');
  serverFunc.AddParam.Describe('collection','certificate');
  res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  Result:=res;
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

class procedure TFRE_DB_WifiNetwork.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','Wifi Network');
  StoreTranslateableText(conn,'scheme_ssid','SSID');
  StoreTranslateableText(conn,'scheme_hidden','Hidden Network');
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

class procedure TFRE_DB_RadiusNetwork.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','Radius Wifi Network');
  StoreTranslateableText(conn,'scheme_caid','CAID');
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

class procedure TFRE_DB_Network.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
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

class procedure TFRE_DB_Network.NetworkOnChange(const dbc: IFRE_DB_Connection; const is_dhcp: boolean; const subnet: string; const ep_id: TGUID; const dns: string; const range_start, range_end: integer);
  var dhcp_obj        : IFRE_DB_Object;
      dhcp_id         : TGUID;
      childs          : TFRE_DB_GUIDArray;
      dhcp_subnet_obj : IFRE_DB_Object;
      i               : integer;
      colls           : IFRE_DB_Collection;
      current_ip      : string;
      highest         : integer;
      sub_ip          : integer;
      do_update       : boolean;
      routing_id      : TGUID;
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
      if       not dbc.Fetch(dhcp_id,dhcp_obj)          then raise EFRE_DB_Exception.Create(edb_ERROR,'NO DHCP SERVICE FOUND IN NETWORK ON CHANGE');
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
        colls          := dbc.Collection('dhcp_subnet');
        dhcp_subnet_obj:= GFRE_DBI.NewObjectScheme(TFRE_DB_DHCP_Subnet);
        _setfields;
        dhcp_subnet_obj.Field('dhcp').AsObjectLink:=dhcp_id;
        CheckDbResult(COLLS.Store(dhcp_subnet_obj),'Add DHCP Subnet');
      end;
    end;

    // check routing
    if   not dbc.Fetch(ep_id,ep_obj)          then raise EFRE_DB_Exception.Create(edb_ERROR,'NO EP FOUND IN NETWORK ON CHANGE');
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
      if       not dbc.Fetch    (routing_id, routing_obj)          then raise EFRE_DB_Exception.Create(edb_ERROR,'NO ROUTING SERVICE FOUND IN NETWORK ON CHANGE');
      for i := 0 to routing_obj.Field('static').ValueCount-1 do begin
        route_obj   :=    routing_obj.Field('static').AsObjectItem[i];
        if route_obj.Field('subnet').AsString=subnet then begin
          writeln('ALREADY IN ROUTING, UPDATING !');
          do_update  := true;
          break;
        end;
      end;

      if do_update then begin
       route_obj.Field('gateway').AsString := '1.1.1.1';
      end else begin
       route_obj:=GFRE_DBI.NewObjectScheme(TFRE_DB_Route);
       route_obj.Field('subnet').AsString:=subnet;
       route_obj.Field('gateway').AsString:='2.2.2.2';
       routing_obj.Field('static').AddObject(route_obj);
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
      ep_id      :   TGUID;
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
    update_object_uid : TGUid;
    raw_object        : IFRE_DB_Object;
    dbc               : IFRE_DB_CONNECTION;
    ep_id             : TGUID;
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

class procedure TFRE_DB_MobileDevice.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','Mobile Device');
  StoreTranslateableText(conn,'scheme_name','Device Name');
  StoreTranslateableText(conn,'scheme_certificate','Certificate');
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

class procedure TFRE_DB_Endpoint.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  StoreTranslateableText(conn,'scheme_main_group','End Point Configuration');
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
  res       : TFRE_DB_DIALOG_DESC;
  scheme    : IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  abort;//FIXME
  //GFRE_DBI.GetSystemScheme(TFRE_DB_OpenWifiNetwork,scheme);
  //res:=TFRE_DB_DIALOG_DESC.Create.Describe('Add Open Wifi Network');
  //res.SendChangedFieldsOnly(false);
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
  res       : TFRE_DB_DIALOG_DESC;
  scheme    : IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  abort;//FIXME
  //GFRE_DBI.GetSystemScheme(TFRE_DB_WPA2NETWORK,scheme);
  //res:=TFRE_DB_DIALOG_DESC.Create.Describe('Add WPA2 Network');
  //res.SendChangedFieldsOnly(false);
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
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_MACHINE.ClassName);
 end;

 class procedure TFRE_DB_ZONE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
 end;

 procedure TFRE_DB_ZONE.CALC_GetDisplayName(const setter: IFRE_DB_CALCFIELD_SETTER);
 begin
   setter.SetAsString('Zone '+Field('objname').AsString);
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

 class procedure TFRE_DB_MACHINE_SETTING_TIME.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   StoreTranslateableText(conn,'scheme_setting','Setting');
   StoreTranslateableText(conn,'scheme_region','Region');
   StoreTranslateableText(conn,'scheme_timezone','Timezone');
   StoreTranslateableText(conn,'scheme_ntpserver','NTP Server');
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

 class procedure TFRE_DB_MACHINE_SETTING_MAIL.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   StoreTranslateableText(conn,'scheme_setting','Mail Parameters');
   StoreTranslateableText(conn,'scheme_smtpserver','SMTP Server');
   StoreTranslateableText(conn,'scheme_smtpuser','SMTP User');
   StoreTranslateableText(conn,'scheme_smtppassword','SMTP Password');
   StoreTranslateableText(conn,'scheme_mailfrom','Mail from');
   StoreTranslateableText(conn,'scheme_mailto','Mail to');
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

 class procedure TFRE_DB_MACHINE_SETTING_HOSTNAME.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   StoreTranslateableText(conn,'scheme_setting','Setting');
   StoreTranslateableText(conn,'scheme_hostname','Hostname');
   StoreTranslateableText(conn,'scheme_domainname','Domainname');
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

 class procedure TFRE_DB_MACHINE_SETTING_POWER.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   StoreTranslateableText(conn,'scheme_setting','Setting');
   StoreTranslateableText(conn,'scheme_uptime','Uptime');
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

 class procedure TFRE_DB_MACHINE_SETTING.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   StoreTranslateableText(conn,'scheme_main','Properties');
   StoreTranslateableText(conn,'scheme_name','Name');
   StoreTranslateableText(conn,'scheme_description','Description');
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

 class procedure TFRE_DB_FC_PORT.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
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

 { TFRE_DB_DATALINK_STUB }

 class procedure TFRE_DB_DATALINK_STUB.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_DATALINK.ClassName);
 end;

 class procedure TFRE_DB_DATALINK_STUB.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
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

 { TFRE_DB_DATALINK_AGGR }

 class procedure TFRE_DB_DATALINK_AGGR.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_DATALINK.ClassName);
 end;

 class procedure TFRE_DB_DATALINK_AGGR.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
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

 { TFRE_DB_DATALINK_VNIC }

 class procedure TFRE_DB_DATALINK_VNIC.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_DATALINK.ClassName);
 end;

 class procedure TFRE_DB_DATALINK_VNIC.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
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

 { TFRE_DB_DATALINK_PHYS }

 class procedure TFRE_DB_DATALINK_PHYS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_DATALINK.ClassName);
 end;

 class procedure TFRE_DB_DATALINK_PHYS.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
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
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.Classname);
   scheme.GetSchemeField('objname').required:=true;
   scheme.AddSchemeField('parentid',fdbft_ObjLink).multiValues:=false;
   scheme.AddSchemeField('ip_net',fdbft_String).SetupFieldDef(false,false,'','ip');
   scheme.AddSchemeField('mtu',fdbft_Uint16);
   scheme.AddSchemeField('vlan',fdbft_Uint16);
   scheme.AddSchemeField('showvirtual',fdbft_Boolean).required:=true;
   scheme.AddSchemeField('showglobal',fdbft_Boolean).required:=true;
   group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
   group.AddInput('objname',GetTranslateableTextKey('scheme_name'),true);
   group.AddInput('desc.txt',GetTranslateableTextKey('scheme_description'));
   group.AddInput('ip_net',GetTranslateableTextKey('scheme_ip_net'));
   group.AddInput('mtu',GetTranslateableTextKey('scheme_mtu'));
   group.AddInput('vlan',GetTranslateableTextKey('scheme_vlan'));
 end;

 class procedure TFRE_DB_DATALINK.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   StoreTranslateableText(conn,'scheme_main_group','Link Properties');
   StoreTranslateableText(conn,'scheme_name','Link Name');
   StoreTranslateableText(conn,'scheme_description','Description');
   StoreTranslateableText(conn,'scheme_ip_net','IP/Subnet');
   StoreTranslateableText(conn,'scheme_mtu','MTU');
   StoreTranslateableText(conn,'scheme_vlan','Vlan');
 end;

 function TFRE_DB_DATALINK.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
 begin
   writeln('DATALINK MENU');
   result := GFRE_DB_NIL_DESC;
 end;

 { TFRE_DB_TESTER }

 class procedure TFRE_DB_TESTER.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 begin
   inherited RegisterSystemScheme(scheme);
 end;

 class procedure TFRE_DB_TESTER.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
 end;


 class procedure TFRE_DB_VMACHINE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_MACHINE.ClassName);
 end;

 class procedure TFRE_DB_VMACHINE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
 end;

 procedure TFRE_DB_VMACHINE.CALC_GetDisplayName(const setter: IFRE_DB_CALCFIELD_SETTER);
 begin
   setter.SetAsString('VM '+Field('objname').AsString);
 end;


 class procedure TFRE_DB_MACHINE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName(TFRE_DB_ObjectEx.ClassName);
   scheme.GetSchemeField('objname').required:=true;
   scheme.AddSchemeField('ip',fdbft_String);
   scheme.AddSchemeField('service',fdbft_ObjLink);
   scheme.AddSchemeFieldSubscheme('position','TFRE_DB_GEOPOSITION').required:=false;
   scheme.AddSchemeFieldSubscheme('address','TFRE_DB_ADDRESS').required:=false;
   scheme.AddCalcSchemeField('displayaddress',fdbft_String,@CALC_GetDisplayAddress);
   scheme.AddSchemeField('domainid',fdbft_GUID);
   scheme.AddCalcSchemeField('displayname',fdbft_String,@CALC_GetDisplayName);

   group:=scheme.AddInputGroup('address').Setup(GetTranslateableTextKey('scheme_address_group'));
   group.UseInputGroup('TFRE_DB_ADDRESS','main','address');
   group.UseInputGroup('TFRE_DB_GEOPOSITION','main','position');

 end;

 class procedure TFRE_DB_MACHINE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   StoreTranslateableText(conn,'scheme_address_group','Site Address');
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
   setter.SetAsString('Machine '+Field('objname').AsString);
 end;



 class procedure TFRE_DB_Service.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName  ('TFRE_DB_OBJECTEX');
   scheme.AddSchemeField('machineid',fdbft_ObjLink);
   scheme.AddSchemeField('servicegroup',fdbft_ObjLink);
   group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
   group.AddInput('machineid','',false,true);
 end;

 class procedure TFRE_DB_Service.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   StoreTranslateableText(conn,'scheme_main_group','General Information');
 end;

 class procedure TFRE_DB_ServiceGroup.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
 var group : IFRE_DB_InputGroupSchemeDefinition;
 begin
   inherited RegisterSystemScheme(scheme);
   scheme.SetParentSchemeByName  ('TFRE_DB_OBJECTEX');
   scheme.GetSchemeField('objname').required:=true;
   scheme.AddSchemeField('customerid',fdbft_ObjLink);
   scheme.AddSchemeField('monitoring',fdbft_Objlink);
   group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
   group.AddInput('objname',GetTranslateableTextKey('scheme_name'));
   group.AddInput('customerid','');
 end;

 class procedure TFRE_DB_ServiceGroup.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; out newVersionId: TFRE_DB_NameType);
 begin
   newVersionId:='1.0';
   StoreTranslateableText(conn,'scheme_main_group','General Information');
   StoreTranslateableText(conn,'scheme_name','Name');
 end;

 procedure Register_DB_Extensions;
 var validator : IFRE_DB_ClientFieldValidator;
     enum      : IFRE_DB_Enum;
 begin
   validator:=GFRE_DBI.NewClientFieldValidator('ip').Setup('^([1-9][0-9]{0,1}|1[013-9][0-9]|12[0-689]|2[01][0-9]|22[0-3])([.]([1-9]{0,1}[0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])){2}[.]([1-9][0-9]{0,1}|[1-9]{0,1}[0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-4])(\/([89]|[12][0-9]|3[0-2])|$)$',
                                                     GFRE_DBI.CreateText('$validator_ip','IP Validator'),
                                                     GFRE_DBI.CreateText('$validator_help_ip','1.0.0.1 - 223.255.255.254 excluding 127.x.x.x'),
                                                     '\d\.\/');
   GFRE_DBI.RegisterSysClientFieldValidator(validator);
   validator:=GFRE_DBI.NewClientFieldValidator('mac').Setup('^([0-9a-fA-F]{2}(:|$)){6}$',
                                                      GFRE_DBI.CreateText('$validator_help_mac','MAC Validator'),
                                                      GFRE_DBI.CreateText('$validator_mac','00:01:02:03:04:05'),
                                                      '\da-fA-F:');
   GFRE_DBI.RegisterSysClientFieldValidator(validator);

   enum:=GFRE_DBI.NewEnum('routing').Setup(GFRE_DBI.CreateText('$enum_routing','Routing Enum'));
   enum.addEntry('enabled',GFRE_DBI.CreateText('$enum_routing_enabled','Enabled'));
   enum.addEntry('disabled',GFRE_DBI.CreateText('$enum_routing_disabled','Disabled'));
   enum.addEntry('nat',GFRE_DBI.CreateText('$enum_routing_nat','NAT'));
   GFRE_DBI.RegisterSysEnum(enum);


   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MACHINE_SETTING);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MACHINE_SETTING_POWER);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MACHINE_SETTING_MAIL);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MACHINE_SETTING_TIME);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MACHINE_SETTING_HOSTNAME);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_FC_PORT);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATALINK);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATALINK_PHYS);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATALINK_VNIC);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATALINK_AGGR);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DATALINK_STUB);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ServiceGroup);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Service);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Machine);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_VMachine);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ZONE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Tester);

   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DEVICE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_NETWORK_GROUP);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CMS);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ROUTE);
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
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Certificate);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DHCP);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DHCP_Subnet);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DHCP_Fixed);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_VPN);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_RADIUS);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Routing);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CMS_PAGE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CMS_ADPAGE);
   GFRE_DBI.RegisterObjectClassEx(TFRE_DB_REDIRECTION_FLOW);

   GFRE_DBI.Initialize_Extension_Objects;
 end;



end.

