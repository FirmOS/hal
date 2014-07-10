unit fos_vm_control_interface;

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
{$codepage UTF8}

interface

uses
  Classes, SysUtils,FRE_DB_INTERFACE,FOS_TOOL_INTERFACES;

  type
    VNC_Rec = record
      guid     : string;
      vnc_port : string;
      vnc_host : string;
      vm_alias : string;
      domain   : string;
    end;

    { IFOS_VM_HOST_CONTROL }

    IFOS_VM_HOST_CONTROL=interface
      function  VM_ListMachines  (out   vm_list: IFRE_DB_Object):TFRE_DB_Errortype;
      function  VM_Reboot        (const vm_key:String):TFRE_DB_Errortype;
      function  VM_Halt          (const vm_key:String;const force:boolean=false):TFRE_DB_Errortype;
      function  VM_Start         (const vm_key:String):TFRE_DB_Errortype;
      function  VM_State         (const vm_key:String;out state:string):TFRE_DB_Errortype;
      function  VM_Host          :String;
      function  DS_IdentifyDisk  (const diskspec : string ; const off : boolean) : TFRE_DB_Errortype;

      function  VM_EnableVMMonitor(const enable:boolean) : TFRE_DB_Errortype;
      function  Get_VM_Data      :IFRE_DB_Object;

      procedure Finalize;
    end;

    function Get_VM_Host_Control(const user,host:string): IFOS_VM_HOST_CONTROL;
    procedure VM_UpdateCollection(const dbc: IFRE_DB_CONNECTION; const vmc: IFRE_DB_COLLECTION; const vmo: IFRE_DB_Object; const vmachineclass: string; const zoneclass:string);

   const cVNCPorts : Array [0..61] of VNC_Rec = (
      (guid: '06dec646-802f-4d99-8eb3-4b50db030224'; vnc_port :'5900'; vnc_host :'10.220.251.10'; vm_alias :'firmbsd82leg' ; domain : 'firmos'),
      (guid: '183934d6-8475-44a7-a6df-e93ea4b1320f'; vnc_port :'5901'; vnc_host :'10.220.251.10'; vm_alias :'ebay_sniper' ; domain : 'firmos'),
      (guid: '2bceffb3-5b9d-4851-8ecc-3f82a8684da8'; vnc_port :'5902'; vnc_host :'10.220.251.10'; vm_alias :'testubuntu2' ; domain : 'demo'),
      (guid: '5c8727f9-89b7-4e47-aa1e-5da46ba8aecf'; vnc_port :'5903'; vnc_host :'10.220.251.10'; vm_alias :'firmdebian64' ; domain : 'firmos'),
      (guid: '7a6999cf-bd18-49c8-8eaf-1590b26e86c8'; vnc_port :'5904'; vnc_host :'10.220.251.10'; vm_alias :'firmdebian32' ; domain : 'firmos'),
      (guid: '9aa85aa6-091d-43f4-8cd4-c75c01dfd856'; vnc_port :'5905'; vnc_host :'10.220.251.10'; vm_alias :'nagios' ; domain : 'firmos'),
      (guid: 'c7e81ecb-107e-424e-9de3-bb4ed2e7e8c7'; vnc_port :'5906'; vnc_host :'10.220.251.10'; vm_alias :'firmbsd90' ; domain : 'firmos'),
      (guid: 'd8402ea1-02ba-44a2-b0d5-c467b28a618a'; vnc_port :'5907'; vnc_host :'10.220.251.10'; vm_alias :'firmbsd9032' ; domain : 'firmos'),
      (guid: '1e0fc382-9a1d-4fd4-9101-70714b7e6124'; vnc_port :'5908'; vnc_host :'10.220.251.10'; vm_alias :'openwrt_ubnt_build' ; domain : 'firmos'),
      (guid: '30505c27-13a1-4653-a4a1-e5f3f8726e32'; vnc_port :'5909'; vnc_host :'10.220.251.10'; vm_alias :'conference_debian64' ; domain : 'firmos'),
      (guid: '4d242470-2994-4536-8fea-3b80fe52b944'; vnc_port :'5910'; vnc_host :'10.220.251.10'; vm_alias :'meeting' ; domain : 'demo'),
      (guid: '6c0117b4-c452-4b56-adf0-6824e5694a7c'; vnc_port :'5911'; vnc_host :'10.220.251.10'; vm_alias :'ddwrt' ; domain : 'firmos'),
      (guid: '79387d0d-4a59-4bae-b29d-55474744733a'; vnc_port :'5912'; vnc_host :'10.220.251.10'; vm_alias :'win7dev' ; domain : 'firmos'),
      (guid: '02dd31e0-49c0-411c-bf6c-fbbeb18b5173'; vnc_port :'5913'; vnc_host :'10.220.251.10'; vm_alias :'fpcFreeDos11' ; domain : 'fpc'),
      (guid: '7e519b99-5955-464b-8b09-08c553cc1a6c'; vnc_port :'5914'; vnc_host :'10.220.251.10'; vm_alias :'fosfip32' ; domain : 'firmos'),
      (guid: '7c652e8b-5e2d-4f84-bd2b-6b215e4482ae'; vnc_port :'5915'; vnc_host :'10.220.251.10'; vm_alias :'openindiana'; domain : 'fpc'),
      (guid: 'a2963da5-fa1d-4584-bf4c-1837f9149c62'; vnc_port :'5916'; vnc_host :'10.220.251.10'; vm_alias :'fpchaiku32'; domain : 'fpc'),
      (guid: '61309ea0-6c47-4afe-a810-d0feaa93fc5e'; vnc_port :'5917'; vnc_host :'10.220.251.10'; vm_alias :'netbsd64'; domain : 'fpc'),
      (guid: '084d525d-40c5-47fe-8b97-b2de15ea1a52'; vnc_port :'5918'; vnc_host :'10.220.251.10'; vm_alias :'fpcfed32'; domain : 'fpc'),
      (guid: 'c5c6f718-0793-4fd6-af9e-db27ac2f5c0c'; vnc_port :'5919'; vnc_host :'10.220.251.10'; vm_alias :'fpcfed64'; domain : 'fpc'),
      (guid: '5c2959d7-4b38-4a1f-81db-3753f22ebeab'; vnc_port :'5920'; vnc_host :'10.220.251.10'; vm_alias :'netbsd32'; domain : 'fpc'),
      (guid: '01dfc6a4-0d7c-401a-abce-ac431199fa45'; vnc_port :'5921'; vnc_host :'10.220.251.10'; vm_alias :'openbsd32'; domain : 'fpc'),
      (guid: '17a9ead7-0e64-491e-905b-7be8f57c26f6'; vnc_port :'5922'; vnc_host :'10.220.251.10'; vm_alias :'openbsd64'; domain : 'fpc'),
      (guid: '84810c1f-171b-447c-8f49-746e7e460805'; vnc_port :'5923'; vnc_host :'10.220.251.10'; vm_alias :'freelove832'; domain : 'fpc'),
      (guid: 'dc432b80-3ba0-43c5-b052-6611079ce2ee'; vnc_port :'5924'; vnc_host :'10.220.251.10'; vm_alias :'freelove864'; domain : 'fpc'),
      (guid: '8dce2673-5461-4f4a-9020-78e0198bf1e1'; vnc_port :'5925'; vnc_host :'10.220.251.10'; vm_alias :'freelove932'; domain : 'fpc'),
      (guid: 'f6ebc131-258b-4e48-89a4-5499646b0cb1'; vnc_port :'5926'; vnc_host :'10.220.251.10'; vm_alias :'freelove964'; domain : 'fpc'),
      (guid: 'fb358524-fecb-48a9-8dc6-50103b81cd90'; vnc_port :'5927'; vnc_host :'10.220.251.10'; vm_alias :'freepascal_dos'; domain : 'fpc'),
      (guid: '6813daeb-6100-4586-8680-94ba376a25b9'; vnc_port :'5929'; vnc_host :'10.220.251.10'; vm_alias :'oraclesolaris'; domain : 'fpc'),
      (guid: 'c657f2ab-221d-43a6-9a24-ac8913bda285'; vnc_port :'5930'; vnc_host :'10.220.251.10'; vm_alias :'fpclin32'; domain : 'fpc'),
      (guid: 'ea5b9377-6d55-4bcb-9c65-f3bcbdbe73cb'; vnc_port :'5931'; vnc_host :'10.220.251.10'; vm_alias :'fpclin64'; domain : 'fpc'),
      (guid: '1de7b282-55ba-4a07-b85e-79b262692b38'; vnc_port :'5932'; vnc_host :'10.220.251.10'; vm_alias :'firmosbackoffice'; domain : 'firmos'),
      (guid: 'fd9bb50e-0f6d-4466-a5b1-56f2be960256'; vnc_port :'5933'; vnc_host :'10.220.251.10'; vm_alias :'lazarusteam32'; domain : 'fpc'),
      (guid: '529130f8-e3dc-4bd9-8e0a-8b0951a99db9'; vnc_port :'5934'; vnc_host :'10.220.251.10'; vm_alias :'fpcvista64'; domain : 'fpc'),
      (guid: 'b59a9558-7e0d-457c-b77c-b2e625bab272'; vnc_port :'5935'; vnc_host :'10.220.251.10'; vm_alias :'fpcwinxp32'; domain : 'fpc'),
      (guid: 'a0c87121-8656-4e82-bbab-77a6632f5736'; vnc_port :'5936'; vnc_host :'10.220.251.10'; vm_alias :'fpcwin764'; domain : 'fpc'),
      (guid: '7aab24fd-a4ce-4192-9498-bc3523947497'; vnc_port :'5937'; vnc_host :'10.220.251.10'; vm_alias :'fpcJenkins'; domain : 'fpc'),
      (guid: '55893f44-72bb-47c2-8f92-23791e0e9ff7'; vnc_port :'5938'; vnc_host :'10.220.251.10'; vm_alias :'fpcOpenSolaris'; domain : 'fpc'),
      (guid: '5e5c91ed-97df-4b35-b6dd-d936bb6052d8'; vnc_port :'5939'; vnc_host :'10.220.251.10'; vm_alias :'fpcAmiga'; domain : 'fpc'),
      (guid: '1cf7270a-baab-4119-b50e-74c1ae077d94'; vnc_port :'5940'; vnc_host :'10.220.251.10'; vm_alias :'fpcreactos'; domain : 'fpc'),
      (guid: '9ccf806f-ce4f-4ff2-9447-4b03956c6933'; vnc_port :'5941'; vnc_host :'10.220.251.10'; vm_alias :'newlazarusteam'; domain : 'fpc'),
      (guid: 'fcdf2316-161b-4454-b34c-d9fbce215d5b'; vnc_port :'5942'; vnc_host :'10.220.251.10'; vm_alias :'fpcwin2008v2'; domain : 'fpc'),

      (guid: '1160215a-c3f8-4aac-b932-46db11a5b398'; vnc_port :''; vnc_host :''; vm_alias :'monsysprod'; domain : 'firmos'),
      (guid: '1e17ec02-2f5e-4372-a508-ac812b46db5c'; vnc_port :''; vnc_host :''; vm_alias :'redmine'; domain : 'firmos'),
      (guid: '92c2014b-e636-4d64-ba02-5bdf168b33e4'; vnc_port :''; vnc_host :''; vm_alias :'evercity'; domain : 'firmos'),
      (guid: '13bad66b-dcbc-4eb1-9779-9b0465fc4159'; vnc_port :''; vnc_host :''; vm_alias :'firmosweb'; domain : 'firmos'),
      (guid: '15843d0e-d5c0-4e19-bd4d-6fd6f7cfd657'; vnc_port :''; vnc_host :''; vm_alias :'smos_package_build'; domain : 'firmos'),
      (guid: '7b161e9f-36a4-4abf-925d-45c1b09ca1dc'; vnc_port :''; vnc_host :''; vm_alias :'fosbuild_fpc'; domain : 'firmos'),
      (guid: '5c1eae31-67bc-4f19-a148-9ede039d7ad8'; vnc_port :''; vnc_host :''; vm_alias :'franzdata'; domain : 'demo'),
      (guid: '563f9e83-71f8-422a-9d69-06a97c9c9193'; vnc_port :''; vnc_host :''; vm_alias :'herrengasse'; domain : 'demo'),
      (guid: 'ad49d4be-156f-4e5a-b108-35339228f196'; vnc_port :''; vnc_host :''; vm_alias :'firmosdrupal'; domain : 'firmos'),
      (guid: '99205c6b-9a05-4033-b94b-93375325111d'; vnc_port :''; vnc_host :''; vm_alias :'webext'; domain : 'firmos'),
      (guid: '67966528-99f4-43c6-9372-363a44a3d17a'; vnc_port :''; vnc_host :''; vm_alias :'ns1'; domain : 'firmos'),
      (guid: 'dc6f9b5b-1dd8-4d34-9645-3450f7c746eb'; vnc_port :''; vnc_host :''; vm_alias :'ns2'; domain : 'firmos'),
      (guid: 'cd96e7f9-931c-4228-a322-2a2dafceee86'; vnc_port :''; vnc_host :''; vm_alias :'demo'; domain : 'demo'),
      (guid: '774ed9e2-21db-413e-9528-225a6dd40e3f'; vnc_port :'5999'; vnc_host :''; vm_alias: 'machine'; domain : 'firmos'),     // local test .116
      (guid: 'd70f8736-fd00-458a-9279-32778b62c0f8'; vnc_port :'5901'; vnc_host :'10.1.0.116'; vm_alias: 'openindiana'; domain : 'firmos'), // local 10.1.0.102
      (guid: 'bc588508-d911-47a6-92b7-5800d50254b8'; vnc_port :'5902'; vnc_host :'10.1.0.116'; vm_alias: 'win2008vsphere'; domain : 'firmos'), // local 10.1.0.102

      (guid: 'artemes'; vnc_port :''; vnc_host :''; vm_alias: 'Artemes Cloud Data'; domain : 'system'),
      (guid: 'infra_test'; vnc_port :''; vnc_host :''; vm_alias: 'Infrastructure Testsystem'; domain : 'system'),
      (guid: '9559fec9-bc0f-4fba-860b-247a21f748c9'; vnc_port :'5901'; vnc_host :'10.220.249.10'; vm_alias: 'debian'; domain : 'system'),
      (guid: 'infra_live'; vnc_port :''; vnc_host :''; vm_alias: 'Infrastructure Livesystem'; domain : 'system')

   );

implementation

uses FRE_PROCESS,FRE_SYSTEM;

type

    { TFOS_VM_VMADM_IMPL }
    TFOS_VM_VMADM_IMPL=class(TObject,IFOS_VM_HOST_CONTROL)
    private
       FRemote                   :boolean;
       cremoteuser               :string;
       cremotehost               :string;
       cremotekeyfilename        :string;
       FProcess_ps               :TFRE_Process;
       memoutstream_ps           :TMemoryStream;
       memerrstream_ps           :TMemoryStream;
       FProcess_za               :TFRE_Process;
       memoutstream_za           :TMemoryStream;
       memerrstream_za           :TMemoryStream;

       FEnabled                  :Boolean;
       VMLock                    :IFOS_LOCK;
       vmdata                    :IFRE_DB_Object;
       stream_ps                 :TStringStream;
       lines_ps                  :TStringList;
       stream_za                 :TStringStream;
       lines_za                  :TStringList;

       procedure  SetupRemote    (const user,host,idrsa:string);

       procedure  PSOutStreamCallBack (const stream:TStream);
       procedure  PSErrStreamCallBack (const stream:TStream);

       procedure  ZAOutStreamCallBack (const stream:TStream);
       procedure  ZAErrStreamCallBack (const stream:TStream);

       function   VM_State       (const vm_key:String;out state:string):TFRE_DB_Errortype;
       function   VM_Reboot      (const vm_key:String):TFRE_DB_Errortype;
       function   VM_Halt        (const vm_key:String;const force:boolean=false):TFRE_DB_Errortype;
       function   VM_Start       (const vm_key:String):TFRE_DB_Errortype;
       function   VM_Host        :String;
       function   DS_IdentifyDisk(const diskspec : string ; const off : boolean) : TFRE_DB_Errortype;
       procedure  Finalize;
       procedure  GetConstants   (const guid : string; out vncport,vnchost,vmalias,domain:string);
       procedure  InitVMData     ;

       function   _PSParameter   : TFRE_DB_StringArray;
       function   _ZAParameter   : TFRE_DB_StringArray;
       procedure  ParsePS        (const vm_list : IFRE_DB_Object; const plines : TStringList);
       procedure  ParseZA        (const vm_list : IFRE_DB_Object; const plines : TStringList);

    public
       constructor Create;
       destructor  Destroy; override ;

       function   VM_EnableVMMonitor(const enable:boolean) : TFRE_DB_Errortype;

       function   Get_VM_Data       :IFRE_DB_Object;
       function   VM_ListMachines    (out vm_list: IFRE_DB_Object):TFRE_DB_Errortype;
    end;

function Get_VM_Host_Control(const user,host:string): IFOS_VM_HOST_CONTROL;
var res : TFOS_VM_VMADM_IMPL;
    dir : string;
begin
  res := TFOS_VM_VMADM_IMPL.Create;
  if user<>'' then begin
    dir := SetDirSeparators(cFRE_SERVER_DEFAULT_DIR+'/ssl/user/id_rsa');
    res.SetupRemote(user,host,dir);
  end;
  Result := res;
end;

procedure VM_UpdateCollection(const dbc: IFRE_DB_CONNECTION; const vmc: IFRE_DB_COLLECTION; const vmo: IFRE_DB_Object; const vmachineclass: string; const zoneclass: string);
var i       : integer;
    vm      : IFRE_DB_Object;
    uvm     : IFRE_DB_Object;

  procedure Obj2Obj(const vm,uvm:IFRE_DB_Object);
  begin
    uvm.Field('Objname').AsString  := vm.Field('MName').AsString;
    uvm.Field('MType').AsString    := vm.Field('MType').AsString;
    uvm.Field('State').AsString    := vm.Field('State').AsString;
    uvm.Field('domainid').AsGUID   := dbc.sys.DomainId('SYSTEM');

    if UpperCase(vm.Field('State').AsString)='RUNNING' then begin
      uvm.Field('StateIcon').AsString   := 'images_apps/hal/vm_running.png';
    end else begin
      uvm.Field('StateIcon').AsString   := 'images_apps/hal/vm_stopped.png';
    end;
    uvm.Field('MBrand').AsString   := vm.Field('MBrand').AsString;
    if vm.FieldExists('PERFPCPU') then begin
      uvm.Field('PERFPCPU').AsString := vm.Field('PERFPCPU').AsString+'%';
      uvm.Field('PERFPMEM').AsString := vm.Field('PERFPMEM').AsString+'%';
    end else begin
      uvm.Field('PERFPCPU').AsString := '';
      uvm.Field('PERFPMEM').AsString := '';
    end;
    if vm.FieldExists('PERFRSS') then begin
      uvm.Field('PERFRSS').AsString := Format('%2.2f MB',[vm.Field('PERFRSS').AsUInt32 / 1024]);
    end else begin
      uvm.Field('PERFRSS').AsString :='-';
    end;
    if vm.FieldExists('PERFVSZ') then begin
      uvm.Field('PERFVSZ').AsString := Format('%2.2f MB',[vm.Field('PERFVSZ').AsUInt32 / 1024]);
    end else begin
      uvm.Field('PERFVSZ').AsString :='-';
    end;
    uvm.Field('MVIOPRIO').AsString := vm.Field('MVIOPRIO').AsString;
//    uvm.Field('VM_PROPS').AsObject := vm.Field('VM_PROPS').AsObject;
    uvm.Field('VM_INFO').AsObject  := vm.Field('VM_INFO').AsObject;
    uvm.Field('VNC_PORT').AsString := vm.Field('VM_INFO').AsObject.Field('VNC').AsObject.Field('PORT').AsString;
    uvm.Field('VNC_HOST').AsString := vm.Field('VM_INFO').AsObject.Field('VNC').AsObject.Field('HOST').AsString;
    if vm.Field('MType').AsString='OS' then begin
      uvm.Field('SHELL').AsString    := uvm.Field('VNC_PORT').AsString; //hack
      uvm.Field('VNC_PORT').AsString := '';
    end;
  end;

begin
  { TODO - Use Transactions }
  //vmc.StartBlockUpdating;
  try
    for i := 0 to high(vmo.Field('Machines').AsObjectArr) do begin
      vm      := vmo.Field('Machines').AsObjectArr[i].CloneToNewObject;
      if vmc.GetIndexedObj(vm.Field('key').AsString,UVM) then begin
        Obj2Obj(vm,uvm);
        vmc.Update(uvm);
      end else begin
        if vm.Field('MType').AsString='OS' then begin
          uvm := GFRE_DBI.NewObjectSchemeByName(zoneclass);
        end else begin
          uvm := GFRE_DBI.NewObjectSchemeByName(vmachineclass);
        end;
        Obj2Obj(vm,uvm);
        //uvm.field('UID').AsGUID:=vm.UID;
        uvm.Field('key').AsString    := vm.Field('key').AsString;
        vmc.Store(uvm);
      end;
    end;
    vmo.Finalize;
  finally
    //vmc.FinishBlockUpdating;
  end;
end;

{ TFOS_VM_VMADM_IMPL }

procedure TFOS_VM_VMADM_IMPL.SetupRemote(const user, host, idrsa: string);
begin
  cremoteuser         := user;
  cremotehost         := host;
  cremotekeyfilename  := idrsa;
  FRemote             := true;
end;

procedure TFOS_VM_VMADM_IMPL.PSOutStreamCallBack(const stream: TStream);
var  s: string;
begin
  stream.Position:=0;
  stream_ps.Position := stream_ps.Size;
  stream_ps.CopyFrom(stream,stream.Size);
  stream.Size:=0;

  s:=Copy(stream_ps.DataString,length(stream_ps.Datastring),1);
  if s<>#10 then begin
    exit;
  end;
  lines_ps.DelimitedText := stream_ps.DataString;
  stream_ps.Size:=0;
  try
    ParsePS(vmdata,lines_ps);
  except on E:Exception do begin
     writeln('Error Parsing ps '+E.Message);
     writeln(lines_ps.DelimitedText);
  end;end;
end;

procedure TFOS_VM_VMADM_IMPL.PSErrStreamCallBack(const stream: TStream);
var st : TStringStream;
begin
  if stream.size=0 then begin
    exit;
  end;
  writeln('ERROR PS STREAMCALLBACK:');
  stream.Position:=0;
  st := TStringStream.Create('');
  try
    st.CopyFrom(stream,stream.Size);
    stream.Size:=0;
    writeln(st.DataString);
  finally
    st.Free;
  end;
end;

procedure TFOS_VM_VMADM_IMPL.ZAOutStreamCallBack(const stream: TStream);
var s: string;
begin
  stream.Position:=0;
  stream_za.Position := stream_za.Size;
  stream_za.CopyFrom(stream,stream.Size);
  stream.Size:=0;

  s:=Copy(stream_za.DataString,length(stream_za.Datastring),1);
  if s<>#10 then begin
    exit;
  end;
  lines_za.DelimitedText := stream_za.DataString;
  stream_za.Size:=0;
  VMLock.Acquire;
  try
    try
      ParseZA(vmdata,lines_za);
    except on E:Exception do begin
       writeln('Error Parsing zoneadm '+E.Message);
       writeln(lines_za.DelimitedText);
    end;end;
  finally
    VMLock.Release;
  end;
end;

procedure TFOS_VM_VMADM_IMPL.ZAErrStreamCallBack(const stream: TStream);
var st : TStringStream;
begin
  if stream.size=0 then begin
    exit;
  end;
  writeln('ERROR ZONEADMIN STREAMCALLBACK:');
  stream.Position:=0;
  st := TStringStream.Create('');
  try
    st.CopyFrom(stream,stream.Size);
    stream.Size:=0;
    writeln(st.DataString);
  finally
    st.Free;
  end;
end;

function TFOS_VM_VMADM_IMPL.VM_ListMachines(out vm_list: IFRE_DB_Object): TFRE_DB_Errortype;
var process    : TFRE_Process;
    outs       : string;
    errors     : string;
    res        : integer;
    machs      : TFOSStringArray;
    line_s     : string;
    line_e     : TFOSStringArray;
    i          : Integer;
    j          : integer;
    sp_pos     : integer;
    m_entry    : IFRE_DB_Object;
    dbp        : TFRE_DB_Process;
    dbmp       : TFRE_DB_Multiprocess;
    mprops     : IFRE_DB_Object;
    pids       : String;
    pid        : string;
    pscmd      : string;
    pmem,pcpu,
    pri,rss,
    vsz,zoneid,
    psr,pset   : String;
    zone,fname : string;
    jsonstring : string;
    vncobject  : IFRE_DB_Object;
    vncport    : string;
    vm_alias   : string;
    plines     : TStringList;


begin
  vm_list  := GFRE_DBI.NewObject;


  // Fetch synchronous zoneadm
  process  := TFRE_Process.Create(nil);
  try
    if FRemote then begin
      process.ConfigureRemote_SSH_Mode(cremoteuser,cremotehost,cremotekeyfilename);
    end;
    res    := process.ExecutePiped('zoneadm',_ZAParameter,'',outs,errors);
    if res<>0 then begin
      raise EFRE_DB_Exception.Create(edb_ERROR,'FETCH LOAD');
    end;

    plines := TStringList.Create;
    try
      plines.TextLineBreakStyle := tlbsLF;
      plines.StrictDelimiter:= true;
      plines.Delimiter      := #10;
      plines.DelimitedText  := outs;
      ParseZA(vm_list,plines);
    finally
      plines.Free;
    end;
  finally
    process.Free;
  end;

  // Fetch synchronous process state
  process  := TFRE_Process.Create(nil);
  try
    if FRemote then begin
      process.ConfigureRemote_SSH_Mode(cremoteuser,cremotehost,cremotekeyfilename);
    end;
    res    := process.ExecutePiped('ps',_PSParameter,'',outs,errors);
    if res<>0 then begin
      raise EFRE_DB_Exception.Create(edb_ERROR,'FETCH LOAD');
    end;

    plines := TStringList.Create;
    try
      plines.TextLineBreakStyle := tlbsLF;
      plines.StrictDelimiter:= true;
      plines.Delimiter      := #10;
      plines.DelimitedText  := outs;
      ParsePS(vm_list,plines);
    finally
      plines.Free;
    end;
  finally
    process.Free;
  end;
  //dbmp := TFRE_DB_Multiprocess.create;
  //try
  //  if FRemote then begin
  //    dbmp.SetRemoteSSH(cremoteuser,cremotehost,cremotekeyfilename);
  //  end;
  //  for i := 0 to high(vm_list.Field('Machines').AsObjectArr) do begin
  //    m_entry := vm_list.Field('Machines').AsObjectArr[i];
  //    dbp := TFRE_DB_Process.create;
  //    dbp.SetupInput('vmadm',TFRE_DB_StringArray.Create('get',m_entry.Field('key').AsString));
  //    dbmp.AddProcess(dbp);
  //    if res<>0 then begin
  //      raise EFRE_DB_Exception.Create(edb_ERROR,'ERROR LISTING MACHINES');
  //    end;
  //  end;
  //  dbmp.ExecuteMulti;
  //  for i:=0 to dbmp.ProcessCount-1 do begin
  //    try
  //      if dbmp.GetProcess(i).ExitStatus=0 then begin
  //        mprops := GFRE_DBI.JSONObject2Object(dbmp.GetProcess(i).OutputToString);
  //        mprops.Field('PROP_EXISTS').AsBoolean :=true;
  //      end else begin
  //        mprops := GFRE_DBI.NewObject;
  //        mprops.Field('PROP_EXISTS').AsBoolean := false;
  //        mprops.Field('PROP_ERROR').AsString   := dbmp.GetProcess(i).ErrorToString;
  //        mprops.Field('PROP_OUTPUT').AsString  := dbmp.GetProcess(i).OutputToString;
  //        writeln('ERROR ',dbmp.GetProcess(i).ErrorToString,' ',dbmp.GetProcess(i).OutputToString,' ',dbmp.GetProcess(i).ExitStatus);
  //      end;
  //    except on e:exception do begin
  //      writeln('FAILURE ',i,' ',e.Message,' OUT:',dbmp.GetProcess(i).OutputToString,' ERR:',dbmp.GetProcess(i).ErrorToString);
  //      writeln('--');
  //      writeln(dbmp.GetProcess(i).OutputToString);
  //    end;end;
  //    vm_list.Field('Machines').AsObjectArr[i].Field('VM_PROPS').AsObject := mprops;
  //  end;
  //finally
  //  dbmp.Finalize;
  //end;
  //dbmp := TFRE_DB_Multiprocess.create;
  //try
  //  if FRemote then begin
  //    dbmp.SetRemoteSSH(cremoteuser,cremotehost,cremotekeyfilename);
  //  end;
  //  for i := 0 to high(vm_list.Field('Machines').AsObjectArr) do begin
  //    m_entry := vm_list.Field('Machines').AsObjectArr[i];
  //    dbp := TFRE_DB_Process.create;
  //    dbp.SetupInput('vmadm',TFRE_DB_StringArray.Create('info',m_entry.Field('key').AsString));
  //    dbmp.AddProcess(dbp);
  //    if res<>0 then begin
  //      raise EFRE_DB_Exception.Create(edb_ERROR,'ERROR LISTING MACHINES');
  //    end;
  //  end;
  //  dbmp.ExecuteMulti;
  //  for i:=0 to dbmp.ProcessCount-1 do begin
  //    try
  //      if dbmp.GetProcess(i).ExitStatus=0 then begin
  //        mprops.Field('INFO_EXISTS').AsBoolean :=true;
  //        jsonstring := dbmp.GetProcess(i).OutputToString;
  //        if jsonstring<>'' then begin
  //          mprops := GFRE_DBI.JSONObject2Object(jsonstring);
  //        end else begin
  //          mprops := GFRE_DBI.NewObject;
  //          mprops.Field('INFO_EXISTS').AsBoolean :=false;
  //        end;
  //      end else begin
  //        mprops := GFRE_DBI.NewObject;
  //        mprops.Field('INFO_EXISTS').AsBoolean :=false;
  //        mprops.Field('INFO_ERROR').AsString   := dbmp.GetProcess(i).ErrorToString;
  //        mprops.Field('INFO_OUTPUT').AsString  := dbmp.GetProcess(i).OutputToString;
  //      end;
  //      vm_list.Field('Machines').AsObjectArr[i].Field('VM_Info').AsObject := mprops;
  //    except on e:exception do begin
  //      writeln('FAILURE ',i,' ',e.Message);
  //      writeln('--');
  //      writeln(dbmp.GetProcess(i).ExitStatus,' ',dbmp.GetProcess(i).OutputToString);
  //      writeln('---');
  //    end;end;
  //  end;
//  finally
//    dbmp.Finalize;
//  end;

end;


function TFOS_VM_VMADM_IMPL.VM_Reboot(const vm_key: String): TFRE_DB_Errortype;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res,i   : integer;
begin
  result := edb_ERROR;
  process := TFRE_Process.Create(nil);
  try
    if FRemote then begin
      process.ConfigureRemote_SSH_Mode(cremoteuser,cremotehost,cremotekeyfilename);
    end;
    res          := process.ExecutePiped('vmadm reboot '+trim(vm_key),nil,'',outs,errors);
    if res<>0 then begin
      exit(edb_ERROR);
    end else begin
      exit(edb_OK);
    end;
  finally
    process.Free;
  end;
end;

function TFOS_VM_VMADM_IMPL.VM_Halt(const vm_key: String;const force:boolean=false): TFRE_DB_Errortype;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res,i   : integer;
    count   : integer;

    qmp_quit: string ='{ "execute": "qmp_capabilities" }'#10'{ "execute": "system_powerdown" }';

begin
  result := edb_ERROR;
  count  := 0;
  process := TFRE_Process.Create(nil);
  try
    if FRemote then begin
      process.ConfigureRemote_SSH_Mode(cremoteuser,cremotehost,cremotekeyfilename);
    end;
    if force then begin
      res          := process.ExecutePiped('vmadm stop -f '+trim(vm_key),nil,'',outs,errors);
    end else begin
      writeln('SHUTDOWN GRACEFUL');
      // KVM
      res          := process.ExecutePiped('nc',TFRE_DB_StringArray.Create('-U','/zones/'+trim(vm_key)+'/root/tmp/vm.qmp'),qmp_quit,outs,errors);
      writeln('QMP OUT:',outs);
//  vmobj.zonename + /usr/sbin/shutdown -y -g 0 -i 5
    end;
    if res<>0 then begin
      exit(edb_ERROR);
    end else begin
      exit(edb_OK);
    end;
  finally
    process.Free;
  end;
end;

function TFOS_VM_VMADM_IMPL.VM_Start(const vm_key: String): TFRE_DB_Errortype;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res,i   : integer;
begin
  result := edb_ERROR;
  process := TFRE_Process.Create(nil);
  try
    if FRemote then begin
      process.ConfigureRemote_SSH_Mode(cremoteuser,cremotehost,cremotekeyfilename);
    end;
    res          := process.ExecutePiped('vmadm start '+trim(vm_key),nil,'',outs,errors);
    //res          := process.ExecutePiped('zoneadm',TFRE_DB_StringArray.Create('-z',trim(vm_key),'boot'),'',outs,errors);
    //res          := process.ExecutePiped('/zones/firmos/vmadm/vmadm',TFRE_DB_StringArray.Create('start',trim(vm_key)),'',outs,errors);
    writeln('VM_START RESULT:',res);
    if res<>0 then begin
      exit(edb_ERROR);
    end else begin
      exit(edb_OK);
    end;
  finally
    process.Free;
  end;
end;

function TFOS_VM_VMADM_IMPL.VM_Host: String;
begin
  result := cremotehost;
end;

function TFOS_VM_VMADM_IMPL.DS_IdentifyDisk(const diskspec: string ; const off : boolean): TFRE_DB_Errortype;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res,i   : integer;
begin
  result := edb_ERROR;
  process := TFRE_Process.Create(nil);
  try
    if FRemote then begin
      process.ConfigureRemote_SSH_Mode(cremoteuser,cremotehost,cremotekeyfilename);
    end;
    if off then
      res          := process.ExecutePiped('/zones/firmos/led/disk.py ledoff '+trim(diskspec),nil,'',outs,errors)
    else
      res          := process.ExecutePiped('/zones/firmos/led/disk.py ledon '+trim(diskspec),nil,'',outs,errors);
    writeln('LEDON RESULT:',res,'  ',outs,'  ',errors);
    if res<>0 then begin
      exit(edb_ERROR);
    end else begin
      exit(edb_OK);
    end;
  finally
    process.Free;
  end;
end;

function TFOS_VM_VMADM_IMPL.VM_State(const vm_key: String; out state: string): TFRE_DB_Errortype;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res,i   : integer;
    machs   : TFOSStringArray;
    line_s  : string;
begin
  result := edb_ERROR;
  process := TFRE_Process.Create(nil);
  try
    if FRemote then begin
      process.ConfigureRemote_SSH_Mode(cremoteuser,cremotehost,cremotekeyfilename);
    end;
    res          := process.ExecutePiped(' vmadm list -o state uuid='+trim(vm_key),nil,'',outs,errors);
    if res<>0 then begin
      raise EFRE_DB_Exception.Create(edb_ERROR,'ERROR FETCHING VM STATE');
    end;
    GFRE_BT.SeperateString(outs,#10,machs);
    for i := 1 to high(machs) do begin
      line_s  := machs[i];
      state   := trim(line_s);
      exit(edb_OK);
    end;
    state:='NOT FOUND';
    exit(edb_NOT_FOUND);
  finally
    process.Free;
  end;
end;

procedure TFOS_VM_VMADM_IMPL.Finalize;
begin
  Free;
end;

procedure TFOS_VM_VMADM_IMPL.GetConstants(const guid: string; out vncport,vnchost,vmalias,domain:string);
var i     : integer;
    lguid : string;
begin
  vncport:= '';
  vmalias:= 'Unknown';
  vnchost:= '';
  lguid  := LowerCase(guid);
  domain := 'firmos';
  //for  i := 0 to high (cVNCPorts) do begin
  // writeln('vmadm update '+cVNCPorts[i].guid+' vnc_port='+cVNCPorts[i].vnc_port+' &');
  //end;
  for  i := 0 to high (cVNCPorts) do begin
   if lguid=cVNCPorts[i].guid then begin
     vncport :=cVNCPorts[i].vnc_port;
     vnchost :=cVNCPorts[i].vnc_host;
     vmalias :=cVNCPorts[i].vm_alias;
     domain  :=cVNCPorts[i].domain;
     break;
   end;
  end;
end;

procedure TFOS_VM_VMADM_IMPL.InitVMData;
begin
  VMLock.Acquire;
  try
    VM_ListMachines (vmdata);
  finally
    VMLock.Release;
  end;
end;

function TFOS_VM_VMADM_IMPL._PSParameter: TFRE_DB_StringArray;
begin
  result := TFRE_DB_StringArray.Create('-e','-o','zone,fname,pid,pmem,pcpu,pri,rss,vsz,zoneid,psr,pset');
end;

function TFOS_VM_VMADM_IMPL._ZAParameter: TFRE_DB_StringArray;
begin
  result := TFRE_DB_StringArray.Create('list','-p','-c');
end;

procedure TFOS_VM_VMADM_IMPL.ParsePS(const vm_list: IFRE_DB_Object; const plines: TStringList);
var i          : integer;
    line_s     : string;
    pid        : string;
    pscmd      : string;
    pmem,pcpu,
    pri,rss,
    vsz,zoneid,
    psr,pset   : String;
    zone,fname : string;

  procedure UpdateVMPS;
  var j      : integer;
      mprops : IFRE_DB_Object;
  begin
    for j := 0 to high(vm_list.Field('Machines').AsObjectArr) do begin
      mprops := vm_list.Field('Machines').AsObjectArr[j];
      if mprops.Field('key').AsString=zone then begin
        mprops.Field('PERFpmem').AsString   := pmem;
        mprops.Field('PERFpcpu').AsString   := pcpu;
        mprops.Field('PERFpri').AsString    := pri;
        mprops.Field('PERFrss').AsString    := rss;
        mprops.Field('PERFvsz').AsString    := vsz;
        mprops.Field('PERFzoneid').AsString := zoneid;
        mprops.Field('PERFpsr').AsString    := psr;
        mprops.Field('PERFpset').AsString   := pset;
        break;
      end;
    end;
  end;

begin
  for i := 0 to plines.count-1 do begin
    line_s := trim (plines[i]);
    if line_s='' then continue;
//    writeln('-->',line_s,'<--');
    zone     := trim(GFRE_BT.SepLeft(line_s,' ')); line_s := trim(GFRE_BT.SepRight(line_s,' '));
    fname    := trim(GFRE_BT.SepLeft(line_s,' ')); line_s := trim(GFRE_BT.SepRight(line_s,' '));
    if (zone<>'ZONE') and (Pos('qemu-sys',fname)=1) then begin
      pid      := trim(GFRE_BT.SepLeft(line_s,' ')); line_s := trim(GFRE_BT.SepRight(line_s,' '));
      pmem     := trim(GFRE_BT.SepLeft(line_s,' ')); line_s := trim(GFRE_BT.SepRight(line_s,' '));
      pcpu     := trim(GFRE_BT.SepLeft(line_s,' ')); line_s := trim(GFRE_BT.SepRight(line_s,' '));
      pri      := trim(GFRE_BT.SepLeft(line_s,' ')); line_s := trim(GFRE_BT.SepRight(line_s,' '));
      rss      := trim(GFRE_BT.SepLeft(line_s,' ')); line_s := trim(GFRE_BT.SepRight(line_s,' '));
      vsz      := trim(GFRE_BT.SepLeft(line_s,' ')); line_s := trim(GFRE_BT.SepRight(line_s,' '));
      zoneid   := trim(GFRE_BT.SepLeft(line_s,' ')); line_s := trim(GFRE_BT.SepRight(line_s,' '));
      psr      := trim(GFRE_BT.SepLeft(line_s,' ')); line_s := trim(GFRE_BT.SepRight(line_s,' '));
      pset     := trim(GFRE_BT.SepLeft(line_s,' ')); line_s := trim(GFRE_BT.SepRight(line_s,' '));
      //writeln('ZONE:',zone,' PID:',pid,'  ',pmem,'  ',pcpu);
      VMLock.Acquire;
      try
        UpdateVMPS;
      finally
        VMLock.Release;
      end;
    end;
  end;
end;

procedure TFOS_VM_VMADM_IMPL.ParseZA(const vm_list: IFRE_DB_Object; const plines: TStringList);
var process    : TFRE_Process;
    outs       : string;
    errors     : string;
    res        : integer;
    line_e     : TFOSStringArray;
    i          : integer;
    m_entry    : IFRE_DB_Object;
    m_props    : IFRE_DB_Object;
    vncobject  : IFRE_DB_Object;
    vncport    : string;
    vnchost    : string;
    vm_alias   : string;
    domain     : string;

    function   _find_or_add_machineobject (const mkey:string) : IFRE_DB_Object;
    var i      : integer;
    begin
      result := nil;
      for i:= 0 to vm_list.Field('Machines').ValueCount-1 do begin
        if vm_list.Field('Machines').AsObjectItem[i].Field('key').asstring=mkey then begin
          result := vm_list.Field('Machines').AsObjectItem[i];
          exit;
        end;
      end;
      result := GFRE_DBI.NewObject;
      result.Field('key').asstring:=mkey;
      vm_list.Field('Machines').AddObject(result);
    end;

begin
//  writeln('PARSE ZA');
  for i := 0 to plines.Count-1 do begin
    if trim(plines[i])='' then continue;    // skipping empty lines
    GFRE_BT.SeperateString(plines[i],':',line_e);
    if line_e[0]='0' then continue;         // skipping global
//    writeln('LINE:',plines[i]);
    m_entry := _find_or_add_machineobject(line_e[1]);
//    writeln('DBG I:',i,' mkey: ',m_entry.Field('key').AsString);
    m_entry.Field('MBrand').AsString     := line_e[5];
    if m_entry.Field('MBrand').AsString  ='joyent' then begin
      m_entry.Field('MType').AsString    := 'OS';
    end else begin
      m_entry.Field('MType').AsString    := 'KVM';
    end;
//    m_entry.Field('MName').AsString      := '';
    m_entry.Field('State').AsString      := line_e[2];
//      m_entry.Field('MPid').AsString       := line_e[5];
//      m_entry.Field('MCPUCAP').AsString    := line_e[6];
//      m_entry.Field('MCPUSHARES').AsString := line_e[7];
//      m_entry.Field('MCPUQUOTA').AsString  := line_e[8];
//      m_entry.Field('MRam').AsString       := line_e[9];
//      m_entry.Field('MVCpus').AsString     := line_e[10];
//      m_entry.Field('MVIOPrio').AsString   := line_e[11];
//      m_entry.Field('MPool').AsString      := line_e[12];
//     writeln('VM VNC:',vncport+' ALIAS:',vm_alias);
    if m_entry.FieldExists('VM_Info')=false then begin
      GetConstants(m_entry.Field('key').AsString,vncport,vnchost,vm_alias,domain);
      m_props := GFRE_DBI.NewObject;
      m_props.Field('INFO_EXISTS').AsBoolean :=true;
      vncobject := GFRE_DBI.NewObject;
      vncobject.Field('PORT').asstring := vncport;
      vncobject.Field('HOST').asstring := vnchost;
      m_entry.Field('MName').AsString   := vm_alias;
      m_entry.Field('domain').AsString   := domain;
      m_props.Field('VNC').AsObject := vncobject;
      m_entry.Field('VM_Info').AsObject := m_props;
    end;
    if UpperCase(m_entry.Field('State').AsString)<>'RUNNING' then begin
   //   writeln('CLEARING MPROPS');
      m_entry.DeleteField('PERFpmem');
      m_entry.DeleteField('PERFpcpu');
      m_entry.DeleteField('PERFpri');
      m_entry.DeleteField('PERFrss');
      m_entry.DeleteField('PERFvsz');
      m_entry.DeleteField('PERFzoneid');
      m_entry.DeleteField('PERFpsr');
      m_entry.DeleteField('PERFpset');
    end;
  end;
end;

constructor TFOS_VM_VMADM_IMPL.Create;
begin
  lines_ps := TStringList.Create;
  lines_ps.TextLineBreakStyle := tlbsLF;
  lines_ps.StrictDelimiter:=true;
  lines_ps.Delimiter:=#10;
  lines_za := TStringList.Create;
  lines_za.TextLineBreakStyle := tlbsLF;
  lines_za.StrictDelimiter:=true;
  lines_za.Delimiter:=#10;

  stream_ps:= TStringStream.Create('');
  stream_za:= TStringStream.Create('');

  vmdata   := GFRE_DBI.NewObject;
  GFRE_TF.Get_Lock(VMLock);
end;

destructor TFOS_VM_VMADM_IMPL.Destroy;
begin
  if assigned(FProcess_ps) then begin
    FProcess_ps.Terminate(99);
    FProcess_ps.WaitForAsyncExecution;
    FProcess_ps.Free;
    memoutstream_ps.Free;
    memerrstream_ps.free;
  end;
  if assigned(FProcess_za) then begin
    FProcess_za.Terminate(99);
    FProcess_za.WaitForAsyncExecution;
    FProcess_za.Free;
    memoutstream_za.Free;
    memerrstream_za.free;
  end;
  stream_ps.Free;
  stream_za.Free;

  lines_za.Free;
  lines_ps.Free;
  VMLock.Finalize;
  vmdata.Finalize;
  inherited Destroy;
end;

function TFOS_VM_VMADM_IMPL.VM_EnableVMMonitor(const enable: boolean): TFRE_DB_Errortype;
begin
  if not enable then begin
    FProcess_ps.Terminate(0);
    FProcess_ps.WaitForAsyncExecution;
    FProcess_ps.Free;
    FProcess_ps:=nil;
    memoutstream_ps.Free;
    memerrstream_ps.Free;
    FProcess_ps.Free;

    FProcess_za.Terminate(0);
    FProcess_za.WaitForAsyncExecution;
    FProcess_za.Free;
    FProcess_za:=nil;
    memoutstream_za.Free;
    memerrstream_za.Free;
    FProcess_za.Free;

    Fenabled:=false;
  end else begin
    if FEnabled then exit;
    FEnabled:=true;
    InitVMData;

    FProcess_za      := TFRE_Process.Create(nil);
    memoutstream_za := TMemoryStream.Create;
    memerrstream_za := TMemoryStream.Create;
    FProcess_za.RegisterCallBacks(@ZAOutStreamCallBack,@ZAErrStreamCallBack);
    if (cremoteuser<>'') then begin
      FProcess_za.ConfigureRemote_SSH_Mode(cremoteuser,cremotehost,cremotekeyfilename);
    end;
    FProcess_za.EnableLoop(1000);
    FProcess_za.StartPipedStreamAsync('zoneadm',_ZAParameter,nil, memoutstream_za, memerrstream_za);

    FProcess_ps      := TFRE_Process.Create(nil);
    memoutstream_ps := TMemoryStream.Create;
    memerrstream_ps := TMemoryStream.Create;
    FProcess_ps.RegisterCallBacks(@PSOutStreamCallBack,@PSErrStreamCallBack);
    if (cremoteuser<>'') then begin
      FProcess_ps.ConfigureRemote_SSH_Mode(cremoteuser,cremotehost,cremotekeyfilename);
    end;
    FProcess_ps.EnableLoop(1000);
    FProcess_ps.StartPipedStreamAsync('ps',_PSParameter,nil, memoutstream_ps, memerrstream_ps);
  end;
end;

function TFOS_VM_VMADM_IMPL.Get_VM_Data: IFRE_DB_Object;
begin
  VMLock.Acquire;
  try
    result := vmdata.CloneToNewObject;
  finally
    VMLock.Release;
  end;
end;

end.

