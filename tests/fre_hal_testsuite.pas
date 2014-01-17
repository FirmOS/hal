unit fre_hal_testsuite;

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
  FOS_TOOL_INTERFACES,FRE_DBBASE,FRE_PROCESS, FRE_HAL_DISK,FRE_SYSTEM;
  // fre_testmethod, FRE_ZFS,
  //fre_testcase, fre_do_safejob,FRE_SYSTEM,  FRE_DBMONITORING,
  //FRE_OPENVPN,fre_alert,fre_diskshelf;

const

  cremoteuser               = 'root';
  cremotehost               = '10.1.0.121';

type



  { TFRE_HAL_Tester_Tests }

  TFRE_HAL_Tester_Tests = class (TTestCase)
  private
    procedure GetPools;
    procedure CreateDiskPool;
  published
    procedure GetPoolConfiguration;
  end;



implementation



function GetRemoteKeyFilename: string;
begin
  result := SetDirSeparators(cFRE_SERVER_DEFAULT_DIR+'/ssl/user/id_rsa');         // must be authorized for remoteuser on remotehost
end;


{ TFRE_HAL_Tester_Tests }

procedure TFRE_HAL_Tester_Tests.GetPoolConfiguration;
var disko  : TFRE_HAL_DISK;
    error  : string;
    res    : integer;
    obj    : IFRE_DB_Object;
begin
  disko     := TFRE_HAL_DISK.create;
  try
    obj       := disko.FetchPoolConfiguration('filetest',cremoteuser,cremotehost,GetRemoteKeyFilename);
    writeln   (obj.DumpToString());
    obj.Finalize;

  finally
    disko.Free;
  end;
end;

procedure TFRE_HAL_Tester_Tests.GetPools;
var disko  : TFRE_HAL_DISK;
    error  : string;
    res    : integer;
    obj    : IFRE_DB_Object;
begin
  disko     := TFRE_HAL_DISK.create;
  try
    obj       := disko.GetPools(cremoteuser,cremotehost,GetRemoteKeyFilename);
    writeln   (obj.DumpToString());
    obj.Finalize;
  finally
    disko.Free;
  end;
end;


procedure TFRE_HAL_Tester_Tests.CreateDiskPool;
var disko  : TFRE_HAL_DISK;
    error  : string;
    res    : integer;
    obj    : IFRE_DB_Object;
    obj2   : IFRE_DB_Object;
begin
  disko     := TFRE_HAL_DISK.create;
  try
    obj       := disko.FetchPoolConfiguration('zones',cremoteuser,cremotehost,GetRemoteKeyFilename);
    writeln   (obj.DumpToString());
    obj2      := disko.CreateDiskpool(obj.Field('data').asobject,cremoteuser,cremotehost,GetRemoteKeyFilename);
    writeln   (obj2.DumpToString());
    obj.Finalize;
    obj2.Finalize;
  finally
    disko.Free;
  end;
end;



initialization
  RegisterTest(TFRE_HAL_Tester_Tests);
end.

