program foscmd;

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


uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,Sysutils,BaseUnix,FOS_DEFAULT_IMPLEMENTATION,FOS_BASIS_TOOLS,FOS_TOOL_INTERFACES,FRE_PROCESS,iostream,fre_zfs,FRE_DB_INTERFACE,
  fre_db_core,FRE_SYSTEM,fre_configuration,fre_testcase,fre_dbbase;

  {$I fos_version_helper.inc}

var
  mode       : string;
  ds         : string;
  job        : string;
  totalsize  : Int64;
  sshcommand : string;

  procedure ZFSReceive(const dataset:string; const compressed:boolean);
  var
    process      : TFRE_Process;
    process2     : TFRE_Process;
    res          : integer;
    stdinstream  : TIOStream;
    stdoutstream : TIOStream;
    stderrstream : TIOStream;
    progress     : TFRE_DB_JobProgress;

  begin
    stdinstream  := TIOStream.Create(iosInput);
    stdoutstream := TIOStream.Create(iosOutPut);
    stderrstream := TIOStream.Create(iosError);

    progress     := TFRE_DB_JobProgress.CreateForDB;
    progress.SetJobID(job);
    progress.SetTotalOutbytes(totalsize);

    try
      if compressed then begin
        process   := TFRE_Process.Create(nil);
        process2  := TFRE_Process.Create(nil);
        process.PreparePipedStreamAsync('bzcat',nil);
        process2.PreparePipedStreamAsync('zfs',TFRE_DB_StringArray.Create('recv','-u','-F',ds)) ;
        process.SetStreams(StdInstream,process2.Input,stderrstream);
        process2.SetStreams(nil,stdoutstream,stderrstream);
        process2.RegisterProgressCallback(@progress.ProgressCallback);
        process.StartAsync;
        process2.StartAsync;
        process.WaitForAsyncExecution;
        process2.CloseINput;
        res       := process2.WaitForAsyncExecution;
      end else begin
        process   := TFRE_Process.Create(nil);
        process.PreparePipedStreamAsync('zfs',TFRE_DB_StringArray.Create('recv','-u','-F',ds));
        process.SetStreams(StdInstream,StdOutStream,stderrstream);
        process.RegisterProgressCallback(@progress.ProgressCallback);
        process.StartAsync;
        res      := process.WaitForAsyncExecution;
      end;
    finally
      if assigned(process) then process.Free;
      if assigned(process2) then process2.Free;
      progress.Finalize;
    end;
  end;

  procedure ZFSSend(const dataset:string; const compressed:boolean);
  var
    process      : TFRE_Process;
    process2     : TFRE_Process;
    res          : integer;
    stdinstream  : TIOStream;
    stdoutstream : TIOStream;
    stderrstream : TIOStream;
    progress     : TFRE_DB_JobProgress;
    zfsparams    : shortstring;
    targetds     : shortstring;
    targetcmd    : shortstring;

  begin
    stdinstream  := TIOStream.Create(iosInput);
    stdoutstream := TIOStream.Create(iosOutPut);
    stderrstream := TIOStream.Create(iosError);

    progress     := TFRE_DB_JobProgress.CreateForDB;
    progress.SetJobID(job);
    progress.SetTotalOutbytes(totalsize);

    zfsparams    := GFRE_BT.SplitString(ds,'*');
    targetds     := ds;

    if compressed then
      targetcmd := 'RECEIVEBZ'
    else
      targetcmd := 'RECEIVE';

    targetcmd    := targetcmd+'%'+targetds+'%R'+job+'%'+inttostr(totalsize)+LineEnding;
    stdoutstream.WriteBuffer(targetcmd[1],byte(targetcmd[0]));

//    writeln(Stderr,'SWL: ZFSPARAMS ',zfsparams);
//    writeln(Stderr,'SWL: targetds ',targetds);
//    writeln(Stderr,'SWL: targetcmd ',targetcmd);

    try
      if compressed then begin
        process   := TFRE_Process.Create(nil);
        process2  := TFRE_Process.Create(nil);
        process.PreparePipedStreamAsync('zfs send '+zfsparams,nil);
        process2.PreparePipedStreamAsync('bzcat',nil) ;
        process.SetStreams(StdInstream,process2.Input,stderrstream);
        process2.SetStreams(nil,stdoutstream,stderrstream);
        process2.RegisterProgressCallback(@progress.ProgressCallback);
        process.StartAsync;
        process2.StartAsync;
        process.WaitForAsyncExecution;
        process2.CloseINput;
        res       := process2.WaitForAsyncExecution;
      end else begin
        process   := TFRE_Process.Create(nil);
        process.PreparePipedStreamAsync('zfs send '+zfsparams,nil);
        process.SetStreams(StdInstream,StdOutStream,stderrstream);
        process.RegisterProgressCallback(@progress.ProgressCallback);
        process.StartAsync;
        res      := process.WaitForAsyncExecution;
      end;
    finally
      if assigned(process) then process.Free;
      if assigned(process2) then process2.Free;
      progress.Finalize;
    end;
  end;

  function ZFSDSExists(const dataset:string) : integer;
  var proc : TFRE_Process;
      stdinstream  : TIOStream;
      stdoutstream : TIOStream;
      stderrstream : TIOStream;
  begin
    stdinstream  := TIOStream.Create(iosInput);
    stdoutstream := TIOStream.Create(iosOutPut);
    stderrstream := TIOStream.Create(iosError);
    proc := TFRE_Process.Create(nil);
    try
      result  := proc.ExecutePipedStream('zfs',TFRE_DB_StringArray.Create('list','-H','-o','name',dataset),stdinstream,stdoutstream,stderrstream);
    finally
      if assigned(proc) then proc.Free;
    end;
  end;

  function ZFSGetSnapshots(const dataset:string) : integer;
  var proc : TFRE_Process;
      stdinstream  : TIOStream;
      stdoutstream : TIOStream;
      stderrstream : TIOStream;
  begin
    stdinstream  := TIOStream.Create(iosInput);
    stdoutstream := TIOStream.Create(iosOutPut);
    stderrstream := TIOStream.Create(iosError);
    proc := TFRE_Process.Create(nil);
    try
      result  := proc.ExecutePipedStream('zfs',TFRE_DB_StringArray.Create('list','-r','-H'b,'-t','snapshot','-o','name,creation,used',dataset),stdinstream,stdoutstream,stderrstream);
    finally
      if assigned(proc) then proc.Free;
    end;
  end;

  function EchoTest : integer;
  var process : TFRE_Process;
      stdinstream  : TIOStream;
      stdoutstream : TIOStream;
      stderrstream : TIOStream;
      progress     : TFRE_DB_JobProgress;
  begin
    stdinstream  := TIOStream.Create(iosInput);
    stdoutstream := TIOStream.Create(iosOutPut);
    stderrstream := TIOStream.Create(iosError);
    progress     := TFRE_DB_JobProgress.CreateForDB;
    progress.SetJobID(job);
    progress.SetTotalOutbytes(totalsize);

    process      := TFRE_Process.Create(nil);
    try
      process.PreparePipedStreamAsync('cat',nil);
      process.SetStreams(StdInstream,StdOutStream,stderrstream);
      process.RegisterProgressCallback(@progress.ProgressCallback);
      process.StartAsync;
      result  := process.WaitForAsyncExecution;
    finally
      process.Free;
      progress.Finalize;
    end;
  end;

  function SendTest : integer;
  var process : TFRE_Process;
      stdinstream  : TIOStream;
      stdoutstream : TIOStream;
      stderrstream : TIOStream;
      progress     : TFRE_DB_JobProgress;
      targetcmd    : ShortString;
  begin
    stdinstream  := TIOStream.Create(iosInput);
    stdoutstream := TIOStream.Create(iosOutPut);
    stderrstream := TIOStream.Create(iosError);
    progress     := TFRE_DB_JobProgress.CreateForDB;
    progress.SetJobID(job);
    progress.SetTotalOutbytes(totalsize);
    targetcmd    := 'ECHOTEST%DS%E'+job+'%'+inttostr(totalsize)+LineEnding;
    stdoutstream.WriteBuffer(targetcmd[1],byte(targetcmd[0]));
    process      := TFRE_Process.Create(nil);
    try
      process.PreparePipedStreamAsync('cat',TFRE_DB_StringArray.Create(ds));   // send testfile
      process.SetStreams(StdInstream,StdOutStream,stderrstream);
      process.RegisterProgressCallback(@progress.ProgressCallback);
      process.StartAsync;
      result  := process.WaitForAsyncExecution;
    finally
      process.Free;
      progress.Finalize;
    end;
  end;


  procedure GetInlineParams;
  var inlineparams : string;
      stdinstream  : TIOStream;
      c            : char;
  begin
    inlineparams := '';
    stdinstream  := TIOStream.Create(iosInput);
    try
      while c<>LineEnding do
        begin
          c:=Char(stdinstream.ReadByte);
          if c<>LineEnding then
            inlineparams := inlineparams+c;
        end;
    finally
      stdinstream.Free;
    end;
    mode      := GFRE_BT.SplitString(inlineparams,'%');
    ds        := GFRE_BT.SplitString(inlineparams,'%');
    job       := GFRE_BT.SplitString(inlineparams,'%');
    totalsize := StrtoInt64Def(inlineparams,0);
//    writeln('SWL:',mode,',', ds, ',', job, ',', totalsize);
  end;



begin
 InitMinimal(false);
 GFRE_DBI.Initialize_Extension_Objects;
 fre_dbbase.Register_DB_Extensions;
 fre_testcase.Register_DB_Extensions;

 Initialize_Read_FRE_CFG_Parameter;
 job       := '0';
 totalsize := 0;

 sshcommand := GetEnvironmentVariable('SSH_ORIGINAL_COMMAND');
 if length(sshcommand)>0 then begin
   GFRE_BT.SplitString(sshcommand,' ');
   mode   := GFRE_BT.SplitString(sshcommand,' ');
   ds     := sshcommand;
 end else begin
   if Paramcount>=2 then
     begin
       mode      := uppercase(ParamStr(1));
       ds        := ParamStr(2);
       if ParamCount>=3 then
         job       := ParamStr(3);
       if ParamCount=4 then
         totalsize := StrtoInt64Def(ParamStr(4),0);
     end
   else
     begin
       GetInlineParams;
     end;
 end;
// writeln('SWL: MODE [',mode,']');

 case mode of
  'RECEIVE': begin
    ZFSReceive(ds,false);
  end;
  'RECEIVEBZ':begin
    ZFSReceive(ds,true);
   end;
  'DSEXISTS':begin
    halt(ZFSDSExists(ds));
   end;
  'GETSNAPSHOTS':begin
    halt(ZFSGetSnapshots(ds));
   end;
  'ECHOTEST': begin
     halt(EchoTest);
   end;
  'SENDTEST': begin
     halt(SendTest);
   end;
  'SEND': begin
     ZFSSend(ds,false);
   end;
  'SENDBZ': begin
     ZFSSend(ds,true);
   end;
  else begin
   writeln(GFOS_VHELP_GET_VERSION_STRING);
   writeln('Usage: foscmd RECEIVE | RECEIVEBZ | SEND | SENDBZ | DSEXISTS | ECHOTEST | SENDTEST | GETSNAPSHOTS <dataset> <jobid> <totalsize>');
   halt(99);
  end;
 end;
end.

