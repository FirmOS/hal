unit fre_do_safejob;

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

uses  Classes, SysUtils,FRE_SYSTEM,FOS_TOOL_INTERFACES,FRE_DB_INTERFACE,BaseUnix, FOS_TOOL_FACTORY,
      FRE_PROCESS,fre_testcase;//Unix,pthreads;

type


  // useradd -d /fre/monitoring  -m -c "Tester" fretest


  { TFRE_DB_TEST_JOB }

  TFRE_DB_TEST_JOB=class(TFRE_DB_ObjectEx)
  protected
    procedure  InternalSetup ; override;
  published
    function   IMI_Do_the_Job(const input:IFRE_DB_Object):IFRE_DB_Object;
  end;




procedure DO_SaveJob(const job_key:String);
procedure DO_SaveJob(const jobdbo : IFRE_DB_Object);


function TestIt:IFRE_DB_Object;

implementation

type

  { TTimeoutThread }

  TTimeoutThread=class(tthread)
  private
    FTimeout     : integer;
    FKillpid     : integer;
    FChildDone   : boolean;
    FTimedEvent  : IFOS_TE;
    fPidlockfile : string;

  public
    constructor Create(const timeout,killpid:integer;const pidlockfile:string);reintroduce;
    procedure   Execute ; override;
    procedure   WorkerDone;
    procedure   SignalBreak;
  end;

procedure LogSaveJob_INFO(const msg: String);
begin
  writeln(msg);
  GFRE_LOG.LogSystem(msg,flf_Kernel,fll_Notice);
end;

procedure LogSaveJob_ERROR(const msg: String);
begin
  writeln(msg);
  GFRE_LOG.LogSystem(msg,flf_Kernel,fll_Error);
end;

procedure LogSaveJob_WARN(const msg: String);
begin
  writeln(msg);
  GFRE_LOG.LogSystem(msg,flf_Kernel,fll_Warning);
end;

procedure LogSaveJob_ERROR(const msg: String ; params : array of const);
begin
  LogSaveJob_ERROR(format(msg,params));
end;

procedure LogSaveJob_WARN(const msg: String ; params : array of const);
begin
  LogSaveJob_WARN(format(msg,params));
end;

procedure LogSaveJob_INFO(const msg: String ; params : array of const);
begin
  LogSaveJob_INFO(format(msg,params));
end;



procedure AbortCritical(const msg:string);
begin
  LogSaveJob_ERROR(msg);
  halt(1);
end;

procedure AbortCritical(const msg:string;params: Array of const);
begin
  AbortCritical(format(msg,params));
end;

procedure _SetupSignals;
var ign , dummy: SigactionRec;
begin
  ign.sa_handler := SigActionHandler(SIG_IGN);
  ign.sa_flags   := 0;
  fpsigemptyset  (ign.sa_mask);
  FPsigaction    (SIGINT,  @ign, @dummy);
  FPSigaction    (SIGUSR1, @ign, @dummy);
  FPSigaction    (SIGHUP,  @ign, @dummy);
  FPSigaction    (SIGTERM, @ign, @dummy);
  FPSigaction    (SIGPIPE, @ign, @dummy); // IGNORE BROKEN PIPE
end;




procedure DO_SaveJob(const job_key: String);
var jobdbo      : IFRE_DB_Object;
    jobfile     : string;
begin
  if job_key='' then
    AbortCritical('NO JOBKEY SPECIFIED');
  jobfile := TFRE_DB_JOB.GetJobBaseFilename(jobStateImmediateStart,job_key)+'.dbo';
  if not FileExists(jobfile) then
    jobfile := TFRE_DB_JOB.GetJobBaseFilename(jobStateToRun,job_key)+'.dbo';
  if not FileExists(jobfile)      then AbortCritical('NO JOB DESC DBO FILE FOUND');
  try
    jobdbo                          := GFRE_DBI.CreateFromFile(jobfile);
  except on E: Exception do begin
    AbortCritical ('EXCEPTION ON LOADING JOB DESC DBO FILE :'+E.Message);
  end; end;
  DO_SaveJob(jobdbo);
end;

procedure DO_SaveJob(const jobdbo: IFRE_DB_Object);
var watchtimeout : integer;
    job_key      : string;
    to_thread    : TTimeoutThread;
    pidlockfile  : string;
    pidlockinfo  : string;
    job          : TFRE_DB_JOB;
begin
  writeln('SWL: DO SAFE JOB');
  _SetupSignals;
  if jobdbo.IsA(TFRE_DB_JOB,job) then
    begin
      job_key     := job.ObjectName;
      try
        writeln('SWL: DO SAFE JOB ',job_key);
        pidlockfile :=  cFRE_PID_LOCK_DIR+DirectorySeparator+'SJ_'+uppercase(GFRE_BT.Str2HexStr(job_key)+'.flck');
        if job.FieldExists('MAX_ALLOWED_TIME') then begin
          watchtimeout := job.Field('MAX_ALLOWED_TIME').AsInt32;
        end else begin
          watchtimeout := 1;
        end;
        if FileExists(pidlockfile) then begin
          AbortCritical('SAFEJOB START FAILED, JOB [%s] IS RUNNING!',[job_key]);
          halt(1);
        end else begin
           pidlockinfo := IntToStr(GetProcessID)+LineEnding+job_key+LineEnding+inttostr(watchtimeout);
           GFRE_BT.StringToFile(pidlockfile,pidlockinfo);
           job.SetPid(GetProcessID);
        end;
        to_thread := TTimeoutThread.Create(watchtimeout*1000,GetProcessID,pidlockfile);
        try
          jobdbo.Invoke('DO_THE_JOB',nil,nil,nil,nil);
        except on e:exception do begin
          LogSaveJob_ERROR('ERROR ON JOB [%s] : [%s]',[job_key,e.Message]);
        end;end;
        to_thread.WorkerDone;
        to_thread.SignalBreak;
      finally
        writeln('DELETE PIDLOCK FILE');
        DeleteFile(pidlockfile);
      end;
    end;
end;

function TestIt:IFRE_DB_Object;
var TESTDBO: IFRE_DB_Object;
    NEWDBO : IFRE_DB_Object;
begin
  TESTDBO := GFRE_DBI.NewObject;
  TESTDBO.Field('TEST_JOB').AsObject := TFRE_DB_TEST_JOB.create;
  NEWDBO := TESTDBO.CloneToNewObject();
  TESTDBO.Finalize;
 // NEWDBO.Field('TEST_JOB').AsObject.Invoke('DO_THE_JOB',nil);
  result := NEWDBO;
end;

{ TTimeoutThread }

constructor TTimeoutThread.Create(const timeout, killpid: integer; const pidlockfile: string);
begin
  FTimeout     := timeout;
  FKillpid     := killpid;
  fPidlockfile := pidlockfile;
  GFRE_TF.Get_TimedEvent(FTimedEvent);
  inherited Create(False);
end;

procedure TTimeoutThread.Execute;
begin
  FreeOnTerminate:=true;
  FTimedEvent.WaitFor(FTimeout);
  if not FChildDone then begin
    writeln('SAFE CHILD NOT DONE - KILLING');
    if not DeleteFile(fpidlockfile) then begin
      writeln('WARNING - COULD NOT DELETE PIDLOCKFILE');
    end;
    FpKill(FKillpid,SIGKILL);
  end;

end;

procedure TTimeoutThread.WorkerDone;
begin
  FChildDone := true;
end;

procedure TTimeoutThread.SignalBreak;
begin
  FTimedEvent.SetEvent;
end;

{ TFRE_DB_TEST_JOB }

procedure TFRE_DB_TEST_JOB.InternalSetup;
begin
  inherited InternalSetup;
  FNamedObject.ObjectName := 'TEST_JOB';
  Field('MAX_ALLOWED_TIME').AsInt32 := 10;
end;

function TFRE_DB_TEST_JOB.IMI_Do_the_Job(const input: IFRE_DB_Object): IFRE_DB_Object;
var i:integer;
begin
  writeln('IM AM DOING MY ANNOYING JOB ',FNamedObject.ObjectName);
  for i:=0 to 500 do begin
     sleep(10);
     if i mod 10 =0 then  writeln(i);
  end;
  writeln('I AM DONE DOING MY ANNOYING JOB ',FNamedObject.ObjectName);
end;

initialization
  //GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_JOB);
  //GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SCP_JOB);

end.

