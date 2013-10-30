unit fre_base_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,FRE_DB_INTERFACE,FOS_TOOL_INTERFACES,FRE_PROCESS,FRE_SYSTEM;

type

    { TFOS_PARSER_PROC }

  TFOS_PARSER_PROC=class
  private
     Fcremoteuser        : string;
     Fcremotehost        : string;
     Fcremotekeyfilename : string;
     FShellCmd           : string;
     FProcess            : TFRE_Process;
     FMemoutstream       : TMemoryStream;
     FMemerrstream       : TMemoryStream;
  protected
     helpline            : TStringlist;
     FData               : IFRE_DB_Object;
     FLine               : TStringlist;
     FLines              : TStringlist;
     FLock               : IFOS_LOCK;
     procedure   MyOutStreamCallBack (const stream:TStream); virtual;
     procedure   MyErrStreamCallBack (const stream:TStream); virtual;
     procedure   MySetup;virtual;
     procedure   MyFinalize;virtual;
     procedure   MyParseOnOnceFinished; virtual;
  public
     constructor Create (const remoteuser,remotekeyfile,remotehost,cmd : string);
     destructor  Destroy;override;
     procedure   Enable;
     procedure   Disable;
     procedure   Once;
     function    Get_Data_Object : IFRE_DB_Object;
     function    IsRunning:boolean;
     function    ExitStatus:integer;
  end;

implementation

{ TFOS_PARSER_PROC }

procedure TFOS_PARSER_PROC.MyOutStreamCallBack(const stream: TStream);
begin

end;

procedure TFOS_PARSER_PROC.MyErrStreamCallBack(const stream: TStream);
var st : TStringStream;
    sl : TStringlist;
    i  : integer;
begin
  stream.Position:=0;
  st := TStringStream.Create('');
  try
    if stream.Size>0 then
      begin
        st.CopyFrom(stream,stream.Size);
        stream.Size:=0;
        writeln('ERRSTREAMCALLBACK: ',ClassName);
        writeln('------------------------------');
        writeln(st.DataString);
        writeln('------------------------------');
      end;
  finally
    st.Free;
  end;
end;

procedure TFOS_PARSER_PROC.MySetup;
begin

end;

procedure TFOS_PARSER_PROC.MyFinalize;
begin

end;

procedure TFOS_PARSER_PROC.MyParseOnOnceFinished;
begin

end;

constructor TFOS_PARSER_PROC.Create(const remoteuser, remotekeyfile, remotehost, cmd: string);
begin
  FLine  := TStringList.Create;
  FLines := TStringList.Create;
  FLines.TextLineBreakStyle := tlbsLF;
  FLines.StrictDelimiter:=true;
  FLines.Delimiter:=#10;
  FLine.StrictDelimiter:=true;
  FLine.Delimiter:=',';
  GFRE_TF.Get_Lock(FLock);
  FData               := GFRE_DBI.NewObject;
  Fcremotehost        := remotehost;
  Fcremoteuser        := remoteuser;
  Fcremotekeyfilename := remotekeyfile;
  FShellCmd           := cmd;
  helpline            := TStringlist.Create;
  MySetup;
end;

destructor TFOS_PARSER_PROC.Destroy;
begin
  helpline.free;
  MyFinalize;
  FLine.Free;
  FLines.Free;
  FData.Finalize;
  FLock.Finalize;
end;

procedure TFOS_PARSER_PROC.Enable;
begin
  if assigned(FProcess) then
    Disable;
  FProcess      := TFRE_Process.Create(nil);
  Fmemoutstream := TMemoryStream.Create;
  Fmemerrstream := TMemoryStream.Create;
  FProcess.RegisterCallBacks(@MyOutStreamCallBack,@MyErrStreamCallBack);
  if (Fcremoteuser<>'') then
    FProcess.ConfigureRemote_SSH_Mode(Fcremoteuser,Fcremotehost,Fcremotekeyfilename);
  FProcess.StartPipedStreamAsync(FShellCmd,nil,nil, Fmemoutstream, Fmemerrstream);
end;

procedure TFOS_PARSER_PROC.Disable;
begin
  if assigned(FProcess) then
    begin
      FProcess.Terminate(0);
      FProcess.WaitForAsyncExecution;
      FProcess.Free;
      FProcess:=nil;
      Fmemoutstream.Free;
      Fmemerrstream.Free;
    end;
end;

procedure TFOS_PARSER_PROC.Once;
begin
  FProcess      := TFRE_Process.Create(nil);
  try
    Fmemoutstream := TMemoryStream.Create;
    Fmemerrstream := TMemoryStream.Create;
    FProcess.RegisterCallBacks(@MyOutStreamCallBack,@MyErrStreamCallBack);
    if (Fcremoteuser<>'') then
      FProcess.ConfigureRemote_SSH_Mode(Fcremoteuser,Fcremotehost,Fcremotekeyfilename);
    FProcess.ExecutePipedStream(FShellCmd,nil,nil, Fmemoutstream, Fmemerrstream);
    MyParseOnOnceFinished;
  finally
    FMemerrstream.Free;
    FMemoutstream.FRee;
    FProcess.Free;
  end;
end;

function TFOS_PARSER_PROC.Get_Data_Object: IFRE_DB_Object;
begin
  FLock.Acquire;
  try
    result := Fdata.CloneToNewObject();
  finally
    FLock.Release;
  end;
end;

function TFOS_PARSER_PROC.IsRunning: boolean;
begin
  if Assigned(FProcess) then
    result := FProcess.IsRunning
  else
    result := false;
end;

function TFOS_PARSER_PROC.ExitStatus: integer;
begin
  if Assigned(FProcess) then
    begin
      result := FProcess.ExitStatus;
      writeln('EXIT STATUS:',result);
    end
  else
    begin
      writeln('NO PROCESS ASSIGNED!');
      result := -1;
    end;
end;

end.

