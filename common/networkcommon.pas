{ Common networking (RNL) code for client and server. }
unit NetworkCommon;

interface

uses
  SysUtils,
  Classes,
  SyncObjs,
  RNL;

procedure ConsoleOutput(const s:string);
procedure FlushConsoleOutput;
procedure LogThreadException(const aThreadName:string;const aException:TObject);

type
  TNetworkingThread = class(TThread)
  private
    ToSendCs: TCriticalSection;
    ToSend: TStringList;
  protected
    { Send scheduled messages.
      Call this often in this thread. }
    procedure ProcessMessages(const Host: TRNLHost);
  public
    constructor Create(const aCreateSuspended: Boolean);
    destructor Destroy; override;
    { Send a message to remote.
      Can be used from other thread than this.
      PeerId = -1 means "broadcast to all peers". }
    procedure SendMessage(const PeerId: Integer; const S: String);
  end;

implementation

type
  TConsoleOutputThread=class(TThread)
  protected
    procedure Execute; override;
  end;

  TConsoleOutputQueue={$ifdef FPC}specialize{$endif} TRNLQueue<string>;

var
  ConsoleOutputQueue:TConsoleOutputQueue=nil;

  ConsoleOutputThread:TConsoleOutputThread=nil;

  ConsoleOutputLock:TCriticalSection=nil;

  ConsoleOutputEvent:TEvent=nil;

procedure ConsoleOutput(const s:string);
begin
 ConsoleOutputLock.Acquire;
 try
  ConsoleOutputQueue.Enqueue(s);
  ConsoleOutputEvent.SetEvent;
 finally
  ConsoleOutputLock.Release;
 end;
end;

procedure FlushConsoleOutput;
var s:string;
begin
 ConsoleOutputLock.Acquire;
 try
  while ConsoleOutputQueue.Dequeue(s) do begin
   writeln(s);
  end;
 finally
  ConsoleOutputLock.Release;
 end;
end;

procedure LogThreadException(const aThreadName:string;const aException:TObject);
{$if defined(fpc)}
var i:int32;
    Frames:PPointer;
    s:string;
begin
 if assigned(aException) then begin
  s:=aThreadName+' thread failed with exception class '+aException.ClassName+LineEnding;
  if aException is Exception then begin
   s:=s+'Exception Message: '+Exception(aException).Message+LineEnding;
  end;
  s:=s+LineEnding+'Stack trace:'+LineEnding+LineEnding;
  s:=s+BackTraceStrFunc(ExceptAddr);
  Frames:=ExceptFrames;
  for i:=0 to ExceptFrameCount-1 do begin
   s:=s+LineEnding+BackTraceStrFunc(Frames);
   inc(Frames);
  end;
  ConsoleOutput(s);
 end;
end;
{$else}
begin
 if assigned(aException) then begin
  if aException is Exception then begin
   ConsoleOutput(aThreadName+' thread failed with exception '+aException.ClassName+': '+Exception(aException).Message);
  end else begin
   ConsoleOutput(aThreadName+' thread failed with exception '+aException.ClassName);
  end;
 end;
end;
{$ifend}

procedure TConsoleOutputThread.Execute;
var s:string;
begin
{$ifndef fpc}
 NameThreadForDebugging('Console output');
{$endif}
 ConsoleOutput('Console output: Thread started');
 try
  while not Terminated do begin
   ConsoleOutputEvent.WaitFor(1000);
   while not Terminated do begin
    ConsoleOutputLock.Acquire;
    try
     if not ConsoleOutputQueue.Dequeue(s) then begin
      break;
     end;
    finally
     ConsoleOutputLock.Release;
    end;
    writeln(s);
   end;
  end;
 except
  on e:Exception do begin
   LogThreadException('Console output',e);
  end;
 end;
 ConsoleOutput('Console output: Thread stopped');
end;

{ TNetworkingThread ---------------------------------------------------------- }

constructor TNetworkingThread.Create(const aCreateSuspended: Boolean);
begin
  ToSendCs := TCriticalSection.Create; // protects ToSend
  ToSend := TStringList.Create;
  inherited Create(aCreateSuspended);
end;

destructor TNetworkingThread.Destroy;
begin
  inherited;
  FreeAndNil(ToSend);
  FreeAndNil(ToSendCs);
end;

procedure TNetworkingThread.SendMessage(const PeerId: Integer; const S: String);
begin
  ToSendCs.Acquire;
  try
    ToSend.AddObject(S, TObject(Pointer(PtrInt(PeerId))));
  finally ToSendCs.Release end;
end;

procedure TNetworkingThread.ProcessMessages(const Host: TRNLHost);
var
  Peer: TRNLPeer;
  S: String;
  PeerId, I: Integer;
begin
  ToSendCs.Acquire;
  try
    if ToSend.Count <> 0 then
    begin
      for I := 0 to ToSend.Count - 1 do
      begin
        S := ToSend[I];
        PeerId := PtrInt(ToSend.Objects[I]);
        ConsoleOutput(Format('Sending "%s" to %d', [S, PeerId]));
        for Peer in Host.Peers do
          if (PeerId = Peer.LocalPeerID) or
             (PeerId = -1) then
            Peer.Channels[0].SendMessageString(S);
      end;
      ToSend.Clear;
    end;
  finally ToSendCs.Release end;
end;

{ initialization / finalization ---------------------------------------------- }

initialization
  ConsoleOutputLock:=TCriticalSection.Create;
  ConsoleOutputEvent:=TEvent.Create(nil,false,false,'');
  ConsoleOutputQueue:=TConsoleOutputQueue.Create;
  ConsoleOutputThread:=TConsoleOutputThread.Create(false);
finalization
  if ConsoleOutputThread <> nil then
  begin
    ConsoleOutputThread.Terminate;
    ConsoleOutputEvent.SetEvent;
    ConsoleOutputThread.WaitFor;
    LogThreadException('Console output',ConsoleOutputThread.FatalException);
    FreeAndNil(ConsoleOutputThread);
  end;

  if ConsoleOutputQueue <> nil then
  begin
    FlushConsoleOutput;
    FreeAndNil(ConsoleOutputQueue);
  end;

  FreeAndNil(ConsoleOutputEvent);

  FreeAndNil(ConsoleOutputLock);
end.
