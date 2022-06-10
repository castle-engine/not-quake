{ Common networking (RNL) code for client and server. }
unit NetworkCommon;

interface

uses SysUtils, Classes, SyncObjs, Generics.Collections,
  RNL,
  CastleLog, CastleApplicationProperties, CastleVectors;

{ Call these from any thread (main or not) to report something. }
procedure ConsoleOutput(const s:string);
procedure LogThreadException(const aThreadName:string;const aException:TObject);

{ Call this from main thread to display logs. }
procedure FlushConsoleOutput;

type
  TMessage = class
  private
    RecipientPeerId: Integer; //< Only used for messages scheduled to send from this process
    Broadcast: Boolean; //< Only used for messages scheduled to send from this process
  end;

  // TMessagePlayerJoin = class
  //   Nick: String;
  //   PeerId: Integer;
  // end;

  TMessagePlayerState = class(TMessage)
  public
    Position, PositionDelta: TVector3;
    Rotation, RotationDelta: Single;
    Life: Byte;
  end;

  TMessagePlayerShoot = class(TMessage)
  public
    PeerId: Integer;
  end;

  TMessageChat = class(TMessage)
  public
    Text: String;
  end;

  TMessageList = {$ifdef FPC}specialize{$endif} TObjectList<TMessage>;

  TNetworkingThread = class(TThread)
  private
    ToSendCs: TCriticalSection;
    ToSend: TMessageList;
  protected
    { Send scheduled messages.
      Call this often in this thread. }
    procedure ProcessMessagesToSend(const Host: TRNLHost);
    { Call in this thread when new message is received.
      M becomes owned by this object. }
    procedure ProcessMessageReceived(const M: TMessage);
  public
    { Protect access to Received using this critical section. }
    ReceivedCs: TCriticalSection;
    Received: TMessageList;
    constructor Create(const aCreateSuspended: Boolean);
    destructor Destroy; override;

    { Send a message to remote. M becomes owned by this object.
      Can be used from other thread than this.

      When Broadcast then it means it send to all peers.
      The PeerId in this case indicates the peer to *avoid* sending it to, and can be -1.

      Note that "all peers" for clients is "just server".
      So this does apply to actual peers to which we talk over the network,
      not for all participants in the game.
      The server will broadcast the message further if needed.
    }
    procedure SendMessage(const M: TMessage; const Broadcast: Boolean; const RecipientPeerId: Integer);
  end;

var
  OnNetworkLog: TLogEvent;

implementation

type
  TConsoleOutputQueue={$ifdef FPC}specialize{$endif} TRNLQueue<string>;

var
  ConsoleOutputQueue:TConsoleOutputQueue=nil;
  ConsoleOutputLock:TCriticalSection=nil;

procedure ConsoleOutput(const s:string);
begin
  if (ConsoleOutputLock = nil) or
     (ConsoleOutputQueue = nil) then
  begin
    WritelnWarning('ConsoleOutput called after console thread resources freed. Message: ' + S);
    Exit;
  end;

  ConsoleOutputLock.Acquire;
  try
    ConsoleOutputQueue.Enqueue(s);
  finally
    ConsoleOutputLock.Release;
  end;
end;

procedure FlushConsoleOutput;
var
  s:string;
begin
  ConsoleOutputLock.Acquire;
  try
    while ConsoleOutputQueue.Dequeue(s) do
    begin
      WritelnLog('Network', s);
      if Assigned(OnNetworkLog) then
        OnNetworkLog(S);
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

{ TNetworkingThread ---------------------------------------------------------- }

constructor TNetworkingThread.Create(const aCreateSuspended: Boolean);
begin
  ToSendCs := TCriticalSection.Create; // protects ToSend
  ToSend := TMessageList.Create(true);
  ReceivedCs := TCriticalSection.Create; // protects Received
  Received := TMessageList.Create(true);
  inherited Create(aCreateSuspended);
end;

destructor TNetworkingThread.Destroy;
begin
  inherited;
  FreeAndNil(ToSend);
  FreeAndNil(ToSendCs);
  FreeAndNil(Received);
  FreeAndNil(ReceivedCs);
end;

procedure TNetworkingThread.SendMessage(const M: TMessage; const Broadcast: Boolean; const RecipientPeerId: Integer);
begin
  ToSendCs.Acquire;
  try
    M.RecipientPeerId := RecipientPeerId;
    M.Broadcast := Broadcast;
    ToSend.Add(M);
  finally ToSendCs.Release end;
end;

procedure TNetworkingThread.ProcessMessagesToSend(const Host: TRNLHost);
var
  Peer: TRNLPeer;
  M: TMessage;
  S: String;
  Send: Boolean;
begin
  ToSendCs.Acquire;
  try
    if ToSend.Count <> 0 then
    begin
      for M in ToSend do
      begin
        if M is TMessageChat then
          S := TMessageChat(M).Text
        else
          S := 'Not supported message: ' + M.ClassName;

        //ConsoleOutput(Format('Sending "%s" to %d', [S, M.RecipientPeerId]));

        for Peer in Host.Peers do
        begin
          if M.Broadcast then
            Send := M.RecipientPeerId <> Peer.LocalPeerID
          else
            Send := M.RecipientPeerId = Peer.LocalPeerID;
          if Send then
            Peer.Channels[0].SendMessageString(S);
        end;
      end;
      Host.Flush;
      ToSend.Clear;
    end;
  finally ToSendCs.Release end;
end;

procedure TNetworkingThread.ProcessMessageReceived(const M: TMessage);
begin
  ReceivedCs.Acquire;
  try
    Received.Add(M);
  finally ReceivedCs.Release end;
end;

{ initialization / finalization ---------------------------------------------- }

initialization
  ConsoleOutputLock:=TCriticalSection.Create;
  ConsoleOutputQueue:=TConsoleOutputQueue.Create;
finalization
  if ConsoleOutputQueue <> nil then
  begin
    FlushConsoleOutput;
    FreeAndNil(ConsoleOutputQueue);
  end;
  FreeAndNil(ConsoleOutputLock);
end.
