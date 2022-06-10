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
  public
    procedure SendSerialized(const RnlChannel: TRNLPeerChannel); virtual; abstract;
    class function TryDeserialize(const RnlMessage: TRNLMessage): TMessage;
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
    class function TryDeserialize(const RnlMessage: TRNLMessage): TMessagePlayerState;
    procedure SendSerialized(const RnlChannel: TRNLPeerChannel); override;
  end;

  TMessagePlayerShoot = class(TMessage)
  public
    { Player that got hit.
      Don't confuse PlayerId (each player has unique id, known to all clients and server)
      with PeerId that is specific to given connection client<->server. }
    PlayerId: Integer;
    class function TryDeserialize(const RnlMessage: TRNLMessage): TMessagePlayerShoot;
    procedure SendSerialized(const RnlChannel: TRNLPeerChannel); override;
  end;

  TMessageChat = class(TMessage)
  public
    Text: String;
    { Note: this is just String, deserialize it as a last resort.
      TODO: chat message should have unique id. }
    class function TryDeserialize(const RnlMessage: TRNLMessage): TMessageChat;
    procedure SendSerialized(const RnlChannel: TRNLPeerChannel); override;
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

{ TMessage ------------------------------------------------------------------- }

class function TMessage.TryDeserialize(const RnlMessage: TRNLMessage): TMessage;
begin
  { Try various TMessage descendants.
    Try TMessageChat as last, as it always returns non-nil. }
  Result := TMessagePlayerState.TryDeserialize(RnlMessage);
  if Result <> nil then Exit;
  Result := TMessagePlayerShoot.TryDeserialize(RnlMessage);
  if Result <> nil then Exit;
  // add more messages here...
  Result := TMessageChat.TryDeserialize(RnlMessage);
  if Result <> nil then Exit;
end;

{ TMessagePlayerState -------------------------------------------------------- }

const
  IdPlayerState = 8217832;
  IdPlayerShoot = 2376;

type
  TRecPlayerState = packed record
    MessageId: Int32;
    Position, PositionDelta: TVector3;
    Rotation, RotationDelta: Single;
    Life: Byte;
  end;
  PRecPlayerState = ^TRecPlayerState;

class function TMessagePlayerState.TryDeserialize(const RnlMessage: TRNLMessage): TMessagePlayerState;
var
  Rec: TRecPlayerState;
begin
  Result := nil;
  if RnlMessage.DataLength = SizeOf(TRecPlayerState) then
  begin
    Rec := PRecPlayerState(RnlMessage.Data)^;
    if Rec.MessageId = IdPlayerState then
    begin
      Result := TMessagePlayerState.Create;
      Result.Position      := Rec.Position;
      Result.PositionDelta := Rec.PositionDelta;
      Result.Rotation      := Rec.Rotation;
      Result.RotationDelta := Rec.RotationDelta;
      Result.Life          := Rec.Life;
    end;
  end
end;

procedure TMessagePlayerState.SendSerialized(const RnlChannel: TRNLPeerChannel);
var
  Rec: TRecPlayerState;
begin
  Rec.MessageId := IdPlayerState;
  Rec.Position      := Position;
  Rec.PositionDelta := PositionDelta;
  Rec.Rotation      := Rotation;
  Rec.RotationDelta := RotationDelta;
  Rec.Life          := Life;
  RnlChannel.SendMessageData(@Rec, SizeOf(Rec));
end;

{ TMessagePlayerShoot -------------------------------------------------------- }

type
  TRecPlayerShoot = packed record
    MessageId: Int32;
    PlayerId: Int32;
  end;
  PRecPlayerShoot = ^TRecPlayerShoot;

class function TMessagePlayerShoot.TryDeserialize(const RnlMessage: TRNLMessage): TMessagePlayerShoot;
var
  Rec: TRecPlayerShoot;
begin
  Result := nil;
  if RnlMessage.DataLength = SizeOf(TRecPlayerShoot) then
  begin
    Rec := PRecPlayerShoot(RnlMessage.Data)^;
    if Rec.MessageId = IdPlayerShoot then
    begin
      Result := TMessagePlayerShoot.Create;
      Result.PlayerId := Rec.PlayerId;
    end;
  end
end;

procedure TMessagePlayerShoot.SendSerialized(const RnlChannel: TRNLPeerChannel);
var
  Rec: TRecPlayerShoot;
begin
  Rec.MessageId := IdPlayerShoot;
  Rec.PlayerId := PlayerId;
  RnlChannel.SendMessageData(@Rec, SizeOf(Rec));
end;

{ TMessageChat --------------------------------------------------------------- }

class function TMessageChat.TryDeserialize(const RnlMessage: TRNLMessage): TMessageChat;
begin
  Result := TMessageChat.Create;
  Result.Text := RnlMessage.AsString;
end;

procedure TMessageChat.SendSerialized(const RnlChannel: TRNLPeerChannel);
begin
  RnlChannel.SendMessageString(Text);
end;

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
  Send: Boolean;
begin
  ToSendCs.Acquire;
  try
    if ToSend.Count <> 0 then
    begin
      for M in ToSend do
      begin
        //ConsoleOutput(Format('Sending "%s" to %d', [S, M.RecipientPeerId]));

        for Peer in Host.Peers do
        begin
          if M.Broadcast then
            Send := M.RecipientPeerId <> Peer.LocalPeerID
          else
            Send := M.RecipientPeerId = Peer.LocalPeerID;
          if Send then
            M.SendSerialized(Peer.Channels[0]);
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
