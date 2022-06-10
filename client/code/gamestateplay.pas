{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Not Quake".

  "Not Quake" is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  "Not Quake" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Playing the game, while being connected to server. }
unit GameStatePlay;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleViewport, CastleTimeUtils, CastleCameras, CastleTransform, CastleFlashEffect;

type
  TStatePlay = class(TUIState)
  private
    { Components designed using CGE editor, loaded from the castle-user-interface file. }
    LabelFps: TCastleLabel;
    LabelNetworkLog: TCastleLabel;
    LabelControls: TCastleLabel;
    LabelPing: TCastleLabel;
    MainViewport: TCastleViewport;
    WalkNavigation: TCastleWalkNavigation;
    LifeBar: TCastleRectangleControl;
    RectDead: TCastleRectangleControl;

    WaitingForChat: Boolean;
    LastBroadcastState: TTimerResult;
    LastPing: TTimerResult;
    LastNickJoined: String;
    HitFlash: TCastleFlashEffect;

    procedure NetworkLog(const Message: String);
    function CameraPositionToSend(const Camera: TCastleCamera): TVector3;
    function CameraDirectionToSend(const Camera: TCastleCamera): TVector3;
  public
    PlayerNick: String; //< Set before starting state
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Pause; override;
    procedure Resume; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses SysUtils,
  CastleStringUtils, CastleLog, CastleUtils, CastleColors,
  GameClient, GameNetwork, GameStateMainMenu, GameStateInputChat, GamePlayers;

constructor TStatePlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestateplay.castle-user-interface';
end;

function TStatePlay.CameraPositionToSend(const Camera: TCastleCamera): TVector3;
begin
  Result := Camera.WorldTranslation - Vector3(0, TCastleWalkNavigation.DefaultPreferredHeight, 0); // calculate legs position
end;

function TStatePlay.CameraDirectionToSend(const Camera: TCastleCamera): TVector3;
begin
  Result := Camera.Direction;
  if not VectorsParallel(Result, Camera.GravityUp) then
    MakeVectorsOrthoOnTheirPlane(Result, Camera.GravityUp);
end;

procedure TStatePlay.Start;

  procedure SendJoin;
  var
    MJ: TMessagePlayerJoin;
    M: TMessageChat;
  begin
    // reuse TMessageChat to pass nick
    // TODO: this is poor to rely on 2 messages, and it depends on ordered channel - when reading we expect SJoinsSuffix in chat before TMessagePlayerJoin
    M := TMessageChat.Create;
    M.Text := PlayerNick + SJoinsSuffix;
    Client.SendMessage(M, true, -1);

    MJ := TMessagePlayerJoin.Create;
    MJ.PlayerId := LocalPlayer.PlayerId;
    Client.SendMessage(MJ, true, -1);
  end;

const
  RandomSpawn = 5;
begin
  inherited;
  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  LabelNetworkLog := DesignedComponent('LabelNetworkLog') as TCastleLabel;
  MainViewport := DesignedComponent('MainViewport') as TCastleViewport;
  WalkNavigation := DesignedComponent('WalkNavigation') as TCastleWalkNavigation;
  LabelControls := DesignedComponent('LabelControls') as TCastleLabel;
  LifeBar := DesignedComponent('LifeBar') as TCastleRectangleControl;
  RectDead := DesignedComponent('RectDead') as TCastleRectangleControl;
  LabelPing := DesignedComponent('LabelPing') as TCastleLabel;

  { avoid spawning everyone at same position }
  MainViewport.Camera.Translation := MainViewport.Camera.Translation + Vector3(
    RandomFloatRange(-RandomSpawn, RandomSpawn),
    0,
    RandomFloatRange(-RandomSpawn, RandomSpawn)
  );

  HitFlash := TCastleFlashEffect.Create(FreeAtStop);
  MainViewport.InsertFront(HitFlash);

  NetworkInitialize;
  OnNetworkLog := {$ifdef FPC}@{$endif} NetworkLog;

  // clear state, in case this state is started multiple times
  WaitingForChat := false;
  LastNickJoined := '';
  // TODO: LastBroadcastState := TTimerResult.Uninitialized;
  FillChar(LastBroadcastState, SizeOf(LastBroadcastState), 0);
  // TODO: LastPing := TTimerResult.Uninitialized;
  FillChar(LastPing, SizeOf(LastPing), 0);

  LocalPlayer := TPlayer.Create;
  LocalPlayer.PlayerId := Random(1000 * 1000 * 1000); // TODO: we just assume everyone will choose different id
  LocalPlayer.Nick := PlayerNick;
  LocalPlayer.Life := MaxLife;
  LocalPlayer.Position := CameraPositionToSend(MainViewport.Camera);
  LocalPlayer.Direction := CameraDirectionToSend(MainViewport.Camera);
  // TODO: rest of TPlayer fill

  Players := TPlayerList.Create;
  Players.Add(LocalPlayer);

  SendJoin;
end;

procedure TStatePlay.Stop;
begin
  // Note: don't bother sending TMessagePlayerDisconnect, server will do it automatically

  FreeAndNil(Players);
  LocalPlayer := nil; // freed alongside Players

  OnNetworkLog := nil;
  NetworkFinish;
  inherited;
end;

procedure TStatePlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  procedure SendPlayerState;
  var
    M: TMessagePlayerState;
  begin
    M := TMessagePlayerState.Create;
    M.PlayerId := LocalPlayer.PlayerId;
    M.Position := CameraPositionToSend(MainViewport.Camera);
    M.Direction := CameraDirectionToSend(MainViewport.Camera);
    M.Life := LocalPlayer.Life;
    Client.SendMessage(M, true, -1);
  end;

  procedure SendPing;
  var
    M: TMessagePing;
  begin
    M := TMessagePing.Create;
    M.ClientSendTime := Timer;
    Client.SendMessage(M, true, -1);
  end;

  procedure HandleChat(const M: TMessageChat);
  begin
    NetworkLog('Chat: ' + M.Text);
    if IsSuffix(SJoinsSuffix, M.Text, false) then
      LastNickJoined := SuffixRemove(SJoinsSuffix, M.Text, false);
  end;

  procedure HandlePlayerJoin(const M: TMessagePlayerJoin);
  var
    NewPlayer: TPlayer;
  begin
    NewPlayer := TPlayer.Create;
    NewPlayer.Nick := LastNickJoined;
    NewPlayer.PlayerId := M.PlayerId;
    NewPlayer.CreateTransform(MainViewport);
    NewPlayer.Life := MaxLife; // will be updated by TMessagePlayerState
    Players.Add(NewPlayer);
    WritelnLog('Joined player %s (%d)', [
      NewPlayer.Nick,
      NewPlayer.PlayerId
    ]);
  end;

  procedure HandlePlayerDisconnect(const M: TMessagePlayerDisconnect);
  var
    OldPlayer: TPlayer;
  begin
    OldPlayer := Players.FindPlayerId(M.PlayerId);
    if OldPlayer <> nil then
    begin
      NetworkLog(Format('%s disconnected', [OldPlayer.Nick]));
      Players.Remove(OldPlayer); // TODO remove by index, faster
    end else
      WritelnWarning('Cannot find disconnecting player id %d', [
        M.PlayerId
      ]);
  end;

  procedure HandlePlayerState(const M: TMessagePlayerState);
  var
    OldPlayer: TPlayer;
  begin
    OldPlayer := Players.FindPlayerId(M.PlayerId);
    if OldPlayer <> nil then
    begin
      OldPlayer.Position := M.Position;
      OldPlayer.PositionDelta := M.PositionDelta;
      OldPlayer.Direction := M.Direction;
      OldPlayer.DirectionDelta := M.DirectionDelta;
      OldPlayer.Life := M.Life;
      OldPlayer.UpdateTransform;
    end else
      WritelnWarning('Cannot find player id sending state %d', [
        M.PlayerId
      ]);
  end;

  procedure HandleHit(const M: TMessagePlayerHit);
  var
    ShooterPlayer, HitPlayer: TPlayer;
  begin
    HitPlayer := Players.FindPlayerId(M.PlayerId);
    if HitPlayer = nil then
    begin
      WritelnWarning('Cannot find player id being hit %d', [
        M.PlayerId
      ]);
      Exit;
    end;

    ShooterPlayer := Players.FindPlayerId(M.ShooterPlayerId);
    if ShooterPlayer = nil then
    begin
      WritelnWarning('Cannot find player id of shooter %d', [
        M.ShooterPlayerId
      ]);
      Exit;
    end;

    HitPlayer.Life := HitPlayer.Life - 1;
    if HitPlayer.Life = 0 then
      NetworkLog(Format('%s was killed by %s', [HitPlayer.Nick, ShooterPlayer.Nick]));
    HitPlayer.UpdateTransform;
    if HitPlayer = LocalPlayer then
    begin
      LifeBar.Exists := HitPlayer.Life > 0; // avoid WidthFraction = 0 meaning "ignore WidthFraction"
      LifeBar.WidthFraction := HitPlayer.Life / MaxLife;
      HitFlash.Flash(Red, true);
      if HitPlayer.Life <= 0 then
      begin
        RectDead.Exists := true;
        WalkNavigation.MoveSpeed := 0; // do not allow movement anymore, but allow rotations
      end;
    end;
  end;

  procedure HandlePing(const M: TMessagePing);
  begin
    LabelPing.Caption := Format('Ping (round-trip to server): %f', [M.ClientSendTime.ElapsedTime]);
  end;

const
  BroadcastStateTimeout = 0.01;
  PingTimeout = 1.0;
var
  M: TMessage;
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  FlushConsoleOutput;

  Client.ReceivedCs.Acquire;
  try
    for M in Client.Received do
    begin
      if M is TMessageChat then
        HandleChat(TMessageChat(M))
      else
      if M is TMessagePlayerJoin then
        HandlePlayerJoin(TMessagePlayerJoin(M))
      else
      if M is TMessagePlayerDisconnect then
        HandlePlayerDisconnect(TMessagePlayerDisconnect(M))
      else
      if M is TMessagePlayerState then
        HandlePlayerState(TMessagePlayerState(M))
      else
      if M is TMessagePlayerHit then
        HandleHit(TMessagePlayerHit(M))
      else
      if M is TMessagePing then
        HandlePing(TMessagePing(M))
      else
      begin
        NetworkLog('Received unhandled message: ' + M.ClassName);
      end;
    end;
    Client.Received.Clear;
  finally Client.ReceivedCs.Release end;

  if (not LastBroadcastState.Initialized) or
     (LastBroadcastState.ElapsedTime > BroadcastStateTimeout) then
  begin
    SendPlayerState;
    LastBroadcastState := Timer;
  end;

  if (not LastPing.Initialized) or
     (LastPing.ElapsedTime > PingTimeout) then
  begin
    SendPing;
    LastPing := Timer;
  end;
end;

function TStatePlay.Press(const Event: TInputPressRelease): Boolean;

  procedure SendHit(const HitPlayer: TPlayer);
  var
    M: TMessagePlayerHit;
  begin
    M := TMessagePlayerHit.Create;
    M.PlayerId := HitPlayer.PlayerId;
    M.ShooterPlayerId := LocalPlayer.PlayerId;
    Client.SendMessage(M, true, -1);

    // also update HitPlayer locally, as we will not get TMessagePlayerHit about it
    HitPlayer.Life := HitPlayer.Life - 1;
    if HitPlayer.Life = 0 then
      NetworkLog(Format('%s was killed by %s', [HitPlayer.Nick, LocalPlayer.Nick]));
    HitPlayer.UpdateTransform;
  end;

var
  HitTransform: TCastleTransform;
  HitPlayer: TPlayerBehavior;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if TUIState.CurrentTop = Self then
  begin
    if Event.IsMouseButton(buttonLeft) then
    begin
      HitTransform := MainViewport.TransformUnderMouse;
      if HitTransform <> nil then
      begin
        HitPlayer := HitTransform.FindBehavior(TPlayerBehavior) as TPlayerBehavior;
        if HitPlayer <> nil then
          SendHit(HitPlayer.Player);
      end;
      Exit(true); // key was handled
    end;
    if Event.IsKey(CtrlQ) then
    begin
      TUIState.Current := StateMainMenu;
      Exit(true); // key was handled
    end;
    if Event.IsKey('/') then
    begin
      TUIState.Push(StateInputChat);
      WaitingForChat := true;
      Exit(true); // key was handled
    end;
    if Event.IsKey(keyEscape) then
    begin
      WalkNavigation.MouseLook := not WalkNavigation.MouseLook;
      Exit(true); // key was handled
    end;
    if Event.IsKey(keyF1) then
    begin
      LabelControls.Exists := not LabelControls.Exists;
      Exit(true); // key was handled
    end;
  end;
end;

procedure TStatePlay.NetworkLog(const Message: String);
const
  MaxNetworkLogLines = 5;
begin
  LabelNetworkLog.Text.Add(Message);
  while LabelNetworkLog.Text.Count > MaxNetworkLogLines do
    LabelNetworkLog.Text.Delete(0);
end;

procedure TStatePlay.Resume;

  procedure SendChat(const S: String);
  var
    M: TMessageChat;
  begin
    M := TMessageChat.Create;
    M.Text := PlayerNick + ' says: ' + S;
    Client.SendMessage(M, true, -1);
    NetworkLog('You (' + PlayerNick + ') say: ' + S);
  end;

begin
  inherited;
  if WaitingForChat then
  begin
    if StateInputChat.ChatToSend <> '' then
      SendChat(StateInputChat.ChatToSend);
    WaitingForChat := false;
  end;
  WalkNavigation.MouseLook := true;
end;

procedure TStatePlay.Pause;
begin
  WalkNavigation.MouseLook := false;
  inherited;
end;

end.
