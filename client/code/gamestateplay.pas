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
  CastleViewport, CastleTimeUtils;

type
  TStatePlay = class(TUIState)
  private
    { Components designed using CGE editor, loaded from the castle-user-interface file. }
    LabelFps: TCastleLabel;
    LabelNetworkLog: TCastleLabel;
    MainViewport: TCastleViewport;
    //TODO WalkNavigation: TCastleWalkNavigation;

    WaitingForChat: Boolean;
    LastBroadcastState: TTimerResult;
    LastNickJoined: String;

    procedure NetworkLog(const Message: String);
  public
    PlayerNick: String; //< Set before starting state
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Resume; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses SysUtils,
  CastleStringUtils, CastleLog,
  GameClient, NetworkCommon, GameStateMainMenu, GameStateInputChat, GamePlayers;

constructor TStatePlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestateplay.castle-user-interface';
end;

const
  // TODO: relying on such suffix is a hack to associate nick names
  SJoinsSuffix = ' joins the game';

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

begin
  inherited;
  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  LabelNetworkLog := DesignedComponent('LabelNetworkLog') as TCastleLabel;
  MainViewport := DesignedComponent('MainViewport') as TCastleViewport;

  NetworkInitialize;
  OnNetworkLog := {$ifdef FPC}@{$endif} NetworkLog;

  WaitingForChat := false;

  // TODO: LastBroadcastState := TTimerResult.Uninitialized;
  FillChar(LastBroadcastState, SizeOf(LastBroadcastState), 0);

  LocalPlayer := TPlayer.Create;
  LocalPlayer.PlayerId := Random(1000 * 1000 * 1000); // TODO: we just assume everyone will choose different id
  LocalPlayer.Nick := PlayerNick;
  LocalPlayer.Life := 10;
  LocalPlayer.Position := MainViewport.Camera.WorldTranslation;
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
    M.Position := MainViewport.Camera.WorldTranslation;
    Client.SendMessage(M, true, -1);
  end;

const
  BroadcastStateTimeout = 0.1;
var
  M: TMessage;
  NewPlayer, OldPlayer: TPlayer;
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  FlushConsoleOutput;

  Client.ReceivedCs.Acquire;
  try
    for M in Client.Received do
    begin
      if M is TMessageChat then
      begin
        NetworkLog('Chat: ' + TMessageChat(M).Text);
        if IsSuffix(SJoinsSuffix, TMessageChat(M).Text, false) then
          LastNickJoined := SuffixRemove(SJoinsSuffix, TMessageChat(M).Text, false);
      end else
      if M is TMessagePlayerJoin then
      begin
        NewPlayer := TPlayer.Create;
        NewPlayer.Nick := LastNickJoined;
        NewPlayer.PlayerId := TMessagePlayerJoin(M).PlayerId;
        Players.Add(NewPlayer);
        WritelnLog('Joined player %s (%d)', [
          NewPlayer.Nick,
          NewPlayer.PlayerId
        ]);
      end else
      if M is TMessagePlayerDisconnect then
      begin
        OldPlayer := Players.FindPlayerId(TMessagePlayerDisconnect(M).PlayerId);
        if OldPlayer <> nil then
        begin
          NetworkLog(Format('%s disconnected', [OldPlayer.Nick]));
          Players.Remove(OldPlayer); // TODO remove by index, faster
        end else
          WritelnWarning('Cannot find disconnecting player id %d', [
            TMessagePlayerDisconnect(M).PlayerId
          ]);
      end else
      begin
        // TODO
        //NetworkLog('Received unhandled message: ' + M.ClassName);
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
end;

function TStatePlay.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if TUIState.CurrentTop = Self then
  begin
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
end;

end.
