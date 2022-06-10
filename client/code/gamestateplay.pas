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
  CastleStringUtils,
  GameClient, NetworkCommon, GameStateMainMenu, GameStateInputChat;

constructor TStatePlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestateplay.castle-user-interface';
end;

procedure TStatePlay.Start;

  procedure SendJoin;
  var
    M: TMessageChat;
  begin
    // reuse TMessageChat, no dedicated TMessageJoin now
    M := TMessageChat.Create;
    M.Text := PlayerNick + ' joins the game';
    Client.SendMessage(M, true, -1);
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

  SendJoin;
end;

procedure TStatePlay.Stop;
begin
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
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  FlushConsoleOutput;

  Client.ReceivedCs.Acquire;
  try
    for M in Client.Received do
    begin
      if M is TMessageChat then
        NetworkLog('Chat: ' + TMessageChat(M).Text)
      else
        // TODO
        NetworkLog('Received unhandled message: ' + M.ClassName);
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
