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
  CastleVectors, CastleUIState, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TStatePlay = class(TUIState)
  private
    { Components designed using CGE editor, loaded from the castle-user-interface file. }
    LabelFps: TCastleLabel;
    LabelNetworkLog: TCastleLabel;
    procedure NetworkLog(const Message: String);
    procedure SendChat(const S: String);
  public
    PlayerNick: String; //< Set before starting state
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses SysUtils,
  CastleStringUtils,
  GameClient, NetworkCommon, GameStateMainMenu;

constructor TStatePlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestateplay.castle-user-interface';
end;

procedure TStatePlay.SendChat(const S: String);
var
  M: TMessageChat;
begin
  M := TMessageChat.Create;
  M.Text := S;
  Client.SendMessage(-1, M);
end;

procedure TStatePlay.Start;
begin
  inherited;
  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  LabelNetworkLog := DesignedComponent('LabelNetworkLog') as TCastleLabel;

  NetworkInitialize;
  OnNetworkLog := {$ifdef FPC}@{$endif} NetworkLog;

  SendChat(PlayerNick + ' joins the game');
end;

procedure TStatePlay.Stop;
begin
  OnNetworkLog := nil;
  NetworkFinish;
  inherited;
end;

procedure TStatePlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
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
end;

function TStatePlay.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(key1) then
  begin
    // TODO: proper chat
    SendChat(PlayerNick + ' says "1"');
    Exit(true); // key was handled
  end;
  if Event.IsKey(CtrlQ) then
  begin
    TUIState.Current := StateMainMenu;
    Exit(true); // key was handled
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

end.
