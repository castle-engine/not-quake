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

procedure TStatePlay.Start;
begin
  inherited;
  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  LabelNetworkLog := DesignedComponent('LabelNetworkLog') as TCastleLabel;

  NetworkInitialize;
  OnNetworkLog := {$ifdef FPC}@{$endif} NetworkLog;
  Client.SendMessage(-1, PlayerNick + ' joins the game');
end;

procedure TStatePlay.Stop;
begin
  OnNetworkLog := nil;
  NetworkFinish;
  inherited;
end;

procedure TStatePlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  FlushConsoleOutput;
end;

function TStatePlay.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keySpace) then
  begin
    Client.SendMessage(-1, 'Hello from GUI client in CGE');
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
