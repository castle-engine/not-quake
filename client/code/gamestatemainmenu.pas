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

{ Main state, where most of the application logic takes place. }
unit GameStateMainMenu;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Main state, where most of the application logic takes place. }
  TStateMainMenu = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemainmenu.castle-user-interface. }
    LabelFps: TCastleLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMainMenu: TStateMainMenu;

implementation

uses SysUtils,
  GameClient;

{ TStateMainMenu ----------------------------------------------------------------- }

constructor TStateMainMenu.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemainmenu.castle-user-interface';
end;

procedure TStateMainMenu.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  NetworkInitialize;
end;

procedure TStateMainMenu.Stop;
begin
  NetworkFinish;
  inherited;
end;

procedure TStateMainMenu.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TStateMainMenu.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keySpace) then
  begin
    Client.SendMessage(-1, 'Hello from GUI client in CGE');
    Exit(true); // key was handled
  end;
end;

end.
