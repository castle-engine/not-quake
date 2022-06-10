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

{ Main menu. }
unit GameStateMainMenu;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  TStateMainMenu = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemainmenu.castle-user-interface. }
    EditNick: TCastleEdit;
    ButtonJoin: TCastleButton;
    procedure ClickJoin(Sender: TObject);
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
  CastleWindow,
  GameStatePlay;

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
  EditNick := DesignedComponent('EditNick') as TCastleEdit;
  ButtonJoin := DesignedComponent('ButtonJoin') as TCastleButton;

  StateContainer.ForceCaptureInput := EditNick; // make key presses go to edit first

  EditNick.Text := 'Viper' + IntToStr(Random(1000));
  ButtonJoin.OnClick := {$ifdef FPC}@{$endif} ClickJoin;
end;

procedure TStateMainMenu.Stop;
begin
  StateContainer.ForceCaptureInput := nil;
  inherited;
end;

procedure TStateMainMenu.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
end;

procedure TStateMainMenu.ClickJoin(Sender: TObject);
begin
  if EditNick.Text = '' then
  begin
    Application.MainWindow.MessageOK('Nick cannot be empty', mtError);
    Exit;
  end;

  StatePlay.PlayerNick := EditNick.Text;
  TUIState.Current := StatePlay;
end;

function TStateMainMenu.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEnter) then
  begin
    ClickJoin(nil);
    Exit(true); // key was handled
  end;
end;

end.
