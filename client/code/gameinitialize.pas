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

{ Game initialization.
  This unit is cross-platform.
  It will be used by the platform-specific program or library file. }
unit GameInitialize;

interface

implementation

uses SysUtils,
  CastleWindow, CastleLog, CastleUIState, CastleParameters, CastleUtils,
  GamePlayers, GameClient
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameStateMainMenu
  , GameStatePlay
  , GameStateInputChat
  {$endregion 'Castle Initialization Uses'};

var
  Window: TCastleWindow;

const
  Options: array [0..0] of TOption =
  (
    (Short: #0 ; Long: 'host'; Argument: oaRequired)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: HostAddress := Argument;
    else raise EInternalError.Create('OptionProc');
  end;
end;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  Parameters.Parse(Options, @OptionProc, nil, true);

  InitializeAvatar;

  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Create TStateMainMenu that will handle "main" state of the game.
    Larger games may use multiple states,
    e.g. TStateMainMenu ("main menu state"),
    TStatePlay ("playing the game state"),
    TStateCredits ("showing the credits state") etc. }
  {$region 'Castle State Creation'}
  // The content here may be automatically updated by CGE editor.
  StateMainMenu := TStateMainMenu.Create(Application);
  StatePlay := TStatePlay.Create(Application);
  StateInputChat := TStateInputChat.Create(Application);
  {$endregion 'Castle State Creation'}

  TUIState.Current := StateMainMenu;
end;

initialization
  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindow.Create(Application);
  Window.ParseParameters; // allows to control window size / fullscreen on the command-line
  Application.MainWindow := Window;

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    (because in case of non-desktop platforms,
    some necessary resources may not be prepared yet). }
end.
