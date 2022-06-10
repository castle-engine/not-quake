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

{ Player management. }
unit GamePlayers;

interface

uses Generics.Collections,
  CastleVectors, CastleTransform, CastleViewport, CastleScene,
  NetworkCommon;

type
  TPlayer = class
    PlayerId: TPlayerId;
    Nick: String;
    Position, PositionDelta: TVector3;
    Rotation, RotationDelta: Single;
    Life: Byte;
    Transform: TCastleTransform;
    procedure CreateTransform(const Viewport: TCastleViewport);
    destructor Destroy; override;
  end;

  TPlayerList = class({$ifdef FPC}specialize{$endif} TObjectList<TPlayer>)
    { Find given PlayerId, returns nil if not found. }
    function FindPlayerId(const PlayerId: TPlayerId): TPlayer;
  end;

var
  { Information about local + remote players. }
  Players: TPlayerList;

  LocalPlayer: TPlayer;

implementation

uses SysUtils,
  CastleUIControls, CastleColors, X3DLoad, X3DNodes;

var
  AvatarRoot: TX3DRootNode;

{ TPlayer -------------------------------------------------------------------- }

destructor TPlayer.Destroy;
begin
  FreeAndNil(Transform); // will also remove it from Viewport
  inherited;
end;

procedure TPlayer.CreateTransform(const Viewport: TCastleViewport);
var
  Scene: TCastleScene;
  Text: TCastleText;
begin
  Transform := TCastleTransform.Create(Viewport);
  Transform.Translation := Position;

  Scene := TCastleScene.Create(Transform);
  Scene.Load(AvatarRoot.DeepCopy as TX3DRootNode, true);
  Scene.PlayAnimation('idle', true);
  Transform.Add(Scene);

  Text := TCastleText.Create(Transform);
  Text.Caption := Nick;
  Text.Translation := Vector3(0, 2, 0);
  Text.Alignment := hpMiddle;
  Text.Size := 0.1;
  Text.Color := Blue;
  Transform.Add(Text);

  Viewport.Items.Add(Transform);
end;

{ TPlayerList ---------------------------------------------------------------- }

function TPlayerList.FindPlayerId(const PlayerId: TPlayerId): TPlayer;
var
  P: TPlayer;
begin
  for P in Self do
    if P.PlayerId = PlayerId then
      Exit(P);
  Result := P;
end;

initialization
  { Reading it once is not only an optimization, it also prevents problems when testing locally
    and multiple clients try to read the same file. }
  AvatarRoot := LoadNode('castle-data:/avatar/avatar.gltf');
finalization
  FreeAndNil(AvatarRoot);
end.
