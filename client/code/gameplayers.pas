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
  GameNetwork;

type
  TPlayer = class
  strict private
    Scene: TCastleScene;
    Text: TCastleText;
  public
    PlayerId: TPlayerId;
    Nick: String;
    Position, PositionDelta: TVector3;
    Direction, DirectionDelta: TVector3;
    Life: Byte;
    Transform: TCastleTransform;
    destructor Destroy; override;
    procedure CreateTransform(const Viewport: TCastleViewport);
    procedure UpdateTransform;
  end;

  TPlayerBehavior = class(TCastleBehavior)
  public
    Player: TPlayer;
  end;

  TPlayerList = class({$ifdef FPC}specialize{$endif} TObjectList<TPlayer>)
    { Find given PlayerId, returns nil if not found. }
    function FindPlayerId(const PlayerId: TPlayerId): TPlayer;
  end;

var
  { Information about local + remote players. }
  Players: TPlayerList;

  LocalPlayer: TPlayer;

procedure InitializeAvatar;

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
  Beh: TPlayerBehavior;
begin
  Transform := TCastleTransform.Create(Viewport);

  Scene := TCastleScene.Create(Transform);
  Scene.Load(AvatarRoot.DeepCopy as TX3DRootNode, true);
  Scene.PlayAnimation('idle', true);
  Scene.Collides := false; // do not collide with other players
  Transform.Add(Scene);

  Text := TCastleText.Create(Transform);
  Text.Translation := Vector3(0, 2, 0);
  Text.Alignment := hpMiddle;
  Text.Size := 0.1;
  Text.Color := Blue;
  Text.Collides := false;
  Transform.Add(Text);

  Beh := TPlayerBehavior.Create(Transform);
  Beh.Player := Self;
  Scene.AddBehavior(Beh);

  UpdateTransform;

  Viewport.Items.Add(Transform);
end;

procedure TPlayer.UpdateTransform;
var
  Alive: Boolean;
begin
  if Transform = nil then // LocalPlayer has transform nil
    Exit;

  Alive := Life > 0;
  Transform.Exists := Alive;
  Text.Exists := Alive;
  if Alive then
  begin
    Transform.Translation := Position;
    Transform.Direction := Direction;
    Text.Caption := Nick + Format(' (%d)', [Life]);
  end;
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

procedure InitializeAvatar;
begin
  { Reading it once is not only an optimization, it also prevents problems when testing locally
    and multiple clients try to read the same file. }
  AvatarRoot := LoadNode('castle-data:/avatar/avatar.gltf');
end;

finalization
  FreeAndNil(AvatarRoot);
end.
