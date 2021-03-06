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
  CastleVectors, CastleTransform, CastleViewport, CastleScene, CastleTimeUtils,
  GameNetwork;

type
  TPlayer = class
  strict private
    Scene: TCastleScene;
    Text: TCastleText;
    LastAnimChange: TTimerResult;
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
    function Alive: Boolean;
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
  Scene.AutoAnimationLoop := true;
  Scene.AutoAnimation := 'idle';
  Scene.DefaultAnimationTransition := 0.1;
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

function TPlayer.Alive: Boolean;
begin
  Result := Life > 0;
end;

procedure TPlayer.UpdateTransform;
const
  MinSpeedToWalk = 0.01;
  { Do not set this to be equal to WalkNavigation.MoveSpeed (5 now),
    as it would mean animation flips walk/run randomly. }
  MinSpeedToRun = 7.5;
  TimeToAllowAnimChange = 0.25; // a bit larger than Scene.DefaultAnimationTransition
var
  Speed: Single;
  NewAnimation: String;
begin
  if Transform = nil then // LocalPlayer has transform nil
    Exit;

  Transform.Exists := Alive;
  Text.Exists := Alive;
  if Alive then
  begin
    Transform.Translation := Position;
    Transform.Direction := Direction;
    Text.Caption := Nick + Format(' (%d)', [Life]);

    Speed := PositionDelta.Length;
    if Speed > MinSpeedToRun then
      NewAnimation := 'run'
    else
    if Speed > MinSpeedToWalk then
      NewAnimation := 'walk'
    else
      NewAnimation := 'idle';

    if (Scene.AutoAnimation <> NewAnimation) and
       ( (not LastAnimChange.Initialized) or
         (LastAnimChange.ElapsedTime > TimeToAllowAnimChange) ) then
    begin
      Scene.AutoAnimation := NewAnimation;
      LastAnimChange := Timer;
    end;
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
