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
  CastleVectors, NetworkCommon;

type
  TPlayer = class
    PlayerId: TPlayerId;
    Nick: String;
    Position, PositionDelta: TVector3;
    Rotation, RotationDelta: Single;
    Life: Byte;
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

function TPlayerList.FindPlayerId(const PlayerId: TPlayerId): TPlayer;
var
  P: TPlayer;
begin
  for P in Self do
    if P.PlayerId = PlayerId then
      Exit(P);
  Result := P;
end;

end.
