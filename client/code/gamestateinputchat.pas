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

{ Compose chat message. }
unit GameStateInputChat;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TStateInputChat = class(TUIState)
  private
    { Components designed using CGE editor, loaded from the castle-user-interface file. }
    ButtonSend: TCastleButton;
    ButtonCancel: TCastleButton;
    EditChatMessage: TCastleEdit;
    procedure ClickSend(Sender: TObject);
    procedure ClickCancel(Sender: TObject);
  public
    ChatToSend: String; //< Set on stop by this state, '' means to not send anything
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateInputChat: TStateInputChat;

implementation

constructor TStateInputChat.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestateinputchat.castle-user-interface';
  InterceptInput := true;
end;

procedure TStateInputChat.Start;
begin
  inherited;
  { Find components, by name, that we need to access from code }
  ButtonSend := DesignedComponent('ButtonSend') as TCastleButton;
  ButtonCancel := DesignedComponent('ButtonCancel') as TCastleButton;
  EditChatMessage := DesignedComponent('EditChatMessage') as TCastleEdit;

  ButtonSend.OnClick := {$ifdef FPC}@{$endif} ClickSend;
  ButtonCancel.OnClick := {$ifdef FPC}@{$endif} ClickCancel;

  StateContainer.ForceCaptureInput := EditChatMessage; // make key presses go to edit first
end;

procedure TStateInputChat.Stop;
begin
  StateContainer.ForceCaptureInput := nil;
  inherited;
end;

procedure TStateInputChat.ClickSend(Sender: TObject);
begin
  ChatToSend := EditChatMessage.Text;
  TUIState.Pop(Self);
end;

procedure TStateInputChat.ClickCancel(Sender: TObject);
begin
  ChatToSend := '';
  TUIState.Pop(Self);
end;

function TStateInputChat.Press(const Event: TInputPressRelease): Boolean;
begin
  if Event.IsKey(keyEnter) then
  begin
    ClickSend(nil);
    Exit(true); // key was handled
  end;
  if Event.IsKey(keyEscape) then
  begin
    ClickCancel(nil);
    Exit(true); // key was handled
  end;

  { since this has InterceptInput, check ancestor last (it will always return true) }
  Result := inherited;
end;

end.
