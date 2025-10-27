{
  Copyright 2022-2022 Michalis Kamburelis, Benjamin Rosseaux.

  This file is part of "Not Quake".

  "Not Quake" is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  "Not Quake" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ "Not Quake" dedicated server. Based on RNL example. }

{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses
  {$ifdef unix} cthreads, {$endif} SysUtils, Classes, SyncObjs, Generics.Collections,
  RNL,
  CastleLog, CastleClassUtils, CastleStringUtils,
  GameNetwork;

type
  TServer = class(TNetworkingThread)
  private
    fReadyEvent:TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(const aCreateSuspended:boolean); reintroduce;
    destructor Destroy; override;
  end;

  // TODO: server should have more player knowledge, to pass position/life to joining players
  TPlayer = class
    PlayerId: TPlayerId;
    Nick: String;
  end;

  TPeerToPlayer = {$ifdef FPC}specialize{$endif} TObjectDictionary<Integer, TPlayer>;

var
  RNLInstance:TRNLInstance=nil;
  RNLCompressorClass:TRNLCompressorClass=TRNLCompressorLZBRRC;
  RNLNetwork:TRNLNetwork=nil;

constructor TServer.Create(const aCreateSuspended:boolean);
begin
 fReadyEvent:=TEvent.Create(nil,false,false,'');
 inherited Create(aCreateSuspended);
end;

destructor TServer.Destroy;
begin
 inherited Destroy;
 FreeAndNil(fReadyEvent);
end;

procedure TServer.Execute;
var
  PeerToPlayer: TPeerToPlayer;

  { When new player joins server already filled with players, notify about new players. }
  procedure SendExistingPlayers(const LocalPeerID: Integer);
  var
    MJ: TMessagePlayerJoin;
    M: TMessageChat;
    Player: TPlayer;
  begin
    for Player in PeerToPlayer.Values do
    begin
      // reuse TMessageChat to pass nick
      // TODO: this is poor to rely on 2 messages, and it depends on ordered channel - when reading we expect SJoinsSuffix in chat before TMessagePlayerJoin
      M := TMessageChat.Create;
      M.Text := Player.Nick + SJoinsSuffix;
      SendMessage(M, false, LocalPeerID);

      MJ := TMessagePlayerJoin.Create;
      MJ.PlayerId := Player.PlayerId;
      SendMessage(MJ, false, LocalPeerID);
    end;
  end;

  procedure SendDisconnect(const LocalPeerID: Integer);
  var
    M: TMessagePlayerDisconnect;
    Player: TPlayer;
  begin
    if not PeerToPlayer.TryGetValue(LocalPeerID, Player) then
    begin
      ConsoleOutput(Format('WARNING: Cannot find on peer->player map peer id %d', [LocalPeerID]));
      Exit;
    end;

    M := TMessagePlayerDisconnect.Create;
    M.PlayerId := Player.PlayerId;
    SendMessage(M, true, LocalPeerID);
    PeerToPlayer.Remove(LocalPeerID);
  end;

const
  { Decrease to send our messages, and check for received messages, more often.
    Value = 0 is OK: RNL code says it will do then "one iteration without waiting". }
  NormalTimeout = 10;
var
  //Address:TRNLAddress;
  Server:TRNLHost;
  Event:TRNLHostEvent;
  M: TMessage;
  NewPlayer: TPlayer;
  LastNickJoined: String;
begin
  // TODO: express rest like this, avoid such nested try finally
  PeerToPlayer := nil;
  try
    PeerToPlayer := TPeerToPlayer.Create([doOwnsValues]);

    {$ifndef fpc}
    NameThreadForDebugging('Server');
    {$endif}
    ConsoleOutput('Server: Thread started');
    try
     Server:=TRNLHost.Create(RNLInstance,RNLNetwork);
     try
      Server.Address^.Host:=RNL_HOST_ANY;
      Server.Address^.Port:=64242;
   {  RNLNetwork.AddressSetHost(Server.Address^,'127.0.0.1');
      Server.Address.Port:=64242;}
      Server.Compressor:=RNLCompressorClass.Create;
      Server.MaximumCountPeers := 1000; // no peer limit in this simple demo, disregard possible server load
      Server.MaximumCountChannels:=4;
      Server.ChannelTypes[0]:=RNL_PEER_RELIABLE_ORDERED_CHANNEL;
      Server.ChannelTypes[1]:=RNL_PEER_RELIABLE_UNORDERED_CHANNEL;
      Server.ChannelTypes[2]:=RNL_PEER_UNRELIABLE_ORDERED_CHANNEL;
      Server.ChannelTypes[3]:=RNL_PEER_UNRELIABLE_UNORDERED_CHANNEL;
      Server.Start(RNL_HOST_ADDRESS_FAMILY_WORK_MODE_IPV4_AND_IPV6);
      fReadyEvent.SetEvent;
      Event.Initialize;
      try
       while (not Terminated) and (Server.Service(Event,NormalTimeout)<>RNL_HOST_SERVICE_STATUS_ERROR) do
       begin
        try
         case Event.Type_ of
          RNL_HOST_EVENT_TYPE_PEER_CHECK_CONNECTION_TOKEN:begin
           if assigned(Event.ConnectionCandidate) then begin
            ConsoleOutput('Server: A new client is connecting');
            Event.ConnectionCandidate^.AcceptConnectionToken;
           end;
          end;
          RNL_HOST_EVENT_TYPE_PEER_CHECK_AUTHENTICATION_TOKEN:begin
           if assigned(Event.ConnectionCandidate) then begin
            ConsoleOutput('Server: A new client is authenticating');
            Event.ConnectionCandidate^.AcceptAuthenticationToken;
           end;
          end;
          RNL_HOST_EVENT_TYPE_PEER_CONNECT:begin
           ConsoleOutput(Format('Server: A new client connected, local peer ID %d, remote peer ID %d, channels count %d',
                                [Event.Peer.LocalPeerID,
                                 Event.Peer.RemotePeerID,
                                 Event.Peer.CountChannels]));
           //Event.Peer.Channels[0].SendMessageString('Hello world from server!');
     //    Server.Flush;
           SendExistingPlayers(Event.Peer.LocalPeerID);
          end;
          RNL_HOST_EVENT_TYPE_PEER_DISCONNECT:
          begin
           ConsoleOutput(Format('Server: A client disconnected, local peer ID %d, remote peer ID %d, channels count %d',
                                [Event.Peer.LocalPeerID,
                                 Event.Peer.RemotePeerID,
                                 Event.Peer.CountChannels]));
           SendDisconnect(Event.Peer.LocalPeerID);
          end;
          RNL_HOST_EVENT_TYPE_PEER_MTU:begin
           ConsoleOutput('Server: A client '+IntToStr(TRNLPtrUInt(Event.Peer))+' has new MTU '+IntToStr(TRNLPtrUInt(Event.MTU)));
          end;
          RNL_HOST_EVENT_TYPE_PEER_RECEIVE:begin
           //ConsoleOutput('Server: A message received on channel '+IntToStr(Event.Channel)+': "'+String(Event.Message.AsString)+'" from ' + IntToStr(Event.Peer.LocalPeerID));

           M := TMessage.TryDeserialize(Event.Message);
           if M <> nil then
           begin
             //ProcessMessageReceived(M);
             // Just process the message immediately in this thread:

             // only log TMessageChat to avoid log flood
             if M is TMessageChat then
             begin
               ConsoleOutput('Server: A chat message received on channel '+IntToStr(Event.Channel)+': "'+String(TMessageChat(M).Text)+'" from ' + IntToStr(Event.Peer.LocalPeerID));

               // record LastNickJoined for later usage
               if IsSuffix(SJoinsSuffix, TMessageChat(M).Text, false) then
                 LastNickJoined := SuffixRemove(SJoinsSuffix, TMessageChat(M).Text, false);
             end;

             if M is TMessagePlayerJoin then
             begin
               NewPlayer := TPlayer.Create;
               NewPlayer.PlayerId := TMessagePlayerJoin(M).PlayerId;
               NewPlayer.Nick := LastNickJoined;
               PeerToPlayer.Add(Event.Peer.LocalPeerID, NewPlayer);
             end;

             if (M is TMessageChat) and
                (TMessageChat(M).Text = 'Hello world from client!') then
               FreeAndNil(M) // do not broadcast this
             else
             if M is TMessagePing then
               SendMessage(M, false, Event.Peer.LocalPeerID)
             else
               SendMessage(M, true, Event.Peer.LocalPeerID); // broadcast everything else, but not to originating client
           end else
             ConsoleOutput('Unrecognized message received');
          end;
         end;
        finally
         Event.Free;
        end;
        ProcessMessagesToSend(Server);
       end;
      finally
       Event.Finalize;
      end;
     finally
      Server.Free;
     end;
    except
     on e:Exception do begin
      LogThreadException('Server',e);
     end;
    end;
    ConsoleOutput('Server: Thread stopped');
  finally
     FreeAndNil(PeerToPlayer);
  end;
end;

const
  LogsFlushTimeout = 10;
var
  Server:TServer;
begin
  LogTimePrefix := ltDateTime;
  // write to stdout, regardless of platform, even on Windows
  InitializeLog(StdOutStream);

  RNLInstance:=TRNLInstance.Create;
  try
    RNLNetwork:={$ifdef VirtualNetwork}TRNLVirtualNetwork{$else}TRNLRealNetwork{$endif}.Create(RNLInstance);
    try
      Server:=TServer.Create(false);
      try
        while true do
        begin
          FlushConsoleOutput;
          Sleep(LogsFlushTimeout);
        end;
      finally
        Server.Terminate;
        Server.WaitFor;
        LogThreadException('Server',Server.FatalException);
        Server.Free;
      end;
    finally
      FreeAndNil(RNLNetwork);
    end;
  finally
    FreeAndNil(RNLInstance);
  end;
end.
