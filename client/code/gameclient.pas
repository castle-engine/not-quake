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

{ Game-specific client. Based on RNL example. }
unit GameClient;

interface

uses NetworkCommon;

type
  TClient=class(TNetworkingThread)
  protected
    procedure Execute; override;
  end;

var
  HostAddress: String = '127.0.0.1';
    // = 'michalis.xyz';

  Client: TClient;

procedure NetworkInitialize;
procedure NetworkFinish;

implementation

uses
  {$ifdef unix}
  cthreads,
  {$endif}
  SysUtils,
  Classes,
  SyncObjs,
  RNL;

var
  RNLInstance:TRNLInstance=nil;
  RNLCompressorClass:TRNLCompressorClass=TRNLCompressorLZBRRC;
  RNLNetwork:TRNLNetwork=nil;

procedure TClient.Execute;
var
  Address:TRNLAddress;
  Client:TRNLHost;
  Event:TRNLHostEvent;
  Peer:TRNLPeer;
  Disconnected:boolean;
  M: TMessageChat;
const
  { Decrease to send our messages, and check for received messages, more often.
    Value = 0 is OK: RNL code says it will do then "one iteration without waiting". }
  NormalTimeout = 10;
  ConnectTimeout = 5000;
  DisconnectTimeout = 3000;
begin
{$ifndef fpc}
 NameThreadForDebugging('Client');
{$endif}
 ConsoleOutput('Client: Thread started');
 try
  Client:=TRNLHost.Create(RNLInstance,RNLNetwork);
  try
   Client.Compressor:=RNLCompressorClass.Create;
   Client.MaximumCountChannels:=4;
   Client.ChannelTypes[0]:=RNL_PEER_RELIABLE_ORDERED_CHANNEL;
   Client.ChannelTypes[1]:=RNL_PEER_RELIABLE_UNORDERED_CHANNEL;
   Client.ChannelTypes[2]:=RNL_PEER_UNRELIABLE_ORDERED_CHANNEL;
   Client.ChannelTypes[3]:=RNL_PEER_UNRELIABLE_UNORDERED_CHANNEL;
   Client.Start(RNL_HOST_ADDRESS_FAMILY_WORK_MODE_IPV4_AND_IPV6);
   ConsoleOutput('Client: Connecting');
   RNLNetwork.AddressSetHost(Address,HostAddress);
   Address.Port:=64242;
   Peer:=Client.Connect(Address,4,0);
   if assigned(Peer) then begin
    Peer.IncRef; // Protect it for the Peer.Free call at the end (increase ReferenceCounter from 1 to 2, so that correct-used DecRef calls never will free this peer class instance)
    try
     Event.Initialize;
     try
      if Client.ConnectService(Event,ConnectTimeout)=RNL_HOST_SERVICE_STATUS_EVENT then begin
       case Event.Type_ of
        RNL_HOST_EVENT_TYPE_PEER_APPROVAL:begin
         if Event.Peer=Peer then begin
          ConsoleOutput(Format('Client: Connected, local peer ID %d, remote peer ID %d, channels count %d',
                               [Event.Peer.LocalPeerID,
                                Event.Peer.RemotePeerID,
                                Event.Peer.CountChannels]));
          Event.Peer.Channels[0].SendMessageString('Hello world from client!');
          Disconnected:=false;
          while (not Terminated) and (Client.Service(Event,NormalTimeout)<>RNL_HOST_SERVICE_STATUS_ERROR) do
          begin
           try
            case Event.Type_ of
             RNL_HOST_EVENT_TYPE_NONE:begin
             end;
             RNL_HOST_EVENT_TYPE_PEER_CONNECT:begin
              if Event.Peer=Peer then begin
               ConsoleOutput(Format('Client: Connected, local peer ID %d, remote peer ID %d, channels count %d',
                                    [Event.Peer.LocalPeerID,
                                     Event.Peer.RemotePeerID,
                                     Event.Peer.CountChannels]));
              end;
             end;
             RNL_HOST_EVENT_TYPE_PEER_DISCONNECT:begin
              ConsoleOutput(Format('Client: Disconnected, local peer ID %d, remote peer ID %d, channels count %d',
                                   [Event.Peer.LocalPeerID,
                                    Event.Peer.RemotePeerID,
                                    Event.Peer.CountChannels]));
              if Event.Peer=Peer then begin
               Disconnected:=true;
               break;
              end;
             end;
             RNL_HOST_EVENT_TYPE_PEER_DENIAL:begin
              if Event.Peer=Peer then begin
               ConsoleOutput('Client: Denied');
               Disconnected:=true;
               break;
              end;
             end;
             RNL_HOST_EVENT_TYPE_PEER_MTU:begin
              ConsoleOutput('Client: New MTU '+IntToStr(TRNLPtrUInt(Event.MTU)));
             end;
             RNL_HOST_EVENT_TYPE_PEER_RECEIVE:
             begin
              ConsoleOutput('Client: A message received on channel '+IntToStr(Event.Channel)+': "'+String(Event.Message.AsString)+'"');
              M := TMessageChat.Create; // TODO: creates TMessageChat always
              M.Text := Event.Message.AsString;
              ProcessMessageReceived(M);
             end;
            end;
           finally
            Event.Free;
           end;
           ProcessMessagesToSend(Client);
          end;
          if not Disconnected then begin
           ConsoleOutput('Client: Disconnecting');
           Peer.Disconnect;
           while Client.Service(Event,DisconnectTimeout)<>RNL_HOST_SERVICE_STATUS_ERROR do begin
            try
             case Event.type_ of
              RNL_HOST_EVENT_TYPE_PEER_RECEIVE:begin
              end;
              RNL_HOST_EVENT_TYPE_PEER_DISCONNECT:begin
               ConsoleOutput(Format('Client: Disconnected, local peer ID %d, remote peer ID %d, channels count %d',
                                    [Event.Peer.LocalPeerID,
                                     Event.Peer.RemotePeerID,
                                     Event.Peer.CountChannels]));
               if Event.Peer=Peer then begin
                break;
               end;
              end;
             end;
            finally
             Event.Free;
            end;
           end;
          end;
         end else begin
          ConsoleOutput('Connection failed');
         end;
        end;
        RNL_HOST_EVENT_TYPE_PEER_DENIAL:begin
         ConsoleOutput('Connection denied');
        end;
        else begin
         ConsoleOutput('Connection failed');
        end;
       end;
      end else begin
       ConsoleOutput('Connection failed');
      end;
     finally
      Event.Finalize;
     end;
    finally
     Peer.Free;
    end;
   end else begin
    ConsoleOutput('Connection failed');
   end;
  finally
   Client.Free;
  end;
 except
  on e:Exception do begin
   LogThreadException('Client',e);
  end;
 end;
 ConsoleOutput('Client: Thread stopped');
end;

procedure NetworkInitialize;
begin
  RNLInstance:=TRNLInstance.Create;
  RNLNetwork:={$ifdef VirtualNetwork}TRNLVirtualNetwork{$else}TRNLRealNetwork{$endif}.Create(RNLInstance);
  Client:=TClient.Create(false);
end;

procedure NetworkFinish;
begin
  if Client <> nil then
  begin
    Client.Terminate;
    Client.WaitFor;
    LogThreadException('Client',Client.FatalException);
    FreeAndNil(Client);
  end;
  FreeAndNil(RNLNetwork);
  FreeAndNil(RNLInstance);
end;

end.
