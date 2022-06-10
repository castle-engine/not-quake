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
  {$ifdef unix}
  cthreads,
  {$endif}
  SysUtils,
  Classes,
  SyncObjs,
  NetworkCommon,
  RNL;

type TServer=class(TNetworkingThread)
      private
       fReadyEvent:TEvent;
      protected
       procedure Execute; override;
      public
       constructor Create(const aCreateSuspended:boolean); reintroduce;
       destructor Destroy; override;
     end;

var RNLInstance:TRNLInstance=nil;

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
var //Address:TRNLAddress;
    Server:TRNLHost;
    Event:TRNLHostEvent;
const
  { Decrease to send our messages, and check for received messages, more often.
    Value = 0 is OK: RNL code says it will do then "one iteration without waiting". }
  NormalTimeout = 10;
begin
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
        Event.Peer.Channels[0].SendMessageString('Hello world from server!');
  //    Server.Flush;
       end;
       RNL_HOST_EVENT_TYPE_PEER_DISCONNECT:begin
        ConsoleOutput(Format('Server: A client disconnected, local peer ID %d, remote peer ID %d, channels count %d',
                             [Event.Peer.LocalPeerID,
                              Event.Peer.RemotePeerID,
                              Event.Peer.CountChannels]));
       end;
       RNL_HOST_EVENT_TYPE_PEER_MTU:begin
        ConsoleOutput('Server: A client '+IntToStr(TRNLPtrUInt(Event.Peer))+' has new MTU '+IntToStr(TRNLPtrUInt(Event.MTU)));
       end;
       RNL_HOST_EVENT_TYPE_PEER_RECEIVE:begin
        ConsoleOutput('Server: A message received on channel '+IntToStr(Event.Channel)+': "'+String(Event.Message.AsString)+'" from ' + IntToStr(Event.Peer.LocalPeerID));
       end;
      end;
     finally
      Event.Free;
     end;
     ProcessMessages(Server);
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
end;

var
  Server:TServer;
  Command: String;
begin
  RNLInstance:=TRNLInstance.Create;
  try
    RNLNetwork:={$ifdef VirtualNetwork}TRNLVirtualNetwork{$else}TRNLRealNetwork{$endif}.Create(RNLInstance);
    try
      Server:=TServer.Create(false);
      try
        readln(Command);
        while Command <> 'q' do
        begin
          Server.SendMessage(-1, Command);
          readln(Command);
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
