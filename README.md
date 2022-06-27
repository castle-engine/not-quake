# Not Quake

**Online multi-player first-person shooter using Castle Game Engine and RNL.**

If you just want to download a ready client and play:
- get it from [release on Itch.io](https://cat-astrophe-games.itch.io/not-quake)
- or from [release on GitHub](https://github.com/castle-engine/not-quake/releases/latest)

Features:

- Choose your nick,
- join a server game,
- send chat messages to everyone,
- move, shoot, die, repeat.

This is a demo of integrating

- [Castle Game Engine](https://castle-engine.io/) - 3D and 2D open-source game engine using Object Pascal,
- [RNL](https://github.com/BeRo1985/rnl) - real-time networking library, open source, using Object Pascal

... to create a simple FPS in 3D online.

Client keys:

- `/` to send chat (type whatever, press `Enter` to send, `Escape` to cancel)
- `Ctrl + Q` to go back to main menu
- `Escape` to toggle mouse look on/off (esp. useful to switch to other applications while playing)
- AWSD, arrow keys, rotate with mouse (when mouse look) to move/rotate
- `Shift` to run
- `Left click` to shoot

Done during our gamejam at _Cat-astrophe Games_ on 2022-06-10. **Done in 1 day.**

Platforms (using GitHub actions to build everywhere):

- Linux: client and server
- Windows: client and server
- macOS: client (TODO mouse look) and server
- Android: client (TODO but input and connectivity broken)

Server can run on any plaform and accept connection from client from any platform. I.e. you can run server on Linux and connect from any platform -- Linux, Windows etc.

## Architecture

Central server. Also developed using RNL. With some code shared with frontend (client). Each frontend is just a client to this server.

You can:

- use the default server. It runs on https://michalis.xyz/ , I (Michalis) will keep it running throughout the gamejam and probably much longer. Ping me on Discord https://castle-engine.io/talk.php if the server seems to be down.

- run your own server. Use the command-line parameter `--host localhost` when running the client (with `localhost` being the name, or IP, of your own server).

You run the game, you input your nick (honestly anything), and you join the game -- essentially one common "room" for all players.

## Code

Checkout with submodules.

```
git clone --recurse-submodules https://github.com/castle-engine/not-quake
```

Build and run server:

```
cd not-quake/server/
castle-engine compile --mode=release
castle-engine run # or just ./not-quake-server
```

Build and run client:

```
cd not-quake/client/
castle-engine compile --mode=release
castle-engine run -- --host localhost
  # or just ./not-quake --host localhost
```

By default it connects to server on `michalis.xyz` (Michalis Kamburelis private host, see https://michalis.xyz/ ) which I will try to keep running for some time. Ping me (find me on Discord, https://castle-engine.io/talk.php ) if the server seems to be down :)

Using [Castle Game Engine](https://castle-engine.io/). You can clone it from https://github.com/castle-engine/castle-engine/ . The `master` branch of CGE is OK now.

## Building

Compile by entering `client` or `server` and build by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `not_quake_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).

## TODO

- macOS client: broken mouse look, known CGE missing thing (Cocoa + SetMousePosition and mouse hide)

- Use speed predictions to move other players (will allow to update state less often?). I planned this for gamejam, but didn't manage.

- Android:
    - cannot connect ("Empty Socket") - likely we miss some Android permissions
    - input nick/chat requires on-screen keyboard
    - 3D navigation requires TCastleTouchNavigation
    - actions like "send char", "quit to menu" must be buttons, not only accessible by keys

- Do more network performance testing.
    - There are various timeouts to test. Like BroadcastTimeout, NormalTimeout,
    - RNL also has easy switches to go between reliable/unreliable, ordered/unordered messages.

- Make input on Android work too.

- Rooms within the server. Before joining, you can list/create/join a room. Each room is a separate play.

- Sounds of hit / miss.

- Cut down to find source of occasional crash in server, for now workarounded:

    ```
    try
      if PayloadData = nil then
        Writeln('PayloadData = nil');
      if NormalPacketHeader = nil then
        Writeln('NormalPacketHeader = nil');
      DispatchIncomingPacket(PayloadData^,
                             PayloadDataLength,
                             TRNLEndianness.LittleEndianToHost16(NormalPacketHeader^.SentTime));
    except
      on E: EAccessViolation do
        Writeln('Caught EAccessViolation around DispatchIncomingPacket, silencing'); // TODO: reproduce, report
    end;
    ```

- Big: Turn this into generic network that can auto-synchronize TCastleViewport or TCastleTransform or other TCastleComponent state "automagically" over the network.
