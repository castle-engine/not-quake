# Not Quake

**Online multi-player first-person shooter using Castle Game Engine and RNL.**

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
- Android: client (TODO test, probably unplayable due to missing special input code)

Server can run on any plaform and accept connection from client from any platform. I.e. you can run server on Linux and connect from any platform -- Linux, Windows etc.

## Architecture

Central server on https://michalis.xyz/ (likewise developed using RNL, with some code shared with frontend), I will keep it running throughout the gamejam and probably much longer. Each frontend is just a client to this server.

You run the game, you input your nick (honestly anything), and you join the game -- essentially one common "room" for all players.

## Code

Note that it relies on CGE [new-cameras" branch](https://github.com/castle-engine/castle-engine/tree/new-cameras) which is not yet merged to CGE master.

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

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by entering `client` or `server` and build by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `not_quake_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).

## TODO

- macOS client: broken mouse look, known CGE missing thing (Cocoa + SetMousePosition and mouse hide)

- Use speed predictions to move other players (will allow to update state less often?). I planned this for gamejam, but didn't manage.

- Test all platforms:
    - Android client

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
