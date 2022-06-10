# not-quake

Online first-person shooter using Castle Game Engine and RNL.

Client keys:

- `/` to send chat (type whatever, press `Enter` to send, `Escape` to cancel)

- `Ctrl + Q` to go back to main menu

- `Escape` to toggle mouse look on/off

- AWSD, arrow keys, rotate with mouse (when mouse look) to move/rotate

- `Left click` to shoot

Done for gamejam in Cat-astrophe Games on 2022-06-10. Plans: https://docs.google.com/document/d/18TMnuJfNZQVhCodwymMnAqt9yUw5070KzoEyz8LA204/edit?usp=sharing

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

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by entering `client` or `server` and build by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `not_quake_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).

## TODO

use speed predictions
show proper anims based on speed
use anim transition
