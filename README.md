# not-quake

Online first-person shooter using Castle Game Engine and RNL.

Checkout with submodules, `git clone --recurse-submodules https://github.com/castle-engine/not-quake`

Done for gamejam in Cat-astrophe Games on 2022-06-10. Plans: https://docs.google.com/document/d/18TMnuJfNZQVhCodwymMnAqt9yUw5070KzoEyz8LA204/edit?usp=sharing

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by entering `client` or `server` and build by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `not_quake_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).

## TODO

pass binary messages, handle all message types

send messages to people *except* this one, to not broadcast back

input any chat msg
