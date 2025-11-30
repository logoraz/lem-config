# lem-config - Lem IDE Configuration 

<p align="center">
  <img src="assets/lem.svg" width="200" />
</p>

Modular configuration for Lem (Common Lisp Editor/IDE).

This configuration is set up as its own Common Lisp system `:lem-config`!

Stages advanced (error) logging, allowing the config to fail *quietly* and
and generates `*.log` files in `lem/logs/` (each log entry is timestamped): 
- `config-error.log` lists any issues encounted upon loading, and
- `config-startup.log` lists successful startup

## System Scaffold

- `init.lisp`      --> User init, sets up ASDF's registry and loads `:lem-config`
- `lem-config.asd` --> System definition for this configuration
- `src/`           --> Contains source files for this configuration
- `assets/`        --> Where images, lem.desktop, and related stuff are held
- `lib/`           --> WIP where lem extension systems will be held

## Build Lem & Setup
I pull Lem's source locally to `~/Work/builds/lem` and likewise
install this repo to `~/Work/lem-config`. After you have both of these there,
you can deploy this config and install lem locally via

```bash
  $ cd ~/Work/lem-config
  $ sbcl --load setup.lisp
```

## TODOs (Wish List)

  - Build out more personal keybindings
  - Build an extension analogous to Emacs' erc-mode

## References:
  - lem source: https://github.com/lem-project/lem
  - General configuration layout inspirations:
    - https://github.com/garlic0x1/.lem/
    - https://github.com/fukamachi/.lem
  - Paredit configuration inspiration:
    - https://github.com/Gavinok/.lem
  - TODO
