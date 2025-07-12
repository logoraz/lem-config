# lem-config - Lem IDE Configuration 

<p align="center">
  <img src="assets/lem.svg" width="200" />
</p>

Personal configuration for Lem (Common Lisp Editor/IDE).

This configuration is set up as its own Common Lisp system --> `:lem-config`!

A cool feature is the system is loaded in such a way to fail *quietly* and
and generate `*.log` files: `error.log` lists any issues encounted upon
loading and `startup.log` lists successfull startup and details of startup (WIP).


- `init.lisp`      --> User init loaded by lem, sets up ASDF's registry and loads `:lem-config` system
- `lem-config.asd` --> System definition for this configuration
- `source/`        --> Contains souce files for this configuration
- `extensions/`    --> WIP where lem extension systems will be held
- `assets/`        --> Where images, lem.desktop, and related stuff is held

## TODOs (Wish List)

  - Build out more personal keybindings
  - Fix bug in `fp-up-directory` function, hangs on some special chars.
  - Configure `C-x k` to actually kill the current buffer/file
  - Build an extension analogous to Emacs' erc-mode
  - TBD

## References:
  - lem source: https://github.com/lem-project/lem
  - General configuration layout inspirations:
    - https://github.com/garlic0x1/.lem/
    - https://github.com/fukamachi/.lem
  - `file-prompt` & `time-stamp` customizations inspiration:
    - https://github.com/vindarel/lem-init
  - Paredit configuration inspiration:
    - https://github.com/Gavinok/.lem
  - TBD

