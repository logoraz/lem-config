(uiop:define-package #:lem-config/source/all
  (:nicknames #:lem-config)
  (:use #:cl #:lem)
  (:use-reexport #:lem-config/source/appearance
                 #:lem-config/source/keybindings
                 #:lem-config/source/completions
                 #:lem-config/source/paredit
                 #:lem-config/source/file-prompt
                 #:lem-config/source/time-stamp
                 #:lem-config/source/utilities
                 #:lem-config/source/playground))