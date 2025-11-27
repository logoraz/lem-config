# Common Lisp Style Guide for lem-config

## Package Definition Order

```lisp
(defpackage #:package-name
  (:use #:cl)                    ; 1. Foundation - what you inherit
  (:import-from #:package        ; 2. Specific external symbols
                #:symbol-1
                #:symbol-2)
  (:local-nicknames (#:u #:uiop) ; 3. Convenience prefixes
                    (#:a #:alexandria))
  (:export #:public-symbol-1     ; 4. Public API
           #:public-symbol-2)
  (:documentation "..."))        ; 5. Package description
```

## When to Use `import-from` vs `local-nicknames`

### Use `import-from` when:
- **1-5 symbols** from a package
- You want symbols to feel "native" (no prefix in code)
- Domain-specific packages (lem-core, your own packages)
- Symbols from subpackages (e.g., `uiop/filesystem`)

```lisp
(:import-from #:lem-core
              #:lem-home)
(:import-from #:uiop/filesystem
              #:ensure-directories-exist)

;; Usage - clean, no prefixes:
(lem-home)
(ensure-directories-exist path)
```

### Use `local-nicknames` when:
- **6+ symbols** from a package
- Utility libraries used throughout (uiop, alexandria, str)
- You want symbol origin to remain clear
- Avoiding namespace pollution

```lisp
(:local-nicknames (#:u #:uiop))

;; Usage - clear origin with short prefix:
(u:getenv "PATH")
(u:split-string ...)
(u:directory-files ...)
```

### Common nickname conventions:
- `#:u` for `#:uiop` (de facto standard)
- `#:a` for `#:alexandria`
- `#:str` for `#:str`

### Combined approach (recommended):
```lisp
(defpackage #:my-package
  (:use #:cl)
  ;; Few specific symbols → import
  (:import-from #:lem-core
                #:lem-home)
  (:import-from #:uiop/filesystem
                #:ensure-directories-exist)
  ;; Many utility functions → nickname
  (:local-nicknames (#:u #:uiop))
  (:export ...))
```

## Symbol Access Rules

### Never use `::` (double colon)
```lisp
;; BAD - accessing internal/private symbols
(package::internal-symbol)

;; GOOD - only access exported symbols
(package:exported-symbol)
```

**Exception:** Very rare cases for debugging or implementation-specific code, but wrap in `#+implementation` reader conditionals.

### Prefer explicit imports over repeated prefixes
```lisp
;; If used once or twice - prefix is fine:
(uiop:getenv "PATH")

;; If used 6+ times - use nickname or import:
(:local-nicknames (#:u #:uiop))
(u:getenv "PATH")
```

## ASDF System Definition

### Component loading
```lisp
:serial t    ; DEFAULT - load sequentially in order (safe)
:serial nil  ; Allow parallel loading (must declare dependencies)
```

**Note:** `:parallel t` is **not valid** - only `:serial` exists.

### For single-file systems:
```lisp
:serial t  ; or omit entirely (it's the default)
```

### For multi-file systems with dependencies:
```lisp
:serial nil  ; Explicitly allow parallel loading
:components
((:file "utilities")              ; No dependencies
 (:file "commands")               ; No dependencies
 (:file "keybindings" :depends-on ("commands")))
```

## Error Handling

### Always wrap risky operations
```lisp
(handler-case
    (potentially-failing-operation)
  (error (e)
    (format t "Warning: ~A~%" e)
    nil))  ; Return safe default
```

### For init files - never crash on errors
```lisp
;; BAD - errors propagate and crash startup
(asdf:load-system :my-config)

;; GOOD - catch and log, continue startup
(handler-case
    (asdf:load-system :my-config)
  (error (condition)
    (message "Warning: config failed to load - continuing with defaults")
    (save-log-file "logs/error.log" condition)))
```

## File Operations

### Use UIOP for portability
```lisp
;; Delete files
(uiop:delete-file-if-exists path)  ; Safe - no error if missing

;; Multiple files
(dolist (file (uiop:directory-files dir "pattern*"))
  (uiop:delete-file-if-exists file))

;; Ensure directories exist
(ensure-directories-exist path)  ; CL standard - available via (:use #:cl)
```

### Path construction
```lisp
;; BAD - doesn't work correctly
(merge-pathnames program dir)  ; Creates /usr/program instead of /usr/bin/program

;; GOOD - ensure directory has trailing slash
(let ((dir-path (uiop:ensure-directory-pathname dir)))
  (merge-pathnames program dir-path))  ; Creates /usr/bin/program
```

## Platform-Specific Code

### Use reader conditionals
```lisp
#+unix (unix-specific-code)
#+windows (windows-specific-code)
#+sbcl (sbcl-specific-code)

;; Combined
#+(or unix linux darwin) (posix-code)
```

### For optional imports
```lisp
(:import-from #:uiop
              #:getenv
              #:split-string
              #+unix #:some-unix-only-function)
```

### SBCL-specific code
```lisp
;; When using SBCL internals, be explicit
#+sbcl
(let ((stat (sb-posix:stat path)))
  (sb-posix:stat-mode stat))
```

## Documentation

### Function docstrings
```lisp
(defun my-function (arg)
  "Short description of what function does.
  
  Longer explanation if needed. Multiple paragraphs okay."
  (body))
```

### Note platform requirements in docstrings
```lisp
(defun executable-find (program)
  "Find executable PROGRAM in PATH, return full path or NIL if not found (SBCL only)"
  ...)
```

## Code Organization

### Section headers
```lisp
;;; =============================================================================
;;; Section Name
;;; =============================================================================

(defun function-1 ...)
(defun function-2 ...)
```

### File structure
1. Package definition
2. `(in-package ...)`
3. Section headers with related functions
4. Keep related functionality together

## Naming Conventions

### Packages
```lisp
#:project-name/module  ; Hierarchical with slash
#:lem-config/utilities
#:lem-config/commands
```

### Functions
```lisp
(defun verb-noun (args) ...)  ; Action-oriented
(defun executable-find ...)
(defun cleanup-debug-logs ...)
```

### Predicates (end with -p)
```lisp
(defun file-exists-p ...)
(defun executable-p ...)
```

## Common Pitfalls to Avoid

### 1. Wrong parenthesis count
Use editor paren-matching or count carefully. Lem's paredit mode helps.

### 2. Not marking directories as directories
```lisp
;; BAD
(merge-pathnames "file" "/usr/bin")  ; Treats "bin" as file

;; GOOD
(merge-pathnames "file" (uiop:ensure-directory-pathname "/usr/bin"))
```

### 3. Using non-existent UIOP functions
Not all functions you expect exist in UIOP. Verify before using:
- `uiop:file-mode` ❌ doesn't exist
- `uiop:find-program` ❌ doesn't exist (in some versions)
- Use `sb-posix:stat` for file modes on SBCL ✅

### 4. Assuming :parallel exists in ASDF
```lisp
:parallel t  ; ❌ NOT VALID

:serial nil  ; ✅ This enables parallel loading
```

### 5. Not handling errors in init files
Init files should **never** crash - always wrap in error handlers.

## Quick Reference

### Standard Common Lisp nicknames
```lisp
(:local-nicknames 
  (#:u #:uiop)        ; Utilities
  (#:a #:alexandria)  ; Data structures & utilities
  (#:str #:str))      ; String manipulation
```

### Common UIOP imports
```lisp
(:import-from #:uiop
              #:getenv              ; Environment variables
              #:split-string        ; String splitting
              #:directory-files     ; List files
              #:delete-file-if-exists  ; Safe delete
              #:ensure-directory-pathname  ; Path handling
              #:run-program)        ; Execute commands
```

### Common lem-core imports
```lisp
(:import-from #:lem-core
              #:lem-home)  ; Config directory
```

## Summary Philosophy

1. **Explicit is better than implicit** - declare dependencies clearly
2. **Fail gracefully** - never crash on errors, especially in init files
3. **Be consistent** - follow the import-from vs local-nicknames guidelines
4. **Document platform requirements** - note SBCL-only, Unix-only code
5. **Use UIOP for portability** - avoid platform-specific code when possible
6. **Respect package boundaries** - never use `::` for internal symbols
