+++
title = "Emacs Configuration"
author = ["Mario Liguori"]
date = 2022-06-13
lastmod = 2023-04-10T01:12:48+02:00
tags = ["emacs", "configuration", "elisp", "dotfiles"]
categories = ["workflow"]
draft = false
+++

## Preface {#preface}


### Overview {#overview}

This is my personal configuration of `GNU Emacs`. Many of these settings have been stolen from research and other configurations.
I have a very basic use-case, and this configuration is, currently, pretty straightforward to read.

Probably I'm using this ~~operating system~~ editor/interactive-interpreter as daily driver now. In the last section I explain my "experience" and reasons why I'm using Emacs.


#### Notes on this document {#notes-on-this-document}

Every file is commented in a decent way, but I'm not very verbose. Sometimes I describe every single things written (in a decent way, I guess), other times, I'm awful.

Anyway, this document is not intended as a way to show my `elisp-fu` or something like that. When you read this, imagine a diary, a tale of my [mis]adventures 🤣.


### Before GUI: "early init" {#before-gui-early-init}

Is good practice to define an `early-init.el` file: this kind of approach provides better loading for essential stuff.

-   There are some tweaks taken from [DOOM Emacs](https://github.com/doomemacs/doomemacs), [David Wilson](https://github.com/daviwil/dotfiles/blob/master/Emacs.org), [Protesilaos Stavrou](https://protesilaos.com/emacs/dotemacs)...but I'll put some credits at the end of this document, along with useful resources.
-   The package manager, [straight.el](https://github.com/radian-software/straight.el), provides reproducibility (like Nix and Guix) with recipes, allows the editing of packages and manual version control operations on repos. [Here](https://github.com/radian-software/straight.el#advantages-of-straightel-5) the list of advantages.

<!--listend-->

```emacs-lisp
;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;;; Commentary:

;; Early init file has been introduced in Emacs 27, it is a file loaded
;; before GUI is initialized, so unwanted elements are here.
;; Example: scroll-bars, fringes, menu-bar, tool-bar.

;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'.
(setq gc-cons-threshold  most-positive-fixnum)

;; Add load-path for submodules
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set a better directory to store the native comp cache
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (add-to-list 'native-comp-eln-load-path (expand-file-name "var/eln-cache/" user-emacs-directory)))

;; From DOOM
;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(when (and (fboundp 'native-comp-available-p)
     (native-comp-available-p))
  (setq native-comp-deferred-compilation nil)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil))

;; Another trick from DOOM
(unless (or (daemonp)
            noninteractive
            init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; `file-name-handler-alist' is consulted on each `require', `load' and
    ;; various path/io functions. You get a minor speed up by unsetting this.
    ;; Some warning, however: this could cause problems on builds of Emacs where
    ;; its site lisp files aren't byte-compiled and we're forced to load the
    ;; *.el.gz files (e.g. on Alpine).
    (setq-default file-name-handler-alist nil)
    ;; ...but restore `file-name-handler-alist' later, because it is needed for
    ;; handling encrypted or compressed files, among other things.
    (defun doom-reset-file-handler-alist-h ()
      (setq file-name-handler-alist
            ;; Merge instead of overwrite because there may have bene changes to
            ;; `file-name-handler-alist' since startup we want to preserve.
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h 101))

  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay))))

;; From DOOM
;;
;; NOTE: In DOOM these are defined in another file, not in early init, that's horrible because
;; starting a client where this settings are defined later causes a little flash at startup (before redisplay)
;; where menu-bar is present.
;;
;; Not calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because they do extra and unnecessary work that can be more
;; concisely and efficiently expressed with these six lines:
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      column-number-mode t
      fringe-mode 10)

;; Minor message for gc after loading
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil
      package-quickstart nil)

;; Configure and bootstrap `straight.el'
(setq straight-repository-branch "develop"
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-profiles `((nil . ,(expand-file-name "straight/versions/lock.el" user-emacs-directory))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Additional post-setup of `straight.el'
(require 'straight-x)
(defalias 'straight-ಠ_ಠ-mode nil)

;;; early-init.el ends here
```


### Mandatory settings for the "init" {#mandatory-settings-for-the-init}

Note that `init.el` is mandatory, however I'm tangling it from this `.org` file (`Emacs.org`).

I have decided to tangle this document in `init.el` because I want to keep a few things in the main directory, while populating submodules to manage all of this.

```emacs-lisp

;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;; NOTE: This file is generated from `Emacs.org`!

;;; Code:

;; We don't want user customizations in `init.el`, instead we use `custom.el`
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Disable damn sleep!
;; Yep, it's mandatory, that's the worst keybind ever, and should be remapped
(global-unset-key (kbd "C-z"))

```


## Core settings {#core-settings}


### Convenience {#convenience}

Functions to determine if we are using a Nix installation of Emacs, or not, then we set our configuration path.

```emacs-lisp

(defun archer-using-nix-p ()
  "Verifies if the running Emacs executable is under the `/nix/store/' path."
  (unless (or (equal system-type 'ms-dos)
              (equal system-type 'windows-nt))
    ;; Since there is no windows implementation of nix
    (string-prefix-p "/nix/store/"
                     (file-truename
                      (executable-find
                       (car command-line-args))))))

(defvar archer-config-path
  (let ((real-path (expand-file-name
                    ".dotfiles/home/modules/editors/emacs/config/"
                    (getenv "HOME"))))
    (if (and (archer-using-nix-p)
             (file-exists-p real-path))
        (expand-file-name real-path)
      (expand-file-name user-emacs-directory))))

```


### Packages bootstrap {#packages-bootstrap}

We are requiring `init-setup`, where configuration tools based on macros (e.g. `use-package`, `leaf.el`, `setup.el`) are initialized. I'm using [setup.el](https://git.sr.ht/~pkal/setup) right now. Compared to `use-package`, `setup.el` is less declarative: you have more control, I would say that it's similar to vanilla Emacs configuration, but less verbose and with easy definition of new macros.

I also install [blackout.el](https://github.com/radian-software/blackout) (and define a macro with `setup.el`) here, to manage modes displayed in the mode-line.

```emacs-lisp

;; Require package management file
(require 'init-setup)

```

```emacs-lisp
;;; init-setup.el --- `setup.el' configuration -*- lexical-binding: t -*-

;;; Commentary:

;; The package `setup.el' is configured here, with new forms and settings.

;;; Code:

(straight-use-package 'setup)
(require 'setup)

;; Forms section

(setup-define :quit
  'setup-quit
  :documentation "Always stop evaluating the body.")

(setup-define :needs
  (lambda (executable)
    `(unless (executable-find ,executable)
       ,(setup-quit)))
  :documentation "If EXECUTABLE is not in the path, stop here."
  :repeatable 1)

(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func) '(quote function))
                  (cadr func)
                func)))
      `(unless (fboundp (quote ,fn))
         (autoload (function ,fn) ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload COMMAND if not already bound."
  :repeatable t
  :signature '(FUNC ...))

(setup-define :load-after
  (lambda (features &rest body)
    (let ((body `(progn
                   (require ',(setup-get 'feature))
                   ,@body)))
      (dolist (feature (nreverse (ensure-list features)))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :indent 1
  :documentation "Load the current feature after FEATURES.")

(setup-define :with-after
  (lambda (features &rest body)
    (let ((body `(progn ,@body)))
      (dolist (feature (nreverse (ensure-list features)))
        (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :indent 1
  :documentation "Evaluate BODY after FEATURES are loaded.")

(setup-define :hooks
  (lambda (hook func)
    `(add-hook ',hook #',func))
  :documentation "Add pairs of hooks."
  :repeatable t)

(setup-define :face
  (lambda (face spec) `(custom-set-faces (quote (,face ,spec))))
  :documentation "Customize FACE to SPEC."
  :signature '(face spec ...)
  :debug '(setup)
  :repeatable t
  :after-loaded t)

;; Blackout to hide minor modes
(straight-use-package 'blackout)
(setup-define :blackout
  (lambda (&optional mode)
    (let* ((mode (or mode (setup-get 'mode)))
           (mode (if (string-match-p "-mode\\'" (symbol-name mode))
                     mode
                   (intern (format "%s-mode" mode)))))
      `(blackout ',mode)))
  :documentation "Hide the mode-line lighter of the current mode with blackout.
MODE can be specified manually, and override the current-mode."
  :after-loaded t)

;; From https://git.acdw.net/emacs/tree/lisp/+setup.el
(defun +setup-warn (message &rest args)
  "Warn the user with that something bad happened in `setup'.
MESSAGE should be formatted (optionally) with ARGS"
  (display-warning 'setup (format message args)))

(defun +setup-wrap-to-demote-errors (body name)
  "Wrap BODY in a `with-demoted-errors' block.
This behavior is prevented if `setup-attributes' contains the
symbol `without-error-demotion'.

This function differs from `setup-wrap-to-demote-errors' in that
it includes the NAME of the setup form in the warning output."
  (if (memq 'without-error-demotion setup-attributes)
      body
    `(with-demoted-errors ,(format "Error in setup form on line %d (%s): %%S"
                                   (line-number-at-pos)
                                   name)
       ,body)))

(add-to-list 'setup-modifier-list '+setup-wrap-to-demote-errors)
(unless (memq debug-on-error '(nil init))
  (define-advice setup (:around (fn head &rest args) +setup-report)
    (+with-progress ((format "[Setup] %S..." head))
      (apply fn head args))))

;; Integration with `straight.el'
(defun setup--straight-handle-arg (arg var)
  (cond
   ((and (boundp var) (symbol-value var)) t)
   ((keywordp arg) (set var t))
   ((functionp arg) (set var nil) (funcall arg))
   ((listp arg) (set var nil) arg)))

(with-eval-after-load 'straight
  (setup-define :pkg
    (lambda (recipe &rest predicates)
      (let* ((skp (make-symbol "straight-keyword-p"))
             (straight-use-p (cl-mapcar
                              (lambda (f) (setup--straight-handle-arg f skp)) predicates))
             (form `(unless (and ,@straight-use-p
                                 (condition-case e (straight-use-package ',recipe)
                                   (error (+setup-warn ":straight error: %S" ',recipe)
                                          ,(setup-quit))
                                   (:success t)))
                      ,(setup-quit))))
        ;; Keyword arguments --- :quit is special and should short-circuit
        (if (memq :quit predicates)
            (setq form `,(setup-quit))
          ;; Otherwise, handle the rest of them ...
          (when-let ((after (cadr (memq :after predicates))))
            (setq form `(with-eval-after-load ,(if (eq after t) (setup-get 'feature) after)
                          ,form))))
        ;; Finally ...
        form))
    :documentation "Install RECIPE with `straight-use-package'.
If PREDICATES are given, only install RECIPE if all of them return non-nil.
The following keyword arguments are also recognized:
- :quit          --- immediately stop evaluating.  Good for commenting.
- :after FEATURE --- only install RECIPE after FEATURE is loaded.
                     If FEATURE is t, install RECIPE after the current feature."
    :repeatable nil
    :indent 1
    :shorthand (lambda (sexp)
                 (let ((recipe (cadr sexp)))
                   (or (car-safe recipe) recipe)))))

(provide 'init-setup)
;;; init-setup.el ends here
```


### Performances enhancement {#performances-enhancement}

[GCMH](https://github.com/emacsmirror/gcmh) allows the auto-regulation of garbage collector based on idle timers. During normal use a high GC threshold is set; when idling GC is triggered and a low threshold is set.

Right now I'm good with 16MB for high threshold.

Other tweaks in this section have been stolen from DOOM and other configurations around.

```emacs-lisp

(require 'init-performance)

```

```emacs-lisp
;;; init-performance.el --- Performances enhancement -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain tweaks to obtain better overall performances.

;;; Code:


(setup (:pkg gcmh)
  (:require)
  (:blackout)
  ;; The GC introduces annoying pauses and stuttering into our Emacs experience,
  ;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
  ;; when it's idle. However, if the idle delay is too long, we run the risk of
  ;; runaway memory usage in busy sessions. If it's too low, then we may as well
  ;; not be using gcmh at all.
  (:option gcmh-idle-delay 'auto ; Default 15 seconds
           gcmh-auto-idle-delay-factor 10
           gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-mode 1))

;; Aaand, here other code stolen from DOOM.
;; Performances are really better with this snippet (for me).
(setup tweaks
  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
  ;; is more than enough.
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        inhibit-default-init t
        ;; Shave seconds off startup time by starting the scratch buffer in
        ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
        ;; pull in a ton of packages.
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil)

  ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
  (setq idle-update-delay 1.0)
  ;; Resizing the Emacs frame can be a terribly expensive part of changing the
  ;; font. By inhibiting this, we halve startup times, particularly when we use
  ;; fonts that are larger than the system default (which would resize the frame).
  (setq frame-inhibit-implied-resize t)

  ;; PGTK builds only: this timeout adds latency to frame operations, like
  ;; `make-frame-invisible', which are frequently called without a guard because
  ;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
  ;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
  ;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
  (setq pgtk-wait-for-event-timeout 0.001)

  ;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
  ;; receiving input, which should help a little with scrolling performance.
  (setq redisplay-skip-fontification-on-input t))

(provide 'init-performance)
;;; init-appearance.el ends here
```


### Pick me up mom, I'm scared! {#pick-me-up-mom-i-m-scared}

Sometimes we forget shortcuts as we type them, [which-key](https://github.com/justbur/emacs-which-key) is a minor mode for Emacs that displays the key bindings following your currently entered incomplete command in a popup.

```emacs-lisp

(require 'init-help)

```

```emacs-lisp
;;; init-help.el --- Sometimes we need help from someone/something :) -*- lexical-binding: t -*-

;;; Commentary:

;; The minibuffer is our best friend, let's use it more with extensions.

;;; Code:

(setup (:pkg which-key)
  (:blackout)
  (:option which-key-idle-delay 0.2)
  (which-key-mode 1))

(setup (:pkg helpful :quit)
  (:bind "C-h f"    helpful-callable
         "C-h v"    helpful-variable
         "C-h k"    helpful-key
         "C-h C"    helpful-command
         "C-c C-d"  helpful-at-point))

(provide 'init-help)
;;; init-help.el ends here
```


### Appearance {#appearance}

In this section are contained line-numbers settings, modeline related configuration, minor tweaks for icons (needed also for dashboard) and colors.


#### Font {#font}

Readability is important, another package from Protesilaos, much more!
Currently using [Victor Mono](https://rubjo.github.io/victor-mono/) as font, I love it, also for variable-pitch face.

```emacs-lisp

(require 'init-fonts)

```

```emacs-lisp
;;; init-fonts.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; Only font configuration, nothing to say.

;;; Code:

(defgroup archer-faces()
  "Extensions for faces."
  :group 'faces)

(defcustom archer-font-height 120
  "Variable that specifies the font height."
  :type 'integer
  :group 'archer-faces)

(setup (:pkg fontaine)
  (:option x-underline-at-descent-line nil
           use-default-font-for-symbols t)

  (unless (version< emacs-version "28")
    (setq-default text-scale-remap-header-line t))

  (:option archer-font-height (pcase (system-name)
                                ("quietfrost" 180)
                                ("mate" 140)))

  (:option fontaine-latest-state-file (locate-user-emacs-file "var/fontaine-state.eld"))

  (:option fontaine-presets
           `((victor
              :default-family "VictorMono Nerd Font"
              :default-height ,archer-font-height)))

  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'victor))

  (:with-hook kill-emacs-hook
    (:hook fontaine-store-latest-preset))

  (:with-hook (modus-themes-after-load-theme-hook ef-themes-post-load-hook)
    (:hook fontaine-apply-current-preset)))

(provide 'init-fonts)
;;; init-fonts.el ends here
```


#### Colors and general UI {#colors-and-general-ui}

I'm currently using [Modus Themes](https://protesilaos.com/emacs/modus-themes), with [Circadian](https://github.com/guidoschmidt/circadian.el) to set light/dark version, based on time. It's possible to switch themes on sunrise and sunset. Protesilaos made a great work, and these themes are, indeed, built into Emacs (but I always get the packaged version :D)

```emacs-lisp

(require 'init-themes)

```

```emacs-lisp
;;; init-themes.el --- Themes -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration of `modus-themes' and `ef-themes', high accessibility themes by Protesilaos.

;;; Code:


(setup (:pkg modus-themes)
  ;; Preferences
  (:option modus-themes-org-blocks 'gray-background
           modus-themes-mixed-fonts nil
           modus-themes-variable-pitch-ui nil)

  ;; Overrides
  (:option modus-themes-common-palette-overrides
           ;; Modeline
           '((bg-mode-line-active bg-blue-subtle)
             (fg-mode-line-active fg-main)
             (border-mode-line-active blue-intense)
             ;; Region
             (bg-region bg-lavender)
             (fg-region unspecified)
             ;; Mouse Hovers
             (bg-hover bg-yellow-intense)
             ;; Fringe
             (fringe unspecified)
             ;; Inline code in prose (markup)
             (prose-block fg-dim)
             (prose-code green-cooler)
             (prose-done green)
             (prose-macro magenta-cooler)
             (prose-metadata fg-dim)
             (prose-metadata-value fg-alt)
             (prose-table fg-alt)
             (prose-tag magenta-faint)
             (prose-todo red)
             (prose-verbatim magenta-warmer)
             ;; Syntax
             (comment yellow-faint)
             (string green-warmer)
             ;; Checkers
             (underline-err red-faint)
             (underline-warning yellow-faint)
             (underline-note cyan-faint)
             ;; Links - No underlines
             (underline-link unspecified)
             (underline-link-visited unspecified)
             (underline-link-symbolic unspecified)
             ;; Box buttons
             (bg-button-active bg-main)
             (fg-button-active fg-main)
             (bg-button-inactive bg-inactive)
             (fg-button-inactive "gray50")
             ;; Prompts
             (fg-prompt cyan)
             (bg-prompt bg-cyan-nuanced)
             ;; Completion
             (fg-completion-match-0 fg-main)
             (fg-completion-match-1 fg-main)
             (fg-completion-match-2 fg-main)
             (fg-completion-match-3 fg-main)
             (bg-completion-match-0 bg-blue-subtle)
             (bg-completion-match-1 bg-yellow-subtle)
             (bg-completion-match-2 bg-cyan-subtle)
             (bg-completion-match-3 bg-red-subtle)
             ;; Mail citations
             (mail-cite-0 blue)
             (mail-cite-1 yellow)
             (mail-cite-2 green)
             (mail-cite-3 magenta)
             (mail-part magenta-cooler)
             (mail-recipient cyan)
             (mail-subject red-warmer)
             (mail-other cyan-cooler)
             ;; Line numbers
             (fg-line-number-inactive "gray50")
             (fg-line-number-active fg-main)
             (bg-line-number-inactive unspecified)
             (bg-line-number-active unspecified)))

  (modus-themes-select 'modus-operandi))

(setup (:pkg ef-themes))

;; I set circadian in the configuration of my themes
(setup (:pkg circadian)
  (:load-after modus-themes)
  (:option circadian-themes '(("8:00" . modus-operandi)
                              ("20:00" . modus-vivendi)))
  (circadian-setup))

(provide 'init-themes)
;;; init-themes.el ends here
```


#### Minor UI settings {#minor-ui-settings}

Nothing special, just `all-the-icons` and misc settings.

```emacs-lisp

(require 'init-appearance)

```

```emacs-lisp
;;; init-appearance.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain appearance settings stuff.

;;; Code:

(setup appearance
  ;; A simple frame title
  (setq frame-title-format '("%b – Emacs")
        icon-title-format frame-title-format)

  ;; Stuff
  (setq calendar-date-style 'european)
  (setq display-time-default-load-average nil)
  (setq highlight-nonselected-windows nil)
  (setq echo-keystrokes 0.1)

  ;; Other graphical stuff
  (setq visible-bell nil)
  (setq x-gtk-use-system-tooltips t)
  (setq x-stretch-cursor nil)

  ;; Dialogs
  (setq use-dialog-box nil      ; Mouse events dialog
        use-file-dialog nil)    ; Disable dialog for files

  ;; Cursor
  (setq-default cursor-in-non-selected-windows nil)
  (setq-default cursor-type 'bar)
  (blink-cursor-mode 0)

  ;; Bidirectional settings
  (setq-default bidi-display-reordering 'left-to-right)
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; Lines related
  (setq-default truncate-lines nil)
  (setq-default visual-line-mode t)

  (setq-default indicate-buffer-boundaries nil))

;; You must run `all-the-icons-install-fonts` the first time.
(setup (:pkg all-the-icons)
  (:require all-the-icons))

(provide 'init-appearance)
;;; init-appearance.el ends here
```


#### Modeline {#modeline}

Just modeline customized.

```emacs-lisp

(require 'init-modeline)

```

```emacs-lisp
;;; init-modeline.el --- Modeline customization -*- lexical-binding: t -*-

;;; Commentary:

;; Modeline customization and other useless/cute packages.

;;; Code:
(setup modeline
  (unless (version< emacs-version "28")
    (setq mode-line-compact nil)
    (setq mode-line-position-column-line-format '("[L%l:C%C]")))

  (setq mode-line-percent-position '(-3 "%P"))
  (setq mode-line-defining-kbd-macro
        (propertize " Macro" 'face 'mode-line-emphasis))

  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  mode-line-client
                  "  "
                  mode-line-mule-info
                  "  "
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "  "
                  mode-line-position
                  (vc-mode vc-mode)
                  "  "
                  mode-line-modes
                  mode-line-misc-info
                  mode-line-end-spaces)))


;; <https://github.com/minad/recursion-indicator>.
(setup (:pkg recursion-indicator)
  (:option recursion-indicator-general (concat "general" (all-the-icons-material "cached" :v-adjust -0.1))
           recursion-indicator-minibuffer (concat "minibuffer " (all-the-icons-material "cached" :v-adjust -0.1)))

  (setq-default mode-line-modes
                (seq-filter (lambda (s)
                              (not (and (stringp s)
                                        (string-match-p
                                         "^\\(%\\[\\|%\\]\\)$" s))))
                            mode-line-modes))

  (recursion-indicator-mode 1))

;;; Keycast mode
(setup (:pkg keycast)
  ;; For `keycast-mode'
  (:option keycast-mode-line-window-predicate #'keycast-active-frame-bottom-right-p
           keycast-separator-width 1
           keycast-mode-line-remove-tail-elements nil
           keycast-mode-line-format "%3s%k%c%r"
           keycast-mode-line-insert-after 'mode-line-misc-info)

  ;; For `keycast-log-mode'
  (:option keycast-log-format "%-20K%C\n"
           keycast-log-newest-first t
           keycast-log-frame-alist '((minibuffer . nil)))

  ;; Based on Prot's configuration
  (:when-loaded
    (dolist (input '(self-insert-command
                     org-self-insert-command))
      (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

    (dolist (event '(mouse-event-p
                     mouse-movement-p
                     mwheel-scroll))
      (add-to-list 'keycast-substitute-alist `(,event nil)))))

(provide 'init-modeline)
;;; init-modeline.el ends here
```


#### Dashboard Configuration {#dashboard-configuration}

Useless and cute dashboard, nothing to say, and there are minor tweaks to make it work with server-mode and Emacs PGTK/NativeComp.

```emacs-lisp

(require 'init-dash)

```

Here the `init-dash.el` file.

```emacs-lisp
;;; init-dash.el --- Dashboard configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration of my dashboard, loaded at startup.

;;; Code:

(setup (:pkg dashboard)
  (:option dashboard-banner-logo-title "SUCK(EMAC)S - Personal Workspace"
           dashboard-startup-banner (expand-file-name "img/stallman.png" user-emacs-directory)
           dashboard-center-content t
           ;; Icons
           dashboard-set-heading-icons t
           dashboard-set-file-icons t
           dashboard-items '((recents . 5)
                             (bookmarks . 5))

           ;; Headings
           dashboard-heading-icons '((recents   . "history")
                                     (bookmarks . "bookmark")
                                     (agenda    . "calendar")
                                     (projects  . "briefcase")
                                     (registers . "database"))

           ;; Navigator under banner
           dashboard-set-navigator t
           dashboard-navigator-buttons
           `(((,(all-the-icons-faicon "archive" :height 1.1 :v-adjust 0.0)
               "Update Packages"
               "Click to updates your packages"
               (lambda (&rest _) (straight-pull-all)))))

           ;; Footer
           dashboard-footer-icon (all-the-icons-fileicon "emacs" :face 'font-lock-keyword-face))

  ;; This is required with PGTK!
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook)

  (:with-hook after-init-hook
    (:hook dashboard-insert-startupify-lists))

  (:with-hook server-after-make-frame-hook
    (:hook dashboard-refresh-buffer)))

(provide 'init-dash)
;;; init-dash.el ends here
```


## Interface interaction {#interface-interaction}

This section contains my file and buffer related configurations. Nothing special.


### Editing enhancement {#editing-enhancement}

Tweaks present here:

-   Scroll (and smooth scroll for Emacs &gt;= 29) and horizontal scroll with mouse;
-   Truncate lines hook for `prog-mode`;
-   Electric-pair mode and show-paren;
-   Autorevert files after changes;
-   [Undo Tree](https://www.emacswiki.org/emacs/UndoTree) mode for simpler undo-redo (and visual branches!).
-   Rainbow-mode;
-   Delete-selection mode to overwrite selected regions;
-   Drag-stuff to...drag stuff around;
-   etc.

<!--listend-->

```emacs-lisp

(require 'init-editing)

```

```emacs-lisp
;;; init-editing.el --- Basic editing configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file is pretty simple, it only contains editing related utilities and preferences.
;; It's still experimental and very poor, so I only consider it a starting point.

;;; Code:

;;
;;; General

;; Force UTF-8
(setup encoding
  (setq coding-system-for-read 'utf-8-unix)
  (setq coding-system-for-write 'utf-8-unix)
  (setq default-process-coding-system '(utf-8-unix utf-8-unix))
  (setq locale-coding-system 'utf-8-unix)
  (setq selection-coding-system 'utf-8)
  (setq x-select-request-type nil)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  (set-clipboard-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-language-environment "UTF-8")
  (set-selection-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8-unix))

;;
;;; Keep history and keep the order

;; The `no-littering` package to keep folders where we edit files and the Emacs configuration folder clean.
(setup (:pkg no-littering)
  ;; The package doesn't set this by default so we must place
  ;; auto save files in the same path as it uses for sessions
  (:option auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/")))))

(setup saveplace
  (:option save-place-file (expand-file-name "var/saveplace" user-emacs-directory))
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

(setup backup
  (:option backup-directory-alist `(("." . ,(expand-file-name "var/backup" user-emacs-directory))))
  (setq backup-by-copying t)
  (setq version-control t)
  (setq delete-old-versions t)
  (setq kept-new-versions 5)
  (setq kept-old-versions 2)
  (setq create-lockfiles nil))

;;
;;; Lines related

(setup display-line-numbers
  ;; Defaults
  (setq-default display-line-numbers-widen t)
  (setq-default display-line-numbers-width 3)

  ;; Preferences
  (:option display-line-numbers-type 'relative
           display-line-numbers-width-start nil
           display-line-numbers-grow-only t)

  ;; Hooks
  (:with-hook (prog-mode-hook text-mode-hook conf-mode-hook)
    (:hook (lambda () (display-line-numbers-mode 1))))
  (:with-hook (org-mode-hook)
    (:hook (lambda () (display-line-numbers-mode 0)))))

(setup hl-line
  (:with-mode (prog-mode dired-mode)
    (:hook hl-line-mode)))

;;
;;; Scrolling

(setup scrolling
  ;; Enable smooth scroll on Emacs 29
  (unless (version< emacs-version "29")
    (pixel-scroll-precision-mode 1))

  ;; Vertical scroll
  (setq scroll-step 1
        scroll-margin 10
        ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
        ;; for tall lines.
        auto-window-vscroll nil)

  ;; Horizontal scroll
  (setq hscroll-margin 16
        hscroll-step 1
        auto-hscroll-mode t)

  ;; General tweaks

  ;; More performant rapid scrolling over unfontified regions. May cause brief
  ;; spells of inaccurate syntax highlighting right after scrolling, which should
  ;; quickly self-correct.
  (setq fast-but-imprecise-scrolling t)

  ;; Emacs spends too much effort recentering the screen if you scroll the
  ;; cursor more than N lines past window edges (where N is the settings of
  ;; `scroll-conservatively'). This is especially slow in larger files
  ;; during large-scale scrolling commands. If kept over 100, the window is
  ;; never automatically re-centered.
  (setq scroll-conservatively 101
        scroll-preserve-screen-position t
        scroll-preserve-screen-position t))

(setup mouse
  ;; Movement related
  (setq focus-follows-mouse t)
  (setq make-pointer-invisible t)
  (setq mouse-autoselect-window t)

  ;; Scroll
  (setq mouse-wheel-scroll-amount '(3 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 2)

  ;; Behavior
  (setq mouse-wheel-follow-mouse t)
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-1-click-follows-link t)
  (setq mouse-yank-at-point t)

  (:global "<mouse-2>" clipboard-yank))

(setup elec-pair
  (electric-pair-mode 1))

(setup paren
  (:option show-paren-style 'parenthesis
           show-paren-when-point-in-periphery t
           show-paren-when-point-inside-paren nil)
  (show-paren-mode 1))

(setup selection
  (setq save-interprogram-paste-before-kill t)
  (setq kill-do-not-save-duplicates t)
  (setq select-enable-clipboard t)
  (setq select-enable-primary nil))

(setup (:require delsel)
  (:blackout delete-selection)
  (:with-hook after-init-hook
    (:hook delete-selection-mode)))

(setup (:pkg drag-stuff)
  (:blackout)
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(setup (:pkg goto-last-change)
  (:global "C-z" goto-last-change))

(setup (:require autorevert)
  (:blackout auto-revert)
  (setq auto-revert-verbose t)
  (setq global-auto-revert-non-file-buffers t)
  (:with-hook after-init-hook
    (:hook global-auto-revert-mode)))

(setup (:require so-long)
  (global-so-long-mode 1))

(setup (:pkg diff-hl)
  (:hook-into prog-mode)

  (:with-mode dired-mode
    (:hook diff-hl-dired-mode))

  (:with-after magit
    (:with-hook magit-pre-refresh-hook
      (:hook diff-hl-magit-pre-refresh))
    (:with-hook magit-post-refresh-hook
      (:hook diff-hl-magit-post-refresh))))

(setup long-lines
  (set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
  (set-display-table-slot standard-display-table 'wrap (make-glyph-code ?↩)))

(provide 'init-editing)
;;; init-editing.el ends here
```

<!--list-separator-->

-  Meow

    [Meow](https://github.com/meow-edit/meow) is yet another modal editing mode for Emacs. Meow aims to blend modal editing into Emacs with minimal interference with its original key-bindings, avoiding most of the hassle introduced by key-binding conflicts.

    Keybindings are listed in `init-meow.el`.

    ```emacs-lisp

    (require 'init-meow)

    ```

    ```emacs-lisp
    ;;; init-meow.el --- Meow modal editing -*- lexical-binding: t -*-

    ;;; Commentary:

    ;; A kind of modal editing, alternative to evil and xah-fly-keys.

    ;;; Code:

    (setup (:pkg meow)
      (:require meow)
      (:blackout meow-mode)

      (defun meow-setup ()
        (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
        (meow-motion-overwrite-define-key
         '("j" . meow-next)
         '("k" . meow-prev)
         '("<escape>" . ignore))
        (meow-leader-define-key
         ;; SPC j/k will run the original command in MOTION state.
         '("j" . "H-j")
         '("k" . "H-k")
         ;; Use SPC (0-9) for digit arguments.
         '("1" . meow-digit-argument)
         '("2" . meow-digit-argument)
         '("3" . meow-digit-argument)
         '("4" . meow-digit-argument)
         '("5" . meow-digit-argument)
         '("6" . meow-digit-argument)
         '("7" . meow-digit-argument)
         '("8" . meow-digit-argument)
         '("9" . meow-digit-argument)
         '("0" . meow-digit-argument)
         '("/" . meow-keypad-describe-key)
         '("?" . meow-cheatsheet))
        (meow-normal-define-key
         '("0" . meow-expand-0)
         '("9" . meow-expand-9)
         '("8" . meow-expand-8)
         '("7" . meow-expand-7)
         '("6" . meow-expand-6)
         '("5" . meow-expand-5)
         '("4" . meow-expand-4)
         '("3" . meow-expand-3)
         '("2" . meow-expand-2)
         '("1" . meow-expand-1)
         '("-" . negative-argument)
         '(";" . meow-reverse)
         '("," . meow-inner-of-thing)
         '("." . meow-bounds-of-thing)
         '("[" . meow-beginning-of-thing)
         '("]" . meow-end-of-thing)
         '("a" . meow-append)
         '("A" . meow-open-below)
         '("b" . meow-back-word)
         '("B" . meow-back-symbol)
         '("c" . meow-change)
         '("d" . meow-delete)
         '("D" . meow-backward-delete)
         '("e" . meow-line)
         '("E" . meow-join)
         '("f" . meow-find)
         '("g" . meow-cancel-selection)
         '("G" . meow-grab)
         '("h" . meow-left)
         '("H" . meow-left-expand)
         '("i" . meow-insert)
         '("I" . meow-open-above)
         '("j" . meow-next)
         '("J" . meow-next-expand)
         '("k" . meow-prev)
         '("K" . meow-prev-expand)
         '("l" . meow-right)
         '("L" . meow-right-expand)
         '("m" . meow-mark-word)
         '("M" . meow-mark-symbol)
         '("n" . meow-search)
         '("o" . meow-block)
         '("O" . meow-to-block)
         '("p" . meow-yank)
         '("P" . meow-yank-pop)
         '("q" . meow-quit)
         '("Q" . meow-goto-line)
         '("r" . meow-replace)
         '("R" . meow-swap-grab)
         '("s" . meow-kill)
         '("S" . meow-kill-whole-line)
         '("t" . meow-till)
         '("u" . meow-undo)
         '("U" . meow-undo-in-selection)
         '("v" . meow-visit)
         '("w" . meow-next-word)
         '("W" . meow-next-symbol)
         '("x" . meow-line)
         '("X" . meow-goto-line)
         '("y" . meow-save)
         '("Y" . meow-sync-grab)
         '("z" . meow-pop-selection)
         '("'" . repeat)
         '("<escape>" . ignore)))

      (:when-loaded
        (meow-setup)
        (meow-setup-indicator)
        (meow-global-mode 1)))

    (provide 'init-meow)
    ;;; init-meow.el ends here
    ```


### Windows navigation {#windows-navigation}

Moving around windows can be painful, but some built-in functions save our a\*s.

```emacs-lisp

(require 'init-windows)

```

```emacs-lisp
;;; init-windows.el --- Windows navigation configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Only movement between buffers/frames, nothing special.

;;; Code:

(setup windmove
  ;; Windmove with shift+arrows
  (windmove-default-keybindings)
  (add-hook 'org-shiftup-final-hook    #'windmove-up)
  (add-hook 'org-shiftdown-final-hook  #'windmove-down)
  (add-hook 'org-shiftleft-final-hook  #'windmove-left)
  (add-hook 'org-shiftright-final-hook #'windmove-right))

(setup window
  (setq window-resize-pixelwise nil)

  ;; Splitting around
  (setq split-width-threshold 160
        split-height-threshold nil)

  ;; Dividers
  (setq window-divider-default-right-width 8)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 0)

  (:global "C-x <up>"   enlarge-window
           "C-x <down>" shrink-window
           "C-x {"      shrink-window-horizontally
           "C-x }"      enlarge-window-horizontally))

(setup (:pkg beframe)
  (:option beframe-functions-in-frames '(project-prompt-project-dir)
           beframe-global-buffers '("*scratch*"
                                    "*Messages"
                                    "*Async-native-compile-log*"
                                    "*straight-byte-compilation*"
                                    "*straight-process*"
                                    "*dashboard*"))

  (:with-after consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defvar beframe--consult-source
      `( :name     "Frame-specific buffers (current frame)"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'beframe-buffer-names
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe--consult-source))

  (beframe-mode 1))

(setup (:pkg ace-window)
  (:global "M-o" ace-window
           "M-O" ace-swap-window)
  (setq aw-scope 'frame
        aw-dispatch-always t
        aw-minibuffer-flag t)
  (ace-window-display-mode 1))

(setup (:pkg avy)
  (:global "M-g j" avy-goto-char-timer)
  (setq avy-all-windows nil   ;; only current
        avy-all-windows-alt t ;; all windows with C-u
        avy-single-candidate-jump t
        avy-case-fold-search nil
        avy-timeout-seconds 0.5
        avy-style 'pre))

(provide 'init-windows)
;;; init-windows.el ends here
```


### Buffer management {#buffer-management}

Sometimes buffers are too much, and I think that the classic buffer-menu is meh.
With `ibuffer` I can group buffers in `Gnus` style, customize actions remembering `Dired`, and so on.

```emacs-lisp

(require 'init-buffers)

```

```emacs-lisp
;;; init-buffers.el --- Buffer navigation -*- lexical-binding: t -*-

;;; Commentary:

;; Buffer navigation and management

;;; Code:

(defun archer-human-readable-file-sizes-to-bytes (string)
  "Convert a human-readable file (as STRING) size into BYTES."
  (interactive)
  (cond
   ((string-suffix-p "G" string t)
    (* 1000000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "M" string t)
    (* 1000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "K" string t)
    (* 1000 (string-to-number (substring string 0 (- (length string) 1)))))
   (t
    (string-to-number (substring string 0 (- (length string) 1))))))

(defun archer-bytes-to-human-readable-file-sizes (bytes)
  "Convert number of BYTES to human-readable file size."
  (interactive)
  (cond
   ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
   ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
   ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
   ((> bytes 100000) (format "%10.0fk" (/ bytes 1000.0)))
   ((> bytes 1000) (format "%10.1fk" (/ bytes 1000.0)))
   (t (format "%10d" bytes))))

(setup (:require ibuffer)
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t :summarizer
           (lambda (column-strings)
             (let ((total 0))
               (dolist (string column-strings)
                 (setq total (+ (float (archer-human-readable-file-sizes-to-bytes string))
                                total)))
               (archer-bytes-to-human-readable-file-sizes total))))
    (archer-bytes-to-human-readable-file-sizes (buffer-size)))
  ;; Modify the default ibuffer-formats
  (:option ibuffer-formats
           '((mark modified read-only locked " "
                   (name 20 20 :left :elide)
                   " "
                   (size-h 11 -1 :right)
                   " "
                   (mode 16 16 :left :elide)
                   " "
                   filename-and-process)
             (mark " "
                   (name 16 -1)
                   " " filename)))
  ;; Add groups
  (:option ibuffer-saved-filter-groups
           '(("default"
              ("dired" (mode . dired-mode))
              ("git"   (or (mode . magit-mode)
                           (mode . magit-process-mode)
                           (mode . magit-diff-mode)
                           (mode . magit-status-mode)))
              ("elisp" (mode . emacs-lisp-mode))
              ("c"     (mode . c-mode))
              ("c++" (mode . c++-mode))
              ("nix" (mode . nix-mode))
              ("rust" (mode . rustic-mode))
              ("java" (mode . java-mode))
              ("telegram"  (or (mode . telega-root-mode)
                               (mode . telega-mode)
                               (mode . telega-chat-mode)))
              ("documents" (or (name . "\\.pdf")
                               (name . "\\.org")))
              ("mails" (or (mode . notmuch-show-mode)
                           (mode . notmuch-tree-mode)
                           (mode . notmuch-search-mode)
                           (mode . notmuch-message-mode)))
              ("emacs" (or
                        (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Warnings\\*$")
                        (name . "^\\*straight-process\\*$")
                        (name . "^\\*dashboard\\*$"))))))

  (:option ibuffer-expert t
           ibuffer-display-summary t
           ibuffer-show-empty-filter-groups nil
           ibuffer-use-other-window nil
           ibuffer-movement-cycle t
           ibuffer-default-sorting-mode 'filename/process
           ibuffer-use-header-line t
           ibuffer-default-shrink-to-minimum-size nil)

  (:hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")
           (ibuffer-auto-mode 1)))

  (:global "C-x C-b" ibuffer))

;;; Unique names for buffers
(setup (:require uniquify)
  (:option uniquify-buffer-name-style 'forward
           uniquify-strip-common-suffix t
           uniquify-after-kill-buffer-p t))

(setup desktop
  (setq desktop-auto-save-timeout 300
           desktop-path `(,user-emacs-directory)
           desktop-base-file-name "desktop"
           desktop-files-not-to-save nil
           desktop-buffers-not-to-save nil
           desktop-globals-to-clear nil
           desktop-load-locked-desktop t
           desktop-missing-file-warning nil
           desktop-restore-eager 0
           desktop-restore-frames nil
           desktop-save 'ask-if-new)

  (:when-loaded
    (dolist (symbol '(kill-ring file-name-history))
      (add-to-list 'desktop-globals-to-save symbol)))

  (desktop-save-mode 1))

(provide 'init-buffers)
;;; init-buffers.el ends here
```


### Dired {#dired}

Dired is a built-in file manager for Emacs that does some pretty amazing things. For example you can enable writable dired buffer to edit everything and just save to apply your changes.

I have disabled `dired-find-alternate-file` warning, I'm using it 'cause pressing `Return` key just opens too many buffers.

There's also a package named `trashed`, to visit system trash.

```emacs-lisp

(require 'init-dired)

```

```emacs-lisp
;;; init-dired.el --- Dired -*- lexical-binding: t -*-

;;; Commentary:

;; Dired utilities and configuration for a better experience.

;;; Code:
(setup dired
  ;; 'Kay, with this I'm good, maybe
  (defun archer-dired-open-file ()
    "In Dired, open the file named on this line through xdg-open."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))

  ;; Kill the current Dired buffer, then visit the file or directory
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Emacs 29 options
  (unless (version< emacs-version "29")
    (setopt dired-mouse-drag-files t
            dired-make-directory-clickable t
            dired-free-space nil))

  (:option dired-listing-switches "-agho --group-directories-first"
           dired-kill-when-opening-new-dired-buffer t
           dired-recursive-copies 'always
           dired-recursive-deletes 'always
           dired-auto-revert-buffer #'dired-directory-changed-p
           dired-dwim-target t
           dired-hide-details-hide-symlink-targets nil
           delete-by-moving-to-trash t)

  (:bind-into dired-jump-map
    "j" dired-jump)

  (:bind-into dired-mode-map
    "C-c o" archer-dired-open-file))

(setup (:require dired-x)
  (:option dired-clean-confirm-killing-deleted-buffers t
           dired-clean-up-buffers-too t
           dired-x-hands-off-my-keys t
           dired-omit-files "^\\.$\\|^\\.[^.]")

  (:bind-into dired-mode-map
    "C-c d" dired-omit-mode)

  (:bind-into dired-mode-map
    "I" #'dired-info)

  (:with-mode dired-mode
    (:hook dired-omit-mode)))

(setup (:require dired-aux)
  (:option dired-create-destination-dirs 'always
           dired-do-revert-buffer t
           dired-isearch-filenames 'dwim
           dired-vc-rename-file t))

(setup (:require wdired)
  (:option wdired-allow-to-change-permissions t
           wdired-create-parent-directories t))

(setup (:require image-dired)
  (:option image-dired-external-viewer "xdg-open"
           image-dired-thumb-size 80
           image-dired-thumb-margin 2
           image-dired-thumb-relief 0
           image-dired-thumbs-per-row 4)

  (:bind-into image-dired-thumbnail-mode-map
    "<return>" #'image-dired-thumbnail-display-external))

(setup (:pkg diredfl)
  (:quit)
  (diredfl-global-mode 1))

(setup (:pkg dired-subtree)
  (:option dired-subtree-use-backgrounds nil)
  (:bind-into dired-mode-map
    "<tab>" dired-subtree-toggle
    "<backtab>" dired-subtree-remove))

(setup (:pkg dired-sidebar)
  (:autoload dired-sidebar-toggle-sidebar)
  (:global "C-x C-n" dired-sidebar-toggle-sidebar))

(setup (:pkg dired-collapse)
  (:load-after dired
    (:hook-into dired-mode-hook)))

(setup (:pkg all-the-icons-dired)
  (:option all-the-icons-dired-monochrome nil)
  (:load-after (all-the-icons dired)
    (:hook-into dired-mode-hook)))

(setup (:pkg trashed)
  (:option trashed-action-confirmer 'y-or-n-p
           trashed-use-header-line t
           trashed-sort-key '("Date deleted" . t)))

(provide 'init-dired)
;;; init-dired.el ends here
```


## Selection and search {#selection-and-search}

This is one of my favourite parts. I think that fast selection, completing and search are a must, always, everywhere.


### Monster trio of completion {#monster-trio-of-completion}

As Completion UI [Vertico](https://github.com/minad/vertico) is my preferred choice, it's lightweight and fast, and relies on Emacs internals. [Marginalia](https://github.com/minad/marginalia/) for rich annotations provides a summary for candidates.
Completion can be better with an [Orderless](https://github.com/oantolin/orderless) (similar to FZF, if you know). Orderless is also customizable for matching style.

```emacs-lisp

(require 'init-complete)

```

```emacs-lisp
;;; init-complete.el --- Completion enhancement -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs' internal completion is awesome, why should you use Ivy/Helm and similar?
;; They're wonderful, but complex and for me are unnecessary.
;; I'm using Vertico, Orderless and Marginalia (monster trio) for rich, orderless completion style.

;;; Code:

(setup minibuffer
  ;; Answers
  (fset #'yes-or-no-p #'y-or-n-p)
  (setq read-answer-short t)
  (setq use-short-answers t)

  ;; Files
  (setq file-name-shadow-properties '(invisible t intangible t))
  (file-name-shadow-mode 1)

  ;; Behavior
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (:with-hook minibuffer-setup-hook
    (:hook cursor-intangible-mode)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(setup (:require savehist)
  (setq savehist-file (locate-user-emacs-file "var/savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (:with-hook after-init-hook
    (:hook savehist-mode)))

;; Vertico
(setup (:pkg (vertico :files (:defaults "extensions/*")))

  (:also-load vertico-indexed
              vertico-flat
              vertico-grid
              vertico-mouse
              vertico-quick
              vertico-buffer
              vertico-repeat
              vertico-reverse
              vertico-directory
              vertico-multiform
              vertico-unobtrusive)

  (:option vertico-scroll-margin 0
           vertico-count 12
           vertico-resize t
           vertico-cycle t)

  (:bind-into vertico-map
    "<escape>" #'minibuffer-keyboard-quit)

  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "λ " 'face 'vertico-current)
                   "  ")
                 cand)))

  (:option vertico-multiform-commands
           '((dired (vertico-sort-function . sort-directories-first))))

  (:option vertico-multiform-categories
           '((consult-grep buffer)
             (consult-ripgrep buffer)
             (consult-git-grep buffer)
             (consult-find buffer)
             (file (vertico-sort-function . sort-directories-first))))

  (:hooks rfn-eshadow-update-overlay-hook vertico-directory-tidy
          minibuffer-setup-hook  vertico-repeat-save)

  ;; Sort directories before files
  (defun sort-directories-first (files)
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (vertico-mode 1)
  (vertico-multiform-mode 1))

;; Marginalia
(setup (:pkg marginalia)
  (:load-after vertico)
  (:bind-into minibuffer-local-map
    "M-A" marginalia-cycle)
  (marginalia-mode 1))

(setup (:pkg all-the-icons-completion)
  (:load-after (all-the-icons marginalia)
    (all-the-icons-completion-mode 1)
    (:with-mode marginalia-mode
      (:hook all-the-icons-completion-marginalia-setup))))

;; Orderless
(defun archer-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher, using equal sign as a suffix."
  (cond
   ((equal "=" pattern)
    '(orderless-literal . "="))
   ((string-suffix-p "=" pattern)
    (cons 'orderless-literal (substring pattern 0 -1)))))

(defun archer-orderless-without-literal-dispatcher (pattern _index _total)
  "Literal without style dispatcher using the exclamation mark as a suffix."
  (cond
   ((equal "!" pattern)
    '(orderless-literal . "!"))
   ((string-suffix-p "!" pattern)
    (cons 'orderless-without-literal (substring pattern 0 -1)))))

(defun archer-orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism dispatcher using comma as suffix."
  (cond
   ((equal "," pattern)
    '(orderless-literal . ","))
   ((string-suffix-p "," pattern)
    (cons 'orderless-initialism (substring pattern 0 -1)))))

(defun archer-orderless-flex-dispatcher (pattern _index _total)
  "Flex dispatcher using the tilde suffix."
  (cond
   ((equal "~" pattern)
    '(orderless-literal . "~"))
   ((string-suffix-p "~" pattern)
    (cons 'orderless-flex (substring pattern 0 -1)))))

(setup (:pkg orderless)
  (setq completion-styles '(orderless basic)
        orderless-component-separator 'orderless-escapable-split-on-space
        completion-category-defaults nil)

  (setq orderless-style-dispatchers
        '(archer-orderless-literal-dispatcher
          archer-orderless-without-literal-dispatcher
          archer-orderless-initialism-dispatcher
          archer-orderless-flex-dispatcher))

  (setq completion-category-overrides
        '((file (styles . (partial-completion basic orderless)))
          (project-file (styles . (partial-completion basic orderless))))))

(provide 'init-complete)
;;; init-complete.el ends here
```


### Embark {#embark}

[Embark](https://github.com/oantolin/embark/) provides contextual menu offering actions for a target determined in the context, exactly like a contextual menu.

```emacs-lisp

(require 'init-embark)

```

```emacs-lisp
;;; init-embark.el --- Embark, run a command based on point-*- lexical-binding: t -*-

;;; Commentary:

;; Sometimes you want to act near point, but there are many actions.
;; Embark ships many actions, dependant on target and modes.

;;; Code:

(defun archer-embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
      '(archer-embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(defun archer-embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'archer-embark-which-key-indicator embark-indicators)))
    (apply fn args)))

;; Embark configuration
(setup (:pkg embark)
  (:load-after consult
    (:pkg embark-consult))

  (:load-after which-key
    (setq prefix-help-command #'embark-prefix-help-command)
    (advice-add #'embark-completing-read-prompter :around #'archer-embark-hide-which-key-indicator))

  (:global "C-." embark-act
           "C-;" embark-dwim
           "C-h B" embark-bindings) ;; alternative for `describe-bindings'

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Used for export and edit after ripgrep magic.
(setup (:pkg wgrep))

(provide 'init-embark)
;;; init-embark.el ends here
```


### Consult {#consult}

[Consult](https://github.com/minad/consult) provides practical commands based on the Emacs completion function completing-read.

Consult offers, for example:

-   Buffer switching command `consult-buffer` to switch between buffers and recently opened files.
-   Multiple asynchronous search commands:
    -   `consult-grep`
    -   `consult-ripgrep`
    -   `consult-line`, which resembles [Swiper](https://github.com/abo-abo/swiper)

<!--listend-->

```emacs-lisp

(require 'init-consult)

```

```emacs-lisp
;;; init-consult.el --- Consult completing read -*- lexical-binding: t -*-

;;; Commentary:

;; Consult provides commands based on Emacs `completion-read' functionality.  Here my basic configuration and key-bindings.  Totally WIP.

;;; Code:

(setup (:pkg consult)
  (:require consult)

  ;; C-c bindings (mode specific)
  (:global "C-c h" consult-history
           "C-c M" consult-mode-command
           "C-c b" consult-bookmark
           "C-c k" consult-kmacro)

  ;; C-x bindings
  (:global "C-x M-c" consult-complex-command      ; orig. repeat-complex-command
           "C-x b"   consult-buffer               ; orig. switch-to-buffer
           "C-x 4 b" consult-buffer-other-window  ; orig. switch-to-buffer-other-window
           "C-x 5 b" consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame

  ;; [C]-[M]-# bindings for registers
  (:global "C-M-#" consult-register
           "M-#"   consult-register-load
           "C-#"   consult-register-store) ; orig. abbrev-prefix-mark (unrelated)

  ;; Other custom bindings
  (:global "M-y"   consult-yank-pop  ; orig. yank-po
           "C-h a" consult-apropos)  ; orig. apropos-comman

  ;; M-g bindings (goto-map)
  (:bind-into goto-map
    "g"   consult-goto-line     ; orig. goto-line
    "o"   consult-org-heading   ; Alternative: consult-org-heading
    "m"   consult-mark
    "k"   consult-global-mark
    "i"   consult-imenu
    "I"   consult-imenu-multi
    "e"   consult-compile-error
    "f"   consult-flymake)     ; Alternative: consult-flymake

  ;; M-s bindings (search-map)
  (:bind-into search-map
    "f" consult-find
    "F" consult-locate
    "g" consult-grep
    "G" consult-git-grep
    "r" consult-ripgrep
    "l" consult-line
    "L" consult-line-multi
    "m" consult-multi-occur
    "k" consult-keep-lines
    "u" consult-focus-lines
    "e" consult-isearch-history)  ; Isearch integration

  ;; ??? From wiki
  ;; (:bind-into isearch-mode-map
  ;;   "M-e" consult-isearch-history       ; orig. isearch-edit-string
  ;;   "M-s e" consult-isearch-history     ; orig. isearch-edit-string
  ;;   "M-s l" consult-line                ; needed by consult-line to detect isearch
  ;;   "M-s L" consult-line-multi)         ; needed by consult-line to detect isearch

  ;; Register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (:option register-preview-delay 0
           register-preview-function #'consult-register-format)

  ;; Optionally configure the narrowing key.
  (:option consult-narrow-key "<")

  ;; Use Consult to select xref locations with preview
  (:option xref-show-xrefs-function #'consult-xref
           xref-show-definitions-function #'consult-xref)

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark :preview-key "M-.")

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  (:hooks completion-list-mode consult-preview-at-point-mode))

(setup (:pkg consult-dir)
  (:load-after consult
    (:global "C-x C-d" consult-dir)
    (:bind-into minibuffer-local-completion-map
      "C-x C-d" consult-dir-maybe
      "C-x C-j" consult-dir-jump-file)))

(setup (:pkg consult-eglot)
  (:load-after (consult eglot)
    (:global "M-g s" consult-eglot-symbols)))

(provide 'init-consult)
;;; init-consult.el ends here
```


### Completion at point {#completion-at-point}

I'm using [Corfu](https://github.com/minad/corfu) with [Cape](https://github.com/minad/cape) right now, while [Company](https://github.com/company-mode/company-mode) stuff is here due to other modes completion backends which relies on it. I prefer Corfu especially because it uses Emacs completion facilities, and child frames instead of overlays.

Completions are provided by commands which provide completion, or by Capfs (`completion-at-point-functions`). Many major modes implement a Capf, also LSP clients which talk to the LSP server to retrieve completion.

Cape provides extensions and backends. A great thing of Cape is the `cape-company-to-capf` adapter for Company backends, and it is very easy to use!

```emacs-lisp

(require 'init-complete-in-buffer)

```

```emacs-lisp
;;; init-complete-in-buffer.el --- In buffer completion configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Corfu completion UI and Cape extensions for better completion at point.

;;; Code:

(setup (:pkg corfu)
  (global-corfu-mode)

  (load "extensions/corfu-history")
  (load "extensions/corfu-popupinfo")

  (corfu-history-mode 1)

  (corfu-popupinfo-mode 1)
  (:option corfu-popupinfo-delay t)

  (add-to-list 'savehist-additional-variables 'corfu-history)

  ;; SECTION FOR SPECIAL FUNCTIONS
  ;; Movement
  (defun contrib-corfu-beginning-of-prompt ()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))

  (defun contrib-corfu-end-of-prompt ()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))

  (define-key corfu-map [remap move-beginning-of-line] #'corfu-beginning-of-prompt)
  (define-key corfu-map [remap move-end-of-line] #'corfu-end-of-prompt)

  ;; From Corfu's manual
  (defun contrib-corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'contrib-corfu-move-to-minibuffer)

  ;; Adapted from Corfu's manual.
  ;; (Found in Prot's configuration)
  (defun contrib-corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (or (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'contrib-corfu-enable-always-in-minibuffer 1)

  (:option corfu-cycle t
           corfu-auto t
           corfu-separator ?\s
           corfu-quit-at-boundary nil
           corfu-quit-no-match t
           corfu-preview-current #'insert
           corfu-preselect-first t
           corfu-on-exact-match #'insert
           corfu-echo-documentation 0.25
           corfu-min-width 30
           corfu-scroll-margin 5)

  (:bind-into corfu-popupinfo-map
    "M-p" corfu-popupinfo-scroll-down
    "M-n" corfu-popupinfo-scroll-up
    "M-d" corfu-popupinfo-toggle))

(setup (:pkg kind-icon)
  (:load-after corfu
    (:option kind-icon-default-face 'corfu-default
       kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.7 :scale 1.0))
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(setup (:pkg cape)
  ;; Needed for company-backends!
  (setup (:pkg company)
    (:autoload company-grab))

  (dolist (backend '(cape-symbol cape-keyword cape-file cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend))

  (:global "C-c p p" completion-at-point
     "C-c p t" complete-tag
     "C-c p d" cape-dabbrev
     "C-c p h" cape-history
     "C-c p f" cape-file
     "C-c p k" cape-keyword
     "C-c p s" cape-symbol
     "C-c p a" cape-abbrev
     "C-c p i" cape-ispell
     "C-c p l" cape-line
     "C-c p w" cape-dict
     "C-c p \\" cape-tex
     "C-c p _" cape-tex
     "C-c p ^" cape-tex
     "C-c p &" cape-sgml
     "C-c p r" cape-rfc1345))

(provide 'init-complete-in-buffer)
;;; init-complete-in-buffer.el ends here
```


## Org Mode {#org-mode}

Org mode is the killer feature of Emacs. Markup language, agenda, brain, templates...you can do _literally_ (xD) everything.


### Essential configuration {#essential-configuration}

I absolutely need focus when I'm editing my documents in the dark, so I want my buffer centered and lines untruncated.

Indentation is defined as a function for basic org-mode setup.

The purpose of ~~[visual-fill-column](https://github.com/joostkremers/visual-fill-column)~~ [olivetti ](https://github.com/rnkn/olivetti)is to center `org-mode` buffers for a more pleasing writing experience as it centers the contents of the buffer horizontally to seem more like you are editing a document.

[Org Modern](https://github.com/minad/org-modern) replaces markup syntax with nice headings, TODOs etc.

```emacs-lisp

(require 'init-org)

```

```emacs-lisp
;;; init-org.el --- Org mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Org mode is certainly the killer feature of Emacs.
;; You can do anything, for example capturing of templates, export, markdown like editing.

;;; Code:

(defun archer-org-mode-setup ()
  "Set important modes for me while editing org documents.

- Setting variable-pitch allows different face definition;
- I prefer visual-line here, instead of truncating lines."
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(setup (:pkg org)
  ;; General
  (:option org-adapt-indentation nil
           org-fold-catch-invisible-edits 'smart
           org-cycle-separator-lines 1
           org-auto-align-tags nil
           org-tags-column 0 ;; place tags directly next to headline text
           org-archive-mark-done nil
           org-startup-folded 'content
           org-insert-heading-respect-content t
           org-read-date-prefer-future 'time
           org-startup-folded t
           org-startup-indented t

           ;; Prettify
           org-ellipsis " ⤵" ;; "…" "⤵"
           org-hide-leading-stars t
           org-pretty-entities t
           org-pretty-entities-include-sub-superscripts t
           org-hide-emphasis-markers t
           org-fontify-quote-and-verse-blocks t
           org-list-allow-alphabetical t
           org-highlight-latex-and-related '(native latex)
           org-image-actual-width 500

           ;; Date
           org-display-custom-times t
           org-time-stamp-custom-formats '("<%d %b %Y>" . "<%d/%m/%y %a %H:%M>")

           ;; Footnotes
           org-footnote-section nil   ;; place footnotes locally
           org-footnote-auto-adjust t ;; renumber footnotes

            ;; Insertion/Yanking
           org-M-RET-may-split-line '((default . t)) ;; don't split line when creating a new headline, list item, or table field
           org-yank-adjusted-subtrees t              ;; adjust subtrees to depth when yanked
           org-yank-folded-subtrees t                ;; fold subtrees on yank

           org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
           org-list-indent-offset 1 ;; increase sub-item indentation

           ;; Movement
           org-return-follows-link t ;; make RET follow links
           org-special-ctrl-a/e t    ;; better movement in headers

           ;; Searching
           org-imenu-depth 8   ;; scan to depth 8 w/imenu
           imenu-auto-rescan t ;; make sure imenu refreshes

           ;; Source block settings
           org-src-fontify-natively t         ;; use lang-specific fontification
           org-src-window-setup 'other-window ;; edit source in other window
           org-src-tab-acts-natively t        ;; use lang bindings
           org-confirm-babel-evaluate t       ;; confirm evaluation

           ;; TODOS
           org-use-fast-todo-selection 'expert ;; don't use popup window for todos
           ;; don't set to DONE if children aren’t DONE
           org-enforce-todo-dependencies t
           org-enforce-todo-checkbox-dependencies t

           ;; Source blocks
           org-hide-block-startup nil
           org-src-preserve-indentation nil
           org-edit-src-content-indentation 2)

  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (shell . t)
                               (groovy . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  (:local-set completion-at-point-functions '(cape-dabbrev cape-file))

  (:hook archer-org-mode-setup))

(setup (:pkg org-appear)
  (:autoload org-appear-mode)
  (:hook-into org-mode)
  (:option org-appear-autoemphasis t
           org-appear-autolinks nil
           org-appear-autosubmarkers t))

(setup (:pkg org-modern)
  (:load-after org)
  (:hook-into org-mode)
  (set-face-attribute 'org-modern-symbol nil :family "Hack")
  (:option org-modern-label-border 1
           org-modern-hide-stars nil      ;; Compatibility with org-indent
           org-modern-block-fringe nil    ;; Bad
           org-modern-variable-pitch nil
           org-modern-timestamp t
           org-modern-table t
           org-modern-table-vertical 1
           org-modern-table-horizontal 0))

(setup (:pkg (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent"))
  (:hook-into org-indent-mode))

(setup (:pkg olivetti)
  (:load-after org)
  (:hook-into org-mode)
  (:option olivetti-body-width 0.75
           olivetti-minimum-body-width 75
           olivetti-style 'fancy))

(provide 'init-org)
;;; init-org.el ends here
```


### Babel and Tempo {#babel-and-tempo}

To execute or export code in `org-mode` code blocks, we need to set up `org-babel-load-languages` for each language. [This page](https://orgmode.org/worg/org-contrib/babel/languages.html) documents all of the languages that you can use with `org-babel`.

Org Mode's [structure templates](https://orgmode.org/manual/Structure-Templates.html) feature enables to quickly insert code blocks into your Org files in combination with `org-tempo` by typing `<` followed by the template name like `el` or `py` and then press `TAB`.
To add more `src` block templates, just copy one of the lines and change the two strings at the end, the first to be the template name and the second to contain the name of the language ([listed here](https://orgmode.org/worg/org-contrib/babel/languages.html)).

There's also a snippet that adds a hook to `org-mode` buffers so that `archer-org-babel-tangle-config` gets executed each time such a buffer gets saved. This function checks to see if the file being saved is the Emacs.org file you're looking at right now, and if so, automatically exports the configuration here to the associated output files. This function is inspired by David Wilson of System Crafters.

```emacs-lisp

(require 'init-org-languages)

```

```emacs-lisp
;;; init-org-languages.el --- Language related org settings -*- lexical-binding: t -*-

;;; Commentary:

;; We can execute code in org-mode, but also define structure templates
;; to insert blocks (like src blocks).
;; Tangling is also an important feature, let's use it.

;;; Code:

(setup org-tempo
  (:load-after org
    (add-to-list 'org-structure-template-alist '("bash" . "src bash"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("cc" . "src c"))
    (add-to-list 'org-structure-template-alist '("j" . "src java")))

  (:with-mode org-mode
    (:local-set electric-pair-inhibit-predicate
                `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(setup ob-tangle
  ;; Auto tangling
  (defun archer-org-babel-tangle-config ()
    "Auto tangle configuration on save if we are in the right directory."
    (when (string-equal (file-name-directory (buffer-file-name))
                        (expand-file-name archer-config-path))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))

  (:with-mode org-mode
    (:with-hook after-save-hook
      (:hook archer-org-babel-tangle-config))))

(provide 'init-org-languages)
;;; init-org-languages.el ends here
```


### Exporting {#exporting}

```emacs-lisp

(require 'init-org-export)

```

Org is surely nice, but what about export?
Wonderful, but it needs some tweaks, and not only for presentations.

The first section of this file is regards good LaTeX export through `ox-latex`.
The best way to set export options is [the following](https://jakebox.github.io/youtube/org_latex_video.html).

Structure every file like this:

```org

#+LaTeX_CLASS: article
#+LaTeX_CLASS_OPTIONS: [letterpaper]
#+OPTIONS: toc:nil
#+SETUPFILE: ~/your/path/to/setup/file.org

```

Reveal.js presentations are exported through `ox-reveal`, which is very simple to configure.

The hidden gem is `ox-hugo`, you can manage your website content from Emacs, that's cool. You can also manage your contents with a single file, multiple files, or both ways!

```emacs-lisp
;;; init-org-export.el --- Org exports configuration -*- lexical-binding: t -*-

;;; Commentary:

;; We can export in any format with org-mode, but we need some
;; tweaks to achieve good results.
;; Here are listed all the settings for ox-latex, ox-reveal, etc.

;;; Code:

;; LaTeX export
(setup ox-latex
  (:load-after ox)
  (:option org-latex-toc-command "\\tableofcontents \\clearpage"  ; Newpage after TOC
           ;; Enable listings
           org-latex-listings t
           ;; Previewing LaTeX fragments in org mode, default changed for bad quality.
           org-latex-create-formula-image-program 'imagemagick
           ;; Using minted for tables
           org-latex-listings 'minted
           org-latex-packages-alist '(("" "minted"))
           org-latex-minted-options '(("breaklines" "true")
                                      ("breakanywhere" "true"))
           ;; PDF process
           ;; '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f")
           org-latex-pdf-process '("pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
                                   "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
                                   "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; (add-to-list 'org-latex-listings-langs '(yaml "yaml"))
  ;; (add-to-list 'org-latex-listings-langs '(groovy "groovy"))

  ;; LaTeX base classes
  (:when-loaded (add-to-list 'org-latex-classes
                             '("org-plain-latex"
                               "\\documentclass{article}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                               ("\\section{%s}" . "\\section*{%s}")
                               ("\\subsection{%s}" . "\\subsection*{%s}")
                               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                               ("\\paragraph{%s}" . "\\paragraph*{%s}")
                               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

;; Reveal.js
(setup (:pkg (ox-reveal :type git :host github :repo "yjwen/org-reveal"))
  (:load-after ox)
  (:option org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

;; Hugo
(setup (:pkg ox-hugo)
  (:load-after ox))

(provide 'init-org-export)
;;; init-org-export.el ends here
```


## Development {#development}

All my packages needed to develop in a decent way. Bye IDEs.


### Projects management and Git {#projects-management-and-git}

```emacs-lisp

(require 'init-projects)

```

[Projectile](https://github.com/bbatsov/projectile) provides easy project management and navigation.

Common Git operations are easy to execute quickly using <span class="underline">Magit</span>'s command panel system.

**NOTE**: Make sure to configure a GitHub token before using this package!

-   [Getting started with Magit](https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started)
-   [Token Creation](https://magit.vc/manual/forge/Token-Creation.html#Token-Creation)

<!--listend-->

```emacs-lisp
;;; init-projects.el --- Projects management -*- lexical-binding: t -*-

;;; Commentary:

;; Git integration and projects' folders management.

;;; Code:

(setup (:pkg direnv)
  (:hook-into prog-mode))

(setup (:pkg magit)
  (:autoload magit-status)
  (:option magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(setup (:pkg forge)
  (:load-after magit))

(setup (:pkg blamer))

;; `projectile', not using to try `project.el'
(setup (:pkg projectile :quit)
  (:blackout)

  ;; NOTE: Set this to the folder where you keep your Git repos!
  (:option projectile-project-search-path '("~/projects")
           projectile-switch-project-action #'projectile-dired)

  (projectile-mode)

  (:global "C-c C-p" projectile-command-map))

(setup (:pkg consult-projectile :quit)
  (:load-after (consult projectile)))

;; `treemacs' stuff, I'm not using it
(setup (:pkg treemacs :quit)
  (:option treemacs-deferred-git-apply-delay        0.5
           treemacs-directory-name-transformer      #'identity
           treemacs-display-in-side-window          t
           treemacs-eldoc-display                   'simple
           treemacs-file-event-delay                2000
           treemacs-file-follow-delay               0.2
           treemacs-file-name-transformer           #'identity
           treemacs-follow-after-init               t
           treemacs-expand-after-init               t
           treemacs-find-workspace-method           'find-for-file-or-pick-first
           treemacs-git-command-pipe                ""
           treemacs-goto-tag-strategy               'refetch-index
           treemacs-header-scroll-indicators        '(nil . "^^^^^^")
           treemacs-hide-dot-git-directory          t
           treemacs-indentation                     2
           treemacs-indentation-string              " "
           treemacs-is-never-other-window           t
           treemacs-max-git-entries                 5000
           treemacs-missing-project-action          'ask
           treemacs-move-forward-on-expand          nil
           treemacs-no-png-images                   nil
           treemacs-no-delete-other-windows         t
           treemacs-project-follow-cleanup          nil
           treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
           treemacs-position                        'left
           treemacs-read-string-input               'from-child-frame
           treemacs-recenter-distance               0.1
           treemacs-recenter-after-file-follow      nil
           treemacs-recenter-after-tag-follow       nil
           treemacs-recenter-after-project-jump     'always
           treemacs-recenter-after-project-expand   'on-distance
           treemacs-litter-directories              '("/.direnv" "/node_modules" "/.venv" "/.cask")
           treemacs-project-follow-into-home        nil
           treemacs-show-cursor                     nil
           treemacs-show-hidden-files               t
           treemacs-silent-filewatch                nil
           treemacs-silent-refresh                  nil
           treemacs-sorting                         'alphabetic-asc
           treemacs-select-when-already-in-treemacs 'move-back
           treemacs-space-between-root-nodes        t
           treemacs-tag-follow-cleanup              t
           treemacs-tag-follow-delay                1.5
           treemacs-text-scale                      nil
           treemacs-user-mode-line-format           nil
           treemacs-user-header-line-format         nil
           treemacs-wide-toggle-width               70
           treemacs-width                           20
           treemacs-width-increment                 1
           treemacs-width-is-initially-locked       nil
           treemacs-workspace-switch-cleanup        nil)

  (:when-loaded
    (setq treemacs-collapse-dirs         (if treemacs-python-executable 3 0)
          treemacs-file-extension-regex  treemacs-last-period-regex-value)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-hide-gitignored-files-mode nil)

    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))


  (:global "M-0"        treemacs-select-window
           "C-c C-t 1"  treemacs-delete-other-windows
           "C-c C-t t"  treemacs
           "C-c C-t d"  treemacs-select-directory
           "C-c C-t b"  treemacs-bookmark
           "C-c C-t f"  treemacs-find-file
           "C-c C-t T"  treemacs-find-tag))

(setup (:pkg treemacs-projectile :quit)
  (:load-after (treemacs projectile)))

(setup (:pkg treemacs-all-the-icons :quit)
  (:load-after treemacs
    (treemacs-load-theme "all-the-icons")))

(setup (:pkg treemacs-magit :quit)
  (:load-after (treemacs magit)))

(provide 'init-projects)
;;; init-projects.el ends here
```


### Code style {#code-style}

[Format-all-the-code](https://github.com/lassik/emacs-format-all-the-code) lets you auto-format source code in many languages. It is very nice, you need only the formatters installed on your system.

[Ethan-wspace](https://github.com/glasserc/ethan-wspace) is a nice package to avoid useless/horrible extra whitespaces.

[Rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters) is useful in programming modes because it colorizes nested parentheses and brackets according to their nesting depth. This makes it a lot easier to visually match parentheses in Emacs Lisp code without having to count them yourself.

The rest of `init-code-style.el` regards tab settings.

```emacs-lisp

(require 'init-code-style)

```

```emacs-lisp
;;; init-code-style.el --- Code style settings -*- lexical-binding: t -*-

;;; Commentary:

;; OCD, so I have to remove useless whitespace after save or on demand, and format all my code.
;; Plus, general tab settings, tree-sitter support, fancy stuff.

;;; Code:

(setup (:pkg format-all)
  (:blackout)
  (:hook-into prog-mode)
  (:global "<f1>" format-all-buffer))

(setup (:pkg editorconfig)
  (:blackout)
  (editorconfig-mode 1))

(setup eldoc
  (:blackout)
  (global-eldoc-mode 1))

(setup (:pkg rainbow-mode)
  (:hook-into web-mode json-mode))

(setup (:pkg ethan-wspace)
  (:blackout)
  (:global "C-c c" ethan-wspace-clean-all)
  (:hook-into prog-mode)
  ;; Required
  (:option mode-require-final-newline nil
           require-final-newline nil))

;; Tabs, indentation, and the TAB key
(setup indent
  (:option tab-always-indent 'complete
           tab-first-completion 'word-or-paren-or-punct
           tab-width 2
           indent-tabs-mode nil)) ; Use spaces!

(setup (:pkg rainbow-delimiters)
  (:hook-into prog-mode))

(setup (:pkg tree-sitter)
  (:autoload tree-sitter-mode tree-sitter-hl-mode)
  (:hook-into nix-mode c-mode c++-mode java-mode python-mode)
  (:hooks tree-sitter-after-on-hook tree-sitter-hl-mode))

(setup (:pkg tree-sitter-langs)
  (:load-after treesitter))

(provide 'init-code-style)
;;; init-code-style.el ends here
```


### Syntax checking {#syntax-checking}

Lately I've been trying `Flymake`, built-in into Emacs. [Flycheck](https://www.flycheck.org/) has many checkers though, so here we go with ["how to use Flycheck chekers in Flymake"](https://github.com/purcell/flymake-flycheck)

```emacs-lisp

(require 'init-spell-and-check)

```

```emacs-lisp
;;; init-spell-and-check.el --- Spell and syntax checking based on modes -*- lexical-binding: t -*-

;;; Commentary:

;; Flyspell as spell checker, while Flycheck as syntax checker for prog-mode.

;;; Code:

(setup flymake
  (:option flymake-fringe-indicator-position 'left-fringe
           flymake-suppress-zero-counters t
           flymake-start-on-flymake-mode t
           flymake-no-changes-timeout 0.3
           flymake-start-on-save-buffer t
           flymake-proc-compilation-prevents-syntax-check t
           flymake-wrap-around nil)

  (:option flymake-mode-line-format
           '("" flymake-mode-line-exception flymake-mode-line-counters))

  (:option flymake-mode-line-counter-format
           '(" " flymake-mode-line-error-counter
             flymake-mode-line-warning-counter
             flymake-mode-line-note-counter ""))

  (add-to-list 'elisp-flymake-byte-compile-load-path load-path)

  (:bind-into ctl-x-x-map
    "m" #'flymake-mode)

  (:bind "C-c ! s" #'flymake-start
         "C-c ! d" #'flymake-show-buffer-diagnostics ; Emacs28
         "C-c ! D" #'flymake-show-project-diagnostics ; Emacs28
         "C-c ! n" #'flymake-goto-next-error
         "C-c ! p" #'flymake-goto-prev-error)

  (:hook-into prog-mode text-mode))

;; From Purcell's dotfiles
(setup (:pkg flymake-flycheck)
  (:load-after flymake)
  (:when-loaded
    (defun sanityinc/enable-flymake-flycheck ()
      (setq-local flymake-diagnostic-functions
                  (append flymake-diagnostic-functions
                          (flymake-flycheck-all-chained-diagnostic-functions))))

    (:option flycheck-emacs-lisp-load-path 'inherit)

    (:hooks flymake-mode sanityinc/enable-flymake-flycheck)))

;; (setup flycheck (:quit) (:pkg flycheck)
;;   (:autoload flycheck-list-errors flycheck-buffer)
;;   (:option flycheck-emacs-lisp-load-path 'inherit
;;            flycheck-idle-change-delay 1.0
;;            flycheck-display-errors-delay 0.25
;;            flycheck-emacs-lisp-initialize-packages t)
;;   (global-flycheck-mode))

(setup flyspell
  (:hooks text-mode-hook (lambda () flyspell-mode 1)
          prog-mode-hook flyspell-prog-mode))

(provide 'init-spell-and-check)
;;; init-spell-and-check.el ends here
```


### Language Server Protocol {#language-server-protocol}

[Language Server Protocol](https://microsoft.github.io/language-server-protocol/) support with multiples languages support for Emacs.

There are two ways to use LSP with Emacs: [lsp-mode](https://github.com/emacs-lsp/lsp-mode) and [Eglot](https://github.com/joaotavora/eglot) (built into Emacs 29). I prefer the latter for the following reason, given by the author of `Eglot`:

>
>
> Eglot is considerably less code and hassle than lsp-mode.el. In most cases, there's nothing to configure. It's a minimalist approach focused on user experience and performance.

To avoid copy-pasting, here the [full comparision](https://github.com/joaotavora/eglot#historical-differences-to-lsp-modeel).

```emacs-lisp

(require 'init-lsp)

```

```emacs-lisp
;;; init-lsp.el --- Language Server Protocol client -*- lexical-binding: t -*-

;;; Commentary:

;; Here the configuration for LSP-Mode.

;;; Code:

;;
;;; NOTE: These are taken from https://github.com/doomemacs/doomemacs/blob/master/modules/tools/lsp/config.el
(defvar archer-lsp--default-read-process-output-max nil)
(defvar archer-lsp--default-gcmh-high-cons-threshold nil)
(defvar archer-lsp--optimization-init-p nil)

(define-minor-mode archer-lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  :group 'lsp
  (if (not archer-lsp-optimization-mode)
      (setq-default read-process-output-max archer-lsp--default-read-process-output-max
                    gcmh-high-cons-threshold archer-lsp--default-gcmh-high-cons-threshold
                    archer-lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless archer-lsp--optimization-init-p
      (setq archer-lsp--default-read-process-output-max (default-value 'read-process-output-max)
            archer-lsp--default-gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))
      (setq-default read-process-output-max (* 1024 1024))
      ;; REVIEW LSP causes a lot of allocations, with or without the native JSON
      ;;        library, so we up the GC threshold to stave off GC-induced
      ;;        slowdowns/freezes. Doom uses `gcmh' to enforce its GC strategy,
      ;;        so we modify its variables rather than `gc-cons-threshold'
      ;;        directly.
      (setq-default gcmh-high-cons-threshold (* 2 archer-lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq archer-lsp--optimization-init-p t))))

(defcustom archer-lsp-client 'eglot
  "Preferred lsp-client."
  :type 'symbol
  :group 'lsp)

;;
;;; LSP-MODE

(setup lsp-mode
  (:quit)
  (:pkg lsp-mode)
  (:autoload lsp)

  (:when-loaded
    ;; Function to enable corfu in lsp-mode
    (defun archer-lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))) ;; Configure orderless

    ;; LSP completion with Corfu
    (when (corfu-mode)
      (setq lsp-completion-provider :none)
      (add-hook 'lsp-completion-mode-hook #'archer-lsp-mode-setup-completion)))

  (:option lsp-keymap-prefix "C-c l"
           lsp-keep-workspace-alive nil
           lsp-auto-guess-root nil
           lsp-log-io nil
           lsp-restart 'auto-restart
           lsp-enable-symbol-highlighting t
           lsp-enable-on-type-formatting t
           lsp-signature-auto-activate nil
           lsp-signature-render-documentation t
           lsp-modeline-code-actions-enable nil
           lsp-modeline-diagnostics-enable nil
           lsp-headerline-breadcrumb-enable t
           lsp-semantic-tokens-enable nil
           lsp-eldoc-render-all t
           lsp-idle-delay 0.5
           lsp-enable-snippet t
           lsp-enable-folding nil
           lsp-enable-imenu t
           lsp-eldoc-hook '(lsp-hover))

  (:with-mode (c-mode c++-mode java-mode nix-mode rustic-mode cmake-mode terraform-mode)
    (:hook lsp-deferred))

  (:hook-into lsp-enable-which-key-integration))

(setup lsp-ui
  (:quit)
  (:pkg lsp-ui)
  (:autoload lsp-ui-mode)
  (:hook-into lsp-mode)
  (:load-after lsp)
  (:when-loaded
    (:option lsp-ui-doc-enable t
             lsp-ui-doc-header t
             lsp-ui-doc-include-signature t
             lsp-ui-doc-border '(face-foreground 'default)
             lsp-ui-sideline-show-code-actions t
             lsp-ui-sideline-delay 0.05)))

(setup lsp-java
  (:quit)
  (:pkg lsp-java)
  (:load-after lsp))

;;
;;; EGLOT

;; Eglot is built-in in Emacs 29+, so this condition doesn't consent the installation
;; if it is already present.
(setup (:and (not (package-installed-p 'eglot))
             (:pkg eglot))
  ;; List of modes and servers
  (:when-loaded
    (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
    (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
    (add-to-list 'eglot-server-programs `(nix-mode . ,(eglot-alternatives '(("nil")
                                                                            ("rnix-lsp"))))))
  ;; Hooks
  (:with-mode (c-mode c++-mode java-mode nix-mode rustic-mode terraform-mode)
    (:hook eglot-ensure)))

(setup (:pkg eglot-java)
  (:load-after eglot)
  (:with-mode (java-mode)
    (:hook eglot-java-mode)))

(setup (:if-feature gcmh)
  (:with-hook (eglot-managed-mode-hook lsp-mode-hook)
    (:hook archer-lsp-optimization-mode)))

(provide 'init-lsp)
;;; init-lsp.el ends here
```


### Snippets {#snippets}

```emacs-lisp

(require 'init-snippets)

```

```emacs-lisp
;;; init-snippets.el --- Snippets -*- lexical-binding: t -*-

;;; Commentary:

;; Remember code snippet for common functions? Bleah.

;;; Code:

(setup (:pkg yasnippet)
  (:blackout yas-minor-mode)
  (:hooks prog-mode-hook yas-minor-mode)
  (:when-loaded
    (yas-reload-all)))

(setup (:pkg yasnippet-snippets)
  (:load-after yasnippet))

(setup (:pkg (cape-yasnippet :type git :host github :repo "elken/cape-yasnippet"))
  (:load-after (cape yasnippet)
    (defun archer-add-cape-yasnippet ()
      (add-to-list 'completion-at-point-functions #'cape-yasnippet))

    (:with-mode eglot-managed-mode-hook
      (:hook archer-add-cape-yasnippet))

    (:global "C-c p y" cape-yasnippet)))


(provide 'init-snippets)
;;; init-snippets.el ends here
```


### Extra modes {#extra-modes}

```emacs-lisp

(require 'init-extra-modes)

```

```emacs-lisp
;;; init-extra-modes.el --- Extra modes -*- lexical-binding: t -*-

;;; Commentary:

;; This is not divided in multiple files, it's useless, I'm good this way :D.

;;; Code:

(setup (:pkg cmake-mode)
  (:file-match (rx (or "CmakeLists.txt" ".cmake") eos)))

(setup (:pkg nix-mode)
  (:file-match (rx ".nix" eos)))

(setup (:pkg markdown-mode)
  (:file-match (rx (or ".md" ".markdown" ".mdown") eos)))

(setup (:pkg yaml-mode)
  (:file-match (rx (or ".yml" ".yaml") eos)))

(setup (:pkg json-mode)
  (:file-match (rx ".json" eos)))

(setup (:pkg rustic)
  (:file-match (rx ".rs" eos))
  (:option rustic-format-on-save nil ; There's `format-all-mode'
           rustic-lsp-client archer-lsp-client))

(setup (:pkg terraform-mode)
  (:file-match (rx ".tf" eos)))

(setup (:pkg company-terraform)
  (:autoload company-terraform)

  (:with-after (cape terraform-mode)
    (defun archer-cape-company-terraform()
      "Add completion at point functions made from company backends for `terraform'."
      (setq-local
       completion-at-point-functions
       (append (list (cape-company-to-capf #'company-terraform)) completion-at-point-functions)))

    (:with-hook terraform-mode-hook
      (:hook archer-cape-company-terraform))))

(provide 'init-extra-modes)
;;; init-extra-modes.el ends here
```


## Frontend for other uses {#frontend-for-other-uses}

Emacs can be a frontend for almost everything.


### Mails {#mails}

I've used `mu4e` (mu-for-emacs) for almost a year. It is an e-mail client for GNU Emacs version 24.4 or higher, built on top of the [mu](https://github.com/djcb/mu) e-mail search engine. `mu4e` is optimized for quickly processing large amounts of e-mail.

However, I've always struggled with it, and I recently tried [notmuch](https://notmuchmail.org/). Oh boy, I wish I had done it sooner!
It is a very fast tag-based email indexer and system to use with multiple clients, Emacs, Neomutt, and so on.

>
>
> "Not much mail" is what Notmuch thinks about your email collection. Even if you receive 12000 messages per month or have on the order of millions of messages that you've been saving for decades. Regardless, Notmuch will be able to quickly search all of it. It's just plain not much mail.

<!--quoteend-->

>
>
> "Not much mail" is also what you should have in your inbox at any time. Notmuch gives you what you need, (tags and fast search), so that you can keep your inbox tamed and focus on what really matters in your life, (which is surely not email).

```emacs-lisp

(require 'init-mail)

```

```emacs-lisp
;;; init-mail.el --- Mail configuration -*- lexical-binding: t -*-

;;; Commentary:

;; `Notmuch' is a fast, tag-based email indexer to use with your favorite interface (e.g. Emacs :D).
;; I previously used `mu4e', I didn't really like it though.

;; This code is heavily based on Prot's code.
;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-notmuch.el
;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-emacs-modules/prot-emacs-email.el

;; I change the prefix of other people's code, and I **always** mention them.  I hope it is not a problem.

;;; Code:

(defgroup archer-notmuch()
  "Extensions for notmuch."
  :group 'notmuch)

(defcustom archer-notmuch-delete-tag "deleted"
  "Tag that applies to mail marked for deletion."
  :type 'string
  :group 'archer-notmuch)

(defcustom archer-notmuch-mark-delete-tags
  `(,(format "+%s" archer-notmuch-delete-tag) "-inbox" "-unread")
  "List of tags to mark for deletion."
  :type '(repeat string)
  :group 'archer-notmuch)

(defcustom archer-notmuch-mark-archive-tags '( "-deleted" "-inbox" "-unread")
  "List of tags to mark for archive."
  :type '(repeat string)
  :group 'archer-notmuch)

(defcustom archer-notmuch-mark-flag-tags '("+flagged" "-unread")
  "List of tags to mark as important (flagged is a special tag)."
  :type '(repeat string)
  :group 'archer-notmuch)

(defcustom archer-notmuch-mark-spam-tags '("+spam" "-inbox" "-unread")
  "List of tags to mark as spam."
  :type '(repeat string)
  :group 'archer-notmuch)

;;;; Autoload of commands
(autoload 'notmuch-interactive-region "notmuch")
(autoload 'notmuch-tag-change-list "notmuch")
(autoload 'notmuch-search-next-thread "notmuch")
(autoload 'notmuch-search-tag "notmuch")

(defmacro archer-notmuch-search-tag-thread (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag beg end)
     ,(format
       "Mark with `%s' the currently selected thread.
Operate on each message in the currently selected thread.  With
optional BEG and END as points delimiting a region that
encompasses multiple threads, operate on all those messages
instead.
With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the tags.
This function advances to the next thread when finished."
       tags)
     (interactive (cons current-prefix-arg (notmuch-interactive-region)))
     (when ,tags
       (notmuch-search-tag
        (notmuch-tag-change-list ,tags untag) beg end))
     (when (eq beg end)
       (notmuch-search-next-thread))))

(archer-notmuch-search-tag-thread
  archer-notmuch-search-delete-thread
  archer-notmuch-mark-delete-tags)

(archer-notmuch-search-tag-thread
  archer-notmuch-search-flag-thread
  archer-notmuch-mark-flag-tags)

(archer-notmuch-search-tag-thread
  archer-notmuch-search-spam-thread
  archer-notmuch-mark-spam-tags)

(defmacro archer-notmuch-show-tag-message (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag)
     ,(format
       "Apply `%s' to message.
With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the tags."
       tags)
     (interactive "P")
     (when ,tags
       (apply 'notmuch-show-tag-message
              (notmuch-tag-change-list ,tags untag)))))

(archer-notmuch-show-tag-message
  archer-notmuch-show-delete-message
  archer-notmuch-mark-delete-tags)

(archer-notmuch-show-tag-message
  archer-notmuch-show-flag-message
  archer-notmuch-mark-flag-tags)

(archer-notmuch-show-tag-message
  archer-notmuch-show-spam-message
  archer-notmuch-mark-spam-tags)

(autoload 'notmuch-refresh-this-buffer "notmuch")
(autoload 'notmuch-refresh-all-buffers "notmuch")

(defun archer-notmuch-refresh-buffer (&optional arg)
  "Run `notmuch-refresh-this-buffer'.
With optional prefix ARG (\\[universal-argument]) call
`notmuch-refresh-all-buffers'."
  (interactive "P")
  (if arg
      (notmuch-refresh-all-buffers)
    (notmuch-refresh-this-buffer)))

(defun archer-lieer-sendmail ()
  "Set the required variables to send a mail through `lieer'.
To improve."
  (let (from (message-fetch-field "from"))
    (when (string= from "mario.liguori.056@gmail.com")
      (setq-local sendmail-program "gmi"
                  message-sendmail-extra-arguments '("send" "--quiet" "-t" "-C" "~/mails/gmail")))))

;; Current client for mails
(setup notmuch
  (:autoload notmuch notmuch-mua-new-mail)
  ;; UI
  (:option notmuch-show-logo t
           notmuch-column-control 0.5
           notmuch-hello-auto-refresh t
           notmuch-hello-recent-searches-max 15
           notmuch-hello-thousands-separator "."
           notmuch-show-all-tags-list t
           notmuch-hello-insert-footer t
           notmuch-hello-sections
           '(notmuch-hello-insert-header
             notmuch-hello-insert-saved-searches
             notmuch-hello-insert-search
             notmuch-hello-insert-recent-searches
             notmuch-hello-insert-alltags))
  ;; Search
  (:option notmuch-search-oldest-first nil
           notmuch-show-empty-saved-searches t
           notmuch-search-result-format
           '(("date" . "%12s ")
             ("count" . "%-7s ")
             ("authors" . "%-20s ")
             ("subject" . "%80s ")
             ("tags" . "[%s]"))
           notmuch-tree-result-format
           '(("date" . "%12s  ")
             ("authors" . "%-20s")
             ((("tree" . "%s")
               ("subject" . "%s"))
              . " %-80s ")
             ("tags" . "[%s]"))
           notmuch-search-line-faces
           '(("unread" . notmuch-search-unread-face)
             ("flagged" . notmuch-search-flagged-face)))

  ;; Saved searches
  (:option notmuch-saved-searches
           ;; Personal
           `(( :name "📥 inbox (personal)"
               :query "tag:inbox and tag:personal"
               :sort-order newest-first
               :key ,(kbd "p i"))
             ( :name "📔 unread (personal)"
               :query "tag:unread and tag:inbox and tag:personal"
               :sort-order newest-first
               :key ,(kbd "p u"))
             ;; University
             ( :name "📥 inbox (university)"
               :query "tag:inbox and tag:university"
               :sort-order newest-first
               :key ,(kbd "u i"))
             ( :name "📔 unread (university)"
               :query "tag:unread and tag:inbox and tag:university"
               :sort-order newest-first
               :key ,(kbd "u u"))))

  ;; Tags
  (:option notmuch-archive-tags archer-notmuch-mark-archive-tags
           notmuch-message-replied-tags '("+replied")
           notmuch-message-forwarded-tags '("+forwarded")
           notmuch-show-mark-read-tags '("-unread")
           notmuch-draft-tags '("+draft")
           notmuch-draft-folder "drafts"
           notmuch-draft-save-plaintext 'ask)

  ;; Tag formats (with emojis)
  (:option notmuch-tag-formats
           '(("unread" (propertize tag 'face 'notmuch-tag-unread))
             ("flagged" (propertize tag 'face 'notmuch-tag-flagged) ;; Icon is enough
              (concat "🚩")))

           notmuch-tag-deleted-formats
           '(("unread" (notmuch-apply-face bare-tag 'notmuch-tag-deleted)
              (concat "🚫" tag))
             (".*" (notmuch-apply-face tag 'notmuch-tag-deleted)
              (concat "🚫" tag)))

           notmuch-tag-added-formats
           '((".*" (notmuch-apply-face tag 'notmuch-tag-added)
              (concat "✏️" tag))))

  ;; Reading
  (:option notmuch-show-relative-dates t
           notmuch-show-all-multipart/alternative-parts nil
           notmuch-show-indent-messages-width 1
           notmuch-show-indent-multipart t
           notmuch-show-part-button-default-action 'notmuch-show-view-part
           notmuch-show-text/html-blocked-images "." ; block everything
           notmuch-wash-wrap-lines-length 120
           notmuch-unthreaded-show-out nil
           notmuch-message-headers '("To" "Cc" "Subject" "Date")
           notmuch-message-headers-visible t)

  (:option notmuch-wash-citation-lines-prefix 3
           notmuch-wash-citation-lines-suffix 3)

  ;; TODO Composition
  (:option notmuch-mua-compose-in 'current-window
           notmuch-mua-hidden-headers nil
           notmuch-address-command 'internal
           notmuch-always-prompt-for-sender t
           notmuch-mua-cite-function 'message-cite-original
           notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never
           notmuch-mua-user-agent-function nil
           notmuch-maildir-use-notmuch-insert t
           notmuch-crypto-process-mime t
           notmuch-crypto-get-keys-asynchronously t
           notmuch-mua-attachment-regexp   ; see `notmuch-mua-send-hook'
           (concat "\\b\\(attache\?ment\\|attached\\|attach\\|"
                   "pi[èe]ce\s+jointe?\\)\\b"))

  ;; Tagging keys
  (:option notmuch-tagging-keys
           `((,(kbd "d") archer-notmuch-mark-delete-tags "⛔ Mark for deletion")
             (,(kbd "a") archer-notmuch-mark-archive-tags "📫 Mark to archive")
             (,(kbd "f") archer-notmuch-mark-flag-tags "🚩 Flag as important")
             (,(kbd "s") archer-notmuch-mark-spam-tags "⚠️ Mark as spam")
             (,(kbd "r") ("-unread") "✅ Mark as read")
             (,(kbd "u") ("+unread") "📔 Mark as unread")))

  ;; Identities
  (:option notmuch-identies '("mario.liguori.056@gmail.com" "mario.liguori6@studenti.unina.it")
           notmuch-fcc-dirs '(("mario.liguori.056@gmail.com" . "gmail +personal +sent")
                              ("mario.liguori6@studenti.unina.it" . "unina/sent +university +sent")))

  ;; Other cosmetic formatting
  (add-to-list 'notmuch-tag-formats '("encrypted" (concat tag "🔒")))
  (add-to-list 'notmuch-tag-formats '("attachment" (concat tag "📎")))

  (:with-hook notmuch-mua-send-hook
    (:hook notmuch-mua-attachment-check))

  (:global "C-c m" notmuch
           "C-x m" notmuch-mua-new-mail)

  (:bind-into notmuch-search-mode-map
    "/" notmuch-search-filter
    "r" notmuch-search-reply-to-thread
    "R" notmuch-search-reply-to-thread-sender)

  (:bind-into notmuch-show-mode-map
    "r" notmuch-show-reply
    "R" notmuch-show-reply-sender)

  (:bind-into notmuch-search-mode-map
    "a" nil
    "A" notmuch-search-archive-thread
    "D" archer-notmuch-search-delete-thread
    "S" archer-notmcuh-search-spam-thread
    "g" archer-notmuch-refresh-buffer)

  (:bind-into notmuch-show-mode-map
    "a" nil
    "A" notmuch-show-archive-message-then-next-or-next-thread
    "D" archer-notmuch-show-delete-message
    "S" archer-notmuch-show-spam-message))

(setup (:pkg consult-notmuch)
  (:load-after (consult notmuch)))

(setup sendmail
  (:option send-mail-function 'sendmail-send-it
           mail-specify-envelope-from t
           message-sendmail-envelope-from 'header
           mail-envelope-from 'header)
  (:with-hook message-send-hook
    (:hook archer-lieer-sendmail)))

(setup message
  (:option message-cite-style 'message-cite-style-gmail
           message-citation-line-function 'message-insert-formatted-citation-line))

(provide 'init-mail)
;;; init-mail.el ends here
```


### Reading {#reading}

I don't like DocView because the rendering is given by images in tmp storage, zoom is "bad" (for me, of course), rendering can be slow, with especially PDFs big.
My choice is [pdf-tools](https://github.com/vedang/pdf-tools), that renders on demand pages, has good quality, and is very comfortable.

```emacs-lisp

(require 'init-pdf)

```

```emacs-lisp
;;; init-pdf.el --- PDF reading customization, using pdf-tools -*- lexical-binding: t -*-

;;; Commentary:

;; Just pdf-tools installation and set as default

;;; Code:

;; ???
;; (pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1)))
;; (pdf-view-mode-hook . pdf-tools-enable-minor-modes)

(setup (:pkg pdf-tools)
  (:option display-buffer-alist '(("^\\*outline"
                                   display-buffer-in-side-window
                                   (side . left)
                                   (window-width . 0.20)
                                   (inhibit-switch-frame . t))))

  (:with-mode pdf-view-mode
    (:file-match "\\.[pP][dD][fF]\\'"))

  (pdf-tools-install :no-query))

(setup (:pkg saveplace-pdf-view)
  (:load-after pdf-tools))

(provide 'init-pdf)
;;; init-pdf.el ends here
```


### Terminal {#terminal}

The "best" terminal emulator in Emacs.

```emacs-lisp

(require 'init-shell)

```

```emacs-lisp
;;; init-shell.el --- Emacs <3 Shell -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain `eshell', `vterm', and similar terminal emulators available for Emacs.

;;; Code:

(setup (:and (not (archer-using-nix-p))
             (:pkg vterm))
  (:autoload vterm vterm-other-window)
  (:option vterm-buffer-name-string "vterm: %s"
           vterm-max-scrollback 5000
           vterm-kill-buffer-on-exit t))

(setup (:pkg multi-vterm)
  (:load-after vterm)
  (:option multi-vterm-dedicated-window-height-percent 20))

(provide 'init-shell)
;;; init-shell.el ends here
```


### Telegram {#telegram}

Beautiful client, maybe the best telegram client around. A PITA, sometimes, due to tdlib compatibility.

```emacs-lisp

(require 'init-telega)

```

```emacs-lisp
;;; init-telega.el --- Telegram on Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; Here we go.  The idea of using Emacs for everything is (almost) real.
;; `telega' is a great client, maybe the best client around for Telegram.
;; Sometimes it has issues which depend on the version `tdlib' installed on your system, but what the hell: it's good!

;;; Code:

(setup (:and (not (archer-using-nix-p))
             (:pkg telega))

  (:autoload telega)

  (:option telega-use-images t
           telega-emoji-font-family "Noto Color Emoji"
           telega-emoji-use-images nil
           telega-emoji-company-backend 'telega-company-emoji
           telega-completing-read-function completing-read-function
           telega-animation-play-inline 2
           telega-inserter-for-chat-button 'telega-ins--chat-full-2lines
           telega-chat-button-width 30
           switch-to-buffer-preserve-window-point t
           telega-chat--display-buffer-action '((display-buffer-reuse-window display-buffer-use-some-window))
           telega-completing-read-function 'completing-read
           telega-root-fill-column (+ 20 telega-chat-button-width))


  (put (get 'telega-chat 'button-category-symbol)
       :inserter 'telega-ins--chat-full-2lines)

  ;; From Andrew Tropin <3
  (defun archer-telega-chat-mode ()
    "Add completion at point functions made from company backends."
    (setq-local
     completion-at-point-functions
     (mapcar #'cape-company-to-capf (append (list 'telega-company-emoji
                                                  'telega-company-username
                                                  'telega-company-hashtag)
                                            (when (telega-chat-bot-p telega-chatbuf--chat)
                                              '(telega-company-botcmd))))))

  (:when-loaded
    (:also-load telega-mnz)
    (:global "C-c t" telega-prefix-map))

  (:with-mode telega-chat-mode
    (:hook archer-telega-chat-mode)
    (:hook telega-mnz-mode))

  (:with-hook telega-load-hook
    (:hook telega-notifications-mode)))

(provide 'init-telega)
;;; init-telega.el ends here
```


### Media {#media}

Manage your media from Emacs? Possible!

```emacs-lisp

(require 'init-media)

```

```emacs-lisp
;;; init-media.el --- Manage your medias and more from Emacs O-O -*- lexical-binding: t -*-

;;; Commentary:

;; This section is poor right now, but should contain multimedia functionality to avoid leaving Emacs.

;;; Code:

(setup (:pkg mpv))

(setup (:pkg emms)
  (:require emms-setup)
  (emms-all)

  (:option emms-mode-line t
           ;; emms-source-file-default-directory "~/idkrn/"
           emms-info-asynchronously t
           emms-playing-time t
           emms-info-functions '(emms-info-exiftool)
           emms-browser-covers 'emms-browser-cache-thumbnail-async)

  (add-hook 'emms-player-started-hook #'emms-notify-track-description))

(provide 'init-media)
;;; init-media.el ends here
```


### Daemons control {#daemons-control}

Nice mode to control your system (and user) services without leaving Emacs.

```emacs-lisp

(setup (:pkg daemons))

```


## End {#end}

```emacs-lisp

;;; init.el ends here

```

This is the end of my `init.el`, and of my configuration.


## Useful things {#useful-things}


### My experience {#my-experience}

I started using Emacs in late 2021, at the beginning of the third year of university.

Why? I needed something to write notes in a fast way, but I didn't last long: writing notes during my lessons slowed me down, probably because slides given by professors were enough.

Anyway, discovering Emacs was a surprise, and at first it was terrible, because I didn't know where to start!
Too many things to learn, but the community is awesome, resources are good, documentation is almost perfect, and it's VERY fun. So, I gave a chance to myself to learn Emacs.


#### How I learned? {#how-i-learned}

`C-h`, essentially, self-documentation is useful ;).
Also EmacsWiki, videos and blog posts, manual, and so on.


#### Good resources {#good-resources}

My learning path has been discontinuous, but good enough to learn this beautiful piece of software from 1976 (1984, for GNU Emacs).

[System Crafters](https://systemcrafters.cc/)
: helped me a lot with the series \`Emacs from Scratch\`, his channel introduced Emacs to me for the first time. My first configuration was almost a copy-paste of David's configuration...This slowed me down **a lot**.


[Protesilaos Stavrou](https://protesilaos.com/)
: is a gold mine, he's a very clever, wonderful person. I appreciate his verbose explanations about any kind of magic trick he does with Emacs.


[Mike Zamansky](https://www.youtube.com/user/mzamansky)
: has a series dedicated to Emacs, and helped me to figure out some obscure matters.


[Andrew Tropin](https://www.youtube.com/channel/UCuj_loxODrOPxSsXDfJmpng)
: helped me on both Emacs and Nix (now he's using Guix), the problem of reproducibility is fascinating, and this guy is really prepared.


[Steve Purcell](https://github.com/purcell/emacs.d)
: has a dev-centered configuration, but everyone can take inspiration from its dotfiles.


[Vincent Zhang](https://github.com/seagle0128)
: author of [Centaur](https://github.com/seagle0128/.emacs.d), really good work.


[Doom Emacs](https://github.com/doomemacs/doomemacs)
: an opinionated distribution of Emacs, providing many modules and optimizations.
