+++
title = "Emacs Configuration"
author = ["Mario Liguori"]
date = 2022-10-19
tags = ["emacs"]
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

Specifically, there are some tweaks taken from [DOOM Emacs](https://github.com/doomemacs/doomemacs), [David Wilson](https://github.com/daviwil/dotfiles/blob/master/Emacs.org), [Protesilaos Stavrou](https://protesilaos.com/emacs/dotemacs)...but I'll put some credits at the end of this document, along with useful resources.

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

;; From DOOM
;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-deferred-compilation nil)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil))

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

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

;; UTF-8 coding system.
(set-language-environment "UTF-8")

;; Minor message for gc after loading
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))

;;; early-init.el ends here
```


### Mandatory settings for the "init" {#mandatory-settings-for-the-init}

Note that `init.el` is mandatory, however I'm tangling it from this `.org` file (`Emacs.org`).

I have decided to tangle this document in `init.el` because I want to keep a few things in the main directory.

```emacs-lisp

;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;; NOTE: This file is generated from `Emacs.org`!

;;; Code:

;; Add load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; We don't want customizations in `init.el`, instead we use `custom.el`.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Set the right directory to store the native comp cache
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Disable damn sleep!
;; Yep, it's mandatory, that's the worst keybind ever.
(global-unset-key (kbd "C-z"))

```


## Core settings {#core-settings}


### Convenience {#convenience}

Functions to determine if we are on NixOS, or not, then set our path, that's useful for auto-tangle function defined later.

```emacs-lisp

(defun archer/using-nix-p ()
  "Verifies if the running Emacs executable is under the `/nix/store/' path."
  (unless (or (equal system-type 'ms-dos)
              (equal system-type 'windows-nt))
    ;; Since there is no windows implementation of nix
    (string-prefix-p "/nix/store/"
                     (file-truename
                      (executable-find
                       (car command-line-args))))))

(defvar archer/config-path
  (if (archer/using-nix-p)
      (if (file-exists-p (expand-file-name ".dotfiles/config/emacs/" (getenv "HOME")))
          (expand-file-name ".dotfiles/config/emacs/" (getenv "HOME")))
    (expand-file-name user-emacs-directory)))

```


### Packages bootstrap {#packages-bootstrap}

We are requiring `init-packages`, where package manager (e.g. `package.el`, `straight.el`) and macro configuration tools (e.g. `use-package`, `leaf.el`, `setup.el`) are initialized.

Right now I'm using `straight.el` along with `leaf.el`. I love this combo.

-   [straight.el](https://github.com/radian-software/straight.el) provides reproducibility (like Nix and Guix) with recipes, allows the editing of packages and manual version control operations on repos. [Here](https://github.com/radian-software/straight.el#advantages-of-straightel-5) the list of advantages.
-   [leaf.el](https://github.com/conao3/leaf.el) is a more verbose `use-package`, but their syntax is similar, and it's easy to mantain (to add new keywords, for example).

Here I also install [blackout.el](https://github.com/radian-software/blackout), to manage modes displayed in the mode-line.

```emacs-lisp

;; Require package management file.
(require 'init-packages)

```

This is the `init-packages.el` file.

```emacs-lisp
;;; init-packages.el --- Package manager and related configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file should be the `core`.
;; Here are initialized `straight.el` and `leaf`.

;;; Code:

;;; SECTION STRAIGHT.EL
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;; SECTION STRAIGHT.EL ENDS HERE

;;; SECTION LEAF INSTALLATION
(eval-and-compile
  (straight-use-package 'leaf)
  (straight-use-package 'leaf-keywords)
  (straight-use-package 'blackout)

  (leaf leaf-keywords
    (straight-use-package 'leaf-keywords)
    :init
    (leaf blackout (straight-use-package 'blackout))
    :config
    (leaf-keywords-init)))
;;; SECTION LEAF INSTALLATION ENDS HERE

(provide 'init-packages)
;;; init-packages.el ends here
```


### Clean and fast {#clean-and-fast}


#### Keep order {#keep-order}

This useful package helps to avoid filling up our work folders with `auto-save` files.

```emacs-lisp

;; The `no-littering` package to keep folders where we edit files and the Emacs configuration folder clean.
(leaf no-littering
  :doc "Keeps folders clean"
  :straight t
  :setq
  ;; The package `no-littering` doesn't set this by default so we must place
  ;; auto save files in the same path as it uses for sessions
  (auto-save-file-name-transforms . `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

```


#### Garbage collector enhancement {#garbage-collector-enhancement}

[GCMH](https://github.com/emacsmirror/gcmh) allows the auto-regulation of garbage collector based on idle timers. During normal use a high GC threshold is set; when idling GC is triggered and a low threshold is set.

Right now I'm good with 16MB for high threshold.

```emacs-lisp

(leaf gcmh
  :straight t
  :init
  ;; The GC introduces annoying pauses and stuttering into our Emacs experience,
  ;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
  ;; when it's idle. However, if the idle delay is too long, we run the risk of
  ;; runaway memory usage in busy sessions. If it's too low, then we may as well
  ;; not be using gcmh at all.
  (setq gcmh-idle-delay 'auto ; Default 15 seconds
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024 1024)) ; 16MB
  :require t
  :config
  (gcmh-mode 1))

```


#### Minor tweaks {#minor-tweaks}

Aaand, here other code stolen from DOOM.
Performances are really better with this snippet (for me).

```emacs-lisp

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; PGTK builds only: this timeout adds latency to frame operations, like
;; `make-frame-invisible', which are frequently called without a guard because
;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
(setq pgtk-wait-for-event-timeout 0.001)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Shave seconds off startup time by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
      ;; pull in a ton of packages. `doom/open-scratch-buffer' provides a better
      ;; scratch buffer anyway.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Introduced in Emacs 28
(setq use-short-answers t)

```


### Pick me up mom, I'm scared! {#pick-me-up-mom-i-m-scared}

```emacs-lisp

(require 'init-help)

```

Sometimes we forget shortcuts as we type them, [which-key](https://github.com/justbur/emacs-which-key) is a minor mode for Emacs that displays the key bindings following your currently entered incomplete command in a popup.

I'm not using helpful anymore.
~~The [helpful](https://github.com/Wilfred/helpful) adds a lot of very helpful information to `describe-` command buffers. For example, if you use `describe-function`, you will not only get the documentation about the function, you will also see the source code of the function and where it gets used in other places in the Emacs configuration.~~

This is the `init-help.el` file, nothing special.

```emacs-lisp
;;; init-help.el --- Sometimes we need help from someone/something :) -*- lexical-binding: t -*-

;;; Commentary:

;; The minibuffer is our best friend, let's use it more with extensions.

;;; Code:

(leaf which-key
  :doc "Useful panel that appears while pressing any partial binding."
  :straight t
  :blackout t
  :setq
  (which-key-idle-delay . 0.2)
  :config
  (which-key-mode))

(leaf helpful
  :doc "Helpful informations in buffers."
  :disabled t
  :straight t
  :bind
  (("C-h f"   . helpful-callable)
   ("C-h v"   . helpful-variable)
   ("C-h k"   . helpful-key)
   ("C-h C"   . helpful-command)
   ("C-c C-d" . helpful-at-point)))

(provide 'init-help)
;;; init-help.el ends here
```


### Appearance {#appearance}

In this section are contained line-numbers settings, modeline related configuration, minor tweaks for icons (needed also for dashboard) and colors.


#### Font {#font}

```emacs-lisp

(require 'init-fonts)

```

Readability is important.
Currently using [Victor Mono](https://rubjo.github.io/victor-mono/) as font, I love it, also for variable-pitch face.

Here the `init.fonts.el` file.

```emacs-lisp
;;; init-fonts.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; Only font configuration, nothing to say.

;;; Code:

(defvar archer/font-height
  (if (string-equal (system-name) "quietfrost")
      180
    140))

(defun archer/font-setup ()
  "Simple function to initialize font, usually called with simple hook."
  ;; Global fonts
  (set-face-attribute 'default nil
                      :font "VictorMono Nerd Font"
                      :height archer/font-height)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
                      :font "VictorMono Nerd Font"
                      :height archer/font-height)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
                      :font "VictorMono Nerd Font"
                      :height archer/font-height
                      :weight 'light)

  (set-face-attribute 'org-modern-symbol nil
                      :family "Iosevka"))

;; Run this hook after we have initialized the first time.
(add-hook 'after-init-hook 'archer/font-setup)
;; Re-run this hook if we create a new frame from daemonized Emacs.
(add-hook 'server-after-make-frame-hook 'archer/font-setup)

(provide 'init-fonts)
;;; init-fonts.el ends here
```


#### Colors and general UI {#colors-and-general-ui}

```emacs-lisp

(require 'init-appearance)

```

I'm currently using [Modus Themes](https://protesilaos.com/emacs/modus-themes), with [Circadian](https://github.com/guidoschmidt/circadian.el) to set light/dark version, based on time. It's possible to switch themes on sunrise and sunset. Protesilaos made a great work, and these themes are, indeed, built into Emacs.

Here the `init.appearance.el` file.

```emacs-lisp
;;; init-appearance.el --- Appearance settings -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain themes related settings and minor appearance stuff.

;;; Code:

;; Preference, if I'm using mouse, I want dialog-box.
(setq use-dialog-box t     ; Mouse events dialog (yes or no predicate)
      use-file-dialog nil) ; Disable dialog for files

;; Favor vertical splits over horizontal ones.
(setq split-width-threshold 160
      split-height-threshold nil)

(setq-default cursor-type 'bar) ; Cursor type default

(leaf display-line-numbers
  :doc "Line numbers configuration."
  :setq
  (display-line-numbers-type . 'relative)
  (display-line-numbers-width-start . nil)
  (display-line-numbers-grow-only . t)
  :hook
  (prog-mode-hook . display-line-numbers-mode))

 '(telega-entity-type-code        ((t (:inherit fixed-pitch))))

(defun my-modus-themes-custom-faces ()
  (modus-themes-with-colors
    (custom-set-faces
     `(telega-entity-type-code ((,class :inherit modus-themes-fixed-pitch :background ,bg-special-calm :foreground ,fg-special-calm))))))

;; Themes section
;; For packaged versions which must use `require':
(leaf modus-themes
  :doc "Wonderful built-in themes by Protesilaos Stavrou"
  :straight t
  :init
  (setq modus-themes-region '(accented no-extend bg-only) ;; Region highlight
        modus-themes-org-blocks 'gray-background ;; Org source blocks background
        modus-themes-mixed-fonts nil ;; Mixed fonts support, for tables etc.
        modus-themes-deuteranopia nil
        modus-themes-intense-mouseovers nil
        modus-themes-variable-pitch-ui nil ;; Use better font for modeline and UI
        modus-themes-tabs-accented t
        modus-themes-fringes nil ;; Fringes { nil, 'subtle, 'intense}
        modus-themes-markup '(intense)
        modus-themes-syntax '(yellow-comments)
        modus-themes-lang-checkers '(straight-underline faint)
        modus-themes-hl-line '(intense)
        modus-themes-paren-match '(intense)
        modus-themes-links '(no-underline)
        modus-themes-box-buttons '(variable-pitch faint 0.9)
        modus-themes-prompts '(intense bold)
        modus-themes-completions '((matches . (extrabold background))
                                   (selection . (bold accented))
                                   (popup . (accented intense)))
        modus-themes-mail-citations 'intense ; {nil,'intense,'faint,'monochrome}
        modus-themes-subtle-line-numbers nil
        modus-themes-mode-line '(borderless accented))
  (modus-themes-load-themes)
  :hook
  (modus-themes-after-load-theme-hook . my-modus-themes-custom-faces)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi))


;; Change based on time
(leaf circadian
  :straight t
  :config
  (setq circadian-themes '(("8:00" . modus-operandi)
                           ("20:00" . modus-vivendi)))
  (circadian-setup))

;;; Minor tweaks

;; You must run `all-the-icons-install-fonts` the first time.
(leaf all-the-icons
  :doc "Needed for modeline and dired"
  :straight t
  :require t)

(provide 'init-appearance)
;;; init-appearance.el ends here
```


#### Dashboard Configuration {#dashboard-configuration}

Useless cute dashboard, nothing to say, and there are minor tweaks to make it work with server-mode and Emacs PGTK/NativeComp.

```emacs-lisp

(require 'init-dash)

```

Here the `init-dash.el` file.

```emacs-lisp
;;; init-dash.el --- Dashboard configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration of my dashboard, loaded at startup.

;;; Code:

(leaf dashboard
  :straight t
  :blackout t
  :commands (all-the-icons-fileicon
             all-the-icons-faicon
             all-the-icons-octicon)
  :init
  ;; Basic UI settings
  (setq dashboard-banner-logo-title "SUCK(EMAC)S - Personal Workspace")
  (setq dashboard-startup-banner "~/.config/emacs/img/stallman.png")
  (setq dashboard-center-content t)
  ;; Icons
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)))
  (setq dashboard-heading-icons '((recents   . "history")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "briefcase")
                                  (registers . "database")))
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "https://github.com/archer-65/emacs-config")))
           (,(all-the-icons-faicon "archive" :height 1.1 :v-adjust 0.0)
            "Update Packages"
            "Click to updates your packages"
            (lambda (&rest _) (straight-pull-all)))
           (,(all-the-icons-octicon "gear" :height 1.1 :v-adjust 0.0)
            "Configuration"
            "Click to config Emacs"
            (lambda (&rest _) (find-file "~/.dotfiles/config/emacs/Emacs.org"))))))
  (dashboard-setup-startup-hook)
  :hook
  (after-init-hook . dashboard-insert-startupify-lists)
  :config
  ;; Needed with PGTK/NativeComp
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))
  ;; (dashboard-refresh-buffer))

(provide 'init-dash)
;;; init-dash.el ends here
```


#### Modeline {#modeline}

```emacs-lisp

(require 'init-modeline)

```

Just modeline customized.

```emacs-lisp
;;; init-modeline.el --- Modeline customization -*- lexical-binding: t -*-

;;; Commentary:

;; Modeline customization and other useless/cute packages.

;;; Code:

(unless (version< emacs-version "28")
  (setq mode-line-compact nil))

(leaf doom-modeline
  :doc "A very attractive and rich (yet still minimal) mode line configuration for Emacs."
  :straight t
  ;:disabled t
  :hook
  (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-bar-width . 1)
  (doom-modeline-height . 30)
  (doom-modeline-minor-modes . nil)
  (doom-modeline-major-mode-icon . t)
  (doom-modeline-major-mode-color-icon . t)
  (doom-modeline-buffer-file-state-icon . t)
  (doom-modeline-buffer-file-name-style . 'truncate-upto-project))

;; Hiding minor mode can be useful, but right now I have disabled this behavior.
(leaf minions
  :doc "Hide minor modes"
  :straight t
  :disabled t
  :config
  (minions-mode 1))

(provide 'init-modeline)
;;; init-modeline.el ends here
```


## Selection and search {#selection-and-search}

This is one of my favourite parts. I think that fast selection, completing and search are a must, always, everywhere.


### Monster trio of completion {#monster-trio-of-completion}

```emacs-lisp

(require 'init-complete)

```

As Completion UI [Vertico](https://github.com/minad/vertico) is my preferred choice, it's lightweight and fast, and relies on Emacs internals. [Marginalia](https://github.com/minad/marginalia/) for rich annotations provides a summary for candidates.
Completion can be better with an [Orderless](https://github.com/oantolin/orderless) (similar to FZF, if you know). Orderless is also customizable for matching style.

Following, the `init-complete.el` file.

```emacs-lisp
;;; init-complete.el --- Completion enhancement -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs' internal completion is awesome, why should you use Ivy/Helm and similar?
;; They're wonderful, but complex and for me are unnecessary.
;; I'm using Vertico, Orderless and Marginalia (monster trio) for rich, orderless completion style.

;;; Code:

;; Vertico
(leaf vertico
  :doc "Performant and minimalistic vertical completion
        UI based on the default completion system."
  :straight t
  :init
  (vertico-mode)
  :setq
  (vertico-scroll-margin . 0)
  (vertico-count . 15)
  (vertico-cycle . t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(leaf savehist
  :init
  (savehist-mode))

;; Other useful settings
(leaf emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Marginalia
(leaf marginalia
  :doc "Annotations placed at the margin of the minibuffer for completion candidates."
  :straight t
  ;; Enable `marginalia-cycle` globally and in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         (minibuffer-local-map
          ("M-A" . marginalia-cycle)))
  :init
  ;; Must be in the :init such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; Orderless
(leaf orderless
  :doc "Orderless completion style for your Completion UI/Framework"
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(provide 'init-complete)
;;; init-complete.el ends here
```


### Consult {#consult}

```emacs-lisp

(require 'init-consult)

```

[Consult](https://github.com/minad/consult) provides practical commands based on the Emacs completion function completing-read.

Consult offers, for example:

-   Buffer switching command `consult-buffer` to switch between buffers and recently opened files.
-   Multiple asynchronous search commands:
    -   `consult-grep`
    -   `consult-ripgrep`
    -   `consult-line`, which resembles [Swiper](https://github.com/abo-abo/swiper).

Keybindings and configuration are in the `init-consult.el` file.

```emacs-lisp
;;; init-consult.el --- Consult completing read -*- lexical-binding: t -*-

;;; Commentary:

;; .

;;; Code:

(leaf consult
  :doc "Practical commands based on the Emacs completion function completing-read."
  :straight t
  :init
  (add-to-list 'consult-buffer-sources 'coding-buffer-source 'append)
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode specific)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings
         ("C-x M-c" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; [C]-[M]-# bindings for registers
         ("C-M-#" . consult-register)
         ("M-#"   . consult-register-load)
         ("C-#"  . consult-register-store)        ;; orig. abbrev-prefix-mark (unrelated)
         ;; Other custom bindings
         ("M-y"   . consult-yank-pop)              ;; orig. yank-pop
         ("C-h a" . consult-apropos)               ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g g"   . consult-goto-line)           ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o"   . consult-org-heading)         ;; Alternative: consult-org-heading
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ("M-g e"   . consult-compile-error)
         ("M-g f"   . consult-flycheck)            ;; Alternative: consult-flycheck
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         (isearch-mode-map
          ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
          ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
          ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
          ("M-s L" . consult-line-multi)))          ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook
  (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  (setq consult-narrow-key "<")

  ;; Configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(provide 'init-consult)
;;; init-consult.el ends here
```


### Embark {#embark}

```emacs-lisp

(require 'init-embark)

```

[Embark](https://github.com/oantolin/embark/) provides contextual menu offering actions for a target determined in the context, exactly like a contextual menu.

Keybindings and configuration are in the `init-embark.el` file.

```emacs-lisp
;;; init-embark.el --- Embark, run a command based on point-*- lexical-binding: t -*-

;;; Commentary:

;; Sometimes you want to act near point, but there are many actions.
;; Embark ships many actions, dependant on target and modes.

;;; Code:

(defun archer/embark-which-key-indicator ()
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
  '(archer/embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

(defun archer/embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'archer/embark-which-key-indicator embark-indicators)))
      (apply fn args)))

;; Embark configuration
(leaf embark
  :doc "Act near point :D"
  :straight t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  (advice-add #'embark-completing-read-prompter :around #'archer/embark-hide-which-key-indicator)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Used for export and edit after ripgrep magic.
(leaf wgrep
  :doc "Edit matches in place."
  :straight t)

;; Integration with Consult
(leaf embark-consult
  :straight t
  :after (embark consult)
  :require t
  :leaf-defer nil ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-embark)
;;; init-embark.el ends here
```


### Completion at point {#completion-at-point}

```emacs-lisp

(require 'init-complete-in-buffer)

```

I'm using Corfu right now, while Company is here due to `telega.el`, especially for child frames instead of overlays.

Here `init-complete-in-buffer.el`.

```emacs-lisp
;;; init-complete-in-buffer.el --- In buffer completion configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Corfu/Company and extra Dabbrev configuration if needed.

;;; Code:

(leaf company
  ;; :after lsp-mode
  :straight t
  :bind
  (company-active-map
   ("<tab>" . company-complete-selection))
  :hook
  (telega-chat-mode-hook . company-mode))

(leaf corfu
  :straight t
  :custom
  (corfu-cycle . t)                 ;; Enable cycling for `corfu-next/previous'
  (corfu-auto . t)                  ;; Enable auto completion
  (corfu-separator . ?\s)           ;; Orderless field separator
  (corfu-quit-at-boundary . nil)    ;; Never quit at completion boundary
  (corfu-quit-no-match . nil)       ;; Never quit, even if there is no match
  (corfu-preview-current . nil)     ;; Disable current candidate preview
  (corfu-preselect-first . nil)     ;; Disable candidate preselection
  (corfu-on-exact-match . nil)      ;; Configure handling of exact matches
  (corfu-echo-documentation . 0.25) ;; Disable documentation in the echo area
  (corfu-scroll-margin . 5)         ;; Use scroll margin

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)

  ;; Load and enable corfu-history
  (load "extensions/corfu-history")
  (corfu-history-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold t))

(provide 'init-complete-in-buffer)
;;; init-complete-in-buffer.el ends here
```


## Interface interaction {#interface-interaction}

This section contains my file and buffer related configurations. Nothing special.


### Editing enhancement {#editing-enhancement}

```emacs-lisp

(require 'init-editing)

```

Tweaks present here:

-   Scroll (and smooth scroll for Emacs &gt;= 29) and horizontal scroll with mouse;
-   Truncate lines hook for `prog-mode`;
-   Electric-pair mode and show-paren;
-   Autorevert files after changes;
-   [Undo Tree](https://www.emacswiki.org/emacs/UndoTree) mode for simpler undo-redo (and visual branches!).
-   Rainbow-mode;
-   Delete-selection mode to overwrite selected regions;
-   Drag-stuff to...drag stuff around.

Following, my `init-editing.el`.

```emacs-lisp
;;; init-editing.el --- Basic editing configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file is pretty simple, it only contains editing related utilities and preferences.
;; It's still experimental and very poor, so I only consider it a starting point.

;;; Code:

;;
;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;;
;;; Scrolling

;; Enable smooth scroll
(unless (version< emacs-version "29")
  (pixel-scroll-precision-mode 1))

;; General tweaks
(setq scroll-preserve-screen-position t
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically re-centered.
      scroll-conservatively 101
      hscroll-margin 2
      hscroll-step 1
      scroll-margin 0
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Editing enhancements for `prog-mode`
(add-hook 'prog-mode-hook #'visual-line-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)

;;; Pairs? I forget to balance every kind of pair, I need this.
(electric-pair-mode 1)
(show-paren-mode 1)

;; Save place
(save-place-mode 1)

;;; Protesilaos Stavrou docet, system clipboard should have priority among kill-ring
(setq save-interprogram-paste-before-kill t)

(leaf delsel
  :doc "Should be default IMHO."
  :blackout t
  :hook
  (after-init-hook . delete-selection-mode))

(leaf drag-stuff
  :doc "Drag stuff around with alt+arrows"
  :straight t
  :blackout t
  :init
  (drag-stuff-global-mode 1)
  :config
  (drag-stuff-define-keys))

(leaf goto-last-change
  :doc "Oops, forgot position of last edit? Go back. (Thanks again, Prot)"
  :straight t
  :bind
  ("C-z" . goto-last-change))

(leaf autorevert
  :doc "Every time I have to confirm buffer reverts, do it yourself, Emacs"
  :blackout t
  :setq
  (auto-revert-verbose . t)
  :hook
  (after-init-hook . global-auto-revert-mode))

(leaf rainbow-mode
  :doc "Minor mode to set background of string matching hex colors to the hex color."
  :straight t
  :hook
  ((emacs-lisp-mode web-mode json-mode) . rainbow-mode))

(provide 'init-editing)
;;; init-editing.el ends here
```


#### Meow {#meow}

```emacs-lisp

;; (require 'init-meow)

```

[Meow](https://github.com/meow-edit/meow) is yet another modal editing mode for Emacs. Meow aims to blend modal editing into Emacs with minimal interference with its original key-bindings, avoiding most of the hassle introduced by key-binding conflicts.

Keybindings are listed in `init-meow.el`.

```emacs-lisp
;;; init-meow.el --- Meow modal editing -*- lexical-binding: t -*-

;;; Commentary:

;; A kind of modal editing, alternative to evil and xah-fly-keys.

;;; Code:

(defun archer/meow-setup ()
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
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
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
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(leaf meow
  :straight t
  :require t
  :config
  (archer/meow-setup)
  (meow-global-mode 1))

(provide 'init-meow)
;;; init-meow.el ends here
```


### Windows navigation {#windows-navigation}

```emacs-lisp

(require 'init-windows)

```

Moving around windows can be painful, but some built-in functions save our a\*s.

```emacs-lisp
;;; init-windows.el --- Windows navigation configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Only movement between buffers/frames, nothing special.

;;; Code:

(leaf windmove
  :doc "Utility to move faster between buffers"
  :config
  (windmove-default-keybindings) ; Windmove with shift+arrows
  :hook
  (org-shiftup-final-hook    . windmove-up)
  (org-shiftdown-final-hook  . windmove-down)
  (org-shiftleft-final-hook  . windmove-left)
  (org-shiftright-final-hook . windmove-right))

(leaf window
  :bind
  ("C-S-k" . enlarge-window)
  ("C-S-j" . shrink-window)
  ("C-S-h" . shrink-window-horizontally)
  ("C-S-l" . enlarge-window-horizontally))

(leaf ace-window
  :bind
  ("M-o" . ace-window)
  ("M-O" . ace-swap-window)
  :init
  (setq aw-scope 'frame
        aw-dispatch-always t
        aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

(leaf avy
  :init
  (setq avy-all-windows nil   ;; only current
        avy-all-windows-alt t ;; all windows with C-u
        avy-single-candidate-jump t
        avy-case-fold-search nil
        avy-timeout-seconds 0.5
        avy-style 'pre)
  :bind
  ("M-g j" . avy-goto-char-timer))

(provide 'init-windows)
;;; init-windows.el ends here
```


### Buffer management {#buffer-management}

```emacs-lisp

(require 'init-buffers)

```

Sometimes buffers are too much, and I think that the classic buffer-menu is meh.
With `ibuffer` I can group buffers in `Gnus` style, customize actions remembering `Dired`, and so on.

```emacs-lisp
;;; init-buffers.el --- Buffer navigation -*- lexical-binding: t -*-

;;; Commentary:

;; Buffer navigation and management

;;; Code:

(defun archer/human-readable-file-sizes-to-bytes (string)
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

(defun archer/bytes-to-human-readable-file-sizes (bytes)
  "Convert number of BYTES to human-readable file size."
  (interactive)
  (cond
   ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
   ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
   ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
   ((> bytes 100000) (format "%10.0fk" (/ bytes 1000.0)))
   ((> bytes 1000) (format "%10.1fk" (/ bytes 1000.0)))
   (t (format "%10d" bytes))))

(leaf ibuffer
  :require t
  :config
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t :summarizer
                 (lambda (column-strings)
                   (let ((total 0))
                     (dolist (string column-strings)
                       (setq total (+ (float (archer/human-readable-file-sizes-to-bytes string))
                                                  total)))
                     (archer/bytes-to-human-readable-file-sizes total))))
    (archer/bytes-to-human-readable-file-sizes (buffer-size)))
  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
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
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("dired" (mode . dired-mode))
           ("git" (or (mode . magit-mode)
                      (mode . magit-process-mode)
                      (mode . magit-diff-mode)
                      (mode . magit-status-mode)))
           ("elisp" (mode . emacs-lisp-mode))
           ("c/c++" (or (mode . c-mode)
                        (mode . c++-mode)))
           ("nix" (mode . nix-mode))
           ("telegram" (or (mode . telega-mode)
                           (mode . telega-chat-mode)))
           ("documents" (or (name . "\\.pdf")
                            (name . "\\.org")))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")
                     (name . "^\\*straight-process\\*$")
                     (name . "^\\*dashboard\\*$"))))))
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary t)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-movement-cycle t)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  :hook
  (ibuffer-mode-hook . (lambda ()
                         (ibuffer-switch-to-saved-filter-groups "default")
                         (ibuffer-auto-mode 1)))
  :bind
  ("C-x C-b" . ibuffer))

;;; Unique names for buffers
(leaf uniquify
  :require t
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

(provide 'init-buffers)
;;; init-buffers.el ends here
```


### Dired {#dired}

```emacs-lisp

(require 'init-dired)

```

Dired is a built-in file manager for Emacs that does some pretty amazing things. For example you can enable writable dired buffer to edit everything and just save to apply your changes.

I have disabled `dired-find-alternate-file` warning, I'm using it 'cause pressing `Return` key just opens too many buffers.

There's also a package named `trashed`, to visit system trash.

This if my `init.dired.el` file.

```emacs-lisp
;;; init-dired.el --- Dired -*- lexical-binding: t -*-

;;; Commentary:

;; Dired utilities and configuration for a better experience.

;;; Code:

(defun archer/dired-open-file ()
  "In Dired, open the file named on this line through xdg-open."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(leaf dired
  :commands (dired dired-jump)
  :bind
  ("C-x C-j" . dired-jump)
  ("C-c d" . dired-omit-mode)
  (:dired-mode-map
   ("C-c o" . archer/dired-open-file))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-omit-files "^\\.?#\\|^\\.$")
  (unless (version< emacs-version "29")
    (setopt dired-mouse-drag-files t))
  :custom
  (dired-listing-switches . "-agho --group-directories-first")
  :hook
  (dired-load-hook . dired-collapse)
  (dired-mode-hook . dired-omit-mode))

(leaf all-the-icons-dired
  :straight t
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(leaf trashed
  :doc "Visit system trash."
  :straight t
  :require t
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t)))

(provide 'init-dired)
;;; init-dired.el ends here
```


## Org Mode {#org-mode}

Org mode is the killer feature of Emacs. Markup language, agenda, brain, templates...you can do _literally_ (xD) everything.


### Essential configuration {#essential-configuration}

```emacs-lisp

(require 'init-org)

```

I absolutely need focus when I'm editing my documents in the dark, so I want my buffer centered and lines untruncated.

Indentation is defined as a function for basic org-mode setup.

The purpose of ~~[visual-fill-column](https://github.com/joostkremers/visual-fill-column)~~ [olivetti ](https://github.com/rnkn/olivetti)is to center `org-mode` buffers for a more pleasing writing experience as it centers the contents of the buffer horizontally to seem more like you are editing a document.

[Org Modern](https://github.com/minad/org-modern) replaces markup syntax with nice headings, TODOs etc.

```emacs-lisp
;;; init-org.el --- Org mode configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Org mode is certainly the killer feature of Emacs.
;; You can do anything, for example capturing of templates, export, markdown like editing.

;;; Code:

(defun archer/org-mode-setup ()
  "Set important modes for me while editing org documents.

- Indentation to distinguish headings is essential;
- Setting variable-pitch allows different face definition;
- I prefer visual-line here, instead of truncating lines."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun archer/org-mode-visual-fill ()
  "Width of visual fill and centered text are configured here."
  (setq visual-fill-column-width 170
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(leaf org
  :straight t
  :require t
  :hook
  (org-mode-hook . archer/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-pretty-entities 't)
  ;; Enable live previews
  (setq org-highlight-latex-and-related '(latex))
  ;; Date settings
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%d %b %Y>" . "<%d/%m/%y %a %H:%M>"))
  ;; Languages for org-src
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(leaf org-modern
  :doc "Org bullets? Old."
  :straight t
  :after org
  :require t
  :hook (org-mode-hook . org-modern-mode))

(leaf visual-fill-column
  :doc "Center buffers, word processor like editing"
  :disabled t
  :straight t
  :hook
  (org-mode-hook . archer/org-mode-visual-fill))

(leaf olivetti
  :doc "Focused writing, like visual-fill-column, but seems better."
  :straight t
  :hook
  (org-mode-hook . olivetti-mode)
  :config
  (setq olivetti-body-width 0.75)
  (setq olivetti-minimum-body-width 75)
  (setq olivetti-style 'fancy))

(provide 'init-org)
;;; init-org.el ends here
```


### Babel and Tempo {#babel-and-tempo}

```emacs-lisp

(require 'init-org-languages)

```

To execute or export code in `org-mode` code blocks, we need to set up `org-babel-load-languages` for each language. [This page](https://orgmode.org/worg/org-contrib/babel/languages.html) documents all of the languages that you can use with `org-babel`.

Org Mode's [structure templates](https://orgmode.org/manual/Structure-Templates.html) feature enables to quickly insert code blocks into your Org files in combination with `org-tempo` by typing `<` followed by the template name like `el` or `py` and then press `TAB`.
To add more `src` block templates, just copy one of the lines and change the two strings at the end, the first to be the template name and the second to contain the name of the language ([listed here](https://orgmode.org/worg/org-contrib/babel/languages.html)).

There's also a snippet that adds a hook to `org-mode` buffers so that `archer/org-babel-tangle-config` gets executed each time such a buffer gets saved. This function checks to see if the file being saved is the Emacs.org file you're looking at right now, and if so, automatically exports the configuration here to the associated output files. This function is inspired by David Wilson of System Crafters.

```emacs-lisp
;;; init-org-languages.el --- Language related org settings -*- lexical-binding: t -*-

;;; Commentary:

;; We can execute code in org-mode, but also define structure templates
;; to insert blocks (like src blocks).
;; Tangling is also an important feature, let's use it.

;;; Code:

;; Org-Babel
(leaf org-babel-load-languages
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (shell . t))))

;; Tempo
(leaf org-tempo
  :after org
  :require t
  :config
  (add-to-list 'org-structure-template-alist '("bash" . "src bash"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cc" . "src c"))
  (add-to-list 'org-structure-template-alist '("j" . "src java"))
  :hook
  (org-mode-hook . (lambda () (setq-local electric-pair-inhibit-predicate
                                          `(lambda (c)
                                             (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))))

;; Auto tangling
(defun archer-65/org-babel-tangle-config ()
  "Auto tangle configuration on save if we are in the right directory."
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name archer/config-path))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(leaf ob-tangle
  :hook
  (org-mode-hook . (lambda () (add-hook 'after-save-hook #'archer-65/org-babel-tangle-config))))

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
(leaf ox-latex
  :require t
  :config
  ;; Newpage after TOC
  (setq org-latex-toc-command "\\tableofcontents \\clearpage")
  ;; LaTeX process
  (setq org-latex-pdf-process
        '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
  ;; Previewing LaTeX fragments in org mode, default changed for bad quality.
  (setq org-latex-create-formula-image-program 'imagemagick)
  ;; Using minted for tables
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-minted-options '(("breaklines" "true")
                                   ("breakanywhere" "true")))
  ;; LaTeX classes
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; Enable listings
  (setq org-latex-listings 't))

;; Reveal.js
(leaf ox-reveal
  :require t
  :straight (ox-reveal :type git :host github :repo "yjwen/org-reveal")
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

;; Hugo
(leaf ox-hugo
  :straight t
  :hook
  (org-mode-hook . org-hugo-auto-export-mode)
  :after ox)

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

(leaf treemacs
  :doc "Tree style directory visualization"
  :straight t
  :config
  (setq treemacs-width-is-initially-locked nil))

(leaf projectile
  :doc "Project management and navigation"
  :straight t
  :blackout t
  :config
  (projectile-mode)
  ;; :bind-keymap
  ;; ("C-c p"   . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(leaf magit
  :doc "Git interface"
  :straight t
  :commands magit-status
  :custom
  (magit-display-buffer-function . 'magit-display-buffer-same-window-except-diff-v1))

(leaf forge
  :after magit
  :straight t)

(provide 'init-projects)
;;; init-projects.el ends here
```


### Code style {#code-style}

```emacs-lisp

(require 'init-code-style)

```

[Ethan-wspace](https://github.com/glasserc/ethan-wspace) is a nice package to avoid useless/horrible extra whitespaces.

[Rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters) is useful in programming modes because it colorizes nested parentheses and brackets according to their nesting depth.  This makes it a lot easier to visually match parentheses in Emacs Lisp code without having to count them yourself.

The rest of `init-code-style.el` regards tab settings.

```emacs-lisp
;;; init-code-style.el --- Code style settings -*- lexical-binding: t -*-

;;; Commentary:

;; OCD, so I have to remove useless whitespace after save or on demand.
;; Plus, general tab settings.

;;; Code:

(leaf ethan-wspace
  :doc "Delete useless whitespaces"
  :straight t
  :bind ("C-c c" . ethan-wspace-clean-all)
  :hook (prog-mode-hook . ethan-wspace-mode)
  :config
  (leaf files
    :doc "Required by ethan-wspace"
    :config
    (setq mode-require-final-newline nil)
    (setq require-final-newline nil)))

;; Tabs, indentation, and the TAB key
(leaf indent
  :doc "Tab settings"
  (setq-default tab-always-indent 'complete)
  (setq-default tab-first-completion 'word-or-paren-or-punct)
  (setq-default tab-width 2)
  ;; Use spaces!
  (setq-default indent-tabs-mode nil))

(leaf rainbow-delimiters
  :straight t
  :require t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf tree-sitter
  :straight t
  :commands (tree-sitter-mode tree-sitter-hl-mode)
  :hook
  ((emacs-lisp-mode c-mode c++-mode java-mode python-mode) . tree-sitter-mode)
  (tree-sitter-after-on-hook . tree-sitter-hl-mode))

(leaf tree-sitter-langs
  :straight t
  :after treesitter)

(provide 'init-code-style)
;;; init-code-style.el ends here
```


### Syntax checking {#syntax-checking}

Modern on-the-fly syntax checking.

```emacs-lisp

(require 'init-spell-and-check)

```

I'm a bit distracted, so spell and syntax checking is essential for me.

This is `init-spell-and-check.el`.

```emacs-lisp
;;; init-spell-and-check.el --- Spell and syntax checking based on modes -*- lexical-binding: t -*-

;;; Commentary:

;; Flyspell as spell checker, while Flycheck as syntax checker for prog-mode.

;;; Code:

(leaf flyspell
  :hook
  (text-mode-hook . (lambda () flyspell-mode 1))
  (prog-mode-hook . flyspell-prog-mode))

(leaf flycheck
  :straight t
  :commands (flycheck-list-errors flycheck-buffer)
  :config
  (global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path . 'inherit)
  (flycheck-idle-change-delay . 1.0)
  (flycheck-display-errors-delay . 0.25)
  (flycheck-emacs-lisp-initialize-packages . t))

(provide 'init-spell-and-check)
;;; init-spell-and-check.el ends here
```


### Extra modes {#extra-modes}

```emacs-lisp

(require 'init-extra-modes)

```

`init.extra.modes.el`

```emacs-lisp
;;; init-extra-modes.el --- Extra modes -*- lexical-binding: t -*-

;;; Commentary:

;; This is not divided in multiple files, it's useless, I'm good this way :D.

;;; Code:

(leaf nix-mode
  :straight t
  :mode "\\.nix\\'")

(leaf yaml-mode
  :straight t
  :mode "\\.yml\\'")

(leaf json-mode
  :straight t
  :mode "\\.json\\'")

(provide 'init-extra-modes)
;;; init-extra-modes.el ends here
```


### Yasnippet {#yasnippet}

```emacs-lisp

(require 'init-snippets)

```

Do I have to explain this?

Here `init-snippets.el`.

```emacs-lisp
;;; init-snippets.el --- Snippets -*- lexical-binding: t -*-

;;; Commentary:

;; Remember code snippet for common functions? Bleah.

;;; Code:

(leaf yasnippet
  :straight t
  :hook
  (prog-mode-hook . yas-minor-mode)
  :config
  (yas-reload-all))

(leaf yasnippet-snippets
  :straight t
  :after yasnippet)

(provide 'init-snippets)
;;; init-snippets.el ends here
```


### LSP {#lsp}

```emacs-lisp

(require 'init-lsp)

```

[Language Server Protocol](https://github.com/emacs-lsp/lsp-mode) support with multiples languages support for Emacs.

```emacs-lisp
;;; init-lsp.el --- Language Server Protocol client -*- lexical-binding: t -*-

;;; Commentary:

;; Here the configuration for LSP-Mode/Eglot.

;;; Code:

(leaf lsp-mode
  :commands lsp
  :straight t
  ;; :bind
  ;; (lsp-mode-map
  ;;   ("<tab>" . company-indent-or-complete-common))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                                        :major-modes '(nix-mode)
                                        :server-id 'nix))
  :hook
  (c-mode-hook    . lsp)
  (c++-mode-hook  . lsp)
  (java-mode-hook . lsp)
  (nix-mode-hook  . lsp)
  (lsp-mode-hook  . lsp-enable-which-key-integration))

(leaf lsp-ui
  :straight t
  :after lsp
  :commands lsp-ui-mode)

(leaf lsp-treemacs
  :straight t
  :after lsp
  :commands lsp-treemacs-errors-list)

(leaf lsp-java
  :straight t
  :after lsp
  :require t
  :hook
  (java-mode-hook . lsp))

;; optionally if you want to use debugger
;; (leaf dap-mode)
;; (leaf dap-LANGUAGE) to load the dap adapter for your language

(provide 'init-lsp)
;;; init-lsp.el ends here
```


## Frontend for other uses {#frontend-for-other-uses}

Emacs can be a frontend for almost everything.


### Mails {#mails}

```emacs-lisp

(require 'init-mail)

```

`mu4e` (mu-for-emacs) is an e-mail client for GNU Emacs version 24.4 or higher, built on top of the `mu` e-mail search engine. mu4e is optimized for quickly processing large amounts of e-mail.

Some of its highlights:

-   Fully search-based: there are no folders, only queries;
-   Fully documented, with example configurations;
-   User-interface optimized for speed, with quick key strokes for common actions;
-   Support for non-English languages (so “angstrom” matches “Ångström”);
-   Asynchronous: heavy actions don’t block emacs;
-   Support for cryptography — signing, encrypting and decrypting;
-   Address auto-completion based on the contacts in your messages;
-   Extendable with your own snippets of elisp.

There's `mu4e-alert` a good package for notifications, a bit buggy, but is working :D.

Last but not least: [org-msg](https://github.com/jeremy-compostella/org-msg), an Outlook email style to compose (and reply to) emails, working also for `Gnus`.

```emacs-lisp
;;; init-mail.el --- Mail configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Right now I'm using MU4E, I'm good with it, is used by many people, so finding snippets
;; is easy.  I could try Notmuch, however.

;;; Code:

;; Actual client for mails
(leaf mu4e
  :require t
  :commands mu4e mu4e-compose-new
  :load-path "~/.nix-profile/share/emacs/site-lisp/mu4e"
  :init
  (provide 'html2text)
  :config
   ;; Load org-mode integration
  (require 'org-mu4e)
  (require 'mu4e-contrib)

  ;; General
  (setq mu4e-get-mail-command "mbsync -a"
        ;; Update every 5 minutes
        mu4e-update-interval (* 5 60)
        ;; Images
        mu4e-view-show-images t
        mu4e-view-image-max-width 800
        ;; Don't keep message buffers around
        message-kill-buffer-on-exit t
        ;; Start with default context
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask-if-none
        ;; Why confirm quit?
        mu4e-confirm-quit nil)

  ;; User agent
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Use mu4e for sending e-mail
  (setq send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail)

  ;; Mail settings
  (setq mu4e-maildir "~/mails"
        ;; This is set to 't' to avoid mail syncing issues when using mbsync
        mu4e-change-filenames-when-moving t
        mu4e-main-buffer-hide-personal-addresses t)

  (setq mu4e-contexts
        `(
          ;; Gmail Primary (new) Account
          ,(make-mu4e-context
            :name "Gmail Primary"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/GmailPrimary" (mu4e-message-field msg :maildir))))
            :vars '((user-email-address . "mario.liguori.056@gmail.com")
                    (smtpmail-smtp-user . "mario.liguori.056@gmail.com")
                    (user-full-name     . "Mario Liguori")
                    (mu4e-sent-folder   . "/Gmail/[Gmail]/Sent Mail")
                    (mu4e-drafts-folder . "/Gmail/[Gmail]/Drafts")
                    (mu4e-trash-folder  . "/Gmail/[Gmail]/Trash")
                    (mu4e-maildir-shortcuts .
                                            (("/Gmail/Inbox"     . ?i)
                                             ("/Gmail/[Gmail]/Sent Mail" . ?s)
                                             ("/Gmail/[Gmail]/Trash"     . ?t)
                                             ("/Gmail/[Gmail]/Drafts"    . ?d)))))

          ;;UniNa
          ,(make-mu4e-context
            :name "Unina"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Unina" (mu4e-message-field msg :maildir))))
            :vars '((user-email-address . "mario.liguori6@studenti.unina.it")
                    (smtpmail-smtp-user . "mario.liguori6@studenti.unina.it")
                    (user-full-name     . "Mario Liguori")
                    (mu4e-drafts-folder . "/Unina/Bozze")
                    (mu4e-sent-folder   . "/Unina/Posta inviata")
                    (mu4e-trash-folder  . "/Unina/Deleted Items")
                    (mu4e-maildir-shortcuts .
                                            (("/Unina/Inbox"         . ?i)
                                             ("/Unina/Posta inviata" . ?s)
                                             ("/Unina/Deleted Items" . ?t)
                                             ("/Unina/Bozze"         . ?d)))))))
  ;; Set Bookmarks for all
  (setq  mu4e-bookmarks '(( :name  "Unread messages"
                            :query "flag:unread AND NOT flag:trashed"
                            :key ?u)
                          ( :name "Today's messages"
                            :query "date:today..now"
                            :key ?t)))

  (mu4e t))

;; Notifications!
(leaf mu4e-alert
  :doc "Enable notifications for mu4e"
  :straight t
  :after mu4e
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-notifications)
  (setq mu4e-alert-notify-repeated-mails nil))

;; Org enhanced messages
(leaf org-msg
  :straight t
  :after (mu4e org)
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-default-alternatives '((new		. (text html))
                                       (reply-to-html	. (text html))
                                       (reply-to-text	. (text)))
        org-msg-convert-citation t)
  (org-msg-mode))

(provide 'init-mail)
;;; init-mail.el ends here
```


### Productivity {#productivity}

```emacs-lisp

(leaf pomodoro
  :straight t
  :require t
  :custom
  (pomodoro-desktop-notification . t)
  :config
  (pomodoro-add-to-mode-line))

```


### Reading {#reading}

```emacs-lisp

(require 'init-pdf)

```

I don't like DocView because the rendering is given by images in tmp storage, zoom is "bad" (for me, of course), rendering can be slow, with especially PDFs big.
My choice is [pdf-tools](https://github.com/vedang/pdf-tools), that renders on demand pages, has good quality, and is very comfortable.

```emacs-lisp
;;; init-pdf.el --- PDF reading customization, using pdf-tools -*- lexical-binding: t -*-

;;; Commentary:

;; Just pdf-tools installation and set as default

;;; Code:

(leaf pdf-tools
  :straight t
  :commands (pdf-view-mode pdf-tools-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (display-buffer-alist . '(("^\\*outline"
                            display-buffer-in-side-window
                            (side . left)
                            (window-width . 0.20)
                            (inhibit-switch-frame . t))))
  :config
  (pdf-tools-install)
  (define-pdf-cache-function pagelabels)
  :hook
  (pdf-view-mode-hook . (lambda () (display-line-numbers-mode -1)))
  (pdf-view-mode-hook . pdf-tools-enable-minor-modes))

(leaf saveplace-pdf-view
  :straight t
  :require t
  :after pdf-tools)

(provide 'init-pdf)
;;; init-pdf.el ends here
```


### Daemons control {#daemons-control}

Nice mode to control your system (and user) services without leaving Emacs.

```emacs-lisp

(leaf daemons
  :straight t)

```


### Terminal {#terminal}

The best terminal emulator in Emacs.

```emacs-lisp

(leaf vterm
  :commands (vterm vterm-other-window)
  :init
  (unless (archer/using-nix-p) straight-use-package 'vterm)
  :bind
  ("<f5>" . vterm)
  :config
  (setq-default vterm-buffer-name " <<Terminal>>")
  (add-to-list 'display-buffer-alist
               '("\xe795 <<Terminal>>" ;; Original regex: "\*vterm\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.25)
                 (side . bottom)
                 (slot . 0))))

```


### Telegram {#telegram}

Beautiful client, maybe the best telegram client around. A PITA, sometimes, due to tdlib compatibility.

```emacs-lisp

(leaf telega
  :commands (telega)
  :init
  (unless (archer/using-nix-p) straight-use-package 'telega)
  (setq telega-directory (expand-file-name "~/.telega/"))
  (setq telega-server-libs-prefix (expand-file-name "~/.nix-profile"))
  (setq telega-use-images t)
  (setq telega-emoji-font-family "Noto Color Emoji")
  (setq telega-emoji-use-images nil)
  :config
  (require 'telega-mnz)
  (setq telega-animation-play-inline 2)
  (setq telega-inserter-for-chat-button 'telega-ins--chat-full-2lines)
  (setq telega-chat-button-width 30)
  (setq telega-root-fill-column (+ 20 telega-chat-button-width))
  (put (get 'telega-chat 'button-category-symbol)
       :inserter 'telega-ins--chat-full-2lines)
  (setq switch-to-buffer-preserve-window-point t)
  (setq telega-chat--display-buffer-action
        '((display-buffer-reuse-window display-buffer-use-some-window)))
  (define-key global-map (kbd "C-c t") telega-prefix-map)
  (setq telega-completing-read-function 'completing-read)
  :hook
  (telega-load-hook . telega-notifications-mode)
  (telega-chat-mode-hook . telega-mnz-mode))

```


### Music {#music}

Manage your music from Emacs? Possible!

```emacs-lisp

(leaf emms
  :straight t
  :init
  ;; Notification on play
  (defun emms-notify-track-description ()
    "Use `notify-send' to show the description of the currecnt track."
    (call-process
     "notify-send"
     nil nil nil
     "-a" "EMMS"
     "-t" "1000"
     "-h" "string:x-dunst-stack-tag:test"
     "-a" "music"
     (emms-track-description
      (emms-playlist-current-selected-track))))
  :config
  ;; Start
  (require 'emms-setup)
  (require 'emms-mode-line)
  (require 'emms-playing-time)
  (emms-all)

  ;; Info
  (setq emms-mode-line t)
  (setq emms-playing-time t)

  ;; Directory
  ;; (setq emms-source-file-default-directory "~/idkrn/")
  (setq emms-info-asynchronously t)

  ;; Other infos, covers
  (setq emms-info-functions '(emms-info-exiftool)
        emms-browser-covers 'emms-browser-cache-thumbnail-async)
  :hook
  (emms-player-started-hook . emms-notify-track-description))

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


#### Good people {#good-people}

My learning path has been discontinuous, but good enough to learn this beautiful piece of software from 1976 (1984, for GNU Emacs).

[System Crafters](https://systemcrafters.cc/)
: helped me a lot with the series \`Emacs from Scratch\`, in fact my first configuration was almost a copy-paste of David's configuration...This slowed me down **a lot**.


[Protesilaos Stavrou](https://protesilaos.com/)
: is a gold mine, he's a very clever and wonderful person. I appreciate his verbose explanations about any kind of magic trick he does with Emacs.


[Mike Zamansky](https://www.youtube.com/user/mzamansky)
: has a series dedicated to Emacs, he works well, and helped me to figure out some obscure matters.


[Andrew Tropin](https://www.youtube.com/channel/UCuj_loxODrOPxSsXDfJmpng)
: helped me on both Emacs and Nix (now he's using Guix), the problem of reproducibility is fascinating, and this guy is really prepared.


[Steve Purcell](https://github.com/purcell/emacs.d)
: has a dev-centered configuration, but everyone can take inspiration from its dotfiles.


[Vincent Zhang](https://github.com/seagle0128)
: author of [Centaur](https://github.com/seagle0128/.emacs.d), really good work.
