(setq user-full-name "Thomas Kobber Panum")

(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(eval-and-compile
  (setq load-prefer-newer t
        package-user-dir "~/.emacs.d/elpa"
        package--init-file-ensured t
        package-enable-at-startup nil)


  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t)))

(setq use-package-always-defer t
      use-package-verbose t)

(eval-and-compile
  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

(eval-when-compile
  (require 'package)

  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

  (unless (assoc-default "org" package-archives)
    (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

  (package-initialize)
  (package-refresh-contents)

  ;; remove built-in org mode
  (package-built-in-p 'org)
  (setq package--builtins (assq-delete-all 'org package--builtins))

  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (require 'use-package)
  (setq use-package-always-ensure t))

(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "") ; print a default message in the empty scratch buffer opened at startup
(setq calendar-date-style "european")

(menu-bar-mode 0)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq mouse-highlight nil)
(setq column-number-mode t)
(setq-default cursor-in-non-selected-windows nil)
(setq x-underline-at-descent-line t)
(setq x-stretch-cursor t)
(setq frame-resize-pixelwise t)
(setq uniquify-buffer-name-style 'forward)
(show-paren-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(set-mouse-color "#ffffff")
(set-frame-font "Hasklig-13")

(use-package hasklig-mode
             :hook (prog-mode))



(use-package modus-operandi-theme
  :config
  (load-theme 'modus-operandi t)
  (setq modus-operandi-theme-slanted-constructs t)
  (setq modus-operandi-theme-bold-constructs t))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  :custom
  (initial-buffer-choice '(lambda ()
                            (setq initial-buffer-choice nil)
                            (get-buffer "*dashboard*")))
  (dashboard-items '((agenda . 10))))

(use-package all-the-icons)

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(setq-default mode-line-modified
              '(:eval
                (if (buffer-file-name)
                    (if (file-exists-p (buffer-file-name)) (if (buffer-modified-p)
                                                               (all-the-icons-faicon "plus-circle" :face 'all-the-icons-lorange)
                                                             " ")
                      (all-the-icons-faicon "plus-circle" :face 'all-the-icons-lblue))
                  " ")))

(setq-default mode-line-format '(
                                 "%e"
                                 mode-line-front-space
                                 mode-line-modified
                                 mode-line-frame-identification
                                 all-the-icons-icon-for-buffer
                                 moody-mode-line-buffer-identification
                                 "   "
                                 (vc-mode moody-vc-mode)
                                 "  "
                                 tracking-mode-line-buffers
                                 mode-line-modes
                                 mode-line-end-spaces))

(use-package evil
    :demand t
    :config
    (evil-mode 1)
(define-key key-translation-map (kbd "ESC") (kbd "C-g")))

(use-package evil-nerd-commenter
:demand t
:config
(evilnc-default-hotkeys))

(use-package evil-surround
:config
(global-evil-surround-mode 1))

(use-package evil-indent-plus
:config
(evil-indent-plus-default-bindings))

(use-package evil-snipe
:after general
  :config
  (setq  evil-snipe-scope 'whole-visible)
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-lion
:config
(evil-lion-mode))

(use-package ivy
    :demand t
    :after general
    :config
    (ivy-mode 1)
    (general-define-key :keymaps 'ivy-mode-map
                                 "C-<return>" 'ivy-immediate-done)

    (use-package smex) ; needed so that commands are ordered by usage
    (global-set-key [remap execute-extended-command] #'counsel-M-x)
    (global-set-key [remap find-file] #'counsel-find-file)
)

(use-package counsel
  :demand t)

(use-package company
             :hook (after-init . global-company-mode)
             :config
             (setq company-dabbrev-downcase 0)
             (setq company-idle-delay 0.2)
             (setq company-tooltip-align-annotations t)
             (setq company-minimum-prefix-length 2))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package swiper
  :commands (swiper swiper-all))

(use-package wgrep)

(use-package magit
  :commands (magit-status magit-blame magit-log-buffer-file magit-log-all))

;; remove built in org-mode from path
(with-no-warnings (require 'cl))
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :after general
  :defer t
  :config
  (setq org-startup-indented t)
  (setq org-src-preserve-indentation t)
  (setq org-log-done t)
  (setq org-latex-caption-above nil)
  (setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (defun my-beamer-bold (contents backend info)
    (when (eq backend 'beamer)
      (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\textbf" contents)))

  (add-to-list 'org-export-filter-bold-functions 'my-beamer-bold)

  (setq tpanum/org-exporters
        '(("latex" . org-latex-export-to-pdf)
          ("beamer" . org-beamer-export-to-pdf)
          ("hugo" . (org-hugo-export-wim-to-md :all-subtrees))))

  (defun tpanum/org-get-default-exporter ()
    (downcase (car (org-element-map
                       (org-element-parse-buffer)
                       'keyword (lambda (el)
                                  (when (string-equal (org-element-property :key el) "DEFAULT_EXPORTER")
                                    (org-element-property :value el)))))))

  (defun tpanum/org-default-export ()
    "Look for the property `DEFAULT_EXPORTER' within an org file, and select exporter based on `tpanum/org-exporters'"
    (interactive)
    (let ((exporter (cdr (assoc (tpanum/org-get-default-exporter) tpanum/org-exporters))))
      (call-interactively exporter)))


  (general-define-key :keymaps 'org-mode-map
                      :states '(normal)
                      "RET" 'org-open-at-point
                      "S-<right>" 'org-shiftmetaright
                      "S-<left>" 'org-shiftmetaleft
                      "S-<up>" 'org-shiftmetaup
                      "S-<up>" 'org-shiftmetadown
                      "C-e" 'tpanum/org-default-export
                      )

  (general-define-key :keymaps 'doc-view-mode-map
                      :states '(emacs)
                      "<escape>" 'kill-buffer-and-window)

  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (add-to-list 'org-latex-classes
               '("IEEEtran"
                 "\\documentclass{IEEEtran}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (general-define-key :keymaps 'org-capture-mode-map
                      :states '(normal)
                      "q" 'org-capture-finalize))

(use-package org-ref
             :after general
             :config
             (setq
              org-ref-default-bibliography '("~/research/bibliography.bib")
              org-ref-pdf-directory "~/research/papers/"
              org-ref-bibliography-notes "~/research/papers/papers.org"
              bibtex-completion-bibliography '("~/research/bibliography.bib")
              bibtex-completion-library-path "~/research/papers"
              bibtex-completion-notes-path "~/research/papers/papers.org"
              org-ref-completion-library 'org-ref-ivy-cite)

             (setq bibtex-completion-notes-template-one-file "
* ${title} (${year}) [${author-or-editor}]
  :PROPERTIES:
  :Custom_ID: ${=key=}
  :END:

"))

(defun tpanum/org-to-pdf ()
  (interactive)
  (if (string-match "latex_class:[ ]*beamer" (buffer-string)) ; current buffer contains beamer class
      (org-beamer-export-to-pdf)
    (org-latex-export-to-pdf)))

(use-package org-re-reveal
  :after org
  :config
  (setq org-re-reveal--href-fragment-prefix org-re-reveal--slide-id-prefix)
)

(use-package worf
             :ensure t
             :after general
             :config
             (defun bjm/worf-insert-internal-link ()
               "Use ivy to insert a link to a heading in the current `org-mode' document. Code is based on `worf-goto'."
               (interactive)
               (let ((cands (worf--goto-candidates)))
                 (ivy-read "Heading: " cands
                           :action 'bjm/worf-insert-internal-link-action)))

(use-package with-simulated-input
:ensure t
:config
(defun bjm/worf-insert-internal-link-action (x)
  "Insert link for `bjm/worf-insert-internal-link'"
  ;; go to heading
  (save-excursion
    (goto-char (cdr x))
    ;; store link
    (call-interactively 'org-store-link))
  ;; return to original point and insert link
  (with-simulated-input "RET" (call-interactively 'org-insert-last-stored-link))
  ;; org-insert-last-stored-link adds a newline so delete this
  (delete-char -1))

             (general-define-key :keymaps 'org-mode-map
                                 :states '(normal)
                                 "C-c v" 'bjm/worf-insert-internal-link)))

(use-package ox-hugo
  :after ox)

(use-package company-org-block
             :load-path "lisp/pkgs"
             :hook ((org-mode . tpanum/org-mode-company-hook-function))
             :config
             (defun tpanum/org-mode-company-hook-function ()
               (setq-local company-begin-commands t)
               (setq-local company-backends '(company-org-block))
               (company-mode +1)))

(use-package toc-org
  :after org
  :init (add-hook 'org-mode-hook #'toc-org-enable))

(use-package which-key
:ensure t
:config
(setq which-key-idle-delay 0.4)
(which-key-mode))

(use-package general
             :ensure t
             :config
             (general-auto-unbind-keys)
             (general-define-key
              :states '(normal visual emacs)
              :prefix "SPC"

              "aa" 'apropos
              "b" 'ivy-switch-buffer
        "co" 'tpanum/open-config
              "f" 'find-file
              "gs" 'magit-status
              "h"  '(:ignore t :which-key "Help")
              "hf" 'describe-function
              "hk" 'describe-key
              "hm" 'describe-mode
              "hr" 'info-emacs-manual
              "hv" 'describe-variable
              "irc" 'tpanum/irc
        "j" 'avy-goto-word-or-subword-1

              "o"  '(:ignore t :which-key "Org")
              "oc" 'tpanum/org-capture-todo
              "oa" 'tpanum/org-agenda

              "p"  '(:ignore t :which-key "Programming")
              "pe" 'next-error

              "q" 'kill-this-buffer
              "r"  '(:ignore t :which-key "Research")
              "rad" 'doi-utils-add-bibtex-entry-from-doi
              "ras" 'doi-utils-add-entry-from-crossref-query
              "rc" 'org-ref-helm-insert-cite-link
              "rw" 'helm-bibtex
              "wo" 'other-window
              "wd" 'delete-other-windows
              "wsh" 'split-window-right
              "wsv" 'split-window-below
              "xx" 'execute-extended-command
              "xc" 'save-buffers-kill-terminal
              "xe" 'eval-last-sexp)

             (general-define-key
              :states '(normal)
              "/" 'swiper)
             )

(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
(define-key ivy-minibuffer-map (kbd "s-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "s-k") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "<RET>") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-s") 'tpanum/ivy-rg-search)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(defmacro minibuffer-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (minibuffer-keyboard-quit)))

(defun tpanum/ivy-rg-search ()
  (interactive)
  (minibuffer-quit-and-run
   (let ((selected-candidate (concat (file-name-as-directory ivy--directory) (ivy-state-current ivy-last))))
     (if (file-directory-p selected-candidate) (counsel-rg "" selected-candidate) (counsel-rg "" ivy--directory)))))

(use-package aggressive-indent
  :ensure t
:config
(global-aggressive-indent-mode)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)
(add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
(add-to-list 'aggressive-indent-excluded-modes 'nix-mode)
(add-to-list 'aggressive-indent-excluded-modes 'vue-mode)
(add-to-list 'aggressive-indent-excluded-modes 'json-mode)
(add-to-list 'aggressive-indent-excluded-modes 'web-mode))

(use-package yasnippet
             :diminish yas-minor-mode
             :commands (yas-global-mode)
             :defer 2
             :init
             (with-eval-after-load 'yasnippet
               (progn
                 (setq yas-snippet-dirs
                       (append yas-snippet-dirs '("~/.emacs.d/snippets")))))
             :config
             (yas-global-mode)
             (setq yas-indent-line 'none))

(use-package nlinum
             :ensure t
             :config (add-hook 'prog-mode-hook '(lambda () (nlinum-mode t)))
             (setq nlinum-highlight-current-line t))

(use-package circe
:config
(defconst irc-left-padding 8 "Padding for nicks")
(defconst irc-time-stamp-format "%H:%M")

(require 'circe-color-nicks)
(enable-circe-color-nicks)

(require 'circe-lagmon)
(circe-lagmon-mode)

(setq
 circe-reduce-lurker-spam t
 circe-default-part-message "Bye"
 circe-default-quit-message "Bye"
 circe-color-nicks-everywhere t
 circe-format-say (format "{nick:+%ss} │ {body}" irc-left-padding)
 circe-format-self-say circe-format-say
 circe-format-action (format "{nick:+%ss} * {body}" irc-left-padding)
 circe-format-self-action circe-format-action
 circe-network-defaults ()
 circe-network-options
 `(("freenode"
    :host "weechat.panum.dk"
    :port 8000
    :server-buffer-name "⇄ freenode"
    :nick "tpanum"
    :user "tpanum"
    :pass weechat-relay-freenode-pass
    :use-tls t
    )
   ("znc-bitlbee"
    :host "znc.panum.dk"
    :port 5000
    :server-buffer-name "⇄ freenode"
    :nick "tpanum"
    :user "znc/freenode"
    :pass personal-znc-pass
    ;; :lagmon-disabled t
    :tls t
    )))

(setq lui-fill-type nil)

(add-hook 'circe-channel-mode-hook #'turn-on-visual-line-mode)

(defvar irc-truncate-nick-char ?…
  "Character to displayed when nick > `irc-left-padding' in length.")

(defun irc-circe-truncate-nicks ()
  "Truncate long nicknames in chat output non-destructively."
  (when-let ((beg (text-property-any (point-min) (point-max) 'lui-format-argument 'nick)))
    (goto-char beg)
    (let ((end (next-single-property-change beg 'lui-format-argument))
          (nick (plist-get (plist-get (text-properties-at beg) 'lui-keywords)
                           :nick)))
      (when (> (length nick) irc-left-padding)
        (compose-region (+ beg irc-left-padding -1) end
                        irc-truncate-nick-char)))))

(add-hook 'lui-pre-output-hook 'irc-circe-truncate-nicks)

(defun irc-init-lui-margins ()
  "Fix margins for irc"
  (setq lui-time-stamp-position 'right-margin
        lui-time-stamp-format irc-time-stamp-format
        right-margin-width (length (format-time-string lui-time-stamp-format))))

(defun irc-init-lui-wrapping ()
  "Fix wrapping for irc"
  (interactive)
  (setq fringes-outside-margins t
        word-wrap t
        wrap-prefix (concat (make-string (+ irc-left-padding 1) ? ) "│ ")))

(add-hook 'lui-mode-hook 'irc-init-lui-wrapping)
(add-hook 'lui-mode-hook 'irc-init-lui-margins))

(use-package circe-notifications
:ensure t
:config
(setq
circe-notifications-wait-for 2
circe-notifications-JOIN nil
circe-notifications-PART nil)
(add-to-list 'circe-notifications-watch-strings "#slack-aau-ntp")
(add-to-list 'circe-notifications-watch-strings "#slack-aau-backend")
(add-to-list 'circe-notifications-watch-strings "#slack-aau-random")
(add-to-list 'circe-notifications-watch-strings "#slack-aau-research")
(add-hook 'circe-server-connected-hook 'tpanum/enable-circe-notifications))

(defun tpanum/enable-circe-notifications ()
  "Turn on notifications."
  (interactive)
  (run-at-time "5sec" nil 'enable-circe-notifications))

(defun circe-notifications-notify (nick body channel)
  (if (and (not (string-match "^\[[0-9]+:[0-9]+\]" body)) ; make sure playback messages from znc are not displayed
  (not (string-match "^\\\*\\\*\\\*$" nick))
  (not (string-match "^/\\(PART\\|JOIN\\)" body)))
      (alert
       (concat "<b>" nick "</b>: " body)
       :severity circe-notifications-alert-severity
       :title channel
       :category "chat"
       :style circe-notifications-alert-style)))

(defun weechat-relay-freenode-pass (server)
  "Return the password for the `SERVER'."
  (concat "freenode" ":" (password-store-get "personal/weechat-relay")))

(defun personal-znc-pass (server)
  "Return the password for the `SERVER'."
  (concat "freenode" ":" (password-store-get "personal/znc")))

(defun tpanum/irc ()
  "Connect to IRC"
  (interactive)
  ;; (circe "znc-freenode")
  (circe "znc-bitlbee"))

(use-package erc
:after password-store
  :custom
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 14)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 20)
  (erc-server-reconnect-timeout 5)
  (erc-prompt "   >")
(erc-notifications-icon "~/.icons/hashtag-solid.svg")
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))

  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-truncate-mode 1)
  (erc-update-modules)
  )


  (defun erc-notifications-notify-on-match (match-type nickuserhost msg)
  (when (eq match-type 'current-nick)
    (let ((nick (nth 0 (erc-parse-user nickuserhost))))
      (unless (or (string-match-p "^Server:" nick)
                  (string-match-p "^\[[0-9]+:[0-9]+\]" msg)
                  (when (boundp 'erc-track-exclude)
                    (member nick erc-track-exclude)))
        (erc-notifications-notify nick msg)))))

(use-package erc-hl-nicks
  :after erc
  :config
  (setq erc-hl-nicks-maximum-luminence 80))

(use-package erc-image
  :after erc)

(use-package password-store
:ensure t)

(use-package smartparens
  :ensure t
  :config
  (progn
  (require 'smartparens-config)
  (add-to-list 'sp-ignore-modes-list 'circe-channel-mode)
  (add-to-list 'sp-ignore-modes-list 'circe-server-mode)
  (add-to-list 'sp-ignore-modes-list 'circe-query-mode)
  (smartparens-global-mode 1)))

(use-package rainbow-mode
:config
(add-hook 'prog-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters
:hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package ispell
             :defer 15
             :after general
             :config
             (progn
               (cond
                ((executable-find "aspell")
                 (setq ispell-program-name "aspell")
                 (setq ispell-extra-args   '("--sug-mode=ultra"
                                             "--lang=en_US")))
                ((executable-find "hunspell")
                 (setq ispell-program-name "hunspell")
                 (setq ispell-extra-args   '("-d en_US"))))

               ;; Save a new word to personal dictionary without asking
               (setq ispell-silently-savep t)

               (use-package flyspell
                            :init
                            (setq flyspell-use-meta-tab nil)
                            (general-define-key
                            :states '(normal visual emacs)
                             :prefix "SPC"
                             "lc" 'cycle-languages
                             )
                            :config
                            (progn
                              (add-hook 'prog-mode-hook #'flyspell-prog-mode)
                              (with-eval-after-load 'auto-complete
                                (ac-flyspell-workaround))
                              ;; https://github.com/larstvei/dot-emacs#flyspell
                              (add-hook 'text-mode-hook #'turn-on-flyspell)
                              (add-hook 'org-mode-hook  #'turn-on-flyspell)

                              ;; https://github.com/d12frosted/flyspell-correct
                              (use-package flyspell-correct-ivy
                                           :after flyspell-correct
                                           :bind (:map modi-mode-map
                                                       ("<f12>" . flyspell-correct-word-generic)))
                              )))
             (defun cycle-languages ()
               "Changes the ispell dictionary to the first element in
ISPELL-LANGUAGES, and returns an interactive function that cycles
the languages in ISPELL-LANGUAGES when invoked."
               (interactive)
               (lexical-let ((ispell-languages '#1=("american" "dansk" . #1#)))
                 (ispell-change-dictionary (car ispell-languages))
                 (lambda ()
                   (interactive)
                   ;; Rotates the languages cycle and changes the ispell dictionary.
                   (ispell-change-dictionary
                    (car (setq ispell-languages (cdr ispell-languages)))))))
             (defadvice turn-on-flyspell (before check nil activate)
               "Turns on flyspell only if a spell-checking tool is installed."
               (when (executable-find ispell-program-name)
                 (local-set-key (kbd "C-c l") (cycle-languages)))))

(use-package pkg-info)

(use-package flycheck
             :commands global-flycheck-mode
             :init (global-flycheck-mode)
             :defer t
             :config
             (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
               (vector #b00000000
                       #b00000000
                       #b00000000
                       #b00000000
                       #b00000000
                       #b00111000
                       #b01111100
                       #b11111110
                       #b11111110
                       #b01111100
                       #b00111000
                       #b00000000
                       #b00000000
                       #b00000000
                       #b00000000
                       #b00000000
                       #b00000000))

             (flycheck-define-error-level 'error
               :severity 100
               :compilation-level 2
               :overlay-category 'flycheck-error-overlay
               :fringe-bitmap 'flycheck-fringe-bitmap-ball
               :fringe-face 'flycheck-fringe-error
               :error-list-face 'flycheck-error-list-error)
             (flycheck-define-error-level 'warning
               :severity 10
               :compilation-level 1
               :overlay-category 'flycheck-warning-overlay
               :fringe-bitmap 'flycheck-fringe-bitmap-ball
               :fringe-face 'flycheck-fringe-warning
               :error-list-face 'flycheck-error-list-warning))

(use-package ace-window
  :config
  (global-set-key (kbd "s-w") 'ace-window)
(global-set-key [remap other-window] 'ace-window))

(use-package avy
  :after (:all general evil-snipe)
  :defer
  :config
(setq avy-background t)
  (general-define-key
  :states '(normal operator motion)
  "s" 'evil-avy-goto-char-timer))

(use-package tramp
  :defer t
  :config
  (setf tramp-persistency-file-name
        (concat temporary-file-directory "tramp-" (user-login-name))))

(use-package ob
  :ensure nil
  :after org
  :functions (org-babel-do-load-languages)
  :config
  (general-define-key
   :states '(normal emacs)
   "C-n" 'org-babel-next-src-block
   "C-p" 'org-babel-previous-src-block
   "C-<return>" 'org-babel-execute-src-block)

  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   "obra" 'org-babel-execute-buffer)
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (sql . t)))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

(use-package ob-python
             :after ob
             :ensure nil
             :config
             (add-to-list 'org-babel-load-languages '(python . t) t)
             (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

             (setq org-babel-default-header-args:python
                   '((:exports  . "both")
                     (:results  . "output"))))

(use-package dired
             :ensure nil
             :config
             (add-hook 'dired-mode-hook 'auto-revert-mode))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook electric-pair
  :config
  (setq rust-format-on-save t))

(use-package nix-mode
:ensure t
:mode ("\\.nix" . nix-mode)
:config
(setq nix-indent-function 'nix-indent-line))

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode)))

(use-package go-mode
  :init
  (progn
    (if (executable-find "goimports") (setq gofmt-command "goimports"))
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save))
    :config
    (add-to-list 'exec-path "/home/tpanum/go/bin")
    :mode "\\.go\\'"
    :general (:keymaps 'go-mode-map
                       :prefix "C-c"
                       "C-c" 'recompile)
    :hook ((go-mode . (lambda () (set (make-local-variable 'compile-command) (concat "go run " buffer-file-name))))
         (go-mode . electric-pair-mode)))

(use-package company-go
:config
(add-to-list 'company-backends 'company-go))

(use-package go-eldoc)

(use-package flycheck-golangci-lint
:after flycheck
:config (setq flycheck-golangci-lint-executable "golangci-lint run --disable-all --enable typecheck ineffassign golint dupl goconst gocyclo gofmt goimports misspell lll nakedret prealloc")
:hook (go-mode . flycheck-golangci-lint-setup))

(use-package dockerfile-mode
:mode "Dockerfile\\'")

(use-package svelte-mode
  :mode ("\\.svelte$" . svelte-mode))

(use-package web-mode
             :mode (("\\.html?\\'" . web-mode)
                    ("\\.xhtml$"   . web-mode))
             :config
             (setq web-mode-enable-engine-detection t))

(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)
         (python-mode . (lambda () (set (make-local-variable 'compile-command) (modify-syntax-entry ?_ "w"))))
         (python-mode . (lambda () (set (make-local-variable 'compile-command) (concat "python " buffer-file-name)))))
  :general (:keymaps 'python-mode-map
                     :prefix "C-c"
                     "C-c" 'recompile))

(use-package eldoc
  :config
  (setq eldoc-idle-delay 1))

(use-package company-anaconda
             :after (:all company anaconda-mode)
             :hook ((anaconda-mode . tpanum/anaconda-company-hook-function))
             :config
             (add-to-list 'company-backends 'company-anaconda)
             (defun tpanum/anaconda-company-hook-function ()
             (set (make-local-variable 'company-backends)
             '((company-anaconda)))))

(use-package elm-mode
  :ensure t
  :mode ("\\.elm\\'" . elm-mode)
  :after company
  :diminish elm-format-on-save-mode
  :diminish elm-indent-mode
  :config
  (when (executable-find "elm-format")
    (setq-default elm-format-on-save t))
  (add-hook 'elm-mode-hook (lambda ()
                                                         (set (make-local-variable 'company-backends)
                                                                  '(company-elm
                                                                        company-yasnippet
                                                                        company-files))))
  (add-hook 'elm-mode-hook (lambda ()
                                                         (setq-default indent-tabs-mode nil))))

(use-package flycheck-elm
  :ensure t
  :after elm-mode flycheck
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))
  )

(use-package sqlup-mode
  :diminish sqlup-mode
  :config
  (add-hook 'sql-mode-hook 'sqlup-mode))

(use-package sql-indent
  :diminish sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(use-package sql
  :config
  (add-hook 'sql-mode-hook
            '(lambda ()
               (flycheck-select-checker 'sql-sqlint)
               )))



(use-package isortify
             :init
             (add-hook 'python-mode-hook 'isortify-mode))

(use-package python-black
             :after python
             :init
             (setq python-black-extra-args '("-l" "79"))
             (add-hook 'python-mode-hook 'python-black-on-save-mode))

(use-package flycheck-mypy
             :after flycheck
             :config
             (setq flycheck-python-mypy-args "--ignore-missing-imports")
             (add-to-list 'flycheck-disabled-checkers 'python-flake8)
             (flycheck-add-next-checker 'python-pylint 'python-mypy t))

(use-package jupyter
             :after (:all org python)
             :config
             (add-to-list 'org-babel-load-languages '(jupyter . t) t)
             (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

             (setq org-babel-default-header-args:jupyter-python
                   '((:session . "py")
                     (:async . "yes")
                     (:exports . "results")
                     (:kernel . "python3")))
             (require 'jupyter-tramp)
             (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
             (org-babel-jupyter-override-src-block "python"))

(use-package haskell-mode
             :mode (("\\.hs\\'"    . haskell-mode)
                    ("\\.cabal\\'" . haskell-cabal-mode)
                    ("\\.hcr\\'"   . haskell-core-mode)))

(use-package vue-mode
:config (setq js-indent-level 2)
:mode (("\\.vue\\'" . vue-mode)))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)))

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)))

(use-package auctex-latexmk
             :config
             (auctex-latexmk-setup))

(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :config
  (setq cdlatex-paired-parens "$[{("
        cdlatex-sub-super-scripts-outside-math-mode nil))

(use-package company-auctex
  :config
  (company-auctex-init))

(use-package latex
             :ensure auctex
             :mode ("\\.tex\\'" . LaTeX-mode)
             :init
             (setq TeX-auto-save t)
             (setq TeX-parse-self t)
             (setq-default TeX-master "main")
             (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package flycheck-vale
  :after flycheck
  :config
  (flycheck-vale-setup)
  (flycheck-add-next-checker 'vale 'proselint))

(use-package protobuf-mode
  :ensure t
:mode ("\\.proto\\'" . protobuf-mode))

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t)
(define-fringe-bitmap 'git-gutter-fr:added
  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
  nil nil 'center)
(define-fringe-bitmap 'git-gutter-fr:modified
  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
  nil nil 'center)
(define-fringe-bitmap 'git-gutter-fr:deleted
  [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
nil nil 'center))

(defun tpanum/open-config ()
(interactive)
(find-file "~/.emacs.d/emacs.org"))

(setq gc-cons-threshold 16777216
   gc-cons-percentage 0.1)
