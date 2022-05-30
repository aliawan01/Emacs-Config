(defun ali/initial-setup ()
  "Basic Settings to make emacs useable"
  (interactive)
  (blink-cursor-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (good-scroll-mode 1)
  (set-language-environment "UTF-8")
  (setq tags-revert-without-query 1)
  (setq inhibit-startup-message t) 
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse 't)
  (setq scroll-step 5)
  (setq scroll-margin 3)
  (setq-default indent-tabs-mode nil)
  (setq debug-on-error t)
  (setq ring-bell-function 'ignore) 
  (setq truncate-lines nil)
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)

  ;; Setting ESC to quit 
  (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

  ;; Making the line numbers wider
  (add-hook 'display-line-numbers-mode-hook '(lambda() (setq display-line-numbers-width 3)))
     
  ;; Getting rid of autosave files and other stuff
  (setq make-lockfiles nil
        make-backup-files nil
        auto-save-default nil)

  ;; Making splits always be vertical by default
  (setq split-height-threshold nil
          split-width-threshold 0)

  ;; Setting up tabs
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil))

(defun ali/colorscheme ()
  (interactive)
  (let ((blue       "#2f0dc9")
        (brown      "#8A3324")
        (red        "#DB5B4F")
        (green      "#008e07")
        (purple     "#CA27B4")
        (selection  "#dcdec3")
        (yellow     "#ffffd7"))

       (set-face-attribute 'font-lock-comment-face nil :foreground red)		
       (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground red) 
       (set-face-attribute 'font-lock-string-face nil :foreground green)		
       (set-face-attribute 'font-lock-doc-face nil :foreground red)		
       (set-face-attribute 'font-lock-keyword-face nil :foreground blue)		
       (set-face-attribute 'font-lock-builtin-face nil :foreground blue)		
       (set-face-attribute 'font-lock-function-name-face nil :foreground "black")	
       (set-face-attribute 'font-lock-variable-name-face nil :foreground "black")	
       (set-face-attribute 'font-lock-type-face nil :foreground blue)		
       (set-face-attribute 'font-lock-constant-face nil :foreground blue)		
       (set-face-attribute 'font-lock-negation-char-face nil :foreground purple)	
       (set-face-attribute 'font-lock-preprocessor-face nil :foreground brown)	
       (set-face-attribute 'highlight-numbers-number nil :foreground green)
       (set-face-attribute 'show-paren-match nil :background selection)
       (set-face-attribute 'region nil :background selection)

       (set-background-color yellow)
       (set-face-attribute 'fringe nil :background yellow)))

(defun ali/colours ()
  (interactive)

  ;; Getting rid of the annoying curly arrows at the end of folded lines
  (setq-default fringe-indicator-alist '(continuation nil nil))

  ;; Setting the font
  (set-face-attribute 'default nil :font "Fira Code Retina" :height 120)

  ;; Colourscheme
  (ali/colorscheme)

  (make-face 'font-lock-todo-face)
  (make-face 'font-lock-note-face)

  ;; Highlighting TODO and NOTE
  (mapc (lambda (mode)
 	 (font-lock-add-keywords
 	  mode
 	  '(("\\<\\(TODO\\)" 1 'font-lock-todo-face t)
        ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
 	'(c++-mode c-mode python-mode emacs-lisp-mode))

  (modify-face 'font-lock-todo-face "Red" nil nil t nil t)
  (modify-face 'font-lock-note-face "medium sea green" nil nil t nil t)
)

;; Setting up indentation for C/C++ files
(require 'cc-mode)
(setq auto-mode-alist (append
            '(("\\.cpp$"    . c++-mode)
              ("\\.h$"    . c++-mode)
              ("\\.c$"   . c++-mode)) auto-mode-alist))

(c-add-style "ali-c-indenting" '((c-tab-always-indent . t)
                                 (c-basic-offset . 4)
                                 (c-offsets-alist (access-label . 0)
                                                  (label . +)
                                                  (arglist-intro . +))))

(setq c-default-style "ali-c-indenting")

;; Eshell Customization
(require 'eshell)

(defun ali/eshell (&optional arg)
  (interactive "P")
  (let ((buf (cond ((numberp arg)
		    (get-buffer-create (format "%s<%d>"
					       eshell-buffer-name
					       arg)))
		   (arg
		    (generate-new-buffer eshell-buffer-name))
\		   (t
		    (get-buffer-create eshell-buffer-name)))))
    (pop-to-buffer-same-window buf)
    (unless (derived-mode-p 'eshell-mode)
      (eshell-mode))
    buf))

(defun clear-eshell-buffer ()
  (interactive)
  "Clears the eshell buffer."
  (eshell/clear 1))

(defun look (text &rest colours)
  (propertize text 'face colours))

(defun ali/eshell-prompt ()
  (interactive)

    (let ((header "#8fb2ff"))
      (concat
       "╭ "
        (propertize (abbreviate-file-name (eshell/pwd)) 'face `(:foreground "green"))
        (look " • " :foreground "purple")
       (look (or (ignore-errors (format " %s" (vc-responsible-backend default-directory))) "") :foreground "magenta")
       "\n"
       "│"
       "\n"
       "╰ "
    
       (look "aliawan 🔥" :foreground "blue")
       (look " " :foreground "black")
       )))

(setq eshell-highlight-prompt nil
      eshell-prompt-function 'ali/eshell-prompt)

(add-hook 'eshell-mode-hook '(lambda()
                               (interactive)
                               (defalias 'clear 'clear-eshell-buffer)
                               (define-key eshell-mode-map (kbd "C-p") 'counsel-switch-buffer)
                               (define-key eshell-mode-map (kbd "C-6") 'evil-switch-to-windows-last-buffer)
                               (define-key eshell-mode-map (kbd "C-;") 'clear-eshell-buffer)
                               (define-key eshell-mode-map (kbd "C-f") 'counsel-find-file)
                               ))

(defun ali/brackets () 
  "Automatically completing brackets and pairs"
  (interactive)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (setq electric-pair-pairs
        '((?\" . ?\")
        (?\[ . ?\])
          (?\` . ?\`)
          (?\{ . ?\}))))

;; Setting up packages
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(require 'package)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Making sure that all the packages are installed
(setq package-selected-packages '(use-package))
(package-install-selected-packages)

(require 'use-package)

(use-package evil
  :ensure t)
(use-package evil-leader
  :ensure t)
(use-package ivy
  :ensure t)
(use-package counsel 
  :ensure t)
(use-package ivy-posframe
  :ensure t)
(use-package highlight-numbers
  :ensure t)
(use-package undo-tree
  :ensure t)
(use-package good-scroll
 :ensure t)
(use-package projectile
 :ensure t)
(use-package counsel-projectile
  :ensure t)
(use-package dumb-jump
  :ensure t)

;; Setting up dumb jump
(defun ali/dumb-jump ()
  (interactive)

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-quiet t)
  (setq dumb-jump-selector 'ivy))

(defun ali/ivy ()
  (interactive)

  ;; Setting up my keybindings for ivy
  (let ((keymaps (list ivy-switch-buffer-map ivy-minibuffer-map)))
    (dolist (keymap keymaps)
      (define-key keymap (kbd "C-j") 'ivy-next-line)
      (define-key keymap (kbd "C-k") 'ivy-previous-line)))

  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1)

  (setq ivy-posframe-parameters
      '((left-fringe . 15)
        (right-fringe . 15)))
  (ivy-posframe-mode 1))


(defun ali/compile-c-project ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "build.bat")))

(defun ali/run-c-project ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "run.bat")))

(setq projectile-completion-system 'ivy)
(projectile-mode 1)

;; Settings for increasing and decreasing size of the image when in the `image-mode'
(define-key image-map (kbd "C-=") 'image-increase-size)
(define-key image-map (kbd "C--") 'image-decrease-size)

(defun indent-at-point ()
  (interactive)
  (insert "    "))

(defun counsel-etags-async-shell-command (command)
  "Execute string COMMAND and create tags file asynchronously."
  (let* ((proc (start-process "Shell" nil shell-file-name shell-command-switch command)))
    (set-process-sentinel
     proc
     `(lambda (process signal)
        (let* ((status (process-status process)))
          (when (memq status '(exit signal))
            (cond
             ((string= (substring signal 0 -1) "finished")
              (let* ((cmd (car (cdr (cdr (process-command process))))))
                (message "Tags file was created.")))
             (t
              (message "Failed to create tags file.\nerror=%s\ncommand=%s"
                       signal
                       ,command)))))))))


;; This will update the tags file in the background
(defun update-ctags ()
  (interactive)
  (if (not (eq (projectile-project-root) nil))
      (let* ((project-dir (string-replace "/" "\\" (projectile-project-root)))
             (update-cmd (message "pushd %s&ctags -e -R&popd" project-dir)))
      (counsel-etags-async-shell-command update-cmd))))

(add-hook 'after-save-hook 'update-ctags)

(defun ali/evil-leader ()
  (interactive)

  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "f" 'projectile-find-file
    "p" 'projectile-switch-project
    "s" 'counsel-rg)

  ;; Compile commands for c/c++ projects
  (evil-leader/set-key-for-mode 'c++-mode "c" 'ali/compile-c-project)
  (evil-leader/set-key-for-mode 'c++-mode "t" 'ali/run-c-project)

  (global-evil-leader-mode))


(defun ali/evil ()
  (interactive)
  (require 'evil)

  ;; Making cursor always be a box
  (setq evil-normal-state-cursor 'box)
  (setq evil-insert-state-cursor 'box)

  (define-key evil-normal-state-map (kbd "C-b c") 'projects)

  ;; Making backspace launch M-x
  (define-key evil-normal-state-map (kbd "M-x") 'counsel-M-x)

  ;; Balancing all the windows on the screen
  (define-key evil-normal-state-map (kbd "=") 'balance-windows)

  ;; Setting C-d to kill the a buffer
  (define-key evil-normal-state-map (kbd "C-d") 'kill-buffer)

  ;; To jump to definition
  (define-key evil-normal-state-map (kbd "gd") 'xref-find-definitions)

  ;; Swapping the windows on the screen
  (define-key evil-normal-state-map (kbd "+") 'window-swap-states)

  ;; Rebinding : to ; in normal mode
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)

  ;; Making TAB autocomplete
  (define-key evil-insert-state-map [tab] 'indent-at-point)

  ;; Commands to navigate through windows
  (let ((keymaps (list evil-normal-state-map evil-insert-state-map)))

    (dolist (keymap keymaps)
      (define-key keymap (kbd "M-v") 'evil-window-vsplit)
      (define-key keymap (kbd "M-h") 'evil-window-split)

      (define-key keymap (kbd "C-j") 'evil-window-down)
      (define-key keymap (kbd "C-k") 'evil-window-up)))

  (evil-make-overriding-map ali-keymap 'normal 'motion)
  (evil-mode 1))

(defun ali/undo-tree ()
  (interactive)
  ;; Settings for undo-tree
  (with-eval-after-load 'undo-tree (defun undo-tree-overridden-undo-bindings-p () nil))
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil)
  (evil-set-undo-system 'undo-tree)
)

(defun ali/compile-python ()
    (interactive)
    (compile (concat "python " (buffer-name)))

    ;; Automatically resizes the compilation window so that it is smaller
    (unless (not (get-buffer-window "*compilation*"))
      (save-selected-window
        (select-window (get-buffer-window "*compilation*")))))

;; Making sure that tabs are used in makefiles
(add-hook 'makefile-mode-hook '(lambda () (interactive) (setq indent-tabs-mode t)))

(let ((ali-keymap (make-sparse-keymap)))
    ;; Keybindings for moving between horizontal splits
    (define-key ali-keymap (kbd "C-h") 'evil-window-left)
    (define-key ali-keymap (kbd "C-l") 'evil-window-right)

    ;; Keybindings for switching buffers and files
    (define-key ali-keymap (kbd "C-p") 'counsel-switch-buffer)
    (define-key ali-keymap (kbd "C-f") 'counsel-find-file)

    ;; Making windows bigger and smaller
    (define-key ali-keymap (kbd "M-=") 'enlarge-window-horizontally)
    (define-key ali-keymap (kbd "M--") 'shrink-window-horizontally)

    ;; Making Fonts Bigger and Smaller
    (global-set-key (kbd "C-=") 'text-scale-increase)
    (global-set-key (kbd "C--") 'text-scale-decrease)

    ;; Comments
    (define-key ali-keymap (kbd "C-/") 'comment-line)

    ;; Making Line numbers toggable
    (define-key ali-keymap (kbd "C-S-i") 'display-line-numbers-mode)

    ;; Setting C-s-h for help
    (define-key ali-keymap (kbd "C-S-h") 'help)

    ;; Testing for running python files (probably going to add this to a hook so that it only works in python-mode)
    (define-key ali-keymap (kbd "M-c") 'ali/compile-python)

    ;; Making a directory without using dired
    (define-key ali-keymap (kbd "M-p") 'ido-make-directory)

    (defvar ali-keymap ali-keymap "These are my keybindings"))

(define-minor-mode ali-keybindings-mode
  nil
  :global t
  :lighter " keys"
  :keymap ali-keymap)

(ali/ivy)
(ali-keybindings-mode 1)

(ali/initial-setup)
(ali/brackets)
(ali/evil-leader)
(ali/evil)
(ali/colours)
(ali/undo-tree)
(ali/dumb-jump)

(define-minor-mode minor-mode-blackout-mode
  "Hides minor modes from the mode line."
  t)

(catch 'done
  (mapc (lambda (x)
          (when (and (consp x)
                     (equal (cadr x) '("" minor-mode-alist)))
            (let ((original (copy-sequence x)))
              (setcar x 'minor-mode-blackout-mode)
              (setcdr x (list "" original)))
            (throw 'done t)))
        mode-line-modes))

(minor-mode-blackout-mode 1)
