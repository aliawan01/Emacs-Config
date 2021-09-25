(defvar ido-cur-list nil)
(defun ali/initial-setup ()
    "Basic Settings to make emacs useable"
    (interactive)
    (setq inhibit-startup-message t) 
    (blink-cursor-mode -1)
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
    (setq mouse-wheel-progressive-speed nil)
    (setq mouse-wheel-follow-mouse 't)
    (setq scroll-step 3)
    (setq scroll-margin 3)
    (setq-default indent-tabs-mode nil)
    (setq debug-on-error t)
    (setq ring-bell-function 'ignore) 
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

(defun ali/colours ()
  (interactive)

  ;; Getting rid of the annoying curly arrows at the end of folded lines and making the colour of the fringes the same colour as the background
  (setq-default fringe-indicator-alist '(continuation nil nil))
  (set-face-attribute 'fringe nil :background nil)

  (set-face-attribute 'default nil :font "Liberation Mono" :height 120)
  ;; (set-background-color "#ffffd8")
  ;; (set-foreground-color "black")
  (set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
  (set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
  (set-face-attribute 'font-lock-constant-face nil :foreground "burlywood3")
  (set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
  (set-face-attribute 'font-lock-function-name-face nil :foreground "#c76f4a")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
  (set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
  (set-face-attribute 'font-lock-type-face nil :foreground "DarkGoldenrod3")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")
  (set-foreground-color "burlywood3")
  (set-background-color "#161616")
  (set-cursor-color "#40FF40")

  (make-face 'font-lock-todo-face)
  (make-face 'font-lock-note-face)

  (mapc (lambda (mode)
 	 (font-lock-add-keywords
 	  mode
 	  '(("\\<\\(TODO\\)" 1 'font-lock-todo-face t)
        ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
 	'(c++-mode c-mode python-mode emacs-lisp-mode))

  (modify-face 'font-lock-todo-face "Red" nil nil t)
  (modify-face 'font-lock-note-face "chartreuse" nil nil t)

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
                               (define-key eshell-mode-map (kbd "C-p") 'ido-switch-buffer)
                               (define-key eshell-mode-map (kbd "C-6") 'evil-switch-to-windows-last-buffer)
                               (define-key eshell-mode-map (kbd "C-;") 'clear-eshell-buffer)
                               (define-key eshell-mode-map (kbd "C-f") 'ido-find-file)
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
(setq package-selected-packages '(evil highlight-numbers undo-tree))
(package-install-selected-packages)

(defun ali/ido () 
  (interactive)
  (require 'ido)
  (setq ido-create-new-buffer 'always)

  ;; Making ido-mode work in M-x
  (global-set-key (kbd "M-x") (lambda() 
  (interactive)
  (let ((enable-recursive-minibuffers t))
      (call-interactively
      (intern
      (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp)))))))

  (ido-everywhere t)
  (ido-mode 1))

(defun ido-keybindings ()
  "Ido keybindings"
  (interactive)

  (set-transient-map ido-completion-map t)
  (let ((enable-recursive-minibuffers t)
        (minibuffer-depth-indicate-mode 10))
        (define-key ido-completion-map (kbd "C-l") 'ido-next-match)
        (define-key ido-completion-map (kbd "M-d") 'ido-enter-dired)
        (define-key ido-completion-map (kbd "M-f") 'ido-find-file-other-window)
        (define-key ido-completion-map (kbd "C-h") 'ido-prev-match)))

(add-hook 'ido-setup-hook 'ido-keybindings)


;; Settings for increasing and decreasing size of the image when in the `image-mode'
(define-key image-map (kbd "C-=") 'image-increase-size)
(define-key image-map (kbd "C--") 'image-decrease-size)
(defun indent-current-line ()
  (interactive)
    (progn 
        (setq regionStart (line-beginning-position))
        (setq regionEnd (line-end-position))

        (when (use-region-p)
            (setq regionStart (region-beginning))
            (setq regionEnd (region-end))
        )
        (save-excursion
            (goto-char regionStart)
            (setq start (line-beginning-position))
            (goto-char regionEnd)
            (setq end (line-end-position))
            (indent-rigidly start end 4)
            (setq deactivate-mark nil)
            ))
)


(defun ali/evil ()
  "Setup for evil mode"
  (interactive)

  (require 'evil)

  ;; Making cursor always be a box
  (setq evil-insert-state-cursor 'box)

  (define-key evil-normal-state-map (kbd "C-b c") 'projects)

  ;; Balancing all the windows on the screen
  (define-key evil-normal-state-map (kbd "=") 'balance-windows)

  ;; Swapping the windows on the screen
  (define-key evil-normal-state-map (kbd "+") 'window-swap-states)

  ;; Rebinding : to ; in normal mode
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)

  ;; Commands for building and running codes
  (define-key evil-normal-state-map (kbd "C-e") 'build-compile)
  (define-key evil-normal-state-map (kbd "C-q") 'run-compile)

  ;; Making TAB autocomplete
  (define-key evil-insert-state-map [tab] 'evil-complete-next)

  ;; Making Shift-Tab indent current line
  (define-key evil-insert-state-map [backtab] 'indent-current-line)

  (evil-make-overriding-map ali-keymap 'normal 'motion)
  (evil-mode 1))


(defun ali/undo-tree ()
  (interactive)
  ;; Settings for undo-tree
  (with-eval-after-load 'undo-tree (defun undo-tree-overridden-undo-bindings-p () nil))
  (global-undo-tree-mode 1)
  (evil-set-undo-system 'undo-tree)
)


(defun ali/compile-python ()
    (interactive)
    (compile (concat "python " (buffer-name)))

    ;; Automatically resizes the compilation window so that it is smaller
    (unless (not (get-buffer-window "*compilation*"))
      (save-selected-window
        (select-window (get-buffer-window "*compilation*"))
        (evil-window-set-width 65))))

(defun ali/find-missing-closing-paranthesis ()
  (interactive)
  (backward-up-list -1))

(defun ali/find-missing-open-paranthesis ()
  (interactive)
  (backward-up-list))

(defun projects ()
  (interactive)
    (switch-to-buffer (find-file-noselect "C:\\Dev\\LearningC++\\main.cpp"))
)

(defun build-compile ()
  (interactive)
  (switch-to-buffer-other-window "*compilation*")
  (compile "build.bat")
  (other-window 1))

(defun run-compile ()
  (interactive)
  (compile "run.bat"))

;; Making sure that tabs are used in makefiles
(add-hook 'makefile-mode-hook '(lambda () (interactive) (setq indent-tabs-mode t)))

(let ((ali-keymap (make-sparse-keymap)))
    "Evil keybindings for navigating through windows"
    (define-key ali-keymap (kbd "C-h") 'evil-window-left)
    (define-key ali-keymap (kbd "C-j") 'evil-window-down)
    (define-key ali-keymap (kbd "C-k") 'evil-window-up)
    (define-key ali-keymap (kbd "C-l") 'evil-window-right)

    "M-d to open dired in the current directory"
    (global-set-key (kbd "M-d") '(lambda() (interactive) (execute-kbd-macro (read-kbd-macro "C-x d RET"))))

    "Keybindings for ido-mode"
    (define-key ali-keymap (kbd "C-p") 'ido-switch-buffer)
    (define-key ali-keymap (kbd "C-f") 'ido-find-file)

    "Making windows bigger and smaller"
    (define-key ali-keymap (kbd "M-=") 'enlarge-window-horizontally)
    (define-key ali-keymap (kbd "M--") 'shrink-window-horizontally)

    "Making Fonts Bigger and Smaller"
    (global-set-key (kbd "C-=") 'text-scale-increase)
    (global-set-key (kbd "C--") 'text-scale-decrease)

    "Comments"
    (define-key ali-keymap (kbd "C-/") 'comment-line)

    "Making Line numbers toggable"
    (define-key ali-keymap (kbd "C-S-i") 'display-line-numbers-mode)

    "Setting M-v to make a vertical split"
    (define-key ali-keymap (kbd "M-v") 'evil-window-vsplit)

    "Setting M-h to make a horizontal split"
    (define-key ali-keymap (kbd "M-h") 'evil-window-split)

    "Setting C-s-h for help"
    (define-key ali-keymap (kbd "C-S-h") 'help)

    "Shortcut to get my init.el"
    (define-key ali-keymap (kbd "M-E") '(lambda() (interactive) (find-file "~/.emacs.d/init.el")))

    "Setting C-d to kill the a buffer"
    (define-key ali-keymap (kbd "C-d") 'kill-buffer)

    "Testing for running python files (probably going to add this to a hook so that it only works in python-mode"
    (define-key ali-keymap (kbd "M-c") 'ali/compile-python)

    "C-x C-b now launches ibuffer"
    (define-key ali-keymap (kbd "C-x C-b") 'ibuffer)

    "Making a directory without using dired"
    (define-key ali-keymap (kbd "M-p") 'ido-make-directory)

    ;; Opening eshell
    (define-key ali-keymap (kbd "C-'") 'ali/eshell)

    ;; To find missing closing and opening paranthesis
    (define-key ali-keymap (kbd "C-q o") 'ali/find-missing-open-paranthesis)
    (define-key ali-keymap (kbd "C-q c") 'ali/find-missing-closing-paranthesis)

    (defvar ali-keymap ali-keymap "These are my keybindings"))


(define-minor-mode ali-keybindings-mode
  nil
  :global t
  :lighter " keys"
  :keymap ali-keymap)


(ali-keybindings-mode 1)

(ali/initial-setup)
(ali/brackets)
(ali/evil)
(ali/ido)
(ali/colours)
(ali/undo-tree)
