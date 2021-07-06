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
  
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 125)
  (set-background-color "#ffffd8")
  (set-foreground-color "black")
)

;; Setting up indentation for C/C++ files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
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
		   (t
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
(setq package-selected-packages '(evil org modus-themes undo-tree))
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

(defun ali/org ()
  (interactive)
  (require 'org)
  (org-mode))

(require 'modus-themes)

(defun ali/evil ()
  "Setup for evil mode"
  (interactive)

  (require 'evil)

  ;; Making cursor always be a box
  (setq evil-insert-state-cursor 'box)

  ;; Using undo-tree for undo and redo
  (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
  (define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)

  ;; Balancing all the windows on the screen
  (define-key evil-normal-state-map (kbd "=") 'balance-windows)

  ;; Swapping the windows on the screen
  (define-key evil-normal-state-map (kbd "+") 'window-swap-states)

  (evil-make-overriding-map ali-keymap 'normal 'motion)
  (evil-mode 1))

;; Making undo-tree start automatically
(add-hook 'prog-mode-hook '(lambda()
                             (interactive)
                             (require 'undo-tree)
                             (undo-tree-mode 1)))

(defun undo-tree-hook ()
  "Hook to make the undo-tree-visualizer window to automatically resize to become smaller"
  (interactive)

  (unless (not (get-buffer-window " *undo-tree*"))
    (save-excursion
            (select-window (get-buffer-window " *undo-tree*"))
            (evil-window-set-width 45)
            (windmove-left 1))))

(add-hook 'undo-tree-visualizer-mode-hook 'undo-tree-hook)

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

    ;; Toggling the undo tree visualizer
    (define-key ali-keymap (kbd "C-c u") 'undo-tree-visualize)

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
