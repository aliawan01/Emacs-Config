(set-default-coding-systems 'utf-8)
(set-face-attribute 'default nil :font "Consolas" :height 120)

(setq scroll-margin 10)
(setq visible-bell 1) 
(setq display-line-numbers-width 2)
(setq make-backup-files nil)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(setq inhibit-startup-message t) 
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 3)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(defun font-size()
  "Increasing and Decreasing Fonts size")
   (global-set-key (kbd "C-=") 'text-scale-increase)
   (global-set-key (kbd "C--") 'text-scale-decrease)

;; evil-motion-state-minor-mode
;; global-unset-key

;; Automatically completing brackets and pairs
(show-paren-mode 1)
(electric-pair-mode 1)
(setq electric-pair-pairs
      '((?\" . ?\")
        (?\{ . ?\})
        (?\' . ?\')))

;; Disabling bold fonts 
(defun remap-faces-default-attributes ()
   (let ((family (face-attribute 'default :family))
         (height (face-attribute 'default :height)))
     (mapcar (lambda (face)
              (face-remap-add-relative
               face :family family :weight 'normal :height height))
          (face-list))))

(when (display-graphic-p)
   (add-hook 'minibuffer-setup-hook 'remap-faces-default-attributes)
   (add-hook 'change-major-mode-after-body-hook 'remap-faces-default-attributes))

;; Installing use-package
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(unless (boundp 'package-pinned-packages)
  (defvar package-pinned-packages '()))

;; Installing General

(use-package general)

;; Installing Diminish

(use-package diminish)

;; Installing Evil Mode

(use-package evil
  :config
  (evil-mode 1)

  (define-key evil-normal-state-map (kbd "C-M-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-M-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-M-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-M-h") 'evil-window-left))

;; Installing Swiper

(use-package swiper
  :ensure t)

;; Setting up ivy

(use-package ivy
  :after (general evil)
  :diminish
  :init
  (general-override-mode)
  (general-define-key 
   :states 'normal

   :keymaps 'override
   "C-s" 'swiper

   :keymaps 'ivy-minibuffer-map
    "TAB"  'ivy-alt-done	
    "C-l"  'ivy-alt-done
    "C-j"  'ivy-next-line
    "C-k"  'ivy-previous-line

   :keymaps 'ivy-switch-buffer-map
    "C-k"  'ivy-previous-line
    "C-l"  'ivy-done
    "C-d"  'ivy-switch-buffer-kill

   :keymaps 'ivy-reverse-i-search-map 
    "C-k"  'ivy-previous-line
    "C-d"  'ivy-reverse-i-search-kill)

  :config
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :after general
  :init
  (general-override-mode)
  (general-define-key
   :states 'normal
   :keymaps 'override
   "M-x"  'counsel-M-x
   "C-p"  'counsel-ibuffer
   "C-f"  'counsel-find-file
   "C-M-v"  'counsel-imenu
   :keymaps 'minibuffer-local-map
    "C-r"  'counsel-minibuffer-history))

;; Making ivy look fancy

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package doom-themes
  :config
  (load-theme 'doom-solarized-light t))

(remap-faces-default-attributes)
(font-size)