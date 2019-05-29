;; 初めてinit.elを読み込む前にやるべきこと
;; "git" nil nil nil "clone" "https://github.com/iRi-E/mozc-el-extensions" ".emacs.d/mozc-el-extensions"

(setq default-directory "~/")
(setq command-line-default-directory "~/")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)
(package-refresh-contents)

(defvar request-packages
  '(use-package helm tabbar mozc mozc-im mozc-popup ace-isearch company dired-toggle hlinum avy
	 migemo helm-swoop helm-company image+ image-dired+ irony markdown-mode helm-xref
	 multi-compile px rainbow-mode rainbow-delimiters restart-emacs shell-pop magit
	 undo-tree visual-regexp visual-regexp-steroids window-numbering yasnippet yasnippet-snippets helm-c-yasnippet))
(dolist (pac request-packages)
  (unless (package-installed-p pac)
    (package-install pac)))

(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))

(use-package magit
  :config
  (global-set-key (kbd "\C-x m") (make-sparse-keymap))
  (global-set-key (kbd "\C-x m s") 'magit-status)
  (global-set-key (kbd "\C-x m +") 'magit-stage-file)
  (global-set-key (kbd "\C-x m c") 'magit-commit))

;;;キーバインド
;;C-c C-cでcomment-region, C-c C-;でuncomment-region
(global-set-key (kbd "\C-c C-c") 'comment-region)
(add-hook 'python-mode-hook (lambda () (define-key python-mode-map (kbd "\C-c C-c") 'comment-region)))
(add-hook 'latex-mode-hook (lambda () (define-key latex-mode-map (kbd "\C-c C-c") 'comment-region)))
(global-set-key (kbd "\C-c C-;") 'uncomment-region)

(keyboard-translate ?\C-h ?\C-?) ;backspace
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key "\M-[" 'forward-sexp) ;対応する開き括弧へ移動
(global-set-key "\M-]" 'backward-sexp) ;対応する閉じ括弧へ移動
(global-set-key "\C-j" 'newline-and-indent)

;;tab挿入
(global-set-key "\C-t" '(lambda ()
			  (interactive)
			  (insert "\t")))
;;文字の大きさ
(global-set-key [(control ?\+)] '(lambda ()
				   (interactive)
				   (text-scale-increase 1)))
(global-set-key [(control ?\-)] '(lambda ()
				   (interactive)
				   (text-scale-decrease 1)))

(define-key global-map "\C-q" (make-sparse-keymap))
;; quoted-insert は C-q C-q へ割り当て
(global-set-key "\C-q\C-q" 'quoted-insert)

;;;マウス無効化
(global-unset-key [mouse-1])
(global-unset-key [down-mouse-1])
(global-unset-key [double-mouse-1])
(global-unset-key [double-drag-Mouse-1])
(global-unset-key [triple-mouse-1])
(global-unset-key [triple-drag-mouse-1])
(global-unset-key [\S-down-mouse-1])
(global-unset-key [\C-down-mouse-1])
(global-unset-key [\M-mouse-1])
(global-unset-key [\M-down-mouse-1])
(global-unset-key [\M-drag-mouse-1])
(global-unset-key [mouse-2])
(global-unset-key [mouse-3])
(global-unset-key [\S-mouse-3])
(global-unset-key [\S-down-mouse-3])
(global-unset-key [\C-down-mouse-3])
(global-unset-key [\M-mouse-3])

;;;http://stackoverflow.com/questions/1771102/changing-emacs-forward-word-behaviour
(defun my-syntax-class (char)
  "Return ?s, ?w or ?p depending or whether CHAR is a white-space, word or punctuation character."
  (pcase (char-syntax char)
      (`?\s ?s)
      (`?w ?w)
      (`?_ ?w)
      (_ ?p)))

(defun my-forward-word (&optional arg)
  "Move point forward a word (simulate behavior of Far Manager's editor).
With prefix argument ARG, do it ARG times if positive, or move backwards ARG times if negative."
  (interactive "^p")
  (or arg (setq arg 1))
  (let* ((backward (< arg 0))
         (count (abs arg))
         (char-next
          (if backward 'char-before 'char-after))
         (skip-syntax
          (if backward 'skip-syntax-backward 'skip-syntax-forward))
         (skip-char
          (if backward 'backward-char 'forward-char))
         prev-char next-char)
    (while (> count 0)
      (setq next-char (funcall char-next))
      (cl-loop
       (if (or                          ; skip one char at a time for whitespace,
            (eql next-char ?\n)         ; in order to stop on newlines
            (eql (char-syntax next-char) ?\s))
           (funcall skip-char)
         (funcall skip-syntax (char-to-string (char-syntax next-char))))
       (setq prev-char next-char)
       (setq next-char (funcall char-next))
       ;; (message (format "Prev: %c %c %c Next: %c %c %c"
       ;;                   prev-char (char-syntax prev-char) (my-syntax-class prev-char)
       ;;                   next-char (char-syntax next-char) (my-syntax-class next-char)))
       (when
           (or
            (eql prev-char ?\n)         ; stop on newlines
            (eql next-char ?\n)
            (and                        ; stop on word -> punctuation
             (eql (my-syntax-class prev-char) ?w)
             (eql (my-syntax-class next-char) ?p))
            (and                        ; stop on word -> whitespace
             this-command-keys-shift-translated ; when selecting
             (eql (my-syntax-class prev-char) ?w)
             (eql (my-syntax-class next-char) ?s))
            (and                        ; stop on whitespace -> non-whitespace
             (not backward)             ; when going forward
             (not this-command-keys-shift-translated) ; and not selecting
             (eql (my-syntax-class prev-char) ?s)
             (not (eql (my-syntax-class next-char) ?s)))
            (and                        ; stop on non-whitespace -> whitespace
             backward                   ; when going backward
             (not this-command-keys-shift-translated) ; and not selecting
             (not (eql (my-syntax-class prev-char) ?s))
             (eql (my-syntax-class next-char) ?s))
            )
         (cl-return))
       )
      (setq count (1- count)))))

(defun my-backward-word (&optional arg)
  (interactive "^p")
  (or arg (setq arg 1))
  (my-forward-word (- arg)))

(defun my-delete-word (&optional arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (my-forward-word arg) (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (my-delete-word (- arg)))

(global-set-key "\M-f" 'my-forward-word)
(global-set-key "\M-b" 'my-backward-word)
(global-set-key (kbd "M-h") 'my-backward-delete-word)

(toggle-scroll-bar -1) ;横スクロールバーを表示しない
(setq ring-bell-function 'ignore) ;ビープ音を無効
(setq backup-inhibited t) ;バックアップファイルを作らない
(setq inhibit-startup-message t) ;セットアップメッセージを非表示
(fset 'yes-or-no-p 'y-or-n-p) ;yes/no の代わりに y/n
(setq initial-scratch-message nil) ;scratchにメッセージを表示しない
(setq select-enable-clipboard t) ;Yankバッファとクリップボードの同期
(tool-bar-mode -1) ;ツールバーを非表示
(menu-bar-mode -1) ;メニューバー非表示
(put 'upcase-region 'disabled nil) ;upcase時にアラートを出さない
(put 'downcase-region 'disabled nil) ;downcase時にアラートを出さない
(auto-image-file-mode t) ;画像ファイルを表示可能に
(ffap-bindings) ;C-x C-fをめちゃめちゃ便利に
(setq frame-title-format (format "emacs")) ;title
(setq-default indicate-buffer-boundaries 'right) ;右にEOFの印を表示

(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode)) ;.texをlatex-modeで開く

;;フォントの設定
(set-face-attribute 'default nil
		    :family "Ricty Diminished" ;; font
		    :height 140)
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty Diminished"))

;;#!が含まれるファイルに、実行許可を与える
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;;;cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil) ;cua-modeの変なキーバインド無効

(use-package tabbar
  :config
  (setq tabbar-buffer-groups-function nil)
  (setq tabbar-separator '(0.2))

  (global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
  (if (eq system-type 'windows-nt)
      (global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)
    (global-set-key (kbd "<C-iso-lefttab>") 'tabbar-backward-tab))

  (set-face-attribute
   'tabbar-default nil
   :background "black")

  (set-face-attribute
   'tabbar-unselected nil
   :background "gray30"
   :foreground "white"
   :box nil)

  (set-face-attribute
   'tabbar-selected nil
   :background "white"
   :foreground "black")

  (set-face-attribute
   'tabbar-separator nil
   :height 0.5
   :background "black")

  (set-face-attribute
   'tabbar-selected-modified nil
   :background "whilte"
   :foreground "red"
   :box nil)

  (set-face-attribute
   'tabbar-modified nil
   :background "gray30"
   :foreground "pink"
   :box nil)

  (dolist (btn '(tabbar-buffer-home-button
		 tabbar-scroll-left-button
		 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
		   (cons "" nil))))

  (defvar my-tabbar-displayed-buffers
    '("*scratch*" "*eshell*"))

  (defun my-tabbar-buffer-list ()
    (let* ((hides (list ?\  ?\*))
	   (re (regexp-opt my-tabbar-displayed-buffers))
	   (cur-buf (current-buffer))
	   (tabs (delq nil
		       (mapcar (lambda (buf)
				 (let ((name (buffer-name buf)))
				   (when (or (string-match re name)
					     (not (memq (aref name 0) hides)))
				     buf)))
			       (buffer-list)))))
      (if (memq cur-buf tabs)
	  tabs
	(cons cur-buf tabs))))

  (require 'cl)
  (defun tabbar-move-current-tab-to (move-func)
    "Move current tab to orientation that indicated by move-func."
    "if move-func is \"1+\", move to next. if move-func is \"1-\", move to privious"
    (let* ((bufset (tabbar-current-tabset t))
	   (bufs (tabbar-tabs bufset))
	   (current-buf-num 0)
	   (temp-buf-num 0)
	   (num-of-tab (safe-length bufs)))
      ;; 現在のバッファと一致するものを探して先頭へ
      (dolist (buf bufs)
	(if (string= (buffer-name) (format "%s" (car buf)))
	    (setq current-buf-num temp-buf-num)
	  (setq temp-buf-num (1+ temp-buf-num))))
      (rotatef (nth current-buf-num bufs) (nth (% (+ (funcall move-func current-buf-num) num-of-tab) num-of-tab) bufs))
      ;; タブバー書き換え
      (tabbar-set-template bufset nil)
      (tabbar-display-update)))

  (defun tabbar-move-current-tab-to-next ()
    (interactive)
    (tabbar-move-current-tab-to #'1+))

  (defun tabbar-move-current-tab-to-previous ()
    (interactive)
    (tabbar-move-current-tab-to #'1-))

  (global-set-key [(control \>)] 'tabbar-move-current-tab-to-next)
  (global-set-key [(control \<)] 'tabbar-move-current-tab-to-previous)

  (setq tabbar-buffer-list-function 'my-tabbar-buffer-list)
  (tabbar-mode))

;;対応する括弧を強調
(show-paren-mode t)
(setq show-paren-delay 0) ;ハイライトまでの遅延
(setq show-paren-style 'expression) ;括弧内を強調
;;対応する括弧までアンダーライン
(set-face-attribute 'show-paren-match-face nil
		    :background nil
		    :foreground nil
		    :underline "red"
		    :weight 'extra-bold)

(use-package rainbow-mode
  :config
  (add-hook 'lisp-interaction-mode-hook 'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

(if window-system
	(progn
	  (put 'modeline 'face-alias 'mode-line)
	  (set-face-foreground 'modeline "white") ;モードラインの文字の色
	  (set-face-background 'modeline "black") ;モードラインの文字の色
	  (set-face-foreground 'region "DeepPink1") ;リージョンの文字の色
	  (set-face-background 'region "LightSkyBlue") ;リージョンの文字の色
	  ;(set-frame-parameter nil 'fullscreen 'maximized) ;起動時最大化

	  ;;現在行をハイライト
	  (global-hl-line-mode t)
	  (set-face-background 'hl-line "gray88")

	  ;;ウィンドウサイズ
	  (setq initial-frame-alist
		(append (list
			 '(top . 100)
			 '(left . 160)
			 '(width . 88)
			 '(height . 42))
			initial-frame-alist))

	  ;;アルファ値
	  (setq default-frame-alist
			(append
			 (list
			  '(alpha . 95))
			 default-frame-alist))))

;;モードラインの時間表示
(setq display-time-string-forms
	  '(month "/" day "(" dayname ") " 24-hours ":" minutes))
(display-time)

;;モードライン
(setq total-lines 0)
(use-package total-lines
  :config
  (global-total-lines-mode t)
  )

(setq-default mode-line-format
	      '("w"
		mode-line-frame-identification
		mode-line-buffer-identification
		" ["
		mode-name
		mode-line-process
		" | "
		mode-line-mule-info
		mode-line-modified
		"] "
		"%[(L:%l C:%c B:"
		(-3 . "%p")
		" T:"
		(:eval (format "%d" (- total-lines 1)))
		" S:"
		(:eval (format "%d" (if (boundp 'text-scale-mode-amount) text-scale-mode-amount 0)))
		")%] "
		(which-func-mode ("" which-func-format "-"))
		global-mode-string))

(use-package window-numbering
	     :config
	     (global-set-key "\C-q0" 'select-window-0)
	     (global-set-key "\C-q1" 'select-window-1)
	     (global-set-key "\C-q2" 'select-window-2)
	     (global-set-key "\C-q3" 'select-window-3)
	     (global-set-key "\C-q4" 'select-window-4)
	     (global-set-key "\C-q5" 'select-window-5)
	     (global-set-key "\C-q6" 'select-window-6)
	     (global-set-key "\C-q7" 'select-window-7)
	     (global-set-key "\C-q8" 'select-window-8)
	     (global-set-key "\C-q9" 'select-window-9)
	     (window-numbering-mode))

(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
		(sdx (nth 0 (window-edges)))
		(sdy (nth 1 (window-edges)))
		(edx (nth 2 (window-edges)))
		(edy (nth 3 (window-edges)))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d] frame[%dx%d] edge[(%d %d) (%d %d)]"
                 (window-width) (window-height)
				 (frame-width) (frame-height)
				 sdx sdy edx edy)
        (setq c (read-char))
        (cond ((= c ?f)
               (enlarge-window-horizontally dx))
			  ((= c ?F)
			   (enlarge-window-horizontally (/ (frame-width) 4)))
              ((= c ?b)
               (shrink-window-horizontally dx))
              ((= c ?B)
               (shrink-window-horizontally (/ (frame-width) 4)))
              ((= c ?n)
               (enlarge-window dy))
              ((= c ?N)
               (enlarge-window (/ (frame-height) 4)))
              ((= c ?p)
               (shrink-window dy))
              ((= c ?P)
               (shrink-window (/ (frame-height) 4)))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))

(global-set-key "\C-q\C-r" 'window-resizer)
(global-set-key "\C-q\C-f" 'windmove-right)
(global-set-key "\C-q\C-b" 'windmove-left)
(global-set-key "\C-q\C-n" 'windmove-down)
(global-set-key "\C-q\C-p" 'windmove-up)

(global-set-key (kbd "C-q k") 'delete-other-windows)
(global-set-key (kbd "C-q K") 'delete-window)

(global-set-key (kbd "C-q w") 'split-window-below)
(global-set-key (kbd "C-q h") 'split-window-right)

;動的に透過率変更
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nalpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(95))))

;;rainbow delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'lisp-interaction-mode-hook '(lambda () (rainbow-delimiters-mode t)))
  (add-hook 'emacs-lisp-mode-hook '(lambda () (rainbow-delimiters-mode t)))

  (custom-set-faces
   '(completions-common-part ((t (:inherit default :foreground "red"))))
   '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))))
   '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))))

   '(rainbow-delimiters-depth-1-face ((t (:foreground "blue"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "dark orange"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "DeepPink3"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "DeepSkyBlue"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "DarkMagenta"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "LimeGreen"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "yellow3"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "tomato"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "forest green"))))))

;;行数の表示
(use-package linum
  :config
  (global-linum-mode 1)
  (setq linum-format " %5d ") ;フォーマット。５桁分の領域を確保
  (custom-set-faces '(linum ((t
			      ( :inherit (shadow default)
					 :foreground "black"
					 :background "white"
					 :height 130))))))

(use-package hlinum
  :config
  (hlinum-activate)
  (custom-set-faces '(linum-highlight-face ((t
					     (:foreground "black"
							  :background "yellow"
							  :underline nil
							  :height 130))))))

(use-package whitespace
   :config
  (setq whitespace-style '(face           ; faceで可視化
  			   trailing       ; 行末
  			   tabs           ; タブ
  			   spaces         ; スペース
  			   empty          ; 先頭/末尾の空行
  			   space-mark     ; 表示のマッピング
  			   tab-mark))

  (setq whitespace-display-mappings
  	'((space-mark ?\u3000 [?\u25a1])
  	  (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

  ;; スペースは全角のみを可視化
  (setq whitespace-space-regexp "\\(\u3000+\\)")

  ;; 保存前に自動でクリーンアップ
  (setq whitespace-action '(auto-cleanup))
  (add-hook 'markdown-mode-hook
  	    '(lambda ()
  	       (set (make-local-variable 'whitespace-action) nil)))

  (set-face-attribute 'whitespace-trailing nil
  		      :background nil
  		      :foreground "DeepPink"
  		      :underline t)

  (set-face-attribute 'whitespace-tab nil
  		      :background nil
  		      :foreground "LightSkyBlue"
  		      :underline t)

  (set-face-attribute 'whitespace-space nil
  		      :background nil
  		      :foreground "#0B6121"
  		      :weight 'bold)

  (set-face-attribute 'whitespace-empty nil
  		      :background nil)

  (global-whitespace-mode t))

;;undo-tee
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (global-set-key "\C-z" 'undo-tree-visualize)
  (define-key undo-tree-visualizer-mode-map (kbd "C-j") 'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map "\r" 'undo-tree-visualizer-quit))

;;*scratch*
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
	(set-buffer (get-buffer-create "*scratch*"))
	(funcall initial-major-mode)
	(erase-buffer)
	(when (and initial-scratch-message (not inhibit-startup-message))
	  (insert initial-scratch-message))
	(or arg (progn (setq arg 0)
				   (switch-to-buffer "*scratch*")))
	(cond ((= arg 0) (message "*scratch* is cleared up."))
		  ((= arg 1) (message "another *scratch* is created.")))))

(add-hook 'kill-buffer-query-functions
		  (lambda ()
			(if (string= "*scratch*" (buffer-name))
				(progn (my-make-scratch 0) nil)
			  t)))

(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'ruby-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-intaraction-mode-hook 'hs-minor-mode)
(define-key global-map (kbd "C-#") 'hs-toggle-hiding)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-show-empty-filter-groups nil)

(setq ibuffer-saved-filter-groups
      '(("default"
	 ("dired" (mode . dired-mode))
	 ("emacs" (or
		   (name . "^\\*scratch\\*$")
		   (name . "^\\*Messages\\*$")
		   (name . "^\\*compilation\\*$")
		   (name . "^\\*Compile-Log\\*$")
		   (name . "^\\*backtrace\\*$")))
	 ("helm" (or
		  (name . "^\\*helm.+\\*$")))
	 ("magit" (or
		   (name . "^magit.+$"))))))

(add-hook 'ibuffer-mode-hook '(lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;;helm
(use-package helm-config
  :config
  (helm-mode 1)
  (defvar helm-source-emacs-commands
    (helm-build-sync-source "Emacs commands"
      :candidates (lambda ()
		    (let ((cmds))
		      (mapatoms
		       (lambda (elt) (when (commandp elt) (push elt cmds))))
		      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "A simple helm source for Emacs commands.")

  (defvar helm-source-emacs-commands-history
    (helm-build-sync-source "Emacs commands history"
      :candidates (lambda ()
		    (let ((cmds))
		      (dolist (elem extended-command-history)
			(push (intern elem) cmds))
		      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "Emacs commands history")

					; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

					;find-file-at-pointのui
  (add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil))

  (custom-set-variables
   '(helm-mini-default-sources '(helm-source-buffers-list
				 helm-source-files-in-current-dir
				 helm-source-emacs-commands-history
				 helm-source-emacs-commands)))

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key global-map (kbd "M-x") 'helm-M-x)
  (define-key global-map (kbd "C-;") 'helm-mini)
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
  (define-key helm-map (kbd "C-j") 'helm-maybe-exit-minibuffer))

;;yasnippet
(use-package yasnippet
  :config
  (yas-global-mode)
  (define-key yas-minor-mode-map (kbd "C-'") 'yas/expand)
  (define-key yas-minor-mode-map "\r" nil)
  (use-package helm-c-yasnippet
    :config
    (setq helm-yas-space-match-any-greedy t)
    (global-set-key (kbd "C-c y") 'helm-yas-complete)
    (push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)))

(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)

  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-j") 'company-complete-selection)

  (set-face-attribute
   'company-tooltip nil
   :foreground "black"
   :background "gray90")

  (set-face-attribute
   'company-tooltip-common nil
   :foreground "black"
   :background "gray90"
   :bold t)

  (set-face-attribute
   'company-tooltip-common-selection nil
   :foreground "black"
   :background "deep sky blue"
   :bold t)

  (set-face-attribute
   'company-tooltip-selection nil
   :foreground "black"
   :background "deep sky blue"
   :bold nil)

  (set-face-attribute
   'company-preview-common nil
   :background "deep sky blue"
   :foreground "black"
   :underline t)

  (set-face-attribute
   'company-scrollbar-fg nil
   :background "black")

  (set-face-attribute
   'company-scrollbar-bg nil
   :background "gray80")
  (global-company-mode))

;;irony
(use-package irony
  :config
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-to-list 'company-backends 'company-irony))

(use-package image+
  :config
  (imagex-auto-adjust-mode 1)
  (imagex-global-sticky-mode 1))

;;preview latex
(use-package px
  :config
  (setq org-format-latex-options
	'(:foreground default
		      :background default
		      :scale 3.0
		      :html-foreground "Black"
		      :html-background "Transparent"
		      :html-scale 1.0
		      :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))

(define-key dired-mode-map "r" 'revert-buffer)

;;org
(define-key org-mode-map (kbd "C-m") 'org-return-indent)
(define-key org-mode-map (kbd "C-j") 'org-insert-heading)
(use-package tabbar
  :config
  (define-key org-mode-map (kbd "<C-tab>") 'tabbar-forward-tab))

;;dired
(use-package dired-toggle
  :config
  (setq dired-toggle-window-size 48)
  (define-key dired-mode-map (kbd "C-j") 'dired-toggle-action-find-file))

(use-package dired-details
  :config
  (dired-details-install))

(use-package migemo
  :config
  (cond ((file-exists-p "/usr/share/cmigemo/")
	 (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"))
	((file-exists-p "~/.emacs.d/conf/migemo")
	 (setq migemo-dictionary "~/.emacs.d/conf/migemo/dict/utf-8/migemo-dict"))
	(t
	  (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")))
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-user-dictionary nil)
  (setq migemo-coding-system 'utf-8)
  (setq migemo-regex-dictionary nil)
  (load-library "migemo")
  (migemo-init))

(setq search-default-regexp-mode nil)

;;正規表現をPython風に
(use-package visual-regexp)
(use-package visual-regexp-steroids
   :config
   (define-key global-map (kbd "C-@") 'vr/query-replace))

(use-package ace-isearch
  :config
  (global-ace-isearch-mode 1)
  (setq ace-isearch-jump-delay 0.5)
  (setq ace-isearch-input-length 2)
  (setq helm-swoop-split-direction 'split-window-horizontally)
  (setq helm-swoop-split-with-multiple-windows t)
  (helm-migemo-mode 1))

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (define-key markdown-mode-map (kbd "\C-c \C-c \C-e") 'markdown-export)
  (define-key markdown-mode-map (kbd "\C-c \C-c \C-v")
    (lambda ()
      (interactive)
      (let ((html-file-name (concat (file-name-sans-extension buffer-file-name) ".html")))
	(markdown-export html-file-name)
	(when (one-window-p) (split-window))
	(other-window 1)
	(w3m-find-file html-file-name)))))

(find-file "~/.emacs.d/init.el") ;emacs起動時にinit.elを開く
(put 'dired-find-alternate-file 'disabled nil)

;;xref
(define-key xref--xref-buffer-mode-map (kbd "C-n") 'xref-next-line)
(define-key xref--xref-buffer-mode-map (kbd "C-p") 'xref-prev-line)

(defun byte-compile-this-file ()
  "Compile current-buffer-file of Lisp into a file of byte code."
  (interactive)
  (byte-compile-file buffer-file-name t))

;;elファイルを保存した時にコンパイルする
(add-hook 'after-save-hook (lambda ()
			     (when (and (string= (file-name-extension buffer-file-name) "el") (not (string= (file-name-nondirectory buffer-file-name) "init.el")))
			       (byte-compile-this-file)
			       (message (format "Wrote %s and byte-compile to %s.elc" buffer-file-name (file-name-sans-extension buffer-file-name))))))

(use-package multi-compile
  :config
  (setq multi-compile-alist '(
			      (c-mode . (("c-release" . "clang -O2 -Wall -lm -o %file-sans %file-name")
					 ("c-cv-release" . "clang -O2 -Wall -o %file-sans %file-name `pkg-config opencv --libs --cflags`")
					 ("c-glfw-release" . "clang -O2 -Wall -o %file-sans %file-name `pkg-config glfw3 --libs --cflags` `pkg-config gl --libs --cflags`"))
				      )
			      (c++-mode . (("c++-release" . "clang++ --std=c++14 -O2 -Wall -o %file-sans %file-name")
					   ("c++-cv-release" . "clang++ --std=c++14 -O2 -Wall -o %file-sans %file-name `pkg-config opencv --libs --cflags`")
					   ("c++-glfw-release" . "clang++ --std=c++14 -O2 -Wall -o %file-sans %file-name `pkg-config glfw3 --libs --cflags`"))
					)
			      (latex-mode . (("platex" . "platex %file-name")
					     ("rake" . "rake"))
					  )
			      ;("\\.dot\\'" . (("dot" . "dot -Teps %file-name -o %file-sans.eps")))
			      ))

;;compilation modeのwindowのサイズ
  (setq compilation-window-height 15)

;;compilation bufferのwindowがでる位置を下の方にする
  (defun custom-compilation-buffer-window ()
    (when (not (get-buffer-window "*compilation*"))
      (save-selected-window
	(save-excursion
	  (let* ((w (split-window-vertically))
		 (h (window-height w)))
	    (select-window w)
	    (switch-to-buffer "*compilation*")
	    (shrink-window (- h compilation-window-height)))))))

;;compilation modeでコンパイルが成功したときにバッファが消える時間
  (setq killing-compilation-buffer-time 1)

;;compilation modeでコンパイルが成功したときに*compilation*バッファを消す
  (defun bury-compile-buffer-if-successful (buffer string)
    "Bury a compilation buffer if succeeded without warnings "
    (if (and
	 (string-match "compilation" (buffer-name buffer))
	 (string-match "finished" string)
	 (not
	  (with-current-buffer buffer
	    (goto-char (point-min))
	    (search-forward "warning" nil t))))
	(save-selected-window
	  (run-with-timer killing-compilation-buffer-time nil
			  (lambda ()
			    (switch-to-buffer "*compilation*")
			    (kill-buffer-and-window)
			    )))))

  (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)
  (add-hook 'compilation-mode-hook 'custom-compilation-buffer-window)

  (setq multi-compile-completion-system 'helm)

  ;;コンパイルの前にセーブする
  (defun save-multi-compile-run ()
    (interactive)
    (save-buffer)
    (multi-compile-run))

  (global-set-key (kbd "C-c c") 'save-multi-compile-run))

;;スタイル設定
(add-hook 'c-mode-hook
          '(lambda()
             (c-set-style "stroustrup")
             ))

(setq eshell-cmpl-ignore-case t) ;補完時に大文字、小文字を区別しない
(setq eshell-hist-ignoredups t) ;;履歴で重複を無視
;;pathの設定
(setq eshell-path-env (getenv "PATH"))

;;clear
(defun eshell-clear-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;;prompt
(setq eshell-prompt-function
      (lambda ()
	(concat (propertize "yakumo@" 'face `(:foreground "lime green"))
		(propertize (eshell/pwd) 'face `(:foreground "lime green"))
		(propertize (if (= (user-uid) 0) "#" "$") 'face `(:foreground "lime green"))
		(propertize " " 'face `(:foreground "black")))))
(setq eshell-prompt-regexp "^[^#$]*[#$] ") ;promptの先頭

;;キーバインド
;;TODO:プロンプトの先頭でC-bを押下するとカーソルが食い込む
(add-hook 'eshell-mode-hook
	  '(lambda ()
	     (progn
	       (define-key eshell-mode-map "\C-a" 'eshell-bol) ;C-aでプロンプトの前に移動
	       ;;C-p,C-nでコマンド履歴ループ
	       (define-key eshell-mode-map "\C-p" 'eshell-previous-matching-input-from-input)
	       (define-key eshell-mode-map "\C-n" 'eshell-next-matching-input-from-input)
	       (define-key eshell-mode-map "\C-j" 'eshell-send-input)
	       (define-key eshell-mode-map "\C-l" 'eshell-clear-buffer))))

(setq eshell-command-aliases-list
      (append
       (list
        (list "ll" "ls -l")
        (list "la" "ls -a")
        (list "cat" "find-file $1"))))

;;helmを使用する
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                (kbd "M-p")
                'helm-eshell-history)))
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                (kbd "M-n")
                'helm-esh-pcomplete)))

;;shell-pop
(use-package shell-pop
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("eshell" "*eshell*"
				  (lambda nil (eshell shell-pop-term-shell)))))
   '(shell-pop-term-shell "/usr/bin/zsh")
   '(shell-pop-universal-key "C-$") ;C-$でshellの出し入れ
   '(shell-pop-window-height 75)
   '(shell-pop-full-span t)
   '(shell-pop-window-position "top")))

;;mozc
(use-package mozc-im
  :config
  (setq default-input-method "japanese-mozc-im")
  ;; mozc-cursor-color を利用するための対策
  (make-variable-buffer-local 'mozc-im-mode)
  (add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
  (add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil)))
  (advice-add 'mozc-cursor-color-update
	      :around (lambda (orig-fun &rest args)
			(let ((mozc-mode mozc-im-mode))
			  (apply orig-fun args))))

  ;; helm でミニバッファの入力時に IME の状態を継承しない
  (setq helm-inherit-input-method nil)

  ;; helm の検索パターンを mozc を使って入力する場合、入力中は helm の候補の更新を停止する
  (advice-add 'mozc-candidate-dispatch
	      :before (lambda (&rest args)
			(when helm-alive-p
			  (cl-case (nth 0 args)
			    ('update
			     (unless helm-suspend-update-flag
			       (helm-kill-async-processes)
			       (setq helm-pattern "")
			       (setq helm-suspend-update-flag t)))
			    ('clean-up
			     (when helm-suspend-update-flag
			       (setq helm-suspend-update-flag nil)))))))

  ;; helm で候補のアクションを表示する際に IME を OFF にする
  (advice-add 'helm-select-action
	      :before (lambda (&rest args)
			(deactivate-input-method)))
  (global-set-key (kbd "C-o") 'toggle-input-method))

(use-package mozc-popup
  :config
  ;; popupスタイル を使用する
  (setq mozc-candidate-style 'popup))

(add-to-list 'load-path "~/.emacs.d/mozc-el-extensions")
(use-package mozc-cursor-color
  :config
  ;; カーソルカラーを設定する
  (setq mozc-cursor-color-alist '((direct        . "green")
				  (read-only     . "red")
				  (hiragana      . "blue")
				  (full-katakana . "firebrick")
				  (half-ascii    . "dark goldenrod")
				  (full-ascii    . "orchid")
				  (half-katakana . "gold")))

  ;; カーソルの点滅を OFF にする
  (blink-cursor-mode 0))

(defadvice cua-sequence-rectangle (around my-cua-sequence-rectangle activate)
  "連番を挿入するとき、紫のところの文字を上書きしないで左にずらす"
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (string-to-number
            (read-string "Start value: (0) " nil nil "0")))
         (string-to-number
          (read-string "Increment: (1) " nil nil "1"))
         (read-string (concat "Format: (" cua--rectangle-seq-format ") "))))
  (if (= (length format) 0)
      (setq format cua--rectangle-seq-format)
    (setq cua--rectangle-seq-format format))
  (cua--rectangle-operation 'clear nil t 1 nil
     '(lambda (s e l r)
         (kill-region s e)
         (insert (format format first))
         (yank)
         (setq first (+ first incr)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-files-in-current-dir helm-source-emacs-commands-history helm-source-emacs-commands)))
 '(package-selected-packages
   (quote
    (total-lines yasnippet-snippets yaml-mode window-numbering visual-regexp-steroids use-package undo-tree tabbar shell-pop restart-emacs rainbow-mode rainbow-delimiters px multi-compile mozc-popup mozc-im migemo markdown-mode magit irony image-dired+ image+ hlinum helm-xref helm-swoop helm-company helm-c-yasnippet graphviz-dot-mode dired-toggle avy ace-isearch)))
 '(shell-pop-full-span t)
 '(shell-pop-shell-type
   (quote
    ("eshell" "*eshell*"
     (lambda nil
       (eshell shell-pop-term-shell)))))
 '(shell-pop-term-shell "/usr/bin/zsh")
 '(shell-pop-universal-key "C-$")
 '(shell-pop-window-position "top")
 '(shell-pop-window-size 75))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:inherit default :foreground "red"))))
 '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))))
 '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))))
 '(linum ((t (:inherit (shadow default) :foreground "black" :background "white" :height 130))))
 '(linum-highlight-face ((t (:foreground "black" :background "yellow" :underline nil :height 130))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "DeepPink3"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "DeepSkyBlue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "DarkMagenta"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "LimeGreen"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "yellow3"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "tomato"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "forest green")))))
