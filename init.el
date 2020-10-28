;;====================================================================================
;; package setting
;;====================================================================================
(setq-default package-archives
              '(("melpa" . "https://melpa.org/packages/")
                ("org" . "https://orgmode.org/elpa/")
                ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

;;====================================================================================
;; defuns
;;====================================================================================
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
               (shrink-windown dy))
              ((= c ?N)
               (shrink-window (/ (frame-height) 4)))
              ((= c ?p)
               (enlarge-window dy))
              ((= c ?P)
               (enlarge-window (/ (frame-height) 4)))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))

(defun text-scale-increase-1 ()
  (interactive)
  (text-scale-increase 1))

(defun text-scale-decrease-1 ()
  (interactive)
  (text-scale-decrease 1))

(defun insert-t ()
  (interactive)
  (insert "\t"))

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

(defun eshell-pronpt-func ()
  (concat (propertize "yakumo@" 'face `(:foreground "lime green"))
          (propertize (eshell/pwd) 'face `(:foreground "lime green"))
          (propertize (if (= (user-uid) 0) "#" "$") 'face `(:foreground "lime green"))
          (propertize " " 'face `(:foreground "black"))))

(defun eshell-clear-buffer ()
  ;;eshell上でのclearコマンド
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun catenate (file)
  ;;eshell上でのcatコマンド
  (interactive "Ffile: ")
  (let ((tfile (file-truename file)))
    (kill-current-buffer)
    (find-file tfile)))

;;====================================================================================
;; leaves
;;====================================================================================
(leaf leaf-keywords
  :ensure t
  :init
  (leaf hydra :ensure t)
  (leaf el-get :ensure t)
  (leaf blackout :ensure t)

  :config
  (leaf-keywords-init))

(leaf base-settings
  :config
  (toggle-scroll-bar -1) ;横スクロールバーを表示しない
  (tool-bar-mode -1) ;ツールバーを非表示
  (menu-bar-mode -1) ;メニューバー非表示
  (fset 'yes-or-no-p 'y-or-n-p) ;yes/no の代わりに y/n
  (blink-cursor-mode 0) ; カーソルの点滅を OFF にする
  (put 'upcase-region 'disabled nil) ;upcase時にアラートを出さない
  (put 'downcase-region 'disabled nil) ;downcase時にアラートを出さない
  (ffap-bindings) ;C-x C-fをめちゃめちゃ便利に
  (cua-mode t)
  (add-hook 'kill-buffer-query-functions
            (lambda ()
              (if (string= "*scratch*" (buffer-name))
                  (progn (my-make-scratch 0) nil)
                t)))
  :setq-default
  `((ring-bell-function . 'ignore) ;ビープ音を無効
    (backup-inhibited . t) ;バックアップファイルを作らない
    (inhibit-startup-message . t) ;セットアップメッセージを非表示
    (initial-scratch-message . "(setq eval-expression-print-length nil)\n(setq eval-expression-print-level nil)") ;scratchにメッセージを表示しない
    (select-enable-clipboard . t) ;Yankバッファとクリップボードの同期
    (indicate-buffer-boundaries . 'left) ;右にEOFの印を表示
    (indent-tabs-mode . nil)
    (default-tab-width . 4) ;タブ幅4
    (cua-enable-cua-keys . nil) ;cua-modeの変なキーバインド無効
    ))

(leaf key-bindings
  :bind
  ("C-j" . newline-and-indent)
  ("M-]" . forward-sexp) ;対応する開き括弧へ移動
  ("M-[" . backward-sexp) ;対応する閉じ括弧へ移動
  ("M-h" . backward-kill-word)
  ("C-c C-c" . comment-region)
  ("C-c C-;" . uncomment-region)
  ("C-t" . insert-t)
  ("C-+" . text-scale-increase-1)
  ("C--" . text-scale-decrease-1))

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>")) ;minibufferにも反映させる
(define-key lisp-interaction-mode-map (kbd "C-j") 'newline-and-indent)
(define-key lisp-interaction-mode-map (kbd "C-m") 'eval-print-last-sexp)
(define-key global-map "\C-q" (make-sparse-keymap))
(global-set-key (kbd "C-q C-q") 'quoted-insert) ;quoted-insert は C-q C-q へ割り当て
;;windowに関するキーバインド
(global-set-key (kbd "C-q k") 'delete-other-windows)
(global-set-key (kbd "C-q K") 'delete-window)
(global-set-key (kbd "C-q w") 'split-window-below)
(global-set-key (kbd "C-q h") 'split-window-right)
(global-set-key (kbd "C-q f") 'windmove-right)
(global-set-key (kbd "C-q b") 'windmove-left)
(global-set-key (kbd "C-q n") 'windmove-down)
(global-set-key (kbd "C-q p") 'windmove-up)
(global-set-key (kbd "C-q r") 'window-resizer)

(leaf font-setting
  :config
  ;;http://extra-vision.blogspot.com/2016/07/emacs.html
  (create-fontset-from-ascii-font
   "Ricty Diminished-16:weight=normal:slant=normal"
   nil
   "Ricty_Diminished")

  (set-fontset-font
   "fontset-Ricty_Diminished"
   'unicode
   "Ricty Diminished-16:weight=normal:slant=normal"
   nil
   'append)

  (add-to-list 'default-frame-alist '(font . "fontset-Ricty_Diminished")))

(leaf face-setting
  :config
  (show-paren-mode t) ;対応する括弧を強調
  (global-display-line-numbers-mode) ;行数表示
  (global-hl-line-mode t) ;現在行ハイライト
  (set-face-attribute 'show-paren-match nil
                      :background nil
                      :foreground nil
                      :underline "red"
                      :weight 'extra-bold)
  (set-face-attribute 'mode-line nil
                      :foreground "white"
                      :background "black")
  (set-face-attribute 'region nil
                      :foreground "DeepPink1"
                      :background "LightSkyBlue")
  (set-face-attribute 'hl-line nil
                      :background "gray88")
  (set-face-attribute 'line-number nil
                      :background "gray88"
                      :foreground "black")
  (set-face-attribute 'line-number-current-line nil
                      :foreground "red"
                      :background nil
                      :weight 'ultra-bold)

  :setq `((show-paren-delay . 0) ;括弧のハイライトまでの遅延
          (show-paren-style . 'expression) ;括弧内を強調
          ))

(leaf smart-mode-line
  :ensure t
  :init
  (column-number-mode t)
  (setq sml/no-confirm-load-theme t)
  (defvar sml/shorten-directory nil)
  (defun my-set-line-numbers ()
    (setq-default mode-line-front-space
                  (append mode-line-front-space
                          '((:eval (format ":%d" total-lines))))))
  (setq display-time-string-forms
        '(month "/" day "(" dayname ") " 24-hours ":" minutes))
  :config
  (leaf total-lines
    :ensure t
    :require t
    :init
    (global-total-lines-mode t))
  (display-time)
  (add-hook 'after-init-hook 'my-set-line-numbers)
  (sml/setup))

(leaf visual-regexp-steroids
  :ensure t
  :init
  (leaf visual-regexp
    :ensure t
    :require t)
  :setq
  (search-default-regexp-mode . nil)
  :config
  (define-key global-map (kbd "C-@") 'vr/query-replace))

(leaf helm
  :ensure t
  :init
  (leaf helm-smex
    :ensure t
    :require t)
  :config
  (global-set-key (kbd "C-;") 'helm-command-prefix)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-map (kbd "C-j") 'helm-maybe-exit-minibuffer)
  (define-key helm-map (kbd "C-l") 'helm-previous-source)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "M-x") 'helm-smex)
  (helm-autoresize-mode t)
  (helm-mode t)
  :setq
  (helm-split-window-in-side-p . t)
  (helm-move-to-line-cycle-in-source . t)
  (helm-ff-search-library-in-sexp . t)
  (helm-scroll-amount . 8)
  (helm-ff-file-name-history-use-recentf . t)
  (helm-autoresize-max-height . 50)
  (helm-autoresize-min-height . 50)
  (helm-M-x-fuzzy-match . t))

(leaf helm-swoop
  :ensure t
  :after helm migemo
  :bind ("M-i" . helm-swoop)
  :config
  (helm-migemo-mode t))

(leaf ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-q o") 'ace-window)
  :custom-face
  (aw-leading-char-face . '((t
                             (:height 5.0
                                      :foreground "red"))))
  :setq
  (aw-keys . '(?j ?k ?l ?\; ?a ?s ?d ?f)))

(leaf ibuffer
  :config
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
  (add-hook 'ibuffer-mode-hook '(lambda () (ibuffer-switch-to-saved-filter-groups "default"))))

(leaf flycheck
  :ensure t)

(leaf yasnippet
  :ensure t
  :init
  (yas-global-mode)
  :config
  (define-key yas-minor-mode-map (kbd "C-'") 'yas-expand)
  (define-key yas-minor-mode-map "\r" nil)
  (locate-user-emacs-file "snippets")
  (leaf yasnippet-snippets
    :ensure t))

(leaf helm-c-yasnippet
  :ensure t
  :after helm yasnippet
  :setq
  (helm-yas-space-match-any-greedy . t)
  :config
  (global-set-key (kbd "C-; y") 'helm-yas-complete)
  (push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist))

(leaf company
  :ensure t
  :after yasnippet
  :init
  (global-company-mode)
  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-j") 'company-complete-selection)
  (define-key company-active-map (kbd "C-m") 'newline-and-indent)
  ;; yasnippetとの連携
  (defvar company-mode/enable-yas t)
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  (set-face-attribute
   'company-tooltip nil
   :foreground "black"
   :background "#eaedf7")
  (set-face-attribute
   'company-tooltip-common nil
   :foreground "#d41513"
   :background nil
   :bold t)
  (set-face-attribute
   'company-tooltip-common-selection nil
   :foreground nil
   :background nil)
  (set-face-attribute
   'company-tooltip-selection nil
   :foreground nil
   :background "sky blue"
   :bold nil)
  (set-face-attribute
   'company-preview-common nil
   :background nil
   :foreground nil
   :underline nil)
  (set-face-attribute
   'company-scrollbar-fg nil
   :background "blue")
  (set-face-attribute
   'company-scrollbar-bg nil
   :background "#c6d1f7")
  :setq
  (company-idle-delay . 0)
  (company-minimum-prefix-length . 2)
  (company-selection-wrap-around . t)
  (company-dabbrev-downcase . nil) ;;case sensitiveに
  )

(leaf hs-minor-mode
  :init
  (define-key global-map (kbd "C-#") 'hs-toggle-hiding)
  :hook
  c-mode-hook
  c++-mode-hook
  ruby-mode-hook
  emacs-lisp-mode-hook
  lisp-intaraction-mode-hook)

(leaf xref
  :config
  (define-key global-map (kbd "C-.") 'xref-find-definitions)
  (define-key global-map (kbd "C-,") 'xref-pop-marker-stack))

(leaf lsp-mode
  :ensure t
  :init
  (lsp)
  :setq
  (lsp-eldoc-render-all . t)
  (lsp-enable-indentation . nil)
  (lsp-prefer-capf . t)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection (executable-find "pyls"))
    :major-modes '(python-mode)
    :remote? t
    :server-id 'pyls-remote))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection (executable-find "clangd-10"))
    :major-modes '(c-mode c++-mode)
    :remote? t
    :server-id 'clangd-remote))
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp))

(leaf lsp-ui
  :ensure t
  :after lsp-mode flycheck xref
  :config
  (define-key lsp-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :bind
  ((:lsp-mode-map
   ;;("C-c C-r" . lsp-ui-peek-find-references)
   ;;("C-c C-j" . lsp-ui-peek-find-definitions)
   ("C-c i"   . lsp-ui-imenu)
   ("M-,"   . lsp-ui-peek-find-implementation)
   ;;("C-c s"   . lsp-ui-sideline-mode)
   ;;("C-c d"   . ladicle/toggle-lsp-ui-doc))
   )
   (:lsp-ui-imenu-mode-map
    ("C-j" . lsp-ui-imenu--visit)))
  :setq
  (lsp-ui-peek-enable . t)
  (lsp-ui-peek-always-show . t)
  (lsp-ui-peek-peek-height . 30)
  (lsp-ui-peek-list-width . 30)
  (lsp-ui-peek-fontify . 'always)
  (lsp-ui-flycheck-enable . t)
  (lsp-ui-sideline-enable . t)
  (lsp-ui-sideline-ignore-duplicate . t)
  (lsp-ui-sideline-show-symbol  . t)
  (lsp-ui-sideline-show-hover . t)
  (lsp-ui-sideline-show-diagnostics . t)
  (lsp-ui-sideline-show-code-actions . t)
  (lsp-ui-imenu-enable . t)
  (lsp-ui-imenu-kind-position . 'top)
  (lsp-ui-doc-position . 'at-point)
  (lsp-ui-doc-delay . 0.2)
  :hook lsp-mode)

(leaf projectile
  :ensure t
  :init
  (projectile-mode t))

(leaf neotree
  :ensure t
  :after projectile
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  :custom
  (neo-theme . 'nerd2)
  :bind
  ("C-c d" . neotree-projectile-toggle)
  :preface
  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
         ;;; Pick one: projectile or find-file-in-project
             (projectile-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name)))))))

(leaf magit
  :ensure t)

(leaf restart-emacs
  :ensure t)

(leaf undo-tree
  :ensure t
  :global-minor-mode global-undo-tree-mode
  :bind
  ("\C-z" . undo-tree-visualize)
  :config
  (define-key undo-tree-visualizer-mode-map (kbd "C-j") 'undo-tree-visualizer-quit))

(leaf rainbow-mode
  :ensure t
  :hook
  lisp-interaction-mode-hook
  emacs-lisp-mode-hook)

(leaf rainbow-delimiters
  :ensure t
  :init
  (rainbow-delimiters-mode t)
  :hook
  lisp-interaction-mode-hook
  emacs-lisp-mode-hook
  :config
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                      :foreground "blue")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil
                      :foreground "dark orange")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil
                      :foreground "DeepPink3")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil
                      :foreground "DeepSkyBlue")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil
                      :foreground "DarkMagenta")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil
                      :foreground "#006432")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil
                      :foreground "yellow3")
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil
                      :foreground "tomato")
  (set-face-attribute 'rainbow-delimiters-depth-9-face nil
                      :foreground "green"))

(leaf tab-bar
  :init
  (tab-bar-mode)

  :config
  (set-face-attribute 'tab-bar-tab nil
                      :foreground "black"
                      :background "white")
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :foreground "white"
                      :background "gray30")
  (set-face-attribute 'tab-bar nil
                      :background "black"))

(leaf whitespace
  :init
  (global-whitespace-mode t)

  :setq-default
  (whitespace-style . '(face           ; faceで可視化
                        trailing       ; 行末
                        tabs           ; タブ
                        spaces         ; スペース
                        empty          ; 先頭/末尾の空行
                        ))
  (whitespace-space-regexp . "\\(\u3000+\\)")
  (whitespace-action . '(auto-cleanup))

  :config
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (set (make-local-variable 'whitespace-action) nil)))
  (set-face-attribute 'whitespace-trailing nil
                      :foreground "DeepPink"
                      :background nil
                      :underline t)
  (set-face-attribute 'whitespace-tab nil
                      :foreground nil
                      :background "azure")
  (set-face-attribute 'whitespace-space nil
                      :foreground nil
                      :background nil)
  (set-face-attribute 'whitespace-empty nil
                      :foreground nil
                      :background nil))

(leaf eshell-mode
  :after helm
  :setq-default
  (eshell-path-env . '(getenv "PATH")) ;pathの設定
  (eshell-prompt-function . 'eshell-pronpt-func) ;prompt
  (eshell-prompt-regexp . "^[^#$]*[#$] ") ;promptの先頭
  (eshell-command-aliases-list .
                               '(append
                                 (list
                                  (list "ll" "ls -l")
                                  (list "la" "ls -a")
                                  (list "c" "cd $wd")
                                  (list "cat" "catenate $1 > /dev/null"))))

  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (progn
                ;;TODO:プロンプトの先頭でC-bを押下するとカーソルが食い込む
                (define-key eshell-mode-map "\C-a" 'eshell-bol) ;C-aでプロンプトの前に移動
                ;;C-p,C-nでコマンド履歴ループ
                (define-key eshell-mode-map "\C-p" 'eshell-previous-matching-input-from-input)
                (define-key eshell-mode-map "\C-n" 'eshell-next-matching-input-from-input)
                (define-key eshell-mode-map "\C-j" 'eshell-send-input)
                (define-key eshell-mode-map "\C-l" 'eshell-clear-buffer))))

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
                  'helm-esh-pcomplete))))

(leaf migemo
  :ensure t
  :require t
  :config
  (cond ((file-exists-p "/usr/share/cmigemo/")
         (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"))
        ((file-exists-p "~/.emacs.d/cmigemo")
         (setq migemo-dictionary (concat (getenv "HOME") ".emacs.d\\cmigemo\\dict\\utf-8\\migemo-dict")))
        (t
         (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")))
  (migemo-init)
  :setq
  (migemo-command . "cmigemo")
  (migemo-options . '("-q" "--emacs"))
  (migemo-user-dictionary . nil)
  (migemo-coding-system . 'utf-8)
  (migemo-regex-dictionary . nil)
  (load-library . "migemo"))

(leaf shell-pop
  :ensure t
  :custom
  (shell-pop-term-shell . "/usr/bin/zsh")
  (shell-pop-universal-key . "C-$")
  (shell-pop-window-height . 75)
  (shell-pop-full-span . t)
  (shell-pop-window-position . "top")
  (shell-pop-shell-type . (quote ("eshell" "*eshell*"
                                  (lambda nil (eshell shell-pop-term-shell)))))
  )

(leaf mozc
  :ensure t
  :el-get iRi-E/mozc-el-extensions
  :after helm
  :config
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

  ;;helm で候補のアクションを表示する際に IME を OFF にする
  (advice-add 'helm-select-action :before (lambda (&rest args)
                                            (deactivate-input-method)))
  (define-key helm-map (kbd "C-o") 'toggle-input-method)
  (leaf mozc-popup
    :ensure t
    :require t)
  (leaf mozc-cursor-color
    :require t)
  :custom
  (default-input-method . "japanese-mozc")
  (helm-inherit-input-method . nil) ;helm でミニバッファの入力時に IME の状態を継承しない
  ;;カーソルカラーを設定する
  (mozc-cursor-color-alist . '((direct        . "black")
                               (read-only     . "red")
                               (hiragana      . "#00b5b5")
                               (full-katakana . "firebrick")
                               (half-ascii    . "dark goldenrod")
                               (full-ascii    . "orchid")
                               (half-katakana . "gold")))
  :bind
  (("C-o" . toggle-input-method))
  )

(leaf tramp
  :setq
  (tramp-default-method . "ssh"))
