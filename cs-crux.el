;;; cs-crux.el --- my ridiculously useful functions  -*- lexical-binding: t; -*-
;; Copyright (C) 2020  chris

;; Author: chris <chris@chris-lenovo>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar cs-crux-directory nil "*Directory for data files belonging to package `foo`.")
(setq cs-crux-directory (file-name-directory (if load-file-name
                                                 load-file-name
                                               (buffer-file-name))))

(defun find-next-file (&optional backward)
  "Find the next file (of the same type, sorted by name) in the current directory.
With prefix arg, find the previous file. Adapted from https://emacs.stackexchange.com/a/12164"
  (interactive "P")
  (when buffer-file-name
    (let* ((file (expand-file-name buffer-file-name))
           (files (cl-remove-if (lambda (file_c)
                                  (not (string-equal (file-name-extension file_c)
                                                     (file-name-extension file))))
                                (cl-remove-if (lambda (file_c)
                                                (and (cl-first (file-attributes file_c))))
                                              (sort (directory-files (file-name-directory file)
                                                                     t
                                                                     nil
                                                                     t)
                                                    'string<))))
           (pos (mod (+ (cl-position file files :test 'equal)
                        (if backward -1 1))
                     (length files))))
      (find-file (nth pos files)))))

(define-key pdf-view-mode-map (kbd "M-n") 'find-next-file)
(define-key pdf-view-mode-map (kbd "M-p") (lambda ()
                                            (interactive)
                                            (let ((current-prefix-arg 4))
                                              (call-interactively 'find-next-file))))

(defun cs-dired-open-file-externally ()
  "In dired, open the filepath named on this line."
  (interactive)
  (let* ((filepath (dired-get-filename nil t)))
    (message "Opening %s..." filepath)
    (cond
     ((and (boundp 'cs-dired-open-notebook)
           (string-equal (file-name-extension filepath)
                         "ipynb")
           (cs-dired-open-notebook filepath)))
     (t (let* ()
          (if (equal system-type 'gnu/linux)
              (call-process "xdg-open" nil 0 nil filepath)
            (if (equal system-type 'windows-nt)
                (shell-command-to-string (concat "start " filepath)))))))))

(define-key dired-mode-map (kbd "C-c C-o") 'cs-dired-open-file-externally)

(defun cs-move-to-beginning-of-visual-line ()
  "Move to the beginning of the visual line minus the whitespace, then toggle."
  (interactive)
  (let* (pos-first-non-ws-char-in-cur-vis-line)
    (save-excursion
      (beginning-of-visual-line)
      (setq pos-first-non-ws-char-in-cur-vis-line (if (string-match "\s" (string (char-after (point))))
                                                      (search-forward-regexp "\s+"
                                                                             (save-excursion
                                                                               (end-of-visual-line)
                                                                               (point))
                                                                             t)
                                                    (point))))
    (if (equal pos-first-non-ws-char-in-cur-vis-line (point))
        (beginning-of-visual-line)
      (goto-char pos-first-non-ws-char-in-cur-vis-line))))

(global-set-key (kbd "C-a")
                #'cs-move-to-beginning-of-visual-line)

(defun my-toggle-margins (&optional enable-thick-margin)
  "Set margins in current buffer."
  (interactive)
  (if (and (or (> left-margin-width 0)
               (> right-margin-width 0))
           (not enable-thick-margin))
      (progn
        (setq left-margin-width 0)
        (setq right-margin-width 0)
        (set-window-buffer (selected-window)
                           (current-buffer)))
    (setq left-margin-width 26)
    (setq right-margin-width 26)
    (set-window-buffer (selected-window)
                       (current-buffer))))

(global-set-key [f5]
                'my-toggle-margins)

(defun cs-make-all-writable ()
  "Sometimes sections (e.g. properties of org files)
  are not writeable. This makes them writeable."
  (interactive)
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min)
                            (point-max)
                            '(read-only t))))

(global-set-key (kbd "C-x w")
                'cs-make-all-writable)

(defun list-packages-and-versions ()
  "Returns a list of all installed packages and their versions"
  (mapcar (lambda (pkg)
            `(,pkg
              ,(package-desc-version (cadr (assq pkg package-alist)))))
          package-activated-list))

(defun google-quickly ()
  "Googles a query or region if any."
  (interactive)
  (browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
                      (if mark-active
                          (buffer-substring (region-beginning)
                                            (region-end))
                        (read-string "Google: ")))))

(global-set-key (kbd "C-x C-g")
                'google-quickly)


(defun outside-terminal-with-tmux ()
  (interactive)
  (shell-command "gnome-terminal -e 'tmux new' >/dev/null"))

(defun outside-terminal-with-windows ()
  "open git-bash or cmd, if git-bash is not installed"
  (interactive)
  (let ((proc
         (let* ((shells-paths
                 (remove nil
                         (mapcar (lambda (path)
                                   (when (file-exists-p path)
                                     path))
                                 (list (concat (file-name-as-directory (expand-file-name "~"))
                                               "Microsoft/Windows/Start Menu/Programs/Anaconda3 (64-bit)/Anaconda Powershell Prompt.lnk")
                                       (concat (file-name-as-directory (expand-file-name "~"))
                                               "%homepath%/AppData/Roaming/Microsoft/Windows/Start Menu/Programs/Anaconda3 (64-bit)/Anaconda Powershell Prompt (anaconda3).lnk")
                                       (concat (file-name-as-directory (expand-file-name "~/../..")
                                                                       ; base dir of user, like "c:/Users/IEUser"nn
                                                                       )
                                               "AppData/Local/Programs/Git/git-bash.exe")
                                       (concat (file-name-as-directory (expand-file-name "/"))
                                               "Program Files/Git/git-bash.exe"))))))))))


  (start-process "cmd"
               nil
               "cmd.exe"
               "/C"
               " start cmd.exe"))

(defun outside-terminal (&optional arg)
  (interactive "p")
  (if (eq system-type 'gnu/linux)
      (outside-terminal-with-tmux)
    (if (eq system-type 'windows-nt)
        (progn
          (outside-terminal-with-windows)
          (when (eq arg 4)
            (message (prin1-to-string (open-anaconda-powershell))))))))

(global-set-key (kbd "C-x C-m C-t")
                'outside-terminal)

(defun outside-explorer ()
  (interactive)
  (cond
   ((eq system-type 'gnu/linux)
    (let* (s)
      (setq s (concat "nautilus "
                      (file-name-directory buffer-file-name)
                      " & "))
      (message s)
      (call-process-shell-command s nil 0)))
   ((eq system-type 'windows-nt)
    (let* (s)
      (setq s (concat "start explorer ."))
      (message s)
      (call-process-shell-command s nil 0)))
   (t (error "system not handled"))))

(global-set-key (kbd "C-x C-m C-e")
                'outside-explorer)  ; open gui file explorer

(defun outside-browser ()
  (interactive)
  (setq s (concat "chromium-browser "
                  (file-name-directory buffer-file-name)
                  " & "))
  (message s)
  (call-process-shell-command s nil 0))

(global-set-key (kbd "C-x C-m C-b")
                'outside-browser)  ; open browser at that file

(defun kill-non-visible-buffers ()
  "Kill all buffers not currently shown in a window somewhere."
  (interactive)
  (dolist (buf (buffer-list))
    (unless (get-buffer-window buf 'visible)
      (kill-buffer buf))))

(defun new-buffer-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer
                    '(display-buffer-pop-up-frame . nil))))

(global-set-key (kbd "C-c n")
                #'new-buffer-frame)

;; search for the current folder's desktop-setup.el file, load it and execute the create-project-desktop-setup function


;; ------- put filename to clipboard --------

(defun cs-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min)
                               (point-max)))
      (message filename))))

(global-set-key (kbd "C-, c f c")
                'cs-put-file-name-on-clipboard)


(defun cs-put-directory-name-on-clipboard ()
  (interactive)
  (let ((dir-name (file-name-directory (if (equal major-mode 'dired-mode)
                                           default-directory
                                         (buffer-file-name)))))
    (when dir-name
      (with-temp-buffer
        (insert dir-name)
        (clipboard-kill-region (point-min)
                               (point-max)))
      (message dir-name))))

(global-set-key (kbd "C-, c d c")
                'cs-put-directory-name-on-clipboard)

(global-set-key (kbd "C-, c d o")
                'cs-open-file-from-clipboard)


;; ---- open file from clipboard

(defun cs-open-file-from-clipboard ()
  (interactive)
  (find-file (helm-read-file-name "open filepath from clipboard: "
                                  :initial-input (with-temp-buffer
                                                   (yank)
                                                   (buffer-string)))))

(global-set-key (kbd "C-, c f o")
                'cs-open-file-from-clipboard)


;; ---- drag and drop files (as links) from explorer into org-mode -----

(defun my-dnd-func (event)
  (interactive "e")
  (goto-char (nth 1
                  (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (type (car payload))
         (fname (cadr payload))
         (img-regexp "\\(png\\|jp[e]?g\\)\\>"))
    (cond
     ;; insert image link
     ((and (eq 'drag-n-drop (car event))
           (eq 'file type)
           (string-match img-regexp fname))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; insert image link with caption

     ((and (eq 'C-drag-n-drop (car event))
           (eq 'file type)
           (string-match img-regexp fname))
      (insert "#+ATTR_ORG: :width 300\n")
      (insert (concat "#+CAPTION: "
                      (read-input "Caption: ")
                      "\n"))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; C-drag-n-drop to open a file

     ((and (eq 'C-drag-n-drop (car event))
           (eq 'file type))
      (find-file fname))
     ((and (eq 'M-drag-n-drop (car event))
           (eq 'file type))
      (insert (format "[[attachfile:%s]]" fname)))
     ;; regular drag and drop on file

     ((eq 'file type)
      (insert (format "[[%s]]\n" fname)))
     (t (error "I am not equipped for dnd on %s" payload)))))





(defun query-swap-strings (from-string to-string &optional delimited start end)
  "Swap occurrences of FROM-STRING and TO-STRING.
   Source: https://emacs.stackexchange.com/a/27170"
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Query swap"
                   (if current-prefix-arg
                       (if (eq current-prefix-arg '-) " backward" " word")
                     "")
                   (if (use-region-p) " in region" ""))
           nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           (if (use-region-p) (region-beginning))
           (if (use-region-p) (region-end)))))
  (save-excursion
    (goto-char 0)
    (perform-replace
     (concat "\\(" (regexp-quote from-string) "\\)\\|" (regexp-quote to-string))
     `(replace-eval-replacement replace-quote (if (match-string 1) ,to-string ,from-string))
     t t delimited nil nil start end)))


(define-key org-mode-map (kbd "<drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<C-drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<M-drag-n-drop>") 'my-dnd-func)

;; ---- jump to anaconda mode python library
(defun cs-python-jump-to-file ()
  "In pyvenv, the WORKON_HOME variable specifies the path to the conda environments.
The name of the conda virtual environment is saved in dir-locals as pyvenv-workon in the project's root.
On an import statement, this function jumps to the file at point in that conda virtual environment.
(word-at-point)"
  (interactive)
  ;; (let* ((wap (word-at-point)))
  ;;   (concat (file-name-as-directory (pyvenv-workon-home))
  ;;           (file-name-as-directory pyvenv-workon)
  ;;           "lib/site-packages/"
  ;;           (set-text-properties 0 (length wap) nil wap)))
)

(defun open-anaconda-powershell (&optional environment-name)
  (interactive)
  (unless environment-name
    (if (and (boundp 'pyvenv-workon) pyvenv-workon)
        (setq environment-name pyvenv-workon)
      (setq environment-name "anaconda3")))
  (let* ((tmp-file-path (concat (temporary-file-directory)
                                "anaconda-powershell-link-no-spaces.lnk"))
         (anaconda-powershell-link-path (concat (file-name-as-directory (expand-file-name "~"))
                                                "Microsoft/Windows/Start Menu/Programs/Anaconda3 (64-bit)/Anaconda Powershell Prompt ("
                                                environment-name
                                                ").lnk")))
    (if (file-exists-p anaconda-powershell-link-path)
        (progn
          (copy-file anaconda-powershell-link-path tmp-file-path
                     t)
          (start-process "cmd"
                         nil
                         "cmd.exe"
                         "/C"
                         (concat "start " tmp-file-path))
          t)
      (message (concat "no file " anaconda-powershell-link-path))
      nil)))

(defun open-cmd-with-anaconda-enabled (&optional environment-name)
  (interactive)
  (unless environment-name
    (if (and (boundp 'pyvenv-workon) pyvenv-workon)
        (setq environment-name pyvenv-workon)
      (setq environment-name "base")))
  (set-process-query-on-exit-flag (start-process "cmd"
                                                 nil
                                                 "cmd.exe"
                                                 "/C"
                                                 (concat "start conda activate " environment-name))
                                  nil))

(defun xah-insert-random-uuid ()
  "Insert a UUID.
This commands calls “uuidgen” on MacOS, Linux, and calls PowelShell on Microsoft Windows.
URL `http://ergoemacs.org/emacs/elisp_generate_uuid.html'
Version 2020-06-04"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (shell-command "pwsh.exe -Command [guid]::NewGuid().toString()" t))
   ((string-equal system-type "darwin") ; Mac
    (shell-command "uuidgen" t))
   ((string-equal system-type "gnu/linux")
    (shell-command "uuidgen" t))
   (t
    ;; code here by Christopher Wellons, 2011-11-18.
    ;; and editted Hideki Saito further to generate all valid variants for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.
    (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                              (user-uid)
                              (emacs-pid)
                              (system-name)
                              (user-full-name)
                              (current-time)
                              (emacs-uptime)
                              (garbage-collect)
                              (buffer-string)
                              (random)
                              (recent-keys)))))
      (insert (format "%s-%s-4%s-%s%s-%s"
                      (substring myStr 0 8)
                      (substring myStr 8 12)
                      (substring myStr 13 16)
                      (format "%x" (+ 8 (random 4)))
                      (substring myStr 17 20)
                      (substring myStr 20 32)))))))

(defun xah-get-random-uuid ()
  "Insert a UUID.
This commands calls “uuidgen” on MacOS, Linux, and calls PowelShell on Microsoft Windows.
URL `http://ergoemacs.org/emacs/elisp_generate_uuid.html'
Version 2020-06-04"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (shell-command "pwsh.exe -Command [guid]::NewGuid().toString()" t))
   ((string-equal system-type "darwin") ; Mac
    (shell-command "uuidgen" t))
   ((string-equal system-type "gnu/linux")
    (shell-command-to-string "uuidgen"))
   (t
    ;; code here by Christopher Wellons, 2011-11-18.
    ;; and editted Hideki Saito further to generate all valid variants for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.
    )))


(defun cs-get-uuid ()
  ""
  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                              (user-uid)
                              (emacs-pid)
                              (system-name)
                              (user-full-name)
                              (current-time)
                              (emacs-uptime)
                              (garbage-collect)
                              (buffer-string)
                              (random)
                              (recent-keys)))))
      (format "%s-%s-4%s-%s%s-%s"
              (substring myStr 0 8)
              (substring myStr 8 12)
              (substring myStr 13 16)
              (format "%x" (+ 8 (random 4)))
              (substring myStr 17 20)
              (substring myStr 20 32))))


(defun create-node (&optional arg)
  "creates an org-mode file with a uuid as the name and inserts a link to that file, plus a marker of type branch with a uuid at the beginning. "
  (interactive "P")

  ;; (when (org-cursor-on-link)
  ;;   (error "move cursor away from link!"))

  (unless (string-equal "org"
                        (file-name-extension (buffer-file-name)))
    (error "not an org file"))

  ;; move cursor so that the links are not directly adjacent to each other, but
  ;; separated by a space
  (let* ((left-or-right (org-cursor-move-next-to-link)))
    (if (eq left-or-right 'left)
        (save-excursion
          (insert " "))
      (if (eq left-or-right 'left)
          (insert " "))))

  (push-mark)
  (let* ((uuid (cs-get-uuid))
         (uuid-str-len (length uuid))
         (uuid-preview-substring (substring uuid
                                            (- uuid-str-len 2)
                                            uuid-str-len)))
    ;; (insert (concat "[[t:" uuid "][t:" uuid-preview-substring "]]"))
    ;; insert source at current position
    (insert (concat "[[s:" uuid "][s:" uuid-preview-substring
                    "]]"))
    (save-buffer)
    (let* ((string-to-insert (concat "[[t:" uuid "][t:" uuid-preview-substring
                                     "]]" " ")))
      (if (equal arg '(4))
          (let* ()
            ;; insert target marker in target file
            (find-file (concat (cs-get-uuid)
                               ".org")))
        (let* ()
          (end-of-buffer)
          (insert "\n\n")))
      (insert string-to-insert)
      (save-buffer)))
  ;; disable god mode -> be ready to insert with cursor

  (if (boundp 'god-local-mode)
      (god-local-mode -1)))

(require 'helm-ag)

(defun cs-helm-follow-if-only-one-candidate ()
  (interactive)
  ;; (helm-execute-selection-action)
  ;; (helm-exit-and-execute-action)
  (when (equal 1 (helm-get-candidate-number))
    (progn
      (helm-execute-selection-action)
      (helm-exit-minibuffer))))

(defun cs-crux-goto-target (&optional link-content)
  "given a source, go to the target (there should be only one target)"
  (push-mark)
  (save-buffer)
  (let* ((grep-find-ignored-files (-flatten (list "*.org#" grep-find-ignored-files)))
         ;; (helm-execute-action-at-once-if-one t)
         (helm-do-ag--sentinel-finished-function 'cs-helm-follow-if-only-one-candidate)
         )
    (helm-do-ag default-directory nil (concat "\\[t:" link-content))))

(defun cs-crux-target-show-sources (&optional link-content)
  "given a target, show all the sources"
  (push-mark)
  (save-buffer)
  (let* ((grep-find-ignored-files (-flatten (list "*.org#" grep-find-ignored-files)))
         ;; (helm-follow-mode-persistent t)
         ;; (helm-execute-action-at-once-if-one t)
         (helm-do-ag--sentinel-finished-function 'cs-helm-follow-if-only-one-candidate)
         (helm-ag--actions (helm-make-actions "Ope file"
                                              (lambda (candidate)
                                                (interactive)
                                                ;; (message ;; (prin1-to-string
                                                ;;  ;;  (list candidate
                                                ;;  ;;        (let* ((str candidate)
                                                ;;  ;;               (filename-substr (st))))
                                                ;;  ;;        helm-ag--default-directory)) (let* ((source-string (buffer-substring-no-properties (point-min)
                                                ;;  ;; (point-max)
                                                ;;  )
                                                (let* ((source-string candidate)
                                                       (string-match-start-pos (string-match "\\(.*\.\\):\\([0-9]+?\\):" source-string))
                                                       (filename (match-string-no-properties 1 source-string))
                                                       (filepath (concat helm-ag--default-directory filename))
                                                       (line (string-to-number (match-string-no-properties 2 source-string))))
                                                  ;; (message filepath)
                                                  (find-file-existing filepath)
                                                  ;; enable god mode -> dont have to use ctrl
                                                  (if (boundp 'god-local-mode)
                                                      (god-local-mode 1))
                                                  ;; (message line)
                                                  (goto-line line)
                                                  (re-search-forward (concat "\\[s:" link-content))))
                                              "Open file other window"
                                              #'helm-ag--action-find-file-other-window
                                              "Save results in buffer"
                                              #'helm-ag--action-save-buffer
                                              "Edit search results"
                                              #'helm-ag--edit))
         ;; (helm-ag-show-status-function '(lambda ()
         ;;                                 (interactive)
         ;;                                 (error "this is wrong")
         ;;                                 ;; (helm-ag-show-status-default-mode-line)
         ;;                                 ;; (prin1-to-string (length (helm-marked-candidates)))
         ;;                                 ))
         )
    (helm-do-ag default-directory
                nil
                (concat "\\[s:" link-content))))

(defun org-delete-link ()
  ""
  (interactive)
  (let ((elem (org-element-context)))
    (if (eq (car elem) 'link)
        (let* ((content-begin (org-element-property :contents-begin elem))
               (content-end  (org-element-property :contents-end elem))
               (link-begin (org-element-property :begin elem))
               (link-end (org-element-property :end elem)))
          (if (and content-begin content-end)
              (let ((content (buffer-substring-no-properties content-begin content-end)))
                (delete-region link-begin link-end)
                ;; (insert content)
                ))))))

(defun org-cursor-on-link ()
  "check if cursor is on link"
  (interactive)
  (let ((elem (org-element-context)))
    (if (eq (car elem) 'link)
        (let* ((content-begin (org-element-property :contents-begin elem))
               (content-end  (org-element-property :contents-end elem))
               (link-begin (org-element-property :begin elem))
               (link-end (org-element-property :end elem)))
          (if (and content-begin content-end)
              (let ((content (buffer-substring-no-properties content-begin content-end)))
                (when (and (< (point) link-end)
                           (> (point) link-begin))
                  (list link-begin link-end))))))))

(defun org-cursor-move-next-to-link ()
  "check if cursor is on link, if it is, move it to the start/end"
  (interactive)
  (let* (result link-begin
                link-end)
    (setq result (org-cursor-on-link))
    (when result
      (setq link-begin (nth 0 result))
      (setq link-end (nth 1 result))
      (if (< (abs (- link-begin (point)))
             (abs (- link-end (point))))
          (progn
            (goto-char link-begin)
            'left)
        (progn
          (goto-char (- link-end 1))
          'right)))))


(org-link-set-parameters "s"
                         :follow #'cs-crux-goto-target
                         :face '(:foreground "green" :underline nil))

(org-link-set-parameters "t"
                         :follow #'cs-crux-target-show-sources
                         :face '(:foreground "yellow" :underline nil))

(define-key org-mode-map (kbd "C-, k") 'org-delete-link)
(define-key org-mode-map (kbd "C-, C-k") 'org-delete-link)
(define-key org-mode-map (kbd "C-, s") 'create-node)
(define-key org-mode-map (kbd "C-, C-d") 'create-node)



(provide 'cs-crux)
;;; cs-crux.el ends here
