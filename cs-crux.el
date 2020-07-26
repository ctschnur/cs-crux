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
(define-key pdf-view-mode-map (kbd "M-p") (lambda () (interactive)
                                            (let ((current-prefix-arg 4))
                                              (call-interactively 'find-next-file))))

(defun cs-dired-open-file-externally ()
    "In dired, open the filepath named on this line."
    (interactive)
    (let* ((filepath (dired-get-filename nil t)))
      (message "Opening %s..." filepath)
      (cond ((string-equal (file-name-extension filepath) "ipynb")
             (cs-dired-open-notebook filepath))
            (t (call-process "xdg-open" nil 0 nil filepath)))))

(define-key dired-mode-map (kbd "C-c C-o") 'cs-dired-open-file-externally)

(defun cs-move-to-beginning-of-visual-line ()
    "Move to the beginning of the visual line minus the whitespace, then toggle."
    (interactive)
    (let* (pos-first-non-ws-char-in-cur-vis-line)
      (save-excursion
        (beginning-of-visual-line)
        (setq pos-first-non-ws-char-in-cur-vis-line
              (if (string-match "\s" (string (char-after (point))))
                  (search-forward-regexp "\s+"
                                         (save-excursion
                                           (end-of-visual-line)
                                           (point))
                                         t)
                (point))))
      (if (equal pos-first-non-ws-char-in-cur-vis-line (point))
          (beginning-of-visual-line)
        (goto-char pos-first-non-ws-char-in-cur-vis-line))))

(global-set-key (kbd "C-a") #'cs-move-to-beginning-of-visual-line)

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

(global-set-key (kbd "C-x w") 'cs-make-all-writable)

(defun list-packages-and-versions ()
  "Returns a list of all installed packages and their versions"
  (mapcar
   (lambda (pkg)
     `(,pkg ,(package-desc-version
              (cadr (assq pkg package-alist)))))
   package-activated-list))

(defun google-quickly()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

(global-set-key (kbd "C-x C-g") 'google-quickly)


(defun outside-terminal-with-tmux ()
  (interactive)
  (shell-command "gnome-terminal -e 'tmux new' >/dev/null"))

(global-set-key (kbd "C-x C-m C-t") 'outside-terminal-with-tmux)

(defun outside-explorer ()
  (interactive)
  (setq s (concat "nautilus " (file-name-directory buffer-file-name) " & "))
  (message s)
  (call-process-shell-command s nil 0))

(global-set-key (kbd "C-x C-m C-e") 'outside-explorer)  ; open gui file explorer

(defun outside-browser ()
  (interactive)
  (setq s (concat "chromium-browser " (file-name-directory buffer-file-name) " & "))
  (message s)
  (call-process-shell-command s nil 0))

(global-set-key (kbd "C-x C-m C-b") 'outside-browser)  ; open browser at that file

(defun kill-non-visible-buffers ()
  "Kill all buffers not currently shown in a window somewhere."
  (interactive)
  (dolist (buf  (buffer-list))
    (unless (get-buffer-window buf 'visible) (kill-buffer buf))))

(defun new-buffer-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

(global-set-key (kbd "C-c n") #'new-buffer-frame)

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
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(global-set-key (kbd "C-, c f c") 'cs-put-file-name-on-clipboard)


;; ---- open file from clipboard

(defun cs-open-file-from-clipboard ()
  (interactive)
  (find-file
   (helm-read-file-name
    "open filepath from clipboard: "
    :initial-input (with-temp-buffer (yank) (buffer-string)))))

(global-set-key (kbd "C-, c f o") 'cs-open-file-from-clipboard)


;; ---- drag and drop files (as links) from explorer into org-mode -----

(defun my-dnd-func (event)
  (interactive "e")
  (goto-char (nth 1 (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (type (car payload))
         (fname (cadr payload))
         (img-regexp "\\(png\\|jp[e]?g\\)\\>"))
    (cond
     ;; insert image link
     ((and  (eq 'drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; insert image link with caption
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert "#+ATTR_ORG: :width 300\n")
      (insert (concat  "#+CAPTION: " (read-input "Caption: ") "\n"))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; C-drag-n-drop to open a file
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type))
      (find-file fname))
     ((and (eq 'M-drag-n-drop (car event))
           (eq 'file type))
      (insert (format "[[attachfile:%s]]" fname)))
     ;; regular drag and drop on file
     ((eq 'file type)
      (insert (format "[[%s]]\n" fname)))
     (t
      (error "I am not equipped for dnd on %s" payload)))))

(define-key org-mode-map (kbd "<drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<C-drag-n-drop>") 'my-dnd-func)
(define-key org-mode-map (kbd "<M-drag-n-drop>") 'my-dnd-func)

(provide 'cs-crux)
;;; cs-crux.el ends here
