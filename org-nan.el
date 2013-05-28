
;;; org-nan.el --- Narrowing to multiple subtrees for Org-mode entries
;;
;; Copyright (C) 2012 Gregor Kappler
;;
;; Author: Gregor Kappler <g dot kappler at gmx dot net>
;; Keywords: outlines, hypermedia
;; Homepage: http://g-kappler.de/orgmode/org-nan.html
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; org-nan is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Commentary:  TODO   
;; This org module provides functions to use many different narrowed
;; buffers for each file that are created when
;; - following org links
;; - open occur matches
;; Narrowed buffers can be
;; - widened to parent of the first visible heading 
;; - narrowed to first child of first visible heading that is ancestor of
;;   point
  
  ;;; Code:
  
  (require 'org)

(defvar org-nan-indirect-mode-map (make-sparse-keymap)
    "Keymap for `org-nan-mode', a minor mode.
    Use this map to set additional keybindings for when Org-mode is used
    for a capture buffer.")
  
  (define-minor-mode org-nan-indirect-mode
    "Minor mode for special key bindings in a clocked buffer."
    nil " NaN" org-nan-indirect-mode-map
;;    (org-nan-update)
    (org-set-local 'org-nan-top-id (save-excursion
                              (org-id-get (point-min) t nil)))
    (when org-nan-top-id
      (widen)
      (goto-char (point-min))
      (when (re-search-forward (format "^ *:ID: *%s" org-nan-top-id) nil t)
        (org-nan-narrow-to-subtree)
        (setq desktop-save-buffer t)
        )
      ))

(defun org-nan-indirect-hook ()
  "defines keybindings."
  (local-set-key (kbd "s-a")               'org-nan-up)
  (local-set-key (kbd "s-y")               'org-nan-down-to-point)
  (local-set-key (kbd "C-s-y")             'org-nan-create-indirect))
(add-hook 'org-mode-hook 'org-nan-indirect-hook)

;;; Customization

(defgroup org-id nil
  "Options concerning global entry identifiers in Org-mode."
  :tag "Org ID"
  :group 'org)

(define-obsolete-variable-alias
  'org-link-to-org-use-id 'org-id-link-to-org-use-id "24.3")

(defun org-nan-narrow-to-subtree ()
  "also sets header line"
  (interactive)
  (org-narrow-to-subtree)
  (org-nan-update)
  )

(require 'org-capture)

(defvar org-nan-create-hook nil
  "Hook run after creating new narrowed buffer.")

(defun org-nan-create-indirect (&optional pos)
  "Create new indirect narrowed buffer.
    An indirect buffer is shown where you can edit part of your text tree."
  ;; TODO: Pressing \\[org-nan-finalize] brings you back to the previous state of Emacs, so that you can continue your work.
  (interactive "d")
  
  (let* ((pos (or pos (point)))
         (narrowheadpos (save-excursion
                          (org-back-to-heading t) (point)))
         (orig-buf (current-buffer)))
    (org-capture-set-plist '()) ; entry
    (org-capture-put :original-buffer orig-buf
                     :original-file (or (buffer-file-name orig-buf)
                                        (and (featurep 'dired)
                                             (car (rassq orig-buf
                                                         dired-buffers))))
                     :original-file-nondirectory
                     (and (buffer-file-name orig-buf)
                          (file-name-nondirectory
                           (buffer-file-name orig-buf)))
                     :pos pos)
    (org-capture-put :default-time
                     (or org-overriding-default-time
                         (org-current-time)))
    (org-nan-indirect-place pos)
    (org-set-startup-visibility)
    (goto-char pos)
    (org-nan-reveal)
    (org-nan-update)
    (org-indent-refresh-maybe (point-min) (point-max) nil)
    (run-hooks 'org-nan-create-hook)
    (current-buffer)
    ))

(defun org-nan-indirect-place (&optional pos)
  "Display the appropriate narrowed buffer."
  (org-capture-put :return-to-wconf (current-window-configuration))
  (widen)

  (if pos (goto-char pos))
(org-nan-reveal t)
(org-nan-reveal-src-block)
  (switch-to-buffer
   (org-capture-get-indirect-buffer nil "NarrInd"))
  (if pos (goto-char pos))
  (org-narrow-to-subtree)
  (org-set-local 'org-capture-target-marker
                 (move-marker (make-marker) (point)))
  (org-nan-indirect-mode 1)
  (org-set-local 'org-capture-current-plist org-capture-plist)
  (org-nan-update)
  (current-buffer)
  )

(defun org-nan-buffer (nbuffname narrowheadpos)
  ""
  (let ((myfile (buffer-file-name)))
    (with-current-buffer
        (clone-indirect-buffer nbuffname nil)
      (set 'buffer-file-name myfile)
                                        ; (set-auto-mode) ; in files.el
                                        ; (set 'buffer-file-name nil) doesn't appear necessary.
      (goto-char narrowheadpos)
      (org-nan-narrow-to-subtree)
      )
    )
  )

;;;###autoload
(defun org-nan-id-goto (id)
  "Switch to the buffer containing the entry with id ID.
          Move the cursor to that entry in that buffer."
  (interactive "sID: ")
  (let ((m (org-id-find id 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    (org-nan-pop-to-marker m 1)
    ))

(defun org-nan-clock-goto (p)
  "go to clocked buffer"
  (interactive "d")
  (org-clock-goto)
  (org-nan-pop-to-marker)
  )

(defun org-nan-forward-to-target (p target)
  "Retrieves position forward to target position in same buffer when climbing headlines (parents, children, and siblings paths)."
  (interactive "d")
  (condition-case ex
      (progn
        (outline-back-to-heading t)
        (cond
         ((> p target)
          (cond
           ((> (outline-level) 2)
            (progn
              (goto-char p)
              (if (= (point) p)
                  (outline-up-heading 1 t)
                )))
           ((let ((lastpos p))
              (goto-char target)
              (org-back-to-heading t)
              (while (and
                      (< 2 (outline-level)))
                (outline-up-heading 1 t)
                )))))
         ((let ((lastpos p))
               (goto-char target)
               (org-back-to-heading t)
               (forward-line 0)
               ;; (message "p %s point %s target %s" p (point) target)
               (while (and
                       (< p (point))
                       (< 2 (outline-level)))
                 (setq lastpos (point))
                 (outline-up-heading 1 t)
                 ;; (message "level %s -- p %s point %s target %s" (outline-level) p (point) target )
                 )
               ;; (message "level %s -- p %s point %s lastpos %s target %s" (outline-level) p (point) lastpos target )
               (cond
                ((= (point) lastpos) (goto-char target))
                ;; ((= 2 (outline-level)) (point))
                (t (goto-char lastpos)))
               (point)))))
    ('error (goto-char target)
            (org-back-to-heading t)
            (if (> (org-outline-level) 1)
                (outline-up-heading (org-outline-level) t))
            (point)
            )))

(defun org-nan-down-to-point (pos)
  "Narrows the buffer one headline forward in the line of parents of p."
  (interactive "d")

  (let* ((headpos
          (save-excursion
            (goto-char (point-min))
            (org-nan-forward-to-target (point) pos)
            (point)))
         (heading (save-excursion (goto-char headpos) (org-get-heading t t))))
    (switch-to-buffer
     (if (buffer-file-name) ;; no indirect buffer: create one
         (org-nan-create-indirect headpos)
       (goto-char headpos)
       (org-nan-narrow-to-subtree)
       (current-buffer)
       ))

    (goto-char pos)
    (org-nan-reveal)
    (org-nan-update)
    ;;    (message "Narrowed: %s %s %s" (point) pos heading))
    ;; show top if possible
        (unless (= (point-at-bol) headpos) (org-nan-display-beginning-point (point)))
    ))

(defun org-nan-up (p)
  "Narrows the buffer one headline forward in the line of parents of p."
  (interactive "d")
  (save-excursion
    (goto-char (point-min))
    (widen)
    (when (and (not (org-before-first-heading-p)) (org-up-heading-safe))
      (org-nan-reveal) 
      (org-nan-narrow-to-subtree)
      )
    (org-nan-update)

    ;; collapse siblings
    (save-excursion
      (goto-char (point-min))
      (org-nan-forward-to-target (point) p)
      (while (org-goto-sibling t)
        (org-flag-subtree t)
        (org-flag-heading nil))

      (org-nan-forward-to-target (point) p)
      (while (org-goto-sibling)
        (org-flag-subtree t)
        (org-flag-heading nil))
      )

    ;; show top if possible
    (org-nan-display-beginning-point (point))
    ))

(defun org-nan-update-header ()
  ""
  (interactive)
  (org-set-local
   'header-line-format
   (list (buffer-name))
   ))

(defun org-nan-update ()
  (interactive)
  (org-nan-update-header)
  (org-nan-indirect-rename-buffer)
  (if (derived-mode-p 'org-nan-indirect-mode)
  (org-set-local 'org-nan-top-id (save-excursion
                            (org-id-get (point-min) t nil))))
  (org-set-local 'outline-level 'org-outline-level)
  )

(defun org-nan-indirect-rename-buffer ()
    "Rename buffer according to filename and first visible heading.
Consider calling org-narrow-to-subtree' first."
    (if (buffer-base-buffer)
        (save-excursion
          (let* ((p (point))
                 (head (or (save-excursion
                             (goto-char (point-min))
                             (if (org-at-heading-p) (org-get-heading t nil) nil))
                           "<no head>"))
                 (bufname (concat
                           "# "
                           (file-name-sans-extension (file-name-nondirectory
                            (or (buffer-file-name)
                                (buffer-file-name (buffer-base-buffer)))))

                           (concat
                            " | "
                            (replace-regexp-in-string
                             org-any-link-re "\\3"
                             head) "")
                           ""
                           )))
            ;; (message "rename buffer %s -> %s" (buffer-name) bufname)
            (if (get-buffer bufname)
                (progn
                  (unless (string-equal (buffer-name) bufname)
                    (message "further instance of buffer %s" bufname)
                    ;; (kill-buffer)
                    ;; (switch-to-buffer bufname)
                    ;;(save-restriction (widen) (goto-char p))
                    )))
            (if (buffer-file-name)
                (rename-buffer (file-name-nondirectory (buffer-file-name)) t)
              (rename-buffer bufname t))
            ))))

(defun org-nan-display-beginning-point (p &optional zoom)
  "Try to make point as well as beginning of buffer visible in window.
    If zoom is not nil, set zoom level such that start of buffer and point are visible."
  (interactive "d")
  ;;    (message "point %s %s" (point) p)
  (require 'face-remap)
  (text-scale-mode 1)
  (if (and zoom (eq last-command 'gk-clocked) org-nan-text-scale-mode-amount)
      (progn
        (text-scale-set org-nan-text-scale-mode-amount)
        (setq org-nan-text-scale-mode-amount nil)
        )
    (setq org-nan-text-scale-mode-amount text-scale-mode-amount)
    (save-excursion

      ;; make window display first visible line after top heading 
      ;; This is useful if the top heading is displayed in the emacs header-line
      (goto-char (point-min))
      (forward-line 1)
      (while (org-invisible-p2) (forward-line 1))
      (recenter 0)

      (when zoom
        (while (and (< text-scale-mode-amount 6) (pos-visible-in-window-p p))
          (text-scale-increase 1))
        (while (and (> text-scale-mode-amount -4) (not (pos-visible-in-window-p p)))
          (text-scale-decrease 1)))
      (goto-char p)
      )
    ))

(defun org-in-drawer-p ()
    "Is cursor within in a drawer (between drawer keyword and \":END:\")? Returns point at beginning of drawer or nil."
    (interactive)
    (save-excursion
      (let* (
             (head (save-excursion (org-back-to-heading t) (point)))
             (nexthead (save-excursion (outline-next-heading) (point)))
             (nextdrawend (save-excursion (re-search-forward "^ *:END: *$" nexthead t)))
             (prevdrawbeg (re-search-backward org-drawer-regexp head t))
             (prevdrawend (if prevdrawbeg (re-search-forward "^ *:END: *$" nexthead t))))
        ;;(message "prevdrawbeg %s, prevdrawend %s, nextdrawend %s" prevdrawbeg prevdrawend nextdrawend)
        (when (and prevdrawbeg nextdrawend 
                   (= prevdrawend nextdrawend)) 
          prevdrawbeg)
        )))
  
  (defun org-nan-reveal-src-block ()
    "If block containing point is invisible, show block contents"
    (interactive)
    (let* ((el (org-element-at-point))
           (begin (org-element-property :begin el)))
      (cond 
       ((and (string-equal (car el) "src-block")
             (invisible-p (point)))
        (save-excursion 
          (goto-char begin)
          (org-hide-block-toggle)))
       )))
  
  (defun org-nan-reveal (&optional SIBLINGS) 
    (interactive)
    (org-reveal SIBLINGS)
    (org-nan-reveal-src-block)
(unless (org-before-first-heading-p)
    (let ((draw (org-in-drawer-p))) 
      (when draw (save-excursion (goto-char (if (numberp draw) draw (car draw))) (org-flag-drawer nil))))))
  
  ;; requires emacs 23+
  (defun org-nan-occur-mode-find-occurrence-hook () 
    (when (derived-mode-p 'org-mode) 
      (org-nan-pop-to-marker nil) 
      (org-nan-reveal)
      ))

  (add-hook 'occur-mode-find-occurrence-hook 'org-nan-occur-mode-find-occurrence-hook)

(provide 'org-nan)
