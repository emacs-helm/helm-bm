;;; helm-bm.el --- helm sources for bm.el -*- lexical-binding: t -*-

;; Copyright (C) 2013-2016 Yasuyuki Oka <yasuyk@gmail.com>

;; Author: Yasuyuki Oka <yasuyk@gmail.com>
;; URL: https://github.com/yasuyk/helm-bm
;; Package-Requires: ((bm "1.0") (cl-lib "0.5") (helm "1.9.3"))
;; Version: 0.3
;; Keywords: helm, bookmark

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Installation:

;; Add this file in your `load-path' and the following to your Emacs init file:
;;
;; (autoload 'helm-bm "helm-bm" nil t) ;; Not necessary if using ELPA package
;; (global-set-key (kbd "C-c b") 'helm-bm)

;;; Code:

(require 'bm)
(require 'cl-lib)
(require 'helm)

(defgroup helm-bm nil
  "Bookmarks of bm.el related Applications and libraries for Helm."
  :prefix "helm-bm-" :group 'helm)

(defun helm-bm-action-bookmark-edit-annotation (candidate)
  "Edit bookmark annotation of CANDIDATE."
  (let ((annotation (read-string "Edit annotation: "
                                 (overlay-get candidate 'annotation))))
    (bm-bookmark-annotate candidate annotation)))

(defun helm-bm-action-switch-to-buffer (candidate)
  "Switch to buffer of CANDIDATE."
  (let ((pos (overlay-get candidate 'position))
        (buf (overlay-buffer candidate)))
    (when buf (switch-to-buffer buf))
    (when pos (goto-char pos))))

(defun helm-bm-action-remove-marked-bookmarks (_candidate)
  "Remove marked bookmarks."
  (mapc 'bm-bookmark-remove (helm-marked-candidates)))

(defun helm-bm-bookmarks-in-all-buffers ()
  (cl-loop for buf in (buffer-list)
           append (helm-bm-bookmarks-in-buffer buf)))

(defun helm-bm-bookmarks-in-buffer (buf)
  "Gets a list of bookmarks in BUF, which can be a string or a buffer."
  (with-current-buffer buf
    (helm-fast-remove-dups (helm-flatten-list (bm-lists)) :test 'eql)))

(defun helm-bm-candidate-transformer-display
    (bufname lineno content annotation)
  "Return a string displayed in helm buffer.

BUFNAME, LINENO, CONTENT and ANNOTATION are concatenated to the string."
  (format "%s:%s:%s%s"
          (propertize bufname 'face 'font-lock-type-face)
          (propertize lineno 'face 'font-lock-keyword-face)
          content
          (if (or (null annotation) (string= annotation ""))
              ""
            (concat "\n  "
                    (propertize
                     annotation 'face 'font-lock-keyword-face)))))

(defun helm-bm-transform-to-candicate (bm)
  "Convert a BM to a CANDICATE."
  (let ((current-buf (overlay-buffer bm)))
    (with-current-buffer current-buf
      (let* ((start (overlay-start bm))
             (end (overlay-end bm))
             (bufname (buffer-name current-buf))
             (annotation (overlay-get bm 'annotation))
             (lineno (line-number-at-pos start)))
        (unless (< (- end start) 1)
          (helm-bm-candidate-transformer-display
           bufname (int-to-string lineno)
           (buffer-substring-no-properties start (1- end)) annotation))))))

(defvar helm-source-bm
  (helm-build-sync-source "Visible bookmarks"
    :multiline t
    :candidates (lambda () (if bm-cycle-all-buffers
                               (helm-bm-bookmarks-in-all-buffers)
                             (helm-bm-bookmarks-in-buffer helm-current-buffer)))
    :candidate-transformer
    (lambda (candidates)
      (cl-loop for ov in candidates
               collect (cons (helm-bm-transform-to-candicate ov) ov)))
    :action '(("Jump to BM" . helm-bm-action-switch-to-buffer)
              ("Remove BM bookmark(s)" . helm-bm-action-remove-marked-bookmarks)
              ("Edit annotation"
               . helm-bm-action-bookmark-edit-annotation))))

;;;###autoload
(defun helm-bm ()
  "Show bookmarks of bm.el with `helm' in `current-buffer'."
  (interactive)
  (helm :sources '(helm-source-bm)
        :quit-if-no-candidate (lambda ()
                                (if bm-cycle-all-buffers
                                    (message "No BM candidates found in buffers")
                                  (message "No BM candidates in this buffer")))
        :buffer "*helm bm*"))

(provide 'helm-bm)


;;; helm-bm.el ends here
