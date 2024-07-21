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

;; Add the following to your Emacs init file:
;;
;; (require 'helm-bm) ;; Not necessary if using ELPA package
;; (global-set-key (kbd "C-c b") 'helm-bm)

;; That's all.

;;; Code:

(require 'bm)
(require 'cl-lib)
(require 'helm)
(require 'compile) ;; compilation-info-face, compilation-line-face

(defgroup helm-bm nil
  "Bookmarks of bm.el related Applications and libraries for Helm."
  :prefix "helm-bm-" :group 'helm)

(defface helm-bm-annotation-face nil
  "Face used for annotation."
  :group 'helm-bm)

(defconst helm-bm-action-name-edit-annotation "Edit annotation")

(defun helm-bm-bookmark-at-line (bufname lineno)
  "Return bookmark in BUFNAME at LINENO."
  (with-current-buffer bufname
    (let ((p (save-restriction
               (goto-char (point-min))
               (forward-line (1- lineno))
               (point))))
      (bm-bookmark-at p))))

(defun helm-bm-action-bookmark-edit-annotation (candidate)
  "Edit bookmark annotation of CANDIDATE."
  (when (string-match "^\\(.+?\\):\\([0-9]+\\):\\(.*\\)$" candidate)
    (let* ((bufname (match-string 1 candidate))
           (lineno (string-to-number (match-string 2 candidate)))
           (bm (helm-bm-bookmark-at-line bufname lineno))
           (annotation (read-string
                        (format "%s: " helm-bm-action-name-edit-annotation)
                        (overlay-get bm 'annotation))))
      (bm-bookmark-annotate bm annotation))))

(defun helm-bm-action-switch-to-buffer (candidate)
  "Switch to buffer of CANDIDATE."
  (let ((pos (overlay-get candidate 'position)))
    (when pos (goto-char pos))))

(defun helm-bm-action-remove-marked-bookmarks (_candidate)
  "Remove marked bookmarks."
  (mapc 'bm-bookmark-remove (helm-marked-candidates)))

(defun helm-bm-bookmarks-in-buffer (buf)
  "Gets a list of bookmarks in BUF, which can be a string or a buffer."
  (with-current-buffer buf
    (helm-flatten-list (bm-lists))))

(defun helm-bm-buffer-name (bm)
  "Return the name of BUFFER with BM."
  (buffer-name (overlay-buffer bm)))

(defun helm-bm< (bm1 bm2)
  "Return t if BM1 is less than BM2 in lexicographic order.
Case is significant.
Symbols are also allowed; their print names are used instead."
  (let ((current-buf (buffer-name (current-buffer)))
        (bm1-name (helm-bm-buffer-name bm1))
        (bm2-name (helm-bm-buffer-name bm2)))
    (if (string-equal bm1-name bm2-name)
        (< (overlay-start bm1) (overlay-start bm2))
      (cond ((string-equal current-buf bm1-name) t)
            ((string-equal current-buf bm1-name) nil)
            (:else (string< bm1-name bm2-name))))))

(defun helm-bm-candidate-transformer-display
    (bufname lineno content annotation)
  "Return a string displayed in helm buffer.

BUFNAME, LINENO, CONTENT and ANNOTATION are concatenated to the string."
  (format "%s:%s:%s%s"
          (propertize bufname 'face compilation-info-face)
          (propertize lineno 'face compilation-line-face)
          content
          (if (or (null annotation) (string= annotation ""))
              ""
            (concat "\n  "
                    (propertize annotation 'face
                                'helm-bm-annotation-face)))))

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
    :candidates (lambda () (helm-bm-bookmarks-in-buffer helm-current-buffer))
    :candidate-transformer
    (lambda (candidates)
      (cl-loop for ov in candidates
               collect (cons (helm-bm-transform-to-candicate ov) ov)))
    :action '(("Jump to BM" . helm-bm-action-switch-to-buffer)
              ("Remove(s)" . helm-bm-action-remove-marked-bookmarks)
              ("Edit annotation"
               . helm-bm-action-bookmark-edit-annotation)
              ("Remove all bookmarks in current buffer"
               . (lambda (_c) (bm-remove-all-current-buffer)))
              ("Remove all bookmarks in all buffers"
               . (lambda (_c) (bm-remove-all-all-buffers))))))

;;;###autoload
(defun helm-bm ()
  "Show bookmarks of bm.el with `helm'."
  (interactive)
  (helm :sources '(helm-source-bm)
        :quit-if-no-candidate (lambda ()
                                (message "No BM candidates in this buffer"))
        :buffer "*helm bm*"))

(provide 'helm-bm)

;; Local Variables:
;; coding: utf-8
;; End:

;;; helm-bm.el ends here
