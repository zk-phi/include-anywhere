;;; include-anywhere.el --- Insert import statements from anywhere

;; Copyright (C) 2017- zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; * Description
;;
;; This package provides `include-anywhere` command, which just
;; inserts an import statement into the "right" position, regardless
;; of where you are now.
;;
;; "Right" position is by default determined by `string<` predicate,
;; and import statements will be ordered lexicographically. (anyway
;; you can customize this behavior).
;;
;; * Usage
;;
;; Load this package in your init.el
;;
;; : (require 'include-anywhere)
;;
;; and call `M-x include-anywhere` in a buffer with supported
;; major-modes listed below :
;;
;; - c-mode, java-mode
;; - lisp-interaction-mode, emacs-lisp-mode
;; - perl-mode, cperl-mode
;;
;; To add support for another major-mode, put something like
;;
;; : (push '(c-mode . ("#include <" . ">")) include-anywhere-alist)
;;
;; into your init.el.

;;; Change Log:

;;; Code:

(defgroup include-anywhere nil
  "Insert import statements from anywhere."
  :group 'emacs)

(defcustom include-anywhere-alist
  '((c-mode . ("#include <" . ">"))     ; WIP: #include "foo.h" syntax ?
    (ruby-mode . ("require '" . "'"))   ; WIP: require_relative ?
    (java-mode . ("import " . ""))
    (ahk-mode . ("#include " . ""))
    (lisp-interaction-mode . ("(require '" . ")"))
    (emacs-lisp-mode . ("(require '" . ")"))
    (perl-mode . ("use " . ";"))
    (prolog-mode . (":- use_module(" . ")"))
    (haskell-mode . ("import " . ""))
    (objc-mode . ("#import <" . ">"))
    (cperl-mode . ("use " . ";")))
  "Alist of the form (MAJOR-MODE . (INCLUDE-PREFIX . INCLUDE-SUFFIX))."
  :group 'include-anywhere
  :type 'sexp)

(defcustom include-anywhere-package< 'string<
  "A function to compare two package names, which defines the
order of import statements."
  :group 'include-anywhere
  :type 'function)

(defface include-anywhere-face
  '((((background light)) (:background "#e0d9de" :foreground "#444444"))
    (t (:background "#594854" :foreground "#aaaaaa")))
  "Face used to highlight import statmeents to be inserted."
  :group 'include-anywhere)

(defvar include-anywhere--window  nil)
(defvar include-anywhere--overlay nil)

(defun include-anywhere--make-regexp (packagename)
  (let ((pair (or (assoc-default major-mode include-anywhere-alist)
                  (error "No syntax defined for the language."))))
    (concat "^" (regexp-quote (car pair)) "\\(\\_<.+\\_>\\).*$")))

(defun include-anywhere--include-stmt (packagename &optional face)
  (let ((pair (or (assoc-default major-mode include-anywhere-alist)
                  (error "No syntax defined for the language."))))
    (concat (if (bobp) "" "\n")
            (propertize (concat (car pair) packagename (cdr pair)) 'face face)
            (if (bobp) "\n" ""))))

(defun include-anywhere--maybe-delete-overlay ()
  (when include-anywhere--overlay
    (delete-overlay include-anywhere--overlay)
    (setq include-anywhere--overlay nil)))

(defun include-anywhere--find-insertion-point (packagename)
  "Find point to insert include statement for PACKAGENAME in the
current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((regex (include-anywhere--make-regexp packagename)) last-match-end)
        (while (and (search-forward-regexp regex nil t)
                    (funcall include-anywhere-package< (match-string 1) packagename))
          (setq last-match-end (match-end 0)))
        (or last-match-end (point-min))))))

(defun include-anywhere--post-command ()
  (include-anywhere--maybe-delete-overlay)
  (let ((pair (assoc-default major-mode include-anywhere-alist))
        (query (minibuffer-contents)))
    (with-selected-window include-anywhere--window
      (goto-char (include-anywhere--find-insertion-point query))
      (setq include-anywhere--overlay (make-overlay (point) (point)))
      (let ((str (include-anywhere--include-stmt query 'include-anywhere-face)))
        (overlay-put include-anywhere--overlay 'before-string str)))))

(defun include-anywhere ()
  "insert import statements from anywhere."
  (interactive)
  (setq include-anywhere--window (selected-window))
  (minibuffer-with-setup-hook
      (lambda () (add-hook 'post-command-hook 'include-anywhere--post-command nil t))
    (unwind-protect
        (save-excursion
          (let ((pair (assoc-default major-mode include-anywhere-alist))
                (packagename (read-from-minibuffer "Import module : ")))
            (goto-char (include-anywhere--find-insertion-point packagename))
            (insert (include-anywhere--include-stmt packagename))
            (include-anywhere--maybe-delete-overlay)
            (sit-for 0.2)))
      (include-anywhere--maybe-delete-overlay))))

(provide 'include-anywhere)

;;; include-anywhere.el ends here
