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
;; inserts an import statement into the suitable position, regardless
;; of where you are now.
;;
;; The insertion position for the package "MyApp::Foo" is roughly
;; determined as follows :
;;
;; 1. Find lexicographically suitable positions (that is, when the
;; import statement is inserted there, it will be ordered
;; lexicographically)
;;
;;   : use AAA;
;;   : <<< suitable ("AAA" < "MyApp::Foo" < "XXX")
;;   : use XXX;
;;   : use MyApp::AAA;
;;   : <<< suitable too ("MyApp::AAA" < "MyApp::Foo" < "MyApp::XXX")
;;   : use MyApp::XXX;
;;
;; 2. Among these "suitable position"s, find the most suitable one as
;; follows: A is more suitable than B if A has longer common substring
;; than B with "MyApp::Foo".
;;
;;   : use AAA
;;   : <<< "AAA" does not match "MyApp::Foo" at all
;;   : use XXX
;;   : use MyApp::AAA;
;;   : <<< "MyApp::AAA" has a common part: "MyApp::"
;;   : use MyApp::XXX;
;;
;;  * Usage
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

(defcustom include-anywhere-enable-auto-yank t
  "When non-nil, the symbol before point is inserted as a
placeholder."
  :group 'include-anywhere
  :type 'boolean)

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

(defface include-anywhere-face
  '((((background light)) (:background "#e0d9de" :foreground "#444444"))
    (t (:background "#594854" :foreground "#aaaaaa")))
  "Face used to highlight import statmeents to be inserted."
  :group 'include-anywhere)

(defcustom include-anywhere-map
  (let ((keymap (copy-keymap minibuffer-local-map)))
    (define-key keymap (kbd "C-w") 'include-anywhere-yank-symbol)
    keymap)
  "Map used while entering package name."
  :group 'include-anywhere
  :type 'sexp)

;; + Utility Fns

(defun include-anywhere--make-include-stmt (packagename &optional face)
  "Make and return an include statement for package PACKAGENAME,
according to the current buffer's major-mode. When FACE is
specified, the returned string will be propertized with FACE."
  (let ((pair (or (assoc-default major-mode include-anywhere-alist)
                  (error "No syntax defined for the language."))))
    (concat (if (bolp) "" "\n")
            (propertize (concat (car pair) packagename (cdr pair)) 'face face)
            (if (bolp) "\n" ""))))

(defun include-anywhere--string<= (str1 str2)
  "Return non-nil if either (string= STR1 STR2) or (string< STR1
STR2)."
  (let ((cmp (compare-strings str1 nil nil str2 nil nil)))
    (or (not (numberp cmp)) (< cmp 0))))

(defun include-anywhere--find-insertion-point (packagename)
  "Find point to insert include statement for PACKAGENAME in the
current buffer and move cursor there."
  (let* ((pair (or (assoc-default major-mode include-anywhere-alist)
                   (error "No syntax defined for the language.")))
         (regex (concat "^" (regexp-quote (car pair)) "\\(\\_<.+\\_>\\).*$"))
         last-match-beg last-match-end last-match
         current-match-beg current-match-end current-match candidates)
    (goto-char (point-min))
    (while (forward-comment 1)) ; skip comments at the beginning of buffer
    (while (search-forward-regexp regex nil t)
      (setq last-match-beg    current-match-beg
            last-match-end    current-match-end
            last-match        current-match
            current-match-beg (match-beginning 0)
            current-match-end (match-end 0)
            current-match     (match-string 1))
      (when (include-anywhere--string<= packagename current-match)
        (cond ((null last-match)        ; [BOF] hoge < Y
               (push (cons (length (fill-common-string-prefix packagename current-match))
                           current-match-beg)
                     candidates))
              ((include-anywhere--string<= last-match packagename) ; X < hoge < Y
               (let ((last (length (fill-common-string-prefix packagename last-match)))
                     (current (length (fill-common-string-prefix packagename current-match))))
                 (if (>= last current)
                     (push (cons last last-match-end) candidates)
                   (push (cons current current-match-beg) candidates))))
              ((include-anywhere--string<= current-match last-match) ; X > hoge < Y
               (push (cons (length (fill-common-string-prefix packagename current-match))
                           current-match-beg)
                     candidates)))))
    (cond ((null candidates)            ; BOF, if no matches
           (push (cons 0 (point)) candidates))
          ((include-anywhere--string<= current-match packagename) ; X < hoge [EOF]
           (push (cons (length (fill-common-string-prefix packagename current-match))
                       current-match-end)
                 candidates)))
    (setq candidates (sort candidates (lambda (a b) (if (= (car a) (car b)) (> (cdr a) (cdr b)) (> (car a) (car b))))))
    (goto-char (cdar candidates))))

;; + User Interface

(defvar include-anywhere--window  nil)
(defvar include-anywhere--overlay nil)
(defvar include-anywhere--pos     nil)

(defun include-anywhere--maybe-delete-overlay ()
  (when include-anywhere--overlay
    (delete-overlay include-anywhere--overlay)
    (setq include-anywhere--overlay nil)))

(defun include-anywhere--post-command-hook ()
  "Make an preview overlay according to the current minibuffer
input, and delete the older one."
  (let ((pair (assoc-default major-mode include-anywhere-alist))
        (query (minibuffer-contents)))
    (include-anywhere--maybe-delete-overlay)
    (with-selected-window include-anywhere--window
      (include-anywhere--find-insertion-point query)
      (setq include-anywhere--overlay (make-overlay (point) (point)))
      (let ((str (include-anywhere--make-include-stmt query 'include-anywhere-face)))
        (overlay-put include-anywhere--overlay 'before-string str)))))

(defun include-anywhere-yank-symbol ()
  "Yank symbol just before the point."
  (interactive)
  (insert
   (with-selected-window include-anywhere--window
     (goto-char include-anywhere--pos)
     (if (looking-back "\\_<\\(.*\\)\\_>.*") (match-string 1) ""))))

(defun include-anywhere ()
  "insert import statements from anywhere."
  (interactive)
  (setq include-anywhere--window (selected-window)
        include-anywhere--pos    (point))
  (minibuffer-with-setup-hook
      (lambda ()
        (add-hook 'post-command-hook 'include-anywhere--post-command-hook nil t)
        (when include-anywhere-enable-auto-yank (include-anywhere-yank-symbol)))
    (unwind-protect
        (save-excursion
          (save-restriction
            (let ((pair (assoc-default major-mode include-anywhere-alist))
                  (packagename (read-from-minibuffer "Import module : " nil include-anywhere-map)))
              (widen)
              (include-anywhere--find-insertion-point packagename)
              (insert (include-anywhere--make-include-stmt packagename))
              (include-anywhere--maybe-delete-overlay)
              (sit-for 0.2))))
      (include-anywhere--maybe-delete-overlay))))

(provide 'include-anywhere)

;;; include-anywhere.el ends here
