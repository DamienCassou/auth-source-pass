;;; auth-password-store.el --- Integrate auth-source with password-store -*- lexical-binding: t -*-

;; Copyright (C) 2015 Damien Cassou & Nicolas Petton

;; Author: Damien Cassou <damien@cassou.me>,
;;         Nicolas Petton <nicolas@petton.fr>
;; Version: 0.1
;; GIT: https://github.com/DamienCassou/auth-password-store
;; Package-Requires: ((emacs "24.4") (password-store "0.1") (seq "1.9") (cl-lib "0.5"))
;; Created: 07 Jun 2015
;; Keywords: pass password-store auth-source username password login

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Integrate Emacs' auth-source with password-store

;;; Code:
(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'cl-macs)
(require 'auth-source)
(require 'password-store)

(cl-defun auth-pass-search (&rest spec
                                  &key backend type host user port
                                  &allow-other-keys)
  "Given a property list SPEC, return search matches from the :backend.
See `auth-source-search' for details on SPEC."
  (cl-assert (or (null type) (eq type (oref backend type)))
             t "Invalid password-store search: %s %s")
  (when (listp host)
    ;; Take the first non-nil item of the list of hosts
    (setq host (seq-find #'identity host)))
  (let ((entry (auth-pass--find-match host user)))
    (when entry
      (list (list
             :host host
             :port (or port (auth-pass-get "port" entry))
             :user (auth-pass-get "user" entry)
             :secret (lambda () (auth-pass-get 'secret entry)))))))

;;;###autoload
(defun auth-pass-enable ()
  "Enable auth-password-store."
  ;; To add password-store to the list of sources, evaluate the following:
  (add-to-list 'auth-sources 'password-store)
  ;; clear the cache (required after each change to #'auth-pass-search)
  (auth-source-forget-all-cached))

(defvar auth-pass-backend
  (auth-source-backend "password-store"
                       :source "." ;; not used
                       :type 'password-store
                       :search-function #'auth-pass-search)
  "Auth-source backend for password-store.")

(defun auth-pass-backend-parse (entry)
  "Create a password-store auth-source backend from ENTRY."
  (when (eq entry 'password-store)
    (auth-source-backend-parse-parameters entry auth-pass-backend)))

(advice-add 'auth-source-backend-parse :before-until #'auth-pass-backend-parse)


(defun auth-pass-get (key entry)
  "Return the value associated to KEY in the password-store entry ENTRY.

ENTRY is the name of a password-store entry.
The key used to retrieve the password is the symbol `secret'.

The convention used as the format for a password-store file is
the following (see http://www.passwordstore.org/#organization):

secret
key1: value1
key2: value2"
  (let ((data (auth-pass-parse-entry entry)))
    (or (cdr (assoc key data))
        (and (string= key "user")
             (cdr (assoc "username" data))))))

(defun auth-pass-parse-entry (entry)
  "Return an alist of the data associated with ENTRY.

ENTRY is the name of a password-store entry."
  (let ((file-contents (ignore-errors (password-store--run-show entry))))
    (and file-contents
         (cons `(secret . ,(auth-pass--parse-secret file-contents))
               (auth-pass--parse-data file-contents)))))

(defun auth-pass--parse-secret (contents)
    "Parse the password-store data in the string CONTENTS and return its secret.
The secret is the first line of CONTENTS."
  (car (split-string contents "\\\n" t)))

(defun auth-pass--parse-data (contents)
  "Parse the password-store data in the string CONTENTS and return an alist.
CONTENTS is the contents of a password-store formatted file."
  (let ((lines (split-string contents "\\\n" t "\\\s")))
    (seq-remove #'null
                (mapcar (lambda (line)
                          (let ((pair (mapcar #'string-trim
                                              (split-string line ":"))))
                            (when (> (length pair) 1)
                              (cons (car pair)
                                    (mapconcat #'identity (cdr pair) ":")))))
                        (cdr lines)))))

(defun auth-pass--find-match (host user)
  "Return a password-store entry name matching HOST and USER.
If many matches are found, return the first one.  If no match is
found, return nil."
  (let ((entries (seq-filter (lambda (entry)
                               (string-match host entry))
                             (password-store-list))))
    (seq-find (lambda (entry)
                  (or (null user)
                      (string= user (auth-pass-get "user" entry))))
                entries)))

(provide 'auth-password-store)
;;; auth-password-store.el ends here
