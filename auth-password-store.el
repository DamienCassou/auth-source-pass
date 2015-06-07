;;; auth-password-store.el --- Integrate auth-source with password-store -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Damien Cassou & Nicolas Petton

;; Author: Damien Cassou <damien@cassou.me>,
;;         Nicolas Petton <nicolas@petton.fr>
;; Version: 0.1
;; GIT: https://github.com/DamienCassou/auth-password-store
;; Package-Requires: ((emacs "24") (password-store "0.1"))
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
(require 'cl-lib)
(require 'cl-macs)
(require 'auth-source)

(defvar auth-pass-backend
  (auth-source-backend "password-store"
                           :source "."
                           :type 'password-store
                           :search-function #'auth-pass-search)
  "Auth-source backend for password-store.")

;; search password store for a password matching the parameters
(cl-defun auth-pass-search (&rest
                            spec
                            &key backend require create delete
                            type max host user port
                            &allow-other-keys)
  "Given a property list SPEC, return search matches from the :backend.
See `auth-source-search' for details on SPEC."
  (cl-assert (or (null type) (eq type (oref backend type)))
          t "Invalid password-store search: %s %s")

  `(:host ,host :port ,port :user "bfoo" :secret ,(lambda () "the secret in a function")))

;; override this function to make it work (if you evaluate this, you
;; won't be able to use Emacs anymore, we really need to change that
;; into an advice)
(defun auth-source-backend-parse (entry)
  (auth-source-backend-parse-parameters entry
                                        auth-pass-backend))

;; clear the cache (required after each change to #'auth-pass-search)
(auth-source-forget-all-cached)

;; try to search a user and password for given host and port
(setq myauth (auth-source-search :max 1
                                 :host "smtps.univ-lille1.fr"
                                 :port "587"
                                 :require '(:user :secret)))



(provide 'auth-password-store)

;;; auth-password-store.el ends here
