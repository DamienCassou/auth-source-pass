;;; auth-password-store-tests.el --- Tests for auth-password-store.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Damien Cassou

;; Author: Damien Cassou <damien.cassou@gmail.com>

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

;; Tests for auth-password-store.el

;;; Code:

(require 'ert)

(require 'auth-password-store)

(eval-when-compile (require 'cl-macs))

(ert-deftest parse-simple ()
  (let ((content "pass\nkey1:val1\nkey2:val2\n"))
    (should (equal (auth-pass--parse-data content)
                   '(("key1" . "val1")
                     ("key2" . "val2"))))))

(ert-deftest parse-with-dash-line ()
  (let ((content "pass\n--\nkey1:val1\nkey2:val2\n"))
    (should (equal (auth-pass--parse-data content)
                   '(("key1" . "val1")
                     ("key2" . "val2"))))))

(ert-deftest parse-with-trailing-spaces ()
  (let ((content "pass\n--\nkey1 :val1   \nkey2:   val2\n\n"))
    (should (equal (auth-pass--parse-data content)
                   '(("key1" . "val1")
                     ("key2" . "val2"))))))

(defmacro auth-pass-deftest (name arglist store &rest body)
  "Define a new ert-test NAME with ARGLIST using STORE as password-store.
BODY is a sequence of instructions that will be evaluated.

This macro overrides `auth-pass-parse-entry' and `password-store-list' to
test code without touching the filesystem."
  (declare (indent 3))
  `(ert-deftest ,name ,arglist
     (cl-letf (((symbol-function 'auth-pass-parse-entry) (lambda (entry) (cdr (cl-find entry ,store :key #'car :test #'string=))) )
               ((symbol-function 'password-store-list) (lambda () (mapcar #'car ,store))))
       ,@body)))

(auth-pass-deftest find-match-matching-at-url ()
                   '(("foo" ("url" . "value")))
  (should (equal (auth-pass--find-match "value" nil)
                 "foo")))

(auth-pass-deftest find-match-matching-at-entry-name ()
                   '(("foo" ("url" . "value")))
  (should (equal (auth-pass--find-match "foo" nil)
                 "foo")))

(auth-pass-deftest find-match-matching-at-entry-name-part ()
                   '(("foo" ("url" . "value")))
  (should (equal (auth-pass--find-match "https://foo" nil)
                 "foo")))

(auth-pass-deftest find-match-matching-at-entry-name-ignoring-user ()
                   '(("foo" ("url" . "value")))
  (should (equal (auth-pass--find-match "https://SomeUser@foo" nil)
                 "foo")))

(auth-pass-deftest find-match-matching-at-entry-name-with-user ()
                   '(("SomeUser@foo" ("url" . "value")))
  (should (equal (auth-pass--find-match "https://SomeUser@foo" nil)
                 "SomeUser@foo")))

(auth-pass-deftest find-match-matching-at-entry-name-prefer-full ()
                   '(("SomeUser@foo" ("url" . "value")) ("foo" ("url" . "value")))
  (should (equal (auth-pass--find-match "https://SomeUser@foo" nil)
                 "SomeUser@foo")))

;; same as previous one except the store is in another order
(auth-pass-deftest find-match-matching-at-entry-name-prefer-full-reversed ()
                   '(("foo" ("url" . "value")) ("SomeUser@foo" ("url" . "value")))
  (should (equal (auth-pass--find-match "https://SomeUser@foo" nil)
                 "SomeUser@foo")))

(auth-pass-deftest find-match-matching-at-entry-name-without-subdomain ()
                   '(("bar.com" ("url" . "value")))
  (should (equal (auth-pass--find-match "foo.bar.com" nil)
                 "bar.com")))

(auth-pass-deftest find-match-matching-at-entry-name-without-subdomain-prefer-full ()
                   '(("bar.com" ("url" . "value")) ("foo.bar.com" ("url" . "value")))
  (should (equal (auth-pass--find-match "foo.bar.com" nil)
                 "foo.bar.com")))

(ert-deftest hostname ()
  (should (equal (auth-pass--hostname "https://foo.bar") "foo.bar"))
  (should (equal (auth-pass--hostname "http://foo.bar") "foo.bar"))
  (should (equal (auth-pass--hostname "https://SomeUser@foo.bar") "foo.bar")))

(ert-deftest hostname-with-user ()
  (should (equal (auth-pass--hostname-with-user "https://foo.bar") "foo.bar"))
  (should (equal (auth-pass--hostname-with-user "http://foo.bar") "foo.bar"))
  (should (equal (auth-pass--hostname-with-user "https://SomeUser@foo.bar") "SomeUser@foo.bar")))

(provide 'auth-password-store-tests)

;;; auth-password-store-tests.el ends here
