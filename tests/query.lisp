;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-htsql-tests; -*-

;; Copyright (c) 2016, Olof-Joachim Frahm
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:cl-htsql-tests)

(in-suite cl-htsql)

(clsql:file-enable-sql-reader-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro is-sql-query (schema query result)
    `(is (equal ',result (clsql-sys::sql-output (cl-htsql::transform-query ,schema (cl-htsql::parse-query ,query)))))))

(def-test skip-sql ()
  (let ((schema (make-instance 'cl-htsql::database-schema)))
    (is-sql-query schema "/" "NULL")))

(def-test entity-sql ()
  (let ((schema (make-instance 'cl-htsql::database-schema)))
    (is-sql-query schema "/foo" "SELECT \"foo\".* FROM \"foo\"")))

(def-test function-call-sql ()
  (let ((schema (make-instance 'cl-htsql::database-schema)))
    (is-sql-query schema "/foo?is_null(x)" "SELECT \"foo\".* FROM \"foo\" WHERE (\"foo\".\"x\" IS NULL)")))

(def-test multiple-attributes-sql ()
  (let ((schema (make-instance 'cl-htsql::database-schema :foreign-keys '((NIL "foo" "bar" ("x" "y") ("x" "y"))))))
    (is-sql-query schema "/foo.bar" "SELECT \"bar\".* FROM \"foo\",\"bar\" WHERE ((\"foo\".\"x\" = \"bar\".\"x\") AND (\"foo\".\"y\" = \"bar\".\"y\"))")))