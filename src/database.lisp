;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-htsql; -*-

;; Copyright (c) 2015, Olof-Joachim Frahm
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

(in-package #:cl-htsql)

(defun make-object-query (type htsql-query)
  (let ((query [select type]))
    (setf (slot-value query 'clsql-sys::exp)
          (list :from (slot-value htsql-query 'clsql-sys::from)
                :where (slot-value htsql-query 'clsql-sys::where)))
    query))

(defclass database-schema ()
  ((tables :initarg :tables)
   (foreign-keys :initarg :foreign-keys)))

(defun fetch-schema (database)
  (make-instance
   'database-schema
   :tables (clsql:list-tables :database database)
   :foreign-keys '(("department_school_fk" "department" "school" ("school_code") ("code")))))

(defun find-table-join (schema table1 table2)
  (let ((rotate (string< table2 table1)))
    (when rotate
      (rotatef table1 table2))
    (dolist (foreign-key (slot-value schema 'foreign-keys))
      (destructuring-bind (name fk-table1 fk-table2 key1 key2) foreign-key
        (when (and (equal table1 fk-table1)
                   (equal table2 fk-table2))
          (let ((result (list (car key1) (car key2))))
            (return (if rotate (rotate result) result))))))))
