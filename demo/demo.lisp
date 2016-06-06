;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-htsql-demo; -*-

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

(in-package #:cl-htsql-demo)

(defun start (&key (port 8080))
  (clsql:connect (list "htsql_demo.sqlite") :database-type :sqlite3)
  (setf *acceptor* (make-instance 'easy-acceptor :port port))
  (hunchentoot:start *acceptor*))

(define-easy-handler (index :uri "/") (query)
  (with-html-output-to-string (stream)
    (:html
     (:body
      (:h1 "CL-HTSQL DEMO")
      (:h2 "TABLES")
      (let ((schema (cl-htsql::fetch-schema clsql:*default-database*)))
        (cond
          ((not query)
           (htm (:ul
                 (dolist (table (slot-value schema 'cl-htsql::tables))
                   (htm (:li (str table)))))))
          (T
           (htm
            (:p (str query))
            (multiple-value-bind (results titles)
                (clsql:query (cl-htsql::transform-htsql-query schema (cl-htsql::simplify-query (cl-htsql::parse-htsql-query query))))
              (htm
               (:table
                (dolist (title titles)
                  (htm (:th (esc (princ-to-string title)))))
                (dolist (row results)
                  (htm (:tr
                        (dolist (column row)
                          (htm (:td (esc (princ-to-string column)))))))))))))))))))
