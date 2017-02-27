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

(defclass demo-acceptor (acceptor) ())

(defun render-graph ()
  (pushnew '(:fontcolor cl-dot::text) cl-dot::*node-attributes*)
  (let* ((cl-dot::*id* 0)
         (schema (cl-htsql::fetch-schema clsql:*default-database*))
         (table (make-hash-table :test 'equal))
         (graph (make-instance 'cl-dot::graph
                               :attributes (list :bgcolor "transparent")
                               :nodes (mapcar (lambda (node)
                                                (setf (gethash node table) (make-instance 'cl-dot::node :attributes (list :label node :color "green" :fontcolor "green"))))
                                              (slot-value schema 'cl-htsql::tables))
                               :edges (mapcar (lambda (edge)
                                                (destructuring-bind (name table1 table2 key1 key2) edge
                                                  (declare (ignore name key1 key2))
                                                  (make-instance 'cl-dot::edge :target (gethash table1 table) :source (gethash table2 table) :attributes (list :color "green" :fontcolor "green"))))
                                              (slot-value schema 'cl-htsql::foreign-keys)))))
    (cl-dot:dot-graph graph "htsql_demo.png" :format :png :directed NIL)))

(defun start (&key (port 8080))
  (clsql:connect (list "htsql_demo.sqlite") :database-type :sqlite3)
  (render-graph)
  (setf *acceptor* (make-instance 'demo-acceptor :port port))
  (hunchentoot:start *acceptor*))

(defun stop ()
  (hunchentoot:stop *acceptor*)
  (setf *acceptor* NIL)
  (clsql:disconnect))

(defmethod acceptor-dispatch-request ((acceptor demo-acceptor) request)
  (let ((query (url-decode (request-uri request))))
    (if (string= query "/htsql_demo.png")
        (handle-static-file "htsql_demo.png")
        (with-html-output-to-string (stream)
          (:html
           (:head
            (:style
             :type "text/css"
             "html, a, a:link, pre { font-family: \"IBM 3270 Narrow\"; background: #000; color: #0f0; } td { border: 1px solid; }"))
           (:body
            (:h1 "CL-HTSQL DEMO")
            (let ((schema (cl-htsql::fetch-schema clsql:*default-database*)))
              (cond
                ((or (string= query "/") (not query))
                 (htm
                  (:h2 "EXAMPLES")
                  (:ol
                   (dolist (query '("/(school?name~Bus).appointment"
                                    "/semester.course"))
                     (htm (:li (:a :href (str query) (str query))))))
                  (:h2 "TABLES")
                  (:ul
                   (dolist (table (slot-value schema 'cl-htsql::tables))
                     (htm (:li (:a :href (format NIL "/~A" table) (str table))))))
                  (:h2 "FOREIGN KEYS")
                  (:ul
                   (dolist (foreign-key (slot-value schema 'cl-htsql::foreign-keys))
                     (htm (:li (str (format NIL "~S" foreign-key))))))
                  (:img :src "/htsql_demo.png")))
                (T
                 (htm
                  (:h2 "RESULTS")
                  (:p (:a :href query (str query)))
                  (handler-case
                      (let* ((parsed (cl-htsql::parse-query query))
                             (simplified (cl-htsql::simplify-query parsed))
                             (transformed (cl-htsql::transform-query schema simplified)))
                        (htm
                         (:h3 "PARSED")
                         (:pre (esc (with-output-to-string (stream) (pprint parsed stream))))
                         (:h3 "SIMPLIFIED")
                         (:pre (esc (with-output-to-string (stream) (pprint simplified stream))))
                         (:h3 "TRANSFORMED")
                         (:pre (esc (with-output-to-string (stream) (pprint transformed stream)))))
                        (multiple-value-bind (results titles)
                            (clsql:query transformed)
                          (htm
                           (:table
                            (dolist (title titles)
                              (htm (:th (esc (princ-to-string title)))))
                            (dolist (row results)
                              (htm (:tr
                                    (dolist (column row)
                                      (htm (:td (esc (princ-to-string column))))))))))))
                    (error (error)
                      (htm (:p (esc (princ-to-string error))))))))))))))))
