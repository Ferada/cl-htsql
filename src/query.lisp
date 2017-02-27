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

(clsql:file-enable-sql-reader-syntax)

(defun traverse-query (function query)
  (labels ((aux (query &aux (car (car query)) (cdr (cdr query)))
             (prog1 (funcall function query)
               (symbol-macrolet ((cadr (car cdr))
                                 (caddr (cadr cdr))
                                 (cadddr (caddr cdr)))
                 (ecase car
                   (:collect
                    (aux cadr))
                   (:compose
                    (aux cadr)
                    (aux caddr))
                   (:locate
                    (aux cadr)
                    (aux caddr))
                   (:group
                    (aux cadr))
                   (:filter
                    (aux cadr))
                   (:function
                    (mapc #'aux (cdr cdr)))
                   ((:identity :identifier :integer)))))))
    (aux query)))

(defun collect-query-tables (query)
  (with-collector (tables)
    (flet ((aux (query)
             (when (eq (car query) :identifier)
               (tables query))))
      (traverse-query #'aux query))
    (tables)))

(defun collect-query-filters (query)
  (with-collector (filters)
    (flet ((aux (query)
             (when (eq (car query) :filter)
               (filters query))))
      (traverse-query #'aux query))
    (filters)))

(defun find-table-name (filter)
  (ecase (car filter)
    (:filter (find-table-name (cadr filter)))
    (:identifier filter)
    (:compose (find-table-name (caddr filter)))))

(defun simplify-query (query)
  (labels ((aux (query &aux (car (car query)) (cdr (cdr query)))
             (symbol-macrolet ((cadr (car cdr))
                               (caddr (cadr cdr)))
               (ecase car
                 (:collect
                  `(,car ,(aux cadr)))
                 (:compose
                  `(,car ,(aux cadr) ,(aux caddr)))
                 (:locate
                  `(,car ,(aux cadr) ,(aux caddr)))
                 ;; drop :GROUP because it's only syntactically useful
                 (:group
                  (aux cadr))
                 (:filter
                  `(,car ,(aux cadr) ,(aux caddr)))
                 (:function
                  `(,car ,cadr ,@(mapcar #'aux (cdr cdr))))
                 ((:identity :identifier :integer :operator :skip)
                  query)))))
    (aux query)))

(defun transform-operator (table clause)
  (ecase (car clause)
    (:operator
     (let ((operator (cadr clause)))
       (list
        (clsql:sql-operation
         (ecase operator
           (~ 'like)
           ((= < > <= >=) operator)
           (|\|| 'or)
           (& 'and))
         (ecase (car (caddr clause))
           ((:identifier :string)
            (clsql:sql-expression :table table :attribute (cadr (caddr clause))))
           ((:operator :function)
            (car (transform-operator table (caddr clause)))))
         (ecase (car (cadddr clause))
           ((:identifier :integer :string)
            (ecase operator
              (~ (format NIL "%~A%" (cadr (cadddr clause))))
              ;; TODO: e.g. PARSE-INTEGER
              ((= |\|| & < > <= >=) (cadr (cadddr clause)))))
           ((:operator :function)
            (car (transform-operator table (cadddr clause)))))))))
    (:function
     (let ((function (cadr clause)))
       (ecase (car function)
         (:identifier
          (cond
            ((string= (cadr function) "is_null")
             (list
              (clsql:sql-operation
               'is
               (ecase (car (caddr clause))
                 ((:identifier)
                  (clsql:sql-expression :table table :attribute (cadr (caddr clause)))))
               [null])))
            (T (error "Unknown function call to ~A." function)))))))))

(defun transform-query (schema query)
  (when (eq (car query) :skip)
    (return-from transform-query))
  (let (path-tables)
    (flet ((record-path (path)
             (pushnew path path-tables :test #'equal)))
      (let* ((tables (collect-query-tables query))
             (filters (collect-query-filters query))
             (where (apply
                     #'clsql:sql-operation
                     'and
                     (append
                      (mapcan (lambda (table1 table2)
                                (let* ((name1 (cadr table1))
                                       (name2 (cadr table2))
                                       (path (find-table-path schema name1 name2 (list NIL))))
                                  (unless path
                                    (error "Couldn't find path between ~A and ~A." name1 name2))
                                  (mapcan (lambda (join &aux (cdr (cdr join)))
                                            (record-path (car cdr))
                                            (record-path (cadr cdr))
                                            (mapcar
                                             (lambda (attribute1 attribute2)
                                               (clsql:sql-operation
                                                '=
                                                (clsql:sql-expression :table (car cdr)
                                                                      :attribute attribute1)
                                                (clsql:sql-expression :table (cadr (cdr join))
                                                                      :attribute attribute2)))
                                             (caddr cdr)
                                             (cadddr cdr)))
                                          path)))
                              tables (cdr tables))
                      (mapcan (lambda (filter)
                                (transform-operator (cadr (find-table-name filter)) (caddr filter)))
                              filters)))))
        (record-path (cadr (car tables)))
        (apply
         #'clsql:sql-operation
         'select
         ;; TODO get some output format where we can retrieve and merge all the components
         (clsql:sql-expression :alias (cadr (car (last tables))) :attribute '*)
         :from (mapcar (lambda (table)
                         (clsql:sql-expression :table table))
                       (nreverse path-tables))
         :where where
         NIL)))))

;; TODO: how to do indirection on the kind of database used?
;; TODO: limit/offset needs to get direct syntax
(defun map-query (function query &key (database clsql:*default-database*))
  (let ((schema (fetch-schema database)))
    ;; TODO: needs indirection to execute top non-SQL loop
    (clsql:map-query NIL function (transform-query schema query) :database database))
  (values))
