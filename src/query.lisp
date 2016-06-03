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

(defun traverse-query (function query)
  (labels ((aux (query &aux (car (car query)) (cdr (cdr query)))
             (prog1 (funcall function query)
               (symbol-macrolet ((cadr (car cdr))
                                 (caddr (cadr cdr)))
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
                   ((:identity :identifier)))))))
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
                 (:group
                  (aux cadr))
                 (:filter
                  `(,car ,(aux cadr) ,(aux caddr)))
                 ((:identity :identifier :operator :skip)
                  query)))))
    (aux query)))

(defun transform-htsql-query (schema query &key (limit 100))
  (if (eq (car query) :skip)
      NIL
      (let* ((tables (collect-query-tables query))
             (filters (collect-query-filters query))
             path-tables
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
                                  (mapcar (lambda (join)
                                            (pushnew (car (cdr join)) path-tables :test #'equal)
                                            (pushnew (cadr (cdr join)) path-tables :test #'equal)
                                            (clsql:sql-operation
                                             '=
                                             (clsql:sql-expression :table (car (cdr join))
                                                                   :attribute (car (caddr (cdr join))))
                                             (clsql:sql-expression :table (cadr (cdr join))
                                                                   :attribute (car (cadddr (cdr join))))))
                                          path)))
                              tables (cdr tables))
                      (labels ((transform-operator (table clause)
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
                                        (clsql:sql-expression :table table
                                                              :attribute (cadr (caddr clause))))
                                       (:operator
                                        (car (transform-operator table (caddr clause)))))
                                     (ecase (car (cadddr clause))
                                       ((:identifier :integer :string)
                                        (ecase operator
                                          (~ (format NIL "%~A%" (cadr (cadddr clause))))
                                          ((= |\|| & < > <= >=) (cadr (cadddr clause)))))
                                       (:operator
                                        (car (transform-operator table (cadddr clause))))))))))
                        (mapcan (lambda (filter)
                                  (transform-operator (cadr (find-table-name filter)) (caddr filter)))
                                filters))))))
        (pushnew (cadr (car tables)) path-tables :test #'equal)
        (apply
         #'clsql:sql-operation
         'select
         ;; TODO get some output format where we can retrieve and merge all the components
         (clsql:sql-expression :alias (cadr (car (last tables))) :attribute '*)
         :from (mapcar (lambda (table)
                         (clsql:sql-expression :table table))
                       (nreverse path-tables))
         :where where
         (append
          (and limit (list :limit limit))))))
  )
