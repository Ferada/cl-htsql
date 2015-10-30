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
    (:identifier filter)))

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
                  `(,car (aux cadr)))
                 ((:identity :identifier)
                  query)))))
    (aux query)))

(defun transform-htsql-query (schema query &key (limit 100))
  (let ((tables (collect-query-tables query))
        (filters (collect-query-filters query)))
    (apply
     #'clsql:sql-operation
     'select
     (clsql:sql-expression :alias (cadr (car (last tables))) :attribute '*)
     :from (mapcar (lambda (table)
                     (clsql:sql-expression :table (cadr table)))
                   tables)
     :where (apply
             #'clsql:sql-operation
             'and
             (append
              (mapcar (lambda (table1 table2)
                        (let* ((name1 (cadr table1))
                               (name2 (cadr table2))
                               (join (find-table-join schema name1 name2)))
                          (clsql:sql-operation
                           '=
                           (clsql:sql-expression :table name1
                                                 :attribute (car join))
                           (clsql:sql-expression :table name2
                                                 :attribute (cadr join)))))
                      tables (cdr tables))
              (mapcar (lambda (filter)
                        (let ((operator (cadr (caddr filter))))
                          (clsql:sql-operation
                           (ecase operator
                             (~ 'like)
                             (= '=)
                             (|\|| 'or)
                             (& 'and))
                           (clsql:sql-expression :table (cadr (find-table-name filter))
                                                 :attribute (cadr (caddr (caddr filter))))
                           (ecase operator
                             (~ (format NIL "%~A%" (cadr (cadddr (caddr filter)))))
                             ((= |\|| &) (cadr (cadddr (caddr filter))))))))
                      filters)))
     (append
      (and limit (list :limit limit))))))
