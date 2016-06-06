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

(defun make-object-query (type htsql-query)
  (let ((query [select type]))
    (setf (slot-value query 'clsql-sys::exp)
          (list :from (slot-value htsql-query 'clsql-sys::from)
                :where (slot-value htsql-query 'clsql-sys::where)))
    query))

(defclass database-schema ()
  ((tables :initarg :tables)
   (foreign-keys :initarg :foreign-keys)))

(defun list-foreign-keys (&key (database clsql:*default-database*))
  "Return a LIST of foreign keys, where each key is represented by a LIST
\(NAME TABLE1 TABLE2 (COLUMN1) (COLUMN2))."
  (%list-foreign-keys database))

(defgeneric %list-foreign-keys (database))

(defmethod %list-foreign-keys ((database clsql-sqlite3:sqlite3-database))
  (with-collector (result)
    (dolist (table-name (clsql:list-tables :database database))
      (let (current)
        (clsql:do-query ((id seq table from to on_update on_delete match)
                         (format NIL "PRAGMA foreign_key_list('~A')" table-name))
          (if current
              (if (eql id (car current))
                  (progn
                    (nconcf (sixth current) (list from))
                    (nconcf (seventh current) (list to)))
                  (progn
                    (result current)
                    (setf current (list id seq NIL table-name table (list from) (list to)))))
              (setf current (list id seq NIL table-name table (list from) (list to)))))
        (when current
          (result current))))
    (mapcar #'cddr (result))))

(defmethod %list-foreign-keys ((database clsql-postgresql:postgresql-database))
  (with-collector (result)
    (clsql:do-query ((foreign-key table1 table2 column1 column2)
                     [select [tc constraint_name] [tc table_name] [ccu table_name] [kcu column_name] [ccu column_name]
                             :from '([[information_schema table_constraints] "tc"]
                                     [[information_schema key_column_usage] "kcu"]
                                     [[information_schema constraint_column_usage] "ccu"])
                             :where [and [= [tc constraint_name] [kcu constraint_name]]
                                         [= [tc constraint_name] [ccu constraint_name]]
                                         [= [tc constraint_type] "FOREIGN KEY"]]]
                     :database database)
      (result `(,foreign-key ,table1 ,table2 (,column1) (,column2))))
    (result)))

(defun fetch-schema (database)
  (make-instance
   'database-schema
   :tables (clsql:list-tables :database database)
   :foreign-keys (list-foreign-keys :database database)))

(defun find-table-join (schema table1 table2)
  (dolist (foreign-key (slot-value schema 'foreign-keys))
    ;; (logv:logv foreign-key)
    (destructuring-bind (name fk-table1 fk-table2 key1 key2) foreign-key
      (when (and (equal table1 fk-table1)
                 (equal table2 fk-table2))
        (return (list name key1 key2)))
      (when (and (equal table1 fk-table2)
                 (equal table2 fk-table1))
        (return (list name key2 key1))))))

(defun find-table-path (schema table1 table2 &optional (seen (list NIL)))
  (let ((join (find-table-join schema table1 table2)))
    (if join
        (list (list* (car join) table1 table2 (cdr join)))
        (dolist (foreign-key (slot-value schema 'foreign-keys))
          (destructuring-bind (name fk-table1 fk-table2 key1 key2) foreign-key
            ;; (logv:format-log "~A" foreign-key)
            (when (equal table1 fk-table2)
              (unless (member fk-table2 (car seen) :test #'equal)
                ;; (logv:format-log "~A <-> ~A      ~A <-> ~A" table1 table2 fk-table1 fk-table2)
                (let ((path (find-table-path schema fk-table1 table2 (prog1 seen (push fk-table1 (car seen))))))
                  (when path
                    (return (list* (list name fk-table2 fk-table1 key2 key1) path))))))
            (when (equal table1 fk-table1)
              (unless (member fk-table1 (car seen) :test #'equal)
                ;; (logv:format-log "~A <-> ~A      ~A <-> ~A" table1 table2 fk-table1 fk-table2)
                (let ((path (find-table-path schema fk-table2 table2 (prog1 seen (push fk-table2 (car seen))))))
                  (when path
                    (return (list* (list name fk-table1 fk-table2 key2 key1) path)))))))))))
