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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun maybe-intern (name &optional (package NIL package-p))
    "If NAME is a SYMBOL, return it, otherwise INTERN it."
    (cond
      ((symbolp name)
       name)
      (package-p
       (intern name package))
      (T
       (intern name))))

  (defmacro define-lexer (name &body patterns)
    "Shortcut for DEFINE-STRING-LEXER."
    `(define-string-lexer ,name
       ,@(mapcar
          (lambda (pattern)
            (etypecase pattern
              ((or symbol string)
               (let ((symbol (maybe-intern pattern))
                     (pattern (string pattern)))
                 `(,pattern (return (values ',symbol ',symbol)))))
              (list
               (destructuring-bind (pattern &optional symbol value) pattern
                 (let* ((symbol (or symbol (intern pattern)))
                        (value (or value symbol)))
                   (etypecase symbol
                     (list
                      `(,pattern ,symbol))
                     (symbol
                      `(,pattern (return (values ',symbol ',value))))))))))
          patterns))))

(define-lexer string-lexer
  /
  ("\\|" \|)
  ("\\&" &)
  "="
  ("\\?" ?)
  ~
  @
  ("\\." \.)
  ("\\(" \()
  ("\\)" \))
  ("\\+" +)
  -
  ("\\*" *)
  !

  ("-?0|[1-9][0-9]*(\\.[0-9]*)?([e|E][+-]?[0-9]+)?"
   (return (cond
             ((find #\e $@)
              (values 'float $@))
             ((find #\. $@)
              (values 'decimal $@))
             (T
              (values 'integer $@)))))
  ("([^\"\\.\\?~\'=\\(\\)@\\|\\&/])+" (return (values 'name $@)))
  ("\'([^\\\']|\\.)*?\'" (return (values 'string (string-trim "\'" $@))))
  ("\"([^\\\"]|\\.)*?\"" (return (values 'string (string-trim "\"" $@)))))

;; parser are results are to be treated immutable
(define-parser *expression-parser*
  (:start-symbol query)
  (:terminals (|\|| & ! |.| ? / = ~ \( \) + - * @ name integer decimal float string))
  (:precedence ((:left @) (:left =) (:left ~) (:left |.|) (:left + -) (:left * /) (:left !) (:left &) (:left |\||) (:left ?)))

  (query
   segment)

  (segment
   (/ segment (lambda (x y) (declare (ignore x)) `(:collect ,y)))
   skip
   flow)

  (flow
   group
   sieve
   or
   and
   not
   addition
   multiplication
   condition
   composition
   detach
   identifier
   (string (lambda (x) `(:string ,x)))
   (integer (lambda (x) `(:integer ,x)))
   (decimal (lambda (x) `(:decimal ,x)))
   (float (lambda (x) `(:float ,x))))

  (skip
   (/ (constantly '(:skip))))

  (group
   (\( segment \) (lambda (x y z) (declare (ignore x z)) `(:group ,y))))

  (sieve
   (flow ? flow (lambda (x y z) (declare (ignore y)) `(:filter ,x ,z))))

  (or
   (flow |\|| flow (lambda (x y z) `(:operator ,y ,x ,z))))

  (and
   (flow & flow (lambda (x y z) `(:operator ,y ,x ,z))))

  (not
   (! flow (lambda (x y) `(:prefix ,x ,y))))

  (addition
   (flow + flow (lambda (x y z) `(:operator ,y ,x ,z)))
   (flow - flow (lambda (x y z) `(:operator ,y ,x ,z))))

  (multiplication
   (flow * flow (lambda (x y z) `(:operator ,y ,x ,z)))
   (flow / flow (lambda (x y z) `(:operator ,y ,x ,z))))

  (condition
   (identifier = term (lambda (x y z) `(:operator ,y ,x ,z)))
   (identifier ~ term (lambda (x y z) `(:operator ,y ,x ,z))))

  (composition
   (flow |.| flow (lambda (x y z) (declare (ignore y)) `(:compose ,x ,z))))

  (detach
   (@ flow (lambda (x y) (declare (ignore x)) `(:detach ,y))))

  (identifier
   (name (lambda (x) `(:identifier ,x))))

  (term
   name
   (string (lambda (x) `(:string ,x)))
   (number (lambda (x) `(:integer ,x)))
   (integer (lambda (x) `(:integer ,x)))
   (decimal (lambda (x) `(:decimal ,x)))
   (float (lambda (x) `(:float ,x)))))

(defun make-lexer-for-source (source)
  "Make a lexer for the SOURCE, either a STRING or a STREAM."
  (etypecase source
    (string (string-lexer source))
    (stream
     (flet ((ignore (c)
              (declare (ignore c))))
       (stream-lexer #'read-line #'string-lexer #'ignore #'ignore)))))

(defun lex-source (source)
  "Debug helper to lex a SOURCE into a list of tokens."
  (let ((lexer (make-lexer-for-source source)))
    (loop
      for (x y) = (multiple-value-list (funcall lexer))
      while x
      collect (list x y))))

(define-condition htsql-parse-error (simple-error) ())

(defun translate-yacc-error (error)
  (make-condition
   'htsql-parse-error
   :format-control "Couldn't parse HTSQL query: ~A."
   :format-arguments (list error)))

(defun parse-htsql-query (source)
  "Parse SOURCE into a syntax tree.  The SOURCE may be either a STRING or
a STREAM."
  (handler-case
      (parse-with-lexer
       (make-lexer-for-source source)
       *expression-parser*)
    (yacc-parse-error (error)
      (error (translate-yacc-error error)))))
