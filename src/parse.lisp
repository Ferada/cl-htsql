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

(define-string-lexer string-lexer
  ("/" (return (values '/ '/)))
  ("\\|" (return (values '\| '\|)))
  ("\\&" (return (values '& '&)))
  ("=" (return (values '= '=)))
  ("\\?" (return (values '? '?)))
  ("~" (return (values '~ '~)))
  ("@" (return (values '@ '@)))
  ("\\." (return (values '\. '\.)))
  ("\\(" (return (values '\( '\()))
  ("\\)" (return (values '\) '\))))
  ("-?0|[1-9][0-9]*(\\.[0-9]*)?([e|E][+-]?[0-9]+)?" (return (values 'number $@)))
  ("([^\"\\.\\?~\'=\\(\\)@\\|\\&/])+" (return (values 'name $@)))
  ("\'([^\\\']|\\.)*?\'" (return (values 'string (string-trim "\'" $@))))
  ("\"([^\\\"]|\\.)*?\"" (return (values 'string (string-trim "\"" $@)))))

;; parser are results are to be treated immutable
(define-parser *expression-parser*
  (:start-symbol flow)
  (:terminals (|\|| & |.| ? / = ~ \( \) @ name number string))
  (:precedence ((:left @) (:left |.|) (:left &) (:left |\||) (:left ?) (:left /)))

  (flow
   segment
   skip
   group
   sieve
   or
   and
   composition
   detach
   identifier
   (string (lambda (x) `(:string ,x)))
   (number (lambda (x) `(:integer ,x))))

  (segment
   (/ flow (lambda (x y) (declare (ignore x)) `(:collect ,y))))

  (skip
   (/ (constantly '(:skip))))

  (group
   (\( flow \) (lambda (x y z) (declare (ignore x z)) `(:group ,y))))

  (sieve
   (flow ? flow (lambda (x y z) (declare (ignore y)) `(:filter ,x ,z))))

  (or
   (flow |\|| flow (lambda (x y z) `(:operator ,y ,x ,z))))

  (and
   (flow & flow (lambda (x y z) `(:operator ,y ,x ,z))))

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
   (number (lambda (x) `(:integer ,x)))))

(defun make-lexer-for-source (source)
  (etypecase source
    (string (string-lexer source))
    (stream
     (flet ((ignore (c)
              (declare (ignore c))))
       (stream-lexer #'read-line #'string-lexer #'ignore #'ignore)))))

(defun lex-source (source)
  (let ((lexer (make-lexer-for-source source)))
    (loop
      for (x y) = (multiple-value-list (funcall lexer))
      while x
      collect (list x y))))

(defun parse-htsql-query (source)
  (parse-with-lexer
   (make-lexer-for-source source)
   *expression-parser*))
