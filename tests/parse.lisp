;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-htsql-tests; -*-

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

(in-package #:cl-htsql-tests)

(in-suite cl-htsql)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro is-htsql-result (query result )
    `(is (equal ',result (parse-htsql-query ,query)))))

(def-test empty ()
  (signals htsql-parse-error (parse-htsql-query "")))

(def-test skip ()
  (is-htsql-result "/" (:SKIP))
  (is-htsql-result "//" (:COLLECT (:SKIP)))
  (is-htsql-result "(/)" (:GROUP (:SKIP)))
  (is-htsql-result "(//)" (:GROUP (:COLLECT (:SKIP)))))

(def-test literals ()
  (is-htsql-result "school" (:IDENTIFIER "school"))
  (is-htsql-result "'school'" (:STRING "school"))
  (is-htsql-result "1" (:INTEGER "1"))
  (is-htsql-result "1.2" (:DECIMAL "1.2"))
  (is-htsql-result "1e2" (:FLOAT "1e2"))
  (is-htsql-result "1.2e3" (:FLOAT "1.2e3")))

(def-test collect ()
  (is-htsql-result "/program" (:COLLECT (:IDENTIFIER "program")))
  (is-htsql-result "/'school'" (:COLLECT (:STRING "school")))
  (is-htsql-result "/5" (:COLLECT (:INTEGER "5"))))

(def-test group ()
  (is-htsql-result "(x)" (:GROUP (:IDENTIFIER "x")))
  (is-htsql-result "((x))" (:GROUP (:GROUP (:IDENTIFIER "x"))))
  (is-htsql-result "/(x)" (:COLLECT (:GROUP (:IDENTIFIER "x")))))

(def-test compose ()
  (is-htsql-result "x.y" (:COMPOSE (:IDENTIFIER "x") (:IDENTIFIER "y")))
  (is-htsql-result "x.y.z" (:COMPOSE (:COMPOSE (:IDENTIFIER "x") (:IDENTIFIER "y")) (:IDENTIFIER "z")))
  (is-htsql-result "/x.y" (:COLLECT (:COMPOSE (:IDENTIFIER "x") (:IDENTIFIER "y")))))

(def-test or ()
  (is-htsql-result "x|y" (:OPERATOR |\|| (:IDENTIFIER "x") (:IDENTIFIER "y")))
  (is-htsql-result "/x|y" (:COLLECT (:OPERATOR |\|| (:IDENTIFIER "x") (:IDENTIFIER "y"))))
  (is-htsql-result "x|y|z" (:OPERATOR |\|| (:OPERATOR |\|| (:IDENTIFIER "x") (:IDENTIFIER "y")) (:IDENTIFIER "z"))))

(def-test and ()
  (is-htsql-result "x&y" (:OPERATOR & (:IDENTIFIER "x") (:IDENTIFIER "y")))
  (is-htsql-result "/x&y" (:COLLECT (:OPERATOR & (:IDENTIFIER "x") (:IDENTIFIER "y"))))
  (is-htsql-result "x&y&z" (:OPERATOR & (:OPERATOR & (:IDENTIFIER "x") (:IDENTIFIER "y")) (:IDENTIFIER "z"))))

(def-test and/or ()
  (is-htsql-result "x&y|z" (:OPERATOR |\|| (:OPERATOR & (:IDENTIFIER "x") (:IDENTIFIER "y")) (:IDENTIFIER "z")))
  (is-htsql-result "x|y&z" (:OPERATOR |\|| (:IDENTIFIER "x") (:OPERATOR & (:IDENTIFIER "y") (:IDENTIFIER "z"))))
  (is-htsql-result "x|y&z|a" (:OPERATOR |\|| (:OPERATOR |\|| (:IDENTIFIER "x") (:OPERATOR & (:IDENTIFIER "y") (:IDENTIFIER "z"))) (:IDENTIFIER "a")))
  (is-htsql-result "x&y|z&a"(:OPERATOR |\|| (:OPERATOR & (:IDENTIFIER "x") (:IDENTIFIER "y")) (:OPERATOR & (:IDENTIFIER "z") (:IDENTIFIER "a"))) ))

(def-test detach ()
  (signals htsql-parse-error (parse-htsql-query "@"))
  (is-htsql-result "@x" (:DETACH (:IDENTIFIER "x")))
  (is-htsql-result "@x.y" (:COMPOSE (:DETACH (:IDENTIFIER "x")) (:IDENTIFIER "y")))
  (is-htsql-result "@x.y.z" (:COMPOSE (:COMPOSE (:DETACH (:IDENTIFIER "x")) (:IDENTIFIER "y")) (:IDENTIFIER "z"))))

#+(or)
(def-test pipe ()
  (is-htsql-result
   "/hue/:csv(asdf, 42)"
   (:PIPE
    (:IDENTIFIER "csv")
    (:COLLECT (:IDENTIFIER "hue"))
    (:IDENTIFIER "asdf")
    (:INTEGER "42"))))
