-*- mode: markdown; coding: utf-8-unix; -*-

CL-HTSQL - An implementation of HTSQL.

Copyright (C) 2015 Olof-Joachim Frahm

Release under a Simplified BSD license.

Protoype state.

# SUMMARY

This library firstly provides a *parser* for the HTSQL syntax, with some
features that make it more useful for the Common Lisp environment;
however these can be turned off if one-to-one compatibility is required.
The result of a parse is a syntax tree consisting of S-expressions,
keywords and literals (as strings and/or parsed).

This tree may also be rendered back into string format with a *pretty
printer*.

Secondly, a *translation* of queries to the CLSQL library is provided.
Again, some particular additional features are provided and some
features may be missing.

# USAGE

Use `PARSE-HTSQL-QUERY` to get back a raw syntax tree:

    > (parse-htsql-query "/x.y.z")
    => (:COLLECT
        (:COMPOSE
         (:COMPOSE
          (:IDENTIFIER "x")
          (:IDENTIFIER "y"))
         (:IDENTIFIER "z")))

## CLSQL

Have an open database:

    (clsql:connect (list (truename #P"~/htsql_demo.sqlite")) :database-type :sqlite3 :encoding :utf-8)

(At the time of writing CLSQL is using `NAMESTRING` instead `TRUENAME`
on `PATHNAME` arguments.)

Print query output:

    > (clsql:print-query
       (transform-htsql-query
        (fetch-schema clsql:*default-database*)
        (parse-htsql-query "/department?code='astro'")))
    => astro Astronomy ns
