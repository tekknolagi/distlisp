#!/bin/bash
erl -setcookie dlisp -pa ./repl.erl -run repl main -run init stop -noshell
