#!/bin/bash
# erl -setcookie dlisp -pa ./repl.erl -run repl main -run init stop -noshell
function getip { ip addr show ${1} | grep inet | head -1 \
    | grep -oP 'inet\K\s(\d+\.\d+\.\d+\.\d+)' | cut -d" " -f2 ;}
IP=$(getip ens192)
erl -noshell -eval "repl:main([${1}])" -name "m@${IP}" -setcookie dlisp
