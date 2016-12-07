#!/bin/bash
# erl -setcookie dlisp -pa ./repl.erl -run repl main -run init stop -noshell
function getip { ifconfig | grep -Eo 'inet (addr:)?([0-9]*\.){3}[0-9]*' | grep -Eo '([0-9]*\.){3}[0-9]*' | grep -v '127.0.0.1' | head -1 ;}
IP=$(getip)
MODE=${1}
echo "IP is ${IP}"
echo "Mode is ${MODE}"
ledit erl -noshell -eval "repl:main([${1}])" -name "m@${IP}" -setcookie dlisp
