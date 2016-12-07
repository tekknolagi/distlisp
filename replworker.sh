#!/bin/bash
# erl -setcookie dlisp -pa ./repl.erl -run repl main -run init stop -noshell
function getip { ifconfig | grep -Eo 'inet (addr:)?([0-9]*\.){3}[0-9]*' | grep -Eo '([0-9]*\.){3}[0-9]*' | grep -v '127.0.0.1' | head -1 ;}
IP=$(getip)
MASTER="${1}"
PERCSLOW="${2}"
STEALSTATUS="${3}"
echo "IP is ${IP}"
echo "Master IP is ${MASTER}"
echo "Percent slow is ${PERCSLOW}"
echo "Stealing status is ${STEALSTATUS}"
erl -noshell \
    -eval "thread_pool:selfstart('m@${MASTER}', 1, {{slow, ${PERCSLOW}}, ${STEALSTATUS}})." \
    -name "w@${IP}" -setcookie dlisp
