#!/bin/bash

#########################
# Останавливает сервер. #
#########################

cd "$HOME/Realm Of Caek/"
source "skriptyBash/pomoshchniki/funktsiiSyervyera.sh"

if syervyerLiZhiv > "/dev/null"; then
    echo "остановите" > "skriptyBash/vryemyennyye/трубаВводаОтСкриптов"

else
    echo "ostanovit_syervyer: Сервера нет" >&2
    exit 1
fi
