#!/bin/bash

#########################
# Перезапускает сервер. #
#########################

cd "$HOME/Realm Of Caek"
source "skriptyBash/pomoshchniki/funktsiiSyervyera.sh"

if syervyerLiZhiv > "/dev/null"; then
    echo "перезапуститесь" > "skriptyBash/vryemyennyye/трубаВводаОтСкриптов"

else
    echo "vruchnuyu_pyeryezapustit_syervyer: Серер нет" >&2
    exit 1
fi
