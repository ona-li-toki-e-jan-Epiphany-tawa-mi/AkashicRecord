#!/bin/bash

#######################################
# Скрипт для ручного запуска сервера. #
#######################################

cd "$HOME/Realm Of Caek"
source "skriptyBash/pomoshchniki/funktsiiSyervyera.sh"

if ! syervyerLiZhiv > "/dev/null"; then
    screen -dm -S realmOfCaek bash skriptyBash/avtomachyeski_nachat_syervyer.sh

else
    echo "vruchnuyu_nachat_syervyer: Сервер уже начат" >&2
    exit 1
fi
