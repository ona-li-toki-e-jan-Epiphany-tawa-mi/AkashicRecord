#!/bin/bash

##############################################################################
# Обновляет сервисы сервера в /etc/systemd/system/ сервисными файлами здесь. #
##############################################################################

cd "/home/lotus/Realm Of Caek/skriptyBash/rootDir/etc/systemd/system"

successes=0

sudo cp nachatRealmOfCaek.service /etc/systemd/system/
if [ $? -eq 0 ]; then successes=$(( $successes + 1 )); fi
sudo cp ostanovitRealmOfCaek.service /etc/systemd/system/
if [ $? -eq 0 ]; then success=$(( $successes + 1 )); fi

if [ $successes -gt 0 ]; then
    sudo systemctl daemon-reload
fi

