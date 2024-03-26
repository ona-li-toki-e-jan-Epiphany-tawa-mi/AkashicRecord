#!/bin/bash

# TODO Добави, чтобы скрипт запуска разделят ввод и вывод как правилная команда.
# TODO Создай скрипт для обновления сервера, который запустит между полными перезапусками в avtomachyeski_pyeryezapustit_syervyer.sh.

#####################################
# Начинает сервер и скрипт запуска. #
#####################################

# Уровень каталога, что и сервер. Это здесь из-за того, что параметр WorkingDirectory в сервисном файле был сучкой.
cd "$HOME/Realm Of Caek"

# I/O труби для коммуникации между сервером и zapuskat_syervyer.sh.
trubaVvodaSyervyera="skriptyBash/vryemyennyye/трубаВводаСервера"
trubaVyvodaSyervyera="skriptyBash/vryemyennyye/трубаВыводаСервера"
if [ ! -e $trubaVvodaSyervyera ]; then mkfifo $trubaVvodaSyervyera; fi
if [ ! -e $trubaVyvodaSyervyera ]; then mkfifo $trubaVyvodaSyervyera; fi

trap "rm $trubaVvodaSyervyera $trubaVyvodaSyervyera" EXIT


# Начинает zapuskat_syervyer.sh и синхронизирует с ним.
bash "skriptyBash/zapuskat_syervyer.sh" &
read < $trubaVvodaSyervyera
echo "Сервер готов" > $trubaVyvodaSyervyera
read < $trubaVvodaSyervyera

# Начинает и перезапускает сервер. Также говорит скрипту запуска, что делает.
while true; do
    echo "режимСервера начинает" > $trubaVyvodaSyervyera
    echo "времяНачалаСервера $(date +'%s')" > $trubaVyvodaSyervyera
    sleep 3 && echo "режимСервера включен" > $trubaVyvodaSyervyera &

    LD_LIBRARY_PATH=. ./bedrock_server

    echo "режимСервера перезапускается" > $trubaVyvodaSyervyera
    echo "[Сервис Перезапуска] Сервер перезапустится через 10 секунд. Нажмите CTRL + C, чтобы отменить. Нажмите на любую кнопку, чтобы пропустить."
    read -n 1 -t 10
done
