#!/bin/bash

# Запустится с crontab 0 0 * * * /bin/bash /home/lotus/Realm Of Caek/skriptyBash/avtomachyeski_pyeryezapustit_syervyer.sh

###############################################################################
# Автомачески полностью перезапускает сервер каждые 24 часа, если он еще нет. #
###############################################################################

cd "$HOME/Realm Of Caek"
source "skriptyBash/pomoshchniki/funktsiiSyervyera.sh"


vryemyaOtNachalaSyervyera=0

# Запрашивает сервер за время от начала сервера.
# Блокирует выполнение до полечния ответа.
zaprositVryemyaSyervyera() {
    echo "запросите времяСервера $trubaVvodaSkriptaPyeryezapuska" > $trubaVvodaOtSkriptov
    read vryemyaOtNachalaSyervyera < $trubaVvodaSkriptaPyeryezapuska
}

# 24 часа - 5 минут внимания.
vryemyaMyezhduPyeryezapuskami=$(( 86400 - 300 ))

if syervyerLiZhiv > "/dev/null"; then
    trubaVvodaOtSkriptov="skriptyBash/vryemyennyye/трубаВводаОтСкриптов"

    # Временная труба для получения ответов.
    trubaVvodaSkriptaPyeryezapuska="skriptyBash/vryemyennyye/трубаВводаСкриптаПерезапуска"
    if [ ! -e $trubaVvodaSkriptaPyeryezapuska ]; then mkfifo $trubaVvodaSkriptaPyeryezapuska; fi


    zaprositVryemyaSyervyera

    if [ "$vryemyaOtNachalaSyervyera" -gt $vryemyaMyezhduPyeryezapuskami ]; then
	# Дает о перезапуске несколько вниманий за 5 минут.
	# Останавливет перезапуск, елси при этом сервер уже перезапустился или остановил.
	echo "напишитеСерверу say\ Server\ restarting\ in\ 5\ minutes" > $trubaVvodaOtSkriptov
	sleep 4m

	zaprositVryemyaSyervyera
	if [ "$(syervyerLiZhiv)" -eq 1 -a "$vryemyaOtNachalaSyervyera" -lt $vryemyaMyezhduPyeryezapuskami ]; then
	    return
	fi

	echo "напишитеСерверу say\ Server\ restarting\ in\ 1\ minute" > $trubaVvodaOtSkriptov
	sleep 1m

	zaprositVryemyaSyervyera
	if [ "$(syervyerLiZhiv)" -eq 1 -a "$vryemyaOtNachalaSyervyera" -lt $vryemyaMyezhduPyeryezapuskami ]; then
	    return
	fi


	echo "остановите" > $trubaVvodaOtSkriptov
	while syervyerLiZhiv > "/dev/null"; do sleep 0.5; done
	screen -dm -S realmOfCaek bash skriptyBash/avtomachyeski_nachat_syervyer.sh
    fi


    rm $trubaVvodaSkriptaPyeryezapuska
fi
