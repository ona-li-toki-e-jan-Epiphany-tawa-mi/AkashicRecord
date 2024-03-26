#!/bin/bash

#######################################################################################################
# Разговоривает одном с сервером и осуществляет контроль взаимодействий между ним и другими скритами. #
#######################################################################################################

source "skriptyBash/pomoshchniki/funktsiiSyervyera.sh"

# I/O труби сервера.
trubaVvodaSyervyera="skriptyBash/vryemyennyye/трубаВводаСервера"
trubaVyvodaSyervyera="skriptyBash/vryemyennyye/трубаВыводаСервера"

# Ввод от других скриптов.
trubaVvodaOtSkriptov="skriptyBash/vryemyennyye/трубаВводаОтСкриптов"
if [ ! -e $trubaVvodaOtSkriptov ]; then mkfifo $trubaVvodaOtSkriptov; fi
trap "rm $trubaVvodaOtSkriptov" EXIT

ryezhimSyervyera="выключен"
vryemyaNachalaSyervyera=$(date +"%s")



# Получает сообщения с сервера.
poluchitSoobshchyeniyaSSyervyera() {
    while read -t 0.001 -a vyvodSyervyera <>$trubaVyvodaSyervyera; do
	case "${vyvodSyervyera[0]}" in
	    "режимСервера")
		ryezhimSyervyera="${vyvodSyervyera[1]}"
	    ;;

	    "времяНачалаСервера")
		vryemyaNachalaSyervyera="${vyvodSyervyera[1]}"
	    ;;
	esac
    done
}


# Перезапускает сервер правилно.
#
# Блокирует выполнение до перезапуска.
pyeryezapustitSyervyer() {
    if [ "$ryezhimSyervyera" = "перезапускается" ]; then return; fi

    while [ "$ryezhimSyervyera" = "начинает" ]; do
	sleep 0.5
	poluchitSoobshchyeniyaSSyervyera
    done

    if [ "$ryezhimSyervyera" = "включен" ]; then
	screen -X -S realmOfCaek -p 0 stuff "stop^M"

	while [ "$ryezhimSyervyera" = "включен" ]; do
	    sleep 0.1

	    poluchitSoobshchyeniyaSSyervyera
	done
    fi
}

# Ответит на запросы о данных сервера.
#
# Типы запросов:
# 	времяСервера - Возвращает время в секундах от начала сервера.
#
# @параметер $1 Имя запроса.
# @параметер $2 Именованная труба, в которую отправить ответ.
otvyetitNaZapros() {
    case "$1" in
	"времяСервера")
	    echo "$(( $(date +'%s') - $vryemyaNachalaSyervyera ))" > "$2"
	;;
    esac
}



# Синхронизирует с сервером.
echo "Скрипт запуски готов" > $trubaVvodaSyervyera
read < $trubaVyvodaSyervyera
echo "Все готовы. Давайте начнем" > $trubaVvodaSyervyera

while syervyerLiZhiv > "/dev/null"; do
    poluchitSoobshchyeniyaSSyervyera

    # Читает ввод от других скриптов.
    #
    # Возможные команды:
    # 	перезапуститесь - Перезапускает сервер.
    # 		Блокирует выполнение скрипт запуска до конца.
    #	остановите - Остановливает сервер.
    # 	запросите - Запрашивает сервер за данные и возвращает их в данную именованную трубу.
    #		Структура - запросите <тип запроса> <труба для ответа>
    #               Смотрите на функцию otvyetitNaZapros для получения подробнее информации о типах запросов.
    # 	напишитеСерверу - Написывает сообщение или комманды серверу Майнкрафта.
    #		Структура - напишитеСерверу <сообщение или комманды>
    #		На данный момент пробелам в <сообщение или комманды> нужны "\" позади них.
    while read -t 0.001 -a vvodSkriptov <>$trubaVvodaOtSkriptov; do
	case "${vvodSkriptov[0]}" in
	    "перезапуститесь")
		pyeryezapustitSyervyer

		while [ "$ryezhimSyervyera" = "перезапускается" ]; do
		    poluchitSoobshchyeniyaSSyervyera
		    sleep 1
		done
	    ;;

	    "остановите")
		pyeryezapustitSyervyer
		screen -X -S realmOfCaek -p 0 stuff "^C"
		
		break 2
	    ;;

	    "запросите")
		otvyetitNaZapros "${vvodSkriptov[1]}" "${vvodSkriptov[2]}"
	    ;;

	    "напишитеСерверу")
		screen -X -S realmOfCaek -p 0 stuff "${vvodSkriptov[1]}^M"
	    ;;
	esac
    done

    sleep 5
done
