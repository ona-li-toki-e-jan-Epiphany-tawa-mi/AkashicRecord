#!/bin/bash

fajlDannykh="$HOME/Realm Of Caek/skriptyBash/dannykh"

# Читает значение из файла данных и возвращает значение под данным ключом.
# Вернет ошибку, если либо значения не существует (с кодом ошибки 1), либо grep ошибается (с кодом 2).
#
# @параметер $1 Ключ - имя - значению, которое хочется.
#
# @возвращает Значение под ключом.
pochitatParu() {
	# Ищет ключ.
	rezultaty=`grep -m 1 --color=never "^$1=" "$fajlDannykh"`
	kodOshibki=$?

	if [ $kodOshibki -eq 0 ]
	then
		# Получает значение.
		echo $rezultaty | cut -d'=' -f 2

	# Сообщения ошибок.
	else
		if [ $kodOshibki -eq 1 ]
		then
			echo "ERROR: Не удалось найти '$1' в файле данных" >&2
		fi

		return $kodOshibki
	fi
}

# Записывает значение и ключ в файл данных.
# Вернет ошибку, если grep ошибается (с кодом 1).
#
# @параметер $1 Ключ - имя - значению.
# @параметер $2 Значение под ключом.
napisatParu() {
	# Ищет ключ.
        rezultaty=`grep -n -m 1 --color=never "^$1=" "$fajlDannykh"`
        kodOshibki=$?

	# Изменяет предыдущее значение на новое, если ключ уже есть в файле.
	if [ $kodOshibki -eq 0 ]
	then
		pryedydushchyeyeZnachyeniye=`echo $rezultaty | cut -d'=' -f 2`

		# Изменяет только тогда, когда нужно.
		if [ $pryedydushchyeyeZnachyeniye != "$2" ]
		then
			nomyerStroki=`echo $rezultaty | cut -d':' -f 1`
			sed -i "$nomyerStroki c $1=$2" "$fajlDannykh"
		fi

	# Пишет пары на конце файла, елси их сейчас нет.
	else
		echo "$1=$2" >> "$fajlDannykh"
	fi
}
