#!/bin/bash

################
# Plays music. #
################

cd "$HOME/Рабочий стол/MUZYKAA"

for song in *.mp4; do
	mpv "$song"
done
