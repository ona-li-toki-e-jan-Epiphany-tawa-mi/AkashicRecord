#!/bin/bash

##
# Converts MP3 files to CD-compatible WAVs en mass.
# You will need to have ffmpeg installed on your system.
#
# Usage:
#     massMP3ToCDWAV.sh INPUT_FOLDER OUTPUT_FOLDER
#
# If nothing is specified the input folder is the current directory and the output file is out, located in the current directory.
##

sourceDirectory="${1:-.}"
outputDirectory="${2:-out}"
if [ ! -e "$outputDirectory" ]; then mkdir "$outputDirectory"; fi


echo "Starting conversion to CD-compatible WAV files. Outputting to $outputDirectory"

oldIFS="$IFS"
trap "IFS='$oldIFS'" EXIT
IFS="\n"

for mp3File in $sourceDirectory/*.mp3; do
    asWav="${mp3File%.*}.wav" && asWav="${asWav##*/}"

    if [ ! -e "$outputDirectory/$asWav" ]; then
       echo "Normalizing $asWav"
       ffmpeg -i "$mp3File" -ar 44100 "$outputDirectory/$asWav" && echo "Finished converting $asWav" &
       
    else
	echo "Skipping $asWav; File already exists"
    fi
done

wait
echo "Finished conversion."

IFS="$oldIFS"
