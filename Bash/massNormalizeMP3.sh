#!/bin/bash

##
# Normalizes MP3 files en mass.
# You will need to have sox and libsox-fmt-all (only if your version of sox dosent come with mp3 support) installed on your system.
#
# Usage:
#     massNormalizeMP3.sh INPUT_FOLDER OUTPUT_FOLDER
#
# If nothing is specified the input folder is the current directory and the output file is out, located in the current directory.
##

sourceDirectory="${1:-.}"
outputDirectory="${2:-out}"
if [ ! -e "$outputDirectory" ]; then mkdir "$outputDirectory"; fi


echo "Starting normalization. Outputting to $outputDirectory"

oldIFS="$IFS"
trap "IFS='$oldIFS'" EXIT
IFS="\n"

for mp3File in $sourceDirectory/*.mp3; do
    normalizedMp3="$outputDirectory/${mp3File##*/}"
    
    if [ ! -e "$normalizedMp3" ]; then
	echo "Normalizing $normalizedMp3"
	sox --norm "$mp3File" "$normalizedMp3" && echo "Finished normalizing $normalizedMp3" &

    else
	echo "Skipping $normalizedMp3; File already exists"
    fi
done

wait
echo "Finished normalization."

IFS="$oldIFS"
