#!/bin/bash

# Takes a directory of mkv files and converts them into mp4 files.
# Usage: processMusic.sh <input directory w/ .MKVs> <output directory for .MP4s>

sourceDirectory="${1:-.}"
outputDirectory="${2:-out}"
if [ ! -e "$outputDirectory" ]; then mkdir "$outputDirectory"; fi


echo "Starting conversion from mkv to mp4."

IFSCopy=$IFS
IFS=$(echo -en "\n\b")

for mkvFile in $sourceDirectory/*.mkv; do
    asMp4="${mkvFile%.*}.mp4" && asMp4="${asMp4##*/}"
    
    if [ ! -e "$outputDirectory/$asMp4" ]; then
	ffmpeg -i "$mkvFile" "$outputDirectory/$asMp4" &
    fi
done

IFS=$IFSCopy

wait
echo "Finished conversion."
