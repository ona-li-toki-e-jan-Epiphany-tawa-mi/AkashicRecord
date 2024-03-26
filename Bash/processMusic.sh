#!/bin/bash

# Takes a directory of mp4 files and converts them into mp3 and wav files.
# The wav files are also downsampled to 44100kHz for usage on CD. Make sure that your final wav files are also 16-bit stereo if you are planning on burning them. 
# Usage: processMusic.sh <input directory w/ .MP4s> <output directory for .MP3s and .WAVs>

temporaryDirectory=".tempProcessMusic"
if [ ! -e $temporaryDirectory ]; then mkdir $temporaryDirectory; fi
trap "if [ -e '$temporaryDirectory' ]; then rm -r '$temporaryDirectory'; fi" EXIT

sourceDirectory="${1:-.}"
outputDirectory="${2:-out}"
if [ ! -e "$outputDirectory" ]; then mkdir "$outputDirectory"; fi

# Converts mp4 files to mp3 files.
echo "Starting conversion from mp4 to mp3."

for mp4File in $sourceDirectory/*.mp4; do
    asMp3="${mp4File%.*}.mp3" && asMp3="${asMp3##*/}"
    
    if [ ! -e "$outputDirectory/$asMp3" ]; then
	ffmpeg -i "$mp4File" "$temporaryDirectory/$asMp3" &
    fi
done

wait
echo "Finished conversion."

# Normalizes file volume.
echo "Starting volume normalization."
echo "Writing mp3s to '$outputDirectory'."

for mp3File in $temporaryDirectory/*.mp3; do
    newFileName="$outputDirectory/${mp3File##*/}"
    
    if [ ! -e "$newFileName" ]; then
	sox --norm "$mp3File" "$newFileName" &
    fi
done

wait
echo "Finished normalization."


echo "Starting Cleanup"
rm -r $temporaryDirectory/*
echo "Cleanup finished."


# Creates wav files from mp4s meant for CDs and DVDs.
echo "Starting conversion and possible downsampling from mp3 to wav @ 44.1kHz."
echo "Writing wavs to '$outputDirectory'."

for mp3File in $outputDirectory/*.mp3; do
    asWav="${mp3File%.*}.wav" && asWav="${asWav##*/}"

    if [ ! -e "$outputDirectory/$asWav" ]; then
	temporary="$temporaryDirectory/$asWav"
	ffmpeg -i "$mp3File" "$temporary" && sox "$temporary" "$outputDirectory/$asWav" rate 44100 &
    fi
done

wait
echo "Finished conversion."


echo "Starting Cleanup"
rm -r $temporaryDirectory
echo "Cleanup finished."

echo "All finished!"
