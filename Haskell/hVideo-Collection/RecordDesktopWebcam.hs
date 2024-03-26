#!/usr/bin/env runghc
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Eta reduce" #-}

{-|
Module      : RecordDesktopWebcam
Description : Small configurable interface with ffmpeg and mpv to record the desktop and webcam.
Copyright   : (c) Nathaniel Needham 2022
License     : MIT

A configurable desktop and webcam recorder that interfaces with ffmpeg and mpv.
You will need to go through the config and set things according to your system and preferences.
You probably won't need to modify the options to ffmpeg and mpv. Just take care if you do

An output file can be specified as the first argument.
Desktop video and audio will be outputted to "desktop-{fileName}".
Webcam video and audio will be outputted to "webcam-{fileName}".
-}

module RecordDesktopWebcam (main) where
import System.Environment
import System.Process
import Text.Printf
import Control.Monad
import GHC.IO.Handle.Types
import System.Directory



{- Config START -}
-- |Whether the webcam is recorded.
includeWebcam :: Bool
includeWebcam = True
-- |Whether to include the webcam directly into the recorded video. If True it will play through mpv. 
-- Requires includeWebcam to be True to have an effect.
separateWebcam :: Bool
separateWebcam = True
--TODO
includeWebcamAudio :: Bool
includeWebcamAudio = True
-- | Whether the desktop is recorded.
includeDesktop :: Bool
includeDesktop = True
--TODO
includeDesktopAudio :: Bool
includeDesktopAudio = True
-- |Path to file in which to place the outputted file
defaultOutputFile :: FilePath
defaultOutputFile = "videoout.mkv"

-- |Number of frames to record per second. No guarantees.
frameRate :: Int
frameRate = 30
-- |The resolution of the desktop (basically just the resolution of your monitor.)
desktopResolution :: (Int, Int)
desktopResolution = (1366, 768)
-- |The resolution of the webcam.
webcamResolution :: (Int, Int)
webcamResolution = (640, 480)
-- |How much to divide the width of the webcam by. Ratio is preserved.
webcamScalar :: Float
webcamScalar = 1.5
-- |Video source to pull the video of the desktop from.
desktopVideoSource :: String
desktopVideoSource = ":0.0"
-- |Audio device to pull desktop audio from. Use [pactl list sources] to find.
desktopAudioDevice :: String
desktopAudioDevice = "alsa_output.pci-0000_00_1b.0.analog-stereo.monitor"
-- |Video source to pull the video of the webcam from.
webcamVideoSource :: String
webcamVideoSource = "/dev/video0"
-- |Audio device to pull webcam audio from. Use [pactl list sources] to find.
webcamAudioDevice :: String
webcamAudioDevice = "alsa_input.pci-0000_00_1b.0.analog-stereo"
{- Config END -}

{- !DANGER! FFmpeg & mov Config START -}
-- |Number of audio channels present in the desktop audio device.
desktopAudioChannels :: Int
desktopAudioChannels = 2
-- |The audio rate present in the desktop audio device.
desktopAudioRate :: Int
desktopAudioRate = 48000
-- |Number of audio channels present in the desktop audio device.
webcamAudioChannels :: Int
webcamAudioChannels = 2
-- |The audio rate present in the desktop audio device.
webcamAudioRate :: Int
webcamAudioRate = 44100
-- |CRF for choosing the video quality from H.264.
x264CRF :: Int
x264CRF = 17
-- |Preset for H.264
x264Preset :: String
x264Preset = "veryfast"
-- |Tune for H.264
x264Tune :: String
x264Tune = "zerolatency"
-- |Size of the thread queue to supply to the video and audio inputs.
threadQueueSize :: Int
threadQueueSize = 16
-- |Number of threads to supply to ffmpeg. 0 means as many as it will take.
threads :: Int
threads = 0

-- |Desktop audio input options for ffmpeg.
ffmpegDesktopAI :: String
ffmpegDesktopAI = printf "-thread_queue_size %d -f pulse -ac %d -ar %d -i %s"
                         threadQueueSize desktopAudioChannels desktopAudioRate desktopAudioDevice
-- |Desktop video input options for ffmpeg.
ffmpegDesktopVI :: String
ffmpegDesktopVI = printf "-thread_queue_size %d -f x11grab -r %d -s %dx%d -i %s"
                         threadQueueSize frameRate (fst desktopResolution) (snd desktopResolution) desktopVideoSource

-- |Webcam audio input options for ffmpeg.
ffmpegWebcamAI :: String
ffmpegWebcamAI = printf "-thread_queue_size %d -f pulse -ac %d -ar %d -i %s"
                        threadQueueSize webcamAudioChannels webcamAudioRate webcamAudioDevice
-- |Webcam video input options for ffmpeg.
ffmpegWebcamVI :: String
ffmpegWebcamVI = printf "-s %dx%d -i %s -vf scale=iw/%f:-1 -vf fps=%d"
                        (fst webcamResolution) (snd webcamResolution) webcamVideoSource webcamScalar frameRate

-- |Final output options for ffmpeg.
ffmpegOutput :: String -> String
ffmpegOutput outputFile = printf "-vcodec libx264 -pix_fmt yuv420p -crf %d -preset %s -tune %s -threads %d -acodec pcm_s16le -y %s"
                                      x264CRF x264Preset x264Tune threads outputFile 

-- |Call to mpv for when a separate webcam is used.
mpvSerparatedWebcamVO :: String
mpvSerparatedWebcamVO = printf "mpv --no-audio %s"
                               webcamVideoSource
{- !DANGER! FFmpeg & mov Config END -}



-- |Starts recording the desktop. Thread-blocking.
recordDesktop :: String -> IO ()
recordDesktop desktopFilePath = callCommand $ "ffmpeg" 
                                           ++ (if includeWebcamAudio && separateWebcam then ' ' : ffmpegWebcamAI  else "")
                                           ++ (if includeDesktopAudio                  then ' ' : ffmpegDesktopAI else "")
                                           ++ ' ' : ffmpegDesktopVI 
                                           ++ ' ' : ffmpegOutput desktopFilePath

-- |Attempts to start recording the webcam. Whether it does depends on the config.
-- Will return a valid dummy process that exits immediately if there is nothing to record.
tryCreateWebcamProcess :: String -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
tryCreateWebcamProcess webcamFilePath = createProcess $ shell $ 
    if includeWebcam
        then if not separateWebcam
            then "ffmpeg" 
              ++ (if includeWebcamAudio then ' ' : ffmpegWebcamAI else "")
              ++ ' ' : ffmpegWebcamVI 
              ++ ' ' : ffmpegOutput webcamFilePath
            else mpvSerparatedWebcamVO
        else ""

main :: IO ()
main = do
    arguments <- getArgs
    let outputFile = if not (null arguments) then head arguments else defaultOutputFile

    if includeDesktop
        then do
            (_, _, _, webcamVideoProcess) <- tryCreateWebcamProcess ("webcam-" ++ outputFile)
            recordDesktop ("desktop-"  ++ outputFile)
            when (includeWebcam && not separateWebcam) $
                terminateProcess webcamVideoProcess

        else if includeWebcam && not separateWebcam
            then do
                (_, _, _, webcamVideoProcess) <- tryCreateWebcamProcess ("webcam-" ++ outputFile)
                waitForProcess webcamVideoProcess
                return ()
            else putStrLn "Nothing to do! Change the configuration if you want more interesting behavior"