import findExec = require("find-exec");
import { ChildProcessWithoutNullStreams, spawn } from "child_process";
import * as fs from "fs";
const R_OK = fs.constants.R_OK;



/*
 * MIT License
 *
 * Copyright (c) 2022 ona-li-toki-e-jan-Epiphany-tawa-mi
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */



/**
 * A mapping between string keys and a given type. Just objects with a specified value type.
 */
export interface Dictionary<Type> {
	[key: string]: Type;
};

/**
 * More relavent type name for returned processes.
 */
export type AudioProcess = ChildProcessWithoutNullStreams;



/**
 * Command line audio players. Must be mp3 compatible.
 */
export const players: string[] = [ "mplayer", "mpv", "ffplay"
								 , "cvlc" /* VLC */, "play" /* SoX */
								 , "mpg123", "mpg321" /* Same player, different name */];

/**
 * A list of command line audio players that are capable of playing audio sourced from a url.
 * @attention SoX and mpg123/mpg321 have URL support, but seem a little unreliable, so I'm not including them.
 */
export const URLPlayers: string[] = [ "mpv", "mplayer", "ffplay"
									, "cvlc" /* VLC */];

/**
 * Various options to supply to each player.
 * Namely makes sure players don't open any windows and exit when done.
 */
export const playerOptions: Dictionary<string[]> = { ffplay:  [ "-nodisp", "-vn" /* Prevents video output and other visual hoo-has. */
															  , "-loglevel", "quiet"
															  , "-autoexit"]
												   , cvlc:    [ "--play-and-exit"
															  , "--no-video"
															  , "--verbose", "0" /* Reduces unneeded text output. */]
												   , mpv:     [ "--no-video"
															  , "--no-terminal"         /* Prevents unneeded terminal output. */
															  , "--no-config"           /* Prevents any possible conflict with user configuration. */
															  , "--profile=low-latency" /* Low-latency specifier to try and play audio ASAP. */]
												   , mplayer: [ "-nogui", "-vc", "null", "-vo", "null" /* Prevents video output and other visual hoo-has. */
															  , "-noconfig", "all"                     /* Prevents any possible conflict with user configuration. */
															  , "-really-quiet"]
												   , play:    [ "--no-show-progress", "-V0" /* Prevents unneeded terminal output. */]
												   , mpg123:  [ "--quiet"]
												   , mpg321:  [ "--quiet"]};

let _player: string | null = null;
/**
 * Gets the first available player on the system.
 * On first call, attempts to select a player from {@link players}.
 *
 * @returns The player.
 * @throws [Error] If there are no available players.
 */
export function getAvaliblePlayer(): string {
	if (!_player)
		reselectPlayer();

	return _player;
}

let _URLPlayer: string | null = null;
/**
 * Gets the first available URL player on the system.
 * On first call, attempts to select a URL player from {@link URLPlayers}.
 *
 * @returns The URL player.
 * @throws [Error] If there are no available URL players.
 */
export function getAvalibleURLPlayer(): string {
	if (!_URLPlayer)
		reselectURLPlayer();

	return _URLPlayer;
}

/**
 * Updates the player to the first available player within the given list.
 *
 * @param playerList The list of players to select from, defaults to {@link players}.
 * @returns The player.
 * @throws [Error] If there are no available players.
 */
export function reselectPlayer(playerList: string[] = players): string {
	let newPlayer = findExec(playerList);

	if (!newPlayer)
		throw new Error(`Unable to find any sound players on the system! (attempted to look for ${playerList})`);

	_player = newPlayer;
	return _player;
}

/**
 * Updates the URL player to the first available player within the given list.
 *
 * @param URLPlayerList The list of players to select from, defaults to {@link URLPlayers}.
 * @returns The URL player.
 * @throws [Error] If there are no available URL players.
 */
export function reselectURLPlayer(URLPlayerList: string[] = URLPlayers): string {
	let newPlayer = findExec(URLPlayerList);

	if (!newPlayer)
		throw new Error(`Unable to find any URL players on the system! (attempted to look for ${URLPlayerList})`);

	_URLPlayer = newPlayer;
	return _URLPlayer;
}

/**
 * Attempts to forcefully set a different player.
 *
 * @param player The path to the new player.
 * @returns Whether the new player was found. If false, the original player is kept.
 */
export function overridePlayer(player: string): boolean {
	let possiblePlayer = findExec(player);

	if (!possiblePlayer)
		return false;

	_player = possiblePlayer
	return true;
}

/**
 * Attempts to forcefully set a different URL player.
 *
 * @param URLPlayer The path to the new URL player.
 * @returns Whether the new URL player was found. If false, the original player is kept.
 */
export function overrideURLPlayer(URLPlayer: string): boolean {
	let possiblePlayer = findExec(URLPlayer);

	if (!possiblePlayer)
		return false;

	_URLPlayer = possiblePlayer
	return true;
}



/**
 * Launches a child process to play the given audio file.
 *
 * @param filePath audio file path.
 * @param options Various options to supply to each player, defaults to {@link playerOptions}.
 * @throws [Error] If the file could not be opened.
 *         [Error] If there are no available players.
 */
export function playFile(filePath: string, options: Dictionary<string[]> = playerOptions): AudioProcess {
	try {
		fs.accessSync(filePath, R_OK);
	} catch (error) {
		throw new Error(`An error occured while trying to open sound file "${filePath}"; unable to open!". Description: ${error}`);
	}

	const player = getAvaliblePlayer();
	const args   = (options[player] || []).concat(filePath);

	return spawn(player, args);
}

/**
 * Launches a child process to play the audio file at the given URL.
 *
 * @param url audio file URL.
 * @param options Various options to supply to each player, defaults to {@link playerOptions}.
 * @throws [Error] If there are no available players.
 */
export function playURL(url: string, options: Dictionary<string[]> = playerOptions): AudioProcess {
	const player = getAvalibleURLPlayer();
	const args   = (options[player] || []).concat(url);

	return spawn(player, args);
}

/**
 * @param audioProcess The audio-playing child process.
 * @returns A promise containing the error code of the process for when the audio player exits because
 *      of an error or it couldn't start in the first place.
 */
 export function onError(audioProcess: AudioProcess): Promise<number> {
	return new Promise((resolve) =>
		audioProcess.on('error', resolve));
}

/**
 * @param audioProcess The audio-playing child process.
 * @returns A promise containing the error code of the process for when the audio player exits.
 */
export function onClose(audioProcess: AudioProcess): Promise<number> {
	return new Promise((resolve) =>
		audioProcess.on('close', resolve));
}

/**
 * Pauses an audio process, does nothing if the process exited.
 * @attention Will terminate process on Windows instead of pausing them.
 *
 * @param audioProcess The audio-playing child process.
 * @returns Whether the process was paused.
 */
export function pause(audioProcess: AudioProcess): boolean {
	// Makes sure process isn't closed.
	if (audioProcess.exitCode === null)
		return audioProcess.kill('SIGSTOP');

	return false;
}

/**
 * Resumes a previously paused audio process, does nothing if the process exited.
 * @attention Will terminate process on Windows instead of resuming them.
 *
 * @param audioProcess The audio-playing child process.
 * @returns Whether the process was resumed.
 */
export function resume(audioProcess: AudioProcess): boolean {
	// Makes sure process isn't closed.
	if (audioProcess.exitCode === null)
		return audioProcess.kill('SIGCONT');

	return false
}

/**
 * "Restarts" the audio process by spawning a new one using the same arguments and returning that. If
 *      the process is currently running it will be stopped.
 *
 * @param audioProcess The audio-playing child process.
 * @returns The new "restarted" audio process if succeded, null if not.
 */
export function restart(audioProcess: AudioProcess): AudioProcess | null {
	// Attempts to kill the process if it's still running.
	if (audioProcess.exitCode === null && !audioProcess.kill())
		return null;

	let [player, ...options] = audioProcess.spawnargs;
	return spawn(player, options);
}
