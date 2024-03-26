import pluginstuff
from mcpi import minecraft
from mcpi import vec3
import threading
import re

# Make command to set username.
# Add generic event handling.
# Add player move event.

# A queue for requests made by the user.
player_input_queue = []
# Used for thread-safety with the input queue.
queue_lock = threading.Lock()

def _process_input():
    ''' Gets player input from the console, parses it, and loads it into a queue for the event listeners. '''
    program_done = False
    ip_pattern = re.compile("\\b(?:(?:2(?:[0-4][0-9]|5[0-5])|[0-1]?[0-9]?[0-9])\\.){3}(?:(?:2([0-4][0-9]|5[0-5])|[0-1]?[0-9]?[0-9]))\\b")

    while not program_done:
        player_input = input()

        if player_input != '':
            # Commands.
            if player_input[0] == '/':
                player_input = player_input[1 : len(player_input)]

                # Special exit command. Used to exit the program.
                if player_input[0:4] == 'exit':
                    program_done = True
                    with queue_lock:
                        player_input_queue.append( ('exit', "") )

                # Special connect command. Used to set the server to place the script on.
                elif player_input[0:7] == 'connect':
                    split_input = player_input.split(' ')

                    # Attempts to connect to a server with the ip given by the player.
                    if len(split_input) > 1:
                        split_ip = split_input[1].split(':', 1)
                        ip = split_ip[0]

                        if ip_pattern.match(ip):
                            port = None

                            # Gets port
                            if len(split_ip) > 1:
                                possible_port = split_ip[1]

                                if possible_port.isdecimal():
                                    possible_port = int(possible_port)

                                    if possible_port < 65535:
                                        port = possible_port

                            with queue_lock:
                                player_input_queue.append( ('connect', ip, port) )

                        else:
                            if len(split_ip) > 1:
                                print("Failed to connect to '" + ip + split_ip[1] + "', invalid IP.")

                            else:
                                print("Failed to connect to '" + ip + "', invalid IP.")

                    # Attempts to connect to local host.
                    else:
                        with queue_lock:
                            player_input_queue.append( ('connect', "") )

                # Generic command handling.
                else:
                    split_input = player_input.split(' ')
                    arguments = []

                    if len(split_input) > 1:
                        arguments = split_input[1 : len(split_input) - 1]

                    with queue_lock:
                        player_input_queue.append( ('command', split_input[0], arguments) )


            # Chat messages.
            else:
                with queue_lock:
                    player_input_queue.append( ('message', player_input) )


# Thread used to listen for player commands.
player_input_thread = threading.Thread(target = _process_input)


def _on_enable():
    ''' Intializes input thread. '''
    player_input_thread.start()


class ChatEvent:
    ''' A chat event. Occurs when a player attempts to send a chat message to the server. '''
    def __init__(self, instance, message):
        # The current instance of minecraft, or None.
        self.__instance = instance
        # The message to be sent to the server chat.
        self.message = message
        # Whether or not the message should be sent.
        self.is_canceled = False


    def get_instance(self):
        ''' Returns the current instance of minecraft, or None. '''
        return self.__instance


class CommandEvent:
    ''' A command event. Occurs when a player attempts to execute a command. '''
    def __init__(self, instance, command, arguments):
        # The current instance of minecraft, or None.
        self.__instance = instance
        # The command to be executed.
        self.__command = command
        # The arguments of the command.
        self.__arguments = arguments
        # Whether or not the command should be executed. Has no direct effect.
        self.is_canceled = False
        # Whether or not the command was successful. Prints a generic error message if false.
        self.successful = False


    def get_instance(self):
        ''' Returns the current instance of minecraft, or None. '''
        return self.__instance


    def get_command(self):
        ''' Gets the command to be executed. '''
        return self.__command


    def get_command(self):
        ''' Gets the arugements of the command. '''
        return self.__arguments.copy()


class PlayerMoveEvent:
    ''' A player move event. Occurs when a player attempts to move. '''
    def __init__(self, instance, player, new_position, last_position, change_vector, change):
        # The current instance of minecraft, or None.
        self.__instance = instance
        # The player that moved.
        self.__player = player
        # The new position of the player.
        self.__new_position = new_position
        # The previous position of the player.
        self.__last_position = last_position
        # A vector that points from the previous position to the current one.
        self.__change_vector = change_vector
        # The magnitude of the change vector. The distance between the previous position and the current one.
        self.__change = change
        # Whether or not the command should be executed. Has no direct effect.
        self.is_canceled = False


    def get_instance(self):
        ''' Returns the current instance of minecraft, or None. '''
        return self.__instance


    def get_player(self):
        ''' Returns the player that moved. '''
        return self.__player


    def get_new_position(self):
        ''' Gets the new position of the player. '''
        return self.__new_position.copy()


    def set_new_position(self, position):
        ''' Sets the new position to move the player to, and recalculates the change in position. '''
        if self.__new_position != position:
            self.__new_position = position
            self.__change_vector = self.__new_position - self.__last_position
            self.__change = self.__change_vector.length()


    def get_last_position(self):
        ''' Returns the previous position of the player. '''
        return self.__last_position.copy()


    def get_change_vector(self):
        ''' Returns a vector that points from the previous position to the current one. '''
        return self.__change_vector.copy()


    def get_change(self):
        ''' Returns the magnitude of the change vector. The distance between the previous position and the current one. '''
        return self.__change


# List of functions to call when a player enters a command.
command_listeners = []
# List of functions to call when a player sends a chat message.
chat_listeners = []
# List of functions to call when a player moves.
player_movement_listeners = []
# List of functions to call every "tick."
runnables = []

# The last position of the player.
last_position = None


def _update():
    ''' Runs event based things. '''
    global last_position

    # Executes runnables.
    for runnable in runnables:
        runnable()

    mc_instance = pluginstuff.get_instance()

    # Runs player movement event listeners.
    if len(player_movement_listeners) > 0 and mc_instance is not None:
        player = mc_instance.player

        if last_position is not None:
            current_position = player.getPos()

            # Tests for movement.
            if current_position != last_position:
                change_vector = current_position - last_position
                change = change_vector.length()

                # Calls player movement event listeners.
                if change >= 0.01:
                    player_move_event = PlayerMoveEvent(mc_instance, player, current_position, last_position, change_vector, change)

                    for player_movement_listener in player_movement_listeners:
                        player_movement_listener(player_move_event)

                    if player_move_event.is_canceled:
                        player.setPos(last_position)

                    # Changes the position of the player to if it was changed in the event.
                    else:
                        new_position = player_move_event.get_new_position()

                        if new_position != current_position:
                            player.setPos(new_position)

                last_position = current_position

        # Initial position get.
        else:
            last_position = player.getPos()

    # Runs event listeners that involve the input queue.
    if len(player_input_queue) > 0:
        # Grabs a copy of the input queue and clears it. This is to reduce down-time from using a lock.
        input_queue_copy = []
        with queue_lock:
            input_queue_copy = player_input_queue.copy()
            player_input_queue.clear()

        # Calls events listeners.
        for player_input in input_queue_copy:
            # Operates chat listeners, and sends chat messages.
            if player_input[0] == 'message':
                if mc_instance is not None:
                    chat_event = ChatEvent(mc_instance, player_input[1])

                    for chat_listener in chat_listeners:
                        chat_listener(chat_event)

                    if not chat_event.is_canceled:
                        mc_instance.postToChat("<Pinku_Jin> " + chat_event.message)


            # Operates command listeners.
            elif player_input[0] == 'command':
                command_event = CommandEvent(mc_instance, player_input[1], player_input[2])

                for command_listener in command_listeners:
                    command_listener(command_event)

                if not command_event.is_canceled and not command_event.successful:
                    print("Command execution was unseccessful.")


            # Attempts to connect the script to a minecraft pi server.
            elif player_input[0] == 'connect':
                ip = player_input[1]

                if ip == "":
                    try:
                        pluginstuff.set_instance(minecraft.Minecraft.create())
                        print("Successfully connected to localhost.")

                    except:
                        print("Failed to connect to localhost.")

                else:
                    port = player_input[2]

                    # With port specified.
                    if port is not None:
                        try:
                            pluginstuff.set_instance( minecraft.Minecraft.create(ip, port) )
                            print("Successfully connected to " + ip + ":" + str(port))

                        except:
                            print("Failed to connect to " + ip + ":" + str(port))

                    # Without port specified.
                    else:
                        try:
                            pluginstuff.set_instance(minecraft.Minecraft.create(ip))
                            print("Successfully connected to " + ip)

                        except:
                            print("Failed to connect to " + ip)


            # Stops the program loop when the exit command is called.
            else:
                player_input_thread.join()
                pluginstuff.stop_program()


def add_runnable(function):
    ''' Adds a function to be called every tick. Must be of the form: function() '''
    runnables.append(function)


def add_player_movement_listener(function):
    ''' Adds a function to be called when a player moves. Must be of the form: function(PlayerMoveEvent) '''
    player_movement_listeners.append(function)


def add_command_listener(function):
    ''' Adds a function to be called when a player executes a command. Must be of the form: function(Minecraft, CommandEvent) '''
    command_listeners.append(function)


def add_chat_listener(function):
    ''' Adds a function to be called when a player sends a chat message. Must be of the form: function(Minecraft, ChatEvent) '''
    chat_listeners.append(function)