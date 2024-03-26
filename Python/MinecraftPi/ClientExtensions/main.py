import events
import pluginstuff
import time

# /usr/lib/python3/dist-packages/mcpi

program_done = False

# The current instance of minecraft
mc_instance = None
has_instance = False


def _on_enable():
    ''' Used to activate other files in the project. '''
    import freecam


def main():
    global has_instance

    pluginstuff._update(globals())
    events._on_enable()

    print("Chat thread started. User input will be sent to chat, if a connection has been made.")
    print("If input starts with a '/', then it will be parsed as a command.")
    print()
    print("Use /connect [ip] to connect this script to a minecraft pi server. Will attempt to connect to localhost if ip is blank.")
    print("Use /exit to stop the program.")

    _on_enable()

    print()

    # Plugin loop.
    while not program_done:
        # Time control.
        last_time = time.process_time()

        # Makes sure that the instance of minecraft is actually there.
        if has_instance:
            try:
                mc_instance.getPlayerEntityIds()

            except:
                has_instance = False
                print("The connection was closed.")

        # Updates the functions in pluginstuff.
        pluginstuff._update(globals())

        # Processes events.
        events._update()

        # Time control.
        delta_time = time.process_time() - last_time
        if delta_time < 1/60:
            time.sleep(1/60 - delta_time)


def _get_instance():
    ''' Gets the current instance of minecraft, or none if there is not an instance. '''
    if has_instance:
        return mc_instance

    return None


def _set_instance(instance):
    ''' Sets the current instance of minecraft. '''
    global mc_instance
    global has_instance

    if instance is not None:
        mc_instance = instance
        has_instance = True

    else:
        if has_instance:
            print("The connection was closed.")

        mc_instance = None
        has_instance = False


def _stop_program():
    ''' Stops the program. '''
    global program_done

    program_done = True
    print("Exiting program...")


if __name__ == '__main__':
    main()