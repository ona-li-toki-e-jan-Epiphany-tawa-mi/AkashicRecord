from mcpi import minecraft
import time

# Add runnables.
# Add functions to register listeners.
# Port over features from ClientExtensions.

def Main():
    mc_instance = minecraft.Minecraft.create()
    events = mc_instance.events
    is_multiplayer = False



    import TNT

    # List of functions to call when a block event occurs.
    block_event_listeners = [TNT.light_tnt]



    # Plugin loop.
    while True:
        # Time control.
        last_time = time.process_time()


        # Checks if there are multiple players on the server.
        is_multiplayer = len(mc_instance.getPlayerEntityIds()) > 1

        # Queries event listeners in block_event_listeners[].
        # Each listener must be a lambda of the form: lambda(mincraft_instance, block_event, is_multiplayer), or else behavior is undefined.
        if len(block_event_listeners) != 0:
            for block_event in events.pollBlockHits():
                for block_event_listener in block_event_listeners:
                    block_event_listener(mc_instance, block_event, is_multiplayer)

        else:
            events.clearAll()


        # Time control.
        delta_time = time.process_time() - last_time
        if delta_time < 1/60:
            time.sleep(1/60 - delta_time)


if __name__ == '__main__':
    Main()