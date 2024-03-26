import events
import pluginstuff
import math

freecam_on = False


def on_command_event(command_event):
    global freecam_on

    if not command_event.is_canceled and command_event.get_command() == 'freecam':
        command_event.is_canceled = True
        command_event.successful = True

        freecam_on = not freecam_on
        print("Toggled freecam mode.")


def on_player_move_event(player_move_event):
    pass


events.add_player_movement_listener(on_player_move_event)
events.add_command_listener(on_command_event)
print("Use /freecam to put your self into freecam mode.")