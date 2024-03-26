import math


def get_month_and_day(days_before):
    """
        Gets the month and day of some day that is days_before days before December 31 @ midnight.

        :param days_before: The number of days before December 31 @ midnight.
        :return: The month and day of some day that is days_before days before December 31 @ midnight.
    """

    if 365 >= days_before >= 335:
        return "January", 365 - days_before + 1
    elif 334 >= days_before >= 307:
        return "February", 334 - days_before + 1
    elif 306 >= days_before >= 276:
        return "March", 306 - days_before + 1
    elif 275 >= days_before >= 246:
        return "April", 275 - days_before + 1
    elif 245 >= days_before >= 215:
        return "May", 245 - days_before + 1
    elif 214 >= days_before >= 185:
        return "June", 214 - days_before + 1
    elif 184 >= days_before >= 154:
        return "July", 184 - days_before + 1
    elif 153 >= days_before >= 123:
        return "August", 153 - days_before + 1
    elif 122 >= days_before >= 93:
        return "September", 122 - days_before + 1
    elif 92 >= days_before >= 62:
        return "October", 92 - days_before + 1
    elif 61 >= days_before >= 32:
        return "November", 61 - days_before + 1
    elif 31 >= days_before >= 1:
        return "December", 31 - days_before + 1
    elif 0 == days_before:
        return "New year, December-January", 0


def sawtooth(number):
    return number - math.floor(number)


events = {"The Big Bang": 13700000000, "Formation of the first neutral atoms": 13699600000,
          "Formation of first stars & galaxies": 13000000000,
          "Formation of the thin disk of the Milky Way Galaxy": 9000000000, "Formation of the Solar System": 4560000000,
          "Formation of the Moon": 4500000000, "Birth of life on Earth": 3800000000,
          "First cells develop on Earth": 2000000000, "First vertebrates": 520000000, "First dinosaurs": 230000000,
          "First mammals": 200000000, "Death of the dinosaurs from giant impact": 65000000,
          "First bipedal hominids": 4000000, "First humans": 200000, "Development of agriculture": 10000,
          "Development of the wheel for transportation": 6500, "Invention of the telescope": 400, "Right now": 0}

scaled_events = {}

for event_name, time in events.items():
    scaled_time = time / 13700000000.0 * 365

    days_ago = round(scaled_time)
    month_of, day_of = get_month_and_day(days_ago)
    # Days unaccounted for -> Hours.
    time_of_hr = math.floor(24 - sawtooth(scaled_time) * 24) + 12
    # Days unaccounted for -> Hours -> Minutes.
    time_of_min = round(60 - sawtooth(sawtooth(scaled_time) * 24) * 60)

    if time_of_min >= 60:
        time_of_hr += 1
        time_of_min %= 60
    time_of_hr %= 25

    events[event_name] = [days_ago, month_of, day_of, time_of_hr, time_of_min]

# Prints the results
for even_name, date_time in events.items():
    hours = date_time[3]
    if date_time[0] == 0:
        hours += 12
        
    hours = str(hours)
    if date_time[3] < 10:
        hours = "0" + hours

    minutes = str(date_time[4])
    if date_time[4] < 10:
        minutes = "0" + minutes

    print(even_name + ": \n    days ago: " + str(date_time[0]) +
          "\n    on: " + str(date_time[1]) + " " + str(date_time[2]) +
          "\n    at: " + hours + ":" + minutes)

print('\n')
