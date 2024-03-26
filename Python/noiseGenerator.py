import tkinter as tk
import random

windowWidth = 1280
windowHeight = 720
pixelsPerSample = 4


window = tk.Tk()
window.title("Noise Gen Output")
window.geometry(str(windowWidth) + 'x' + str(windowHeight))

canvas = tk.Canvas(window, width=windowWidth, height=windowHeight, bd=0, bg="#ffffff")
canvas.pack()


def invert_y(y):
    return windowHeight - y


def adjust_coord(coord, factor):
    return (coord + ((coord - 1) * factor)) - 1


def compute_noise(x, y):
   tmp = random.random()

   return hex(int(tmp * 255))[2:4]


for x in range(1, (windowWidth + 1) // pixelsPerSample):
    for y in range(1, (windowHeight + 1) // pixelsPerSample):
        canvas.create_rectangle(adjust_coord(x, pixelsPerSample),
                                invert_y(adjust_coord(y, pixelsPerSample)),
                                adjust_coord(x, pixelsPerSample) + pixelsPerSample + 1,
                                invert_y(adjust_coord(y, pixelsPerSample)) - pixelsPerSample - 1,
                                fill='#' + (compute_noise(x, y) * 3),
                                width=0)


tk.mainloop()

