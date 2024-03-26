import tkinter as tk


windowWidth = 1920
windowHeight = 1080
pixelsPerSample = 7


window = tk.Tk()
window.title("IM'A FIRIN' ME LAZARRRRRRRRRRRRRRRRRRRRRRRRRRRRR")
window.geometry(str(windowWidth) + 'x' + str(windowHeight))

canvas = tk.Canvas(window, width=windowWidth, height=windowHeight)
canvas.pack()


def invert(y):
    return windowHeight - y


def adjust_coord(coord, factor):
    return coord + ((coord - 1) * factor)


for x in range(0, windowWidth // pixelsPerSample):
    for y in range(0, windowHeight // pixelsPerSample):
        canvas.create_oval(adjust_coord(x, pixelsPerSample), invert(adjust_coord(y, pixelsPerSample)),
                           (x + pixelsPerSample) * pixelsPerSample, invert((y + pixelsPerSample) * pixelsPerSample))

tk.mainloop()
