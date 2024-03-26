i = 1
pool = 3
entFormat = open("play_script.txt", "w")

while i == 1:
    readLine = input()
    if readLine == "stop":
        i = 0
        entFormat.close()
    else:
        if readLine == "":
            wool = "{}".format(pool)
            entFormat.write("    elseif time < "+wool+" then\n")
            pool = pool + 1
        else:
            if readLine == "  ":
                p = 0
            else:
                entFormat.write("        print(\""+readLine+"\")\n")