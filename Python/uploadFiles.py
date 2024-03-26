import os


def grab_subdirectories(path):
    found_dirs = []
    crawl_through_subdirectories(path, found_dirs)

    return found_dirs


def crawl_through_subdirectories(path, array):
    for i in os.listdir(path):
        if os.path.isdir(path + "\\" + i):
            array.append(path + "\\" + i)
            crawl_through_subdirectories(path + "\\" + i, array)


def prep_uploads():
    print("Prepping uploads...")
    basePath = os.path.dirname(os.path.abspath(__file__))

    if not os.listdir(basePath).__contains__("in"):
        os.mkdir("in")
    if not os.listdir(basePath).__contains__("out"):
        os.mkdir("out")

    print("Reading contents...")
    dir_list = grab_subdirectories(basePath + "\\in")

    print("Creating directories...")
    if dir_list.__len__() > 0:
        for i in dir_list:
            i = i.replace("\\in\\", "\\out\\")

            if not os.path.exists(i):
                os.mkdir(i)

    print("Prep finished!\n")


def do_uploads():
	print("Doing uploads...")
	
    print("Uploads finished!\n")


def post_uploads():
	print("Doing post processes...")

    print("Post processes finished!\n")


prep_uploads()
do_uploads()
post_uploads()

print("Upload complete!")
