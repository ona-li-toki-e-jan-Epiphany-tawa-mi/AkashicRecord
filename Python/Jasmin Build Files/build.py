import os
import os.path as path
import shutil
import build_config


# Custom file handling, mainly used for compilations.
file_type_handling = {
    'j': lambda jasmin_file, destination: os.system('python -Wi::DeprecationWarning C:/JASMIN/assemble.py -out build/classes -q ' + jasmin_file)
}


def attempt_transfer(file_path, build_path, file_name):
    """ Attempts to move a file from src to build/classes, compiling along the way. """
    try:
        type_handle = file_type_handling[file_path.split(".")[-1]]

        # Copies file to temporary directory and executes type handling.
        shutil.copy(file_path, "build/tmp")
        type_handle("build/tmp/" + file_name, build_path)

        # Clears temporary directory.
        for file in os.listdir("build/tmp"):
            tmp_file_path = "build/tmp/" + file

            if path.isfile(tmp_file_path):
                os.remove(tmp_file_path)

            elif path.isdir(tmp_file_path):
                shutil.rmtree(tmp_file_path)

    except KeyError:
        if path.isfile(file_path):
            os.remove(file_path)

        shutil.copy(file_path, build_path)


def copy_directories(directory, build_path):
    """ Copies over the source directory, compiling along the way"""
    for file in os.listdir(directory):
        file_path = directory + '/' + file

        # Copies over directories.
        if path.isdir(file_path):
            build_path += '/' + file

            if not path.isdir(build_path):
                os.mkdir(build_path)

            copy_directories(file_path, build_path)

        # Copies over files.
        elif path.isfile(file_path):
            attempt_transfer(file_path, build_path, file)


# TODO Handle errors with batch commands.
# TODO Make less primitive.

""" Compiles project. """
if __name__ == '__main__':
    print("Setting up...")

    if path.isdir("build/classes"):
        shutil.rmtree("build/classes")

    for directory in ["build", "build/classes", "build/tmp"]:
        if not path.isdir(directory):
            os.mkdir(directory)

    print("Compiling project...")
    copy_directories(build_config.source_directory, "build/classes")

    print("Generating Jar...")
    os.system('jar -cvfe build/' + build_config.output_name + '-' + build_config.version + '.jar ' + build_config.main_class + ' -C build/classes ' + " ".join(os.listdir("build/classes")))

    print("Cleaning up...")
    os.rmdir("build/tmp")

    if not build_config.keep_classes:
        shutil.rmtree("build/classes")

    print("Done!")
