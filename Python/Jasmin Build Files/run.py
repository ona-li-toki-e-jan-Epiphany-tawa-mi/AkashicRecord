import os
import os.path as path
import sys
import build_config

""" Runs Project. """
if __name__ == '__main__':
    file_name = build_config.output_name + "-" + build_config.version + '.jar'
    file_path = "build/" + file_name

    if not path.isfile(file_path):
        sys.exit("Error: Jar file " + file_name + " does not exist in build directory!")

    os.system('java -verify -jar ' + file_path + ' ' + build_config.run_arguments)
