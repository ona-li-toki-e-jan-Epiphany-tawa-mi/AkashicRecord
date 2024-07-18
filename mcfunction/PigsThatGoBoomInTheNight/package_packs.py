#!/usr/bin/env python3

import logging
from shutil import make_archive
import os.path as path
from enum import Enum, auto

################################################################################
# MIT License                                                                  #
#                                                                              #
# Copyright (c) 2023 ona-li-toki-e-jan-Epiphany-tawa-mi                        #
#                                                                              #
# Permission is hereby granted, free of charge, to any person obtaining a copy #
# of this software and associated documentation files (the "Software"), to     #
# deal in the Software without restriction, including without limitation the   #
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or  #
# sell copies of the Software, and to permit persons to whom the Software is   #
# furnished to do so, subject to the following conditions:                     #
#                                                                              #
# The above copyright notice and this permission notice shall be included in   #
# all copies or substantial portions of the Software.                          #
#                                                                              #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  #
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      #
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS #
# IN THE SOFTWARE.                                                             #
################################################################################
# This file is used to package the data and resource packs in zip files.
#
# Run this script in the project directory. The packaged packs will appear in
#   {output_directory}.
#

################################################################################
# Config START                                                                 #
################################################################################
# The relative path from the project directory to the data pack's directory.
data_pack_directory = 'PigsThatGoBoomInTheNight/'
# The relative path from the project directory to the folder to put the packaged
# packs inside of.
pack_output_directory = 'dist/'
# The file name of the outputted packaged datapack. Do not include a file
# extension.
data_pack_output_file_name = 'PigsThatGoBoomInTheNight'
# A version specifier to add to the end of the outputted files.
pigsthatgoboominthenight_version = 'V0.2.1'

logging_level = logging.NOTSET
################################################################################
# Config END                                                                   #
################################################################################



class PackageType(Enum):
    """ The type of the package to be packed, just used for logging. """
    DATA_PACK     = auto()
    RESOURCE_PACK = auto()

def package_pack(pack_directory: str, output_file_name: str, package_type: PackageType):
    """ Packages the pack at the given directiory and outputs as the given file
        name in the pack output directory. """
    logging_name = "data pack" if package_type == PackageType.DATA_PACK else "resource pack"

    if path.exists(pack_directory):
        logging.info(f'Packaging {logging_name} directory "{path.abspath(pack_directory)}"')

        output_path = path.join(pack_output_directory, output_file_name + '-' + pigsthatgoboominthenight_version)
        make_archive(output_path, 'zip', pack_directory, verbose=True)

        logging.info(f'Outputted packaged {logging_name} -> {path.abspath(output_path)}')
    else:
        logging.error(f'Could not locate directory {path.abspath(pack_directory)}! Skipping {logging_name}')



def main():
    logging.basicConfig(level=logging_level, format='[%(asctime)s] %(levelname)s: %(message)s')

    package_pack(data_pack_directory, data_pack_output_file_name, PackageType.DATA_PACK)

if __name__ == '__main__':
    main()
