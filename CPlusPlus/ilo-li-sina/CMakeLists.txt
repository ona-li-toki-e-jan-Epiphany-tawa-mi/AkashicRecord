# MIT License
#
# Copyright (c) 2022 ona-li-toki-e-jan-Epiphany-tawa-mi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to
# deal in the Software without restriction, including without limitation the
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.

cmake_minimum_required(VERSION 3.0.2)
set(CMAKE_CXX_STANDARD 17)
project(ilo_li_sina VERSION 1.0.0)



if (UNIX)
    set(lawaOS UNIX)
    find_library(TERMINFO NAMES terminfo tinfo curses ncurses ncursesw)
elseif (WIN32)
    set(lawaOS WINDOWS)
endif ()



file(GLOB_RECURSE pokiPiNimiWawa ${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp)
add_executable(${CMAKE_PROJECT_NAME} ${pokiPiNimiWawa})
add_compile_definitions(${lawaOS} NANPA_PI_ILO_PI_ILO_LI_SINA=\"${CMAKE_PROJECT_VERSION}\")
target_link_libraries(${CMAKE_PROJECT_NAME} ${TERMINFO})
#target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)



# li pali e ante toki.
find_package(PythonInterp REQUIRED)
if (NOT ${PYTHON_VERSION_MAJOR} STREQUAL 3)
    message(FATAL_ERROR "ilo pi toki Python pi nanpa 3 anu suli tawa 3 li wile! li kama jo e "
                        ${PYTHON_VERSION_MAJOR} "! (nanpa ali li '" ${PYTHON_VERSION_STRING} "')")
endif()

set(lipuPiPaliPiLipuToki       ${CMAKE_CURRENT_SOURCE_DIR}/paliELipuToki.py)
file(GLOB pokiPiLipuPiNimiToki ${CMAKE_CURRENT_SOURCE_DIR}/nimi_en_toki/*.lang)
set(lipuPiAnteToki             ${CMAKE_CURRENT_SOURCE_DIR}/src/ante_toki/nimi_toki.hpp)

add_custom_command(
    COMMAND ${PYTHON_EXECUTABLE} ${lipuPiPaliPiLipuToki}
    DEPENDS ${pokiPiLipuPiNimiToki} ${lipuPiPaliPiLipuToki}
    OUTPUT  ${lipuPiAnteToki}
    COMMENT "li pali e poki pi nimi nimi en nimi tawa jan"
)
add_custom_target(
    anteToki ALL
    DEPENDS  ${lipuPiAnteToki}
)
add_dependencies(${CMAKE_PROJECT_NAME} anteToki)
