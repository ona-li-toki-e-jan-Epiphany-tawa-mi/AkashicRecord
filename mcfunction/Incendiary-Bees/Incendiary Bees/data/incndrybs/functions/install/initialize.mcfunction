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
# Runs the installation process.
# Do not call directly, but through incndrybs:try_install, as it only runs when not already installed (unless you
#   want to reset the variables.)
#

# Create scoreboard in case of force-install and scoreboard does not exist for some reason.
# Used for math and general data storage.
scoreboard objectives add incndrybs dummy
# Flags that installation has occured.
scoreboard players set installed incndrybs 1

# The amount of ticks between each bomb dropped by a bee.
scoreboard players set bee_bomb_cooldown incndrybs 15
# The explosion power of bee bombs.
scoreboard players set bee_bomb_power incndrybs 10

# Used to put in a delay on how often bees can drop a bomb.
scoreboard objectives add incndrybs_bomb_cooldown dummy



tellraw @a {"text":"===================================================","color":"gold"}
tellraw @a {"text":"Successfully installed Incendiary Bees!","color":"gold"}
tellraw @a [{"text":"To uninstall, run '","color":"gold"},{"text":"/function incndrybs:uninstall","color":"white","bold":true,"clickEvent":{"action":"run_command","value":"/function incndrybs:uninstall"},"hoverEvent":{"action":"show_text","value":{"text":"Click to run command","italic":true}}},{"text":"' and remove it from the world's datapack directory"}]
tellraw @a {"text":"===================================================","color":"gold"}
