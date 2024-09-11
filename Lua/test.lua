#!/usr/bin/env lua

-- This file is part of elephant_veins.
--
-- Copyright (c) 2024 ona-li-toki-e-jan-Epiphany-tawa-mi
--
-- elephant_veins is free software: you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as published by the
-- Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- elephant_veins is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
-- details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with elephant_veins. If not, see <https://www.gnu.org/licenses/>.

-- elephant_veins testing script.

local luacheck = require "luacheck"
local lfs      = require "lfs"



--- Recursively globs files.
-- @param directory The starting directory.
-- @param ending The ending the files must have to be globbed. Set to "" to glob
-- all files.
-- @param globbed_files The table to store globbed files in.
-- @return globbed_files
local function glob_files(directory, ending, globbed_files)
   for filename in lfs.dir(directory) do
      if "." ~= filename and ".." ~= filename then
         local path = directory .. "/" .. filename
         local attributes = lfs.attributes(path)

         if "directory" == attributes.mode then
            glob_files(path, ending, globbed_files)
         elseif "file" == attributes.mode then
            if "" == ending or ending == string.sub(filename, string.len(filename)
                                                            - string.len(ending)
                                                            + 1) then
               table.insert(globbed_files, path)
            end
         end
      end
   end

   return globbed_files
end




for _, file in pairs(glob_files(".", ".lua", {})) do
   print("Checking", file)

   for _, report in pairs(luacheck.check_files({ file }, {})) do
      for _, issue in pairs(report) do
         print("    " .. luacheck.get_message(issue))
      end
   end
end
