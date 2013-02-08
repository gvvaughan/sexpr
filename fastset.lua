-- Fast sets (compared to lua-stdlib)
--
-- Copyright (c) 2013 Free Software Foundation, Inc.
-- Written by Gary V. Vaughan, 2013
--
-- This program is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 3, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to the
-- Free Software Foundation, Fifth Floor, 51 Franklin Street, Boston,
-- MA 02111-1301, USA.


-- Extremely cut down implementation of sets with only three features
-- (aside from x5 speed-up for member lookups):
--   1. construct a new set with `a_set = Set {m, n...}'
--   2. extend a set with `another = Set {o, p...} + a_set'
--   3. check set membership with `a_set[possible_member]'

local _mt, Set

_mt = {
  -- Return a new set containing the union of values from s and t.
  __add = function (s, t)
    local r = Set ()
    for k in pairs (s) do rawset (r, k, true) end
    for k in pairs (t) do rawset (r, k, true) end
    return r
 end,
}


Set = setmetatable ({}, {
  -- Return a new set containing values from t.
  __call = function (s, t)
    local r = setmetatable ({}, _mt)
    if t ~= nil then
      for _, v in pairs (t) do rawset (r, v, true) end
    end
    return r
  end,
})

return Set
