-- This software is licensed under the M.I.T. license.
-- The license text is found in "license.txt"
--
-- Environment class
-- Author: David Bergman
-- env[symbol] : lookup symbol


local M = {}

function M.bind (scope, parms, vals)
  if parms.type == "cons" then
    scope[parms.car.lexeme] = vals.car
    M.bind (scope, parms.cdr, vals.cdr)
  end
end

function M:addBindings (parms, vals)
  local scope = {}
  M.bind (scope, parms, vals)
  return setmetatable (scope, { __index = self })
end


return M
