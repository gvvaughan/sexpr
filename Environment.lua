-- This software is licensed under the M.I.T. license.
-- The license text is found in "license.txt"
--
-- Environment class
-- Author: David Bergman
-- Env.new : Creating a new environment
-- env[symbol] : lookup symbol (i.e., symbol:lookup(symbol) )
-- Env:lookup : lookup symbol
-- Env:addScope : add local scope


local M = {}

local metatable = {
  __index = function (self, key)
    -- Lookup a symbol, going from the most local to the most global scope
    for i = self.scopeCount, 1, -1 do
      local tab = self.scopes[i]
      local val = tab[key]
      if val then
        return val
      end
    end
    return nil
  end,

  __newindex = function (self, key, value)
    -- Add a new key or change an existing one in the most local scope
    self.scopes[self.scopeCount][key] = value
    return value
  end,

  __tostring = function (self)
    local str = {}
    table.insert(str, "Environment[\n    scopeCount=" .. self.scopeCount .. "\n")
    local prefix=""
    for _, scope in ipairs (self.scopes) do
      local newprefix = "\t" .. prefix
      table.insert (str, prefix .. "    Scope[\n")
      for key, value in pairs (scope) do
        table.insert (str,
          newprefix .. tostring (key) .. "\t= " .. tostring (value)..",\n")
      end
      table.insert (str, prefix .. "    ]\n")
      prefix = newprefix
    end
    table.insert (str, "]\n")
    return table.concat (str)
  end,
}

function M:addBindings(formalList, actualList)
  local localScope = {}
  M.bind (localScope, formalList, actualList)
  return self:addLocalScope (localScope)
end

function M.bind (scope, formalList, actualList)
  if formalList.type == "cons" then
    scope[formalList.car.lexeme] = actualList.car
    M.bind (scope, formalList.cdr, actualList.cdr)
  end
end

-- Create local scope and return new extended environment
function M:addLocalScope (localScope)
  -- Add a new empty local scope
  local newScopes = {}
  for _, scope in ipairs (self.scopes) do
    table.insert (newScopes, scope)
  end
  table.insert (newScopes, localScope)

  return setmetatable ({
    scopeCount    = self.scopeCount + 1,
    scopes        = newScopes,
    addBindings   = M.addBindins,
    addLocalScope = M.addLocalScope,
  }, metatable)
end

function M.new (initialScope)
  -- The scopes are stored from most global to most local
  return setmetatable ({
    scopeCount    = 1,
    scopes        = { initialScope },
    addBindings   = M.addBindings,
    addLocalScope = M.addLocalScope,
  }, metatable)
end

return M
