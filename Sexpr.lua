-- This software is licensed under the M.I.T. license.
-- The license text is found in "license.txt"
--
-- Sexpr.lua
-- Author: David Bergman
--
-- Deals with (unevaluated or not) S-expressions, which are simply
-- atoms or CONS cells
--
-- The atoms types are
-- 1. constant (t or nil)
-- 2. string
-- 3. number
-- 4. operator [',`]
-- 5. symbol
-- 6. function
-- 8. macro

local M = {}

local metatable = { __tostring = M.prettyPrint }

-- Atoms

function M.newAtom (type, lexeme)
  return setmetatable ({ type = type, lexeme = lexeme }, metatable)
end

function M.newBool (cond)
  return M.newAtom ("constant", cond and "t" or "nil")
end

-- Create a new function reference, where the
-- special parameter can be nil (for a normal function)
-- or 'lazy' for functions handling their own internal
-- evaluation, or 'macro' for functions mereley replacing
-- their body for further evaluation
function M.newFun(name, fun, special)
  return { type = "function", lexeme = name, fun = fun, special = special }
end

function M:car()
   return self.car
end

function M:cdr()
   return self.cdr
end

function M.cons(a, b)
  return setmetatable ({ type="cons", car = a, cdr = b }, metatable)
end


-- Pretty printer
function M.prettyPrint (sexpr, inList)
  local s = ""
  if sexpr.type == "cons" then
    -- If we are inside a list, we skip the initial
    -- '('
    if inList then
      s = s .. " "
    else
      s = s .. "("
    end
    s = s .. M.prettyPrint (sexpr.car)

    -- Pretty print the CDR part in list mode
    s = s .. M.prettyPrint (sexpr.cdr, true)

    -- Close with a ')' if we were not in a list mode already
    if not inList then
      s = s .. ")"
    end
  else
    if inList and (sexpr.type ~= "constant" or sexpr.lexeme ~= "nil") then
      s = s .. " . "
    end
    if sexpr.type == "function" then
      if sexpr.special == "macro" then
        s = s .. "#macro'"
      else
        s = s .. "#'"
      end
    end
    -- We just add the lexeme, unless we are a nil in the
    -- end of a list...
    if not inList or sexpr.type ~= "constant" or sexpr.lexeme ~= "nil" then
      if sexpr.type == "string" then s = s .. '"' end
      s = s .. sexpr.lexeme
      if sexpr.type == "string" then s = s .. '"' end
    end
  end
  return s
end

return M
