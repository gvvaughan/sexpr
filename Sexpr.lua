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

local metatable = {
    __tostring = function (expr)
      if expr.type == "cons" then
        return "(" .. tostring(expr.car) .. " . " .. tostring(expr.cdr) .. ")"
      end
      return "atom[type=" .. expr.type .. ', lexeme="' .. expr.lexeme .. '"]'
     end,
}

-- Atoms

function M.newAtom (type, lexeme)
  return setmetatable ({ type = type, lexeme = lexeme }, metatable)
end

function M.newBool (cond)
  return M.newAtom ("constant", cond and "t" or "nil")
end

function M.newString (s)
  return M.newAtom ("string", s)
end

function M.newOperator (op)
  local type = "operator"
  if op == "(" then
    type = "open paren"
  elseif op == ")" then
    type = "close paren"
  end
  return M.newAtom (type, op)
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
function M.prettyPrint(sexpr, inList)
  local pretty
  if sexpr.type == "cons" then
    local str = {}
    -- If we are inside a list, we skip the initial
    -- '('
    if inList then
      table.insert(str, " ")
    else
      table.insert(str, "(")
    end
    table.insert(str, M.prettyPrint(sexpr.car))      
      
    -- Pretty print the CDR part in list mode
    table.insert(str, M.prettyPrint(sexpr.cdr, true))
     
    -- Close with a ')' if we were not in a list mode already
    if not inList then
      table.insert(str, ")")
    end
    pretty = table.concat(str)
  else
    local str = {}
    if inList and (sexpr.type ~= "constant" or sexpr.lexeme ~= "nil") then
      table.insert(str, " . ")
    end
    if sexpr.type == "function" then
      if sexpr.special == "macro" then
        table.insert(str, "#macro'")
      else
        table.insert(str, "#'")
      end
    end
    -- We just add the lexeme, unless we are a nil in the
    -- end of a list...
    if not inList or sexpr.type ~= "constant" or  sexpr.lexeme ~= "nil" then
      if sexpr.type == "string" then table.insert(str, '"') end
      table.insert(str, sexpr.lexeme)
      if sexpr.type == "string" then table.insert(str, '"') end
    end
    pretty = table.concat(str)
  end
  return pretty
end

return M