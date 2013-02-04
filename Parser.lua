-- This software is licensed under the M.I.T. license.
-- The license text is found in "license.txt"
--
-- Parser.lua
-- Author: David Bergman
--
-- A Scheme parser
--

require "std"

Sexpr = require "Sexpr"

local M = {}

local isconstant = set.new { "nil", "t" }
local isoperator = set.new { "(", ")", ",", "'", "`", "." }

local function newToken (t)
  local s = type (t) == "table" and table.concat (t) or t

  if isconstant[s] then return Sexpr.newAtom ("constant", s) end
  if s:match ("^%d+$") then return Sexpr.newAtom ("number", tonumber (s)) end
  return Sexpr.newAtom ("symbol", s)
end


-- Parse a sub expression, returning both an expression and
-- the index following this sub expression
function M.parseTokens (expr)
  tokens = {}

  -- We do it character by character, using queues to
  -- handle strings as well as regular lexemes

  local currentToken = {}
  local inString = false
  local isEscaping = false

  for i = 1, #expr do
    local c = expr:sub (i, i)
    -- 1. Escaping this character, whether in a string or not
    if isEscaping then
      table.insert (currentToken, c)	
      isEscaping = false

    -- 2. An escape character
    elseif c == "\\" then
      isEscaping = true

    -- 3. A quotation mark
    elseif c == '"'  then
      -- Two sub cases:
      if not inString then
        -- a. starting a new string
        -- If we already had a token, let us finish that
        -- up first
        if #currentToken > 0 then
          table.insert (tokens, newToken (table.concat (currentToken)))
        end
      else
        -- b. ending a string
        table.insert (tokens, Sexpr.newString (table.concat (currentToken)))
      end	
      currentToken = {}
      inString = false

    -- 4. inside a string, so just add the character
    elseif inString then
      table.insert (currentToken, c)

    -- 5. special operator (and not inside string)
    elseif isoperator[c] then
      -- We add any saved token
      if #currentToken > 0 then
        table.insert (tokens, newToken (table.concat (currentToken)))
        currentToken = {}
      end
      table.insert (tokens, Sexpr.newOperator(c))

    -- 6. A blank character, which should add the current token, if any
    elseif c:find ("%s") then
      if #currentToken > 0 then
        table.insert(tokens, newToken (table.concat (currentToken)))
        currentToken = {}
      end

    -- 7. A non-blank character being part of the symbol
    else
      table.insert (currentToken, c)
    end
  end

  -- Add any trailing token...
  if #currentToken > 0 then
    if inString then
      table.insert (tokens, Sexpr.newString (table.concat (currentToken)))
    else
      table.insert (tokens, newToken (table.concat (currentToken)))
    end
  end

  return tokens
end


local createSexpr, createCons

-- If the first token is a '.', we just return the second token, as is,
-- while skipping a subsequent ')', else if it is a ')' we return NIL,
-- else we get the first Sexpr and CONS it with the rest.
function createCons (tokens, start)
  local firstTok = tokens[start]
  if not firstTok then
    error ("Token index " .. start ..
           " is out of range when creating CONS S-Expr", 2)
  end

  if firstTok.type == "operator" and firstTok.lexeme == "." then
    -- We skip the last ')'
    local i, cdr = createSexpr (tokens, start+1)
    if not tokens[i] or tokens[i].type ~= "close paren" then
      error("The CDR part ending with " .. tokens[i - 1].lexeme ..
            " was not followed by a ')'")
    end
    return i + 1, cdr
  elseif firstTok.type == "close paren" then
    return start + 1, newToken ("nil")
  else
    local i, car = createSexpr (tokens, start)
    local rest, cdr = createCons (tokens, i)
    return rest, Sexpr.cons (car, cdr)
  end
end

function createSexpr (tokens, start)
  -- If the first token is a '(', we should expect a "list"
  local firstToken = tokens[start]
  if not firstToken then
    return start, nil
  end
  if firstToken.type == "open paren" then
    return createCons (tokens, start + 1)
  elseif firstToken.type == "operator" then
    local i, cdr = createSexpr (tokens, start + 1)
    return i, Sexpr.cons (firstToken, cdr)
  else
    return start + 1, firstToken
  end
end

-- Parse the code snippet, yielding a list of (unevaluated) S-expr
function M.parseSexpr (expr)
  local tokenList = M.parseTokens (expr)
  local expr
  local i = 1
  local sexprList = {}
  repeat
    i, sexpr = createSexpr (tokenList, i)
    if sexpr then
      table.insert (sexprList, sexpr)
    end
  until not sexpr
  return sexprList
end


return M
