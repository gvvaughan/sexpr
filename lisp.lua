-- This software is licensed under the M.I.T. license.
-- The license text is found in "license.txt"
--
-- lisp.lua
-- Author: David Bergman
--
-- A Scheme parser
--

require "std"

local M = {}



--[[ ------ ]]--
--[[ Atoms. ]]--
--[[ ------ ]]--


local metatable = {}


function atom (kind, value)
  return setmetatable ({ kind = kind, value = value }, metatable)
end


function M.bool (cond)
  return atom ("constant", cond and "t" or "nil")
end


function M.cons (a, b)
  return setmetatable ({ kind = "cons", car = a, cdr = b }, metatable)
end


function M.func (name, func, special)
  return setmetatable ({
      kind = "function", value = name, func = func, special = special
    }, metatable)
end


function M.number (num)
  return atom ("number", num)
end


function M.tostring (sexpr, nested)
  local s = ""
  if sexpr.kind == "cons" then
    -- If we are inside a list, we skip the initial
    -- '('
    if nested then
      s = s .. " "
    else
      s = s .. "("
    end
    s = s .. M.tostring (sexpr.car)

    -- Pretty print the CDR part in list mode
    s = s .. M.tostring (sexpr.cdr, true)

    -- Close with a ')' if we were not in a list mode already
    if not nested then
      s = s .. ")"
    end
  else
    if nested and (sexpr.kind ~= "constant" or sexpr.value ~= "nil") then
      s = s .. " . "
    end
    if sexpr.kind == "function" then
      if sexpr.special == "macro" then
        s = s .. "#macro'"
      else
        s = s .. "#'"
      end
    end
    -- We just add the value, unless we are a nil in the
    -- end of a list...
    if not nested or sexpr.kind ~= "constant" or sexpr.value ~= "nil" then
      if sexpr.kind == "string" then s = s .. '"' end
      s = s .. sexpr.value
      if sexpr.kind == "string" then s = s .. '"' end
    end
  end
  return s
end

metatable.__tostring = M.tostring



--[[ ------------------- ]]--
--[[ Scanner and parser. ]]--
--[[ ------------------- ]]--


local isconstant = set.new { "nil", "t" }
local isoperator = set.new { "(", ")", ",", "'", "`", "." }
local isdelimiter = set.new { ";", " ", "\t", "\n", "\r", '"' } + isoperator


-- Increment index into s and return that character.
local function nextch (s, i)
  return i < #s and s[i + 1] or nil, i + 1
end


-- Lexical scanner: Return three values: `token', `kind', `i', where
-- `token' is the content of the just scanned token, `kind' is the
-- type of token returned, and `i' is the index of the next unscanned
-- character in `s'.
local function lex (s, i)
  local c
  repeat
    c, i = nextch (s, i)

    -- Comments start with `;'.
    if c == ';' then
      repeat
        c, i = nextch (s, i)
      until c == '\n' or c == '\r' or c == nil
    end

    -- Continue skipping additional lines of comments and whitespace.
  until c ~= ' ' and c ~= '\t' and c ~= '\n' and c ~= '\r'

  -- Return end-of-file immediately.
  if c == nil then return nil, "eof", i end

  -- Return delimiter tokens.
  if isoperator[c] then
    return c, "operator", i
  end

  -- Strings start and end with `"'.
  -- Note we read another character immediately to skip the opening
  -- quote, and don't append the closing quote to the returned token.
  local token = ''
  if c == '"' then
    repeat
      c, i = nextch (s, i)
      if c == nil then
        return token, "incomplete string", i - 1
      elseif c == '\\' then
        c, i = nextch (s, i)
	-- `\' can be used to escape `"', `\n' and `\' in strings
	if c ~= '"' and c ~= '\n' and c ~= '\\' then
	  token = token .. '\\'
	end
	if c ~= '\n' then
	  token = token .. c
	end
      elseif c ~= '"' then
	token = token .. c
      end
    until c == '"'

    return token, "string", i
  end

  -- Anything else is a `word' - up to the next whitespace or delimiter.
  repeat
    token = token .. c
    c, i = nextch (s, i)
    if isdelimiter[c] or c == nil then
      local kind = "symbol"
      if isconstant[token] then
        kind = "constant"
      elseif token:match ("^%d+$") then
        token = tonumber (token)
	kind = "number"
      end
      return token, kind, i - 1
    end
  until false
end


-- Call `lex' repeatedly to parse `s', yielding a table of
-- (unevaluated) S-expr.
function M.parse (s)
  local i = 0
  local read, push

  function push (s, start)
    local token, kind, n = lex (s, start)
    if kind == "eof" then
      error ("Token index " .. start ..
             " is out of range when creating CONS S-Expr", 2)
    end

    if kind == "operator" then
      -- If the first token is a '.', return the second token.
      if token == "." then
        local cdr, i = read (s, n)
        -- Skip over the closing ')'.
        token, kind, n = lex (s, i)
        if kind == "eof" or token ~= ")" then
          error("The CDR part ending with " .. tostring (cdr) ..
                " was not followed by a ')'")
        end
        return cdr, n

      -- If the first token is is a ')', return NIL.
      elseif token == ")" then
        return M.bool (nil), n
      end
    end

    -- Otherwise, get the first Sexpr and CONS it with the rest.
    local car, i = read (s, start)
    local cdr, rest = push (s, i)
    return M.cons (car, cdr), rest
  end

  function read (s, i)
    local token, kind, cdr
    token, kind, i = lex (s, i)
    if kind == "eof" then
      return nil, i
    end

    -- If the first token is a '(', we should expect a "list"
    if kind == "operator" then
      if token == "(" then
        return push (s, i)
      end

      cdr, i = read (s, i)
      return M.cons (atom (kind, token), cdr), i
    end

    return atom (kind, token), i
  end

  local sexpr
  local sexprList = {}
  repeat
    sexpr, i = read (s, i)
    if sexpr then
      table.insert (sexprList, sexpr)
    end
  until not sexpr
  return sexprList
end


--[[ ------------- ]]--
--[[ Environments. ]]--
--[[ ------------- ]]--


function M.bind (scope, parms, vals)
  if parms.kind == "cons" then
    scope[parms.car.value] = vals.car
    M.bind (scope, parms.cdr, vals.cdr)
  end
end

function M:addBindings (parms, vals)
  local scope = {}
  M.bind (scope, parms, vals)
  return setmetatable (scope, { __index = self })
end

-- Apply an environment and get the substituted S-exp
function M.applyEnv (env, expr)
  if expr.kind == "cons" then
    return M.cons (M.applyEnv (env, expr.car), M.applyEnv (env, expr.cdr))
  elseif expr.kind == "symbol" then
    return env[expr.value] or expr
  end
  return expr
end



--[[ --------------- ]]--
--[[ Lisp Evaluator. ]]--
--[[ --------------- ]]--


function M.evalQuote (env, sexpr)
  local value
  if not sexpr.kind then
    error ("Invalid S-expr: ", 2)
  end
  if sexpr.kind == "cons" then
    local car = sexpr.car
    if car.kind == "operator" and car.value == "," then
      value = M.evalSexpr (env, sexpr.cdr)
    else	
      value = M.cons (M.evalQuote (env, car), M.evalQuote (env, sexpr.cdr))
    end
  else
    value = sexpr
  end
  return value
end


-- Evaluate each item in a list
local function evalList (env, list)
  local value
  if list.kind == "cons" then
    value = M.cons (M.evalSexpr (env, list.car), evalList (env, list.cdr))
  else
    value = list
  end
  return value
end


function M.evalSexpr (env, sexpr)
  local value
  if not sexpr.kind then
    error ("Invalid S-expr: " .. sexpr, 2)
  end
  if sexpr.kind == "cons" then
    -- 1. Cons cell
    local car = sexpr.car
    if car.kind == "operator" and car.value == "'" then
      value = sexpr.cdr
    elseif car.kind == "operator" and car.value == "`" then
      local cdr = M.evalQuote (env, sexpr.cdr)
      value = cdr
    else
      local func = M.evalSexpr (env, car)
      if not func or func.kind ~= "function" then
        error ("The S-expr did not evaluate to a function: " .. tostring (car))
      end

      -- The function can be either "lazy", in that it deals with eval-
      -- uation of its arguments itself, a "macro", which requires a second
      -- evaluation after the macro expansion, or a regular eager function.
      local args
      if func.special == "lazy" or func.special == "macro"  then
        args = sexpr.cdr
      else
        args = evalList (env, sexpr.cdr)
      end
      value = func.func (env, args)
    end
  elseif sexpr.kind == "symbol" then
    -- a. symbol
    value = env[sexpr.value]
    if not value then
      error ("The symbol '" .. sexpr.value .. "' is not defined")
    end
  else
    -- b. constant
    value = sexpr
  end
  return value
end


-- Evaluate a string of lisp.
function M.evalstring (env, s)
  local t, errmsg = M.parse (s)
  if t == nil then
    return nil, errmsg
  end

  local result
  for _, sexpr in ipairs (t) do
    result = M.evalSexpr (env, sexpr)
  end
  return result
end


-- Evaluate a file of lisp.
function M.evalfile (env, filename)
  local s, errmsg = io.slurp (filename)

  if s then
    s, errmsg = M.evalstring (env, s)
  end

  return s, errmsg
end


return M
