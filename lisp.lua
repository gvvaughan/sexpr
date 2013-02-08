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


local Atom

Atom = Object {
  _init = { "kind" },

  -- Objects are their own metatables, so define metamethods here:
  __tostring = function (sexpr, nested)
    local s = ""
    -- A cons list:
    if sexpr.kind == "cons" then
      return (nested and " " or "(") ..
             tostring (sexpr.car) ..
             Atom.__tostring (sexpr.cdr, true) ..
             (nested and "" or ")")
    end

    -- Separator for cdr of a non-list cons cell:
    if nested and sexpr.kind ~= "nil" then
      s = s .. " . "
    end

    -- A function:
    if sexpr.kind == "function" then
      s = s .. (sexpr.special == "macro" and "#macro'" or "#'")
    end

    -- Any other value, except nil at the end of a cons list:
    if not (nested and sexpr.kind == "nil") then
      local dq = sexpr.kind == "string" and '"' or ""
      s = s .. dq .. sexpr.value .. dq
    end
    return s
  end,
}

M.Nil      = Atom { "nil"; value = "nil" }
M.T        = Atom { "t";   value = "t"   }

M.Cons     = Atom { "cons";     _init = { "car", "cdr" } }
M.Function = Atom { "function"; _init = { "value", "func", "special" } }
M.Number   = Atom { "number";   _init = { "value" } }
M.Operator = Atom { "operator"; _init = { "value" } }
M.String   = Atom { "string";   _init = { "value" } }
M.Symbol   = Atom { "symbol";   _init = { "value" } }



--[[ ------------------- ]]--
--[[ Scanner and parser. ]]--
--[[ ------------------- ]]--


local isconstant = set.new { "nil", "t" }
local isskipped = set.new { ";", " ", "\t", "\n", "\r" }
local isoperator = set.new { ",", "'", "`" }
local issyntax = set.new { "(", ".", ")" }
local isdelimiter = set.new { '"' } + isskipped + isoperator + issyntax


-- Return the 1-based line number at which offset `i' occurs in `s'.
local function iton (s, i)
  local n = 1
  for _ in string.gmatch (s:sub (1, i), "\n") do n = n + 1 end
  return tostring (n)
end


-- Increment index into s and return that character.
local function nextch (s, i)
  return i < #s and s[i + 1] or nil, i + 1
end


-- Call `lex' repeatedly to parse `s', yielding a table of
-- (unevaluated) S-expr.
function M.parse (s)
  local i = 0
  local read_sexpr, read_list

  -- Return the next atom by scanning unconsumed characters of `s'.
  local function lex ()
    local c

    -- Skip initial whitespace and comments.
    repeat
      c, i = nextch (s, i)

      -- Comments start with `;'.
      if c == ';' then
        repeat
          c, i = nextch (s, i)
        until c == '\n' or c == '\r' or c == nil
      end

      -- Continue skipping additional lines of comments and whitespace.
    until c == nil or not isskipped[c]

    -- Return end-of-file immediately.
    if c == nil then return nil end

    -- Syntax tokens are consumed by parse(), so it is an error for them
    -- to ever appear in the assembled parse tree; hence `#error' value.
    if issyntax[c] then
      return Atom {c; value = "#error"}
    end

    -- Return delimiter tokens.
    if isoperator[c] then
      return M.Operator {c}
    end

    -- Strings start and end with `"'.
    -- Note we read another character immediately to skip the opening
    -- quote, and don't append the closing quote to the returned token.
    local token = ''
    if c == '"' then
      repeat
        c, i = nextch (s, i)
        if c == nil then
          error (iton (s, i - 1) .. ': incomplete string: "' .. token, 0)
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

      return M.String {token}
    end

    -- Anything else is a token of all the characters up to the next
    -- whitespace or delimiter.
    repeat
      token = token .. c
      c, i = nextch (s, i)
      if c == nil or isdelimiter[c] then
	if not isskipped[c] then
	  -- Don't consume non-skippable characters.
	  i = i - 1
	end

        if token == "nil" then
          -- Literal lisp `nil' constant:
          return M.Nil

        elseif token == "t" then
          -- Literal lisp `t' constant:
          return M.T

        elseif token:match ("^%d+$") then
          -- A number:
          return M.Number {tonumber (token)}

        else
          -- Otherwise a symbol:
          return M.Symbol {token}
        end
      end
    until false
  end

  function read_list (atom)
    if atom == nil then
      -- When called without an argument, read the next atom here:
      atom = lex ()
    end

    if atom == nil then
      -- Parse error: end-of-file between '(' and ')'.
      error (iton (s, i) .. ": unexpected end-of-file", 0)

    elseif atom.kind == ")" then
      -- ')' is the end of the list, return NIL.
      return M.Nil

    elseif atom.kind == "." then
      -- '.' separates CAR and CDR, return the following CDR.
      local cdr = read_sexpr ()

      -- Consume the list closing ')'.
      local n, close = i, lex ()
      if close and close.kind == ")" then
	return cdr
      end

      -- Parse error:
      error (iton (s, n) .. ": missing ')'", 0)

    else
      -- Otherwise, get the first s-expr and cons it with the rest.
      return M.Cons {read_sexpr (atom), read_list ()}
    end
  end

  function read_sexpr (atom)
    if atom == nil then
      -- When called without an argument, read the next atom here:
      atom = lex ()
    end

    if atom == nil then
      -- Return from end-of-file immediately.
      return nil

    elseif atom.kind == "(" then
      -- '(' indicates the beginning of a list.
      return read_list ()

    elseif atom.kind == "operator" then
      -- Cons quotes ("'" and "`") and unquote (",") into the s-expr.
      return M.Cons {atom, read_sexpr ()}

    else
      -- Otherwise, return the (non-list) s-expr.
      return atom
    end

    -- TODO: What about "." and ")"?
  end

  local sexprlist = {}
  repeat
    local sexpr = read_sexpr ()
    if sexpr == nil then break end
    table.insert (sexprlist, sexpr)
  until false

  return sexprlist
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
    return M.Cons {M.applyEnv (env, expr.car), M.applyEnv (env, expr.cdr)}
  elseif expr.kind == "symbol" then
    return env[expr.value] or expr
  end
  return expr
end



--[[ --------------- ]]--
--[[ Lisp Evaluator. ]]--
--[[ --------------- ]]--


local function evalquote (env, sexpr)
  local value
  if not sexpr.kind then
    error ("invalid s-expr: " .. tostring (sexpr), 0)
  end
  if sexpr.kind == "cons" then
    local car = sexpr.car
    if car.kind == "operator" and car.value == "," then
      value = M.evalsexpr (env, sexpr.cdr)
    else
      value = M.Cons {evalquote (env, car), evalquote (env, sexpr.cdr)}
    end
  else
    value = sexpr
  end
  return value
end


-- Evaluate each item in argument list.
local function evalargs (env, list)
  local value
  if list.kind == "cons" then
    value = M.Cons {M.evalsexpr (env, list.car), evalargs (env, list.cdr)}
  else
    value = list
  end
  return value
end


function M.evalsexpr (env, sexpr)
  local value
  if not sexpr.kind then
    error ("invalid s-expr: " .. tostring (sexpr), 0)
  end
  if sexpr.kind == "cons" then
    -- 1. Cons cell
    local car = sexpr.car
    if car.kind == "operator" and car.value == "'" then
      value = sexpr.cdr
    elseif car.kind == "operator" and car.value == "`" then
      local cdr = evalquote (env, sexpr.cdr)
      value = cdr
    else
      local func = M.evalsexpr (env, car)
      if not func or func.kind ~= "function" then
        error ("symbol's function definition is void: " .. tostring (car), 0)
      end

      -- The function can be either "lazy", in that it deals with eval-
      -- uation of its arguments itself, a "macro", which requires a second
      -- evaluation after the macro expansion, or a regular eager function.
      local args
      if func.special == "lazy" or func.special == "macro"  then
        args = sexpr.cdr
      else
        args = evalargs (env, sexpr.cdr)
      end
      value = func.func (env, args)
    end
  elseif sexpr.kind == "symbol" then
    -- a. symbol
    value = env[sexpr.value]
    if not value then
      error ("undefined symbol '" .. sexpr.value .. "'", 0)
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
    result = M.evalsexpr (env, sexpr)
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
