-- This software is licensed under the M.I.T. license.
-- The license text is found in "license.txt"
--
-- lisp.lua
-- Author: David Bergman
--
-- A Scheme parser
--

require "io_ext"



--[[ ------ ]]--
--[[ Atoms. ]]--
--[[ ------ ]]--


require "object"

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

local Nil      = Atom { "nil"; value = "nil" }
local T        = Atom { "t";   value = "t"   }

local Cons     = Atom { "cons";     _init = { "car", "cdr" } }
local Function = Atom { "function"; _init = { "value", "func", "special" } }
local Number   = Atom { "number";   _init = { "value" } }
local String   = Atom { "string";   _init = { "value" } }
local Symbol   = Atom { "symbol";   _init = { "value" } }



--[[ ------------------- ]]--
--[[ Scanner and parser. ]]--
--[[ ------------------- ]]--


Set = require "fastset"

local isconstant  = Set { "nil", "t" }
local isskipped   = Set { ";", " ", "\t", "\n", "\r" }
local isquote     = Set { ",", "'", "`" }
local isterminal  = Set { "(", ".", ")" } + isquote
local isdelimiter = Set { '"' } + isskipped + isterminal


-- Return the 1-based line number at which offset `i' occurs in `s'.
local function iton (s, i)
  local n = 1
  for _ in string.gmatch (s:sub (1, i), "\n") do n = n + 1 end
  return tostring (n)
end


-- Call `lex' repeatedly to parse `s', yielding a table of
-- (unevaluated) S-expr.
local function parse (s)
  local i, n = 0, #s

  -- Increment index into s and return that character.
  local function nextch ()
    i = i + 1
    if i <= n then return s[i] end
  end

  -- Return the next atom by scanning unconsumed characters of `s'.
  local function lex ()
    local c

    -- Skip initial whitespace and comments.
    repeat
      c = nextch ()

      -- Comments start with `;'.
      if c == ';' then
        repeat
          c = nextch ()
        until c == '\n' or c == '\r' or c == nil
      end

      -- Return end-of-file immediately.
      if c == nil then return nil end

      -- Continue skipping additional lines of comments and whitespace.
    until not isskipped[c]

    -- Return delimiter tokens.
    if isterminal[c] then
      return Atom {c; value = c}
    end

    -- Strings start and end with `"'.
    -- Note we read another character immediately to skip the opening
    -- quote, and don't append the closing quote to the returned token.
    local token = ''
    if c == '"' then
      repeat
        c = nextch ()
        if c == nil then
          error (iton (s, i - 1) .. ': incomplete string: "' .. token, 0)
        elseif c == '\\' then
          c = nextch ()
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

      return String {token}
    end

    -- Anything else is a token of all the characters up to the next
    -- whitespace or delimiter.
    repeat
      token = token .. c
      c = nextch ()
      if c == nil or isdelimiter[c] then
	if not isskipped[c] then
	  -- Don't consume non-skippable characters.
	  i = i - 1
	end

        if token == "nil" then
          -- Literal lisp `nil' constant:
          return Nil

        elseif token == "t" then
          -- Literal lisp `t' constant:
          return T

        elseif token:match ("^%d+$") then
          -- A number:
          return Number {tonumber (token)}

        else
          -- Otherwise a symbol:
          return Symbol {token}
        end
      end
    until false
  end

  local read_list, read_sexpr

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
      return Nil

    elseif atom.kind == "." then
      -- '.' separates CAR and CDR, return the following CDR.
      local cdr = read_sexpr ()

      -- Consume the list closing ')'.
      local j, close = i, lex ()
      if close and close.kind == ")" then
	return cdr
      end

      -- Parse error:
      error (iton (s, j) .. ": missing ')'", 0)

    else
      -- Otherwise, get the first s-expr and cons it with the rest.
      return Cons {read_sexpr (atom), read_list ()}
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

    elseif isquote[atom.kind] then
      -- Cons quotes ("'" and "`") and unquote (",") into the s-expr.
      return Cons {atom, read_sexpr ()}

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


local function bind (scope, parms, vals)
  if parms.kind == "cons" then
    scope[parms.car.value] = vals.car
    bind (scope, parms.cdr, vals.cdr)
  end
end

local function addBindings (env, parms, vals)
  local scope = {}
  bind (scope, parms, vals)
  return setmetatable (scope, { __index = env })
end

-- Apply an environment and get the substituted S-exp
local function applyEnv (env, expr)
  if expr.kind == "cons" then
    return Cons {applyEnv (env, expr.car), applyEnv (env, expr.cdr)}
  elseif expr.kind == "symbol" then
    return env[expr.value] or expr
  end
  return expr
end



--[[ --------------- ]]--
--[[ Lisp Evaluator. ]]--
--[[ --------------- ]]--


local evalquote, evalargs, evalsexpr

function evalquote (env, sexpr)
  local value
  if not sexpr.kind then
    error ("invalid s-expr: " .. tostring (sexpr), 0)
  end
  if sexpr.kind == "cons" then
    local car = sexpr.car
    if car.kind == "," then
      value = evalsexpr (env, sexpr.cdr)
    else
      value = Cons {evalquote (env, car), evalquote (env, sexpr.cdr)}
    end
  else
    value = sexpr
  end
  return value
end


-- Evaluate each item in argument list.
function evalargs (env, list)
  local value
  if list.kind == "cons" then
    value = Cons {evalsexpr (env, list.car), evalargs (env, list.cdr)}
  else
    value = list
  end
  return value
end


function evalsexpr (env, sexpr)
  local value
  if not sexpr.kind then
    error ("invalid s-expr: " .. tostring (sexpr), 0)
  end
  if sexpr.kind == "cons" then
    -- 1. Cons cell
    local car = sexpr.car
    if car.kind == "'" then
      value = sexpr.cdr
    elseif car.kind == "`" then
      local cdr = evalquote (env, sexpr.cdr)
      value = cdr
    else
      local func = evalsexpr (env, car)
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
local function evalstring (env, s)
  local t, errmsg = parse (s)
  if t == nil then
    return nil, errmsg
  end

  local result
  for _, sexpr in ipairs (t) do
    result = evalsexpr (env, sexpr)
  end
  return result
end


-- Evaluate a file of lisp.
local function evalfile (env, filename)
  local s, errmsg = io.slurp (filename)

  if s then
    s, errmsg = evalstring (env, s)
  end

  return s, errmsg
end



--[[ ----------------- ]]--
--[[ Public Interface. ]]--
--[[ ----------------- ]]--


local public = {
  -- Atoms:
  Cons     = Cons,
  Function = Function,
  Nil      = Nil,
  Number   = Number,
  String   = String,
  Symbol   = Symbol,
  T        = T,

  -- Parser:
  parse = parse,

  -- Environments:
  addBindings = addBindings,
  applyEnv    = applyEnv,
  bind        = bind,

  -- Evaluator:
  evalfile   = evalfile,
  evalsexpr  = evalsexpr,
  evalstring = evalstring,
}

return public
