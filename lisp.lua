-- A lisp parser and evaluator.
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


require "io_ext"



--[[ ----------- ]]--
--[[ Lisp Atoms. ]]--
--[[ ----------- ]]--

-- The parser returns a Lua list of s-expressions built from
-- the following atoms.  Nil and T are singletons so that
-- equality checks do the right thing, and some syntactical
-- tokens such as quotes and commas are cloned directly from
-- the Atom prototype object with `kind' set to the terminal
-- character.


require "object"

local Atom

Atom = Object {
  _init = { "kind" },

  __tostring = function (sexpr, nested)
    local s = ""
    if sexpr.kind == "cons" then
      return (nested and " " or "(") ..
             tostring (sexpr.car) ..
             Atom.__tostring (sexpr.cdr, true) ..
             (nested and "" or ")")
    end

    if nested and sexpr.kind ~= "nil" then
      s = s .. " . "
    end

    if sexpr.kind == "function" then
      s = s .. (sexpr.special == "macro" and "#macro'" or "#'")
    end

    -- Ignore the nil at the end of a cons list.
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

-- The parser is a closure over the lex function, and several
-- helpers, which allows them all to reference the shared state
-- of the parser in the closure to simplify the implementation.
-- parse () adds s-expressions of Atoms to a list by calling
-- read_sexpr () and read_list () recursively.  The each call
-- lex () to collect Atoms, which are tokenized by lex (). The
-- nextch () function keeps track of where in the string parsing
-- has reached, and passes characters to lex () one at a time
-- as it decides what Atoms to produce for the parser.


Set = require "fastset"

local isskipped   = Set { ";", " ", "\t", "\n", "\r" }
local isquote     = Set { ",", "'", "`" }
local isterminal  = Set { "(", ".", ")" } + isquote
local isdelimiter = Set { '"' } + isskipped + isterminal


-- Return the line-number at which index I occurs in S.
local function iton (s, i)
  local n = 1
  for _ in string.gmatch (s:sub (1, i), "\n") do n = n + 1 end
  return tostring (n)
end


-- Parse S, a string of lisp code, returning a list of (unevaluated)
-- s-expressions.
local function parse (s)
  local i, n = 0, #s

  local function parse_error (errmsg, index)
    index = index or i
    -- Final 0 argument means not to append Lua backtrace texts.
    error (iton (s, index) .. ": " .. errmsg, 0)
  end

  local function nextch ()
    i = i + 1
    if i <= n then return s[i] end
  end

  -- Tokenizer.
  local function lex ()
    local c

    -- Skip whitespace and comments.
    repeat
      c = nextch ()
      if c == ';' then
        repeat
          c = nextch ()
        until c == '\n' or c == '\r' or c == nil
      end
      if c == nil then return nil end
    until not isskipped[c]

    -- Return terminal tokens in an Atom `kind' field.
    if isterminal[c] then
      return Atom {c; value = c}
    end

    -- Strings start and end with `"'.
    local token = ''
    if c == '"' then
      repeat
        c = nextch ()
        if c == nil then
          parse_error ('incomplete string: "' .. token, i - 1)
        elseif c == '\\' then
          c = nextch ()
          -- `\' can be used to escape `"', `\n' and `\' in strings.
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

    -- Everything else!
    repeat
      token = token .. c
      c = nextch ()
      if c == nil or isdelimiter[c] then
	if not isskipped[c] then
	  -- Don't consume non-skippable characters.
	  i = i - 1
	end

        if token == "nil" then
          return Nil

        elseif token == "t" then
          return T

        elseif token:match ("^%d+$") then
          return Number {tonumber (token)}

        else
          return Symbol {token}
        end
      end
    until false
  end

  local read_list, read_sexpr -- mutually recursive functions

  function read_list (atom)
    atom = atom or lex ()

    if atom == nil then
      -- reached end-of-file between '(' and ')'.
      parse_error "unexpected end-of-file"

    elseif atom.kind == ")" then
      -- ')' is the end of the list, return NIL.
      return Nil

    elseif atom.kind == "." then
      -- '.' separates CAR and CDR, return the following CDR.
      local cdr = read_sexpr ()

      -- Consume the list-closing ')'.
      local j, close = i, lex ()
      if close and close.kind == ")" then
	return cdr
      end

      parse_error ("missing ')'", j)
    else
      -- Otherwise, get the first s-expr and cons it with the rest.
      return Cons {read_sexpr (atom), read_list ()}
    end
  end

  function read_sexpr (atom)
    atom = atom or lex ()

    if atom == nil then
      -- No more Atoms.
      return nil

    elseif atom.kind == "(" then
      return read_list ()

    elseif isquote[atom.kind] then
      return Cons {atom, read_sexpr ()}

    else
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

-- Environments are nested symbol tables used to provide scopes
-- in which symbol values are stored and looked up.


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

-- The main function here is evalsexpr () which substitutes symbol
-- names for values and/or calls functions according to the active
-- environment.  evalstring () and evalfile () are convenience
-- wrappers for evalsexpr (), and the rest are helper functions.


local evalquote, evalargs, evalsexpr


-- Evaluate only "," sub-epressions of quoted SEXPR inside ENV.
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


-- Evaluate each item in argument LIST inside ENV.
function evalargs (env, list)
  local value
  if list.kind == "cons" then
    value = Cons {evalsexpr (env, list.car), evalargs (env, list.cdr)}
  else
    value = list
  end
  return value
end


-- Evaluate SEXPR inside ENV.
function evalsexpr (env, sexpr)
  local value
  if not sexpr.kind then
    error ("invalid s-expr: " .. tostring (sexpr), 0)
  end
  if sexpr.kind == "cons" then
    local car = sexpr.car
    if car.kind == "'" then
      -- A quoted expression is protected from further evaluation.
      value = sexpr.cdr

    elseif car.kind == "`" then
      -- Comma expressions inside a back-quoted expression ARE evaluated.
      local cdr = evalquote (env, sexpr.cdr)
      value = cdr

    else
      -- Otherwise the first symbol of a CONS list should be a function.
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
    value = env[sexpr.value]
    if not value then
      error ("undefined symbol '" .. sexpr.value .. "'", 0)
    end
  else
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

-- Return a table of the public interface to this file.

local public = {
  -- Lisp Atoms:
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
