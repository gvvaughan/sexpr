-- Copyright (c) 2013-2014 Free Software Foundation, Inc.
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

--[[--
 A lisp parser and evaluator.
 @module sexpr.lisp
]]


local io     = require "std.io"
local Object = require "std.object"



--[[ =========== ]]--
--[[ Lisp Atoms. ]]--
--[[ =========== ]]--


--- Pretty print an S-Expression.
-- @function __tostring
-- @tparam Atom sexpr an S-Expression
-- @param[opt] nested non-nil when outer parens have been emitted
-- @treturn string pretty string representation of `sexpr`
local function stringify (sexpr, nested)
  local s = ""
  if sexpr.kind == "cons" then
    return (nested and " " or "(") ..
            tostring (sexpr.car) ..
	    stringify (sexpr.cdr, "nested") ..
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
    if sexpr.kind == "string" then
      -- Quote and escape a string properly.
      s = s .. string.format ('"%s"', sexpr.value:gsub ('["\\]', "\\%0"))
    else
      s = s .. (sexpr.name or sexpr.value)
    end
  end
  return s
end


-- The parser returns a Lua list of s-expressions built from
-- the following atoms.  Nil and T are singletons so that
-- equality checks do the right thing, and some syntactical
-- tokens such as quotes and commas are cloned directly from
-- the Atom prototype object with `kind' set to the terminal
-- character.


--- Root object.
-- A `std.object` derived base type for lisp atoms.
-- @function Atom
-- @string kind type identifier
-- @param ... additional fields
local Atom     = Object { _init = { "kind" }, __tostring = stringify }


--- Nil
-- @function Nil
-- @treturn Nil singleton `nil` valued atom.
local Nil      = Atom { "nil"; value = "nil" }


--- T
-- @function T
-- @treturn T singleton `t` valued atom.
local T        = Atom { "t";   value = "t"   }


--- Cons cell.
-- @function Cons
-- @param[opt] car address register contents
-- @param[opt] cdr decrement register contents
-- @treturn Cons a new cons atom, with `car` and `cdr` set.
local Cons     = Atom { "cons";     _init = { "car", "cdr" } }


--- Function atom.
-- @function Function
-- @string name name of the function
-- @func func handler function
-- @string[opt=nil] special "lazy" for special forms that evaluate their
--   own parameters, or "macro" for forms that output an sexpr for
--   further evaluation
-- @treturn Function a new function atom
local Function = Atom { "function"; _init = { "name", "func", "special" } }


--- Number atom.
-- @function Number
-- @number value a number
-- @treturn Number a new number atom
local Number   = Atom { "number";   _init = { "value" } }


--- String atom.
-- @function String
-- @string value a string
-- @treturn String a new string atom
local String   = Atom { "string";   _init = { "value" } }


--- Symbol atom.
-- The parser produces these when it encounters symbol read syntax in
-- the input stream.
-- @function Symbol
-- @string name symbol name
-- @treturn Symbol a new symbol atom.
local Symbol   = Atom { "symbol";   _init = { "name" } }



--[[ ======== ]]--
--[[ Obarray. ]]--
--[[ ======== ]]--


--- Global symbol table.
-- A mapping of symbol-names to symbol-values.  _Interned_ symbols are
-- stored here.
--
-- There is no way to access the contents of the global symol table,
-- except to use the mapatoms function.
-- @table obarray
local obarray = {}


--- Intern a symbol.
-- @string name symbol name
-- @tparam[opt=obarray] table env an environment table
-- @treturn Symbol interned symbol.
local function intern (name, env)
  env = env or obarray
  if not env[name] then
    env[name] = Symbol {name}
  end
  return env[name]
end


--- Check whether `name` was previously interted.
-- @string name possibly interned name
-- @tparam[opt=obarray] table env an environment table
-- @return symbol previously interned wih `name`, or `nil`
local function intern_soft (name, env)
  return (env or obarray)[name]
end


------
-- Function signature for `mapatoms` callback function.
-- @function map_cb
-- @tparam Symbol symbol a symbol
-- @return if `true` then mapatoms returns immediately.


--- Call a function on every symbol in `obarray`.
-- If `func` returns `true`, mapatoms returns immediately.
-- @tparam map_cb func a function
-- @tparam[opt=obarray] table env an environment table
-- @return `true` if `func` signalled early exit, otherwise `nil`
local function mapatoms (func, env)
  for _, symbol in pairs (env or obarray) do
    if func (symbol) == true then return true end
  end
end



--[[ ========== ]]--
--[[ Utilities. ]]--
--[[ ========== ]]--


--- Recursively append elements to an existing list.
-- The last argument is not copied, just used as the tail of the new list.
-- @tparam Atom first head of a new cons list
-- @tparam Atom rest list of addional elements to append
-- @treturn Cons a single cons list of `first` followed by elements of
--   `rest`
local function append (first, rest)
  if rest.cdr ~= Nil and rest.cdr.car.kind == "cons" then
    -- Concatenate REST to a single list.
    rest = Cons {append (rest.car, rest.cdr), Nil}
  end
  if first == Nil then
    return rest.car
  elseif first.kind ~= "cons" then
    error ("non-sequence argument to append: " .. tostring (first), 0)
  end
  return Cons {first.car, append (first.cdr, rest)}
end



--[[ =================== ]]--
--[[ Scanner and parser. ]]--
--[[ =================== ]]--


-- The parser is a closure over the lex function, and several
-- helpers, which allows them all to reference the shared state
-- of the parser in the closure to simplify the implementation.
-- parse () adds s-expressions of Atoms to a list by calling
-- read_sexpr () and read_list () recursively.  The each call
-- lex () to collect Atoms, which are tokenized by lex (). The
-- nextch () function keeps track of where in the string parsing
-- has reached, and passes characters to lex () one at a time
-- as it decides what Atoms to produce for the parser.


local set = require "std.set"

local isspace     = set.new { " ", "\t", "\n", "\r" }
local isskipped   = set.new { ";" } + isspace
local isquote     = set.new { ",", ",@", "'", "`" }
local isterminal  = set.new { "(", ".", ")" } + isquote
local isdelimiter = set.new { '"' } + isskipped + isterminal


-- Return the line-number at which index I occurs in S.
local function iton (s, i)
  local n = 1
  for _ in string.gmatch (s:sub (1, i), "\n") do n = n + 1 end
  return tostring (n)
end


--- Lisp parser.
-- @string s a string of lisp code.
-- @treturn Cons a list of (unevaluated) s-expressions.
local function parse (s)
  local i, n = 0, #s

  local function parse_error (errmsg, index)
    index = index or i
    -- Final 0 argument means not to append Lua backtrace texts.
    error (iton (s, index) .. ": parse error: " .. errmsg, 0)
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

    -- Look ahead for potential ,@ token.
    if c == "," then
      local lookahead = nextch ()
      if lookahead == "@" then
        return Atom {",@"; value = ",@"}
      end

      -- Not a ",@", so fall through to terminal token handler.
      i = i - 1
    end

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
	  c = nil -- in case it holds " which would exit the loop
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
	if not isspace[c] then
	  -- Don't consume non-space characters.
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

    elseif atom.kind == ")" then
      parse_error "unmatched ')'"

    elseif isterminal[atom.kind] then
      -- Raise an error for terminals not handled above.
      parse_error ("unexpected '" .. atom.kind .. "'")

    else
      return atom
    end
  end

  local sexprlist = {}
  repeat
    local sexpr = read_sexpr ()
    if sexpr == nil then break end
    table.insert (sexprlist, sexpr)
  until false

  return sexprlist
end



--[[ ============= ]]--
--[[ Environments. ]]--
--[[ ============= ]]--

-- Environments are nested symbol tables used to provide scopes
-- in which symbol values are stored and looked up.


--- Recursively bind arguments to parameters.
-- @tparam table env an environment table
-- @tparam Cons paramlist a list of parameters
-- @tparam Cons arglist a list of arguments
-- @treturn table modified `env`
local function env_bind (env, paramlist, arglist)
  if paramlist.kind ~= "cons" then
    return env
  end
  local param = intern (paramlist.car.name, env)
  param.value = arglist.car
  return env_bind (env, paramlist.cdr, arglist.cdr)
end


--- Make a new local environment.
-- @tparam table env an existing environment table
-- @treturn table a new local environment "inside" `env`
local function env_push (env)
  return setmetatable ({}, { __index = env })
end


--- Partial evaluation.
-- Recursively substitute Symbol elements in `sexpr` for the values
-- bound to them in `env`
-- @tparam table env an environment table
-- @tparam Cons sexpr an S-Expression
-- @treturn Cons `sexpr` with Symbol elements applied
local function env_apply (env, sexpr)
  if sexpr.kind == "cons" then
    return Cons {env_apply (env, sexpr.car), env_apply (env, sexpr.cdr)}
  elseif sexpr.kind == "symbol" then
    return (intern_soft (sexpr.name, env) or {}).value or sexpr
  end
  return sexpr
end



--[[ =============== ]]--
--[[ Lisp Evaluator. ]]--
--[[ =============== ]]--

-- The main function here is evalsexpr () which substitutes symbol
-- names for values and/or calls functions according to the active
-- environment.  evalstring () and evalfile () are convenience
-- wrappers for evalsexpr (), and the rest are helper functions.


local evalquote, evalargs, evalsexpr


--- Evaluate only escaped sub-expressions of quoted `sexpr`.
-- @tparam table env an environment table
-- @tparam Atom sexpr the balance of a "`" quoted S-Expression
-- @treturn Atom `sexpr` with commas and splices evaluated
function evalquote (env, sexpr)
  if sexpr.kind == "cons" then
    local car = sexpr.car
    if car.kind == "," then
      -- Unquote s-expression following "," operator.
      return evalsexpr (env, sexpr.cdr)

    elseif car.kind == "cons" and car.car.kind == ",@" then
      -- Splice s-expression following ",@" operator.
      local rest = Cons {evalquote (env, sexpr.cdr), Nil}
      return append (evalsexpr (env, car.cdr), rest)

    else
      return Cons {evalquote (env, car), evalquote (env, sexpr.cdr)}
    end
  end
  return sexpr
end


--- Evaluate a list of sexprs.
-- @tparam table env an environment table
-- @tparam Cons list an argument list
-- @treturn Cons evaluated `list`
function evalargs (env, list)
  if list.kind ~= "cons" then
    return list
  end
  return Cons {evalsexpr (env, list.car), evalargs (env, list.cdr)}
end


--- Evaluate a single sexpr.
-- @tparam table env an environment table
-- @tparam Atom sexpr an S-Expression
-- @return evaluation result
function evalsexpr (env, sexpr)
  if not sexpr.kind then
    error ("invalid s-expr: " .. tostring (sexpr), 0)
  end
  if sexpr.kind == "cons" then
    local car = sexpr.car
    if car.kind == "'" then
      -- A quoted expression is protected from further evaluation.
      return sexpr.cdr

    elseif car.kind == "`" then
      -- Comma expressions inside a back-quoted expression ARE evaluated.
      return evalquote (env, sexpr.cdr)

    else
      -- Otherwise the first symbol of a CONS list..
      local func
      if car.kind == "symbol" then
	-- ...can be a function symbol
	func = (intern_soft (car.name, env) or {}).value
      else
	-- ...or a function valued expression
	func = evalsexpr (env, car)
      end
      if func == nil or func.kind ~= "function" then
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
      return func.func (env, args)
    end

  elseif sexpr.kind == "symbol" then
    local value = (intern_soft (sexpr.name, env) or {}).value
    if value ~= nil then
      return value
    end
    error ("symbol's value as a variable is void: " .. sexpr.name, 0)
  end

  return sexpr
end


--- Evaluate a string of lisp.
-- @tparam table env an environment table
-- @string s a string of lisp code
-- @return evaluation result of last S-Expression in `s`
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


--- Evaluate a file of lisp.
-- @tparam table env an environment table
-- @string filename name of a file
-- @return evaluation result of last S-Expression in `filename`
local function evalfile (env, filename)
  local s, errmsg = io.slurp (filename)

  if s then
    s, errmsg = evalstring (env, s)
  end

  return s, errmsg
end



--[[ ================= ]]--
--[[ Public Interface. ]]--
--[[ ================= ]]--

-- Return a table of the public interface to this file.

--- @export
return {
  -- Lisp Atoms:
  Cons     = Cons,
  Function = Function,
  Nil      = Nil,
  Number   = Number,
  String   = String,
  Symbol   = Symbol,
  T        = T,

  -- Obarray:
  intern      = intern,
  intern_soft = intern_soft,
  mapatoms    = mapatoms,
  obarray     = obarray,  --FIXME

  -- Utilities:
  append   = append,

  -- Parser:
  parse = parse,

  -- Environments:
  env_apply = env_apply,
  env_bind  = env_bind,
  env_push  = env_push,

  -- Evaluator:
  evalfile   = evalfile,
  evalsexpr  = evalsexpr,
  evalstring = evalstring,
}
