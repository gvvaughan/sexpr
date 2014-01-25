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
 Primitive lisp commands.
 @module sexpr.primitive
]]

local lisp = require "sexpr.lisp"

local Nil, T, Cons, Function, Number, append =
      lisp.Nil, lisp.T, lisp.Cons, lisp.Function, lisp.Number, lisp.append


-- Helper functions

local symbols = {}


--- Define a new primitive function.
-- @string name symbol name
-- @string[opt=nil] special "lazy" for special forms that evaluate their
--   own parameters, or "macro" for forms that output an sexpr for
--   further evaluation
-- @func func handler function
-- @treturn Symbol interned function symbol
local function Primitive (name, special, func)
  if func == nil then
    special, func = func, special
  end
  symbols[name] = Function {name, func, special}
end



--[[ ---------------------- ]]--
--[[ Primitive Definitions. ]]--
--[[ ---------------------- ]]--


-- (* &rest NUMBERS)
-- Return product of any number of arguments, which are numbers.
Primitive ("*",
  function (env, args)
    local product = 1
    while args and args.car do
      product = product * tonumber (args.car.value)
      args    = args.cdr
    end
    return Number {product}
  end
)


-- (+ &rest NUMBERS)
-- Return sum of any number of arguments, which are numbers.
Primitive ("+",
  function (env, args)
    local sum = 0
    while args and args.car do
      sum   = sum + tonumber (args.car.value)
      args  = args.cdr
    end
    return Number {sum}
  end
)


-- (- &rest NUMBERS)
-- Negate number or subtract numbers and return the result.
-- With one argument, negates it.  With more than one argument,
-- subtracts all but the first from the first.
Primitive ("-",
  function (env, args)
    if args.car == nil then
      return Number {0}
    elseif args.cdr.car == nil then
      return Number {0 - tonumber (args.car.value)}
    else
      local difference = tonumber (args.car.value)
      repeat
        args = args.cdr
        difference = difference - tonumber (args.car.value)
      until args.cdr == nil or args.cdr.car == nil
      return Number {difference}
    end
  end
)


-- (< NUM1 NUM2)
-- Return t if first argument is less than second argument. Both must be
-- numbers.
Primitive ("<",
  function (env, args)
    return tonumber (args.car) < tonumber (args.cdr.car) and T or Nil
  end
)


-- (append &rest LISTS)
-- Concatenate all the arguments and make the result a list.
-- Return a list whose elements are the elements of all the arguments.
-- The last argument is not copied, just used as the tail of the new list.
Primitive ("append",
  function (env, args)
    if args == Nil then
      return Nil
    elseif args.cdr == Nil then
      return args.car
    else
      return append (args.car, args.cdr)
    end
  end
)


-- (car LIST)
-- Return the car of LIST.  If LIST is nil, return nil.
Primitive ("car",
  function (env, args) return args.car.car end
)


-- (cdr LISP)
-- Return the cdr of LIST,  If LIST is nil, return nil.
Primitive ("cdr",
  function (env, args) return args.car.cdr end
)


-- (cons CAR CDR)
-- Create a new cons, give it CAR and CDR as components, and return it.
Primitive ("cons",
  function (env, args) return Cons {args.car, args.cdr.car} end
)


-- (consp OBJECT)
-- Return t if OBJECT is a cons cell.
Primitive ("consp",
  function (env, args)
    return args.car.kind == "cons" and T or Nil
  end
)


-- (defmacro NAME (PARAMS) BODY)
-- Define NAME as a macro.
Primitive ("defmacro",
  "lazy",
  function (env, sexpr)
    local name   = sexpr.car.name
    local params = sexpr.cdr.car
    local body   = sexpr.cdr.cdr.car
    local value  = string.format ("(defmacro %s %s %s)", name, params, body)
    local macro  = function (env2, args)
                     local scope   = lisp.env_bind ({}, params, args)
                     local applied = lisp.env_apply (scope, body)
                     return lisp.evalsexpr (env2, applied)
                   end
    local func   = Function { value, macro, "macro" }
    env[name]    = func
    return func
  end
)


-- (eq OBJ1 OBJ2)
-- Return t if the OBJ1 and OBJ2 are the same Lisp object.
Primitive ("eq",
  function (env, args)
    local arg1 = args.car
    local arg2 = args.cdr.car
    if arg1.kind == "cons" or arg1.kind ~= arg2.kind then return Nil end
    if arg1.value ~= nil and arg1.value == arg2.value then return T end
    assert (arg1.kind == arg2.kind and arg1.name ~= nil)
    return arg1.name == arg2.name and T or Nil
  end
)


-- (eval FORM)
-- Evaluate FORM and return its value.
Primitive ("eval",
  function (env, sexpr)
    local car = sexpr.car
    if car.kind == "string" then
      return lisp.evalstring (env, sexpr.car.value)
    end
    return lisp.evalsexpr (env, car)
  end
)


-- (if COND THEN &rest ELSE)
-- If COND yields non-nil, do THEN, else do ELSE...
-- Returns the value of THEN or the value of the last of the ELSE's.
-- THEN must be one expression, but ELSE... can be zero or more expressions.
-- If COND yields nil, and there are no ELSE's, the value is nil.
Primitive ("if",
  "lazy",
  function (env, forms)
    local cond = lisp.evalsexpr (env, forms.car)
    if cond.kind ~= "nil" then
      return lisp.evalsexpr (env, forms.cdr.car)
    end

    local result = Nil
    forms = forms.cdr.cdr
    while forms and forms.car do
      result = lisp.evalsexpr (env, forms.car)
      forms = forms.cdr
    end
    return result
  end
)


-- (lambda ARGS BODY)
-- Return a lambda expression.
-- A call of the form (lambda ARGS BODY) is self-quoting; the result of
-- evaluating the lambda expression is the expression itself. The lambda
-- expression may then be treated a a function, i.e, stored as the function
-- value of a symbol, passed to `funcall' or `mapcar', etc.
Primitive ("lambda",
  "lazy",
  function (env, args)
    local paramlist = args.car
    local body      = args.cdr.car
    return Function {
      string.format ("(lambda %s %s)", paramlist, body),
      function (_, arglist)
        local scope = lisp.env_push (env)
        lisp.env_bind (scope, paramlist, arglist)
        return lisp.evalsexpr (scope, body)
      end
    }
  end
)

-- (load FILE)
-- Execute a file of Lisp code named FILE.
Primitive ("load",
  function (env, sexpr)
    lisp.evalfile (env, sexpr.car.value)
    return T
  end
)


-- (prin1 OBJECT)
-- Output the printed repreresentation of OBJECT, any Lisp object.
Primitive ("prin1",
  function (env, sexpr)
    print (tostring (sexpr.car))
    return T
  end
)


-- (progn BODY...)
-- Evaluate BODY forms sequentially and return value the last one.
Primitive ("progn",
  function (env, forms)
    local result = Nil
    while forms and forms.car do
      result = lisp.evalsexpr (env, forms.car)
      forms = list.cdr
    end
    return result
  end
)


-- (setq [SYMBOL VALUE]...)
-- Set each SYMBOL to the following nVALUE.
-- Each SYMBOL is a variable; they are literal (not evaluated).
-- Each VALUE is an expression; they are evaluated.
-- Thus, (setq x (1+ y)) sets `x' to the value of `(1+ y)'.
-- The second VALUE is not computed until after the first SYMBOL is set, and
-- so on; each VALUE can use the new value of variables set earlier in the
-- `setq'.
-- The return value of the `setq' form is the value of the last VALUE.
Primitive ("setq",
  "lazy",
  function (env, args)
    local value
    repeat
      value = lisp.evalsexpr(env, args.cdr.car)
      env[args.car.name] = value
      args = args.cdr.cdr
    until args == nil or args.car == nil
    return value
  end
)


local M = {
  symbols = symbols,
}

return M
