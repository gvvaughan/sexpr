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

local Nil, T, Cons, Function, Number, String =
      lisp.Nil, lisp.T, lisp.Cons, lisp.Function, lisp.Number, lisp.String
local append, intern, intern_soft =
      lisp.append, lisp.intern, lisp.intern_soft

--- Define a new primitive function.
-- @string name symbol name
-- @string[opt] special "lazy" for special forms that evaluate their
--   own parameters, or "macro" for forms that output an sexpr for
--   further evaluation
-- @string[opt] doc documentation string
-- @func func handler function
-- @treturn Symbol interned function symbol
local function Primitive (name, special, doc, func)
  if func == nil then func, doc, special = doc, special, nil end
  if func == nil then func, special = special, doc end

  local symbol = intern (name)
  symbol.value = Function {name, func, special}
  if doc then
    symbol:put ("function-documentation", String {doc})
  end
end



--[[ ---------------------- ]]--
--[[ Primitive Definitions. ]]--
--[[ ---------------------- ]]--


Primitive ("*",
[[
(* &rest NUMBERS)
Return product of any number of arguments, which are numbers.
]],
  function (args)
    local product = 1
    while args and args.car do
      product = product * tonumber (args.car.value)
      args    = args.cdr
    end
    return Number {product}
  end
)


Primitive ("+",
[[
(+ &rest NUMBERS)
Return sum of any number of arguments, which are numbers.
]],
  function (args)
    local sum = 0
    while args and args.car do
      sum   = sum + tonumber (args.car.value)
      args  = args.cdr
    end
    return Number {sum}
  end
)


Primitive ("-",
[[
(- &rest NUMBERS)
Negate number or subtract numbers and return the result.
With one argument, negates it.  With more than one argument,
subtracts all but the first from the first.
]],
  function (args)
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


Primitive ("<",
[[
(< NUM1 NUM2)
Return t if first argument is less than second argument. Both must be
numbers.
]],
  function (args)
    return tonumber (args.car) < tonumber (args.cdr.car) and T or Nil
  end
)


Primitive ("append",
[[
(append &rest LISTS)
Concatenate all the arguments and make the result a list.
Return a list whose elements are the elements of all the arguments.
The last argument is not copied, just used as the tail of the new list.
]],
  function (args)
    if args == Nil then
      return Nil
    elseif args.cdr == Nil then
      return args.car
    else
      return append (args.car, args.cdr)
    end
  end
)


Primitive ("car",
[[
(car LIST)
Return the car of LIST.  If LIST is nil, return nil.
]],
  function (args) return args.car.car end
)


Primitive ("cdr",
[[
(cdr LISP)
Return the cdr of LIST,  If LIST is nil, return nil.
]],
  function (args) return args.car.cdr end
)


Primitive ("cons",
[[
(cons CAR CDR)
Create a new cons, give it CAR and CDR as components, and return it.
]],
  function (args) return Cons {args.car, args.cdr.car} end
)


Primitive ("consp",
[[
(consp OBJECT)
Return t if OBJECT is a cons cell.
]],
  function (args)
    return args.car.kind == "cons" and T or Nil
  end
)


Primitive ("defmacro",
  "lazy",
[[
(defmacro NAME (PARAMS) BODY)
Define NAME as a macro.
]],
  function (sexpr, env)
    local name      = sexpr.car.name
    local paramlist = sexpr.cdr.car
    local body      = sexpr.cdr.cdr.car

    local symbol    = intern (name, env)
    symbol.value    = Function {
      string.format ("(defmacro %s %s %s)",
                     name, tostring (paramlist), tostring (body)),
      function (args, env2)
        local scope   = lisp.bind (paramlist, args, {})
        local applied = lisp.apply (body, scope)
        return lisp.evalsexpr (applied, env2)
      end,
      "macro"
    }

    return symbol
  end
)


Primitive ("eq",
[[
(eq OBJ1 OBJ2)
Return t if the OBJ1 and OBJ2 are the same Lisp object.
]],
  function (args)
    local arg1 = args.car
    local arg2 = args.cdr.car
    if arg1.kind == "cons" or arg1.kind ~= arg2.kind then return Nil end
    if arg1.value ~= nil and arg1.value == arg2.value then return T end
    assert (arg1.kind == arg2.kind and arg1.name ~= nil)
    return arg1.name == arg2.name and T or Nil
  end
)


Primitive ("eval",
[[
(eval FORM)
Evaluate FORM and return its value.
]],
  function (sexpr, env)
    local car = sexpr.car
    if car.kind == "string" then
      return lisp.evalstring (sexpr.car.value, env)
    end
    return lisp.evalsexpr (car, env)
  end
)


Primitive ("get",
[[
(get SYMBOL PROPNAME)
Return the value of SYMBOL's PROPNAME property.
]],
  function (args, env)
    local name     = args.car.name
    local propname = args.cdr.car.name
    local symbol   = intern_soft (name, env)
    return symbol and symbol:get (propname) or Nil
  end
)


Primitive ("if",
  "lazy",
[[
(if COND THEN &rest ELSE)
If COND yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE's.
THEN must be one expression, but ELSE... can be zero or more expressions.
If COND yields nil, and there are no ELSE's, the value is nil.
]],
  function (forms, env)
    local cond = lisp.evalsexpr (forms.car, env)
    if cond.kind ~= "nil" then
      return lisp.evalsexpr (forms.cdr.car, env)
    end

    local result = Nil
    forms = forms.cdr.cdr
    while forms and forms.car do
      result = lisp.evalsexpr (forms.car, env)
      forms = forms.cdr
    end
    return result
  end
)


Primitive ("lambda",
  "lazy",
[[
(lambda ARGS BODY)
Return a lambda expression.
A call of the form (lambda ARGS BODY) is self-quoting; the result of
evaluating the lambda expression is the expression itself. The lambda
expression may then be treated a a function, i.e, stored as the function
value of a symbol, passed to `funcall' or `mapcar', etc.
]],
  function (args, env)
    local paramlist = args.car
    local body      = args.cdr.car
    return Function {
      string.format ("(lambda %s %s)",
                     tostring (paramlist), tostring (body)),
      function (arglist)
        local scope = lisp.pushenv (env)
        lisp.bind (paramlist, arglist, scope)
        return lisp.evalsexpr (body, scope)
      end
    }
  end
)


Primitive ("load",
[[
(load FILE)
Execute a file of Lisp code named FILE.
]],
  function (sexpr, env)
    lisp.evalfile (sexpr.car.value, env)
    return T
  end
)


Primitive ("prin1",
[[
(prin1 OBJECT)
Output the printed repreresentation of OBJECT, any Lisp object.
]],
  function (_, sexpr)
    print (tostring (sexpr.car))
    return T
  end
)


Primitive ("progn",
[[
(progn BODY...)
Evaluate BODY forms sequentially and return value of last one.
]],
  function (forms, env)
    local result = Nil
    while forms and forms.car do
      result = lisp.evalsexpr (forms.car, env)
      forms = forms.cdr
    end
    return result
  end
)


Primitive ("put",
[[
(put SYMBOL PROPNAME VALUE)
Store VALUE in SYMBOL's PROPNAME property.
]],
  function (args, env)
    local name     = args.car.name
    local propname = args.cdr.car.name
    local value    = args.cdr.cdr.car
    local symbol   = intern_soft (name, env)
    return symbol and symbol:put (propname, value) or Nil
  end
)


Primitive ("setq",
  "lazy",
[[
(setq [SYMBOL VALUE]...)
Set each SYMBOL to the following VALUE.
Each SYMBOL is a variable; they are literal (not evaluated).
Each VALUE is an expression; they are evaluated.
Thus, (setq x (1+ y)) sets `x' to the value of `(1+ y)'.
The second VALUE is not computed until after the first SYMBOL is set, and
so on; each VALUE can use the new value of variables set earlier in the
`setq'.
The return value of the `setq' form is the value of the last VALUE.
]],
  function (args, env)
    local symbol
    repeat
      symbol = intern (args.car.name)
      symbol.value = lisp.evalsexpr(args.cdr.car, env)
      args = args.cdr.cdr
    until args == nil or args.car == nil
    return symbol.value
  end
)


return {
  Primitive = Primitive,
}
