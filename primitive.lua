-- This software is licensed under the M.I.T. license.
-- The license text is found in "license.txt"
--
-- primitive.lua
-- Author: David Bergman
--
-- This is a Scheme/Lisp interpreter, written in Lua
--

lisp = require "lisp"

local lisp_bool, lisp_cons, lisp_func, lisp_number, lisp_tostring =
      lisp.bool, lisp.cons, lisp.func, lisp.number, lisp.tostring

local M = {}

-- Some primitives

M.symbols = {}

-- Primitive (NAME, [FLAGS], FUNC)
local function Primitive (name, flags, func)
  if func == nil then
    flags, func = func, flags
  end
  M.symbols[name] = lisp_func (name, func, flags)
end

-- (* NUMBER-1 NUMBER-2)
Primitive ("*",
  function (env, args)
    local num1 = tonumber (args.car.lexeme)
    local num2 = tonumber (args.cdr.car.lexeme)
    return lisp_number (num1 * num2)
  end
)

-- (+ NUMBER-1 NUMBER-2)
Primitive ("+",
  function (env, args)
    local num1 = tonumber (args.car.lexeme)
    local num2 = tonumber (args.cdr.car.lexeme)
    return lisp_number (num1 + num2)
  end
)

-- (< NUMBER-1 NUMBER-2)
Primitive ("<",
  function (env, args)
    local num1 = tonumber (args.car.lexeme)
    local num2 = tonumber (args.cdr.car.lexeme)
    return lisp_bool (num1 < num2)
  end
)

-- (car CONS)
Primitive ("car",
  function (env, args)
    return args.car.car
  end
)

-- (cdr CONS)
Primitive ("cdr",
  function (env, args)
    return args.car.cdr
  end
)

-- (cons ATOM LIST)
Primitive ("cons",
  function (env, args)
    return lisp_cons (args.car, args.cdr.car)
  end
)

-- (consp ARG)
Primitive ("consp",
  function (env, args)
    return lisp_bool (args.car.type == "cons")
  end
)

-- (defmacro NAME (PARAMS) BODY)
Primitive ("defmacro",
  "lazy",
  function (env, sexpr)
    local name   = sexpr.car
    local params = sexpr.cdr.car
    local body   = sexpr.cdr.cdr.car
    local macro  = function (env2, args)
                     local scope = {}
                     lisp.bind (scope, params, args)
                     local applied = lisp.applyEnv (scope, body)
                     return lisp.evalSexpr (env2, applied)
                   end
    local func = lisp_func (
      string.format ("(defmacro %s %s %s)", name.lexeme,
                     lisp_tostring (params), lisp_tostring (body)),
      macro, "macro")
    env[name.lexeme] = func
    return func
  end
)

-- (eq ARG-1 ARG-2)
Primitive ("eq",
  function (env, args)
    local arg1 = args.car
    local arg2 = args.cdr.car
    return lisp_bool (arg1.type == arg2.type
                      and arg1.type ~= "cons"
     		      and arg1.lexeme == arg2.lexeme)
  end
)

-- (eval S-EXPR)
Primitive ("eval",
  function (env, sexpr)
    local value
    local car = sexpr.car
    if car.type == "string" then
      -- Our eval actually handles both strings and S-exprs
      return lisp.evalExpr (env, sexpr.car.lexeme)
    end
    return lisp.evalSexpr (env, car)
  end
)

-- (if COND TRUE-CLAUSE FALSE-CLAUSE)
Primitive ("if",
  "lazy",
  function (env, args)
    local cond = lisp.evalSexpr (env, args.car)
    local expr
    if cond.type == "constant" and cond.lexeme == "nil" then
      expr = args.cdr.cdr.car
    else
      expr = args.cdr.car
    end
    return lisp.evalSexpr (env, expr)
  end
)

-- (lambda (PARAMS) BODY)
Primitive ("lambda",
  "lazy",
  function (env, args)
    local formalParams = args.car
    local body = args.cdr.car
    return lisp_func (
      string.format ("(lambda %s %s)", lisp_tostring (formalParams),
                     lisp_tostring (body)),
      function (env2, actualParams)
        local localEnv = lisp.addBindings (env, formalParams, actualParams)
        return lisp.evalSexpr (localEnv, body)
      end
    )
  end
)

-- (load FILENAME)
-- Evaluate a whole lisp file, and return 't'
Primitive ("load",
  function (env, sexpr)
    lisp.runFile (env, sexpr.car.lexeme)
    return lisp_bool (true)
  end
)

-- (neg NUMBER)
Primitive ("neg",
  function (env, args)
    return lisp_number (0 - tonumber (args.car.lexeme))
  end
)

-- (prin1 OBJECT)
-- Print OBJECT to standard output
Primitive ("prin1",
  function (env, sexpr)
    print (lisp_tostring (sexpr.car))
    return lisp_bool (true)
  end
)

-- (progn SEXPR...)
Primitive ("progn",
  function (env, args)
    local result = lisp_bool (nil)
    while args and args.car do
      result = lisp.evalSexpr (env, args.car)
      args = args.cdr
    end
    return result
  end
)

-- (setq NAME VALUE)
Primitive ("setq",
  "lazy",
  function (env, args)
    local value = lisp.evalSexpr(env, args.cdr.car)
    env[args.car.lexeme] = value
    return value
  end
)


return M
