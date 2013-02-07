-- This software is licensed under the M.I.T. license.
-- The license text is found in "license.txt"
--
-- primitive.lua
-- Author: David Bergman
--
-- This is a Scheme/Lisp interpreter, written in Lua
--

lisp = require "lisp"

local lisp_nil, lisp_t, lisp_cons, lisp_function, lisp_number =
      lisp.Nil, lisp.T, lisp.Cons, lisp.Function, lisp.Number

local M = {}

-- Some primitives

M.symbols = {}

-- Primitive (NAME, [FLAGS], FUNC)
local function Primitive (name, flags, func)
  if func == nil then
    flags, func = func, flags
  end
  M.symbols[name] = lisp_function {name, func, flags}
end

-- (* NUMBER-1 NUMBER-2)
Primitive ("*",
  function (env, args)
    local num1 = tonumber (args.car.value)
    local num2 = tonumber (args.cdr.car.value)
    return lisp_number {num1 * num2}
  end
)

-- (+ NUMBER-1 NUMBER-2)
Primitive ("+",
  function (env, args)
    local num1 = tonumber (args.car.value)
    local num2 = tonumber (args.cdr.car.value)
    return lisp_number {num1 + num2}
  end
)

-- (< NUMBER-1 NUMBER-2)
Primitive ("<",
  function (env, args)
    local num1 = tonumber (args.car.value)
    local num2 = tonumber (args.cdr.car.value)
    return num1 < num2 and lisp_t or lisp_nil
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
    return lisp_cons {args.car, args.cdr.car}
  end
)

-- (consp ARG)
Primitive ("consp",
  function (env, args)
    return args.car.kind == "cons" and lisp_t or lisp_nil
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
                     return lisp.evalsexpr (env2, applied)
                   end
    local func = lisp_function {
      string.format ("(defmacro %s %s %s)", name.value,
                     tostring (params), tostring (body)),
      macro, "macro"}
    env[name.value] = func
    return func
  end
)

-- (eq ARG-1 ARG-2)
Primitive ("eq",
  function (env, args)
    local arg1 = args.car
    local arg2 = args.cdr.car
    return (arg1.kind == arg2.kind
            and arg1.kind ~= "cons"
            and arg1.value == arg2.value) and lisp_t or lisp_nil
  end
)

-- (eval S-EXPR)
Primitive ("eval",
  function (env, sexpr)
    local value
    local car = sexpr.car
    if car.kind == "string" then
      -- Our eval actually handles both strings and S-exprs
      return lisp.evalExpr (env, sexpr.car.value)
    end
    return lisp.evalsexpr (env, car)
  end
)

-- (if COND TRUE-CLAUSE FALSE-CLAUSE)
Primitive ("if",
  "lazy",
  function (env, args)
    local cond = lisp.evalsexpr (env, args.car)
    local expr
    if cond.kind == "constant" and cond.value == "nil" then
      expr = args.cdr.cdr.car
    else
      expr = args.cdr.car
    end
    return lisp.evalsexpr (env, expr)
  end
)

-- (lambda (PARAMS) BODY)
Primitive ("lambda",
  "lazy",
  function (env, args)
    local formalParams = args.car
    local body = args.cdr.car
    return lisp_function {
      string.format ("(lambda %s %s)", tostring (formalParams),
                     tostring (body)),
      function (env2, actualParams)
        local localEnv = lisp.addBindings (env, formalParams, actualParams)
        return lisp.evalsexpr (localEnv, body)
      end
    }
  end
)

-- (load FILENAME)
-- Evaluate a whole lisp file, and return 't'
Primitive ("load",
  function (env, sexpr)
    lisp.runFile (env, sexpr.car.value)
    return lisp_t
  end
)

-- (neg NUMBER)
Primitive ("neg",
  function (env, args)
    return lisp_number (0 - tonumber (args.car.value))
  end
)

-- (prin1 OBJECT)
-- Print OBJECT to standard output
Primitive ("prin1",
  function (env, sexpr)
    print (tostring (sexpr.car))
    return lisp_t
  end
)

-- (progn SEXPR...)
Primitive ("progn",
  function (env, args)
    local result = lisp_nil
    while args and args.car do
      result = lisp.evalsexpr (env, args.car)
      args = args.cdr
    end
    return result
  end
)

-- (setq NAME VALUE)
Primitive ("setq",
  "lazy",
  function (env, args)
    local value = lisp.evalsexpr(env, args.cdr.car)
    env[args.car.value] = value
    return value
  end
)


return M
