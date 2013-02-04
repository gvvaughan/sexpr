-- This software is licensed under the M.I.T. license.
-- The license text is found in "license.txt"
--
-- LispInterpreter.lua
-- Author: David Bergman
--
-- This is a Scheme/Lisp interpreter, written in Lua
--

Env    = require "Environment"
Parser = require "Parser"
Sexpr  = require "Sexpr"

local M = {}

function M.evalExpr (env, expr)
  return M.evalSexprList (env, Parser.parseSexpr (expr))
end

function M.evalQuote (env, sexpr)
  local value
  if not sexpr.type then
    error ("Invalid S-expr: ", 2)
  end
  if sexpr.type == "cons" then
    local car = sexpr.car
    if car.type == "operator" and car.lexeme == "," then
      value = M.evalSexpr (env, sexpr.cdr)
    else	
      value = Sexpr.cons (M.evalQuote (env, car), M.evalQuote (env, sexpr.cdr))
    end
  else
    value = sexpr
  end
  return value
end

function M.evalSexprList (env, sexprList, i)
  i = i or 1
  local count = #sexprList
  if i > count then return nil end

  local firstValue = M.evalSexpr (env, sexprList[i])
  if i == count then
    return firstValue
  else
    return M.evalSexprList (env, sexprList, 1+ i)
  end
end

function M.evalSexpr (env, sexpr)
  local value
  if not sexpr.type then
    error ("Invalid S-expr: " .. sexpr, 2)
  end
  if sexpr.type == "cons" then
    -- 1. Cons cell
    local car = sexpr.car
    if car.type == "operator" and car.lexeme == "'" then
      value = sexpr.cdr
    elseif car.type=="operator" and car.lexeme == "`" then
      local cdr = M.evalQuote (env, sexpr.cdr)
      value = cdr
    else
      local fun = M.evalSexpr (env, car)
      if not fun or fun.type ~= "function" then
        error ("The S-expr did not evaluate to a function: " .. tostring (car))
      end

      -- The function can be either "lazy", in that it deals with eval-
      -- uation of its arguments itself, a "macro", which requires a second
      -- evaluation after the macro expansion, or a regular eager function.
      local args
      if fun.special == "lazy" or fun.special == "macro"  then
        args = sexpr.cdr
      else
        args = M.evalList (env, sexpr.cdr)
      end
      value = fun.fun (env, args)
    end
  elseif sexpr.type == "symbol" then
    -- a. symbol
    value = env[sexpr.lexeme]
    if not value then
      error ("The symbol '" .. sexpr.lexeme .. "' is not defined")
    end
  else
    -- b. constant
    value = sexpr
  end
  return value
end

-- Evaluate each item in a list
function M.evalList (env, list)
  local value
  if list.type == "cons" then
    value = Sexpr.cons (M.evalSexpr (env, list.car), M.evalList (env, list.cdr))
  else
    value = list
  end
  return value
end

-- Apply an environment and get the substituted S-exp
function M.applyEnv (env, expr)
  if expr.type == "cons" then
    return Sexpr.cons (M.applyEnv (env, expr.car), M.applyEnv(env, expr.cdr))
  elseif expr.type == "symbol" then
    return env[expr.lexeme] or expr
  end
  return expr
end

-- Some primitives

local primitive = {}

-- Primitive (NAME, [FLAGS], FUNC)
local function Primitive (name, flags, func)
  if func == nil then
    flags, func = func, flags
  end
  primitive[name] = Sexpr.newFun (name, func, flags)
end

-- (* NUMBER-1 NUMBER-2)
Primitive ("*",
  function (env, args)
    local num1 = tonumber (args.car.lexeme)
    local num2 = tonumber (args.cdr.car.lexeme)
    return Sexpr.newAtom ("number", num1 * num2)
  end
)

-- (+ NUMBER-1 NUMBER-2)
Primitive ("+",
  function (env, args)
    local num1 = tonumber (args.car.lexeme)
    local num2 = tonumber (args.cdr.car.lexeme)
    return Sexpr.newAtom ("number", num1 + num2)
  end
)

-- (< NUMBER-1 NUMBER-2)
Primitive ("<",
  function (env, args)
    local num1 = tonumber (args.car.lexeme)
    local num2 = tonumber (args.cdr.car.lexeme)
    return Sexpr.newBool (num1 < num2)
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
    return Sexpr.cons (args.car, args.cdr.car)
  end
)

-- (consp ARG)
Primitive ("consp",
  function (env, args)
    return Sexpr.newBool (args.car.type == "cons")
  end
)

-- (defmacro NAME (PARAMS) BODY)
Primitive ("defmacro",
  "lazy",
  function (env, sexpr)
    local name   = sexpr.car
    local params = sexpr.cdr.car
    local body   = sexpr.cdr.cdr.car
    local macro  = function (env2, e)
                     local paramScope = {}
                     Env.bind (paramScope, params, e)
                     local subsEnv  = Env.new (paramScope)
                     local expanded = M.applyEnv (subsEnv, body)
                     return M.evalSexpr (env2, expanded)
                   end
    local fun = Sexpr.newFun (
      string.format ("(defmacro %s %s %s)", name.lexeme,
                     Sexpr.prettyPrint (params), Sexpr.prettyPrint (body)),
      macro, "macro")
    env[name.lexeme] = fun
    return fun
  end
)

-- (eq ARG-1 ARG-2)
Primitive ("eq",
  function (env, args)
    local arg1 = args.car
    local arg2 = args.cdr.car
    return Sexpr.newBool (arg1.type == arg2.type
                          and arg1.type ~= "cons"
     		          and arg1.lexeme == arg2.lexeme)
  end
)

-- (eval S-EXPR)
-- Our eval actually handles both strings and S-exprs
Primitive ("eval",
  function (env, sexpr)
    local value
    local car = sexpr.car
    if car.type == "string" then
      return M.evalExpr (env, sexpr.car.lexeme)
    end
    return M.evalSexpr (env, car)
  end
)

-- (if COND TRUE-CLAUSE FALSE-CLAUSE)
Primitive ("if",
  "lazy",
  function (env, args)
    local cond = M.evalSexpr (env, args.car)
    local expr
    if cond.type == "constant" and cond.lexeme == "nil" then
      expr = args.cdr.cdr.car
    else
      expr = args.cdr.car
    end
    return M.evalSexpr (env, expr)
  end
)

-- (lambda (PARAMS) BODY)
Primitive ("lambda",
  "lazy",
  function (env, args)
    local formalParams = args.car
    local body = args.cdr.car
    return Sexpr.newFun (
      string.format ("(lambda %s %s)", Sexpr.prettyPrint(formalParams),
                     Sexpr.prettyPrint(body)),
      function (env2, actualParams)
        local localEnv = env:addBindings (formalParams, actualParams)
        return M.evalSexpr (localEnv, body)
      end
    )
  end
)

-- (load FILENAME)
-- Evaluate a whole lisp file, and return 't'
Primitive ("load",
  function (env, sexpr)
    M.runFile (env, sexpr.car.lexeme)
    return Sexpr.newBool (true)
  end
)

-- (neg NUMBER)
Primitive ("neg",
  function (env, args)
    return Sexpr.newAtom ("number", - tonumber (args.car.lexeme))
  end
)

-- (prin1 OBJECT)
-- Print OBJECT to standard output
Primitive ("prin1",
  function (env, sexpr)
    print (Sexpr.prettyPrint (sexpr.car))
    return Sexpr.newBool (true)
  end
)

-- (setq NAME VALUE)
Primitive ("setq",
  "lazy",
  function (env, args)
    local value = M.evalSexpr(env, args.cdr.car)
    env[args.car.lexeme] = value
    return value
  end
)


function M.runFile (env, filename)
  local s, errmsg = io.slurp (filename)

  if s then
    s, errmsg = M.evalExpr (env, s)
  end

  return s, errmsg
end


-- The top read-eval loop...
function M.readEval ()
  local env = Env.new (primitive)

  -- Run the prelude
  M.runFile (env, "Prelude.lsp")

  local line
  repeat
    io.write("> ")
    line = io.read()
    if line and line ~= ":q" then
      local ok, value = pcall (M.evalExpr, env, line)
      if ok then
        if value then
          print (Sexpr.prettyPrint (value))
        end
      else
        print ("#error: ", value)
      end
    end
  until not line or line == ":q"
end

return M
