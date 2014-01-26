lisp = require "sexpr.lisp"
primitive = require "sexpr.primitive"

Cons, Function, Nil, Number, String, Symbol, T =
  lisp.Cons, lisp.Function, lisp.Nil, lisp.Number, lisp.String, lisp.Symbol, lisp.T
append, evalstring, parse = lisp.append, lisp.evalstring, lisp.parse
intern, intern_soft = lisp.intern, lisp.intern_soft
pushenv = lisp.pushenv


global_env = {}

function import (env, name)
  local export = intern_soft (name) -- from obarray
  if export ~= nil then
    local global = intern (name, env or global_env) -- to env
    global.value = export.value
  end
end

function eval (s, env)
  return evalstring (s, env or global_env)
end

function streval (s, env)
  return tostring (eval (s, env))
end

function setq (name, value, env)
  local symbol = intern (name, env)
  symbol.value = value
end

function seteval (var, val, s, env)
  local env = pushenv (env or global_env)
  setq (var, val, env)
  return evalstring (s, env), env
end

handlers = {
  called  = Function {"call-me!", function ()
	      return String {"called!"}
            end},

  cons    = Function {"cons", function (args)
              return Cons {String {"called!"}, args}
            end},

  reverse = Function {"reverse", function (args)
              local reverse = Nil
              while args and args.car do
                reverse, args = Cons {args.car, reverse}, args.cdr
              end
              return reverse
            end},
}
