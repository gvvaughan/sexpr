lisp = require "sexpr.lisp"
primitive = require "sexpr.primitive"

Cons, Function, Nil, Number, String, Symbol, T =
  lisp.Cons, lisp.Function, lisp.Nil, lisp.Number, lisp.String, lisp.Symbol, lisp.T
append, evalstring, parse = lisp.append, lisp.evalstring, lisp.parse
env_push = lisp.env_push

global_env = {}

function eval (s, env)
  return evalstring (env or global_env, s)
end

function streval (s, env)
  return tostring (eval (s, env))
end

function seteval (var, val, s, env)
  local env = env_push (env or global_env)
  env[var] = val
  return evalstring (env, s)
end

handlers  = {
  called  = Function {"call-me!", function (...)
	      return String {"called!"}
            end},

  cons    = Function {"cons", function (env, args)
              return Cons {String {"called!"}, args}
            end},

  reverse = Function {"reverse", function (env, args)
              local reverse = Nil
              while args and args.car do
                reverse, args = Cons {args.car, reverse}, args.cdr
              end
              return reverse
            end},
}
