-- This software is licensed under the M.I.T. license.
-- The license text is found in "license.txt"
--

Env    = require "Environment"
Parser = require "Parser"
Lisp   = require "LispInterpreter"

-- The top read-eval loop...
local function readEval ()
  local env = Lisp.primitive

  -- Run the prelude
  Lisp.runFile (env, "Prelude.lsp")

  local line
  repeat
    io.write("> ")
    line = io.read()
    if line and line ~= ":q" then
      local ok, value = pcall (Lisp.evalExpr, env, line)
      if ok then
        if value then
          print (Sexpr.prettyPrint (value))
        end
      else
        print ("#error: " .. value)
      end
    end
  until not line or line == ":q"
end

readEval()
