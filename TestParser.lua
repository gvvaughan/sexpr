Parser = require "Parser"
Sexpr = require "Sexpr"

sexpr = Parser.parseSexpr(arg[1])[1]
print (Sexpr.prettyPrint (sexpr))
