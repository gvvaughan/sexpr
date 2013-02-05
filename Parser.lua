-- This software is licensed under the M.I.T. license.
-- The license text is found in "license.txt"
--
-- Parser.lua
-- Author: David Bergman
--
-- A Scheme parser
--

require "std"

Sexpr = require "Sexpr"

local M = {}

local isconstant = set.new { "nil", "t" }
local isoperator = set.new { "(", ")", ",", "'", "`", "." }
local isdelimiter = set.new { ";", " ", "\t", "\n", "\r", '"' } + isoperator


-- Increment index into s and return that character.
local function nextch (s, i)
  return i < #s and s[i + 1] or nil, i + 1
end


-- Lexical scanner: Return three values: `token', `kind', `i', where
-- `token' is the content of the just scanned token, `kind' is the
-- type of token returned, and `i' is the index of the next unscanned
-- character in `s'.
local function lex (s, i)
  local c
  repeat
    c, i = nextch (s, i)

    -- Comments start with `;'.
    if c == ';' then
      repeat
        c, i = nextch (s, i)
      until c == '\n' or c == '\r' or c == nil
    end

    -- Continue skipping additional lines of comments and whitespace.
  until c ~= ' ' and c ~= '\t' and c ~= '\n' and c ~= '\r'

  -- Return end-of-file immediately.
  if c == nil then return nil, "eof", i end

  -- Return delimiter tokens.
  if isoperator[c] then
    return c, "operator", i
  end

  -- Strings start and end with `"'.
  -- Note we read another character immediately to skip the opening
  -- quote, and don't append the closing quote to the returned token.
  local token = ''
  if c == '"' then
    repeat
      c, i = nextch (s, i)
      if c == nil then
        return token, "incomplete string", i - 1
      elseif c == '\\' then
        c, i = nextch (s, i)
	-- `\' can be used to escape `"', `\n' and `\' in strings
	if c ~= '"' and c ~= '\n' and c ~= '\\' then
	  token = token .. '\\'
	end
	if c ~= '\n' then
	  token = token .. c
	end
      elseif c ~= '"' then
	token = token .. c
      end
    until c == '"'

    return token, "string", i
  end

  -- Anything else is a `word' - up to the next whitespace or delimiter.
  repeat
    token = token .. c
    c, i = nextch (s, i)
    if isdelimiter[c] or c == nil then
      local kind = "symbol"
      if isconstant[token] then
        kind = "constant"
      elseif token:match ("^%d+$") then
        token = tonumber (token)
	kind = "number"
      end
      return token, kind, i - 1
    end
  until false
end


local createSexpr, createCons

-- If the first token is a '.', we just return the second token, as is,
-- while skipping a subsequent ')', else if it is a ')' we return NIL,
-- else we get the first Sexpr and CONS it with the rest.
function createCons (s, start)
  local token, kind, n = lex (s, start)
  if kind == "eof" then
    error ("Token index " .. start ..
           " is out of range when creating CONS S-Expr", 2)
  end

  if kind == "operator" then
    if token == "." then
      local cdr, i = createSexpr (s, n)
      -- We skip the last ')'
      token, kind, n = lex (s, i)
      if kind == "eof" or token ~= ")" then
        error("The CDR part ending with " .. tostring (cdr) ..
              " was not followed by a ')'")
      end
      return cdr, n
    elseif token == ")" then
      return Sexpr.newAtom ("constant", "nil"), n
    end
  end

  local car, i = createSexpr (s, start)
  local cdr, rest = createCons (s, i)
  return Sexpr.cons (car, cdr), rest
end

function createSexpr (s, i)
  local token, kind, cdr
  token, kind, i = lex (s, i)
  if kind == "eof" then
    return nil, i
  end

  -- If the first token is a '(', we should expect a "list"
  if kind == "operator" then
    if token == "(" then
      return createCons (s, i)
    end

    cdr, i = createSexpr (s, i)
    return Sexpr.cons (Sexpr.newAtom (kind, token), cdr), i
  end

  return Sexpr.newAtom (kind, token), i
end

-- Parse the code snippet, yielding a list of (unevaluated) S-expr
function M.parseSexpr (s)
  local i = 0

  local sexpr
  local sexprList = {}
  repeat
    sexpr, i = createSexpr (s, i)
    if sexpr then
      table.insert (sexprList, sexpr)
    end
  until not sexpr
  return sexprList
end


return M
