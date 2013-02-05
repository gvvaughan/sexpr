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

local function newToken (t)
  local s = type (t) == "table" and table.concat (t) or t

  if isconstant[s] then return Sexpr.newAtom ("constant", s) end
  if s:match ("^%d+$") then return Sexpr.newAtom ("number", tonumber (s)) end
  return Sexpr.newAtom ("symbol", s)
end


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
  -- These are returned in the `kind' field so we can immediately tell
  -- the difference between a ')' delimiter and a ")" string token.
  if c == '(' or c == ')' or c == "'" or c == '`' then
    return "", c, i
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
      return token, "word", i - 1
    end
  until false
end


-- Parse a sub-expression, returning a list of parsed tokens.
function M.parseTokens (s)
  local tokens = {}
  local token, kind, i

  i = 0
  repeat
    token, kind, i = lex (s, i)
    if kind == "string" then
      table.insert (tokens, Sexpr.newAtom ("string", token))
    elseif isoperator[kind] then
      table.insert (tokens, Sexpr.newOperator (kind))
    elseif isconstant[token] then
      table.insert (tokens, Sexpr.newAtom ("constant", token))
    elseif token and token:match ("^%d+$") then
      table.insert (tokens, Sexpr.newAtom ("number", tonumber (token)))
    elseif kind ~= "eof" then
      table.insert (tokens, Sexpr.newAtom ("symbol", token))
    end
  until kind == "eof"

  return tokens
end


local createSexpr, createCons

-- If the first token is a '.', we just return the second token, as is,
-- while skipping a subsequent ')', else if it is a ')' we return NIL,
-- else we get the first Sexpr and CONS it with the rest.
function createCons (tokens, start)
  local firstTok = tokens[start]
  if not firstTok then
    error ("Token index " .. start ..
           " is out of range when creating CONS S-Expr", 2)
  end

  if firstTok.type == "operator" and firstTok.lexeme == "." then
    -- We skip the last ')'
    local i, cdr = createSexpr (tokens, start+1)
    if not tokens[i] or tokens[i].type ~= "close paren" then
      error("The CDR part ending with " .. tokens[i - 1].lexeme ..
            " was not followed by a ')'")
    end
    return i + 1, cdr
  elseif firstTok.type == "close paren" then
    return start + 1, newToken ("nil")
  else
    local i, car = createSexpr (tokens, start)
    local rest, cdr = createCons (tokens, i)
    return rest, Sexpr.cons (car, cdr)
  end
end

function createSexpr (tokens, start)
  -- If the first token is a '(', we should expect a "list"
  local firstToken = tokens[start]
  if not firstToken then
    return start, nil
  end
  if firstToken.type == "open paren" then
    return createCons (tokens, start + 1)
  elseif firstToken.type == "operator" then
    local i, cdr = createSexpr (tokens, start + 1)
    return i, Sexpr.cons (firstToken, cdr)
  else
    return start + 1, firstToken
  end
end

-- Parse the code snippet, yielding a list of (unevaluated) S-expr
function M.parseSexpr (expr)
  local tokenList = M.parseTokens (expr)
  local expr
  local i = 1
  local sexprList = {}
  repeat
    i, sexpr = createSexpr (tokenList, i)
    if sexpr then
      table.insert (sexprList, sexpr)
    end
  until not sexpr
  return sexprList
end


return M
