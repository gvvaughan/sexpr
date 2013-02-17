local lisp = require "lisp"

describe "with the lisp parser" do
  describe "when scanning its input" do
    it "should return an empty table for no input" do
      expect (lisp.parse ("")).should_equal {}
    end
    it "should skip white space" do
      expect (lisp.parse (" \t\n\rt \t\n\r")).should_equal {lisp.T}
    end
    it "should ignore comments" do
      expect (lisp.parse ("t;;nil;;t")).should_equal {lisp.T}
      expect (lisp.parse (";;nil\nt\n;nil")).should_equal {lisp.T}
      expect (lisp.parse (";\n ;nil\r\t;nil \t;nil;; ;\nt")).should_equal {lisp.T}
    end
    it "should not ignore non-whitespace delimiters" do
      expect (lisp.parse (";nil;t\nnil;t\n;;")).should_equal {lisp.Nil}
    end
  end

  describe "when parsing a Nil reference" do
    it 'should recognize read-syntax of "nil"' do
      expect (lisp.parse ("nil")).should_equal {lisp.Nil}
    end
  end

  describe "when reading a T reference" do
    it 'should recognize read-syntax of "t"' do
      expect (lisp.parse ("t")).should_equal {lisp.T}
    end
  end

  describe "when parsing a number" do
    before_each do
      value = 42
      read_syntax = tostring (value)
    end

    it "should construct a number atom" do
      expect (lisp.parse (read_syntax)).should_equal {lisp.Number {value}}
    end
    it "should understand read-syntax for a number" do
      expect (lisp.parse ("42;")).should_equal {lisp.Number {42}}
      expect (lisp.parse ("4 2")).should_equal {lisp.Number {4}, lisp.Number {2}}
      expect (lisp.parse ("42x")).should_not_equal {lisp.Number {42}}
    end
  end

  describe "when parsing a string" do
    before_each do
      value = "Hello, World!"
      read_syntax = string.format ('"%s"', value)
    end

    it "should construct a string atom" do
      expect (lisp.parse (read_syntax)).should_equal {lisp.String {value}}
    end
    it "should understand read-syntax for a string" do
      expect (lisp.parse ('"two\nlines"')).should_equal {lisp.String {"two\nlines"}}
      expect (lisp.parse ('"; comment?"')).should_equal {lisp.String {"; comment?"}}
      expect (lisp.parse ('"2\n;lines"')).should_equal {lisp.String {"2\n;lines"}}
    end
    it "should allow escaped quote marks" do
      expect (lisp.parse ([["escaped \" mark"]])).should_equal {lisp.String {[[escaped " mark]]}}
    end
    it "should allow escaped backslashs" do
      expect (lisp.parse ([["escaped \\ char"]])).should_equal {lisp.String {[[escaped \ char]]}}
    end
    it "should ignore escaped new-lines" do
      expect (lisp.parse ('"nothing\\\n to see"')).should_equal {lisp.String {[[nothing to see]]}}
    end

    it "should diagnose unterminated string" do
      err = track_error (function () lisp.parse ('"no closing quote') end)
      expect (err).should_match ('.*incomplete string: "no closing quote.*')
    end
  end

  describe "when parsing a symbol" do
    before_each do
      value = "hello-world!"
      read_syntax = string.format ("%s", value)

      -- make a silly symbol name from all non-terminal characters
      not_symbol_chars = "; \t\n\r,'`(.)" .. '"'
      silly_name = ""
      for i = 32, 126 do
        local c = string.char (i)
        if not not_symbol_chars:find (c, 1, true) then
          silly_name = silly_name .. c
        end
      end
    end

    it "should construct a symbol atom" do
      expect (lisp.parse (read_syntax)).should_equal {lisp.Symbol {read_syntax}}
    end
    it "should understand read-syntax for a symbol" do
      expect (lisp.parse ("42x")).should_equal {lisp.Symbol {"42x"}}
      expect (lisp.parse ("symname ; comment")).should_equal {lisp.Symbol {"symname"}}
    end
    it "should produce a symbol when a token is nothing else" do
      expect (lisp.parse (silly_name)).should_equal {lisp.Symbol {silly_name}}
    end
  end

  describe "when parsing a cons cell" do
    before_each do
      car = lisp.Number {4}
      cdr = lisp.Number {2}
      read_syntax = string.format ("(%s . %s)", car, cdr)
    end

    it "should construct a cons atom" do
      expect (lisp.parse (read_syntax)).should_equal {lisp.Cons {car, cdr}}
    end
    it "should understand read-syntax for a cons cell" do
      expect (lisp.parse '("Hello". "World!")').should_equal {lisp.Cons {lisp.String {"Hello"}, lisp.String {"World!"}}}
    end
    it "should insert Nil for elided cdr" do
      expect (lisp.parse ("(42)")).should_equal {lisp.Cons {lisp.Number {42}, lisp.Nil}}
    end

    it "should diagnose unterminated cons read" do
      local err = track_error (function () lisp.parse ('(foo . bar') end)
      expect (err).should_contain "parse error: missing ')'"
    end
  end

  describe "when parsing a list" do
    it "should treat the empty list as a reference to Nil" do
      expect (lisp.parse ("()")).should_equal {lisp.Nil}
    end
    it "should understand arbitrarily long lists" do
      local read_syntax, list = {}, lisp.Nil
      for i = 10000, 1, -1 do
	read_syntax[i] = tostring (i)
	list = lisp.Cons {lisp.Number {i}, list}
      end
      read_syntax = "(" .. table.concat (read_syntax, " ") .. ")"
      expect (lisp.parse (read_syntax)).should_equal {list}
    end

    it "should diagnose unterminated list read" do
      local err = track_error (function () lisp.parse ('(foo bar') end)
      expect (err).should_contain "parse error: unexpected end-of-file"
    end
    it 'should diagnose unmatched "("' do
      local err = track_error (function () lisp.parse ('foo bar)') end)
      expect (err).should_contain "parse error: unmatched ')'"
    end
  end
end
