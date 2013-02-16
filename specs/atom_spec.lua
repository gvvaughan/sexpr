local lisp = require "lisp"

describe "with the Atom prototype" do
  describe "when constructing a Nil atom" do
    it 'should be of type "nil"' do
      expect (lisp.Nil.kind).should_be "nil"
    end
    it 'should stringify to "nil"' do
      expect (tostring (lisp.Nil)).should_be "nil"
    end
  end

  describe "when constructing a T atom" do
    it 'should be of type "t"' do
      expect (lisp.T.kind).should_be "t"
    end
    it "should be non-nil valued" do
      expect (lisp.T).should_not_be (lisp.Nil)
    end
    it 'should stringify to "t"' do
      expect (tostring (lisp.T)).should_be "t"
    end
  end

  describe "when constructing a Number atom" do
    before_each do
      value = 42
      atom = lisp.Number {value}
      display_syntax = string.format ("%d", value)
    end

    it 'should be of type "number"' do
      expect (atom.kind).should_be "number"
    end
    it "should stringify to its value" do
      expect (tostring (atom)).should_be (display_syntax)
    end
  end

  describe "when constructing a String atom" do
    before_each do
      value = "Hello, World!"
      atom = lisp.String {value}
      display_syntax = string.format ('"%s"', value)
    end

    it 'should be of type "string"' do
      expect (atom.kind).should_be "string"
    end
    it "should stringify to its value enclosed by quotes" do
      expect (tostring (atom)).should_be (display_syntax)
    end
    it "should escape quote marks within the contained value"
    it "should escape backslashes within the contained value"
  end

  describe "when constructing a Symbol atom" do
    before_each do
      value = "hello-world!"
      atom = lisp.Symbol {value}
      display_syntax = string.format ("%s", value)
    end

    it 'should be of type "symbol"' do
      expect (atom.kind).should_be "symbol"
    end
    it "should stringify to its value" do
      expect (tostring (atom)).should_be (display_syntax)
    end
  end

  describe "when constructing a cons cell" do
    before_each do
      car = lisp.Number {4}
      cdr = lisp.Number {2}
      atom = lisp.Cons {car, cdr}
      display_syntax = string.format ("(%s . %s)", tostring (car), tostring (cdr))
    end

    it 'should be of type "cons"' do
      expect (atom.kind).should_be "cons"
    end
    it "should stringify as a dotted pair" do
      expect (tostring (atom)).should_be (display_syntax)
    end
    it "should not display nil valued cdr" do
      local atom = lisp.Cons {car, lisp.Nil}
      local display_syntax = string.format ("(%s)", tostring (car))
      expect (tostring (atom)).should_be (display_syntax)
    end
    it "should display cons lists without dot separators" do
      local cons = lisp.Cons {car, lisp.Nil}
      cons = lisp.Cons {lisp.Nil, lisp.Cons {cdr, cons}}
      expect (tostring (cons)).should_be "(nil 2 4)"
    end
    it "should allow arbitrarily long lists" do
      local list, display_syntax = lisp.Nil, {} 
      for i = 10000, 1, -1 do
        list = lisp.Cons {lisp.Number {i}, list}
        display_syntax [i] = tostring (i)
      end
      display_syntax = "(" .. table.concat (display_syntax, " ") .. ")"
      expect (tostring (list)).should_be (display_syntax)
    end
  end

  describe "when constructing a function atom" do
    before_each do
      func = function () return "it works!" end
      fname = "callme"
      atom = lisp.Function {fname, func, "flags"}
      display_syntax = string.format ("#'%s", fname)
    end

    it 'should be of type "function"' do
      expect (atom.kind).should_be "function"
    end
    it "should stringify to its display syntax" do
      expect (tostring (atom)).should_be (display_syntax)
    end
    it "should contain a function reference from its constructor" do
      expect (atom.func ()).should_be "it works!"
    end
    it "should contain the flags from its constructor" do
      expect (atom.special).should_be "flags"
    end
  end

  describe "when constructing a function macro" do
    before_each do
      func = function () return "still working" end
      fname = "callme"
      atom = lisp.Function {fname, func, "macro"}
      display_syntax = string.format ("#macro'%s", fname)
    end

    it 'should be of type "function"' do
      expect (atom.kind).should_be ("function")
    end
    it "should stringify to its display syntax" do
      expect (tostring (atom)).should_be (display_syntax)
    end
    it "should contain a function reference from its constructor" do
      expect (atom.func ()).should_be "still working"
    end
  end
end
