/*
 */

package see
import org.junit.Assert._
import org.junit._

import see.values._

/** Tests functions calls and definitions.
  */

//@Ignore
class FunctionCallTest extends TestCase {
  @Test
  def testCallParsing() {
    println("Function Call Parsing")

    // regular function calls
    parse("f()")
    parse("f(1)")
    parse("f(a)")
    parse("f(g())")
    parse("x(a,b,c)")
    parse("x(a,b,(c,d,f))")

    // calling vectors
    parse("()(a,b,c)") // rather useless, always returns (), but allowed for formal reasons
    parse("(f)()")
    parse("(f, g)(a, b, c)")
    parse("(f, g)(a, b, (c, d, e))")

    // chained calls: function result is assumed to be function and will
    // be called with next parameter list:
    parse("f()()")
    parse("f()(a,b,c)")
    parse("f(a,b,c)(d,e,f)")
    parse("f()()()")
  }

  @Test
  def testDefParsing() {
    println("Function Definition Parsing")

    parse("f() := {1; 2; 3}")
    parse("x() := {a + b}")
    parse("x(a,b,c) := {10 * (a + b)}")
    parse("x(a,b,c) := {1}")
    parse("name(a) := {(1, 2, 3)}")
    parse("name(a) := {(1, 2, 3)};")
    shouldFail("Missed Invalid Function name") {
      parse("1x(a,b,c) := {1}")
    }
    shouldFail("Missed Invalid param name") {
      parse("f(a,1b,c) := {1}")
    }
    shouldFail("Missed empty function body") {
      parse("f(a,b,c) := ")
    }
    shouldFail("Missed Invalid param list") {
      parse("f(a,b,c=1) := {1}")
    }
    shouldFail("Missing separator") {
      parse("f(a) := {1} g(a) := {a}")
    }
    expect("1; f() := {10}; a = f(); g(x) := {2*x}; g(a)", 20)

  }

  @Test
  def testDefinitions() {
    println("Function Definitions")
    expectFalse( """ f1(x) := {2 * x} """)
    expectFalse( """ f(x) := {3 * x};
				   g(x) := { 4 * x };
				   h(x) := {y = 5 * x}
                 				   """)
    expect("f(2)", 6)
    expect("g(2)", 8)
    expect("h(2)", 10)
    assertFalse(scope contains "y")
    assertFalse(scope contains "x")
    expectFalse( """ f(x) := {x / 2} """)
    expect("f(2)", 1)
    scope.set("y", 1)
    expect("h(3)", 15)
    testVar("y", 15)
    expectFalse( """ h2(x) := {_y = 10 * x} """)
    scope.set("_y", 1)
    expect("h2(3)", 30)
    // must not have changed
    testVar("_y", 1)

    // nested functions:
    expectFalse( """ _f(x) := {3 * x};
				   g(x) := {_f(x) := {2 * x}; _f(x) }
                 				   """)
    // using inner f()
    expect("g(4)", 8)
    // outer f() still there?
    expect("_f(4)", 12)
  }


  @Test
  def testCallWithValue() {
    println("Call by Value")

    expectFalse("c() := {5}")
    expectFalse("f(x) := {2 * x}")
    expect("c()", 5)
    expect("f(3)", 6)
    expect("f(c())", 10)
    shouldFail("int * string not supported") {
      expect( """ f("a") """, 0)
    }

    // must not affect outer scope
    scope.set("x", 42)
    expect("f(-1)", -2)
    testVar("x", 42)
    expect("f(x)", 84)
    testVar("x", 42)

    // redefine:
    expectFalse("c() := {6}")
    expect("c()", 6)
    expectFalse("f(x) := {x * 2}")
    expect( """ f("a") """, Str("aa")) // string * int is allowed

    // globals:
    expectFalse("g(x) := {b = 5; 2 * x + a}")
    shouldFail("Undefined globals") {
      expect("g(1)", 0)
    }
    scope.set("a", 10)
    expect("g(1)", 12)
    assertFalse(scope contains "b") // set in local scope
    scope.set("b", 0)
    expect("g(2)", 14)
    testVar("b", 5) // now in outer scope
    expectFalse("h(x) := {a = 2 * x}")
    expect("h(2)", 4)
    testVar("a", 4)

    scope.set("x", 6)
    // assignment to parameter is fine, although pointless here
    expectFalse("h(x) := {x += 2}")
    expect("h(x)", 8)
    // ... but will not affect outer scope
    testVar("x", 6)
  }

  @Test
  def testCallWithClosure() {
    println("Call with Closure")

    expectFalse("c() := {5}")
    expectFalse("f(x) := {2 * x}")
    expect("f( {1; 1 + 2} )", 6)

    // assignment to parameter
    expectFalse("h(x) := {x += 2}")
    // Note that this will replace and thereby destroy the inner reference
    expect("h(7)", 9)

    // here, a closure should really make a difference:
    expectFalse("h(x) := {x ?? a += 2 : 0}")

    shouldFail("a still undefined") {
      expect("h(7)", 9)
    }
    scope.set("a", 42)
    expect("h(false)", 0)
    testVar("a", 42)
    scope.set("a", 0)
    // This would cause an infinite loop:
    // expect("h(a < 3)", 4)
    // but this should work:
    expect("h( {a < 3} )", 4)
    testVar("a", 4)

  }

  @Test
  def testExamples() {
    println("Function examples")

    // classic factorial (recursive)
    expectFalse("fact1(x) := {x < 2 ? 1 : x * fact1(x-1)}")
    // same as loop
    expectFalse("fact2(x) := {_a = 1; _c = 1; _c < x ?? _c += 1; _a *= _c : 1}")
    // or shorter...
    expectFalse("fact3(x) := {_a = 1; x > 1 ?? x -= 1; _a *= x+1 : _a}")

    expect("fact1(0)", 1)
    expect("fact2(0)", 1)
    expect("fact3(0)", 1)
    println("Scope: " + scope)
    expect("fact1(5)", 120)
    expect("fact2(5)", 120)
    expect("fact3(5)", 120)
  }

  @Test
  def testProgram() {
    println("Whole program test")
    var prog = """
			f(x) := { 2 * x };
			g(a, b) := { a + b };

			n = f(3);
			m = g(6, 5);
			n + m
               		"""
    expect(prog, 17)

    scope.clear()

    // Not a program, but a single fdef:
    prog = """
			f(x) := { n = 5 * x;
			m = (5 + 6);
			n}
           		"""
    expectFalse(prog)
    // should not have been executed
    assertFalse(scope contains "n")
    // since all variables are local, they shouldn't show up
    expect("f(3)", 15)
    assertFalse(scope contains "n")
    assertFalse(scope contains "m")
  }


  @Test
  def testAssign() {
    println("Function assignment")

    val prog = """
			f(x) := {2 * x};
			g(x) := {3 * x};
			F(x) := {2 * x};
			z = f;
			z(3)
               		"""
    expect(prog, 6)

    // Since functions with one parameter accept any vector,
    // they also accept the empty one.
    expectTrue("z() == ()")
    expect("z(1, 2)", IntVector(2, 4))
    expect("z(5)", 10)
    // we may redefine this:
    expectFalse("z = g")
    expect("z(5)", 15)
    // and move it around
    expectFalse("q = z")
    expect("q(8)", 24)
    expect("q = f; q(8)", 16)
    expect("z(5)", 15)
    expect("F(5)", 10) // Will parse into a constant, not into a fcall!
    expectFalse("Q = f")
    expect("Q(8)", 16) // same, transparent from outside
    shouldFail("Function assignment to const not allowed") {
      expectFalse("Q = g")
    }
  }


  @Test
  def testFuncArgs() {
    println("Function arguments")

    val prog = """
			f(x) := {2 * x};
			g(x) := {3 * x};

			call(f, x) := { 1 + f(x) };

			z = f;
			c = call
               		"""
    expectFalse(prog)
    shouldFail("call needs function") {
      expect("call(1, 2)", 0)
    }
    shouldFail("call needs 2 params") {
      expect("call(f)", 0)
    }
    shouldFail("Invalid arguments for call") {
      expect("call(f, g)", 0)
    }
    expect("call(f, 5)", 11)
    expect("call(g, 5)", 16)

    expect("c(f, 5)", 11)
    expect("c(g, 5)", 16)
  }

  @Test
  def testVarArgs() {
    println("Function variable arguments")

    val prog = """
			f(x) := {2 + x};
			g(x) := {3 + x};
			h(x, y) := {x * y}
               		"""
    expectFalse(prog)

    expect("f(1, 2)", IntVector(3, 4))
    expect("f(1, 2, 3)", IntVector(3, 4, 5))
    shouldFail("call needs 2 params") {
      expect("h(1)", 0)
    }
    expect("h(2, 3)", 6)
    expect("h((1,2), (3,4,5))", IntVector(3, 8))
    shouldFail("call needs 2 params") {
      expect("h(1, 2, 3)", 0)
    }
    expect("x = (3,4); h(x)", 12)
    shouldFail("call needs 2 params") {
      expect("x = (3, 4, 5); h(x)", 7)
    }
    expect("x = ((3,4), (5,6)); h(x)", IntVector(15, 24))
    expect("x = (f, g); h(x)(2, 3)", IntVector(7, 8))

  }


  @Test
  def testCurry1() {
    println("Currying 1")

    val prog = """
			f(x) := {x * 2};
			g(x) := {x * 3};
			h(x) := {f(x)};
			curry(c) := {r(f) := {f(c)}; r }
               		"""
    expectFalse(prog)

    expectFalse("c3 = curry(3)")
    expectFalse("c4 = curry(4)")
    shouldFail("Invalid arguments for call") {
      expect("c3(1)", 6)
    }
    expect("h(3)", 6)
    expect("c3(f)", 6)
    expect("c4(g)", 12)

    expect("curry(3)(g)", 9)
  }

  @Test
  def testCurry2() {
    println("Currying 2")

    val prog = """
			f(x, y) := {x * y};
			g(x, y) := {x * 2 * y};
			curry(f, c) := { {f(_, c)} }
               		"""
    expectFalse(prog)
    expectFalse("c3 = curry(f, 3)")
    expectFalse("c4 = curry(f, 4)")
    expectFalse("d5 = curry(g, 5)")

    expect("c3(1)", 3)
    expect("c4(2)", 8)
    expect("d5(2)", 20)

    expect("curry(g, 1)(10)", 20)

  }

}