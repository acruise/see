/*
 */

package see

import org.junit._

/** Tests control constructs
  */
//@Ignore
class ControlTest extends TestCase {
  @Test
  def testParse() {
    println("control statement parsing")
    expect(" true ? 1 : 0", 1)
    expectFail("asrt") {
      expectTrue(" false ?! \"asrt\" ")
    }
    expect(" true ?= 1 ; 0", 1)
  }

  @Test
  def testIf() {
    println("If statement")
    shouldFail("Incomplete.") {
      parse("? 1 2")
    }
    shouldFail("Incomplete.") {
      parse("? 1 : ;")
    }
    expect("true ? 1 : 2;", 1)
    expect("false ? 1 : 2;", 2)

    expect("false ? 1 : true ? 2 : 3", 2)
    expect("false ? 1 : false ? 2 : 3", 3)

    expect("1 ? {1 + 1} : yy ", 2)
    shouldFail("Undefined.") {
      expect("0 ? { fff } : { r }", 0)
    }
    scope.set("r", 99)
    expect("0 ? { fff } : { r }", 99)

    shouldFail("Undefined.") {
      expect("c ? 1 : 0", 0)
    }
    scope.set("c", 99)
    expect(1)
    scope.set("c", -1)
    expect(0)

    expect( """ () ? 1 : 0""", 0)
    expect( """ (1, 2) ? 1 : 0""", 1)

    // must not access xxx
    expect( """ defined(xxx) ? xxx : 0""", 0)

    expect( """ "" ? 1 : 0""", 0)

  }

  @Test
  def testLoop() {
    println("Loop statement")
    shouldFail("Incomplete.") {
      // will parse, but not eval
      expectFalse("false ?? 1 : ")
    }
    // we should catch at least the trivial case
    shouldFail("Infinite.") {
      expect("true ?? 1 : 2", 1)
    }

    scope.set("x", 3)
    scope.set("y", 1)
    expect("x > 0 ?? x -= 1; y*=2 : 0", 8)
    scope.set("x", 3)
    scope.set("y", 2)
    expect("x > 0 ?? {x -= 1; y*=2} : 0", 16)
    // Scoping failure -> running infinite!
    // No idea how to catch this with reasonable effort
    // expect("x > 0 ?? {_x -= 1; y*=2} : 0", 8)

  }

  @Test
  def testWhileIf() {
    println("If/while combination")
    node = parse(" y = 1; x > 0 ?? x -= 1; y *= 2 : x < 0 ? -1 : 0")

    scope.set("x", 8)
    expect(256)

    scope.set("x", 0)
    expect(0)

    scope.set("x", -5)
    expect(-1)
  }

  @Test
  def testReturn() {
    println("Premature return")
    var prog = """
		1;
		x ?= 42;
		5
               		"""

    shouldFail("Still undefined.") {
      expect(prog, 42)
    }
    scope.set("x", false)
    expect(5)

    scope.set("x", true)
    expect(42)

    scope.set("x", 0)
    expect(5)

    scope.set("x", 1)
    expect(42)

    prog = """
		1;
		// 0 + ... to force closure into value
		z = 0 + {10;  x ?= 55; y = 20 };
		5
           		"""
    scope.set("y", 0)
    expect(prog, 5)
    testVar("y", 0) // y = 20 should not have been reached
    testVar("z", 55)

    scope.set("x", 0)
    expect(5)
    testVar("y", 20)
    testVar("z", 20)

    prog = """
		f(x) := {
			z = 0 + { x ?= 100;  y += 20 };
			// we should end up here!
			x + 1
			// not here
		}
           		"""
    expectFalse(prog)
    expect("f(0)", 1)
    testVar("z", 40)
    testVar("y", 40)

    expect("f(1)", 2)
    testVar("z", 100)
    testVar("y", 40)

    // xxx doesn't need to exist, if we don't get that far
    expect(" true ?= 1; xxx", 1)
    expect(" !defined(cond) ?= 1; xxx", 1)
    scope.set("cond", 1)
    expect(" cond ?= 1; xxx", 1)

  }


}