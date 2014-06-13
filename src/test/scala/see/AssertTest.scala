/*
 */

package see

import org.junit._

/** Tests assertions and catching.
  */

//@Ignore
class AssertTest extends TestCase {

  @Test
  def testAssert() {
    println("Assert")

    expectTrue( """ true assert "Not thrown" """)
    expectTrue( """ true ?! "Not thrown" """)
    expectFail("Thrown") {
      expectTrue( """ false ?! "Thrown" """)
    }

    expectFail("math") {
      expectTrue( """ 1 > 2 ?! "Strange math" """)
    }
    expectTrue( """ 2 > 1 ?! "Not thrown" """)
  }

  @Test
  def testAssert2() {
    println("Assert in seq")
    var prog = """
			a = 1;
			b = 2;
			a != b ?! "why?";
			c = 3;
               		"""
    expect(prog, 3)

    prog = """
			a = 1;
			b = 2;
			a == b ?! "a=" + a;
           		"""
    expectFail("a=1") {
      expectTrue(prog)
    }
  }

  @Test
  def testAssertInner() {
    println("Assert from inner scope")

    val prog = """
			f() := {
				false ?! "inner"
			}
               		"""
    expectFalse(prog) // not yet called
    expectFail("inner") {
      expectTrue("f()")
    }
  }

  @Test
  def testCatch() {
    println("Catch block")

    // catch block executed
    var prog = """
			{
				false ?! "inner";
				222
			}! 333}
               		"""
    expect(prog, 333)

    // catch block not executed
    prog = """
			{
				true ?! "inner";
				222
			}! 333}
           		"""
    expect(prog, 222)

    // we may also catch other errors,
    scope.set("x", 1)
    prog = """
			y = { x / 0 }! 0};
			y
           		"""
    expect(prog, 0)

    // ... that would otherwise cause failure
    scope.set("x", 1)
    prog = """
			y = { x / 0 };
			y
           		"""
    expectFail("Zero") {
      expect(prog, 1)
    }

    // An empty phrase will yield an empty vector.
    // If that doesn't matter the error is muted.
    prog = """
			{
				false ?! "inner";
				222
			}!};
			10 // error propagation stops here
           		"""
    expect(prog, 10)
  }
}
