/*
 */

package see

import values._

import org.junit._
import org.junit.Assert._

package st {
  class X(val y: Int) {
    def f(n: Int) = {
      y * n
    }
  }

  class XX extends X(4) {
    def g() = y
  }

  object Y {
    def f(n: Int) = {
      2 * n
    }
  }
}

/** Tests native calls.
  */

//@Ignore
class NativeCallTest extends TestCase {

  class NoArgWrapper extends JavaWrapper {
    override def name = "NoArgWrapper"

    def invoke(): Int = 21
  }

  @Test
  def testCallNoArgs() {
    println("Native Call without arguments")
    val wr = new NoArgWrapper
    scope.set("simple", wr)
    expectTrue(" type(simple) == Native")
    expectTrue(" simple istype Native")

    expect("simple()", 21)
    shouldFail("No matching invoke") {
      expect( """ simple(1) """, 0)
    }
  }

  class OneArgWrapper extends JavaWrapper {
    override def name = "OneArgWrapper"

    def invoke(k: Int): Int = {
      k + 10
    }
  }

  @Test
  def testCallOneArg() {
    println("Native Call with one arguments")
    val wr = new OneArgWrapper
    scope.set("simple", wr)

    shouldFail("No matching invoke") {
      expect("simple()", 21)
    }
    expect( """ simple(1) """, 11)
    shouldFail("No matching invoke") {
      expect("simple(1, 2)", 21)
    }
    expect( """ simple(10.5) """, 20)
    expect( """ simple(20L) """, 30)
    expect( """ simple(1 + 3) """, 14)
    shouldFail("Requires explicit conversion") {
      expect( """ simple(" 5 ") """, 15)
    }
    shouldFail("Requires explicit conversion") {
      expect( """ simple(false) """, 11)
    }
  }

  @Test
  def testVoidCall() {
    println("Native Call of void method")
    val wr = new JavaWrapper {
      def invoke(b: Bool) {}
    }
    scope.set("void", wr)

    shouldFail("No matching invoke") {
      expectFalse("void()")
    }
    expectFalse( """ void(true) """)
    shouldFail("No matching invoke") {
      expectFalse("void(true, true)")
    }
    assertSame(see.values.VoidVal, result( """ void(false) """))
  }

  @Test
  def testCallWithObj() {
    println("Native Call with boxed arguments")
    scope.set("f", new JavaWrapper {
      def invoke(obj: java.lang.Double) = {
        obj * 5.5
      }
    })

    shouldFail("No matching invoke") {
      expect("f()", 21)
    }
    expect( """ f(1) """, 5.5)
    shouldFail("No matching invoke") {
      expect("f(1, 2)", 21)
    }
    expect( """ f(0.1) """, 0.55)
    expect( """ f(20L) """, 110.0)
    expect( """ f(1 + 3) """, 22.0)
    shouldFail("Requires explicit conversion") {
      expect( """ simple(" 1 ") """, 5.5)
    }
  }

  @Test
  def testCallWithString() {
    println("Native Call with String argument")
    scope.set("f", new JavaWrapper {
      def invoke(s: String) = {
        s + "!"
      }
    })

    shouldFail("No matching invoke") {
      expect("f()", "!")
    }
    expect( """ f("abc") """, "abc!")
    shouldFail("No matching invoke") {
      expect("f(1, 2)", 21)
    }
    expect( """ f(1) """, "1!")
    expect( """ f(false) """, "false!")
  }

  @Test
  def testAmbigiousCall() {
    println("Native Call with ambigious args")
    scope.set("f", new JavaWrapper {
      def invoke(s: String) = {
        s + "!"
      }

      def invoke(b: Byte) = {
        b + 3
      }
    })
    scope.set("g", new JavaWrapper {
      def invoke(b: Short) = {
        b + 2
      }
    })

    shouldFail("No matching invoke") {
      expect("f()", "!")
    }
    shouldFail("No matching invoke") {
      expect("f(1, 2)", 21)
    }
    expect( """ f("abc") """, "abc!")
    expect( """ f(1) """, 4)
    shouldFail("No matching overload") {
      expect( """ f(false) """, "false!")
    }
    shouldFail("Exact match required") {
      expect( """ f(1.0) """, 4)
    }
    // but this works, if not overloaded
    expect( """ g(1.0) """, 3)
    expect( """ g(1) """, 3)
    shouldFail("No matching overload") {
      expect( """ g("abc") """, "abc!")
    }
    shouldFail("No matching overload") {
      expect( """ g(false) """, "false!")
    }
  }

  @Test
  def testMoreParams() {
    println("Native Call with more args")
    scope.set("f", new JavaWrapper {
      def invoke() = {
        42
      }

      def invoke(s: String) = {
        s + "!"
      }

      def invoke(l: Long) = {
        l + 5
      }

      def invoke(b: Byte, d: Double) = {
        d + b
      }
    })

    expect("f()", 42)
    expect("f(1, 2)", 3.0)
    expect( """ f("abc") """, "abc!")
    expect( """ f(1) """, 6)
  }

  @Test
  def testObjectRef() {
    import st._
    println("Native Call with  object")
    scope.set("f", new JavaWrapper {
      def invoke(x: X, n: Int) = {
        x.f(n)
      }
    })
    scope set("x2", new X(2))
    scope set("x3", new X(3))
    scope set("xx", new XX)
    scope set("y", Y)

    expect("f(x2, 5)", 10)
    expect("f(x3, 5)", 15)
    expect("f(xx, 6)", 24)
    shouldFail("Type failure") {
      expect("f(y, 6)", 12)
    }
  }
}
