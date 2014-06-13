/*
 */

package see

import java.util.Locale

import org.junit.Assert._
import org.junit._

package rt {
  class X(val y: Int) {
    def f(n: Int) = {
      y * n
    }
  }

  class XX extends X(4) {
    def g() = y

    private def pg() = y

    def ov1(i: Int): Double = i.doubleValue

    def ov1(i: Int, d: Double): Double = d + i

    def ov1(d: Double, i: Int): Double = d * i
  }

  object Y {
    def f(n: Int) = {
      2 * n
    }

    def fvoid(n: Int) {}

    def withX(x: X, n: Int) = x.f(n)
  }
}

/** Tests native calls.
  */

//@Ignore
class ReflectTest extends TestCase {

  @Test
  def testDisable() {
    println("Reflection disable")
    // enabled by default:
    expectFalse( """ x = "see.rt.X":>new(5)""")
    expect( """ x:>f(6)""", 30)

    See.disableJavaReflection()
    shouldFail("Reflection disabled") {
      expectFalse( """ y = "see.rt.X":>new(5)""")
    }
    shouldFail("Reflection disabled") {
      expect( """ x:>f(6)""", 30)
    }

    See.enableJavaReflection()
    expectFalse( """ y = "see.rt.X":>new(5)""")
    expect( """ y:>f(6)""", 30)
  }


  @Test
  def testVoidCall() {
    println("Reflection of void method")
    expectFalse( """ y = "see.rt.Y":>fvoid(5)""")
    assertSame(see.values.VoidVal, result( """  "see.rt.Y":>fvoid(5) """))
  }

  @Test
  def testPrivateCall() {
    println("Reflection of private method")
    expectFalse( """ x = "see.rt.XX":>new() """)
    expect( """ x:>g() """, 4)
    expect( """ x:>f(10) """, 40)
    shouldFail("No access to private method") {
      expect( """ x:>pg()""", 4)
    }
  }

  @Test
  def testObjCall() {
    println("Reflection Call object")
    expectFalse( """ x = "see.rt.X":>new(10)""")
    expect( """ "see.rt.Y":>withX(x, 3) """, 30)
  }

  @Test
  def testOverload() {
    println("Reflection Call of overloaded methods")
    expectFalse( """ x = "see.rt.XX":>new()""")
    expect( """ x:>ov1(3) """, 3.0)
    expect( """ x:>ov1(7L) """, 7.0)
    expect( """ x:>ov1(int("9")) """, 9.0)
    shouldFail("No matching overload") {
      expect( """ x:>ov1("6") """, 6.0)
    }
    expect( """ x:>ov1(2, 5.0) """, 7.0)
    expect( """ x:>ov1(2.0, 5) """, 10.0)
    shouldFail("No matching overload") {
      expect( """ x:>ov1(2.0, 5.0) """, 10.0)
    }
    shouldFail("No matching overload") {
      expect( """ x:>ov1(2, 5) """, 10.0)
    }
    shouldFail("No matching overload") {
      expect( """ x:>ov1(2, "5") """, 10.0)
    }
    shouldFail("No matching overload") {
      expect( """ x:>ov1("2", 5.0) """, 10.0)
    }
  }

  @Test
  def testFieldAccess() {
    println("Reflection Field Access")
    // We need Java here, since Scala fields are always accessed by methods
    expectFalse( """ pt = "java.awt.Point":>new(2,3) """)
    expect( """ pt:>getX() """, 2.0)
    expect( """ pt:>getY() """, 3.0)
    expect( """ pt:>x! """, 2)
    expect( """ pt:>y! """, 3)

    expect( """ pt:>x!  = 1 """, 1)
    expect( """ pt:>getX() """, 1.0)
  }

  @Test
  def testJreMethods() {
    println("Reflection into some JRE methods")
    result( """ str = "java.lang.String" """)
    Locale.setDefault(Locale.US) // ensure, we really get a decimal point
    // Note that varargs are transferred as array:
    expect( """ ss = str:>format("%04x x %.2f", (10, 3.0)) """, "000a x 3.00")
    expectFalse( """ "java.lang.System":>out!:>println("Hello") """)
  }

}
