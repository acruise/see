/*
 */

package see

import org.junit.Assert._
import org.junit._

/** Tests logical operations.
  */

//@Ignore
class LogicTest extends TestCase {

  @Test
  def testTruth() {
    println("Truth")

    // should actually work
    shouldFail("Failed to catch empty exception.") {
      expectFalse(" ")
    }

    expectFalse(" 0 ")
    expectFalse(" -1 ")
    expectFalse(" -1.0 ")
    expectFalse(" false ")
    expectFalse(" () ")

    expectTrue(" 1 ")
    expectTrue(" 1.0 ")
    expectTrue(" true ")
    expectFalse(" ( 0, 0 ) ")

    expectFalse(" ( 0 ) ") // since this is simplified to " 0 "
    expectTrue(" ( 1 ) ") // since this is simplified to " 1 "

    shouldFail("Undefined should throw.") {
      expectFalse(" undefined ")
    }
  }


  @Test
  def testLiberalEqual() {
    println("Liberal Equal")

    expectTrue("0 == 0")
    expectFalse("0 != 0")
    expectTrue("1 == 1")
    expectFalse("1 != 1")
    expectTrue("2 == 2.0")
    expectFalse("2 != 2.0")
    scope.set("x", 5)
    scope.set("y", 6)
    expectTrue("5 == x")
    expectFalse("5 != x")
    expectFalse("y == x")
    expectTrue("y != x")

    expectFalse("0 == 1")
    expectFalse("5.1 == 5")
    expectTrue("0.5 == 0.5")
    expectFalse("0.5 == -0.5")
    expectTrue("0.5 == 1/2")

    expectTrue("true == true")
    expectTrue("false == false")
    expectFalse("true == false")
    expectFalse("true == (1 == 0)")
    expectTrue("true == (1 == 1)")

    expectTrue("0 NE 1")
    expectFalse("5.1 EQ 5")
    expectFalse("5 EQ 5.1")

    expectTrue( """ 5 == "5" """)
    expectTrue( """ 5 != "5+1" """)
    expectTrue( """ "5" == 5 """)
    expectTrue( """ "5+1" != 5 """)
    expectFalse( """ "abc" == "def" """)
    expectTrue( """ "abc" != "def" """)
    scope.set("abc", "abc")
    expectTrue( """ "abc" == abc """)
    expectTrue( """ "y" != "x" """)

    expectFalse( """ 5 == "five" """)
    expectFalse( """ "five" == 5 """)
    expectTrue( """ 5 != "five" """)
    expectTrue( """ "five" != 5 """)
  }


  @Test
  def testExactEqual() {
    println("Exact Equal")

    expectTrue("0 === 0")
    expectFalse("0 !== 0")
    expectTrue("1 === 1")
    expectFalse("1 !== 1")
    expectFalse("2 === 2.0")
    expectTrue("2 !== 2.0")
    scope.set("x", 5)
    scope.set("y", 6)
    expectTrue("5 === x")
    expectFalse("5 !== x")
    expectFalse("y === x")
    expectTrue("y !== x")

    expectFalse("0 === 1")
    expectFalse("5.1 === 5")
    expectTrue("0.5 === 0.5")
    expectFalse("0.5 === -0.5")
    expectTrue("0.5 === 1/2")

    expectTrue("true === true")
    expectTrue("false === false")
    expectFalse("true === false")
    expectFalse("true === (1 == 0)")
    expectTrue("true === (1 == 1)")

    expectTrue("0 NEE 1")
    expectFalse("5.1 EEQ 5")
    expectFalse("5 EEQ 5.1")

    expectFalse( """ 5 === "5" """)
    expectTrue( """ 5 !== "5+1" """)
    expectFalse( """ "5" === 5 """)
    expectTrue( """ "5+1" !== 5 """)
    expectFalse( """ "abc" === "def" """)
    expectTrue( """ "abc" !== "def" """)
    scope.set("abc", "abc")
    expectTrue( """ "abc" === abc """)
    expectTrue( """ "y" !== "x" """)

    expectFalse( """ 5 === "five" """)
    expectFalse( """ "five" === 5 """)
    expectTrue( """ 5 !== "five" """)
    expectTrue( """ "five" !== 5 """)
  }

  @Test
  def testRelations() {
    println("Relations")

    expectFalse("0 < 0")
    expectFalse("0 > 0")
    expectTrue("1 >= 1")
    expectTrue("1 <= 1")
    expectTrue("3 > 2.0")
    expectFalse("3 < 2.0")
    scope.set("x", 5)
    expectTrue("6 >= x")
    expectFalse("6 <= x")

    expectFalse("0 GT 1")
    expectFalse("5.1 LE 5")
    expectTrue("0.4 LT 0.5")
    expectTrue("0.5 GE 0")

    expectTrue("(1 < 2) && (2 < 3)")
    expectTrue("(3 > 2) && (2 > 1)")

    shouldFail("Relations cannot work on bool operands") {
      expectTrue("3 > false")
    }

    // chained compare:
    expectTrue("1 < 2 < 3")
    expectTrue("3 > 2 > 1")
    expectFalse("3 > 5 > 1")
    expectFalse("3 > 0 > 1")
    expectTrue("3 == 3 == 3")
    expectFalse("3 == 3 == 2")
    expectFalse("3 == 2 == 2")
    expectTrue("1 < 2 < 3 < 4")
    expectTrue("1 == 1 == 1 == 1")
  }

  @Test
  def testBoolOps() {
    println("Boolean Operations")

    expectFalse("false && true")
    expectFalse("false AND true")
    expectTrue("false || true")
    expectTrue("false OR true")



    expectTrue("true || false && true") // because of precedence
    expectTrue("false && true || true") // because of precedence

    expectFalse("false ^^ false")
    expectFalse("true ^^ true")
    expectTrue("false ^^ true")
    expectTrue("true ^^ false")

    expectTrue("!false")
    expectFalse("!true")

    // should work due to truth table:
    expectTrue("! 0")
    expectTrue("! -1e-5")
    expectFalse("! 1")
    expectFalse("! 0.5e-10")
    expectTrue("0 || true")
    expectTrue("false || 1")
    expectTrue("0 || 1")
    expectFalse("false && 1.0")
    expectFalse("-1 && true")
    expectFalse("-1.0 && 1")

    // bool reduction of vectors must be explicitly requested!
    expectTrue("!() == ()")
    expectTrue("!(0) == (true)") // simplifies -> !0 == true -> true
    expectTrue("!(-1, 2) == (true, false)")
    expectFalse("bool(!())") // because !() -> ()
    expectTrue("bool(!(0))")
    expectFalse("bool((-1, 2))")
    expectTrue("bool((3, 2))")

    scope.set("x", false)
    scope.set("y", true)
    expectFalse("x && y")
    expectTrue("x || y")
    expectTrue("!x")
    expectFalse("NOT y")
  }

  @Test
  def testEvalShort() {
    println("Evaluation Shortcuts")

    scope setParent new GuardResolver()
    // contains any name, so use setLocal!

    expectFalse("false && x")
    expectTrue("true || x")

    scope.setLocal("y", true)
    expectTrue("y || x")
    shouldFail("Evaluation expected.") {
      expectTrue("x || y")
    }

    scope.set("y", false)
    expectFalse("y && x")
    shouldFail("Evaluation expected.") {
      expectFalse("x && y")
    }

    expectTrue("true ? true : false")
    expectFalse("false ? true : false")
    expectFalse("y ? true : false")

    scope.set("y", true)
    assertTrue(scope.eval(node).toBool)

    // should not eval unused path:
    expectTrue("true ? true : x")
    expectFalse("false ? x : false")

    // SHOULD eval used path
    scope.setLocal("cond", true)
    shouldFail("Evaluation expected.") {
      expectTrue("cond ? x : false")
    }
    scope.set("cond", false)
    shouldFail("Evaluation expected.") {
      expectTrue("cond ? true : x")
    }

  }

}