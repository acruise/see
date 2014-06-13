/*
 */

package see


import org.junit._

/** Tests builtin functions, that aren't tested anywhere else
  */
//@Ignore
class BuiltinTest extends TestCase {
  @Test
  def testIntConv() {

    println("Int conversion")
    shouldFail("Makes no sense from bool.") {
      expectTrue("int(true)")
    }

    expect("int(1)", 1)
    expect("int(-1)", -1)
    expect("int(1.1)", 1)
    expect("int(-1.1)", -2)
    expect("int(1.9)", 1)
    expect("int(-1.9)", -2)

    shouldFail("Makes no sense from bool.") {
      expectTrue("floor(true)")
    }
    expect("floor(1)", 1)
    expect("floor(-1)", -1)
    expect("floor(1.1)", 1)
    expect("floor(-1.1)", -2)
    expect("floor(1.9)", 1)
    expect("floor(-1.9)", -2)

    shouldFail("Makes no sense from bool.") {
      expectTrue("ceil(true)")
    }
    expect("ceil(1)", 1)
    expect("ceil(-1)", -1)
    expect("ceil(1.1)", 2)
    expect("ceil(-1.1)", -1)
    expect("ceil(1.9)", 2)
    expect("ceil(-1.9)", -1)

    shouldFail("Makes no sense from bool.") {
      expectTrue("round(true)")
    }
    expect("round(1)", 1)
    expect("round(-1)", -1)
    expect("round(1.1)", 1)
    expect("round(-1.1)", -1)
    expect("round(1.9)", 2)
    expect("round(-1.9)", -2)

    shouldFail("Makes no sense from bool.") {
      expectTrue("abs(true)")
    }
    expect("abs(1)", 1)
    expect("abs(-1)", 1)
    expect("abs(1.1)", 1.1)
    expect("abs(-1.1)", 1.1)

  }

  @Test
  def testMiscMath() {

    println("Misc math Functions")

    shouldFail("Makes no sense from bool.") {
      expectTrue("sqrt(true)")
    }
    expect("sqrt(1)", 1.0)
    expect("sqrt(4)", 2.0)
    expect("sqrt(0.25)", 0.5)
    expect("sqrt(0)", 0.0)
    shouldFail("Did not catch domain error.") {
      expectTrue("x = sqrt(-1.5)")
      println("x= " + scope.get("x"))
    }

    shouldFail("Makes no sense from bool.") {
      expectTrue("log(true)")
    }
    expect("log(E)", 1.0)
    expect("log(E**5)", 5.0)
    expect("log(E** -1)", -1.0)
    shouldFail("Did not catch domain error.") {
      expectTrue("log(0)")
    }
    shouldFail("Did not catch domain error.") {
      expectTrue("log(-1)")
    }

    shouldFail("Makes no sense from bool.") {
      expectTrue("log10(true)")
    }
    expect("log10(10)", 1.0)
    expect("log10(10**5)", 5.0)
    expect("log10(0.1)", -1.0)
    shouldFail("Did not catch domain error.") {
      expectTrue("log10(0)")
    }
    shouldFail("Did not catch domain error.") {
      expectTrue("log10(-1)")
    }

    expect("min(1, 2)", 1)
    expect("min(1, 2, -1)", -1)
    expect("max(1, 2)", 2)
    expect("max(1, 2, 2.5)", 2.5)
    expect("mean(1, 2)", 1.5)

    expectTrue("x = rnd(1000); 1")
    expectTrue("y = rnd(1000); 1")
    expectTrue("z = rnd(1000); 1")
    // might fail in extremely rare cases
    expectFalse("x == y && z == y")

    expectTrue("x = rnd(1000.0); 1")
    expectTrue("y = rnd(1000.0); 1")
    expectTrue("z = rnd(1000.0); 1")
    // might fail in even rarer cases
    expectFalse("x == y && z == y")

  }


  @Test
  def testTrigonometry() {

    println("Trigonometric Functions")

    shouldFail("Makes no sense from bool.") {
      expectTrue("sin(true)")
    }
    shouldFail("Makes no sense from bool.") {
      expectTrue("cos(true)")
    }
    shouldFail("Makes no sense from bool.") {
      expectTrue("tan(true)")
    }

    expect("sin(0)", 0.0)
    expect("sin(PI/4) - 1/sqrt(2)", 0.0)
    expect("sin(PI/2)", 1.0)
    expect("sin(PI)", 0.0)
    expect("sin(-PI/2)", -1.0)

    expect("cos(0)", 1.0)
    expect("cos(PI/4) - 1/sqrt(2)", 0.0)
    expect("cos(PI/2)", 0.0)
    expect("cos(PI)", -1.0)
    expect("cos(-PI/2)", 0.0)

    expect("tan(0)", 0.0)
    expect("tan(PI/4)", 1.0)
    // will be quite big, but will not fail.
    //		shouldFail("Did not catch domain error."){
    //			expect("tan(PI/2)", 0.0)
    //		}
    expect("tan(PI)", 0.0)

  }


  @Test
  def testAssignment() {
    println("Builtin assignment")

    expectFalse("f = sqrt")
    expect("f(4)", 2.0)

    expectFalse("vf = (ceil, floor)")
    expect("vf(2.5)", IntVector(3, 2))

    // we need parens here, because it would read as vf @ ( 0(3.5) )
    // otherwise, which doesn't form a valid expression'
    expect("(vf@0)(3.5)", 4)
  }

  @Test
  def testNesting() {
    println("Function nesting")

    expectFalse(
      """
			f(x) := {x + 5 };
			g(x) := { x * 10 }
      			""")

    expectFalse("c  = g * f") // g(f(x))
    expectFalse("d  = f * g") // f(g(x))
    expect("c(1)", 60)
    expect("d(1)", 15)
    shouldFail("Only functions:") {
      expectFalse("d  = f * 2")
    }

    // builtin should also work:
    expectFalse("db  = g * log10")
    expect("db(1000)", 30.0)

  }

  @Test
  def testSort() {

    println("Testing sort")
    expectTrue("v = (3,2,1)")

    // sorting any scalar returns argument
    expect("sort(1)", 1)
    expectTrue("sort(true)")
    // uncomparables remain unchanged
    expectTrue("sort((true, false)) == (true, false)")
    expectTrue("sort((false, true)) == (false, true)")

    expect("sort(1, 2, 3)", IntVector(1, 2, 3))
    expect("sort(3,2,1)", IntVector(1, 2, 3))
    expect("sort(v)", IntVector(1, 2, 3))
    expect("sort(3,2,1, 2, 4, 1)", IntVector(1, 1, 2, 2, 3, 4))
    expect("sort(3,-2,1)", IntVector(-2, 1, 3))

    expectTrue( """sort("def", "abc") == ("abc", "def")""")

    // no type conversions (assume stable sort)
    expect("sort(3.0, 2, 1)@2", 3.0)
    expect("sort(3, 2, 2.0, 1)@2", 2.0)
    expect("sort(3, 2, 2.0, 1)@1", 2)
    // uses liberal equality
    expect( """sort(3, 2, "1")@0""", "1")

  }

  @Test
  def testDistinct() {

    println("Testing distinct")
    expectTrue("v = (3,2,2,1)")

    expect("distinct(1)", 1)
    expectTrue("distinct(true)")
    expectFalse("distinct()")

    expect("distinct(1, 2, 3)", IntVector(1, 2, 3))
    expect("distinct(3,2,1)", IntVector(3, 2, 1))
    expect("distinct(v)", IntVector(3, 2, 1))
    expect("distinct(3,2,1, 2, 4, 1)", IntVector(3, 2, 1, 4))

    expectTrue( """distinct("def", "abc", "def") == ("def","abc")""")

    // uses exact equality
    expect("distinct(2.0, 2, 1)@0", 2.0)
    expect("distinct(2.0, 2, 1)@1", 2)
    expect("distinct(2, 2.0, 1)@0", 2)
    expect( """distinct(1, 1.0, "1")@0""", 1)
    expect( """distinct(1, 1.0, "1")@2""", "1")
    expect( """len(distinct(1, 1.0, "1"))""", 3)
  }

  @Test
  def testUnique() {

    println("Testing unique")
    expectTrue("v = (3,2,2,1)")

    expect("unique(1)", 1)
    expectTrue("unique(true)")
    expectFalse("unique()")

    expect("unique(1, 2, 3)", IntVector(1, 2, 3))
    expect("unique(3,2,1)", IntVector(3, 2, 1))
    expect("unique(v)", IntVector(3, 2, 1))
    expect("unique(3,2,1, 2, 4, 1)", IntVector(3, 2, 1, 4))

    expectTrue( """unique("def", "abc", "def") == ("def","abc")""")

    // uses liberal equality
    expect("unique(2.0, 2, 1)@0", 2.0)
    expect("unique(2.0, 2, 1)@1", 1)
    expect("len(unique(2.0, 2, 1))", 2)
    expect("unique(2, 2.0, 1)@0", 2)
    expect("unique(2, 2.0, 1)@1", 1)
    expect("len(unique(2, 2.0, 1))", 2)

    expect( """unique(1, 1.0, "1")@0""", 1)
    expect( """len(unique(1, 1.0, "1"))""", 1)
    expect( """unique("1", 1, 1.0)@0""", "1")
    expect( """len(unique("1", 1, 1.0))""", 1)

  }

}