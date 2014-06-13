/*
 */

package see


import org.junit._

/** Tests table handling.
  */

//@Ignore
class TableTest extends TestCase {


  @Before
  override def setUp() {
    //TestCase.
    super.setUp()
    result("ta = table(0, 10->100)")
    result("tb = table(0, 10->200)")
  }

  @Test
  def testParsing() {
    println("Table Parsing")

    parse("table()")
    parse("table(a, b, c)")
    parse("table(a->x, b->y, c)")
    parse( """table(0 -> 1L, 1.0 -> "abc", 10L->(1,2,3))""")

    shouldFail("Failed to catch syntax error.") {
      node = parse("table(1, 2, 2, )")
      println(node)
    }
  }

  @Test
  def testComposition() {
    println("Table Composition")
    expect("ta@4", 40)
    shouldFail("Undefined key.") {
      expect("ta@true", 0)
    }
    expect("table(1->4, 2->8, yy = 3->16)@0", 4)
    expectTrue("yy == 3->16")
    expectTrue("type(yy) == Assoc")
    expectTrue("type(ta) == Table")
    expectTrue("table(yy) == table(3->16)")

    expectTrue("t1 = table(1->1, 2->2, 3->10.0)")
    expect("len(t1)", 3)
    result("t3 = table(1->1, 2->ta, 3->tb, 4->6)")
    expect("len(t3)", 4)

    shouldFail("Did not catch sequence error.") {
      result("t = table(1->1 , 2->3, -1 -> 5)")
    }

    shouldFail("Did not catch undefined.") {
      expectTrue("t = table(1->1 , 2->x)")
    }
    scope.set("x", 111)
    println(result())
    expect("t@2", 111)


    expectTrue("defined(table(1->1, 1->x))")
    expectFalse("defined(table(1->1, 2->y))")

    shouldFail("Did not catch undefined.") {
      result("t2 = table(1->1 , y->2)")
    }
    result("t2 = table(1->1 , x->2)")
    expect("t2@111", 2)

    expectTrue("defined(table(1->1, x->2))")
    expectFalse("defined(table(1->1, y->2))")

  }

  @Test
  def testInterpolation() {
    println("Table Interpolation")
    result("t = table(-10->-5, 0, 0->10, 10 -> 1, 100 -> 100 )")
    expect("t@-100", -5)
    expect("t@-10", -5)
    expect("t@-1", -0.5)
    expect("t@-0.0001", -0.00005)
    expect("t@0", 10.0)
    expect("t@1", 9.1)
    expect("t@10", 1.0)
    expect("t@11", 1 + (1.0 / 90) * 99)
    expect("t@100", 100)
    expect("t@1e500L", 100)
  }

  @Test
  def testComparison() {
    println("Table Comparison")

    expectTrue("table() == table()")
    expectFalse("table() == ()")
    expectFalse("table(1,2,3) == (1,2,3)")
    expectFalse("table(1,10) == ta")
    expectFalse("table(1->10) == table(2->10)")

    expectFalse("ta == tb")
    expectFalse("ta == table()")
    expectFalse("ta == table(0, 10->1000)")
    expectTrue("ta == table(0->0, 10->100)")
    expectTrue("ta * 2 == tb")

    expectTrue("ta != tb")
    expectTrue("ta != table()")
    expectTrue("ta != table(0, 10->1000)")
    expectFalse("ta != table(0->0, 10->100)")
    expectFalse("ta * 2 != tb")

    // other relations work, but have no defined meaning
  }


  @Test
  def testDefine() {
    println("Table, Defined")

    val prog = """
			f(x) := { x * a};
			g(x) := { x * 2};
			tf = table(0, 0 -> f);
			tg = table(0, 0 -> g)
               		"""

    expectFalse(prog)
    expectTrue(" defined(ta)")
    expectTrue(" defined(tf)")
    expectTrue(" defined(tg)")
    expectTrue(" defined(ta(1))")
    expectFalse(" defined(tf(1))")
    expectTrue(" defined(tg(1))")
    scope.set("a", 1)
    expectTrue(" defined(tf(1))")
  }


  @Test
  def testSubscript() {
    println("Table Subscripting")

    expect("ta@0", 0)
    expect("ta@1", 10)
    expect("tb@1", 20)

    expect("ta@-1", 0)
    expect("tb@-1", 0)

    expect("ta@10", 100)
    expect("ta@100", 100)

    expect("ta@5.5", 55.0)
    expect("ta@2L", BI(20))

    expectTrue("defined(ta@1)")
    expectTrue("defined(table()@0)")

    result("tz = table(1)")
    expectTrue("defined(tz)")
    expectTrue("defined(tz@0)")
    expectFalse("defined(tz@false)")

    expectFalse("defined(ta@index)")
    scope.set("index", 2)
    expectTrue("defined(ta@index)")
    expect("ta@index", 20)

    // Table with 2 dimensions:
    result("tx = table(0 -> ta, 2 -> table(0, 10 -> 20), 3)")
    expect("tx@1", 10) // one dim. interpolation, second table ignored
    expect("tx@2.5", 5.0) // one dim. interpolation, first table ignored
    expect("tx@(1,4)", 24) // two dim.: both tables used

  }


  @Test
  def testArith() {
    println("Table Arithmetics")

    expectTrue("1 + ta == table(0-> 1, 10 -> 101)")
    expectTrue("ta - 1 == table(0-> -1, 10 -> 99)")
    expectTrue("tx = ta * 2; tx == tb")
    expectTrue("tx / 2 == ta")
    expectTrue("-ta == table(0-> 0, 10 -> -100)")

    shouldFail("Illegal operation.") {
      result("ta + tb")
    }

    expectTrue("ta + (1,2) == table(0-> (1,2), 10 -> (101,102))")
    println(result("tx = (1,2) - ta"))
    expectTrue("tx == table(0-> (1, 2), 10 -> (-99, -98))")
  }


  @Test
  def testConcatenation() {
    println("Table Concatenation")
    // Concatenation
    expectTrue("ta ++ table() == ta")
    shouldFail("Illegal operand.") {
      result("ta ++ ()")
    }
    shouldFail("Cannot concat overlaps.") {
      result("tx = ta ++ tb")
    }


    expectTrue("ta ++ table(11->0) == table(0-> 0, 10 -> 100, 11 -> 0)")
    shouldFail("Cannot concat overlaps.") {
      result("ta ++ table(0, -1)")
    }
    expectTrue("table(0 -> -1) ++ ta == table(0-> -1, 0 -> 0, 10 -> 100)")
    expectTrue("-1 ++ ta == table(-1-> -1, 0 -> 0, 10 -> 100)")
    expectTrue("ta ++ (11->0) == table(0-> 0, 10 -> 100, 11 -> 0)")
    // !! but !!
    // !! although consistent, but probably not what was intended
    expectTrue("t = (-1 -> -10) ++ ta; type( t ) == Assoc")

    expectTrue("table(-1 -> -10) ++ ta == table(-1 -> -10, 0 -> 0, 10 -> 100)")
    expectTrue("ta ++ 1000 == table(0-> 0, 10 -> 100, 1000 -> 1000)")

    expect("vx = ta +++ tb; len(vx)", 2)
    expectTrue("type(vx) == Vector")
    expectTrue("vx@0 == ta")
    expectTrue("vx@1 == tb")
  }


  @Test
  def testCall() {
    println("Table Calling")
    // Once in place, a table works more like a function than a container.
    // Consequently, a table call is nearly identical to a subscription.
    // The only difference is that subscription will not work, if the table
    // contains a global reference, that hasn't been resolved yet.


    // an empty table returns zero, whatever arguments
    expectTrue("table()(Int) == 0")

    // If elements are not callable, linear interpolation is used.
    // Excessive arguments are ignored.
    expectTrue("table(1,2,3)(1.5,true, Value) == 1.5")
    expect("ta(1,true, Value)", 10)
    expect("ta(-1)", 0)
    expect("ta(10)", 100)
    expect("ta(100)", 100)

    // If a table contains a function it will be called instead of interpolation:
    // Note that we have to supply some upper limit of the domain!
    result("f(x) := {2 * x}; t = table(-1 -> 0, 0 -> f, 100->f(100))")
    expect("t(-0.5)", 0)
    expect("t(90)", 180)
    expect("t(100)", 200)
    expect("t(200)", 200)
    // we may use a table to transform function calls into subscript syntax:
    expect("t@10", 20)

    // 2-dim table using functions, somewhat conceived
    val prog = """
			g(x,y) := { 2 * y + x*x};
			t = table(0,
				0 -> table(0, 0 ->{g(0,_)}, 11),
				10 -> table(0, 0 ->{g(10,_)}, 11),
				10)
               		"""
    result(prog)
    expect("t(-1, -1)", 0)
    expect("t(20, 20)", 10)
    expect("t(5, 20)", 11)
    expect("t(4,5)", 50) // interpolation between both curves, not g(4,5)!

  }

}