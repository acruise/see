/*
 */

package see


import org.junit._
import org.junit.Before
import org.junit.Assert._

/** Tests map handling.
*/

//@Ignore
class MapTest extends TestCase {


    @Before
    override def setUp() {
		//TestCase.
		super.setUp()
		result("ma = map(1->1,2->4,4->16)")
		result("mb = map(1->2,2->8,4->32)")
		result("""mc = map("a"->4,"b"->5,"c"->6)""")
    }

	@Test
	def testParsing() {
		println("Map Parsing")

		parse("map()")
		parse("map(a, b, c)")
		parse("map(a->x, b->y, c)")
		parse("""map(true -> 1L, 1.0 -> "abc", false->(1,2,3))""")

		shouldFail("Failed to catch synax error."){
			node = parse("map(1, 2, 2, )")
			println (node)
		}
	}

	@Test
	def testComposition() {
		println("Map Composition")
		expect("ma@4", 16 )
		shouldFail("Undefined key."){
			expect("ma@3", 16 )
		}
		expect("""mc@"c" """, 6 )
		expect("map(1->4, 2->2 + 3, yy = 6->7)@2", 5)
		expectTrue("yy == 6->7")
		expectTrue("type(yy) == Assoc")
		expectTrue("type(mc) == Map")
		expectTrue("map(yy) == map(6->7)")

		expectTrue("m1 = map(1->1, 2->true, 3->1.0)")
		expect("len(m1)", 3)
		expectTrue("m3 = (1->1, 2->ma, 3->mb, 4->6)")
		expect("len(m3)", 4)

		shouldFail("Did not catch undefined."){
			expectTrue("m1 = map(1->1 , 2->x)")
		}
		scope.set("x", 111)
		println(result())
		expect("m1@2", 111)

		expectTrue("defined(map(1->1, 1->x))")
		expectFalse("defined(map(1->1, 2->y))")

		shouldFail("Did not catch undefined."){
			result("m2 = map(1->1 , y->2)")
		}
		result("m2 = map(1->1 , x->2)")
		expect("m2@111", 2)

		expectTrue("defined(map(1->1, x->2))")
		expectFalse("defined(map(1->1, y->2))")
		// we may zip vectors this way:
		expectTrue("map((1,2) -> (3,4)) == map(1->3, 2->4)")
		expectTrue("map((1,2,3) -> 10) == map(1->10, 2->10, 3->10)")
		// but not the other way round:
		result("mx = map(1 -> (1,2,3))") // should really give 1 -> (1,2,3)
		expect("len(mx)", 1)
		expect("mx@1", IntVector(1,2,3))
		expect("mx@(1,1)", 2)

	}


	@Test
	def testComparison() {
		println("Map Comparison")

		expectTrue("map() == map()")
		expectFalse("map() == ()")
		expectFalse("map(1,2,3) == (1,2,3)")
		expectFalse("map(1,2,3) == ma")
		expectFalse("map(1->10) == map(2->10)")

		expectFalse("ma == mb")
		expectFalse("ma == map()")
		expectFalse("ma == map(1->1, 2->4, 3->16)")
		expectTrue("ma == map(1->1, 2->4, 4->16)")
		expectTrue("ma * 2 == mb")

		expectTrue("ma != mb")
		expectTrue("ma != map()")
		expectTrue("ma != map(1->1, 2->4, 3->16)")
		expectFalse("ma != map(1->1, 2->4, 4->16)")
		expectFalse("ma * 2 != mb")

		// other relations work, but have no defined meaning
	}


	@Test
	def testCall() {
		println("Map Calling")

		// Calling an empty vector with any arguments results in empty vector
		// However, arguments must be still defined.
		expectTrue("map()(1,2,Int) == map()")

		// If elements are not callable, arguments are ignored
		expectTrue("map(1,2,3) == map(1,2,3)(1,true, Value)")
		expectTrue("ma(1,true, Value) == ma")

		expectFalse("f(x) := {2 * x}")
		expect("z = map(100,200->f,300)(5); (z@100,z@200,z@300)",
			   IntVector(100, 10, 300))
		expectFalse("g(x,y) := { y * x}" )
		expect("z = map(11,22->g)(5,6); (z@11, z@22)", IntVector(11,30))
		expect("z = map(10,20->f,30)(ma); (z@10, z@(20,2) )", IntVector(10, 8))

	}

	@Test
	def testDefine() {
		println("Map, Defined")

		var prog = """
			f(x) := { x * a};
			g(x) := { x * 2};
			mf = map(1, f);
			mg = map(1, g)
		"""

		expectFalse(prog)
		expectTrue(" defined(ma)")
		expectTrue(" defined(mf)")
		expectTrue(" defined(mg)")
		expectTrue(" defined(ma(1))")
		expectFalse(" defined(mf(1))")
		expectTrue(" defined(mg(1))")
		scope.set("a", 1)
		expectTrue(" defined(mf(1))")
	}

	@Test
	def testDefault() {
		println("Map Default")
		// Sel maps don't support a default value, but the following comes near:
		expect("{ ma@999 }! 42} ", 42)
	}

	@Test
	def testSubscript() {
		println("Map Subscripting")

		expect("ma@1", 1)
		expect("ma@2", 4)
		expect("mb@2", 8)
		shouldFail("Bounds check failed."){
			expect("ma@3", 0)
		}
		shouldFail("Bounds check failed."){
			expect("ma@0", 0)
		}
		shouldFail("Bounds check failed."){
			expect("ma@-1", 0)
		}

		expectTrue("defined(ma@1)")
		expectFalse("defined(map()@0)")
		expectFalse("mz = map()")
		expectTrue("defined(mz)")
		expectFalse("defined(mz@0)")

		expectFalse("defined(ma@index)")
		scope.set("index", 2)
		expectTrue("defined(ma@index)")
		expect("ma@index", 4)

		expectTrue ("mx = map(1, 2 -> map(2->22, 3-> 33), 4)")
		expectTrue ("len(mx@2) == 2")
		expectTrue("mx@2 == map(2->22, 3-> 33)")
		expect("mx@(2,3)", 33)

	}


	@Test
	def testArith() {
		println("Map Arithmetics")

		expectTrue("1 + ma == map(1-> 2, 2 -> 5, 4 -> 17)")
		expectTrue("ma - 1 == map(1-> 0, 2 -> 3, 4 -> 15)")
		expectTrue("mx = ma * 2; mx == mb")
		expectTrue("mx / 2 == ma")
		expectTrue("-ma == map(1-> -1, 2 -> -4, 4 -> -16)")

		shouldFail("Illegal operation."){
			result("ma + mb")
		}

		expectTrue("ma + (1,2) == map(1-> (2,3), 2 -> (5,6), 4 -> (17,18))")
		println(result("mx = (1,2) - ma"))
		expectTrue("mx == map(1-> (0, 1), 2 -> (-3, -2), 4 -> (-15, -14))")
	}


	@Test
	def testConcatenation() {
		println("Map Concatenation")
		// Concatenation
		expectTrue("ma ++ map() == ma")
// although formally true, right generation will discard the empty association.
//		expectTrue("ma ++ () == map(1-> 1, 2 -> 4, 4 -> 16, () -> ())")
//		in favour of this one: map((1,2) -> (3,4)) == map(1->3, 2->4)

		result("mx = ma ++ mb")
		expectTrue("mx == mb") // because all keys should have been updated


		expectTrue("ma ++ map(9->0) == map(1-> 1, 2 -> 4, 4 -> 16, 9 ->0)")
		expectTrue("ma ++ map(0, -1) == map(1-> 1, 2 -> 4, 4 -> 16, 0 ->0, -1 -> -1)")
		expectTrue("map(0, -1) ++ ma == map(1-> 1, 2 -> 4, 4 -> 16, 0 ->0, -1 -> -1)")

		// !!! Result is correct in principle, but rhs won't work this way.
		// Note that there is no way to actually access the key (0,-1), since
		// the subscript m@(0,-1) is equivalent to (m@0)@-1, which doesn't exist here
		//expectTrue("ma ++ (0, -1) == map(1-> 1, 2 -> 4, 4 -> 16, (0,-1) -> (0,-1))")

		expectTrue("(0, -1) ++ ma == (0, -1, map(1-> 1, 2 -> 4, 4 -> 16))")

		// .. shall always produce a map
		expectTrue("ma ++ 7 == map(1-> 1, 2 -> 4, 4 -> 16, 7 -> 7)")
		expectTrue("5 ++ map() == map(5)")
		expectTrue("map() ++ 5 == map(5)")
		expect("len( map() ++ map() )", 0)

		expect("vx = ma +++ mb; len(vx)", 2)
		expectTrue("type(vx) == Vector")
		expectTrue("vx@0 == ma")
		expectTrue("vx@1 == mb")
	}


}