/*
 */

package see


import org.junit._
import org.junit.Before
import org.junit.Assert._

/** Tests set handling.
*/

//@Ignore
class SetTest extends TestCase {

	// We only test "pure" sets (i.e. such containing values of the same type)
	// Mixed type set behavior is somewhat undefined


    @Before
    override def setUp() {
		//TestCase.
		super.setUp()
		result("ma = map(1->1,2->4,4->16)")
		result("va = (1,2,3,4)")
    }

	@Test
	def testVectorContains() {
		println("Set Vector Containment")

		expectFalse("() @? 1")
		expectFalse("() @?? 1")
		expectFalse("va @? 10")
		expectFalse("va @?? 10")
		expectFalse("va @? -1")
		expectFalse("va @?? -1")
		expectTrue("va @? 1")
		expectTrue("va @?? 1")
		expectTrue("va @? 1.0")
		expectFalse("va @?? 1.0")
		expectTrue("va @? 4")
		expectTrue("va @?? 4")
		expectTrue("va @? 2L")
		expectFalse("va @?? 2L")
		expectTrue("va @? \"7-4\" ")
	}
	
	@Test
	def testMapContains() {
		println("Set Map Containment")

		expectFalse("map() @? 1")
		expectFalse("map() @?? 1")
		expectFalse("ma @? 10")
		expectFalse("ma @?? 10")
		expectFalse("ma @? -1")
		expectFalse("ma @?? -1")
		expectFalse("ma @? 16")
		expectFalse("ma @?? 16")
		expectTrue("ma @? 1")
		expectTrue("ma @?? 1")
		expectTrue("ma @? 4")
		expectTrue("ma @?? 4")
		expectFalse("ma @?? 4.0")
		expectTrue("ma @? 2L")
		expectFalse("ma @?? 2L")
	}

	@Test
	def testVectorUnion() {
		println("Set Union/Vectors")

		expectTrue("() @| () == ()")
		expect("va @| ()", IntVector(1,2,3,4))
		expect("va @| 1", IntVector(1,2,3,4))
		expect("va @| 4", IntVector(1,2,3,4))
		expect("va @| 5", IntVector(1,2,3,4,5))
		expect("5 @| va ", IntVector(1,2,3,4,5))
		expectTrue("va @| \"a\" == va ++ \"a\" ")
		expect("va @| (1,2)", IntVector(1,2,3,4,1,2)) // builds multi-set!
		expect("va @| (1,1)", IntVector(1,2,3,4,1,1))

		expect("va @| map()", IntVector(1,2,3,4))
		expect("va @| ma", IntVector(1,2,3,4, 1,2,4))
		expect("va @| map(1->5,7->9)", IntVector(1,2,3,4,1,7))

	}

	@Test
	def testMapUnion() {
		println("Set Union/Map")

		expectTrue("map() @| () == map()")
		expectTrue("map() @| map() == map()")
		expectTrue("ma @| () == ma")
		expectTrue("ma @| 1 == ma")
		expectTrue("ma @| 4 == ma")
		expectTrue("ma @| 5 == map(1->1, 2->4, 4->16, 5->5)")
		expectTrue("5 @| ma == map(1->1, 2->4, 4->16, 5->5)")
		expectTrue("ma @| map(1,2) == map(1->1, 2->2, 4->16)")
		expectTrue("ma @| map(1->10,6) == map(1->10, 2->4, 4->16, 6->6)")

		expectTrue("ma @| va == map(1->1, 2->2, 4->4, 3->3)")
		expectTrue("ma @| map(1->5,7->9) == map(1->5, 2->4, 4->16, 7->9)")

		expectTrue("1 @| 2 == map(1->1, 2->2)")

	}

	@Test
	def testVectorIntersect() {
		println("Set Intersect/Vectors")

		expectTrue("() @& () == ()")
		expectTrue("va @& () == ()")
		expect("va @& 1", IntVector(1))
		expect("va @& 4", IntVector(4))
		expectTrue("va @& 5 == ()")
		expect("va @& va ", IntVector(1,2,3,4))
		expect("va @& (1,2)", IntVector(1,2))
		expect("va @& (1,1)", IntVector(1))
		expect("(1,1,2,3,4) @& (1,1,4)", IntVector(1,1,4))

		expectTrue("va @& map() == ()")
		expect("va @& ma", IntVector(1,2,4))
		expect("va @& map(1->5,7->9)", IntVector(1))
	}

	@Test
	def testMapIntersect() {
		println("Set Intersect/Maps")

		expectTrue("map() @& () == map()")
		expectTrue("map() @& map() == map()")
		expectTrue("ma @& () ==  map()")
		expectTrue("ma @& 1 == map(1->1)")
		expectTrue("ma @& 4 == map(4->16)")
		expectTrue("ma @& 5 == map()")
		expectTrue("5 @& ma == map()")
		expectTrue("ma @& map(1,2) == map(1->1, 2->4)")
		expectTrue("ma @& map(1->10,6) == map(1->1)")

		expectTrue("ma @& va == map(1->1, 2->4, 4->16)")
		expectTrue("ma @& map(1->5,7->9) == map(1->1)")

		expectTrue("1 @& 1 == map(1->1)")
		expectTrue("1 @& 2 == map()")
	}

	@Test
	def testVectorDifference() {
		println("Set Difference/Vectors")
		expectTrue("() @^ () == ()")
		expect("va @^ () ", IntVector(1,2,3,4))
		expect("va @^ 1", IntVector(2,3,4))
		expect("va @^ 4", IntVector(1,2,3))
		expect("va @^ 5", IntVector(1,2,3,4))
		expectTrue("va @^ va == ()")
		expect("va @^ (1,2)", IntVector(3,4))
		expect("va @^ (1,1)", IntVector(2,3,4))
		expect("(1,1,2,3,4) @^ (1,4)", IntVector(1,2,3))
		expect("(1,1,2,3,4) @^ 1", IntVector(2,3,4))

		expect("va @^ map()", IntVector(1,2,3,4))
		expect("va @^ ma", IntVector(3))
		expect("va @^ map(1->5,7->9)", IntVector(2,3,4))
	}

	@Test
	def testMapDifference() {
		println("Set Difference/Maps")

		expectTrue("map() @^ () == map()")
		expectTrue("map() @^ map() == map()")
		expectTrue("ma @^ () == ma")
		expectTrue("ma @^ 1 == map(2->4, 4->16)")
		expectTrue("ma @^ 4 == map(1->1, 2->4)")
		expectTrue("ma @^ 5 == map(1->1, 2->4, 4->16)")
		expectTrue("5 @^ ma == map(5->5)")
		expectTrue("ma @^ map(1,2) == map(4->16)")
		expectTrue("ma @^ map(1->10,6) == map(2->4, 4->16)")

		expectTrue("ma @^ va == map()")
		expectTrue("ma @^ map(1->5,7->9) == map(2->4, 4->16)")

		expectTrue("1 @^ 2 == map(1->1)")
		expectTrue("1 @^ 1 == map()")
	}


}