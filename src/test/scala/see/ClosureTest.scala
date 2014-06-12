/*
 *  
 */

package see
import values._

import org.junit._
import org.junit.Assert._

/** Tests anonymous closures.
 */

//@Ignore
class ClosureTest extends TestCase {


	@Test
	def testClosure() {
		println("Closure basics")

		// A block that is assigned to a variable generates a closure.
		var prog = """
			x = z = {a + b}; x + 3
		"""

		scope.set("a", 3)
		shouldFail("Still undefined."){
			expect(prog, 10)
		}
		scope.set("b", 4)
		expect(10)

		// x and z should be a closures, although you won't see that from outside:
		testVar("x", 7)
		testVar("z", 7)
		expect("str(type(x))", "Anonymous")
		expect("str(type(z))", "Anonymous")
		expectTrue("x == z") // Note that both closures will be evaluated before compare!

		// We may change them indirectly:
		scope.set("b", 5)
		expectTrue("z == x")
		expect("z", 8)
		expect("x", 8)
		expectTrue("z == 8")

		// This will not produce a closure:
		prog = """
			a = 1; b = 2; z = 3 + {a + b}
		"""
		expect(prog, 6)
		testVar("z", 6)

		// Same with this one
		prog = """
			z += {a + b}
		"""
		expect(prog, 9)
		testVar("z", 9)
	}

	@Test
	def testLocalVar() {
		println("Closure Local var")

		// Here, changing 'a'  should not affect the closure result.
		var prog = """
			_a = 1; b = 2;
			z = {_a = 3; {_a + b}};
			z1 = z;
			5 + z
		"""
		expect(prog, 10)
		expect("str(type(z))", "Anonymous")
		expect("str(type(z1))", "Anonymous")
		expect("z", 5)
		// b still will:
		scope.set("b", 5)
		expect("z", 8)
		scope.set("_a", 100)
		expect("z", 8)

		// Using a closure as assignment target will destroy it.
		expect("z += 2", 10)
		testVar("z", 10)

		expect("z1 = z1 + 2", 10)
		testVar("z1", 10)
	}

	@Test
	def testCompare() {
		println("Closure comparison")

		// Closures will be evaluated before comparison
		scope.set("a", 10)
		scope.set("b", 12)
		expect("xc = { a + 1} ", 11)
		expect("yc = { b - 1} ", 11)

		expectTrue(" xc == yc ")
		val n1 = node

		// not closures:
		expect("xa = ( a + 1) ", 11)
		expect("ya = ( b - 1) ", 11)

		expectTrue(" xa == ya ")
		val n2 = node

		scope.set("b", 13)
		expectFalse(n1)
		expectTrue(n2)

	}

	@Test
	def testAssign() {
		println("Closure Assign")

		// Closures may be assigned without getting dereferenced.
		scope.set("a", 10)
		scope.set("b", 10)
		expect("xc = { a + 1} ", 11)
		expect("yc = { b - 1} ", 9)

		expect("zc = xc ", 11)

		scope.set("a", 5)
		expect("xc", 6)
		expect("zc", 6)
		expect("zc = yc ", 9)
		expect("xc", 6)
		expect("yc = xc ", 6)
		expect("zc", 9)
	}

	@Test
	def testInternal() {
		println("Internal result")
		//
		// Note that referencing and changing $ is rather susceptible
		// to self references which may cause infinite recursion,
		// and therefore best avoided, but sometimes it comes handy.

		scope.set("r", 0)
		// $ should be initially taken from outer scope,
		// but then redefined within inner!
		expect("5; { $ += 1; r = $; 0} ", 0)
		testVar ("r", 6)
		// If we evaluate this again...
		expect(0)
		// ... we get the same result, because different
		// Closures were created with each call.
		testVar ("r", 6)
		// However:
		expect("5; cl = {r = $ + 2} ", 7)
		testVar ("r", 7)
		expect("5; cl", 9)
		testVar ("r", 9)     // !!! Transferred from last evaluation,
		expect("1; cl", 11)	 // !!! outer won't matter anymore.
		testVar ("r", 11)

	}

	@Test
	def testLocals() {
		println("Implicit locals")

		// Variables starting with _ will be implicitly local,
		// even without declaration.
		// This is rather subtile stuff, too.
		// E.g. { [a] a += 1} will always fail, while { _a += 1 } may succeed.
		// To achieve something similar, you might write: { [x] x = a [a] a = x; ... }
		// which is even more obscure.
		scope.set("_a", 10)

		expect("5; { _a = 25} ", 25)
		testVar("_a", 10)

		// if undefined, they should take their init value from outer scope
		expect("5; c = { _a += 1} ", 11)
		testVar("_a", 10)

		// if any ...
		shouldFail("Still undefined."){
			expect("5; { _b += 1} ", 11)
		}

		// The local should remember its last value without affecting the outer one
		expect("c", 12)
		testVar("_a", 10)

		// Using such constructs may produce very interesting side effects,
		// so beware!
		expectFalse("c == c")
		expectTrue("c < c")
		expectFalse("x = c; x == c")

	}


}