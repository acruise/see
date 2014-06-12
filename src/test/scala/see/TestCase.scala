/*
 */

package see

import org.junit.After
import org.junit.Before
import org.junit.Test
import org.junit.Assert._
import values._

/** Superclass for all see test cases.
 */
class TestCase {

	private[see] object IntVector{
		def apply(vs: Int* ) = new Vector(for(v <- vs) yield Lint(v) )
	}

	private[see] object BoolVector{
		def apply(vs: Boolean* ) = new Vector(for(v <- vs) yield Bool(v) )
	}

	private[see] object BI{
		def apply(s: String) = BigI(BigInt(s))
		def apply(l: Long) = BigI(BigInt(l))
	}
	private[see] object BR{
		def apply(s: String) = BigR(BigDecimal(s))
		def apply(d: Double) = BigR(BigDecimal(d))
	}

	var node: INode = null
	var scope: See = null

    @Before
    def setUp() {
		scope = See.create()
		node = null
    }

    @After
    def tearDown() {
    }

	def parse(expr: String) = {node = scope.parse(expr); node}
	def result(expr: String) = scope eval (parse(expr))
	def result() = scope eval node

	def shouldThrow(failMsg: String)(f: => Any ) {
		try{
			f
			fail(failMsg)
		}
		catch {
			case ex: Exception => println("Ok, got " + ex)
		}
	}

	def shouldFail(failMsg: String)(f: => Any ) {
		try{
			f
			fail(failMsg)
		}
		catch {
			case ex: SeeException => println("Ok, got " + ex)
			case ex: Exception => fail("Unexpected exception: " + ex)
		}
	}

	def expectFail(failCheck: String)(f: => Any ) {
		try{
			f
			fail("Succeeded inexpectedly")
		}
		catch {
			case ex: SeeException => {
					if (failCheck.r.findFirstIn(ex.toString).isEmpty)
						fail("Unexpected fail result: " + ex)
					println("Ok, got " + ex)
				}
			case ex: Exception => fail("Unexpected exception: " + ex)
		}
	}

	def expect(expr: String, result: Val) {
		node = parse(expr)
		val r = scope.eval(node)
		assertEquals(result.getClass, r.getClass)
		assertEquals(result, r)
	}

	def expect(result: Val) {
		assertEquals(result, scope.eval(node))
	}

	def expect(expr: String, result: Long) {
		node = parse(expr)
		expect((result))
	}

	def expect(result: Long) {
		val r = scope.eval(node)
		assertEquals(classOf[Lint], r.getClass)
		assertEquals(result.toLong,  r.toLong);
	}

	def expect(expr: String, result: Double) {
		node = parse(expr)
		expect(result)
	}

	def expect(result: Double) {
		val r = scope.eval(node)
		assertEquals(classOf[Real], r.getClass)
		val d = r.toDouble
		// trigonometric precision isn't THAT good, so stay at 10e-14:
		assertEquals(result, d , 1e-14);
	}

	def expect(expr: String, result: String) {
		node = parse(expr)
		expectStr(result)
	}

	def expectStr(result: String) {
		val r = scope.eval(node)
		assertEquals(classOf[Str], r.getClass)
		assertEquals(result, r.toJava); // not toStr here
	}

	def expectVar(expr: String, result: Any, vn: String) {
		node = parse(expr)
		scope.eval(node)
		assertEquals(result, scope get vn)
	}

	def expectVar(result: Any, vn: String) {
		scope.eval(node)
		assertEquals(result, scope get vn)
	}

	def testVar(vn: String, result: Long) {
		val r = scope get vn
		assertEquals(classOf[java.lang.Long], r.getClass)
		assertEquals(result, r.asInstanceOf[java.lang.Long])
	}

	def testVar(vn: String, result: String) {
		val r = scope get vn
		assertEquals(classOf[java.lang.String], r.getClass)
		assertEquals(result, r.asInstanceOf[java.lang.String])
	}

	def testVar(vn: String, result: Boolean) {
		val r = scope get vn
		assertEquals(classOf[java.lang.Boolean], r.getClass)
		assertEquals(result, r.asInstanceOf[java.lang.Boolean])
	}

	def expectTrue(n: INode) {
		assertTrue(scope.eval(n).toBool)
	}

	def expectTrue(expr: String) {
		node = parse(expr)
		assertTrue(scope.eval(node).toBool)
	}

	def expectFalse(n: INode) {
		assertFalse(scope.eval(n).toBool)
	}

	def expectFalse(expr: String) {
		node = parse(expr)
		assertFalse(scope.eval(node).toBool)
	}

}
