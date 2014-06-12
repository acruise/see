/*
 */

package see
import see.nodes.Constant
import values._

import org.junit._
import org.junit.Assert._

/** Tests function scoping behaviour.
 */

//@Ignore
class FunctionScopeTest extends TestCase {


	@Test
	def testScope() {
		println("Function within Scopes")

		// A function should keep track of its definition scope
		var prog = """
			outer() := {
				f(x) := { a * x };

				a = 1;
				f(2) == 2 ?! "1 failed";
				a = 2;
				f(4) == 8 ?! "2 failed";
				{
					a = 3; // changed in outer scope!
					f(2) == 6 ?! "Inner scope failed";
				};
				f(10) == 30 ?! "still 3 failed:" + f(10);
				f
			};
			pf = outer();
			// no way to change a anymore:
			x = pf(7);
			x == 21 ?! "Whats this?: " + x;
			a = 123445;
			x = pf(11);
			x == 33 ?! "Shouldn't have changed: " + x;
		"""

		expectTrue(prog)
	}


	@Test
	def testVarScopes() {
		println("Function Scopes, variable")

		// A function should keep track of its definition scope
		var prog = """
			outer() := {
				f(x) := { a * x };
				g(x) := { a = x };
				// a must be defined here, otherwise this won't work
				a = 2;
				f(2) == 4 ?! "2 failed";
				(f, g)
			};
			vf = outer();
			pf = vf@0;
			pg = vf@1;

			x = pf(7);
			x == 14 ?! "Not 14: " + x;
			a = 123445;
			x = pf(11);
			x == 22 ?! "Not 22: " + x;
			// but now:
			pg(3);
			x = pf(7);
			x == 21 ?! "Not 21: " + x;
		"""

		expectTrue(prog)
	}

	@Test
	def testVarScopes2() {
		println("Function Scopes, globals")

		// A function should keep track of its definition scope
		var prog = """
			outer() := {
				f(x) := { a * x };
				// a must *not* be defined here
				f(2) == 6 ?! "2 failed";
				f
			};
			a = 3;
			pf = outer();
			// since a is defined here, f will see the change
			a = 5;
			x = pf(7);
			x == 35 ?! "Not 35: " + x;
		"""

		expectTrue(prog)
	}


	@Test
	def testInnerFunction() {
		println("Inner function")

		// The block should use its inner fdef first,
		// but have access to outer fdefs
		var prog = """
			f(x) := { 2 * x};
			_g(a, b) := { a + b };

			z = {
				_g(a, b) := { a - b };
				n = f(3);
				m = _g(6, 5);
				n + m
			};
			_g(6, 5)
		"""
		expect(prog, 11) // outer g still intact
		expect("z", 7) // should have used inner g

	}


	@Test
	def testFunctionConst() {
		println("Functions with constants")
		var prog = """
			g(x) := { x * A}
		"""

		expectFalse(prog)
		expect("A = 2", 2)
		expect("g(3)", 6)
		// although g is stable, it cannot be eliminated,
		// because the definition of g itself may change
		assertFalse(parse("g(2)").isInstanceOf[Constant])

		// Not so for internal functions:
		assertTrue(parse("bigint(A)").isInstanceOf[Constant])

		shouldFail("No const redecl."){
			expect ("A = 5", 5)
		}
		expect("g(3)", 6)

		// not even from outside!
		shouldFail("No const redecl."){
			scope.set("A", 3)
		}
		expect("g(3)", 6)

		// but we cannot prevent an external resolver from doing so:
		val r = new Resolver{
			var a = 10

			override def get(name: String) = {
				if (name == "A") new java.lang.Integer(a) else null
			}
			override def set(name: String, v: AnyRef) {}
			override def contains(name: String) = (name == "A")
		}
		scope.clear()
		expectFalse(prog)
		scope.setParent(r)
		expect("g(3)", 30)
		r.a = 20
		// A was undefined at time of definition anyway
		expect("g(3)", 60)

		scope.clear()
		expectFalse(prog)
		expect("g(3)", 60)
		r.a = 30
		val g = scope.get("g")
		
		// supposed to work, because A should not be a constant in such a case.
		expect("g(3)", 90)

	}

	@Test
	def testStableFunction() {
		println("Stable Functions")
		// A function is called stable, if it guarantees to
		// produces the same output, if given the same input arguments.
		// All built-in functions except rnd are stable.
		// User functions may be declared stable, if they are
		// assigned to a constant name.
		// In this case, any attempt to use a non-constant variable
		// from an outer scope causes an evaluation error.
		// Variables resolved by an external resolver also
		// cause an evaluation error, because their behaviour is unknown.

		var prog = """
			f(x) := { x * a};
			g(x) := { x * A};
			h(x) := { x * 2};
			G(x) := { x * A};
		"""
		scope.set("a", 1)
		shouldFail("Must not access a"){
			expectFalse("GG(x) := {x * a}")
		}

		shouldFail("A still undefined"){
			expectFalse(prog)
		}
		assertFalse(scope contains "G")
		scope.set("A", 2)
		expectFalse(prog)

		expect("f(3)", 3)
		expect("g(3)", 6)
		expect("G(4)", 8)
		assertTrue(parse("G(10)").isInstanceOf[Constant])

		scope.set("a", 3)
		expect("f(3)", 9)
		expect("g(3)", 6)
		expect("G(4)", 8)

		// cannot assign instable function to constant
		shouldFail("Stable func must not access outer var"){
			expectFalse("F = f")
		}
		expectFalse("H = h")
		expect("H(10)", 20)
		expectFalse("GG = g")
		expect("GG(5)", 10)
	}

	@Test
	def testStableResolver() {

		val r = new Resolver{
			var a = 10

			override def get(name: String) = {
				if (name.toUpperCase == "A") new java.lang.Integer(a) else null
			}
			override def set(name: String, v: AnyRef) {}
			override def contains(name: String) = (name.toUpperCase == "A")
		}
		expectFalse("g(x) := { x * a}")
		expectFalse("h(x) := { x * A}")

		scope.setParent(r)
		expect("a", 10)
		expect("A", 10)
		shouldFail("Stable func must not access outer var"){
			expectFalse("G(x) := { x * a}")
		}
		shouldFail("Stable func must not access outer var"){
			expectFalse("H(x) := { x * A}")
		}
		expect("g(3)", 30)
		expect("h(3)", 30)
		r.a = 20
		// should not have been simplified
		expect("g(3)", 60)
		expect("h(3)", 60)
	}


	@Test
	def testDefine() {
		println("Functions, Defined")

		var prog = """
			f(x) := { x * a};
			g(x) := { x * A};
			h(x,y) := { x * y};
			fg = f * g;
			i(x) := {sin(x)};
		"""

		expectFalse(prog)
		expectTrue(" defined(h)")
		expectTrue(" defined(f)")
		expectTrue(" defined(g)")
		expectTrue(" defined(i)")
		expectTrue(" defined(i(1))")
		expectTrue(" defined(h(1,2))")
		expectTrue(" defined(fg)")
		expectFalse(" defined(f(1))")
		expectFalse(" defined(g(1))")
		expectFalse(" defined(fg(1))")
		scope.set("a", 1)
		expectTrue(" defined(f(1))")
		expectFalse(" defined(g(1))")
		expectFalse(" defined(fg(1))")
		scope.set("A", 2)
		expectTrue(" defined(g(1))")
		expectTrue(" defined(fg(1))")
	}
}
