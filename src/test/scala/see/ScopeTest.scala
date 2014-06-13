/*
 *  
 */

package see

import org.junit.Assert._
import org.junit._

//@Ignore
class ScopeTest extends TestCase {

  @Test
  def testExplicitLocals() {
    println("Explicit locals")

    // This is the only situation, I could come up with,
    // where explicit locals are actually required.
    // Apart from aesthetic reasons (if you don't like the underscores),
    // I recommend using implicit locals, nevertheless, because they are
    // always defined, while explicit locals are not.
    val prog = """
			a = 1;
			fs = {
				local (a, b);
				a = 10;
				b = 20;
				// You could not do this with implicits,
				// because they would be redefined within the function's scope,
				// and therefore could not affect the variables in this one.
				(()=>{a}, () =>{b}, {a = _}, {b = _})
			};
			geta = fs@0;
			getb = fs@1;
			seta = fs@2;
			setb = fs@3;
               		"""
    result(prog)
    shouldFail("b undefined in top scope") {
      expect("b", 0)
    }
    expect("geta()", 10)
    expect("getb()", 20)
    expect("seta(30)", 30)
    expect("geta()", 30)
    expect("a", 1)
  }

  @Test
  def testSubContext() {
    println("Scopes SubContext")
    assert(scope.getNames.isEmpty)
    assertNull(scope.getParent)
    val inner = scope.createSubContext
    assertNull(scope.getParent)
    assertSame(inner.getParent, scope)
    val outer = scope

    outer.set("xo", 1)
    inner.set("xi", 2)
    outer.set("z", 10)
    inner.setLocal("z", 20)
    assertTrue(inner contains "xi")
    assertTrue(inner contains "xo")
    assertFalse(outer contains "xi")
    assertTrue(outer contains "xo")
    assertTrue(inner contains "z")
    assertTrue(outer contains "z")

    expect(" xo + 1 ", 2)
    shouldFail("xi unknown in scope") {
      expect(" xi + 1 ", 3)
    }
    expect(" z + 1 ", 11)
  }

  @Test
  def testSetParent() {
    println("Scopes setParent")

    val outer = See.create()
    scope setParent outer
    val inner = scope
    assertNull(outer.getParent)
    assertSame(inner.getParent, outer)

    outer.set("xo", 1)
    inner.set("xi", 2)
    outer.set("z", 10)
    inner.setLocal("z", 20)
    assertTrue(inner contains "xi")
    assertTrue(inner contains "xo")
    assertFalse(outer contains "xi")
    assertTrue(outer contains "xo")
    assertTrue(inner contains "z")
    assertTrue(outer contains "z")

    expect(" xo + 1 ", 2)
    expect(" xi + 1 ", 3)
    expect(" z + 1 ", 21)
    expect(" xi + z ", 22)
    expect(" (xo + xi) + z ", 23)

    expectVar(" x = xo + xi", 3L, "x")
    assertTrue(inner contains "x")
    assertFalse(outer contains "x")

    expectVar(" z = 55", 55L, "z")
    assertEquals(55L, inner get "z")
    assertEquals(10L, outer get "z")

    expectVar(" xo = 100", 100L, "xo")
    assertEquals(100L, outer get "xo")
  }

  @Test
  def testCopySimple() {
    println("Simple Scope Copy")

    scope.set("x", 1)
    val copy = scope.copy
    assertTrue(copy contains "x")
    assertEquals(1L, copy get "x")

    scope.set("x", 2)
    assertEquals(1L, copy get "x")

    copy.set("x", 3)
    assertEquals(2L, scope get "x")

    scope.set("y", 4)
    assertFalse(copy contains "y")
  }

  @Test
  def testCopySubscope() {
    println("SubScope Copy")

    val inner = scope.createSubContext
    val outer = scope

    outer.set("xo", 1)
    inner.set("xi", 2)
    outer.set("z", 10)
    inner.setLocal("z", 20)

    val cinner = inner.copy
    assertNotSame(cinner, inner)
    val couter = cinner.getParent.asInstanceOf[See]
    assertNotNull(couter)
    assertNotSame(couter, outer)
    assertEquals(1L, couter.get("xo"))
    assertEquals(2L, cinner.get("xi"))
    assertEquals(10L, couter.get("z"))
    assertEquals(20L, cinner.get("z"))

    outer.set("xo", 5)
    assertEquals(1L, couter.get("xo"))

    cinner.set("xi", 6)
    assertEquals(2L, inner.get("xi"))

    scope = cinner
    expect(" (xo + xi) + z ", 27)
    scope = couter
    expect(" xo + z ", 11)
    shouldFail("xi unknown in scope") {
      expect(" xi + z ", 16)
    }
  }

  @Test
  def testCopyComplex() {
    // The ugly rest
    println("Complex Scope Copy")

    val outer = scope
    var prog = """
		a = 1; b = 1;
		f(x) := { x - 1 };
		fa(x) := { a + x };
		c = 10; cc = { $ + 1 };
               		"""

    result(prog)
    val inner = scope.createSubContext
    scope = inner

    prog = """
			a = 2;
			e = 6;
			fs = {
				local a;
				a = 20;
				(()=>{a}, {a = _})
			};
			geta = fs@0;
			seta = fs@1;

			s2 = {
				local f;
				d = 50;
				f(x) := { 3 * x };
				fb(x) := { b + x };
				fd(x) := {d + x};
				funcs = (f, fb, fd, f * fb, abs);
			};

			fm = map(
				"geta" -> geta,
				"fb" -> s2@1,
				"fa" -> fa,
				"anon" -> {_ + e},
				"const" -> 42
			);

           		"""
    result(prog)

    assertEquals(12L, outer.get("cc"))
    val cinner = scope.copy
    val couter = cinner.getParent.asInstanceOf[See]

    scope = couter
    assertEquals(2L, couter.get("a"))
    expect("f(33)", 32)
    expect("fa(33)", 35)

    assertEquals(13L, outer.get("cc"))
    assertEquals(14L, outer.get("cc"))

    assertEquals(13L, couter.get("cc"))
    assertEquals(15L, outer.get("cc"))
    assertEquals(14L, couter.get("cc"))

    couter.set("a", 100)
    expect("f(33)", 32)
    expect("fa(33)", 133)

    scope = outer
    expect("f(33)", 32)
    expect("fa(33)", 35)

    scope = cinner
    expect("a", 100)
    expect("fa(33)", 133)

    expect("geta()", 20)
    expect("seta(30)", 30)
    expect("geta()", 30)

    scope = inner
    expect("geta()", 20)
    expect("seta(40)", 40)
    scope = cinner
    expect("geta()", 30)

    expect("(s2@0)(3)", 9) // f
    expect("(s2@1)(3)", 4) // fb
    expect("(s2@2)(3)", 53) // fd
    expect("(s2@3)(3)", 12) // f(fb) => 3 * (b + x)
    expect("(s2@4)(-1)", 1) // abs
    expect("b = 20", 20)
    assertEquals(20L, couter.get("b"))
    assertEquals(1L, outer.get("b"))
    expect("(s2@1)(3)", 23)
    expect("(s2@3)(3)", 69) // 3 * (b + x)

    scope = inner
    expect( """(fm@"geta")()""", 40)
    expect( """(fm@"fb")(4)""", 5)
    expect( """(fm@"fa")(4)""", 6)
    expect( """(fm@"anon")(4)""", 10)
    expect( """(fm@"const")""", 42)

    scope = cinner
    expect( """(fm@"geta")()""", 30)
    expect( """(fm@"fb")(4)""", 24)
    expect( """(fm@"fa")(4)""", 104)
    expect( """(fm@"anon")(4)""", 10)
    expect( """(fm@"const")""", 42)

  }


}