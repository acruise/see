/*
 */

package see

import org.junit.Assert._
import org.junit._

//@Ignore
class VariableTest extends TestCase {

  @Test
  def testNames() {
    println("Var Names")

    shouldFail("Not a name.") {
      parse("01 = 5")
    }
    expect("x = 3; x", 3)
    shouldFail("Literal, no name.") {
      parse("true = 5")
    }
    shouldFail("Literal, no name.") {
      parse("E = 5")
    }
    // names starting with literals should work, however
    // all other cases are uncritical anyway
    expect("Ex = 4; Ex", 4)
    expect("Intx = 5; Intx", 5)
    expect("truex = 6; truex", 6)
    expect("REGEX_HEXx = 7; REGEX_HEXx", 7)
    expect("definedx = 8; definedx", 8)
    expect("istypex = 9; istypex", 9)

  }


  @Test
  def testBinding() {
    println("Bindings")
    assert(scope.getNames.isEmpty)
    expectVar(" 555 ", 555L, "$")

    scope.clear()
    assert(scope.getNames.isEmpty)

    expectVar(" x = 10 ", 10L, "$")
    assertEquals(10L, scope get "x")

    expectVar(" x + 10 ", 20L, "$")
    assertEquals(10L, scope get "x")

    scope.set("y", 42)
    expectVar(" x + y ", 52L, "$")

    node = parse(" abc ")
    shouldFail("Didn't catch unresolved variable.") {
      scope.eval(node)
    }
    scope.set("abc", 123)
    expectVar(123L, "$")
  }

  @Test
  def testCleanParse() {
    println("Clean Parsing")
    assertTrue(scope.getNames.isEmpty)

    // Parsing any expression must not affect the global scope,
    // no matter what.
    // This test is not so trivial as it may sound, since simplification
    // requires nodes to be at least partially evaluated.

    val prog = """
			a(x) := { b = 2 * x };

			c = 5;
			c += 5;
			{1;  d = 6; 2 };
			(1, {d = 7}, 3);

			a(10);
			a(c);

               		"""
    assertTrue(scope.getNames.isEmpty)
    parse(prog)
    assertTrue(scope.getNames.isEmpty)

    expect("a=0; b=0; c=0; d=0", 0)
    parse(prog)

    testVar("a", 0)
    testVar("b", 0)
    testVar("c", 0)
    testVar("d", 0)
  }


  @Test
  def testDefine() {
    println("Define")

    expectTrue(" defined(1)")

    // defined is prefix, so parentheses are optional:
    expectTrue(" defined 1")

    // result of defined is always defined, even if argument isn't:
    expectTrue("defined defined xxx")

    expectFalse(" defined(x)")
    scope.set("x", 1)
    assertTrue(scope.eval(node).toBool)

    scope.clear()
    assertFalse(scope.eval(node).toBool)

    scope.set("x", 0)
    assertTrue(scope.eval(node).toBool)

    shouldFail("Undefined should throw.") {
      expectFalse("x + y") // y undefined
    }
    expectFalse("defined(y)")
    expectFalse("defined(x + y)")

    expectTrue("y = 1")
    expectTrue("defined(y)")
    val n = parse("c = a + b")
    shouldFail("Should not evaluate undefined assignment") {
      scope.eval(n)
    }
    expectFalse("defined(c)")
    expectTrue("!defined(c)")

    scope.set("a", 1)
    scope.set("b", 2)
    // re-evaluation is required here
    scope.eval(n)
    expectTrue("defined(c)")
    expect("c", 3)
  }

  @Test
  def testAssign() {
    println("Assign")
    assert(scope.getNames.isEmpty)

    // indirect logic consts:
    expectTrue("bt = true")
    expectFalse("bf = false")

    expectVar(" x = 10; x", 10L, "$")
    expectVar(" x += 5 ", 15L, "x")
    expectVar(" x -= 10 ", 5L, "x")
    expectVar(" x *= 4 ", 20L, "x")
    expectVar(" x /= 10 ", 2L, "x")
    shouldFail("Should not accept boolean operand.") {
      expectTrue(" bt += 1 ")
    }
    shouldFail("Should not accept boolean operand.") {
      expectTrue(" x += bt ")
    }

    node = parse(" x /= 0 ")
    shouldFail("Didn't catch zero division.") {
      scope.eval(node)
    }

    expectVar(" x <<= 2 ", 8L, "x")
    expectVar(" x >>= 1 ", 4L, "x")
    expectVar(" x >>= -2 ", 16L, "x")
    expectVar(" x <<= -3 ", 2L, "x")
    expectVar(" x *= 5; x %= 8 ", 2L, "x")
    expectVar(" x |= 3 ", 3L, "x")
    expectVar(" x ^= 5 ", 6L, "x")
    expectVar(" x &= 0xFC ", 4L, "x")
  }

  @Test
  def testConstants() {
    println("Constants")

    // Once is ok
    expect(" C1 = 1 ", 1)
    // but
    shouldFail("No reassignment") {
      expect(" C1 = 10 ", 1)
    }
    shouldFail("No reassignment either") {
      expect(" C1 += 10 ", 1)
    }

    expect("C1 + 2", 3)
    val old = node

    // we may not even reassign from outside...
    shouldFail("No external redefine") {
      scope set("C1", 42)
    }

    // ... Should not work in inner scope either
    shouldFail("No inner redefine") {
      expect(" { C1 = 5} ", 5)
    }
  }

  @Test
  def testConstantNames() {
    println("Constant Names")

    val consts = """
			C = 1;
			C1 = 2;
			C$ = 3;
			$C = 4;
			_C = 5;
			#$._0C = 6;
			0
                 		"""

    val vars = """
			Cc = 1;
			cC = 2;
			$ = 3;
			#$._0 = 4;
			#$._0Cc = 5;
			# = 6;
			_ = 7;
			_0 = 8;
			0;
               		"""

    // Once should always work
    expect(consts, 0)
    expect(vars, 0)
    // second time only for vars:
    expect(vars, 0)

    // consts must be checked individually
    shouldFail("No const redefinition") {
      expect("C = 0 ", 0)
    }
    shouldFail("No const redefinition") {
      expect("C1 = 0 ", 0)
    }
    shouldFail("No const redefinition") {
      expect("C$ = 0 ", 0)
    }
    shouldFail("No const redefinition") {
      expect("$C = 0 ", 0)
    }
    shouldFail("No const redefinition") {
      expect("_C = 0 ", 0)
    }
    shouldFail("No const redefinition") {
      expect("#$._0C = 0 ", 0)
    }
  }


  @Test
  def testSymbol() {
    println("Symbolic names")
    assert(scope.getNames.isEmpty)

    shouldFail("Undefined symbol.") {
      expectTrue(" `1 ")
    }
    shouldFail("Undefined symbol.") {
      expectTrue(" `x ")
    }
    shouldFail("Undefined symbol.") {
      expectTrue( """ `"x" """)
    }

    scope set("1", 42)
    expect(" `1 ", 42)

    scope set("x", 100)
    expect( """ `"x" """, 100)

    shouldFail("Indirect still undefined.") {
      expect(" `x ", 21)
    }
    scope set("x", "y")
    shouldFail("y still undefined.") {
      expect(21)
    }
    scope set("y", 21)
    expect(21)

    scope set("21", 96)
    expect(" ``x ", 96)

  }

  @Test
  def testLvalueSymbol() {
    println("Symbolic names as lvalue")
    assert(scope.getNames.isEmpty)

    // Reassign
    scope set("1", 42)
    expect(" `1 ", 42)
    expect(" `1 = 43", 43)
    testVar("1", 43)

    scope set("x", 100)
    expect( """ `"x" """, 100)
    expect( """ y = "x" """, "x")
    expect( """ `y """, 100)

    expect( """ `"x" = 20""", 20)
    testVar("x", 20)
    testVar("y", "x")

    expect( """ `y = 30""", 30)
    testVar("x", 30)
    testVar("y", "x")
    expect( """ `y += 30""", 60)
    testVar("x", 60)
    testVar("y", "x")

    // Create:
    expect( """ `2 = 222""", 222)
    testVar("2", 222)

    expect( """ z = "zz" """, "zz")
    expect( """ `z = 123 """, 123)
    testVar("zz", 123)
    // double indirection:
    expect( """ ``z = 456 """, 456)
    testVar("123", 456)

    shouldFail("Indirect still undefined.") {
      expect(" `u += 1 ", 1)
    }
    scope set("u", "uu")
    shouldFail("uu still undefined.") {
      expect(" `u += 1 ", 1)
    }
    scope set("uu", 1)
    expect(" `u += 1", 2)

  }

}