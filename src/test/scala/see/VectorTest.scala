/*
 */

package see


import org.junit._

/** Tests vector handling.
  * For now, we only test vectors of dim 1, although see will put no limits
  */

//@Ignore
class VectorTest extends TestCase {
  @Before
  override def setUp() {
    //TestCase.
    super.setUp()
    scope.eval(parse("va = (1,2,3)"))
    scope.eval(parse("vb = (4,5,6)"))
  }

  @Test
  def testParsing() {
    println("Vector Parsing")

    parse("(a, b, c)")
    parse("((a, b, c))")
    parse("((a, b, c), (1))")
    parse("((a, true, 1.0), (1))")

    shouldFail("Failed to catch syntax error.") {
      node = parse("(1, 2, 2, )")
      println(node)
    }
    // actually, this should be made to work, too
    shouldFail("Failed to catch syntax error.") {
      parse("(1 ; 2)")
    }
  }

  @Test
  def testComposition() {
    println("Vector Composition")
    expect("va", IntVector(1, 2, 3))
    expect("vb", IntVector(4, 5, 6))
    expect("(4, 2 + 3, yy = 6)", IntVector(4, 5, 6))
    testVar("yy", 6)

    expectTrue("v1 = (1, true, 1.0)")
    expect("len(v1)", 3)
    expectFalse("v2 = ((1, 1), (true), (1.0, (1, 2), false))")
    expect("len(v2)", 3)
    expectTrue("v3 = (1, va, vb, 6)")
    expect("len(v3)", 4)

    shouldFail("Did not catch undefined.") {
      expectTrue("(1 , x)")
    }
    scope.set("x", 1)
    expect(IntVector(1, 1))

    expectTrue("defined((1, x))")
    expectFalse("defined((1, y))")
  }

  @Test
  def testCall() {
    println("Vector Calling")

    // Calling an empty vector with any arguments results in empty vector
    // However, arguments must be still defined.
    expectTrue("()(1,2,Int) == ()")

    // If elements are not callable, arguments are ignored
    expect("(1,2,3)(1,true, Value)", IntVector(1, 2, 3))
    expect("va(1,true, Value)", IntVector(1, 2, 3))

    expectFalse("f(x) := { 2 * x}")
    expect("(1,f,3)(5)", IntVector(1, 10, 3))
    expectFalse("g(x,y) := { y * x}")
    expect("(1,g)(5,6)", IntVector(1, 30))
    expectTrue("(1,f,3)(va) == (1,(2,4,6),3)")

  }

  @Test
  def testDefine() {
    println("Vector, Defined")

    val prog = """
			f(x) := { x * a};
			g(x) := { x * 2};
			vf = (1, f);
			vg = (1, g)
               		"""

    expectFalse(prog)
    expectTrue(" defined(va)")
    expectTrue(" defined(vf)")
    expectTrue(" defined(vg)")
    expectTrue(" defined(va(1))")
    expectFalse(" defined(vf(1))")
    expectTrue(" defined(vg(1))")
    scope.set("a", 1)
    expectTrue(" defined(vf(1))")
  }

  @Test
  def testSubscript() {
    println("Vector Subscripting")

    expect("va@0", 1)
    expect("va@1", 2)
    expect("vb@2", 6)
    shouldFail("Bounds check failed.") {
      expect("va@3", 0)
    }

    expect("va@-1", 3)
    expect("va@-2", 2)
    expect("va@-3", 1)
    shouldFail("Bounds check failed.") {
      expect("va@-4", 0)
    }

    expectTrue("defined((1, va)@1)")
    expectFalse("defined((1, y)@0)")
    expectFalse("defined((1, y)@1)")
    expectFalse("defined(va@4)")
    expectFalse("defined(va@ -4)")
    expectTrue("defined(va@1)")
    expectTrue("defined(va@ -1)")

    expectFalse("defined(()@0)")
    expectFalse("vz = ()")
    expectTrue("defined(vz)")
    expectFalse("defined(vz@ -1)")

    expectFalse("defined(va@index)")
    scope.set("index", 2)
    expectTrue("defined(va@index)")

    expectTrue("vx = (1, (2, 3), 4)")
    expectTrue("len(vx@1) == 2")
    expect("vx@1", IntVector(2, 3))
    expect("vx@(1,1)", 3)

    // subscripting the first element should work, no matter what
    expect("1@(0,0,0,0)", 1)
    expect("(2,3,4)@(0,0,0,0)", 2)
    expect("((9,8),3,4)@(0,0,0,0)", 9)
    expect( """ "abc"@(0,0,0,0)""", "a")
    expect( """ ("abc", 5)@(0,0,0,0)""", "a")
    // ... except for an empty vector/string
    shouldFail("Bounds check failed.") {
      expectTrue( """ ()@(0,0,0,0) == ()""")
    }
    shouldFail("Bounds check failed.") {
      expect( """ ""@(0,0,0,0)""", "")
    }
    // ... or if we hit some undefined on the way
    shouldFail("Bounds check failed.") {
      expect("xx@(0,0,0,0)", 42)
    }
    scope.set("xx", 42)
    expect(42)
    shouldFail("Bounds check failed.") {
      expect("(yy,0)@(0,0,0,0)", 43)
    }
    scope.set("yy", 43)
    expect(43)

    // same for last element
    scope.clear()
    expect("1@(-1,-1,-1,-1)", 1)
    expect("(2,3,4)@(-1,-1,-1,-1)", 4)
    expect("((9,8),3,(4,5))@(-1,-1,-1,-1)", 5)
    expect( """ "abc"@(-1,-1,-1,-1)""", "c")
    expect( """ ("abc", "def")@(-1,-1,-1,-1)""", "f")
    shouldFail("Bounds check failed.") {
      expectTrue( """ ()@(-1,-1,-1,-1) == ()""")
    }
    shouldFail("Bounds check failed.") {
      expect( """ ""@(-1,-1,-1,-1)""", "")
    }
    // ... or if we hit some undefined on the way
    shouldFail("Bounds check failed.") {
      expect("xx@(-1,-1,-1,-1)", 42)
    }
    scope.set("xx", 42)
    expect(42)
  }

  @Test
  def testSlice() {
    println("Vector Slicing")

    // slicing, produces [start , end[ !
    expect("va@@(0, 2)", IntVector(1, 2))
    expect("va@@(1, 3)", IntVector(2, 3))
    expect("va@@(3, 1)", IntVector(3, 2))
    // A slice shall always produce a vector, even id only a single ele,ent is selected
    expect("va@@(1, 2)", IntVector(2))
    // out of bounds shall produce as much as possible
    expect("va@@(1,4)", IntVector(2, 3))
    expect("va@@(-2, -5)", IntVector(2, 1))
    // .. and an empty vector, if it is totally invalid
    expect("len(va@@(4, 100))", 0)
    expect("len(va@@(-5, -10))", 0)
    expect("len(va@@(-5, 10))", 0)
    // Although both indices  are out of bounds, the described range includes
    // the whole vector reversed. Strange, but makes sense, if you think about it.
    // Probably best to avoid such slice indices...
    expect("va@@(10, -10)", IntVector(3, 2, 1))

    expect("va@@(0, len(va))", IntVector(1, 2, 3))
    expectTrue("va@@(0, len(va)) == va")
    expectTrue("va@@(1, 1) == ()")
    expectTrue("defined( va@@(0, 2) )")
    expectTrue("defined( va@@(-1, 0) )")
    // .. consequently, it will be always defined, even if out of bounds.
    expectTrue("defined( va@@(0, 4) )")
    expectTrue("defined( va@@(-1, -5) )")

    // whole vector
    expect("va@@(0, -1)", IntVector(1, 2, 3))
    // reversed
    expect("va@@(-1, 0)", IntVector(3, 2, 1))
    expect("va@@(-1, -2)", IntVector(3))
    expect("va@@(-1, -3)", IntVector(3, 2))
    expect("va@@(-1, -4)", IntVector(3, 2, 1))

    // There's more about that, but that is too esoteric
  }

  @Test
  def testComparison() {
    println("Vector Comparison")

    expectFalse("va == vb")
    expectFalse("va == (4,5,6)")
    expectFalse("va == ()")
    expectTrue("va == (1,2,3)")
    expectTrue("va + 3 == vb")

    expectTrue("va != vb")
    expectTrue("va != (4,5,6)")
    expectTrue("va != ()")
    expectFalse("va != (1,2,3)")
    expectFalse("va + 3 != vb")

    // other relations are not very well defined
    // but at least size comparisons should give expected results:
    expectTrue("va < (0,0,0,0)")
    expectTrue("va > (0,0)")
    expectTrue("va > (1,2)")
    expectFalse("va < (1000,1000)")
  }

  @Test
  def testArith() {
    println("Vector Arithmetics")

    expect("va + 1", IntVector(2, 3, 4))
    expect("va - 1", IntVector(0, 1, 2))
    expect("vx = va * 2", IntVector(2, 4, 6))
    expect("vx / 2", IntVector(1, 2, 3))
    expect("-va", IntVector(-1, -2, -3))

    expect("va + vb", IntVector(5, 7, 9))
    expect("vb - va", IntVector(3, 3, 3))
    expect("vb * va", IntVector(4, 10, 18))
    // scalar product
    expect("vb *+ va", 32)
  }

  @Test
  def testZip() {
    println("Vector Zipping")

    expectTrue("vx = zip(va, vb)")
    expect("vx@0", IntVector(1, 4))
    expect("vx@1", IntVector(2, 5))
    expect("vx@2", IntVector(3, 6))
  }

  @Test
  def testGenerators() {
    println("Vector Generators")
    // Generators
    //expectFalse("gen = {$ += 1}")

    expect("rep(4, 66)", IntVector(66, 66, 66, 66))
    expect("5; rep(4, $+1)", IntVector(6, 6, 6, 6))
    expect("8; vx = rep(5, $+=1 )", IntVector(9, 9, 9, 9, 9))
    expect("8; vx = rep(5, {$+=1} )", IntVector(9, 10, 11, 12, 13))
    expect("vy = pad(vx, 3, 0)", IntVector(9, 10, 11))
    expect("vy = pad(vy, 6, 1)", IntVector(9, 10, 11, 1, 1, 1))
    expect("vy = pad(vy, 2, 0)", IntVector(9, 10))
    expect("99; vy = pad(vy, 3, $)", IntVector(9, 10, 99))
    expect("88; vy = pad(vy, 4, vy@1)", IntVector(9, 10, 99, 10))
    expect("vy = pad(vx, 7, vx@ -1)", IntVector(9, 10, 11, 12, 13, 13, 13))
    expect("-1; vy = pad(va, 5, $-1)", IntVector(1, 2, 3, -2, -2))
    // All elements should be the same...
    expectTrue("vx = rep(4, rnd(0.5) ); true")
    println(result("vx"))
    expectTrue("vx@0 == vx@1 == vx@2")
    // ... while this would be an extreme coincidence here...
    expectTrue("vx = rep(4, {rnd(0.5)} ); true")
    println(result("vx"))
    expectFalse("vx@0 == vx@1 == vx@2")
  }

  @Test
  def testConcatenation() {
    println("Vector Concatenation")
    // Concatenation
    expect("va ++ ()", IntVector(1, 2, 3))
    expect("vx = vb ++ va", IntVector(4, 5, 6, 1, 2, 3))
    expect("4 ++ 5 ++ 7", IntVector(4, 5, 7))
    expect("va ++ 7", IntVector(1, 2, 3, 7))
    // .. shall always produce a vector
    expect("5 ++ ()", IntVector(5))
    expect("() ++ 5", IntVector(5))
    expect("len( () ++ () )", 0)

    expect("vx = va +++ vb; len(vx)", 2)
    expect("vx@0", IntVector(1, 2, 3))
    expect("vx@1", IntVector(4, 5, 6))

    expect("len(() +++ ())", 2) // -> ((),())
  }

  @Test
  def testReduction() {
    println("Vector Reduction")
    // Reduction
    expect("min(va)", 1)
    expect("max(vb)", 6)
    expect("sum(vb)", 15)
    expect("prod(va)", 6)
    expect("mean(va)", 2)
  }

  @Test
  def testFold() {
    println("Vector Folding")

    expectFalse("plus(x,y) := {x + y}")
    expect("fold(1, plus, va)", 7)

    expectFalse("or_(x,y) := {x | y}")
    expect("fold(1, or_, (0x80, 0x20, 0x180))", 0x1a1)
    shouldFail("Invalid operands.") {
      expect("fold(0, or_, (10, 1.5))", 0)
    }
    expect("fold(1, or_, ())", 1)
    expect("fold(1, or_, 4)", 5)

    scope.set("a", 1)
    scope.set("b", 2)
    expectFalse("bor(x,y) := {x || y}") // note that this will not shortcut!
    expectTrue("fold(false, bor, (0, 1.5, \"a\", {a > b} ) )")
  }


}