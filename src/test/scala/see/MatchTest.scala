/*
 *  
 */

package see

import org.junit._

/* Tests pattern matching.
 */
//@Ignore
class MatchTest extends TestCase {
  @Test
  def testTypeMatch() {

    println("Type Match")
    val prog = """
		y =	x ?~ Bool -> 1 :
				 Int -> 2 :
				 Real -> x + 1:
				 "test" -> 555:
				 ? -> -1;
		y
               		"""

    node = parse(prog)

    scope.set("x", true)
    expect(1)
    scope.set("x", 55)
    expect(2)

    scope.set("x", 1.5)
    expect(2.5)

    scope.set("x", "test")
    expect(555)

    scope.set("x", new Array[Object](0)) // should result in empty vector
    expect(-1)
  }


  @Test
  def testConstMatch() {

    println("Const Match")
    val prog = """
		y =	x ?~ 1 -> 1 :
				 55 -> 2 :
				 "asd" -> 3 :
				 "test" -> 4 :
				 1.5 -> 5 :
				 '\d\d\d' -> 6 :
				 true -> 7 : // exact match required, so nothing but true will be taken.
				 vector(1) -> 8 :
				 () -> 9 :
				 ? -> -1;
		y
               		"""

    node = parse(prog)

    scope.set("x", true)
    expect(7)
    scope.set("x", 55)
    expect(2)
    scope.set("x", 22)
    expect(-1)
    scope.set("x", 1.5)
    expect(5)
    scope.set("x", "test")
    expect(4)
    scope.set("x", new Array[Object](0)) // should result in empty vector
    expect(9)
    scope.set("x", new Array[Object](1))
    expectFail("Undefined") {
      // cannot evaluate null
      expect(-1)
    }
    val x = new Array[Object](1) // note that Array[Int] would not work
    x(0) = new java.lang.Integer(1)
    scope.set("x", x)
    expect(8)
    scope.set("x", "asd")
    expect(3)
    scope.set("x", 123)
    expect(6)
  }


  @Test
  def testSelPatternMatch() {

    println(" Match Sel patterns")
    val prog = """
		y =	x ?~ REGEX_DECIMAL -> 1 :
				 REGEX_FLOAT -> 2 : // must be after DECIMAL, if both are used, because it maches decimal, too
				 REGEX_STRING -> 3 :
				 REGEX_NAME -> 4 :
				 ? -> -1;
		y
               		"""

    node = parse(prog)

    scope.set("x", true) // !! "true" is also a valid name !!
    expect(4)
    scope.set("x", 55)
    expect(1)
    scope.set("x", 22)
    expect(1)
    scope.set("x", 1.5)
    expect(2)
    scope.set("x", "test")
    expect(4)
    scope.set("x", new Array[Object](0)) // should result in empty vector
    expect(-1)
    scope.set("x", "asd")
  }

  @Test
  def testVariableMatch() {

    println(" Match Variables")
    val prog = """
		y =	x ?~ p1 -> 1 :
				 p2 -> 2 :
				 ? -> -1;
		y
               		"""

    node = parse(prog)
    val m = node
    scope.set("x", 5)
    expect(-1) // p1, p2 undefined, so nothing found
    scope.set("p2", 5)
    expect(2)
    scope.set("p1", 5)
    expect(1)
    scope.set("p1", 1.5)
    expect(2)
    // mute unresolved a
    // (1 required, or closure would be evaluated as return value outside of block!)
    expectFalse(" {p1 = {1 + a} }!}")
    node = m

    // matching undefined variables is ok, but not eval errors caused by them
    expectFail("Unresolved") {
      expect(2) // a still undefined
    }

    scope.set("a", 4)
    expect(1)
    scope.set("a", 3)
    expect(2)
    scope.set("x", "4") // performs conversion to string to compare
    expect(1)
    scope.set("x", "'\\d'") // regex match
    expect(-1) // This will NOT match, because a comparison is performed!

  }

  @Test
  def testFunctionMatch() {

    println(" Match Functions")
    val prog = """
		f(x) := {x > 2};
		g(x) := {x < 0};
		h(x) := {x - 1};

		y =	x ?~ f -> 1 :
				 g -> 2 :
				 h -> 3 :
				 ? -> -1;
		y
               		"""
    // Pattern will be called with x as argument.
    // This means function should be some form of predicate,
    // although other functions will work, too.
    node = parse(prog)
    scope.set("x", 5)
    expect(1)
    scope.set("x", 0)
    expect(-1) // actually -1, but numbers <= 0 evaluate to false!
    scope.set("x", 1.5)
    expect(3)

  }

  @Test
  def testStatements() {

    println(" Match Statements")
    val prog = """

		y =	x ?~ 0 < $ < 2 -> 1 :
				 $ < -1 -> 2 :
				 $ + 100 == 200 -> 3 :
				 $ -> 4 :
				 ? -> -1 ;
		y
               		"""
    node = parse(prog)
    val m = node
    scope.set("x", -1)
    expect(4) // $ alway matches, except for really strange closures
    scope.set("x", -2)
    expect(2)
    scope.set("x", 1.5)
    expect(1)
    scope.set("x", 100)
    expect(3)
    expect("5; x = {$ += 1}", 6)
    node = m
    expect(4) // even here, because closure is coerced exactly once,
    // although:
    // in match: expect("x", 7)
    expect("x", 8)
    expect("x", 9)

  }


  @Test
  def testResults() {

    println(" Various result content")
    var prog = """
		y =	x ?~ 1 -> 10; 20 + $ : // bad idea anyway, $ destroyed by 10 !
				 ? -> 0 ;
               		"""
    shouldFail("Result allows only a single statement") {
      node = parse(prog)
    }
    prog = """
		y =	x ?~ 1 -> 100 * $ :
				 2 -> {_a = $; 5 + _a} + 5 : // blocks are allowed
				 ? -> 0 - 1 ;
		y
           		"""
    node = parse(prog)
    scope.set("x", 0)
    expect(-1)
    scope.set("x", 1)
    expect(100)
    scope.set("x", 2)
    expect(12)
  }

}