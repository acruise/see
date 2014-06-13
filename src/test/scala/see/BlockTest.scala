/*
 *  
 */

package see
import org.junit.Assert._
import org.junit._

/** Tests block behaviour.
  */

//@Ignore
class BlockTest extends TestCase {

  @Test
  def testEmpty() {

    println("Empty statements")

    node = parse(" ")
    expectFail("Empty") {
      scope eval node
    }

    node = parse(" ;;; ; ; ")
    expectFail("Empty") {
      scope eval node
    }

    node = parse(" {} ")
    expectFail("Empty") {
      scope eval node
    }

    node = parse(" ; ; {} ; {} ;")
    expectFail("Empty") {
      scope eval node
    }
    node = parse(" ; ; {} ; {} ;")
    node = parse(" ; ; {{} ; {}} ;")
    expectFail("Empty") {
      scope eval node
    }

    node = parse("  {;; ; ; ;};{;; }")
    expectFail("Empty") {
      scope eval node
    }
    // getting more and more obscure...
    parse(" {; {{};} } ;")
    parse(" {} ")
    shouldFail("Nesting error ") {
      parse(" {{{} ; {}} ")
    }
    shouldFail("Nesting error ") {
      parse(" {{ {} {} }}} ")
    }
  }


  @Test
  def testConstBlock() {

    println("Constant Block constructs")
    // optional separators
    expect(" {1} ", 1)
    expect(" {1;} ", 1)
    expect(" {1}; ", 1)
    expect(" {; 10;}", 10)
    expect(" 5; {10}; 20", 20)
    expect(" {10}; 20", 20)

    expect("  y = 5; {; x = 10; ;};{; 3; }", 3)
    testVar("y", 5)
    assertFalse(scope contains "x") // because first set in inner scope

    // again, but with x defined in outer scope
    scope.set("x", 1)
    expect(3)
    testVar("x", 10)

    // now with local x declaration:
    scope.set("_x", 1)
    expect("  y = 5; {_x = 10}; 3", 3)
    testVar("_x", 1)
  }

  @Test
  def testBlockOps() {

    println("Block operations")
    // as a block is basically atomic, all operations should succeed
    // Note that closure related stuff is tested elsewhere
    expect(" {1} + 1 ", 2)
    expect(" 3 + {1} ", 4)
    expect(" x = {5} ", 5)
    testVar("x", 5)

    expect(" {2} + {1 + 2} ", 5)
    expect(" {x} + {1 + 2} ", 8)
    shouldFail("Block is not an l-value") {
      expect(" {x} += {1 + 2} ", 8)
    }
    expect("({1}, 2, {100; 1+2})", IntVector(1, 2, 3))
  }

}