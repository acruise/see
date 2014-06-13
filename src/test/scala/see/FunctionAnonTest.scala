/*
 */

package see

import org.junit._

/** Tests anonymous functions calls and definitions.
  */

//@Ignore
class FunctionAnonTest extends TestCase {
  @Test
  def testDefParsing() {
    println("AnonFunction Definition Parsing")

    parse("() => {1; 2; 3}")
    parse("() => {a + b}")
    parse("(a,b,c) => {10 * (a + b)}")
    parse("(a,b,c) => {1}")
    parse("(a) => {(1, 2, 3)}")
    parse("(a) => {(1, 2, 3)};")
    shouldFail("Missed Invalid param name") {
      parse("(a,1b,c) => {1}")
    }
    shouldFail("Missed empty function body") {
      parse("(a,b,c) => ")
    }
    shouldFail("Missed Invalid param list") {
      parse("(a,b,c=1) => {1}")
    }
    shouldFail("Missing separator") {
      // first part should in fact parse as call
      parse("(a) => {1} (a) => {a}")
    }
    expect("1; f = () => {10}; a = f(); g = (x) => {2*x}; g(a)", 20)
  }

  @Test
  def testShort() {
    println("Anon Function short")
    var prog = """
			f = {_1 + 2 * _2};
               		"""
    expectFalse(prog)
    expect("f(3, 5)", 13)
    expect("g = {_ + _}; g(10)", 20)
    expect("g(5,6)", IntVector(10, 12))
    shouldFail("Wrong arg number") {
      result("h = {_4}; h(1,2,3)")
    }
    expect("h(1,2,3,4)", 4)
    // an Anonym that produces a function :
    result("x = {{ a + _ }}")
    scope.set("a", 1)
    expect("x(2)", 3)
    scope.set("a", 2)
    expect(4)
  }


  @Test
  def testDirectCall() {
    println("Direct Anon Function call")

    // Probably not very useful, but should work nevertheless.
    // Note that parentheses around () => {} are required in this case.
    var prog = """
			((x,y) => { x + y}) (3,4);
               		"""
    expect(prog, 7)

    // The short form is supposed to work without parentheses:
    prog = """
			{_1 + 3 * _2} (3,4);
           		"""
    expect(prog, 15)
  }

  @Test
  def testFuncArgs() {
    println("Anon Function arguments")

    // This makes a bit more sense
    var prog = """
			call(f, y) := { 1 + f(y) };
               		"""
    expectFalse(prog)
    expect("call((x)=>{2*x}, 5)", 11)
  }


  @Test
  def testCurry() {
    println("Anon Currying")

    // Actual use of anon functions:
    var prog = """
			curry(c) := { (f) => {f(c)} }
               		"""
    expectFalse(prog)

    expectFalse("c3 = curry(3)")
    expectFalse("c4 = curry(4)")
    shouldFail("Invalid arguments for call") {
      expect("c3(1)", 6)
    }
    expect("c3((x)=>{2*x})", 6)
    expect("c4((x)=>{3*x})", 12)

    expect("curry(3)((x)=>{3*x})", 9)
  }


}