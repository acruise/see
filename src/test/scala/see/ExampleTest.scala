/*
 */

package see

import org.junit._

/** Tests examples from documentation.
  */

//@Ignore
class ExampleTest extends TestCase {

  @Test
  def testObfuscated() {
    println("Obfuscated")
    val prog = """
	and = 1;
	xor = 2;
	or = 3;
	if = and or xor xor and or not or and and or xor;
               """
    expect(prog, 3)
  }

  @Test
  def testPatternSample() {
    println("Pattern Sample")
    val prog = """
pred(x) := { 10 < x < 100 };
a = 101;
x = 20;
y = x ?~   "abc"  -> 0x41L :
           '\d+\*a' -> a * int($~~~'(\d*).'@1) :
           111    -> 112 :
           a      -> a + 1:
           $ < 0  -> $ + 1:
           pred   -> 2 * $:
           Number -> bigint($) :
           ?      -> 0L ; // note semicolon after last alternative!
               		   """

    expect(prog, 40)
  }

  @Test
  def testReturnSample() {
    println("Return Sample")
    val prog = """
		x = -1;
		y = {
			x <= 0 ?= 0;
			log10(x);       // not executed
		};             // will end up here
		10 * y;
		// not here!
               	   """
    expect(prog, 0)
  }


  @Test
  def testAssertSample() {
    println("Assert Sample")
    val prog = """
		x = 0;
		{
			y = 10 * {
				x > 0 ?! "Undefined log10() operand";
				log10(x);    // not executed
			};
			y += 5       // not executed
		}!               // error catched here
        0            // result returned in case of error
		}
               	   """
    expect(prog, 0)
  }

}