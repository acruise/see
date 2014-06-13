/*
 */

package see

import org.junit._

/** Tests string operations.
  */

//@Ignore
class StringTest extends TestCase {

  @Test
  def testConv() {
    println("String Conversions")

    expectFalse( """ "" """)
    expectTrue( """ "f" """)
    expect( """ len("") """, 0)
    expect( """ len("x") """, 1)
    expect( """ len("xy") """, 2)

    expect( """ str("x") """, "x")
    expect( """ str(1) """, "1")
    expect( """ str(true) """, "true")

    shouldFail("No conversion possible") {
      expect( """ int(" abcd ") """, 1)
    }
    expect( """ int(" 1 ") """, 1)
    expect( """ int("100") """, 100)
    expect( """ int("10 + 20") """, 30)

    expect( """ real("1") """, 1.0)
    expect( """ real("-.1e-10") """, -1e-11)
    expect( """ real("10 + 20") """, 30.0)

    expect( """ "x = " + 5  """, "x = 5")
    expect( """ "v = " + (1, 2)  """, "v = (1, 2)")

    shouldFail("No implicit conversion") {
      expect( """ 1 + "100" """, 101)
    }
    shouldFail("No implicit conversion") {
      expect( """ "100" / 2 """, 50)
    }
    // May also cause strange math ;-)
    expect( """ "100" + 1 """, "1001")
  }

  @Test
  def testComparison() {
    println("String comparison")

    scope set("s", "def")
    expectTrue( """ "" == "" """)
    expectTrue( """ "a" == "a" """)
    expectFalse( """ "a" == "A" """)
    expectTrue( """ "abc" == "abc" """)
    expectFalse( """ "abc" == "abC" """)
    expectTrue( """ "def" == s """)
    expectFalse( """ "de" == s """)

    expectFalse( """ "" != "" """)
    expectFalse( """ "a" != "a" """)
    expectTrue( """ "a" != "A" """)
    expectFalse( """ "abc" != "abc" """)
    expectTrue( """ "abc" != "abC" """)
    expectFalse( """ "def" != s """)
    expectTrue( """ "de" != s """)

    expectFalse( """ "" < "" """)
    expectFalse( """ "a" < "a" """)
    expectFalse( """ "a" < "A" """)
    expectFalse( """ "abc" < "abc" """)
    expectFalse( """ "abc" < "abC" """)
    expectTrue( """ "abC" < "abc" """)
    expectTrue( """ "abc" < "abd" """)
    expectTrue( """ "ab" < "abc" """)
    expectFalse( """ "ab" < "aac" """)

    expectTrue( """ "" >= "" """)
    expectTrue( """ "a" >= "a" """)
    expectTrue( """ "a" >= "A" """)
    expectTrue( """ "abc" >= "abc" """)
    expectTrue( """ "abc" >= "abC" """)
    expectFalse( """ "abC" >= "abc" """)
    expectFalse( """ "abc" >= "abd" """)
    expectFalse( """ "ab" >= "abc" """)
    expectTrue( """ "ab" >= "aac" """)

    expectFalse( """ "" > "" """)
    expectFalse( """ "a" > "a" """)
    expectTrue( """ "a" > "A" """)
    expectFalse( """ "abc" > "abc" """)
    expectTrue( """ "abc" > "abC" """)
    expectFalse( """ "abC" > "abc" """)
    expectFalse( """ "abc" > "abd" """)
    expectFalse( """ "ab" > "abc" """)
    expectTrue( """ "ab" > "aac" """)

    expectTrue( """ "" <= "" """)
    expectTrue( """ "a" <= "a" """)
    expectFalse( """ "a" <= "A" """)
    expectTrue( """ "abc" <= "abc" """)
    expectFalse( """ "abc" <= "abC" """)
    expectTrue( """ "abC" <= "abc" """)
    expectTrue( """ "abc" <= "abd" """)
    expectTrue( """ "ab" <= "abc" """)
    expectFalse( """ "ab" <= "aac" """)

  }

  @Test
  def testNumberComparison() {
    println("String comparison with numbers")
    // hold for positive numbers
    expectTrue( """ "100" == "100" """)
    expectTrue( """ "100" < "101" """)
    expectTrue( """ "101" > "100" """)
    expectTrue( """ "1000" > "100" """)
    // strange for negative numbers
    expectTrue( """ "-100" == "-100" """)
    expectTrue( """ "-100" < "-101" """)
    expectTrue( """ "-101" > "-100" """)
    expectTrue( """ "-1000" > "-100" """)
    // but not, if either operand is given as number
    // Since anything can be converted to string, we better support this:
    expectTrue( """ "100" == 100 """)
    expectTrue( """ "100" < 101 """)
    expectTrue( """ "101" > 100 """)
    expectTrue( """ "1000" > 100 """)

    expectTrue( """ "-100" == -100 """)
    expectFalse( """ "-100" < -101 """)
    expectFalse( """ "-101" > -100 """)
    expectFalse( """ "-1000" > -100 """)

    expectTrue( """ -100 == "-100" """)
    expectFalse( """ -100 < "-101" """)
    expectFalse( """ -101 > "-100" """)
    expectFalse( """ -1000 > "-100" """)

    expectTrue( """ "100L" == 100 """)
    expectTrue( """ 100L == "100" """)
    expectFalse( """ -100 < "-101L" """)
    expectFalse( """ "-101L" > -100 """)

  }

  @Test
  def testSubscript() {
    println("String subscripting")

    expect( """ "x"@0 """, "x")
    expect( """ "xy"@1 """, "y")
  }

  @Test
  def testSlice() {
    println("String slicing")

    expect( """ "abcdef"@@(0, -1) """, "abcdef")
    expect( """ "abcdef"@@(1, 3) """, "bc")
    expect( """ "abcdef"@@(-2, 1) """, "edcb")
  }

  @Test
  def testConcat() {
    println("String concatenation")

    expect( """ "abc" + "def" """, "abcdef")
    expect( """ "abc" ++ "def" """, "abcdef")
    // should produce ("abc","def")
    expectTrue( """ vx = "abc" +++ "def" """)
    expect( """ vx@0 """, "abc")
    expect( """ vx@1 """, "def")

    // should produce ("abcdef")
    expectTrue( """ vx = "abc" ++ "def" ++ ()""")
    expect( """ vx@0 """, "abcdef")

    expect( """ "abc" * 3 """, "abcabcabc")

  }

  @Test
  def testOperations() {
    println("String operations")

    expect( """ "abc" * 3 """, "abcabcabc")

    shouldFail("num * string not allowed") {
      expect( """ 3 * "abc" """, "abcabcabc")
    }

    expect( """ "abc " + 3 """, "abc 3")
    shouldFail("num * string not allowed") {
      expect( """ 3 + "abc" """, "abcabcabc")
    }
  }

}