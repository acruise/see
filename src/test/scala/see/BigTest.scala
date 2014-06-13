/*
 */

package see

import org.junit._

/** Tests BigInteger/Decimal operations.
  */

//@Ignore
class BigTest extends TestCase {
  @Test
  def testParser() {
    println("Big Parsing")

    expect("0L", BI(0))
    expect("1L", BI(1))
    expect("12345678901234567890L", BI("12345678901234567890"))
    expect("-12345678901234567890L", BI("-12345678901234567890"))
    expect("0.0L", BR(0))
    expect("1.0L", BR(1))
    expect("0.0001L", BR(0.0001))
    expect("-0.0001L", BR(-0.0001))
    expect(".123L", BR(0.123))
    expect("-.123L", BR(-0.123))
    expect("0.1e5L", BR(1e4))
    expect("0.1e-5L", BR(1e-6))
    expect("-.123L", BR(-0.123))
    expect("-.1e+5L", BR(-1e4))
    expect("1123456.0987654321E-500L", BR("1123456.0987654321E-500"))
    expect("1e+500L", BR("1e+500"))
    shouldThrow("Number out of range") {
      parse("1.e+10000000000L")
    }
  }

  @Test
  def testConv() {
    println("Big Conversions")

    expect( """ len(12345L) """, 1)
    expect( """ str(1234567890L) """, "1234567890L")
    expect( """ 1234567890L """, BI("1234567890"))

    expect( """ 100L + 1 """, BI("101"))
    expect( """ 100.0L + 1 """, BR("101"))
    expect( """ 100.0L + 1 """, BR("101"))
    expect( """ 100.0L / 50L """, BI("2"))

    expect( """ bigint(5) """, BI("5"))
    expect( """ bigreal(5) """, BR("5"))

    expect( """ real(100e10L) """, 100e10)
    expect( """ real(100e500L) """, Double.PositiveInfinity)
    expectTrue( """ bigint(100e500L) == 100 * 10L**500""")

    expect( """ 5L * 1.5 """, BR("7.5"))
    expect( """ 1.5 * 5L """, BR("7.5"))
  }

  @Test
  def testPropagation() {
    println("Type Propagation on Overflow")

    // Real should be propagated to Big on overflow.
    expect( """ 1.5e308 + 1.5e308 """, BR("3e308"))
    expect( """ -1.5e308 - 1.5e308 """, BR("-3e308"))
    expectTrue( """ 1e200 * 1e200 == 1e400""")
    expectTrue( """ 1e200 / 1e-200 == 1e400 """)

    // but not Int!
    expect( """ 4 * 0xFFFF_FFFF_FFFF_FFFF """, 0xFFFFFFFFFFFFFFFCL)
  }


  @Test
  def testComparison() {
    println("Big comparison")

    expectTrue( """ 0L == 0 """)
    expectTrue( """ 1 == 1L """)
    expectTrue( """ 1.0 == 1L """)
    //expectFalse(""" 0.1 == 0.1L """)
    expectFalse( """ -2L > 0.1L """)

  }


  @Test
  def testExp() {
    println("Big exponents")

    expect( """ 10L**0 """, BI("1"))
    expect( """ 10L**20 """, BI("100000000000000000000"))
    expect( """ 10L** -2 """, BR("0.01"))
    // goes through BigReal, but should be accurate enough for this case
    expect( """ 4L**0.5 """, 2.0)

    // whole numbers should be reduced to BigInt
    expect( """ 10.L**0 """, BI("1"))
    expect( """ 10.L**20 """, BI("100000000000000000000"))
    expect( """ 10.L** -2 """, BR("0.01"))
    expect( """ 4.L**0.5 """, 2.0)

    expect( """ 0.1L**0 """, BI("1"))
    expect( """ 0.1L**2 """, BR("0.01"))
    expect( """ 0.1L** -2 """, BI("100"))
    expect( """ 0.04L**0.5 """, 0.2)
    // not good enough here:
    //expect(""" 0.04L**-0.5 """, BI("5"))
    //println(scope.eval(parse("0.04L**-0.5")))
    expectTrue( """ 4.99999 < 0.04L** -0.5 < 5.00001 ?! "Neg. Exp failed." """)

    expectTrue( """ 1e200L**100 == 1e20000L ?! "Big Exp failed." """)
    expectTrue( """ 1e400L**0.5 == 1e200 ?! "Big sqrt failed." """)

  }

  @Test
  def testOperations() {
    println("Big operations")

    // real function result will remain big:
    expect( """ sin(3.0L * PI) """, BR(math.sin(3 * math.Pi)))

    // except upon overflow (output here):
    shouldFail("math.asin cannot handle this") {
      expect( """ asin(5L) """, 0)

    }

    // Input overflow is easier.
    shouldFail("math.log cannot handle this") {
      expect( """ log(1e1000) """, 0)

    }


  }


}