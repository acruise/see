/*
 */

package see

import org.junit._
import org.junit.Assert._
import nodes.Constant
import values._


//@Ignore
class BasicTest extends TestCase {


    @Before
    override def setUp() {
		super.setUp()
		// indirect logic consts:
		expectTrue("bt = true")
		expectFalse("bf = false")

	}

	@Test
	def testSyntax() {
		println("Syntax Errors")
		node = parse("") // parses, but would not evaluate

		shouldFail("Failed to catch synax error."){
			parse("2 - ")
		}
		parse("a; b")
		shouldFail("Failed to catch synax error."){
			node = parse("a b")
			println(node)
		}
		shouldFail("Failed to catch synax error."){
			parse("a , b")
		}
		shouldFail("Failed to catch synax error."){
			parse("a ,")
		}
		shouldFail("Failed to catch synax error."){
			parse(" ,")
		}
		shouldFail("Failed to catch synax error."){
			parse(" + ")
		}
		shouldFail("Failed to catch synax error."){
			parse("1 2")
		}
		shouldFail("Failed to catch synax error."){
			parse("1 (2,3)")
		}
		shouldFail("Failed to catch synax error."){
			parse("(1,2) 2")
		}
		shouldFail("Failed to catch synax error."){
			parse("x 2")
		}
		shouldFail("Failed to catch synax error."){
			parse("1 x")
		}
		// This is valid syntax! See VectorTest
		//	shouldFail("Failed to catch exec error."){
		//		result("(1,2) (3,4)")
		//	}
		shouldFail("Failed to catch synax error."){
			parse("(a,b) x")
		}
		shouldFail("Failed to catch synax error."){
			parse("(1,2) x;")
		}
		shouldFail("Failed to catch synax error."){
			parse(";(a,b) x")
		}


	}

	@Test
	def testParser() {

		println("Parser")

		assertEquals(Constant(Lint(2)), parse("2"))

		parse(" a + b ")
		parse(" {a + b} ")
		parse(" (a + b) ")
		parse(" a + b ; c")
		parse(" a + b ; ; c;")
		parse("; a + b ; c")
		parse(" {a + b; c} ")
		parse(" {a + b; c}; d ")
		// line breaks shouldn't matter
		parse("""1;
			  {a +
				b
				;
			   c};
			  d """)

	}

	@Test
	def testNumberParser() {
		println("Parsing numbers")

		expect(" 10 ", 10)
		expect(" 0.1 ", 0.1)
		expect(" E ", math.E)
		expect(" PI ", math.Pi)
		expect(" 0x10 ", 16)
		expect(" 0x10_10 ", 0x1010)
		shouldThrow("Invalid literal"){
			parse(" 0x_1010 ")
		}
		expect(" 0b10 ", 2)
		expect(" 0b10_00 ", 8)
		shouldThrow("Invalid literal"){
			parse(" 0x10.10 ")
		}
		//expect(" 1000h ", 0x1000) // no longer supported

		// border cases
		expect(" 0xffff_ffff_ffff_ffff ", -1)
		expectTrue(" 0x1_0000_0000_0000_0000 == 0x10000000000000000L")
		expect(" -9999999999999999999999999 ", BI("-9999999999999999999999999") )
		expect(" 1.7e308 ", 1.7e308)
		expect(" 1.8e308 ", BR("1.8e308"))
		expect(" -1.7e308 ", -1.7e308)
		expect(" -1.8e308 ", BR("-1.8e308"))
		// probably within epsilon anyway
		expect(" 1e-323 ", 1e-323)
		expect(" 1e-324 ", 0.0)

		// bool covered by logic test.
	}


	@Test
	def testTypes() {
		println("Types")

		expectTrue("1 istype Int")
		expect("str(type(1))", "Int" )
		expectTrue("1 istype Number")
		expectTrue("(x = 1) istype Integral")
		expectTrue("x istype Comparable")
		expectTrue("x istype Scalar")
		expectTrue("x istype Value")
		expectFalse("x istype String")
		expectFalse("x istype Closure")
		expectFalse("x istype BigInt")
		expectFalse("x istype Real")
		expectFalse("x istype BigReal")
		expectFalse("x istype Bool")
		expectFalse("x istype Anonymous")
		expectFalse("x istype Function")

		shouldFail ("Nothing but type after isType") {
			expectFalse("x istype 1 ")
		}

		expectTrue("type(x) == Int")
		expectFalse("type(x) == Value") // !! exact match required


		expectTrue("(x = 1 > 0) istype Bool")
		expectTrue("x istype Scalar")
		expectTrue("x istype Value")
		expectFalse("x istype String")
		expectFalse("x istype Comparable") // although == works, but not <,> etc

		expectTrue("1L istype BigInt")
		expectFalse("1L istype Int")
		expectTrue("1L istype Integral")
		expectFalse("1L istype Real")

		expectTrue("1.0L istype BigReal")
		expectFalse("1.0L istype Int")
		expectFalse("1.0L istype Integral")
		expectFalse("1.0L istype Real")

		expectTrue(""" "a" istype String""")
		expectFalse(""" "a" istype Regex""")
		expectTrue(""" "a" istype Comparable""")
		expectTrue(""" "a" istype Scalar""")
		expectTrue(""" "a" istype Value""")

		expectTrue(""" 'a' istype Regex""")
		expectFalse(""" 'a' istype String""")
		expectTrue(""" 'a' istype Comparable""")
		expectTrue(""" 'a' istype Scalar""")
		expectTrue(""" 'a' istype Value""")

		expect("f() := {a + b}; x = {5;  1 + 2}", 3) // note that { 1 + 2} would be simplified away
		expectTrue(""" f istype Closure""")
		expectTrue(""" f istype Function""")
		expectFalse(""" f istype Anonymous""")
		expectTrue(""" x istype Closure""")
		expectTrue(""" x istype Anonymous""")
		expectFalse(""" x istype Function""")

		expectFalse(""" f istype Value""")
		expectFalse(""" x istype Value""")

		// enough of that...
		// you probably won't need types anyway.

	}

	@Test
	def testComment() {
		println("Comments")
		expect(" 10 / 5  // * 100", 2)
		expect(" 10 / 5  //* 100", 2)
		expect(""" 2 +  // x // &x
			      2 """, 4)
		expect(""" 2 +  /* 2 + */ + 2 """, 4)
		// comment should work even inside operator
		expect(""" 2 +/*2 +*/+ 2 """, IntVector(2,2))
		expect(""" 2 /*2 +*/+ 2 // +2""", 4)
		shouldFail("Unclosed comment"){
			expect(""" 2 + /* // 2 + */+ 2
				   2""", 4)
		}
		shouldFail("Unclosed comment"){
			expect(""" 2 + /* // 2 + */+ 2
				 // */  +2""", 4)
		}
		expect(""" 2 /* // 2 + */+ 2
				  */ +2 """, 4)
		// More than one comment per line:
		expect(""" 2 /* a */ + 2 /* b*/ + 2 /**/ +2""", 8)

	}

	@Test
	def testAdd() {
		println("Addition");

		shouldFail("Should not accept boolean operand."){
			expectTrue(" 1 + bt ")
		}
		shouldFail("Should not accept boolean operand."){
			expectTrue(" bt + 1 ")
		}

		expect(" 1 + 1 ", 2)
		expect("1+1", 2)
		expect(" 1 + 2 + 3", 6)
		expect(" 1 + 2 + 3.0", 6.0)
		expect(" 1 + 0", 1)
		expect(" 5 + -1", 4)
		expect(" 1 + (2 + 3)", 6)
		expect(" 1 + 2; 10 + 11", 21)
		expect(" 10 + 0x10", 26)
	}

	@Test
	def testSubtract() {
		println("Subtraction");

		shouldFail("Should not accept boolean operand."){
			expectTrue(" bt - 1 ")
		}
		expect(" 1 - 1 ", 0)
		expect("1-1", 0)
		expect(" 4 + 2 - 3", 3)
		expect(" 4 - 2 + 3", 5)
		expect(" 4 - 2 - 3", -1)
		expect(" 2 - 3.0", -1.0)
		expect(" 1 - 0", 1)
		expect(" -5 - -1", -4)
		expect(" 1 + (2 - 3)", 0)
		expect(" 1 + 2; 10 - 11", -1)
	}

	@Test
	def testMult() {
		println("Multiplication");

		shouldFail("Should not accept boolean operand."){
			expectTrue(" bt * 1 ")
		}
		expect("1 * 1", 1)
		expect("5 * 10", 50)
		expect(" 4 + 2 * 3", 10)
		expect(" 4 * 2 + 3", 11)
		expect(" 4 * -2", -8)
		expect("10* -2", -20)
		expect("10* +3", 30)
		expect(" 2 * 3.0", 6.0)
		expect(" 5 * 0", 0)
		expect(" -5 * -2", 10)
		expect(" 3 * (5 - 1)", 12)
		expect(" 2 * 3 * 4", 24)
		expect(" 2.5 * 3.2", 8.0)
	}

	@Test
	def testDiv() {
		println("Division");

		shouldFail("Should not accept boolean operand."){
			expectTrue(" bt / 1 ")
		}
		expect("1 / 1", 1)
		expect("10 / 5", 2)
		expect(" 10 / 4", 2.5)
		expect(" 4 * 4 / 2", 8)
		expect(" 4 / 2 * 3", 6)
		expect("10/ -2", -5)
		expect("0/2", 0)

		shouldFail("Failed to catch zero division."){
			scope.eval(parse("2 / 0"))
		}
		shouldFail("Failed to catch zero division."){
			scope.eval(parse("2 / 0"))
		}

		expect(" -5 / -2", 2.5)
		expect(" 3 *(6 / 2)", 9)
		expect(" 20 / 5 / 4", 1)
		expect("10 % 3", 1)
	}

	@Test
	def testGcd() {
		println("Greatest common denominator");

		shouldFail("Should not accept boolean operand."){
			expectTrue(" bt gcd 5 ")
		}
		shouldFail("Should not accept real operands."){
			expectTrue(" 10.0 gcd 5.0 ")
		}
		expect("20 gcd 10", 10)
		expect("10 gcd 20", 10)
		expect("25 gcd 7", 1)
		expect("100 gcd 75", 25)
	}


	@Test
	def testExp() {
		println("Exponent");

		shouldFail("Should not accept boolean operand."){
			expectTrue(" bt ** 1 ")
		}
		expect("1 ** 1", 1)
		expect("2 ** 10", 1024)
		expect("2 ** 0", 1)
		expect(" 4 + 3 * 2**3", 28)
		expect(" 4 ** -2", 0) // because of int propagation
		expect(" 4.0 ** -2", 1.0 / 16)
		expect("4 ** (1/2)", 2.0)
		expect("10.0 ** +3", 1000.0)
		expect(" -5 ** 2", 25)
		expect(" 3 ** 3 ** 2", 729)
		expect(" 3 ** (3 ** 2)", 19683)
		expect(" 3 ** 1.5", 5.1961524227066318805823390245176)
	}

	@Test
	def testShift() {
		println("Shift");

		shouldFail("Should not accept boolean operand."){
			expectTrue(" bt >> 1 ")
		}
		shouldFail("Should not accept boolean operand."){
			expectTrue(" 1 >> bt ")
		}
		shouldFail("Should not accept real operand."){
			expectTrue(" 0.5 >> 1 ")
		}
		shouldFail("Should not accept real operand."){
			expectTrue(" 1 >> 0.5 ")
		}

		shouldFail("Should not accept boolean operand."){
			expectTrue(" bt << 1 ")
		}
		shouldFail("Should not accept boolean operand."){
			expectTrue(" 1 << bt ")
		}
		shouldFail("Should not accept real operand."){
			expectTrue(" 0.5 << 1 ")
		}
		shouldFail("Should not accept real operand."){
			expectTrue(" 1 << 0.5 ")
		}

		expect("1 >> 1", 0)
		expect("1 << 1", 2)
		expect("2 >> 1", 1)
		expect("2 << 1", 4)

		expect("1 >> -1", 2)
		expect("1 << -1", 0)

		expect("0xaa >> 1", 0x55)
		expect("0xaa << 1", 0x154)

		expect("-1 >> 63", -1) // int shift!
		expect("-1 << -63", 1) // unsigned shift!
		expect("-1 << 63", 0x8000000000000000L)

		expect("-1 >> 64", -1) // int shift!
		expect("-1 << 64", 0)
	}

	@Test
	def testBitAnd() {
		println("Bitwise And");

		shouldFail("Should not accept boolean operand."){
			expectTrue(" bt & 1 ")
		}
		shouldFail("Should not accept boolean operand."){
			expectTrue(" 1 & bt ")
		}

		shouldFail("Should not accept real operand."){
			expectTrue(" 0.5 & 1 ")
		}
		shouldFail("Should not accept real operand."){
			expectTrue(" 1 & 0.5 ")
		}

		expect("1 & 1", 1)
		expect("1 & 0", 0)
		expect("0 & 1", 0)
		expect("0 & 0", 0)
		expect("0xaa & 0x55", 0)
		expect("0xff & 0x55", 0x55)
		// text form:
		expect("0xff and 0x55", 0x55)
		scope.set("x", 0x0f)
		expect("x and 0x55", 0x05)

	}

	@Test
	def testBitOr() {
		println("Bitwise Or");

		shouldFail("Should not accept boolean operand."){
			expectTrue(" bt | 1 ")
		}
		shouldFail("Should not accept boolean operand."){
			expectTrue(" 1 | bt ")
		}

		shouldFail("Should not accept real operand."){
			expectTrue(" 0.5 | 1 ")
		}
		shouldFail("Should not accept real operand."){
			expectTrue(" 1 | 0.5 ")
		}

		expect("1 | 1", 1)
		expect("1 | 0", 1)
		expect("0 | 1", 1)
		expect("0 | 0", 0)
		expect("0xaa | 0x55", 0xff)
		expect("0 | 0x55", 0x55)

		// text form:
		expect("0 or 0x55", 0x55)
		scope.set("x", 0x0f)
		expect("x or 0x55", 0x5f)

		// precedence correct?
		expect ("0 & 1 | 2 & 2", 2)
	}

	@Test
	def testBitXOr() {
		println("Bitwise Xor");

		shouldFail("Should not accept boolean operand."){
			expectTrue(" bt ^ 1 ")
		}
		shouldFail("Should not accept boolean operand."){
			expectTrue(" 1 ^ bt ")
		}

		shouldFail("Should not accept real operand."){
			expectTrue(" 0.5 ^ 1 ")
		}
		shouldFail("Should not accept real operand."){
			expectTrue(" 1 ^ 0.5 ")
		}

		expect("1 ^ 1", 0)
		expect("1 ^ 0", 1)
		expect("0 ^ 1", 1)
		expect("0 ^ 0", 0)
		expect("0xaa ^ 0x55", 0xff)
		expect("0 ^ 0x55", 0x55)

		// text form:
		expect("0 xor 0x55", 0x55)
		scope.set("x", 0x0f)
		expect("x xor 0x55", 0x5a)
	}


	@Test
	def testBitNot() {
		println("Bitwise Not");

		shouldFail("Should not accept real operand."){
			expectTrue(" ~0.5 ")
		}
		// Although it looks unusual, bool is supported
		expectFalse("~true")
		expectTrue("~false")

		expect("~0", -1)
		expect("~-1", 0)
		expect("~ -1", 0)
		expect("~0xaa & 0xff", 0x55)

		// text form:
		expect("0xff and not 0x55 ", 0xaa)
	}

}