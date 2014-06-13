/*
 *  
 */

package see

import org.junit._

import see.values.Rexp

/** Tests regular expressions.
  */

//@Ignore
class RegexTest extends TestCase {

  @Test
  def testParsing() {
    println("Regex Parsing")

    parse(" '' ")
    parse(" 'abc' ")
    parse( """ 'ab\d' """)
    parse( """ 'ab\'c' """)
  }

  @Test
  def testConv() {
    println("RegEx to String Conversions")
    expect( """ str('abc') """, "abc")
    expectTrue( """ 'abc' == "abc" """)
    expectTrue( """ 'ab.' == "ab." """)
    // Strange due to escaping needed here.
    // Actual input is much easier.
    expectTrue( """ 'ab\b' == "ab\\b" """)

    expect( """ x = '\d' + "a" """, new Rexp("\\da"))
    expectTrue( """ regex(x) ~~ "1a"  """)
    expect( """ x = '\s' + 1 """, new Rexp("\\s1"))
    expectTrue( """ regex(x) ~~ " 1"  """)
    // but not the other way round
    expectFail("Illegal") {
      expect( """ x = 1 + '\s'""", 0)
    }
    expect( """ x = '\d' * 2 """, new Rexp("\\d\\d"))
    expectTrue( """ regex(x) ~~ "15"  """)
    expectFail("Illegal") {
      expect( """ x = 2 * '\s'""", 0)
    }

  }

  @Test
  def testMatch() {
    println("RegEx Matching")
    expectTrue( """ num = '\d' """)
    expectTrue( """ ma = 'a' """)
    expectTrue( """ mnum = '\d+' """)
    expectTrue( """ mns = '\s*\d' """)

    expectFalse( """ num ~~ "" """)
    expectFalse( """ mns ~~ "" """)
    expectFalse( """ num ~~ "x" """)
    expectFalse( """ mns ~~ "x" """)

    expectTrue( """ num ~~ "1" """)
    expectTrue( """  "1" ~~ num """)
    expectTrue( """ "a" ~~ ma """)
    expectTrue( """ ma ~~ "a" """)
    expectTrue( """ mnum ~~ "1" """)
    expectTrue( """ mnum ~~ "1234567" """)
    expectFalse( """ num ~~ "1234567" """)
    expectFalse( """ mnum ~~ "1234567x" """)

    expectTrue( """ num ~~ 1 """)

    expectTrue( """ mns ~~ "1" """)
    expectTrue( """ mns ~~ " 2" """)
    expectTrue( """ mns ~~ "    9" """)
    expectFalse( """ mns ~~ "    9 " """)
    expectFalse( """ mns ~~ "    x" """)

    // Caution: ~~ and == are both relations,
    // so 'a' ~~ () == () produces ('a' ~~ ()) && (() == ()) ,
    // which evaluates to () && true -> false && true -> false
    expectTrue( """ (num ~~ ()) == ()""")
    expect( """ num  ~~ (1, "a", "5") """, BoolVector(true, false, true))

  }

  @Test
  def testExtract() {
    println("RegEx Extraction")
    expectTrue( """ num = '\d' """)
    expectTrue( """ ma = 'a' """)
    expectTrue( """ mnum = '\d+' """)
    expectTrue( """ mns = '\s*\d' """)

    expectFalse( """ num ~~ "" """)
    expectFalse( """ mns ~~ "" """)
    expectFalse( """ num ~~ "x" """)
    expectFalse( """ mns ~~ "x" """)
  }

}