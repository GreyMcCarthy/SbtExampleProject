import org.scalatest.funsuite.AnyFunSuite

class Base2ConversionTest extends AnyFunSuite {
  test("") {
    assert(Base2Conversion.numDigits(600, 10) === 3)
  }

  test("2") {
    val listNumber= 1.to((5000))
    val listNumberTuple=listNumber.map { n =>
      val numDigits = n match {
        case n if n < 10 => 1
        case n if 10 <= n && n < 100 => 2
        case n if 100 <= n && n < 1000 => 3
        case n if 1000 <= n => 4
      }

      (n, 10, numDigits)
    }

    listNumberTuple.foreach { case (x, base, numDigits) =>
      assert(Base2Conversion.numDigits2(x, base) === numDigits, x)
    }
  }
  test("3"){
  assert(Base2Conversion.baseConversion(2,2) === "10")
  assert(Base2Conversion.baseConversion(50,4) === "302")
    assert(Base2Conversion.baseConversion(500,15) === "235")
  }
}

