import org.scalatest._

class ExtendIntArraySpec extends FlatSpec with Matchers {

  "ExtendIntArray" should "Array(2,1,2,4,2,6,7,8,9,10,0) must return correct answers" in {
    val ex_arr = ExtendedIntArray.extendedIntArray(Array(2,1,2,4,2,6,7,8,9,10,0))
    ex_arr.maxEl should equal(10)
    ex_arr.minEl should equal(0)
    ex_arr.sumEl should equal(51)
    ex_arr.sizeAr should equal(11)
    ex_arr.mean should equal(4.636363636363637)
    ex_arr.median should equal (4.0)
    ex_arr.stdDev should equal(3.3377381364883307)
    ex_arr.moreFreq._1 should equal (2)
    ex_arr.moreFreq._2 should equal (3)
    ex_arr.lessFreq._1 should equal (0)
    ex_arr.lessFreq._2 should equal (1)
  }

}
