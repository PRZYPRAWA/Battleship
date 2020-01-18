import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import board.{Board, Field}

class FieldSpec extends AnyWordSpec with Matchers {

  "A Field" should {

    "return valid string in toString method" in {
      val field = Field(1, 1)
      val fieldString = field.toString
      fieldString shouldBe "a1"
    }

    "return true if valid" in {
      val field1 = Field(1, 1)
      val isValid1 = field1.isValid
      isValid1 shouldBe true

      val field2 = Field(1, Board.boardSize)
      val isValid2 = field2.isValid
      isValid2 shouldBe true

      val field3 = Field(Board.boardSize, 1)
      val isValid3 = field3.isValid
      isValid3 shouldBe true

      val field4 = Field(Board.boardSize, Board.boardSize)
      val isValid4 = field4.isValid
      isValid4 shouldBe true
    }

    "return false if invalid" in {
      val field1 = Field(0, 5)
      val isValid1 = field1.isValid
      isValid1 shouldBe false

      val field2 = Field(5, 0)
      val isValid2 = field2.isValid
      isValid2 shouldBe false

      val field3 = Field(11, 0)
      val isValid3 = field3.isValid
      isValid3 shouldBe false
    }
  }
}
