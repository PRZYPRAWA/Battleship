import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import board._

class BoardSpec extends AnyWordSpec with Matchers {

  "A Board" should {

    "return true if added a ship" in {
      val board = new Board
      val shipToAdd = new Carrier
      val firstField = Field(1, 1)
      val secondField = Field(5, 1)
      val didAdd = board.addShip(firstField, secondField, shipToAdd)
      didAdd shouldBe true
    }

    "return false if tried to add a ship adjacent to another ship" in {
      val board = new Board

      val shipToAdd = new Carrier
      val firstField = Field(1, 1)
      val secondField = Field(5, 1)
      val didAddValidShip = board.addShip(firstField, secondField, shipToAdd)

      val invalidShip = new Battleship
      val invalidFirstField = Field(2, 1)
      val invalidSecondField = Field(5, 1)
      val didNotAddInvalidShip = board.addShip(invalidFirstField, invalidSecondField, invalidShip)

      didAddValidShip shouldBe true
      didNotAddInvalidShip shouldBe false
    }

    "return false if tried to add a ship at invalid field" in {
      val board = new Board

      val shipToAdd = new Carrier
      val firstField = Field(1, 0)
      val secondField = Field(5, 0)
      val didAdd = board.addShip(firstField, secondField, shipToAdd)
      didAdd shouldBe false
    }

    "return Hit if shoot at occupied field" in {
      val board = new Board
      val fieldToShoot = Field(1, 1)

      val shipToAdd = new Carrier
      val firstField = Field(1, 1)
      val secondField = Field(5, 1)
      val didAdd = board.addShip(firstField, secondField, shipToAdd)

      didAdd shouldBe true

      val reply = board.shoot(fieldToShoot)
      reply shouldBe Hit
    }

    "return Mishit if shoot at unoccupied field" in {
      val board = new Board
      val fieldToShoot = Field(1, 1)
      val reply = board.shoot(fieldToShoot)
      reply shouldBe Mishit
    }
  }
}