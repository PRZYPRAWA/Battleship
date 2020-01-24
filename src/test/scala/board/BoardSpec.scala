import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import board._

class BoardSpec extends AnyWordSpec with Matchers {

  "A Board" should {

    "return a new Board with a Ship if added the ship" in {
      val board = Board()
      val shipToAdd = Carrier()
      val firstField = Field(1, 1)
      val secondField = Field(5, 1)
      val boardWithShip = Board(Map(
        Field(1, 1) -> shipToAdd,
        Field(2, 1) -> shipToAdd,
        Field(3, 1) -> shipToAdd,
        Field(4, 1) -> shipToAdd,
        Field(5, 1) -> shipToAdd
      ))

      val newBoard = board.addShip(firstField, secondField, shipToAdd)
      newBoard.board shouldBe boardWithShip.board

      val shipToAdd2 = Battleship()
      val firstField2 = Field(2, 3)
      val secondField2 = Field(5, 3)
      val boardWithShip2 = Board(Map(
        Field(1, 1) -> shipToAdd,
        Field(2, 1) -> shipToAdd,
        Field(3, 1) -> shipToAdd,
        Field(4, 1) -> shipToAdd,
        Field(5, 1) -> shipToAdd,
        Field(2, 3) -> shipToAdd2,
        Field(3, 3) -> shipToAdd2,
        Field(4, 3) -> shipToAdd2,
        Field(5, 3) -> shipToAdd2
      ))

      val newBoard2 = newBoard.addShip(firstField2, secondField2, shipToAdd2)
      newBoard2.board should not be newBoard.board
      newBoard2.board shouldEqual boardWithShip2.board

    }

    "return the same Board if tried to add a ship adjacent to another ship" in {
      val board = Board()

      val shipToAdd = Carrier()
      val firstField = Field(1, 1)
      val secondField = Field(5, 1)
      val boardWithShip = board.addShip(firstField, secondField, shipToAdd)

      val invalidShip = Battleship()
      val invalidFirstField1 = Field(2, 1)
      val invalidSecondField1 = Field(5, 1)
      val boardAfterFirstAttempt = boardWithShip.addShip(invalidFirstField1, invalidSecondField1, invalidShip)

      boardWithShip.board shouldEqual boardAfterFirstAttempt.board

      val invalidFirstField2 = Field(2, 2)
      val invalidSecondField2 = Field(5, 2)
      val boardAfterSecondAttempt = boardWithShip.addShip(invalidFirstField2, invalidSecondField2, invalidShip)

      boardWithShip.board shouldEqual boardAfterSecondAttempt.board
    }

    "return the same Board if tried to add a ship at an invalid field" in {
      val board = Board()

      val shipToAdd = Carrier()
      val firstField = Field(1, 0)
      val secondField = Field(5, 0)
      val boardAfterAttempt = board.addShip(firstField, secondField, shipToAdd)
      boardAfterAttempt.board shouldBe board.board
    }

    "return Hit if shoot at occupied field" in {
      val board = new Board
      val fieldToShoot = Field(1, 1)

      val shipToAdd = Carrier()
      val firstField = Field(1, 1)
      val secondField = Field(5, 1)
      val boardWithShip = board.addShip(firstField, secondField, shipToAdd)

      boardWithShip.board should not be board.board

      val (boardAfterShoot, reply) = boardWithShip.shoot(fieldToShoot)
      reply shouldBe Hit
      boardAfterShoot.board should not be boardWithShip.board
      boardAfterShoot.board shouldBe Map(
        Field(1, 1) -> Shot
        , Field(2, 1) -> shipToAdd
        , Field(3, 1) -> shipToAdd
        , Field(4, 1) -> shipToAdd
        , Field(5, 1) -> shipToAdd
      )
    }

    "return Mishit if shoot at unoccupied field" in {
      val board = new Board
      val fieldToShoot = Field(1, 1)
      val (boardAfterShot, reply) = board.shoot(fieldToShoot)

      reply shouldBe Mishit
      boardAfterShot.board shouldBe Map(
        fieldToShoot -> Miss
      )
    }
  }
}