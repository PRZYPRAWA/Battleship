package player

import board.{Board, Carrier, Field}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AiSpec extends AnyWordSpec with Matchers {

  "AI" should {
    "add a ship at valid fields" in {
      val ai = AI(Board(), Board())
      val carrier = Carrier()
      val aiAfterAddingShip = ai.addShip(carrier)

      ai should not be aiAfterAddingShip

      val boardAfterAddingShip = aiAfterAddingShip.playerBoard.board

      for ((field, ship) <- boardAfterAddingShip) {
        assert(field.isValid)
        assert(ship == Carrier())
      }
    }

    "return a valid and different field to shoot if last shot was missed" in {
      val lastField = Field(1, 1)
      val didHit = false
      val ai = AI(Board(), Board()).setNextFieldAndDirection(lastField, didHit)

      assert(ai.nextFieldToShoot.isDefined)
      assert(ai.nextFieldToShoot.get.isValid)
      assert(ai.nextFieldToShoot.get != lastField)
    }

    "return a valid and relative field to shoot if last shot hit" in {
      val lastField = Field(5, 5)
      val didHit = true
      val ai = AI(Board(), Board()).setNextFieldAndDirection(lastField, didHit)

      val relativeFields = List(
        lastField.relative(0, 1),
        lastField.relative(1, 0),
        lastField.relative(0, -1),
        lastField.relative(-1, 0)
      )

      val nextFieldToShootOption = ai.nextFieldToShoot
      assert(nextFieldToShootOption.isDefined)
      assert(nextFieldToShootOption.get.isValid)
      assert(nextFieldToShootOption.get != lastField)
      assert(relativeFields.contains(nextFieldToShootOption.get))
    }
  }
}
