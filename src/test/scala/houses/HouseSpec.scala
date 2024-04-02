package houses

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class HouseSpec extends AnyFlatSpec with Matchers {
  "House smart-constructor" should "throw if negative value was given" in {
    val houses = Table(
      "house",
      House(Economy, -1, 1, 1, 1),
      House(Premium, 1, -1, 1, 1),
      House(Economy, 1, 1, -1, 1),
      House(Premium, -1, 1, 1, -1)
    )

    forAll(houses) { case Left(NegativeValuesError) => }
  }

  it should "create valid houses" in {
    val houseArgs = Table(
      ("houseClass", "floors", "length", "width", "height"),
      (Economy, 1, 1, 1, 1),
      (Premium, 1, 1, 1, 1),
      (Premium, 1, 100, 200, 1)
    )

    forAll(houseArgs) { (houseClass, floors, length, width, height) =>
      {
        val house = House(houseClass, floors, length, width, height)

        house match {
          case Right(House(`houseClass`, `floors`, `length`, `width`, `height`)) =>
        }
      }
    }
  }

  "calculateParquetPrice" should "calculate house price" in {
    val houseArgs = Table(
      ("house", "price"),
      (House(Economy, 2, 1, 2, 3), 2 * 10_000 + 1 + 2 + 3),
      (House(Premium, 2, 1, 1, 1), 3 * 3 * (1 + 1 + 1)),
      (House(Premium, 5, 100, 200, 1), 2 * 2 * 2 * 2 * 2 * (100 + 200 + 1))
    )

    forAll(houseArgs) { case (Right(house), price) =>
      house.calculateParquetPrice shouldBe price
    }
  }
}
