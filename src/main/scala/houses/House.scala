package houses

sealed trait HouseClass
object Economy extends HouseClass
object Premium extends HouseClass

// Осознанно использую не исключения, как написано в тз, а adt ошибки
// Т.к. на лекции и семинарах обсуждалось, что это более предпочтительный подход
sealed trait HouseCreationError
case object NegativeValuesError extends HouseCreationError

final case class House private (houseClass: HouseClass, floors: Int, length: Int, width: Int, height: Int) {
  def calculateParquetPrice(): Int = houseClass match {
    case Premium if floors < 5 => math.pow(3, floors).toInt * dimensionsSum
    case Premium               => math.pow(2, floors).toInt * dimensionsSum
    case Economy               => length * width * height + floors * 10_000
  }

  private def dimensionsSum = length + width + height
}

object House {
  def apply(
    houseClass: HouseClass,
    floors: Int,
    length: Int,
    width: Int,
    height: Int
  ): Either[HouseCreationError, House] =
    Either.cond(
      floors > 0 && length > 0 && width > 0 && height > 0,
      new House(houseClass, floors, length, width, height),
      NegativeValuesError
    )
}
