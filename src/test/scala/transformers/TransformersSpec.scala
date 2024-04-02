package transformers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import transformers.Transformers.{duplicate, reverse, transform, truncateHalf}

class TransformersSpec extends AnyFlatSpec with Matchers {
  "duplicate" should "duplicate string" in {
    duplicate("abc") shouldBe "abcabc"
    duplicate("123") shouldBe "123123"
    duplicate("  ") shouldBe "    "
    duplicate("¡™£") shouldBe "¡™£¡™£"
  }

  "truncateHalf" should "truncate strings" in {
    truncateHalf("ab") shouldBe "a"
    truncateHalf("a") shouldBe ""
    truncateHalf("") shouldBe ""

    truncateHalf("abab") shouldBe "ab"
    truncateHalf("aba") shouldBe "a"
    truncateHalf("1234") shouldBe "12"
    truncateHalf("  ") shouldBe " "
    truncateHalf("¡™£¢") shouldBe "¡™"
  }

  "reverse" should "reverse strings" in {
    reverse("") shouldBe ""
    reverse("a") shouldBe "a"
    reverse("abc") shouldBe "cba"
    reverse("1234") shouldBe "4321"
    reverse("¡™£¢") shouldBe "¢£™¡"
  }

  "transform" should "apply transformations" in {
    val testData = Table(
      ("value", "duplicated", "truncated", "reversed"),
      ("abc", "abcabc", "a", "cba"),
      ("123321", "123321123321", "123", "123321"),
      ("", "", "", ""),
      ("¡™£", "¡™£¡™£", "¡", "£™¡")
    )

    forAll(testData) { (value, duplicated, truncated, reversed) =>
      {
        val transformValue = transform(value)

        transformValue(duplicate) shouldBe duplicated
        transformValue(truncateHalf) shouldBe truncated
        transformValue(reverse) shouldBe reversed
      }
    }
  }
}
