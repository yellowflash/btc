package btc

import Field.Element
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class FieldSpec extends AnyFlatSpec:
    val `zero` = Element(BigInt(0))
    val `one` = Element(BigInt(1))
    val `three` = Element(BigInt(3))
    val `four` = Element(BigInt(4))
    val `five` = Element(BigInt(5))
    val `seven` = Element(BigInt(7))
    val `eight` = Element(BigInt(8))
    val `ten` = Element(BigInt(10))
    implicit val `mod 11`: Field.Element.ModP = 
        new Field.Element.ModP(BigInt(11));

    import Group._

    "Mod P" should "add numbers modulo prime" in {
        (four + four) shouldBe eight
        (four + eight) shouldBe one
    }

    it should "subtract numbers modulo prime" in {
        (zero - four) shouldBe seven
        (four - four) shouldBe zero
        (eight - four) shouldBe four
        (one - four) shouldBe eight
    }

    it should "multiply numbers modulo prime" in {
        (one * four) shouldBe four
        (zero * four) shouldBe zero
        (four * four) shouldBe five
        (eight * four) shouldBe ten
        (four * eight) shouldBe ten
    }

    it should "divide numbers modulo prime" in {
        (one / four) shouldBe three
        (four * (one / four)) shouldBe one
        (four / four) shouldBe one
        (eight / eight) shouldBe one
        (zero / four) shouldBe zero
    }

    it should "find n times group" in {
        import Group.bigIntGroup
        import Field.Element.*

        four * BigInt(10)  shouldBe (BigInt(40))
        (- four) * BigInt(10) shouldBe (BigInt(70))
    }