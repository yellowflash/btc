package btc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.Inside._

class BitCoinSpec extends AnyFlatSpec:
    val privateKey = BitCoin.hashedElement("my secret")
    val publicKey = BitCoin.PublicKey(privateKey)
    val verifier = new BitCoin.Verifier(publicKey)
    val signer = new BitCoin.Signer(privateKey)

    "BitCoin" should "compute correct signature" in {
        privateKey shouldBe Field.Element("8b387de39861728c92ec9f589c303b1038ff60eb3963b12cd212263a1d1e0f00")

        val expectedP = Curve.Point.InCurve(
            Field.Element(BigInt("028d003eab2e428d11983f3e97c3fa0addf3b42740df0d211795ffb3be2f6c52", 16)),
            Field.Element(BigInt("0ae987b9ec6ea159c78cb2a937ed89096fb218d9e7594f02b547526d8cd309e2", 16))
        )

        import BitCoin._
        import Group._
        import Field.Element.{given, _}
        import Curve.Point.group

        publicKey shouldBe expectedP
        val k = Field.Element(1234567890)
        val expectedZ = Field.Element("231c6f3d980a6b0fb7152f85cee7eb52bf92433d9919b9c5218cb08e79cce78")
        val expectedR = Field.Element("2b698a0f0a4041b77e63488ad48c23e8e8838dd1fb7520408b121697b782ef22")
        val expectedS = Field.Element("bb14e602ef9e3f872e25fad328466b34e6734b7a0fcd58b1eb635447ffae8cb9")


        inside(signer.sign("my message".getBytes, k)) {
            case Some(signature) =>
                signature.z shouldBe expectedZ
                signature.r shouldBe expectedR
                signature.s shouldBe expectedS

                verifier.verify(signature) shouldBe true
        }
       
    }

