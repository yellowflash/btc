package  btc

import java.security.MessageDigest

object BitCoin:
    implicit val field: Field[Field.Element] = Field.modP(
        BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141", 16)
    )
    implicit val curve: Curve[Field.Element] = Curve(
        Field.Element(BigInt(0)),
        Field.Element(BigInt(7))
    )

    val Gx = Field.Element(
        BigInt("79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798", 16)
    )

    val Gy = Field.Element(
        BigInt("483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8", 16)
    )

    val G = Curve.Point.InCurve(Gx, Gy)

    def sha256(message: Array[Byte]): Array[Byte] =
        val algo = MessageDigest.getInstance("SHA-256");
        algo.digest(message)

    def hash256(message: Array[Byte]) =
        sha256(sha256(message))


    case class Signature(
        z: Field.Element,
        r: Field.Element,
        s: Field.Element
    )

    class Verifier(P: Curve.Point[Field.Element]):
        import btc.Field._
        import btc.Group._
        def verify(signature: Signature) =
            val u = signature.z/signature.s
            val v = signature.r/signature.s
            (u * G + v * P) match
                case Curve.Point.Infinity => false
                case Curve.Point.InCurve(x, _) => x == signature.r

    class Signer(
        e: Field.Element, // Private Key
        P: Curve.Point[Field.Element]): // Public Key)
        import btc.Field._
        import btc.Group._

        assert(e * G == P)

        def toBigInt(bytes: Array[Byte]) =
            bytes.foldLeft(BigInt(0))(_ << 8 + _)

        def sign(message: Array[Byte], k: Field.Element): Option[Signature] =
            val z = Field.Element(toBigInt(hash256(message)))
            (k * G) match
                case Curve.Point.Infinity => None
                case Curve.Point.InCurve(r, _) =>
                    val s = (z + r * e) / k
                    Some(Signature(z, r, s))

                

            
            
            





