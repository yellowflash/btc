package  btc

import java.security.MessageDigest

object BitCoin:
    val n = BigInt("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)

    val Gx = Field.Element("79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798")
    val Gy = Field.Element("483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8")

    val G = Curve.Point.InCurve(Gx, Gy)

    def sha256(message: Array[Byte]): Array[Byte] =
        val algo = MessageDigest.getInstance("SHA-256");
        algo.digest(message)

    def hash256(message: Array[Byte]) =
        sha256(sha256(message))

    import Field.Element._
    import Group._
    import Field._

    implicit val curveGroup: Group[Curve.Point[Field.Element]] =  {
      implicit val field: Field.Element.ModP = new Field.Element.ModP(
          BigInt("fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f", 16)
      )
      implicit val curve: Curve[Field.Element] = Curve(
          Field.Element(BigInt(0)),
          Field.Element(BigInt(7))
      )
      Curve.Point.group
    }

    implicit val curveFactor: Field.Element.ModP = new Field.Element.ModP(n)

    def hashedElement(message: String): Field.Element =
        hashedElement(message.getBytes)

    def hashedElement(bytes: Array[Byte]): Field.Element =
        Field.Element(BigInt(1, hash256(bytes)))

    def PublicKey(e: Field.Element) = e * G

    case class Signature(
        z: Field.Element,
        r: Field.Element,
        s: Field.Element
    )

    class Verifier(P: Curve.Point[Field.Element]):
        def verify(signature: Signature) =
            val u = signature.z/signature.s
            val v = signature.r/signature.s
            (u * G + v * P) match
                case Curve.Point.Infinity => false
                case Curve.Point.InCurve(x, _) => x == signature.r

    class Signer(e: Field.Element):
        def sign(message: Array[Byte], k: Field.Element): Option[Signature] =
            val z = hashedElement(message)
            (k * G) match
                case Curve.Point.Infinity => None
                case Curve.Point.InCurve(r, _) =>
                    val s = (z + (r * e)) / k
                    Some(Signature(z, r, s))

                

            
            
            





