package btc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class CurveSpec extends AnyFlatSpec:
    implicit val curve: Curve[Double] = Curve(5.0, 7.0)
    val inf: Curve.Point[Double] = Curve.Point.Infinity

    import Group._
    import Field.doubleIsField
    import Curve.Point.group


    "Curve" should "add points to inf, when x1 = x2 and y1 = -y2" in {
        val point1 = Curve.Point.InCurve(-1.0, -1.0)
        val point2 = Curve.Point.InCurve(-1.0, 1.0)


        (point1 + point2) shouldBe inf
        (point2 + point1) shouldBe inf
        (inf + point1) shouldBe (point1)
        (point1 + inf) shouldBe (point1)
    }

    it should "add points, when x1 != x2" in {
        val point1 = Curve.Point.InCurve(2.0, 5.0)
        val point2 = Curve.Point.InCurve(-1.0, -1.0)
        val point3 = Curve.Point.InCurve(3.0, -7.0)

        (point1 + point2) shouldBe point3
        (point2 + point1) shouldBe point3
        (point1.mirror + point2.mirror) shouldBe point3.mirror
        (point1 + point2 - point3) shouldBe inf
    }

    it should "add points when x1 = x2 and y1 = y2" in {
        val point = Curve.Point.InCurve(-1.0, -1.0)
        val result = Curve.Point.InCurve(18.0, 77.0)

        (point + point) shouldBe result
    }

    it should "do scalar multiply by field" in {
        given modP: Field.Element.ModP = new Field.Element.ModP(223)
        given curve: Curve[Field.Element] = Curve(
            Field.Element(0), 
            Field.Element(7)
        )
        val two = Field.Element(2)
        val four = Field.Element(4)
        val eight = Field.Element(8)
        val twentyOne = Field.Element(21)

        def point(x: Int, y: Int): Curve.Point[Field.Element] = 
            Curve.Point.InCurve(Field.Element(x), Field.Element(y))

        (two * point(192, 105)) shouldBe point(49, 71)
        (two * point(143,98)) shouldBe point(64, 168)
        (two * point(47,71)) shouldBe point(36, 111)
        (four * point(47,71)) shouldBe point(194, 51)
        (eight * point(47,71)) shouldBe point(116, 55)
        (twentyOne * point(47,71)) shouldBe inf
    }


