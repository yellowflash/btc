package btc

import java.nio.ByteOrder
import java.nio.ByteBuffer
import javax.lang.model.element.Element

case class Curve[E](a: E, b: E):
    def ySquared(x: E)(using field: Field[E]): E =
        import Group._
        import Field._
        x * x * x + a * x + b

object Curve:
    import btc.Field._
    import btc.Group._

    enum Point[+E]:
        case InCurve(x: E, y: E) extends Point[E]
        case Infinity

        def mirror[K >: E](using field: Field[K]) = this match
            case InCurve(x, y) => InCurve(x, field.neg(y))
            case Infinity => Infinity

    object Point:
        import Serde.{read, write}

        val InfinityType: Byte = 0x00
        val CompressedOddType: Byte = 0x02
        val CompressedEvenType: Byte = 0x03
        val UncompressedInCurveType: Byte = 0x04

        given deserialize(
            using field: Field.Element.ModP, 
                  curve: Curve[Field.Element]): Deserialize[Curve.Point[Field.Element]] = 
            
            given canSqrt: CanFastSqrt[Field.Element] = Field.Element.canFastSqrt
            given ellipticGroup: Group[Curve.Point[Field.Element]] = Curve.Point.group
            new Deserialize[Curve.Point[Field.Element]]:
                def read(buffer: ByteBuffer) = buffer.get() match
                    case InfinityType => Right(Point.Infinity)
                    case UncompressedInCurveType => 
                        import Serde.u256BigEndian
                        import Field.Element.deserialize
                        for {
                            x <- buffer.read[Field.Element]
                            y <- buffer.read[Field.Element]
                        } yield Point.InCurve(x, y)

                    case other@(CompressedOddType | CompressedEvenType) =>
                        import Serde.u256BigEndian 
                        import Group.unary_-
                        import Field.Element.deserialize
                        for {
                            x <- buffer.read[Field.Element]
                            ySquared = curve.ySquared(x)
                            y = canSqrt.sqrt(ySquared)
                        } yield Curve.Point.InCurve(
                                x, 
                                other match
                                    case CompressedOddType => 
                                        if (y.value % 2 == 0) then -y else y
                                    case CompressedEvenType =>
                                        if (y.value % 2 == 0) then y else -y
                            )

        given uncompressed: Serialize[Point[Field.Element]] = new Serialize[Point[Field.Element]]:
            def write(value: Point[Field.Element], buffer: ByteBuffer) = value match
                case Point.InCurve(x, y) =>
                    import Serde.u256BigEndian
                    buffer
                        .put(UncompressedInCurveType)
                        .write(x.value)
                        .write(y.value)
                case Point.Infinity =>
                    buffer.put(InfinityType)
        given compressed: Serialize[Point[Field.Element]] = new Serialize[Point[Field.Element]]:
             def write(value: Point[Field.Element], buffer: ByteBuffer) = value match
                case Point.InCurve(x, y) if y.value % 2 == 0 =>
                    import Serde.u256BigEndian
                    buffer
                        .put(CompressedEvenType)
                        .write(x.value)
                case Point.InCurve(x, y) =>
                    import Serde.u256BigEndian
                    buffer
                        .put(CompressedOddType)
                        .write(x.value)
                case Point.Infinity =>
                    buffer.put(InfinityType)

        given group[E](using field: Field[E], curve: Curve[E]): Group[Point[E]] = new Group:
            def three: E = (field.one + field.one + field.one)
            def two: E = field.one + field.one

            def zero = Point.Infinity
            def neg(a: Point[E]) = a.mirror
            def add(a: Point[E], b: Point[E]) = a match 
                case Point.Infinity => b
                case Point.InCurve(x1, y1) => b match 
                    case Point.Infinity =>  a
                    case Point.InCurve(x2, y2) if x1 == x2 && y1 == -y2 =>
                        Point.Infinity
                    case Point.InCurve(x2, y2) if x1 == x2 && y1 == y2 =>
                        val s = (three * x1 * x1 + curve.a)/(two * y1)
                        val x3 = s * s - two * x1
                        val y3 = s * (x1 - x3) - y1
                        Point.InCurve(x3, y3)
                    case Point.InCurve(x2, y2) =>
                        val m = (y1 - y2)/(x1 - x2)
                        val x3 = m * m - x1 - x2
                        val y3 = (m * (x1 - x3)) - y1
                        Point.InCurve(x3, y3)

        










