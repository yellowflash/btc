package btc

case class Curve[E](a: E, b: E)

object Curve:
    import btc.Field._
    import btc.Group._

    enum Point[+E]:
        case InCurve(x: E, y: E) extends Point[E]
        case Infinity

        def mirror[K >: E](using field: Field[K]) = this match
            case InCurve(x, y) => InCurve(x, field.neg(y))
            case Infinity => Infinity

    given pointIsGroup[E](using field: Field[E], curve: Curve[E]): Group[Point[E]] = new Group:
        def three: E = (field.one + field.one + field.one)
        def two: E = field.one + field.one

        def zero = Point.Infinity
        def neg(a: Point[E]) = a.mirror
        def add(a: Point[E], b: Point[E]) = a match 
            case Point.Infinity => b.mirror
            case Point.InCurve(x1, y1) => b match 
                case Point.Infinity =>  a.mirror
                case Point.InCurve(x2, y2) if x1 == x2 && y1 == -y2 =>
                    Point.Infinity
                case Point.InCurve(x2, y2) if x1 == x2 && y1 == 0 =>
                    val s = (three * x1 * x1 + curve.a)/(two * y1)
                    val x3 = s * s - two * x1
                    val y3 = s * (x1 - x3) - y1
                    Point.InCurve(x3, y3)
                case Point.InCurve(x2, y2) =>
                    val m = (y1 - y2)/(x1 - x2)
                    val x3 = m * m - x1 - x2
                    val y3 = (m * (x1 - x3)) - y1
                    Point.InCurve(x3, y3)

        










