package btc

trait Group[E]:
    def zero: E
    def add(a: E, b: E): E
    def neg(a: E): E

object Group:
    extension [E](a: E)(using group: Group[E])
        inline def unary_- = group.neg(a)
        inline def -(b: E) = group.add(a, group.neg(b))
        inline def +(b: E) = group.add(a, b)

    given bigIntGroup: Group[BigInt] = new Group[BigInt]:
        def zero = BigInt(0)
        def add(a: BigInt, b: BigInt) = a + b
        def neg(a: BigInt) = - a