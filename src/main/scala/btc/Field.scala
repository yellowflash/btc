package btc

trait Field[E] extends Group[E]:
    def one: E
    def prod(a: E, b: E): E
    def inv(a: E): E

object Field:
    extension [E](a: E)(using f: Field[E])
        inline def *(b: E) = f.prod(a, b)
        inline def /(b: E) = f.prod(a, f.inv(b))

    case class Element(value: BigInt) extends AnyVal

    def modP(prime: BigInt): Field[Element] = new Field[Element]:
        def norm(value: BigInt): Element = 
            if value > 0 then Element(value % prime) else Element((prime + value) % prime)
        def zero = Element(BigInt(0))
        def one = Element(BigInt(1))
        def add(a: Element, b: Element) =
            norm(a.value + b.value)
        def prod(a: Element, b: Element) = 
            norm(a.value * b.value)
        def neg(a: Element) =
            norm(-a.value)
        def inv(a: Element) =
            norm(a.value.modPow(prime - 2, prime))

    extension (a: Element)(using field: Field[Element])
        inline def *[G](b: G)(using group: Group[G]) = 
            import btc.Group._

            var result = group.zero
            var x = b
            
            var p = a.value
            while p > 0 do
                if p % 2 == 1 then 
                    result = result + x
                x = x + x
                p = p >> 1

            result
            
            
