package btc

import java.nio.ByteBuffer
import scala.annotation.threadUnsafe

trait Field[E] extends Group[E]:
    def one: E
    def prod(a: E, b: E): E
    def inv(a: E): E

object Field:
    extension [E](a: E)(using f: Field[E])
        inline def *(b: E) = f.prod(a, b)
        inline def /(b: E) = f.prod(a, f.inv(b))

    case class Element(value: BigInt) extends AnyVal:
        override def toString: String = value.toString(16)

    trait CanFastSqrt[E]: 
        def sqrt(a: E): E
    object Element:
        def apply(value: Int): Field.Element = Field.Element(BigInt(value))
        def apply(value: String, radix: Int = 16): Field.Element = 
            Field.Element(BigInt(value, radix))
        given serialize(using serialize: Serialize[BigInt]): Serialize[Element] = new Serialize[Element]:
            def write(value: Element, buffer: ByteBuffer) = serialize.write(value.value, buffer)

        given deserialize(using deserialize: Deserialize[BigInt], modP: ModP): Deserialize[Element] = new Deserialize[Element]:
            def read(buffer: ByteBuffer) = for {
                    value <- deserialize.read(buffer)
                } yield modP.norm(value)

        class ModP(val prime: BigInt) extends Field[Element]:
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

        def canFastSqrt(using modP: ModP): CanFastSqrt[Element] = 
            assert((modP.prime + 1) % 4 == BigInt(0))
            val factor = (modP.prime + 1) / 4
            new CanFastSqrt[Element]:
                def sqrt(a: Element): Element = 
                    Element(a.value.modPow(factor, modP.prime))

        extension (a: Element)(using field: Field[Element])
            inline def *[G](b: G)(using group: Group[G]) = 
                import btc.Group._

                var result = group.zero
                var x = b
               
                var p = a.value
                while p > 0 do
                    if p.testBit(0) then 
                        result = result + x
                    x = x + x
                    p = p >> 1

                result
                
    given doubleIsField: Field[Double] = new Field[Double]:
        def zero = 0.0
        def one = 1.0
        def add(a: Double, b: Double) = a + b
        def prod(a: Double, b: Double) = a * b 
        def neg(a: Double) = -a
        def inv(a: Double) = 1 / a


            
