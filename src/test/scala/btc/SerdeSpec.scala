package btc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class SerdeSpec extends AnyFlatSpec:
  "Serde" should "write bigint in bigendian 256 bit unsigned int" in {
    import Serde.{read, write, u256BigEndian}
    val buffer = java.nio.ByteBuffer.allocate(32)
    val value = BigInt(0x10)
    val expectedBytes = Vector.fill(31)(0x00.toByte) :+ 0x10.toByte

    val actual = buffer.write(value).flip().array().toVector

    buffer.read[BigInt].merge shouldBe value
    actual shouldEqual expectedBytes
  }
