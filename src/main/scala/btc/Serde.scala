package btc

import java.nio.ByteBuffer
import java.nio.ByteOrder

trait Serialize[T]:
    def write(value: T, buffer: ByteBuffer): ByteBuffer

trait Deserialize[T]:
    def read(buffer: ByteBuffer): Either[String, T]

trait Serde[T] extends Serialize[T] with Deserialize[T]

object Serde:
    extension (buffer: ByteBuffer)
        def write[T](value: T)(using serde: Serialize[T]): ByteBuffer = serde.write(value, buffer)
        def read[T](using serde: Deserialize[T]) = serde.read(buffer)

    given u256BigEndian: Serde[BigInt] = new Serde[BigInt]:
        val length = 256/8;
        val zero: Byte = 0x00
        def write(value: BigInt, buffer: ByteBuffer): ByteBuffer = 
            val bytes = value.toByteArray
            
            bytes
                .length
                .to(length)
                .foreach(_ => buffer.put(zero))
            
            val lastIndex = if bytes.length > length then 1 else 0

            bytes
                .length
                .to(lastIndex)
                .foreach(i => buffer.put(bytes(i)))

            buffer

        def read(buffer: ByteBuffer): Either[String, BigInt] = 
            val bytes: Array[Byte] = Array.fill(length + 1)(0x00)
            if buffer.remaining() < 256 
            then {
                Left(s"Insufficient bytes ${buffer.remaining()} < 256 found")
            } else {
                buffer.order(ByteOrder.BIG_ENDIAN).get(bytes, 1, length)
                Right(BigInt(bytes))
            }
        