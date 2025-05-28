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
            val paddedBytes = Array.fill(length)(zero)
            
            // Copy the least significant bytes into the padded array
            val copyStart = Math.max(0, bytes.length - length)
            val copyLength = Math.min(bytes.length, length)
            System.arraycopy(bytes, copyStart, paddedBytes, length - copyLength, copyLength)
            
            buffer.order(ByteOrder.BIG_ENDIAN).put(paddedBytes)

        def read(buffer: ByteBuffer): Either[String, BigInt] = 
            val bytes: Array[Byte] = Array.fill(length)(0x00)
            if buffer.remaining() < length then 
                Left(s"Insufficient bytes ${buffer.remaining()} < 256 found")
            else
                buffer.order(ByteOrder.BIG_ENDIAN).get(bytes)
                Right(BigInt(1, bytes))
