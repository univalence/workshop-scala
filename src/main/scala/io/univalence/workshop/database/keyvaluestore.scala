package io.univalence.workshop.database

import io.univalence.workshop.database.Stock.StockCodec
import java.nio.ByteBuffer
import java.nio.channels.SeekableByteChannel
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.concurrent.ConcurrentSkipListMap
import scala.util.Try

object keyvaluestore {

  trait KeyValueStore[K, V] {
    def update(key: K, value: V): Unit
    def find(key: K): V
    def delete(key: K): Unit
  }

  /*
  HashMap
    find: O(1) - O(N)
    update: O(1) - O(N)
    delete: O(1) - O(N)

  TreeMap
    find:   O(log(N))
    update: O(log(N))
    delete: O(log(N))

  SkipList
    find:   O(log(N))
    update: O(log(N))
    delete: O(log(N))
   */

  class MemoryKVStore[K, V](implicit ordering: Ordering[K])
      extends KeyValueStore[K, V] {

    val data: scala.collection.mutable.Map[K, V] =
      scala.collection.mutable.Map.empty

    override def update(key: K, value: V): Unit =
      data(key) = value

    override def find(key: K): V =
      data(key)

    override def delete(key: K): Unit =
      data -= key
  }

  class FileKVStore[K, V](
      dataFile: String,
      keyCodec: Codec[K],
      valueCodec: Codec[V]
  ) extends KeyValueStore[K, V] {
    override def update(key: K, value: V): Unit = ???

    override def find(key: K): V = ???

    override def delete(key: K): Unit = ???

    case class Record(key: K, timestamp: Instant, value: V)
    object Record {
      // NNNN||M|key|timestamp||value\n
      // NNNN: record size in bytes pad with 0 on left
      // M: key size in char
      def encode(record: Record): String = {
        val keyData = keyCodec.encode(record.key)
        val data: String =
          ("||" + keyData.length + "|" + keyData
            + "|" + record.timestamp.toEpochMilli
            + "|" + valueCodec.encode(record.value)
            + "\n")
        val n = data.getBytes.length

        f"$n%04d" + data
      }

      def decode(channel: SeekableByteChannel): Record = {
        val dataSizeBuffer = ByteBuffer.allocate(4)
        val bytes = channel.read(dataSizeBuffer)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    import StockKey.StockKeyCodec

    val keyCodec   = new StockKeyCodec
    val valueCodec = new StockCodec

    implicit val ordering: Ordering[StockKey] =
      (x: StockKey, y: StockKey) =>
        Ordering[String].compare(x.store + x.product, y.store + y.product)

    val kvstore: KeyValueStore[StockKey, Stock] =
      new MemoryKVStore[StockKey, Stock]

    val key = StockKey(store = "Plouzané", product = "kouign amann")
    val value = Stock(
      key = key,
      Instant.now(),
      200.0,
      Instant.now().plusSeconds(60 * 60 * 24 * 30)
    )
    kvstore.update(key, value)
    println(kvstore.find(key))
    kvstore.delete(key)
    println(Try { kvstore.find(key) })

    println(keyCodec.encode(key))
    println(valueCodec.encode(value))

    println(keyCodec.decode(keyCodec.encode(key), 0))
    println(valueCodec.decode(valueCodec.encode(value), 0))
  }

}

trait Codec[A] {
  def encode(a: A): String
  def decode(input: String, offset: Int): A
}

case class StockKey(store: String, product: String)
object StockKey {
  // StockKey(Plouzané,kouign amann) => Plouzané,kouign amann

  class StockKeyCodec extends Codec[StockKey] {
    override def encode(a: StockKey): String = {
      a.store + "," + a.product
    }

    //...Plouzané,kouign amann,lkjlkjlk
    //...Plouzané,kouign amann\n
    //...Plouzané,kouign amann
    override def decode(input: String, offset: Int): StockKey = {
      val xs = input.substring(offset).split(",")
      new StockKey(xs(0), xs(1))

    }
  }
}

case class Stock(
    key: StockKey,
    timestamp: Instant,
    quantity: Double,
    expiry: Instant
)
object Stock {
  class StockCodec extends Codec[Stock] {
    override def encode(a: Stock): String = {
      val kcodec = new StockKey.StockKeyCodec
      kcodec.encode(
        a.key
      ) + "," + a.timestamp.toEpochMilli + "," + a.quantity + "," + a.expiry.toEpochMilli
    }

    override def decode(input: String, offset: Int): Stock = {
      val kcodec   = new StockKey.StockKeyCodec
      val stockKey = kcodec.decode(input, offset)
      val xs       = input.substring(offset).trim.split(",")
      new Stock(
        stockKey,
        Instant.ofEpochMilli(xs(2).toLong),
        xs(3).toDouble,
        Instant.ofEpochMilli(xs(4).toLong)
      )
    }
  }
}
