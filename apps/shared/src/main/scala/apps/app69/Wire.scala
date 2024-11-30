package apps

import cs214.webapp.*
import cs214.webapp.DecodingException
import ujson.*

import scala.util.{Failure, Success, Try}
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable
import javax.swing.plaf.metal.MetalIconFactory.FolderIcon16

trait Encoder[T]:
    def encode(t: T): Value

trait Decoder[T]:
    def decode(json: Value): Try[T]

trait WireFormat[T] extends Encoder[T] with Decoder[T]

def encodeWire[T](t: T)(using wireFormat: WireFormat[T]): Value =
    wireFormat.encode(t)

def decodeWire[T](json: Value)(using wireFormat: WireFormat[T]): Try[T] =
    wireFormat.decode(json)

object Wire extends AppWire[Event, View]:
    import Event.*

    given WireFormat[Event] with
        def encode(event: Event): Value =
            event match
                case PlayerAction(action) =>
                    Obj("tag" -> "PlayerAction", "value" -> encodeWire(action))
                case EndGameChoice(choice) => 
                    Obj("tag" -> "EndGameChoice", "value" -> Bool(choice))
            
        def decode(json: Value): Try[Event] = Try:
            json("tag").str match
                case "PlayerAction" => PlayerAction(decodeWire(json("value")).get)
                case "EndGameChoice" => EndGameChoice(json("value").bool)

    import Choice.* 
    given WireFormat[Choice] with
        def encode(choice: Choice): Value =
            choice match
                case Check => Obj("tag" -> "Check")
                case Call => Obj("tag" -> "Call")
                case Fold => Obj("tag" -> "Fold")
                case Raise(value: Bet) => Obj("tag" -> "Raise", "amount" -> Num(value))
        def decode(json: Value): Try[Choice] = Try:
            json("tag").str match
                case "Check" => Check
                case "Call" => Call
                case "Fold" => Fold
                case "Raise" => Raise(json("amount").num.toInt)

    val eventFormat: WireFormat[Event] = ???
    val phaseFormat: WireFormat[Phase] = ???
