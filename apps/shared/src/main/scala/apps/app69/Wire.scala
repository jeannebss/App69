package apps

package app69

import cs214.webapp.*
import cs214.webapp.DecodingException
import ujson.*

import scala.util.{Failure, Success, Try}
import javax.swing.plaf.metal.MetalIconFactory.FolderIcon16

/** 
  * Wire conects the server and the client by encoding and decoding
  * all the types of the game.
  */
object Wire extends AppWire[Event, View]:
    import Event.*
    import Choice.* 
    import Phase.* 
    import PhaseView.*
    import PlayersView.*
    import Hands.*

    /**
      * WireFormat for encoding and decoding the Choice type.
      */
    object ChoiceWire extends WireFormat[Choice]:
         
        /**
          * Encodes the Choice type to a JSON value.
          * 
          * @param choice the Choice to be encoded.
          * @return the JSON object representing the Choice.
          */
        def encode(choice: Choice): Value =
            choice match
                case Check => Obj("tag" -> "Check")
                case Call => Obj("tag" -> "Call")
                case Fold => Obj("tag" -> "Fold")
                case Raise(value: Bet) => 
                    Obj(
                        "tag" -> "Raise",
                        "amount" -> IntWire.encode(value)
                    )
        
         /**
          * Tries to decode the JSON value to a Choice type.
          * 
          * @param json the JSON object to be decoded.
          * @return the Choice type.
          * @throws DecodingException if the JSON object is not a valid Choice.
          */
        def decode(json: Value): Try[Choice] = Try:
            json("tag").str match
                case "Check" => Check
                case "Call" => Call
                case "Fold" => Fold
                case "Raise" => Raise(IntWire.decode(json("amount")).get)
                case _ => throw new DecodingException("Not a valid choice!")

    /**
      * WireFormat for encoding and decoding the Event type.
      */
    override object eventFormat extends WireFormat[Event]:
        /**
          * Encodes the Event type to a JSON value.
          * 
          * @param event the Event to be encoded.
          * @return the JSON object representing the Event.
          */
        override def encode(event: Event): Value =
            event match
                case PlayerAction(choice) =>
                    Obj(
                        "tag" -> "PlayerAction", 
                        "value" -> ChoiceWire.encode(choice)
                    )
                case EndGameChoice(choice) => 
                    Obj(
                        "tag" -> "EndGameChoice",
                        "value" -> BooleanWire.encode(choice)
                    )
                case Ready => 
                    Obj("tag" -> "Ready")

        /**
          *  Tries to decode the JSON value to an Event type.
          * 
          * @param json the JSON object to be decoded.
          * @return the Event type.
          * @throws DecodingException if the JSON object is not a valid Event.
          */
        override def decode(json: Value): Try[Event] = Try:
            json("tag").str match
                case "PlayerAction" => PlayerAction(ChoiceWire.decode(json("value")).get)
                case "EndGameChoice" => EndGameChoice(BooleanWire.decode(json("value")).get)
                case "Ready" => Ready
                case _ => throw new DecodingException("Not a valid event!")

    /**
      * WireFormat for encoding and decoding the PhaseWire type.
      */
    object PhaseWire extends WireFormat[Phase]:
        /**
          * Encodes the Phase type to a JSON value.
          * 
          * @param phase the Phase to be encoded.
          * @return the JSON object representing the Phase.
          */
        def encode(phase: Phase): Value =
            phase match
                case InGame(turn) => 
                    Obj(
                        "phase" -> "InGame",
                        "turn" -> IntWire.encode(turn)
                    )
                case PlayerChoice(turn, choice) =>
                    Obj(
                        "phase" -> "PlayerChoice",
                        "turn" -> IntWire.encode(turn),
                        "choice" -> ChoiceWire.encode(choice)
                    )
                case CardReveal(winners) => 
                    Obj(
                        "phase" -> "CardReveal",
                        "Winners" -> VectorWire(StringWire).encode(winners)
                    )
                case Reveal  => Obj("phase" -> "Reveal")
                case EndGame => Obj("phase" -> "EndGame")

        /**
          * Tries to decode the JSON value to a Phase type.
          * 
          * @param json the JSON object to be decoded.
          * @return the Phase type.
          * @throws DecodingException if the JSON object is not a valid Phase.
          */
        def decode(json: Value): Try[Phase] = Try:
            json("phase").str match
                case "InGame" => InGame(IntWire.decode(json("turn")).get)
                case "PlayerChoice" => 
                    PlayerChoice(
                        IntWire.decode(json("turn")).get,
                        ChoiceWire.decode(json("choice")).get
                    )
                case "CardReveal" => 
                    val winners = VectorWire(StringWire).decode(json("Winners")).get
                    CardReveal(winners)
                case "Reveal" => Reveal
                case "EndGame" => EndGame
                case _ => throw new DecodingException("Not a valid phase!")

    /**
      * WireFormat for encoding and decoding the Card type.
      */
    object CardWire extends WireFormat[Card]:
        /**
          * Encodes the Card type to a JSON value.
          * 
          * @param card the Card to be encoded.
          * @return the JSON object representing the Card.
          */
        def encode(card: Card): Value =
            Obj(
                "value" -> IntWire.encode(card.value),
                "suit" -> StringWire.encode(card.suit)
            )
        
        /**
          * Tries to decode the JSON value to a Card type.
          * 
          * @param json the JSON object to be decoded.
          * @return the Card type.
          */
        def decode(json: Value): Try[Card] = Try:
            val obj = json.obj
            Card(IntWire.decode(obj("value")).get, StringWire.decode(obj("suit")).get)

    /**
      * WireFormat for encoding and decoding the Hands type.
      */
    object HandsWire extends WireFormat[Hands]:
        /**
          * Encodes the Hands type to a JSON value.
          * 
          * @param hands the Hands to be encoded.
          * @return the JSON object representing the Hands.
          */
        def encode(hands: Hands): Value =
            hands match 
                case EmptyHand => Obj("tag" -> "EmptyHand")
                case Hand(first, second) =>
                    Obj(
                        "tag" -> "Hand",
                        "first" -> CardWire.encode(first),
                        "second" -> CardWire.encode(second)
                    )
        
        /**
          * Tries to decode the JSON value to a Hands type.
          *
          * @param json the JSON object to be decoded.
          * @return the Hands type.
          */
        def decode(json: Value): Try[Hands] = Try:
            json("tag").str match
                case "EmptyHand" => EmptyHand
                case "Hand" =>
                    val first = CardWire.decode(json("first")).get
                    val second = CardWire.decode(json("second")).get
                    Hand(first, second)

    /**
      * WireFormat for encoding and decoding the PhaseView type.
      */
    object PhaseViewWire extends WireFormat[PhaseView]:
        /**
          * Encodes the PhaseView type to a JSON value.
          * 
          * @param phaseView the PhaseView to be encoded.
          * @return the JSON object representing the PhaseView.
          */
        def encode(phaseView: PhaseView): Value =
            phaseView match
                case ChoiceSelection => Obj("tag" -> "ChoiceSelection")
                case NotPlaying => Obj("tag" -> "NotPlaying")
                case ChoiceMade(choice, player) => 
                    Obj(
                        "tag" -> "ChoiceMade",
                        "Choice" -> ChoiceWire.encode(choice),
                        "Player" -> StringWire.encode(player)
                    )
                case Winners(winners) =>
                    Obj(
                        "tag" -> "Winners",
                        "Winners" -> VectorWire(StringWire).encode(winners)
                    )
                case IsReady(ready) => 
                    Obj(
                        "tag" -> "IsReady",
                        "Ready" -> BooleanWire.encode(ready)
                    )
                case End(winners, balance) =>
                    Obj(
                        "tag" -> "End",
                        "Winners" -> VectorWire(StringWire).encode(winners),
                        "Balance" -> IntWire.encode(balance)
                    )

        /**
          * Tries to decode the JSON value to a PhaseView type.
          * 
          * @param json the JSON object to be decoded.
          * @return the PhaseView type.
          * @throws DecodingException if the JSON object is not a valid PhaseView.
          */
        def decode(json: Value): Try[PhaseView] = Try:
            json("tag").str match
                case "ChoiceSelection" => ChoiceSelection
                case "NotPlaying" => NotPlaying
                case "ChoiceMade" =>
                    val choice = ChoiceWire.decode(json("Choice")).get
                    val player = StringWire.decode(json("Player")).get
                    ChoiceMade(choice, player)
                case "Winners" =>
                    val winners = VectorWire(StringWire).decode(json("Winners")).get
                    Winners(winners)
                case "IsReady" => 
                    val ready = BooleanWire.decode(json("Ready")).get
                    IsReady(ready)
                case "End" =>
                    val winners = VectorWire(StringWire).decode(json("Winners")).get
                    val balance = IntWire.decode(json("Balance")).get
                    End(winners, balance)
                case _ =>
                    throw new DecodingException("Not a valid phase view")

    /**
      * WireFormat for encoding and decoding the TableView type.
      */
    object TableViewWire extends WireFormat[TableView]:
        /**
          * Encodes the TableView type to a JSON value.
          * 
          * @param tableView the TableView to be encoded.
          * @return the JSON object representing the TableView.
          */
        def encode(tableView: TableView): Value =
            Obj(
                "dealerCards" -> VectorWire(CardWire).encode(tableView.dealerCards),
                "poolBalance" -> IntWire.encode(tableView.poolBalance)
            )
        
         /**
          * Tries to decode the JSON value to a TableView type.
          * 
          * @param json the JSON object to be decoded.
          * @return the TableView type.
          */
        def decode(json: Value): Try[TableView] = Try:
            val dealerCards = VectorWire(CardWire).decode(json("dealerCards")).get
            val poolBalance = IntWire.decode(json("poolBalance")).get
            TableView(dealerCards, poolBalance)
    
    /**
      * WireFormat for encoding and decoding the PlayersView type.
      */
    object PlayersViewWire extends WireFormat[PlayersView]:
        /**
          * Encodes the PlayersView type to a JSON value.
          * 
          * @param playersView the PlayersView to be encoded.
          * @return the JSON object representing the PlayersView.
          */
        def encode(playersView: PlayersView): Value = playersView match
            case InGamePlayer(playerIndex, playerBalance, activePlayers, currentPlayer, turnBets, hand) =>
                Obj(
                    "tag" -> "InGamePlayer",
                    "PlayerIndex" -> MapWire(StringWire, IntWire).encode(playerIndex),
                    "PlayerBalance" -> MapWire(StringWire, IntWire).encode(playerBalance),
                    "ActivePlayers" -> MapWire(StringWire, BooleanWire).encode(activePlayers),
                    "CurrentPlayer" -> StringWire.encode(currentPlayer),
                    "TurnBets" -> MapWire(StringWire, IntWire).encode(turnBets),
                    "Hand" -> HandsWire.encode(hand)
                )

            case PlayerCardReveal(playerIndex, playerBalance, activePlayers, playerHands) =>
                Obj(
                    "tag" -> "PlayerCardReveal",
                    "PlayerIndex" -> MapWire(StringWire, IntWire).encode(playerIndex),
                    "PlayerBalance" -> MapWire(StringWire, IntWire).encode(playerBalance),
                    "ActivePlayers" -> MapWire(StringWire, BooleanWire).encode(activePlayers),
                    "PlayerHands" -> MapWire(StringWire, HandsWire).encode(playerHands)
                )
        
        /**
          * Tries to decode the JSON value to a PlayersView type.
          * 
          * @param json the JSON object to be decoded.
          * @return the PlayersView type.
          * @throws DecodingException if the JSON object is not a valid PlayersView.
          */
        def decode(json: Value): Try[PlayersView] = Try:
            json("tag").str match
                case "InGamePlayer" =>
                    val playerIndex = MapWire(StringWire, IntWire).decode(json("PlayerIndex")).get
                    val playerBalance = MapWire(StringWire, IntWire).decode(json("PlayerBalance")).get
                    val activePlayers = MapWire(StringWire, BooleanWire).decode(json("ActivePlayers")).get
                    val currentPlayer = StringWire.decode(json("CurrentPlayer")).get
                    val turnBets = MapWire(StringWire, IntWire).decode(json("TurnBets")).get
                    val hand = HandsWire.decode(json("Hand")).get
                    InGamePlayer(playerIndex, playerBalance, activePlayers, currentPlayer, turnBets, hand)

                case "PlayerCardReveal" =>
                    val playerIndex = MapWire(StringWire, IntWire).decode(json("PlayerIndex")).get
                    val playerBalance = MapWire(StringWire, IntWire).decode(json("PlayerBalance")).get
                    val activePlayers = MapWire(StringWire, BooleanWire).decode(json("ActivePlayers")).get
                    val playerHands = MapWire(StringWire, HandsWire).decode(json("PlayerHands")).get
                    PlayerCardReveal(playerIndex, playerBalance, activePlayers, playerHands)
                
                case _ => throw DecodingException("Not a valid players view!")
    
     /**
      * WireFormat for encoding and decoding the View type.
      */
    override object viewFormat extends WireFormat[View]:
        /**
          * Encodes the View type to a JSON value.
          * 
          * @param view the View to be encoded.
          * @return the JSON object representing the View.
          */
        override def encode(view: View): Value =
            Obj(
                "Phase" -> PhaseViewWire.encode(view.phaseView),
                "Players" -> PlayersViewWire.encode(view.playersView),
                "Table" -> TableViewWire.encode(view.tableView)
            )

        /**
          * Tries to decode the JSON value to a View type.
          * 
          * @param json the JSON
          * @return the View type.
          * @throws DecodingException if the JSON object is not a valid View.
          */
        override def decode(json: Value): Try[View] = Try:
            val phaseView = PhaseViewWire.decode(json("Phase")).get
            val playersView = PlayersViewWire.decode(json("Players")).get
            val tableView = TableViewWire.decode(json("Table")).get
            View(phaseView, playersView, tableView)
            