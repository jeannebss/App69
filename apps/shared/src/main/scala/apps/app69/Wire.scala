package apps

package app69

import cs214.webapp.*
import cs214.webapp.DecodingException
import ujson.*

import scala.util.{Failure, Success, Try}
import javax.swing.plaf.metal.MetalIconFactory.FolderIcon16

object Wire extends AppWire[Event, View]:
    import Event.*
    import Choice.* 
    import Phase.* 
    import PhaseView.*
    import PlayersView.*
    import Hands.*

    object ChoiceWire extends WireFormat[Choice]:
        def encode(choice: Choice): Value =
            choice match
                case Check => Obj("tag" -> "Check")
                case Call => Obj("tag" -> "Call")
                case Fold => Obj("tag" -> "Fold")
                case Raise(value: Bet) => Obj("tag" -> "Raise", "amount" -> IntWire.encode(value))
        def decode(json: Value): Try[Choice] = Try:
            json("tag").str match
                case "Check" => Check
                case "Call" => Call
                case "Fold" => Fold
                case "Raise" => Raise(IntWire.decode(json("amount")).get)
                case _ => throw new DecodingException("Wrong choice!")

    override object eventFormat extends WireFormat[Event]:
        override def encode(event: Event): Value =
            event match
                case PlayerAction(choice) =>
                    Obj("tag" -> "PlayerAction", "value" -> ChoiceWire.encode(choice))
                case EndGameChoice(choice) => 
                    Obj("tag" -> "EndGameChoice", "value" -> Bool(choice))
                case Ready => 
                    Obj("tag" -> "Ready")

        override def decode(json: Value): Try[Event] = Try:
            json("tag").str match
                case "PlayerAction" => PlayerAction(ChoiceWire.decode(json("value")).get)
                case "EndGameChoice" => EndGameChoice(json("value").bool)
                case "Ready" => Ready
                case _ => throw new DecodingException("Not a valid event!")

    object PhaseWire extends WireFormat[Phase]:
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
                case CardReveal => Obj("phase" -> "CardReveal")
                case Reveal  => Obj("phase" -> "Reveal")
                case EndGame => Obj("phase" -> "EndGame")

        def decode(json: Value): Try[Phase] = Try:
            json("phase").str match
                case "InGame" => InGame(IntWire.decode(json("turn")).get)
                case "PlayerChoice" => 
                    PlayerChoice(
                        IntWire.decode(json("turn")).get,
                        ChoiceWire.decode(json("choice")).get
                    )
                case "CardReveal" => CardReveal
                case "Reveal" => Reveal
                case "EndGame" => EndGame
                case _ => throw new DecodingException("Not a valid phase!")

    object CardWire extends WireFormat[Card]:
        def encode(card: Card): Value =
            Obj(
                "value" -> IntWire.encode(card.value),
                "suit" -> StringWire.encode(card.suit)
            )
        
        def decode(json: Value): Try[Card] = Try:
            val obj = json.obj
            Card(IntWire.decode(obj("value")).get, StringWire.decode(obj("suit")).get)

    object HandsWire extends WireFormat[Hands]:
        def encode(hands: Hands): Value =
            hands match 
                case EmptyHand => Obj("tag" -> "EmptyHand")
                case Hand(first, second) =>
                    Obj(
                        "tag" -> "Hand",
                        "first" -> CardWire.encode(first),
                        "second" -> CardWire.encode(second)
                    )
            
        def decode(json: Value): Try[Hands] = Try:
            json("tag").str match
                case "EmptyHand" => EmptyHand
                case "Hand" =>
                    val first = CardWire.decode(json("first")).get
                    val second = CardWire.decode(json("second")).get
                    Hand(first, second)

    object PhaseViewWire extends WireFormat[PhaseView]:
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

    object TableViewWire extends WireFormat[TableView]:
        def encode(tableView: TableView): Value =
            Obj(
                "dealerCards" -> VectorWire(CardWire).encode(tableView.dealerCards),
                "poolBalance" -> IntWire.encode(tableView.poolBalance)
            )
            
        def decode(json: Value): Try[TableView] = Try:
            val dealerCards = VectorWire(CardWire).decode(json("dealerCards")).get
            val poolBalance = IntWire.decode(json("poolBalance")).get
            TableView(dealerCards, poolBalance)
    
    object PlayersViewWire extends WireFormat[PlayersView]:
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
        
    override object viewFormat extends WireFormat[View]:
        override def encode(view: View): Value =
            Obj(
                "Phase" -> PhaseViewWire.encode(view.phaseView),
                "Players" -> PlayersViewWire.encode(view.playersView),
                "Table" -> TableViewWire.encode(view.tableView)
            )
        
        override def decode(json: Value): Try[View] = Try:
            val phaseView = PhaseViewWire.decode(json("Phase")).get
            val playersView = PlayersViewWire.decode(json("Players")).get
            val tableView = TableViewWire.decode(json("Table")).get
            View(phaseView, playersView, tableView)
            