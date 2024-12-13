package apps

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
    import CardView.*

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

    object HandWire extends WireFormat[Hand]:
        def encode(hand: Hand): Value =
            Obj(
                "first" -> CardWire.encode(hand.first),
                "second" -> CardWire.encode(hand.second)
            )
            
        def decode(json: Value): Try[Hand] = Try:
            Hand(CardWire.decode(json("first")).get, CardWire.decode(json("second")).get)

    object PhaseViewWire extends WireFormat[PhaseView]:
        def encode(phaseView: PhaseView): Value =
            phaseView match
                case ChoiceSelection =>
                    Obj("tag" -> "ChoiceSelection")
                case NotPlaying =>
                    Obj("tag" -> "NotPlaying")
                case ChoiceMade(choice: Choice) => 
                    Obj(
                        "tag" -> "ChoiceMade",
                        "Choice" -> ChoiceWire.encode(choice)
                    )
                case Winner =>
                    Obj("tag" -> "Winner")

        def decode(json: Value): Try[PhaseView] = Try:
            json("tag").str match
                case "ChoiceSelection" =>
                    ChoiceSelection
                case "NotPlaying" => NotPlaying
                case "ChoiceMade" =>
                    val choice = ChoiceWire.decode(json("Choice")).get
                    ChoiceMade(choice)
                case "Winner" =>
                    Winner
                case _ =>
                    throw new DecodingException("Not a valid phase view")

    object ScoresViewWire extends WireFormat[ScoresView]:
        def encode(scoresView: ScoresView): Value =
            Obj(
                "Scores" -> MapWire(StringWire, IntWire).encode(scoresView.playerScores),
                "Pool" -> IntWire.encode(scoresView.poolBalance)
            )
        
        def decode(json: Value): Try[ScoresView] = Try:
            val scores = MapWire(StringWire, IntWire).decode(json("Scores")).get
            val pool = IntWire.decode(json("Pool")).get
            ScoresView(scores, pool)

    object CardViewWire extends WireFormat[CardView]: 
        def encode(cardView: CardView): Value =
            cardView match
                case InGameCards(playerCards, dealerCards) =>
                    Obj(
                        "tag" -> "InGameCards",
                        "PlayerCards" -> HandWire.encode(playerCards),
                        "DealerCards" -> VectorWire(CardWire).encode(dealerCards)
                    )
                case RevealCards(playerCards, dealerCards) =>
                    Obj(
                        "tag" -> "RevealCards",
                        "PlayerCards" -> MapWire(StringWire, HandWire).encode(playerCards),
                        "DealerCards" -> VectorWire(CardWire).encode(dealerCards)
                    )

        def decode(json: Value): Try[CardView] = Try:
            json("tag").str match
                case "InGameCards" =>
                    val playerCards = HandWire.decode(json("PlayerCards")).get
                    val dealerCards = VectorWire(CardWire).decode(json("DealerCards")).get
                    InGameCards(playerCards, dealerCards)
                case "RevealCards" =>
                    val playerCards = MapWire(StringWire, HandWire).decode(json("PlayerCards")).get
                    val dealerCards = VectorWire(CardWire).decode(json("DealerCards")).get
                    RevealCards(playerCards, dealerCards)
                case _ => throw new DecodingException("Not a valid card view!")

    override object viewFormat extends WireFormat[View]:
        
        override def encode(view: View): Value =
            Obj(
                "Phase" -> PhaseViewWire.encode(view.phaseView),
                "Scores" -> ScoresViewWire.encode(view.scoresView),
                "Cards" -> CardViewWire.encode(view.cardView)
            )
        
        override def decode(json: Value): Try[View] = Try:
            val phaseView = PhaseViewWire.decode(json("Phase")).get
            val scoresView = ScoresViewWire.decode(json("Scores")).get
            val cardsView = CardViewWire.decode(json("Cards")).get
            View(phaseView, scoresView, cardsView)
            