package apps

import cs214.webapp.UserId

type Suit = String

type Value = Int

type Bet = Int

type Balance = Int

object Suit:
    val Heart = "♥️"
    val Club = "♣️"
    val Spade = "♠️"
    val Diamond = "♦"
    val AllSuits = Set(Heart, Club, Spade, Diamond)

object Value:
    val AllValues: Set[Value] = (2 to 14).toSet

case class Card(value: Value, suit: Suit):
    require(value >= 2 && value <= 14)
    
    def completeName(): String =
        if value <= 10 then s"$value$suit"
        else if value == 11 then s"J$suit"
        else if value == 12 then s"Q$suit"
        else if value == 13 then s"K$suit"
        else s"A$suit"

    def sameSuit(other: Card): Boolean =
        other.suit == this.suit
    
    def sameValue(other: Card): Boolean = 
        other.value == this.value

    val cardSymbols: Map[String, String] = Map(
    "A♥️" -> "🂱",
    "2♥️" -> "🂲",
    "3♥️" -> "🂳",
    "4♥️" -> "🂴",
    "5♥️" -> "🂵",
    "6♥️" -> "🂶",
    "7♥️" -> "🂷",
    "8♥️" -> "🂸",
    "9♥️" -> "🂹",
    "10♥️" -> "🂺",
    "J♥️" -> "🂻",
    "Q♥️" -> "🂽",
    "K♥️" -> "🂾",
    
    "A♦️" -> "🃁",
    "2♦️" -> "🃂",
    "3♦️" -> "🃃",
    "4♦️" -> "🃄",
    "5♦️" -> "🃅",
    "6♦️" -> "🃆",
    "7♦️" -> "🃇",
    "8♦️" -> "🃈",
    "9♦️" -> "🃉",
    "10♦️" -> "🃊",
    "J♦️" -> "🃋",
    "Q♦️" -> "🃍",
    "K♦️" -> "🃎",
    
    "A♠️" -> "🂡",
    "2♠️" -> "🂢",
    "3♠️" -> "🂣",
    "4♠️" -> "🂤",
    "5♠️" -> "🂥",
    "6♠️" -> "🂦",
    "7♠️" -> "🂧",
    "8♠️" -> "🂨",
    "9♠️" -> "🂩",
    "10♠️" -> "🂪",
    "J♠️" -> "🂫",
    "Q♠️" -> "🂭",
    "K♠️" -> "🂮",
    
    "A♣️" -> "🃑",
    "2♣️" -> "🃒",
    "3♣️" -> "🃓",
    "4♣️" -> "🃔",
    "5♣️" -> "🃕",
    "6♣️" -> "🃖",
    "7♣️" -> "🃗",
    "8♣️" -> "🃘",
    "9♣️" -> "🃙",
    "10♣️" -> "🃚",
    "J♣️" -> "🃛",
    "Q♣️" -> "🃝",
    "K♣️" -> "🃞"
)

object AllCards:
    def apply: Set[Card] =
        for
            value <- Value.AllValues
            suit <- Suit.AllSuits
        yield Card(value, suit)

case class Hand(first: Card, second: Card)

case class GameState(
    players: List[UserId],
    playerBalance: Map[UserId, Balance],
    poolValue: Balance,
    currentPlayer: UserId,
    dealerCards: List[Card],
    playerCards: Map[UserId, Hand],
    phase: Phase,
    activePlayer: Map[UserId, Boolean],
    smallBlind: UserId,
    highestBetter: UserId,
    turnBets: Map[UserId, Bet]
)

enum Phase:
    case InGame(turn: Int)
    case PlayerChoice(turn: Int, choice: Choice)
    case CardReveal
    case Reveal
    case EndGame 

case class View(
    phaseView: PhaseView,
    scoresView: ScoresView,
    cardView: CardView
)

enum PhaseView:
    case ChoiceSelection
    case ChoiceMade(choice: Choice)
    case Winner

enum CardView:
    case InGameCards(playerCards: Hand, dealerCards: Vector[Card])
    case RevealCards(playerCards: Map[UserId, Hand], dealerCards: Vector[Card])

case class ScoresView(
    playerScores: Map[UserId, Balance],
    poolBalance: Balance
)
    
enum Event:
    case PlayerAction(choice: Choice)
    case EndGameChoice(choice: Boolean)
    case Ready

enum Choice:
    case Check
    case Call 
    case Fold
    case Raise(value: Bet)
