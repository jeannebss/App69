package apps

import cs214.webapp.UserId

type Suit = String

type Value = Int

type Bet = Int

type Balance = Int
object Suit:
    val Spade = "♠️"
    val Heart = "♥️"
    val Diamond = "♦"
    val Club = "♣️"
    val AllSuits = Set(Heart, Club, Spade, Diamond)
    
    def order(suit: Suit): Int =
        suit match
            case Spade   => 0
            case Heart   => 1
            case Diamond => 2
            case Club    => 3

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

object CardSymbols:
    def apply(card: Card): String =
        val base = 0x1F0A1
        val suitDecalage = Suit.order(card.suit)*16
        val cardValueOffset = card.value match
            case 12 => 12
            case 13 => 13
            case 14 => 0
            case _ => card.value - 1
        val unicode = (base + suitDecalage + cardValueOffset)
        new String(Character.toChars(unicode))

    val back: String = "🂠"

object AllCards:
    def apply: List[Card] =
        (for
            value <- Value.AllValues
            suit <- Suit.AllSuits
        yield Card(value, suit)).toList

case class Hand(first:Card, second:Card)
/*
enum Hands:
    case EmptyHand
    case Hand(first: Card, second: Card)
*/

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
    case InGame
    case PlayerChoice(choice: Choice)
    case CardReveal
    case Reveal
    case EndGame 

case class View(
    phaseView: PhaseView,
    playersView: PlayersView,
    tableView: TableView
)


enum PhaseView:
    case Selecting
    case NotPlaying
    case MadeChoice(choice: Choice)
    case Winner(winners: Vector[UserId])
    case IsReady
    case End(winners: Vector[UserId], balance: Balance)

case class TableView(
    dealerCards: Vector[Card],
    poolBalance: Balance
)

enum PlayersView:
    case InGame(playerAttributes: Map[UserId, (Int, Balance, Boolean, Boolean)], hand: Hand)
    case Reveal(playerAttributes: Map[UserId, (Int, Balance, Boolean, Hand)])

enum Event:
    case PlayerAction(choice: Choice)
    case EndGameChoice(choice: Boolean)
    case Ready

enum Choice:
    case Check
    case Call 
    case Fold
    case Raise(value: Bet)
