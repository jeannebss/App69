package apps

import cs214.webapp.UserId
import Value.*
import apps.*

type Suit = String

type Value = Int

type Bet = Int

type Balance = Int

type Hand = (Card, Card)

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

object AllCards:
    def get(): List[Card] =
        val deck = 
            for
                value <- AllValues
                suit <- Suit.AllSuits
            yield Card(value, suit)
        deck.toList

case class GameState(
    playerBalance: Map[UserId, Balance],
    poolValue: Int,
    roundBets: Map[UserId, Bet],
    currentPlayer: UserId,
    dealerCards: List[Card],
    playerCards: Map[UserId, Hand],
    phase: Phase,
    canStillPlay: Map[UserId, Boolean],
    smallBlind: UserId,
    turnBets: Map[UserId, Bet],
    players: Seq[UserId],
    highestBet: UserId,
    betAmount: Int
)

enum Phase:
    case PreFlop, Flop, Turn, Reverse, EndGame 

case class View(
    phaseView: PhaseView,
    scoresView: ScoresView,
    cardView: CardView
)

enum PhaseView:
    case InGame(currentPlayer: UserId, playersCards: Hand)
    case Winner(winnerId: UserId)

type CardView = List[Card]

type ScoresView = (Map[UserId, Int], Int)
    
enum Event:
    case PlayerAction(action: Play)
    case EndGameChoice(choice: Boolean)

enum Play:
    case Check
    case Call 
    case Fold
    case Raise(value: Bet)
