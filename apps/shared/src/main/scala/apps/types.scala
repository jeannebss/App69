package apps

import cs214.webapp.UserId
import Value.*
import apps.Suit.*

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

    def allCards(): List[Card] =
        val deck = 
            for
                value <- AllValues
                suit <- AllSuits
            yield Card(value, suit)
        deck.toList
    
    def completeName(): String =
        if value <= 10 then f"$value$suit"
        else if value == 11 then f"J$suit"
        else if value == 12 then f"Q$suit"
        else if value == 13 then f"K$suit"
        else f"A$suit"

    def sameSuit(other: Card): Boolean =
        other.suit == this.suit
    
    def sameValue(other: Card): Boolean = 
        other.value == other.value
    
case class GameState(
    playersBets: Map[UserId, Balance],
    poolValue: Int,
    roundBets: Map[UserId, Bet],
    currentPlayer: UserId,
    dealerCards: Set[Card],
    playerCards: Map[UserId, Hand],
    pahse: Phase
)

enum Phase:
    case Setup, Playing, EndRound, EndGame

case class View(
    phaseView: PhaseView
)

enum PhaseView:
    case Start
    case Playing
    case Done

enum Event:
    case PlayerAction(action: Action)
    case EndGameChoice(choice: Boolean)

enum Action:
    case Check
    case Call 
    case Fold
    case Raise(value: Bet)