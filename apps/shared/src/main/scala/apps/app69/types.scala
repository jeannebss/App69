package apps
package app69

import cs214.webapp.UserId

// Type alias for suits
type Suit = String

// Type alias for values
type Value = Int

// Type alias for bets
type Bet = Int

// Type alias for balances
type Balance = Int

/**
  * Suit of card : can take values of: spade, heart, diamond and club
  * like in other classic card games.
  */
object Suit:
    val Spade = "♠️"
    val Heart = "♥️"
    val Diamond = "♦"
    val Club = "♣️"
    val AllSuits = Set(Heart, Club, Spade, Diamond)
    
    /**
      * Defines the order of a suit
      *
      * @param suit 
      * @return order of the suit
      */
    def order(suit: Suit): Int = suit match
        case Spade   => 0
        case Heart   => 1
        case Diamond => 2
        case Club    => 3

/**
  * All possible values for cards.
  * For easier implementation we use both 1 and 14 for the ace.
  * Other cards use their respective value and Jack takes 11, Queen 12,
  * and finally King takes 13.
  */
object Value:
    val AllValues: Set[Value] = (2 to 14).toSet

/**
  * Card representation that takes two parameters: its
  * value and suit.
  *
  * @param value the value of the card
  * @param suit the suit of the card
  */
case class Card(value: Value, suit: Suit):
    require(value >= 1 && value <= 14)
    
    /**
      * String representation of the whole card with
      * both its value and suit
      *
      * @return card representation
      */
    def completeName(): String =
        if value <= 10 then s"$value$suit"
        else if value == 11 then s"J$suit"
        else if value == 12 then s"Q$suit"
        else if value == 13 then s"K$suit"
        else s"A$suit"

    /**
      * Compares the suit of this card with the suit of another one.
      *
      * @param other card to be compared to this
      * @return true if both cards have the same suit, else false
      */
    def sameSuit(other: Card): Boolean =
        other.suit == this.suit
    
    /**
      * Compares the value of this card with the value of another one.
      *
      * @param other card to be compared to this
      * @return true if both cards have the same value, else false
      */
    def sameValue(other: Card): Boolean = 
        other.value == this.value

/**
  * Card symbols is the in game representation of cards
  * using emojis as symbols for a better user experience.
  */
object CardSymbols:
    /**
      * Returns the emoji corresponding to the card
      *
      * @param card
      * @return representation of the card
      */
    def apply(card: Card): String =
        // Unicode base (first card representation)
        val base = 0x1F0A1
        val suitDecalage = Suit.order(card.suit) * 16
        val cardValueOffset = card.value match
            case 12 => 12
            case 13 => 13
            case 14 => 0
            case _ => card.value - 1
        val unicode = (base + suitDecalage + cardValueOffset)
        new String(Character.toChars(unicode))

    // Represents the back of a card
    val back: String = "🂠"

/**
  * Object for all cards of a poker game that are in the deck
  */
object AllCards:
    /**
      * The function apply returns a deck with all cards of a poker game
      *
      * @return deck of all possible cards
      */
    def apply: List[Card] =
        (for
            value <- Value.AllValues
            suit <- Suit.AllSuits
        yield Card(value, suit)).toList

/**
  * Hand represents the two cards of a player in a game.
  * It takes values of an empty hand when we are in game so that
  * people cannot see other player's cards and a hand with both cards visible.
  */
enum Hands:
    case EmptyHand
    case Hand(first: Card, second: Card)

/**
  * Represents the game state of the actual poker game.
  *
  * @param players list of all players in a game
  * @param playerBalance maps all players and their current balances
  * @param poolValue value in the pool (sum of all bets in a round)
  * @param currentPlayer current player of a round
  * @param dealerCards cards of the dealer that will  be placed on the table
  * @param playerCards maps players to their cards (hands)
  * @param phase phase of the game
  * @param activePlayer players that are active in a round (have not folded yet)
  * @param smallBlind small blind of a round
  * @param highestBetter player that bet the highest amount
  * @param turnBets maps players with their current bet in a turn
  */
case class GameState(
    players: List[UserId],
    playerBalance: Map[UserId, Balance],
    poolValue: Balance,
    currentPlayer: UserId,
    dealerCards: List[Card],
    playerCards: Map[UserId, Hands],
    phase: Phase,
    activePlayer: Map[UserId, Boolean],
    smallBlind: UserId,
    highestBetter: UserId,
    turnBets: Map[UserId, Bet]
)

/**
  * Represents all phases of a poker game
  */
enum Phase:
    case InGame(turn: Int)
    case PlayerChoice(turn: Int, choice: Choice)
    case CardReveal(winners: Vector[UserId])
    case Reveal
    case EndGame 

/**
  * View of a player based on the phase of the game
  *
  * @param phaseView view of the phase
  * @param playersView view of all players
  * @param tableView view of the table
  */
case class View(
    phaseView: PhaseView,
    playersView: PlayersView,
    tableView: TableView
)

/**
  * View of the current phase of the game
  */
enum PhaseView:
    case ChoiceSelection
    case NotPlaying
    case ChoiceMade(choice: Choice, player: UserId)
    case Winners(winners: Vector[UserId])
    case IsReady(ready: Boolean)
    case End(winners: Vector[UserId], balance: Balance)

/**
  * View of the table during a game
  */
case class TableView(
    dealerCards: Vector[Card],
    poolBalance: Balance
)

/**
  * View of the players during a game
  */
enum PlayersView:
    case InGamePlayer(
        playerIndex: Map[UserId, Int],
        playerBalance: Map[UserId, Balance],
        activePlayers: Map[UserId, Boolean],
        currentPlayer: UserId,
        turnBets: Map[UserId, Bet],
        hand: Hands
    )
    case PlayerCardReveal(
        playerIndex: Map[UserId, Int],
        playerBalance: Map[UserId, Balance],
        activePlayers: Map[UserId, Boolean],
        playerHands: Map[UserId, Hands]
    )

/**
  * Events that can occur during a game (by a user)
  */
enum Event:
    case PlayerAction(choice: Choice)
    case EndGameChoice(choice: Boolean)
    case Ready

/**
  * Choices of a player during his turn in a game
  */
enum Choice:
    case Check
    case Call 
    case Fold
    case Raise(value: Bet)

    /**
      * String representation of player choices for better
      * UI experience.
      *
      * @return string representation of a choice
      */
    override def toString(): String = this match
        case Check => "checked"
        case Call => "called"
        case Fold => "folded"
        case Raise(value) => s"raised by ${value}"
