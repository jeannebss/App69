/**
 * The `apps.app69` package provides logic to evaluate poker hands and determine winners in a poker game.
 * It includes utilities for identifying hand values, checking for specific combinations, and calculating
 * the winners based on the highest hand.
 */

package apps
package app69

import cs214.webapp.UserId
import app69.Hands.*

/**
 * Enum representing different hand values in poker.
 */
enum HandValue:
    case High(value: Value)
    case Pair(value: Value)
    case DoublePair(value: Value)
    case Brelan(value: Value)
    case Straight(value: Value)
    case Flush(value: Value)
    case Full(value: Value)
    case House(value: Value)
    case StraightFlush(value: Value)

/** 
 * Represents a set of winners in the game.
 */    
type Winners = Set[UserId]

/**
 * The `WinnerLogic` object contains functions for evaluating poker hands and determining winners.
 */
object WinnerLogic:

    /**
     * Determines if a given set of cards contains a straight (five consecutive values).
     * 
     * @param card The set of cards to evaluate.
     * @return An optional value representing the highest card in the straight, if found.
     */
    def straight(card: Iterable[Card]): Option[Value] = 
        val listofStraight = card.map(_.value).toList.sortBy(identity).sliding(5).filter(_.sliding(2).forall({
            case List(a, b) => b == a + 1
            case _ => true
        }))
        listofStraight.flatten.maxOption

    /**
     * Determines if a given set of cards contains a flush (five cards of the same suit).
     * 
     * @param card The set of cards to evaluate.
     * @return An optional card representing the highest card in the flush, if found.
     */
    def flush(card: Iterable[Card]): Option[Card] =
        val groupCard = card.groupBy(_.suit)
        groupCard.filter(_._2.size >= 5).values.headOption.map(x => x.maxBy(_.value))


    /**
     * Finds the highest card value with a specific multiplicity in the set of cards.
     * 
     * @param num The number of cards required to have the same value.
     * @param card The set of cards to evaluate.
     * @return An optional value representing the card value, if found.
     */
    def multi(num: Int, card: Iterable[Card]): Option[Value] = 
        val groupCard = card.groupBy(_.value)
        groupCard.filter(_._2.size == num).keys.maxOption


    /**
     * Evaluates the value of a poker hand given a player's hand and the dealer's cards.
     * 
     * @param hand The player's hand.
     * @param dealer The dealer's cards.
     * @return The determined `HandValue` for the player's hand.
     */
    def handValue(hand: Hands, dealer: List[Card]): HandValue = 
        hand match
            case Hand(card1, card2) =>
                val set = dealer.toSet + card1 + card2 ++ (if card1.value == 14 then Set(Card(1, card1.suit)) else Set.empty) ++ (if card2.value == 14 then Set(Card(1, card2.suit)) else Set.empty)
                if flush(set).isDefined then
                    (if straight(set.filter(_.suit == flush(set).get.suit)).isDefined then return HandValue.StraightFlush(straight(set.filter(_.suit == flush(set).get.suit)).get))
                    
                if multi(4, set).isDefined then return HandValue.House(multi(4, set).get)

                if multi(3, set).isDefined then
                    if multi(2, set.filter(_.value != multi(3, set).get)).isDefined then return HandValue.Full(multi(3, set).get)

                if flush(set).isDefined then return HandValue.Flush(flush(set).get.value)

                if straight(set).isDefined then return HandValue.Straight(straight(set).get)

                if multi(3, set).isDefined then return HandValue.Brelan(multi(3, set).get)

                if multi(2, set).isDefined then
                    (if multi(2, set.filter(_.value != multi(2, set).get)).isDefined then return HandValue.DoublePair(multi(2, set).get)
                    else return HandValue.Pair(multi(2, set).get))
                        else return HandValue.High(set.map(_.value).max)
            case EmptyHand => throw IllegalArgumentException("Impossible case")

    /**
     * Determines the winners of the poker game based on the players' hands and the dealer's cards.
     * 
     * @param players A map of player IDs to their respective hands.
     * @param dealer The dealer's cards.
     * @param activePlayer A map of player IDs to their active status in the game.
     * @return A set of user IDs representing the winners.
     */
    def winner(players: Map[UserId, Hands], dealer: List[Card], activePlayer: Map[UserId, Boolean]): Winners =
        val activeHand = players.filter((id, h) => activePlayer(id))
        val handValue: Map[UserId, HandValue] = activeHand.map(tup => (tup._1, WinnerLogic.handValue(tup._2, dealer)))
        val max = handValue.maxBy(_._2.ordinal)._2
        handValue.filter(tup => tup._2 == max).keys.toSet