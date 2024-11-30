package apps
package poker

import cs214.webapp.UserId

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

    
type Winners = Set[UserId]

object WinnerLogic:
    def straight(card: Iterable[Card]): Option[Value] = 
        val listofStraight = card.map(_.value).toList.sortBy(identity).sliding(5).filter(_.sliding(2).forall({
            case List(a, b) => b == a + 1
            case _ => true
        }))
        listofStraight.flatten.maxOption

    def flush(card: Iterable[Card]): Option[Card] =
        val groupCard = card.groupBy(_.suit)
        groupCard.filter(_._2.size >= 5).values.headOption.map(x => x.maxBy(_.value))

    def multi(num: Int, card: Iterable[Card]): Option[Value] = 
        val groupCard = card.groupBy(_.value)
        groupCard.filter(_._2.size == num).keys.maxOption

    def handValue(hand: Hand, dealer: Set[Card]): HandValue = 
            val set = dealer + hand._1 + hand._2
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

    def winner(players: Map[UserId, Hand], dealer: Set[Card]): Winners =
        val handValue: Map[UserId, HandValue] = players.map(tup => (tup._1, WinnerLogic.handValue(tup._2, dealer)))
        val max = handValue.maxBy(_._2.ordinal)._2
        handValue.filter(tup => tup._2 == max).keys.toSet