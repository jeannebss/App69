package apps
package app69


class Tests extends munit.FunSuite:
    val dealer = List(Card(7, "♥️"), Card(6, "♥️"), Card(11, "♠️"), Card(5, "♥️"), Card(11, "♦️"))
    val hand1 = Hand(Card(8, "♥️"), Card(4, "♥️"))
    val hand2 = Hand(Card(2, "♣️"), Card(4, "♣️"))
    val game = Map(("p1", hand1), ("p2", hand2))
    val players = Map(("p1" -> true),("p2" -> true) )
    test("Player Hand test"):
        val win = HandValue.StraightFlush(8)
        assertEquals(win, WinnerLogic.handValue(hand1, dealer))

    test("Round winner"):
        val winners = Set("p1")
        assertEquals(winners, WinnerLogic.winner(game, dealer, players))

    test("Round Winner: the ace counts as an Ace and a 1"):
        val deal = List(Card(2, "♥️"), Card(3, "♥️"), Card(4, "♠️"), Card(12, "♥️"), Card(13, "♦️"))
        val p1 = Hand(Card(14, "♣️"), Card(5, "♣️"))
        val p2 = Hand(Card(2, "♣️"), Card(13, "♣️"))
        val winnner = Set("p1")
        val handMap = Map(("p1" -> p1), ("p2" -> p2))
        val active = Map(("p1" -> true), ("p2"-> true))
        assertEquals(winnner, WinnerLogic.winner(handMap, deal, active))

    test("Round Winners: two players should win"):
        val deal = List(Card(2, "♥️"), Card(3, "♥️"), Card(4, "♠️"), Card(12, "♥️"), Card(13, "♦️"))
        val p1 = Hand(Card(6, "♣️"), Card(5, "♣️"))
        val p2 = Hand(Card(5, "♦️"), Card(6, "♦️"))
        val p3 = Hand(Card(13, "♥️"),  Card(4, "♣️"))
        val winnner = Set("p1", "p2")
        val handMap = Map(("p1" -> p1), ("p2" -> p2), ("p3" -> p3))
        val active = Map(("p1" -> true), ("p2"-> true), ("p3" -> true))
        assertEquals(winnner, WinnerLogic.winner(handMap, deal, active))

    test("Round Winner: tree players should win"):
        val deal = List(Card(2, "♥️"), Card(3, "♥️"), Card(4, "♠️"), Card(12, "♥️"), Card(13, "♦️"))
        val p1 = Hand(Card(6, "♣️"), Card(5, "♣️"))
        val p2 = Hand(Card(5, "♦️"), Card(6, "♦️"))
        val p3 = Hand(Card(6, "♠️"),  Card(5, "♠️"))
        val winnner = Set("p1", "p2", "p3")
        val handMap = Map(("p1" -> p1), ("p2" -> p2), ("p3" -> p3))
        val active = Map(("p1" -> true), ("p2"-> true), ("p3" -> true))
        assertEquals(winnner, WinnerLogic.winner(handMap, deal, active))

    test("Round Winner: tree players should win but one is out and must not"):
        val deal = List(Card(2, "♥️"), Card(3, "♥️"), Card(4, "♠️"), Card(12, "♥️"), Card(13, "♦️"))
        val p1 = Hand(Card(6, "♣️"), Card(5, "♣️"))
        val p2 = Hand(Card(5, "♦️"), Card(6, "♦️"))
        val p3 = Hand(Card(6, "♠️"),  Card(5, "♠️"))
        val winnner = Set("p1", "p2")
        val handMap = Map(("p1" -> p1), ("p2" -> p2), ("p3" -> p3))
        val active = Map(("p1" -> true), ("p2"-> true), ("p3" -> false))
        assertEquals(winnner, WinnerLogic.winner(handMap, deal, active))

    test("Round Winner: four players should win"):
        val deal = List(Card(2, "♥️"), Card(3, "♥️"), Card(4, "♠️"), Card(12, "♦️"), Card(13, "♦️"))
        val p1 = Hand(Card(6, "♣️"), Card(5, "♣️"))
        val p2 = Hand(Card(5, "♦️"), Card(6, "♦️"))
        val p3 = Hand(Card(6, "♠️"),  Card(5, "♠️"))
        val p4 = Hand(Card(6, "♥️"),  Card(5, "♥️"))
        val winnner = Set("p1", "p2", "p3", "p4")
        val handMap = Map(("p1" -> p1), ("p2" -> p2), ("p3" -> p3), ("p4" -> p4))
        val active = Map(("p1" -> true), ("p2"-> true), ("p3" -> true), ("p4" -> true))
        assertEquals(winnner, WinnerLogic.winner(handMap, deal, active))


    

