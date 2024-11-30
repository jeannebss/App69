package apps

class Tests extends munit.FunSuite:
    val dealer = Set(Card(7, "♥️"), Card(6, "♥️"), Card(11, "♠️"), Card(5, "♥️"), Card(11, "♦️"))
    val hand1 = (Card(8, "♥️"), Card(4, "♥️"))
    val hand = (Card(2, "♣️"), Card(4, "♣️"))
    val game = Map(("1", hand1))
    test("Player Hand test"):
        val win = HandValue.StraightFlush(8)
        assertEquals(win, WinnerLogic.handValue(hand1, dealer))

    test("Round winner"):
        val winners = Set("1")
        assertEquals(winners, WinnerLogic.winner(game, dealer))