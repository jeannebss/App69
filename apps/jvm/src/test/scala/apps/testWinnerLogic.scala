package apps

class Tests extends munit.FunSuite:
    val dealer = Set(Card(7, "♥️"), Card(6, "♥️"), Card(11, "♠️"), Card(5, "♥️"), Card(11, "♦️"))
    val hand = (Card(8, "♥️"), Card(4, "♥️"))
    test("Player Hand test"):
        val win = HandValue.StraightFlush(8)
        assertEquals(win, WinnerLogic.handValue(hand, dealer))