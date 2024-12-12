package apps
package app69
import cs214.webapp.*
import cs214.webapp.Action
import Card.*
import Suit.*


class TestLogic extends munit.FunSuite{
    var logique = Logic()
    val clients = Seq("Alexis", "Jeanne", "Antoine", "Jakub", "Guillaume")
    var gameState = logique.init(clients)
    val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState

    // pour tester les mains je peux créer mon propre gamestate sans faire le init et y foutre directement les cartes que je veux
    // rajouter des tests dans le cas ou 
    test("GameState Init: Verify that every parameter is set correctly"):
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = logique.init(clients)
        val joueurs = List("Alexis", "Jeanne", "Antoine", "Jakub", "Guillaume")
        val money = Map(("Alexis" -> 1000), ("Jeanne" -> 1000), ("Antoine" -> 1000), ("Jakub" -> 1000), ("Guillaume" -> 1000))
        assertEquals(players, joueurs)
        assertEquals(playerBalance, money)
        assertEquals(poolValue, 0)
        assertEquals(currentPlayer, "Alexis")
        assertEquals(activePlayer,  Map(("Alexis" -> true), ("Jeanne" -> true), ("Antoine" -> true), ("Jakub" -> true), ("Guillaume" -> true)))
        assertEquals(smallBlind, "Alexis")
        assertEquals(highestBetter, "Alexis")
        assertEquals(turnBets,  Map(("Alexis" -> 0), ("Jeanne" -> 0), ("Antoine" -> 0), ("Jakub" -> 0), ("Guillaume" -> 0)))

    test("Game Simulation: can't raise an amount superior to his balance"):
        try{logique.transition(gameState)("Alexis", Event.PlayerAction(Choice.Raise(1200)))}
        catch
            case IllegalMoveException("not your turn")=> true
            case _ => throw new IllegalMoveException("not your turn")


    test("Game Simulation: Test on the Raise action"):
        val action = logique.transition(gameState)("Alexis", Event.PlayerAction(Choice.Raise(100))).get(2)
        action match {
                case Action.Render(st) => gameState = st
                case _ => throw new IllegalStateException("Unexpected action type")
            }
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState
        val money = Map(("Alexis" -> 900), ("Jeanne" -> 1000), ("Antoine" -> 1000), ("Jakub" -> 1000), ("Guillaume" -> 1000))
        assertEquals(playerBalance, money)
        assertEquals(poolValue, 0)
        assertEquals(currentPlayer, "Jeanne")
        assertEquals(Phase.InGame(0), phase)
        assertEquals(activePlayer, Map(("Alexis" -> true), ("Jeanne" -> true), ("Antoine" -> true), ("Jakub" -> true), ("Guillaume" -> true)))
        assertEquals(smallBlind, "Alexis")
        assertEquals(highestBetter, "Alexis")
        assertEquals(turnBets, Map(("Alexis" -> 100), ("Jeanne" -> 0), ("Antoine" -> 0), ("Jakub" -> 0), ("Guillaume" -> 0)))

    test("Game Simulation: should throw an exception because player can't check"):
        try{logique.transition(gameState)("Jeanne", Event.PlayerAction(Choice.Check))}
        catch
            case IllegalMoveException("not your turn")=> true
            case _ => throw new IllegalMoveException("not your turn")

    test("Game Simulation: Test on the Fold action"):
        val action = logique.transition(gameState)("Jeanne", Event.PlayerAction(Choice.Fold)).get(2)
        action match {
                case Action.Render(st) => gameState = st
                case _ => throw new IllegalStateException("Unexpected action type")
            }
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState
        assertEquals(playerBalance, Map(("Alexis" -> 900), ("Jeanne" -> 1000), ("Antoine" -> 1000), ("Jakub" -> 1000), ("Guillaume" -> 1000)))
        assertEquals(poolValue, 0)
        assertEquals(currentPlayer, "Antoine")
        assertEquals(phase, Phase.InGame(0))
        assertEquals(activePlayer, Map(("Alexis" -> true), ("Jeanne" -> false), ("Antoine" -> true), ("Jakub" -> true), ("Guillaume" -> true)))
        assertEquals(smallBlind, "Alexis")
        assertEquals(highestBetter, "Alexis")
        assertEquals(turnBets, Map(("Alexis" -> 100), ("Jeanne" -> 0), ("Antoine" -> 0), ("Jakub" -> 0), ("Guillaume" -> 0)))

    test("Game Simulation: Test on the Call action"):
        val action = logique.transition(gameState)("Antoine", Event.PlayerAction(Choice.Call)).get(2)
        action match {
                case Action.Render(st) => gameState = st
                case _ => throw new IllegalStateException("Unexpected action type")
            }
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState
        assertEquals(playerBalance, Map(("Alexis" -> 900), ("Jeanne" -> 1000), ("Antoine" -> 900), ("Jakub" -> 1000), ("Guillaume" -> 1000)))
        assertEquals(poolValue, 0)
        assertEquals(currentPlayer, "Jakub")
        assertEquals(phase, Phase.InGame(0))
        assertEquals(activePlayer, Map(("Alexis" -> true), ("Jeanne" -> false), ("Antoine" -> true), ("Jakub" -> true), ("Guillaume" -> true)))
        assertEquals(smallBlind, "Alexis")
        assertEquals(highestBetter, "Alexis")
        assertEquals(turnBets, Map(("Alexis" -> 100), ("Jeanne" -> 0), ("Antoine" -> 100), ("Jakub" -> 0), ("Guillaume" -> 0)))

    test("Game Simulation: Test on the arguments of the Game State"):
        val action = logique.transition(gameState)("Jakub", Event.PlayerAction(Choice.Fold)).get(2)
        action match {
                case Action.Render(st) => gameState = st
                case _ => throw new IllegalStateException("Unexpected action type")
            }
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState
        assertEquals(playerBalance, Map(("Alexis" -> 900), ("Jeanne" -> 1000), ("Antoine" -> 900), ("Jakub" -> 1000), ("Guillaume" -> 1000)))
        assertEquals(poolValue, 0)
        assertEquals(currentPlayer, "Guillaume")
        assertEquals(phase, Phase.InGame(0))
        assertEquals(activePlayer, Map(("Alexis" -> true), ("Jeanne" -> false), ("Antoine" -> true), ("Jakub" -> false), ("Guillaume" -> true)))
        assertEquals(smallBlind, "Alexis")
        assertEquals(highestBetter, "Alexis")
        assertEquals(turnBets, Map(("Alexis" -> 100), ("Jeanne" -> 0), ("Antoine" -> 100), ("Jakub" -> 0), ("Guillaume" -> 0)))

    test("Game Simulation: Test on the end of a round. Balance, Pool, TurnBets"):
        val action = logique.transition(gameState)("Guillaume", Event.PlayerAction(Choice.Call)).get(2)
        action match {
                case Action.Render(st) => gameState = st
                case _ => throw new IllegalStateException("Unexpected action type")
            }
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState
        assertEquals(playerBalance, Map(("Alexis" -> 900), ("Jeanne" -> 1000), ("Antoine" -> 900), ("Jakub" -> 1000), ("Guillaume" -> 900)))
        assertEquals(poolValue, 300)
        assertEquals(currentPlayer, "Alexis")
        assertEquals(phase, Phase.InGame(1))
        assertEquals(activePlayer, Map(("Alexis" -> true), ("Jeanne" -> false), ("Antoine" -> true), ("Jakub" -> false), ("Guillaume" -> true)))
        assertEquals(smallBlind, "Alexis")
        assertEquals(highestBetter, "Alexis")
        assertEquals(turnBets, Map(("Alexis" -> 0), ("Jeanne" -> 0), ("Antoine" -> 0), ("Jakub" -> 0), ("Guillaume" -> 0)))

    test("Game Simulation: First player of the turn checks"):
        val action = logique.transition(gameState)("Alexis", Event.PlayerAction(Choice.Check)).get(2)
        action match {
                case Action.Render(st) => gameState = st
                case _ => throw new IllegalStateException("Unexpected action type")
            }
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState
        assertEquals(playerBalance, Map(("Alexis" -> 900), ("Jeanne" -> 1000), ("Antoine" -> 900), ("Jakub" -> 1000), ("Guillaume" -> 900)))
        assertEquals(poolValue, 300)
        assertEquals(currentPlayer, "Antoine")
        assertEquals(phase, Phase.InGame(1))
        assertEquals(activePlayer, Map(("Alexis" -> true), ("Jeanne" -> false), ("Antoine" -> true), ("Jakub" -> false), ("Guillaume" -> true)))
        assertEquals(smallBlind, "Alexis")
        assertEquals(highestBetter, "Alexis")
        assertEquals(turnBets, Map(("Alexis" -> 0), ("Jeanne" -> 0), ("Antoine" -> 0), ("Jakub" -> 0), ("Guillaume" -> 0)))
    
    test("Game Simulation: The highest better isn't the small blind"):
        val action = logique.transition(gameState)("Antoine", Event.PlayerAction(Choice.Raise(200))).get(2)
        action match {
                case Action.Render(st) => gameState = st
                case _ => throw new IllegalStateException("Unexpected action type")
            }
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState
        assertEquals(playerBalance, Map(("Alexis" -> 900), ("Jeanne" -> 1000), ("Antoine" -> 700), ("Jakub" -> 1000), ("Guillaume" -> 900)))
        assertEquals(poolValue, 300)
        assertEquals(currentPlayer, "Guillaume")
        assertEquals(phase, Phase.InGame(1))
        assertEquals(activePlayer, Map(("Alexis" -> true), ("Jeanne" -> false), ("Antoine" -> true), ("Jakub" -> false), ("Guillaume" -> true)))
        assertEquals(smallBlind, "Alexis")
        assertEquals(highestBetter, "Antoine")
        assertEquals(turnBets, Map(("Alexis" -> 0), ("Jeanne" -> 0), ("Antoine" -> 200), ("Jakub" -> 0), ("Guillaume" -> 0)))

    test("Game Simulation: Test if the next player is the correct one and that the turn isn't done"):
        val action = logique.transition(gameState)("Guillaume", Event.PlayerAction(Choice.Call)).get(2)
        action match {
                case Action.Render(st) => gameState = st
                case _ => throw new IllegalStateException("Unexpected action type")
            }
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState
        assertEquals(playerBalance, Map(("Alexis" -> 900), ("Jeanne" -> 1000), ("Antoine" -> 700), ("Jakub" -> 1000), ("Guillaume" -> 700)))
        assertEquals(poolValue, 300)
        assertEquals(currentPlayer, "Alexis")
        assertEquals(phase, Phase.InGame(1))
        assertEquals(activePlayer, Map(("Alexis" -> true), ("Jeanne" -> false), ("Antoine" -> true), ("Jakub" -> false), ("Guillaume" -> true)))
        assertEquals(smallBlind, "Alexis")
        assertEquals(highestBetter, "Antoine")
        assertEquals(turnBets, Map(("Alexis" -> 0), ("Jeanne" -> 0), ("Antoine" -> 200), ("Jakub" -> 0), ("Guillaume" -> 200)))

    test("Game Simulation: Test on the transition to the next round"):
        val action = logique.transition(gameState)("Alexis", Event.PlayerAction(Choice.Fold)).get(2)
        action match {
                case Action.Render(st) => gameState = st
                case _ => throw new IllegalStateException("Unexpected action type")
            }
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState
        assertEquals(playerBalance, Map(("Alexis" -> 900), ("Jeanne" -> 1000), ("Antoine" -> 700), ("Jakub" -> 1000), ("Guillaume" -> 700)))
        assertEquals(poolValue, 700)
        assertEquals(currentPlayer, "Antoine")
        assertEquals(phase, Phase.InGame(2))
        assertEquals(activePlayer, Map(("Alexis" -> false), ("Jeanne" -> false), ("Antoine" -> true), ("Jakub" -> false), ("Guillaume" -> true)))
        assertEquals(smallBlind, "Alexis")
        assertEquals(highestBetter, "Antoine")
        assertEquals(turnBets, Map(("Alexis" -> 0), ("Jeanne" -> 0), ("Antoine" -> 0), ("Jakub" -> 0), ("Guillaume" -> 0)))

    test("Game Simulation: Test on the maximum a player can Raise depending on the amount of each player"):
        val action = logique.transition(gameState)("Antoine", Event.PlayerAction(Choice.Raise(200))).get(2)
        action match {
                case Action.Render(st) => gameState = st
                case _ => throw new IllegalStateException("Unexpected action type")
            }
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState
        assertEquals(playerBalance, Map(("Alexis" -> 900), ("Jeanne" -> 1000), ("Antoine" -> 500), ("Jakub" -> 1000), ("Guillaume" -> 700)))
        assertEquals(poolValue, 700)
        assertEquals(currentPlayer, "Guillaume")
        assertEquals(phase, Phase.InGame(2))
        assertEquals(activePlayer, Map(("Alexis" -> false), ("Jeanne" -> false), ("Antoine" -> true), ("Jakub" -> false), ("Guillaume" -> true)))
        assertEquals(smallBlind, "Alexis")
        assertEquals(highestBetter, "Antoine")
        assertEquals(turnBets, Map(("Alexis" -> 0), ("Jeanne" -> 0), ("Antoine" -> 200), ("Jakub" -> 0), ("Guillaume" -> 0)))


    test("Game Simulation: Test on the players still in the game and on the Raise"):
        val action = logique.transition(gameState)("Guillaume", Event.PlayerAction(Choice.Raise(200))).get(2)
        action match {
                case Action.Render(st) => gameState = st
                case _ => throw new IllegalStateException("Unexpected action type")
            }
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState
        assertEquals(playerBalance, Map(("Alexis" -> 900), ("Jeanne" -> 1000), ("Antoine" -> 500), ("Jakub" -> 1000), ("Guillaume" -> 300)))
        assertEquals(poolValue, 700)
        assertEquals(currentPlayer, "Antoine")
        assertEquals(phase, Phase.InGame(2))
        assertEquals(activePlayer, Map(("Alexis" -> false), ("Jeanne" -> false), ("Antoine" -> true), ("Jakub" -> false), ("Guillaume" -> true)))
        assertEquals(smallBlind, "Alexis")
        assertEquals(highestBetter, "Guillaume")
        assertEquals(turnBets, Map(("Alexis" -> 0), ("Jeanne" -> 0), ("Antoine" -> 200), ("Jakub" -> 0), ("Guillaume" -> 400)))

    test("Game Simulation: Should throw an exception because it's not the player's turn"):
        try{logique.transition(gameState)("Alexis", Event.PlayerAction(Choice.Raise(200)))}
        catch
            case IllegalMoveException("not your turn")=> true
            case _ => throw new IllegalMoveException("not your turn")

    test("Game Simulation: Same test"):
        val action = logique.transition(gameState)("Antoine", Event.PlayerAction(Choice.Raise(200))).get(2)
        action match {
                case Action.Render(st) => gameState = st
                case _ => throw new IllegalStateException("Unexpected action type")
            }
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState
        assertEquals(playerBalance, Map(("Alexis" -> 900), ("Jeanne" -> 1000), ("Antoine" -> 100), ("Jakub" -> 1000), ("Guillaume" -> 300)))
        assertEquals(poolValue, 700)
        assertEquals(currentPlayer, "Guillaume")
        assertEquals(phase, Phase.InGame(2))
        assertEquals(activePlayer, Map(("Alexis" -> false), ("Jeanne" -> false), ("Antoine" -> true), ("Jakub" -> false), ("Guillaume" -> true)))
        assertEquals(smallBlind, "Alexis")
        assertEquals(highestBetter, "Antoine")
        assertEquals(turnBets, Map(("Alexis" -> 0), ("Jeanne" -> 0), ("Antoine" -> 600), ("Jakub" -> 0), ("Guillaume" -> 400)))

    test("Game Simulation: Test on the end ou a complete round. Everything is reset and the money is given to the winner"):
        val action = logique.transition(gameState)("Guillaume", Event.PlayerAction(Choice.Fold)).get(2)
        action match {
                case Action.Render(st) => gameState = st
                case _ => throw new IllegalStateException("Unexpected action type")
            }
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState
        assertEquals(playerBalance, Map(("Alexis" -> 900), ("Jeanne" -> 1000), ("Antoine" -> 1800), ("Jakub" -> 1000), ("Guillaume" -> 300)))
        assertEquals(poolValue, 0)
        assertEquals(phase, Phase.Reveal)
        assertEquals(activePlayer, Map(("Alexis" -> false), ("Jeanne" -> false), ("Antoine" -> false), ("Jakub" -> false), ("Guillaume" -> false)))
        assertEquals(smallBlind, "Jeanne")
        assertEquals(turnBets, Map(("Alexis" -> 0), ("Jeanne" -> 0), ("Antoine" -> 0), ("Jakub" -> 0), ("Guillaume" -> 0)))

// basic tests on errors
    test("Game Simulation: Sends an IllegalMoveException when it's not your turn"):
        try{logique.transition(gameState)("Guillaume", Event.PlayerAction(Choice.Fold))}
        catch
            case IllegalMoveException("not your turn")=> true
            case _ => throw new IllegalMoveException("not your turn")


    test("Card Test: dealer cards and players hand change at the end of a round"):

        def updateGameState(player: String, choice: Choice): GameState = 
            val action = logique.transition(gameState)(player, Event.PlayerAction(choice)).get(2)
            action match 
                case Action.Render(st) => st
                case _ => throw new IllegalStateException("Unexpected action type")
        

        val allCards = List(Card(14, Suit.Heart), Card(2, Suit.Spade), Card(3, Suit.Diamond), Card(12, Suit.Club), Card(13, Suit.Heart), Card(4, Suit.Spade), Card(5, Suit.Diamond), Card(10, Suit.Club), Card(11, Suit.Club), Card(10, Suit.Diamond), Card(11, Suit.Spade), Card(7, Suit.Diamond), Card(8, Suit.Spade), Card(14, Suit.Club), Card(14, Suit.Spade))
        val dCards = allCards.take(5)
        val remainingCard = allCards.drop(5)
        val joueurs = List("Alexis", "Jeanne", "Antoine", "Jakub", "Guillaume")
        val pCards = (0 until joueurs.size).map(n => (joueurs(n), Hand(remainingCard(n * 2), remainingCard(n*2+1)))).toMap
        val money = Map(("Alexis" -> 1000), ("Jeanne" -> 1000), ("Antoine" -> 1000), ("Jakub" -> 1000), ("Guillaume" -> 1000))
        gameState = GameState(joueurs,
                            money,
                            0,
                            joueurs.head,
                            dCards,
                            pCards,
                            Phase.InGame(0),
                            joueurs.map(_ -> true).toMap,
                            joueurs.head,
                            joueurs.head,
                            joueurs.map(_ -> 0).toMap)
        
        gameState = updateGameState("Alexis", Choice.Raise(1000))

        gameState = updateGameState("Jeanne", Choice.Call)

        gameState = updateGameState("Antoine", Choice.Call)

        gameState = updateGameState("Jakub", Choice.Fold)

        gameState = updateGameState("Guillaume", Choice.Fold)

        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState

        assertEquals(players, List("Jeanne", "Antoine", "Jakub", "Guillaume"))
        assertEquals(playerBalance, Map(("Jeanne" -> 1500), ("Antoine" -> 1500), ("Jakub" -> 1000), ("Guillaume" -> 1000)))
        assertEquals(poolValue, 0)
        assertEquals(currentPlayer, "Jeanne")
        assert(dealerCards != dCards)
        assert(playerCards != pCards)
        assertEquals(activePlayer,  Map(("Jeanne" -> true), ("Antoine" -> true), ("Jakub" -> true), ("Guillaume" -> true)))
        assertEquals(smallBlind, "Jeanne")
        assertEquals(highestBetter, "Jeanne")
        assertEquals(turnBets, Map(("Jeanne" -> 0), ("Antoine" -> 0), ("Jakub" -> 0), ("Guillaume" -> 0)))


    test("Game Start Test: Game shouldn't start with only one player"): // not sure about that one
        
        try{logique.init(Seq("Jakub"))}
        catch
            case IllegalMoveException("You cannot play alone")=> true
            case _ => throw new IllegalMoveException("You cannot play alone")


    test("Card Test: everyone folds and cards are randomly shuffuled between rounds"):
        def updateGameState(player: String, choice: Choice): GameState = 
            val action = logique.transition(gameState)(player, Event.PlayerAction(choice)).get(2)
            action match 
                case Action.Render(st) => st
                case _ => throw new IllegalStateException("Unexpected action type")
        
        gameState = logique.init(Seq("Guillaume", "Jeanne", "Jakub", "Alexis", "Antoine"))

        println("creation")

        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = gameState

        val dCards = dealerCards
        val pCards = playerCards

        var gameState1 = gameState

        gameState1 = updateGameState("Guillaume", Choice.Fold)

        gameState = gameState1

        gameState1 = updateGameState("Jeanne", Choice.Fold)

        gameState = gameState1

        gameState1 = updateGameState("Jakub", Choice.Fold)

        gameState = gameState1

        gameState1 = updateGameState("Alexis", Choice.Fold)

        gameState = gameState1

        gameState1 = updateGameState("Antoine", Choice.Fold)

        gameState = gameState1

        val GameState(players1, playerBalance1, poolValue1, currentPlayer1, dealerCards1, playerCards1, phase1, activePlayer1, smallBlind1, highestBetter1, turnBets1) = gameState1

        assert(!dealerCards1.equals(dealerCards))
        assert(!playerCards1.equals(playerCards))

        




        
        


        
        



}
