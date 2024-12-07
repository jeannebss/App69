package apps
package app69
import cs214.webapp.*
import cs214.webapp.Action


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
}
