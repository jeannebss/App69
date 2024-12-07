package apps
package app69

import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.server.{StateMachine}
import ujson.Value
import apps.*


import scala.annotation.unused
import scala.util.{Random, Try}

class Logic extends StateMachine[Event, GameState, View]:
    
    val appInfo: AppInfo = AppInfo(
        id = "app69",
        name = "6poker9",
        description = "6poker9 is a strategic poker game designed for players of all skill " +
            "levels, emphasizing thoughtful decision-making and competitive, engaging gameplay.",
        year = 2024
        )

    override val wire = Wire

    private val END_ROUND_PAUSE_MS = 5000

    private val STARTING_BALANCE = 1000

    import apps.Phase.* 
    import apps.Event.*
    import apps.Choice.* 
    import apps.PhaseView.*
    import apps.Card.*

    override def init(clients: Seq[UserId]): GameState =
        val allCards = Random.shuffle(AllCards.apply)
        val dealerCards = allCards.take(5)
        val remainingCard = allCards.drop(5).toList
        val players = clients.toList
        val playerCards = (0 until players.size).map(n => (players(n), Hand(remainingCard(n * 2), remainingCard(n*2+1)))).toMap
        GameState(
            players,
            Map(clients.map(_ -> STARTING_BALANCE)*),
            0,
            clients.head,
            dealerCards,
            playerCards,
            Phase.InGame(0),
            clients.map(_ -> true).toMap,
            clients.head,
            clients.head,
            clients.map(_ -> 0).toMap,
        )


    override def transition(state: GameState)(userId: UserId, event: Event): Try[Seq[Action[GameState]]] = Try:
        import Phase.*
        import Event.*
        val GameState(players, playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = state
        
        val number = players.size

        def selectNextPlayer(newActivePlayer: Map[UserId, Boolean], ref: Int): UserId =
            val player = players(ref)
            return if newActivePlayer(player) then player else selectNextPlayer(newActivePlayer, (ref + 1)%number)


        //C'est sur qu'on a du oublier des truc mais on se rapproche
        (phase, event) match
            case (InGame(turn), PlayerAction(choice)) =>
                require(userId == currentPlayer)

                val highestBet = turnBets.values.max
                //Check the highest amount you can raise(Not sure)
                val raiseCapacity = playerBalance.filter((k, v) => activePlayer(k)).map((k, v) => v - (highestBet - turnBets(k)))
                val maxRaise = raiseCapacity.min

                choice match
                    case Check =>
                        //No player actions
                        require(turnBets(userId) == highestBet)
                        
                        val updateCurrentPlayer = selectNextPlayer(activePlayer, (players.indexOf(currentPlayer)+1)%number)
                        val turnOver = updateCurrentPlayer == highestBetter


                        val nextPoolValue = if turnOver then poolValue + turnBets.values.sum else poolValue
                        val nextTurnBet = if turnOver then players.map(_ -> 0).toMap else turnBets
                        val nextCurrentPlayer = if turnOver then 
                            selectNextPlayer(activePlayer, players.indexOf(smallBlind)) else updateCurrentPlayer


                        //Passe au reveal si un joueur est tapis ou si on est a la fin du dernier round
                        val nextPhase = if turnOver then 
                            if turn == 3 || playerBalance.exists((id, v) => v == 0) then Reveal else InGame(turn + 1)
                            else InGame(turn)

                        //Si reveal calcul du gagnant et update avant de switch to reveal
                        if nextPhase == Reveal then
                            val playerInCard = playerCards.filter((id, v) => activePlayer(id))
                            val winner = WinnerLogic.winner(playerInCard, dealerCards, activePlayer) // détermine les Id du/des gagnants
                            val updatedPlayerBalance = playerBalance.map( (id, amount) =>
                                val newAmount = if winner.contains(id) then amount + poolValue/winner.size else amount
                                (id, newAmount))


                            val nextPlayers = players.filter(id => updatedPlayerBalance(id) > 0)

                            val nextActivePlayer = nextPlayers.map( _ -> false).toMap

                            val nextActiveBlind = players.map(id => (id, nextPlayers.contains(id))).toMap
                            val nextSmallBlind = selectNextPlayer(nextActiveBlind, players.indexOf(smallBlind))

                            
                            //val showCardPhase = state.copy() // Phase spécial ou les cartes sont montré, pas de transisition juste un wait  ??

                            val nextState = state.copy(phase = nextPhase, playerBalance = updatedPlayerBalance, activePlayer = nextActivePlayer, players = nextPlayers)
                            
                            Seq(Action.Render(nextState))
                        

                        //TODO reveal phase logic

                        
                        val nextState = state.copy(poolValue = nextPoolValue, currentPlayer = updateCurrentPlayer, phase = nextPhase, turnBets = nextTurnBet)
                        Seq(Action.Render(nextState))

                    case Call =>
                        //Modify all the balance
                        val diff = highestBet - turnBets(currentPlayer)
                        val updateTurnBet = turnBets.updated(userId, highestBet)
                        val nextPlayerBalance = playerBalance.updated(userId, playerBalance(userId) - diff)

                        val updateCurrentPlayer = selectNextPlayer(activePlayer, (players.indexOf(currentPlayer)+1)%number)
                        val turnOver = updateCurrentPlayer == highestBetter

                        //Update pool, bet and active player
                        val nextPoolValue = if turnOver then poolValue + updateTurnBet.values.sum else poolValue
                        val nextTurnBet = if turnOver then players.map(_ -> 0).toMap else updateTurnBet
                        val nextCurrentPlayer = if turnOver then 
                            selectNextPlayer(activePlayer, players.indexOf(smallBlind)) else updateCurrentPlayer

                        //Passe au reveal si un joueur est tapis ou si on est a la fin du dernier round
                        val nextPhase = if turnOver then 
                            if turn == 3 || nextPlayerBalance.exists((id, v) => v == 0) then Reveal else InGame(turn + 1)
                            else InGame(turn)

                        //Si reveal calcul du gagnant et update avant de switch to reveal
                        if nextPhase == Reveal then
                            val playerInCard = playerCards.filter((id, v) => activePlayer(id))
                            val winner = WinnerLogic.winner(playerInCard, dealerCards, activePlayer) // détermine les Id du/des gagnants
                            val updatedPlayerBalance = playerBalance.map( (id, amount) =>
                                val newAmount = if winner.contains(id) then amount + poolValue/winner.size else amount
                                (id, newAmount))


                            val nextPlayers = players.filter(id => updatedPlayerBalance(id) > 0)

                            val nextActivePlayer = nextPlayers.map( _ -> false).toMap

                            val nextActiveBlind = players.map(id => (id, nextPlayers.contains(id))).toMap
                            val nextSmallBlind = selectNextPlayer(nextActiveBlind, players.indexOf(smallBlind))

                            
                            //val showCardPhase = state.copy() // Phase spécial ou les cartes sont montré, pas de transisition juste un wait  ??

                            val nextState = state.copy(phase = nextPhase, playerBalance = updatedPlayerBalance, activePlayer = nextActivePlayer, players = nextPlayers)
                            
                            Seq(Action.Render(nextState))
                        
                        val nextState = state.copy(playerBalance = nextPlayerBalance, poolValue = nextPoolValue, currentPlayer = nextCurrentPlayer, phase = nextPhase, turnBets = nextTurnBet)
                        Seq(Action.Render(nextState))

                    case Fold =>
                        //Fold Action
                        val nextActivePlayer = activePlayer.updated(userId, false)
                        
                        //Turn end and next player action
                        val updateCurrentPlayer = selectNextPlayer(nextActivePlayer, (players.indexOf(currentPlayer)+1)%number)
                        val turnOver = updateCurrentPlayer == highestBetter

                        //TurnOver reaction
                        val nextPoolValue = if turnOver then poolValue + turnBets.values.sum else poolValue
                        val nextTurnBet = if turnOver then players.map(_ -> 0).toMap else turnBets
                        val nextCurrentPlayer = if turnOver then 
                            selectNextPlayer(activePlayer, players.indexOf(smallBlind)) else updateCurrentPlayer

                        //Passe au reveal si un joueur est tapis ou si on est a la fin du dernier round
                        val nextPhase = if turnOver then 
                            if turn == 3 || playerBalance.exists((id, v) => v == 0) then Reveal else InGame(turn + 1)
                            else InGame(turn)

                        //Si reveal calcul du gagnant et update avant de switch to reveal phase
                        if nextPhase == Reveal then
                            val winner = WinnerLogic.winner(playerCards, dealerCards, activePlayer) // détermine les Id du/des gagnants
                            val updatedPlayerBalance = playerBalance.map( (id, amount) =>
                                val newAmount = if winner.contains(id) then amount + poolValue/winner.size else amount
                                (id, newAmount))

                            


                            val nextPlayers = players.filter(id => updatedPlayerBalance(id) > 0)

                            val nextActiveBlind = players.map(id => (id, nextPlayers.contains(id))).toMap
                            val nextSmallBlind = selectNextPlayer(nextActiveBlind, players.indexOf(smallBlind))

                            val nextActivePlayer = nextPlayers.map( _ -> false).toMap
                            
                            //val showCardPhase = state.copy() // Phase spécial ou les cartes sont montré, pas de transisition juste un wait  ??

                            val nextState = state.copy(phase = nextPhase, playerBalance = updatedPlayerBalance, activePlayer = nextActivePlayer, players = nextPlayers)
                            
                            Seq(Action.Render(nextState))

                        val nextState = state.copy(activePlayer = nextActivePlayer, poolValue = nextPoolValue, currentPlayer = nextCurrentPlayer, phase = nextPhase, turnBets = nextTurnBet)
                        Seq(Action.Render(nextState))

                    case Raise(value) =>
                        //Players action link code, cannot turnOver if someone raise
                        val diff = highestBet + value - turnBets(currentPlayer)
                        require(diff <= playerBalance(userId) && value <= maxRaise)
                        val nextPlayerBalance = playerBalance.updated(userId, playerBalance(userId) - diff)
                        val nextTurnBet = turnBets.updated(userId, turnBets(userId) + value)

                        val nextCurrentPlayer = selectNextPlayer(activePlayer, (players.indexOf(currentPlayer)+1)%number)

                        val nextState = state.copy(currentPlayer = nextCurrentPlayer, playerBalance = nextPlayerBalance, turnBets = nextTurnBet)

                        Seq(Action.Render(nextState))
            case (InGame(turn),_) => throw IllegalMoveException("You can only play in this phase of the game")

            case (Reveal, Ready) => 
                // on calcule le winner avant comme ca on peut savoir si le jeu est fini ou non
                // ensuite, on actualise les joueurs du prochain tour pour avoir une idée de qui reste en jeu
                // ca nous permet ensuite de tester si on a un gagnant ou non et surtout, quand on veut passer
                // au prochain round, on ne prend en compte le fait que les joueurs soient prêts seulement si ils font encore parti du jeu

                val newActivePlayer = activePlayer.updated(userId, true)

                 
                if newActivePlayer.forall((k,v) => v) then//ready means a player is ready to start a new round
                    // penser au fait que les joueurs qui ont perdus ne servent a rien dans le restart

                    val newAllCards = Random.shuffle(AllCards.apply) // nouveau melange des cartes
                    val newDealerCards = newAllCards.take(5) // nouvelles cartes du croupier, sur le model du init
                    val newRemainingCard = newAllCards.drop(5).toList // utile pour éviter d'utiliser les cartes du croupier
                                        // la ligne au dessus est mauvaise, il faut prendre les cartes dans le bon ordre, sauf si on s'en fout
                    val newPlayerCards = (0 until players.size).map(n => (players(n), Hand(newRemainingCard(n * 2), newRemainingCard(n*2+1)))).toMap
                    val newGameState = state.copy(  activePlayer = newActivePlayer, 
                                                    highestBetter = smallBlind, 
                                                    poolValue = 0,
                                                    currentPlayer = smallBlind,
                                                    dealerCards = newDealerCards,
                                                    playerCards = newPlayerCards, 
                                                    phase = InGame(0))
                    Seq(Action.Render(newGameState))
                else
                    //Good
                    val newGameState = state.copy(activePlayer = newActivePlayer)
                    Seq(Action.Render(newGameState))
                                    
            case (Reveal, _) => throw IllegalMoveException("Compare your cards to the other's") //TODO change message 
            case (_) => throw IllegalMoveException("Unsupported phase") //TODO Implement if we want a endgame
            // faire le cas ou on a un tapis

    override def project(state: GameState)(userId: UserId): View = ???
        /*
        val GameState(
            players,
            playerBalance,
            poolValue,
            currentPlayer,
            dealerCards,
            playerCards,
            phase,
            activePlayer,
            smallBlind,
            highestBetter,
            turnBets
        ) = state
        
        phase match
            case InGame(turn) =>
                val scoresView = ScoresView(playerBalance, poolValue)
                val cardView = CardView(
                    playerCards(userId), 
                    dealerCards.take(
                        if turn == 0 then 0
                        else if turn == 1 then 3
                        else if turn == 2 then 4
                        else 5
                    )
                )
                val phaseView = ChoiceSelection(currentPlayer)
                View(phaseView, scoresView, cardView)
            case Reveal => 
                val scoresView = ScoresView(playerBalance, poolValue)
                val cardView = CardView(playerCards(userId), dealerCards) // needs to know turn
                val phaseView = ChoiceMade(currentPlayer, Choice.Call) // adapt
                View(phaseView, scoresView, cardView)
            case EndGame => 
                val scoresView = ScoresView(playerBalance, poolValue)
                val cardView = CardView(playerCards(userId), dealerCards) // no need 
                val phaseView = ChoiceSelection(currentPlayer) // No choice
                View(phaseView, scoresView, cardView)

            case _ => ??? //TODO transition state
            */