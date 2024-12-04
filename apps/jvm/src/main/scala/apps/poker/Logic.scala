package apps.poker

import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.server.{StateMachine}
import ujson.Value
import apps.*
import scala.collection.immutable.{SortedMap}


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
        val listClients = clients.toList
        val playerCards = (0 until listClients.size).map(n => (listClients(n), Hand(remainingCard(n * 2), remainingCard(n*2+1)))).toMap
        GameState(
            SortedMap(clients.map(_ -> STARTING_BALANCE)*),
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
        val GameState(playerBalance, poolValue, currentPlayer, dealerCards, playerCards, phase, activePlayer, smallBlind, highestBetter, turnBets) = state
        val players = playerBalance.keys.toList
        val number = players.size

        def selectNextPlayer(newActivePlayer: Map[UserId, Boolean], ref: Int): UserId =
            val player = players(ref)
            return if newActivePlayer(player) then player else selectNextPlayer(newActivePlayer, (ref + 1)%number)

        //TODO add modulo and check a new turnOver check
        (phase, event) match
            case (InGame(turn), PlayerAction(choice)) =>
                require(userId == currentPlayer)

                val highestBet = turnBets.values.max
                //Check the highest amount you can raise(Not sure)
                val raiseCapacity = playerBalance.filter((k, v) => activePlayer(k)).map((k, v) => v - (highestBet - turnBets(k)))
                val maxRaise = raiseCapacity.min

                choice match
                    case Check =>
                        require(turnBets(userId) == highestBet)
                        
                        val updateCurrentPlayer = selectNextPlayer(activePlayer, (players.indexOf(currentPlayer)+1)%number)
                        val turnOver = updateCurrentPlayer == highestBetter

                        val nextPoolValue = if turnOver then poolValue + turnBets.values.sum else poolValue
                        val nextTurnBet = if turnOver then players.map(_ -> 0).toMap else turnBets
                        val nextCurrentPlayer = if turnOver then 
                            selectNextPlayer(activePlayer, players.indexOf(smallBlind)) else updateCurrentPlayer


                        val nextPhase = if turnOver then
                            if turn == 3 then Reveal else InGame(turn + 1)
                            else InGame(turn)
                        val updatedActivePlayer = if turnOver then players.map(_ -> false).toMap else activePlayer// in case end reveal, all not ready in order to set for the ready part
                        
                        val nextState = state.copy(poolValue = nextPoolValue, currentPlayer = updateCurrentPlayer, phase = nextPhase, turnBets = nextTurnBet, activePlayer = updatedActivePlayer)
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

                        // rajout du cas ou qqn aurait mit tapis et on passe au reveal direct
                        val nextPhase = if turnOver then 
                            if turn == 3 || !activePlayer.forall((k,v) => if v && nextPlayerBalance.getOrElse(k, 0) != 0 then true 
                                                                        else if v && nextPlayerBalance.getOrElse(k, 0) == 0 then false
                                                                        else true) then Reveal else InGame(turn + 1)
                            else InGame(turn)
                        val updatedActivePlayer = if turnOver then players.map(_ -> false).toMap else activePlayer// in case end reveal, all not ready in order to set for the ready part
                        
                        
                        val nextState = state.copy(playerBalance = nextPlayerBalance, poolValue = nextPoolValue, currentPlayer = nextCurrentPlayer, phase = nextPhase, turnBets = nextTurnBet)
                        Seq(Action.Render(nextState))

                    case Fold =>
                        val nextActivePlayer = activePlayer.updated(userId, false)
                        
                        val updateCurrentPlayer = selectNextPlayer(nextActivePlayer, (players.indexOf(currentPlayer)+1)%number)
                        val turnOver = updateCurrentPlayer == highestBetter

                        val nextPoolValue = if turnOver then poolValue + turnBets.values.sum else poolValue
                        val nextTurnBet = if turnOver then players.map(_ -> 0).toMap else turnBets
                        val nextCurrentPlayer = if turnOver then 
                            selectNextPlayer(activePlayer, players.indexOf(smallBlind)) else updateCurrentPlayer

                        // rajout du cas ou qqn aurait mit tapis et on passe au reveal direct
                        val nextPhase = if turnOver then 
                            if turn == 3 || !activePlayer.forall((k,v) => if v && playerBalance.getOrElse(k, 0) != 0 then true 
                                                                        else if v && playerBalance.getOrElse(k, 0) == 0 then false
                                                                        else true)then Reveal else InGame(turn + 1)
                                        else InGame(turn)
                        val updatedActivePlayer = if turnOver then players.map(_ -> false).toMap else activePlayer// in case end reveal, all not ready in order to set for the ready part

                        val nextState = state.copy(activePlayer = nextActivePlayer, poolValue = nextPoolValue, currentPlayer = nextCurrentPlayer, phase = nextPhase, turnBets = nextTurnBet)
                        Seq(Action.Render(nextState))

                    case Raise(value) =>
                        val diff = highestBet + value - turnBets(currentPlayer)
                        require(diff <= playerBalance(userId) && value <= maxRaise)
                        val nextPlayerBalance = playerBalance.updated(userId, playerBalance(userId) - diff)
                        val nextTurnBet = turnBets.updated(userId, turnBets(userId) + value)

                        val nextCurrentPlayer = selectNextPlayer(activePlayer, (players.indexOf(currentPlayer)+1)%number)

                        val nextState = state.copy(currentPlayer = nextCurrentPlayer, playerBalance = nextPlayerBalance, turnBets = nextTurnBet)

                        Seq(Action.Render(nextState))
            // guetter quand qqn est a tapis
            case (InGame(turn),_) => throw IllegalMoveException("You can only play in this phase of the game")

            case (Reveal, Ready) => 
                // on calcule le winner avant comme ca on peut savoir si le jeu est fini ou non
                // ensuite, on actualise les joueurs du prochain tour pour avoir une idée de qui reste en jeu
                // ca nous permet ensuite de tester si on a un gagnant ou non et surtout, quand on veut passer
                // au prochain round, on ne prend en compte le fait que les joueurs soient prêts seulement si ils font encore parti du jeu
                val winner = WinnerLogic.winner(playerCards, dealerCards) // détermine les Id du/des gagnants
                var updatedPlayerBalance = SortedMap(players.map(player => //map triée des montants des joueurs à la fin de ce round
                    if (winner.contains(player)) then player -> (playerBalance(player) + poolValue / winner.size)
                    else player -> playerBalance(player) ): _*)// à revoir a cause de la mauvaise verion de scala

                val restartActivePlayers =players.map(_ -> true).toMap.filter((k,v) => playerBalance(k) != 0)// on doit pouvoir faire plus simple mais ca marche

                if(restartActivePlayers.size == 1) then Seq(Action.Render(state.copy(phase = EndGame))) // cas ou on a un gagnant final
                else 
                    if activePlayer.forall((k,v) => v) then//ready means a player is ready to start a new round
                        // penser au fait que les joueurs qui ont perdus ne servent a rien dans le restart
                        val newHighestBetter = currentPlayer // on pose la référence au premier joueur jusqu'au prochain raise
                        val newPoolValue = 0
                        val newSmallBlind = currentPlayer // la nouvelle petite blinde
                        val newPlayers = restartActivePlayers.filter((k,v) => v).keys.toList//pour la prochaine distribution de cartes, on garde que les joueurs actifs pour distrib

                        val newAllCards = Random.shuffle(AllCards.apply) // nouveau melange des cartes
                        val newDealerCards = newAllCards.take(5) // nouvelles cartes du croupier, sur le model du init
                        val newRemainingCard = newAllCards.drop(5).toList // utile pour éviter d'utiliser les cartes du croupier
                        val newPlayerCards = restartActivePlayers.map((k,v) => (k, Hand(newRemainingCard(newPlayers.indexOf(k) * 2), newRemainingCard(newPlayers.indexOf(k*2+1))))).toMap //  distribution des mains adaptée aux joueurs restants
                        // la ligne au dessus est mauvaise, il faut prendre les cartes dans le bon ordre, sauf si on s'en fout
                        val newPhase = InGame(0)// on recommence un round si il reste plusieurs joueurs
                        // pas besoin de changer les turnbets parce ue c'est deja fait avant de passer à la phase reveal
                        val newGameState = state.copy(playerBalance = updatedPlayerBalance,
                                                        activePlayer = restartActivePlayers, 
                                                        highestBetter = newHighestBetter, 
                                                        poolValue = newPoolValue, 
                                                        smallBlind = newSmallBlind, 
                                                        dealerCards = newDealerCards, 
                                                        playerCards = newPlayerCards, 
                                                        phase = newPhase)
                        Seq(Action.Render(newGameState))

                    else
                        val newActivePlayer = activePlayer.updated(userId, true)
                        val newGameState = state.copy(activePlayer = newActivePlayer)
                        Seq(Action.Render(newGameState))
                                    
            case (Reveal, _) => throw IllegalMoveException("Compare your cards to the other's") //TODO change message 
            case (_) => throw IllegalMoveException("Unsupported phase") //TODO Implement if we want a endgame
            // faire le cas ou on a un tapis

    override def project(state: GameState)(userId: UserId): View = 
        val GameState(
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