package apps

package app69

import cs214.webapp.*
import cs214.webapp.client.*
import scalatags.JsDom.all
import scalatags.JsDom.all.*

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}
import cs214.webapp.client.graphics.WebClientAppInstance

import Phase.*
import PhaseView.* 
import Choice.*
import CardView.*
import CardSymbols.back

@JSExportTopLevel("app69_html")
object HtmlUI extends WSClientApp:
    
    def appId: String = "app69"
    
    def uiId: UIId = "html"

    def init(userId: UserId, sendMessage: ujson.Value => Unit, target: Target): ClientAppInstance =
        HtmlUIInstance(userId, sendMessage, target)

class HtmlUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target)
    extends WebClientAppInstance[Event, View](userId, sendMessage, target):

    override val wire = Wire

    override def render(userId: UserId, view: View): Frag =
        frag(
            head(
                link(
                    href := "https://fonts.googleapis.com/css2?family=Inknut+Antiqua&display=swap",
                    rel := "stylesheet"
                )
            ),
            renderView(userId, view)
        )

    def renderView(userId: UserId, view: View): Frag =
        frag(
            renderPhase(userId, view.phaseView),
            renderScores(userId, view.scoresView, view),
            renderCards(userId, view.cardView, view.playerNames)
        )

    def renderPhase(userId: UserId, phaseView: PhaseView): Frag = phaseView match
        case ChoiceSelection =>

        case NotPlaying =>

        case ChoiceMade(choice) =>

        case Winner =>

    def renderPlayer(userId: UserId, playerId: Int, balance: Balance, hand: Hand, active: Boolean): Frag = frag(
        if active then 
            div(cls := "player", id := s"player${playerId}")(
                div(cls := "player-name")(userId),
                render
            )
    )

    def renderScores(userId: UserId, scoresView: ScoresView, players: Vector[UserId]): Frag = 
        val ScoresView(playerScores, poolBalance) = scoresView
        frag(
            div(cls := "player", id := "player0")(
                
                div(cls := "balance")("Balance :" )
            )
    )

    
    def renderCards(userId: UserId, cardView: CardView, players: Vector[UserId]): Frag = cardView match
        case InGameCards(playerCards, dealerCards) => frag(
            renderHand(playerCards, true),
            for user <- players do
                renderHand(_, false),
            renderDealerCards(dealerCards)
        )

        case RevealCards(playerCards, dealerCards) => frag(
            for user <- playerCards.keys do
                renderHand(playerCards(user), true),
            renderDealerCards(dealerCards)
        )

    def renderHand(hand: Hand, front: Boolean): Frag =
        if front then frag(
            div(cls := "cards")(CardSymbols(hand.first) + CardSymbols(hand.second))
        ) else frag(
            div(cls := "cards")(CardSymbols.back * 2)
        )
    
    def renderDealerCards(dealerCards: Vector[Card]): Frag =
        div(cls := "deck")(
            span(cls := "cards-on-table")(CardSymbols.back * (5 - dealerCards.size)),
            span(cls := "turned-cards")(dealerCards.map(CardSymbols.apply(_)).mkString + ()),
        )

    override def css: String = super.css + 
        """
            html{
                text-align: center;
                font-family: 'Inknut Antiqua', serif;
                color: black;
                background: #f6f6f6;
            }
            .player > div {
                line-height: 1.2; 
            }
            body{
                background-color: #e1e1e1;
                margin:1em;
                height:100%;
            }
            .all-table {
                position: absolute;  
                top: 50%;            
                left: 50%;           
                transform: translate(-50%, -50%); 
                display: grid;
                place-items: center;
                width: 50%;
                height: 60%;
                color: white;
                background-color: #bababa;
                border-radius: 50%;
            }
            .center-table{
                position: absolute;
                top:6%;
                display: grid;
                place-items: center;
                line-height:2;
                width: 92%;
                height: 88%;
                background-color: #4c654d;
                border-radius: 50%;
            }
            .deck{
                display:flex;
                flex-direction:column;
                font-size:4.5em;
                line-height:120%;
            }
            .player {
                position: absolute;
                display: flex;
                flex-direction: column;
            }
            .cards {
                font-size:3em;
                margin-bottom: 5%;
            }
            .pot {
                position: absolute;
                top: 50%;
                left: 10%;
                transform: translateY(-50%); 
                font-size: 2em;
                padding: 0.5em;
            }
            #player1 {
                top:30%;
                left:10%;
            }
            #player2 {
                top:5%;
                left:75%;
            }
            #player0 {
                top:85%;
                left:45%;
            }
            #player3 {
                top:60%;
                left:5%	
            }
            #player4 {
                top:50%;
                left:85%;
            }

            .controls {
                position: absolute;
                bottom: 1%;   
                display: flex;
                flex-direction: row;
                align-items: flex-end;  
                width: 60%;
                gap: 2%;
            }

            button {
                padding: 1px 20px;
                background-color: white;
                border: none;
                border-radius: 15px;
                cursor: pointer;
                font-family: 'Inknut Antiqua', serif;
            }

            #continue {
                background-color:black;
                color:white;
                padding: 5px 25px;
            }
            #raise {
                background-color: #4ba652;
                cursor: default;
            }
            #check {
                background-color: #8e8e8e;
            }
            #call {
                background-color: #954d3f;
            }
            #fold {
                background-color:#f4625c;
            }
        """
