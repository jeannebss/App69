package apps.poker

import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.{WebClientAppInstance}
import scalatags.JsDom.all._

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportTopLevel("app69")
object UI extends WSClientApp:

    def appId: String = "app69"
    
    def init(userId: UserId, sendMessage: ujson.Value => Unit, target: dom.Element): ClientAppInstance =
        Instance(userId, sendMessage, target)

class Instance(userId: UserId, sendMessage: ujson.Value => Unit, target: dom.Element)
  extends WebClientAppInstance[Event, View](userId, sendMessage, target):

  override val wire: AppWire[Event, View] = app69.Wire


override def css: String = super.css + """
  html {
    font-family: sans-serif;
    background: #f6f6f6;
  }
"""

override def render(userId: UserId, view: View): Frag = 
    ???