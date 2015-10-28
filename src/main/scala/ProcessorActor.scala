import akka.actor.{Props, Actor}
import akka.actor.Actor.Receive
import Toolbox._
import com.sun.xml.internal.ws.resources.SenderMessages
import scala.util.Try
import scala.collection.immutable.StringOps


sealed abstract class AbstractCmd

case class DoubleCommand(id: String, msg: String) extends AbstractCmd

case class HelpCommand(id: String) extends AbstractCmd

case class YoloCommand(id: String, msg: String) extends AbstractCmd

case class BadCommand(id: String) extends AbstractCmd

case class FeatureCommand(id: String, feature: String) extends AbstractCmd

case class GreetingCommand(id: String) extends AbstractCmd

case class SwitchCommand(id: String, msg: String, lastMsg: String) extends AbstractCmd

case class LastMsgTest(id: String, msg: String) extends AbstractCmd

case class NoCommand() extends AbstractCmd

class ProcessorActor extends Actor {
  val apiActor = context.system.actorSelection("/user/apiActor")

  override def receive: Receive = {
    case DoubleCommand(id, args) =>
      val doubled: Double = Try(args.toDouble).toOption.flatMap(x => Option(x * 2)).getOrElse(0)
      apiActor ! SendMessage(id, s"Doubled $args: " + doubled.toString)
    case LastMsgTest(id, msg) => apiActor ! SendMessage(id, "Last: " + msg)
    case HelpCommand(id) => apiActor ! SendMessage(id,
      """
        |B-Bot v1.4.8.8.1.4
        |Когда-нибудь он будет полезен
        |Формат обращения: буги/вуги/boogie/woogie %команда% [%аргументы%]
        |На данный момент доступные команды:
        |switch/свич [%текст%] - перевести текст вида 'ghbdtn ehjl' в 'привет урод',
        |---Если текст не задан, переведено будет последнее сообщение в чате(исключая команду)
        |double %число% - удвоить заданное число(я не знаю, зачем)
        |help/хелп - вывести это сообщение
        |запили %фича% - отправить реквест на новую фичу. Возможно, вас услышат
        |
        |Последние исправления(27.10.2015):
        |-Работа с последними сообшениями(дополнена команда Switch)
      """.stripMargin)
    case YoloCommand(id, args) => apiActor ! SendMessage(id, args.toUpperCase)
    case BadCommand(id) => apiActor ! SendMessage(id, "Солнышко ты мое рваное, а не пойти ли тебе сменить фамилию")
    case FeatureCommand(id, feature) =>
      apiActor ! SendMessage("2873432" , "Feature request: " + feature)
      apiActor ! SendMessage(id, "Запрос отправлен. Please wait patiently.")
    case GreetingCommand(id) => apiActor ! SendMessage(id, "Привет!")
    case SwitchCommand(id, args, lastMsg) => apiActor ! SendMessage(id,
      "Switched: " + jcukenSwitch(if (args.length == 0) lastMsg else args))
    case _ =>
  }

  def jcukenSwitch(msg: String): String = {
    val lol = msg.foldLeft("": String)((acc, x) => {acc + ProcessorActor.keyMap.getOrElse(x, x)})
    println(lol)
    lol
  }
}

object ProcessorActor {
  val ruKeyboard = """йцукенгшщзхъфывапролджэячсмитьбю.ЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ,"""
  val enKeyboard = """qwertyuiop[]asdfghjkl;'zxcvbnm,./QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM<>?"""
  val keyMap = enKeyboard zip ruKeyboard toMap
}
