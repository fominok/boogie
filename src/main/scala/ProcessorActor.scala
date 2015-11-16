import akka.actor.{Props, Actor}
import org.mongodb.scala._
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.bson._
import org.mongodb.scala.Document
import scala.util.{Random, Try}


sealed abstract class AbstractCmd

case class HelpCommand(id: String) extends AbstractCmd

case class YoloCommand(id: String, msg: String) extends AbstractCmd

case class FeatureCommand(id: String, feature: String) extends AbstractCmd

case class SwitchCommand(id: String, msg: String, lastMsg: String) extends AbstractCmd

case class DiceCommand(id: String, left: String, right: String, names: String) extends AbstractCmd

case class LearnCommand(id: String, keyword: String, text: String) extends AbstractCmd

case class UserCommand(id: String, msg: String) extends AbstractCmd

case class WrongCommand() extends AbstractCmd

class ProcessorActor(val db: MongoDatabase) extends Actor {
  val apiActor = context.system.actorSelection("/user/apiActor")
  val collection: MongoCollection[Document] = db.getCollection("dick")

  override def receive: Receive = {
    case HelpCommand(id) => apiActor ! SendMessage(id,
      """
        |BoogieBot v4
        |Возможно, он уже полезен
        |
        |Формат обращения: буги/вуги/boogie/woogie/б/b/. %команда% [%аргументы%]
        |На данный момент доступные команды:
        |
        |learn/учи %слово% %текст% -- буги запоминает вариант ответа на %слово%(%обращение% %слово% вернет %текст%)
        |
        |switch/свич [%текст%] - перевести текст вида 'ghbdtn ehjl' в 'привет урод' или обратно,
        |---Если текст не задан, переведено будет последнее сообщение в чате(исключая команду)
        |help/хелп - вывести это сообщение
        |
        |запили %фича% - отправить реквест на новую фичу. Возможно, вас услышат
        |
        |Бросок дайса: %обращение% XdY -> бросок х костей с y гранями и вернуть сумму
        |---Выполнить комбинацию XdY можно несколько раз, присвоив каждой сумме имя: boogie 1d6 Шавуха Бк Лапша
        |
        |
        |Последние исправления(16.11.2015):
        |-Бота можно научить простым ответочкам
      """.stripMargin)
    case YoloCommand(id, args) => apiActor ! SendMessage(id, args.toUpperCase)
    case FeatureCommand(id, feature) =>
      apiActor ! SendMessage("2873432" , "Feature request: " + feature)
      apiActor ! SendMessage(id, "Запрос отправлен. Please wait patiently.")
    case SwitchCommand(id, args, lastMsg) => apiActor ! SendMessage(id,
      "Switched: " + jcukenSwitch(if (args.length == 0) lastMsg else args))
    case DiceCommand(id, l, r, names) => apiActor ! SendMessage(id,
      names.split(" ").foldLeft(""){(acc, x) => acc + x + ": " + roll(l, r) + "\n"}
    )
    case LearnCommand(id, kw, txt) => {
      collection.insertOne(Document("keyword" -> kw, "message" -> (">" + txt))).subscribe(new Observer[Completed] {
        override def onNext(result: Completed): Unit = println(s"onNext: $result")
        override def onError(e: Throwable): Unit = println(s"onError: $e")
        override def onComplete(): Unit = println("onComplete")
      })
      apiActor ! SendMessage(id, "Got it: " + kw)
    }
    case UserCommand(id, msg) => {
      collection.find(equal("keyword", msg)).collect().subscribe((results: Seq[Document]) => {
        results.size match {
          case x if x > 1 => {
            val rnd = new Random()
            apiActor ! SendMessage(id, results(rnd.nextInt(x))("message").asString().getValue)
          }
          case 1 => apiActor ! SendMessage(id, results.head("message").asString().getValue)
          case 0 =>
        }
      })
    }
    case _ =>
  }

  def jcukenSwitch(msg: String): String = {
    if (ProcessorActor.ruKeyboard.contains(msg(0))) {
      msg.foldLeft("": String)((acc, x) => {acc + ProcessorActor.ruEnMap.getOrElse(x, x)})
    } else {
      msg.foldLeft("": String)((acc, x) => {acc + ProcessorActor.enRuMap.getOrElse(x, x)})
    }
  }

  def roll(l: String, r: String): String = {
    val rnd = new Random()
    (1 to l.toInt map { _ => rnd.nextInt(r.toInt) + 1 } sum).toString
  }
}

object ProcessorActor {
  val ruKeyboard = """йцукенгшщзхъфывапролджэячсмитьбю.ЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ,"""
  val enKeyboard = """qwertyuiop[]asdfghjkl;'zxcvbnm,./QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM<>?"""
  val enRuMap = enKeyboard zip ruKeyboard toMap
  val ruEnMap = ruKeyboard zip enKeyboard toMap
}
