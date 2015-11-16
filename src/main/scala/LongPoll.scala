import akka.actor._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import Toolbox._
import org.mongodb.scala.MongoClient

import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}

case class LpServerData(key: String, server: String)

object LongPoll {
  implicit val formats = DefaultFormats

  def init(token: String): Unit = {
    val mdbClient = MongoClient("mongodb://127.0.0.1")

    val system = ActorSystem("system")
    val apiActor = system.actorOf(Props(classOf[ApiActor], token), "apiActor")
    val processorActor = system.actorOf(Props(classOf[ProcessorActor], mdbClient.getDatabase("test")), "procActor")
    longPollStart(token, processorActor)
  }

  @tailrec
  def longPollStart(token: String, processorActor: ActorRef): Unit = {
    Try[Unit] {
      val json = parse(timedGet("https://api.vk.com/method/messages.getLongPollServer").params(Map(
        "access_token" -> token,
        "use_ssl" -> "1"
      )).asString.body)
      println(pretty(render(json)))
      val key = (json \\ "key").extract[String]
      val server = (json \\ "server").extract[String]
      val ts = (json \\ "ts").extract[String]

      LongPoll(LpServerData(key, server), ts, processorActor, Map[String, String]())
    } match {
      case _ => longPollStart(token, processorActor)
    }
  }

  @tailrec
  private def apply(sd: LpServerData, ts: String, processorActor: ActorRef, lastMsgMap: Map[String, String]): Unit = {
    Try[(String, Map[String, String])] {
      val json = parse(timedGet(s"http://${sd.server}").params(Map(
        "act" -> "a_check",
        "key" -> sd.key,
        "ts" -> ts,
        "wait" -> "25",
        "mode" -> "2"
      )).asString.body)
      println(pretty(render(json)))
      val newTs = (json \\ "ts").extract[String]
      val updates: List[JArray] = (json \ "updates").extract[List[JArray]]
      val msgUpdates: List[JArray] = updates.filter(x => x(0).extract[Int] == 4).map(x => x.extract[JArray])

      for (incoming <- msgUpdates) {
        val id = incoming(3).extract[String]
        val msg = incoming(6).extract[String]
        val parser = new CoolParse(id, lastMsgMap)
        val cmd = parser.parseAll(parser.expr, msg).getOrElse(UserCommand)

        processorActor ! cmd
      }

      //TODO: test it
      val lastIds = msgUpdates.foldLeft(List[String]())((a,x) => a.::(x(3).extract[String]))
      val lastMsgs = msgUpdates.foldLeft(List[String]())((a,x) => a.::(x(6).extract[String]))

      (newTs, lastMsgMap ++ (lastIds zip lastMsgs toMap))
    } match {
      case Success((t, m)) => LongPoll(sd, t, processorActor, m)
      case Failure(_) =>
    }
  }
}
