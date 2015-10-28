import scala.util.parsing.combinator.RegexParsers

class CoolParse(id: String, lastMsgs: Map[String, String]) extends RegexParsers {
  def expr: Parser[AbstractCmd] = greeting ~> opt(""",\s*""".r) ~> statement
  def greeting = """(?>[БбвВ]уги|[bBwW]oogie)""".r
  def statement = cmd ~ args ^^ {
    case "yolo" ~ args => YoloCommand(id, args)
    case "double" ~ args => DoubleCommand(id, args)
    case ("help" | "хелп") ~ _ => HelpCommand(id)
    case ("соси" | "пидр" | "говно" |"не очень") ~ _ => BadCommand(id)
    case "запили" ~ feature => FeatureCommand(id, feature)
    case "привет" ~ _ => GreetingCommand(id)
    case ("свич" | "switch") ~ text => SwitchCommand(id, text, lastMsgs(id))
    case ("ласт" | "last") ~ _ => LastMsgTest(id, lastMsgs(id))
    case _ ~ _ => NoCommand()
  }
  def cmd = """[а-яА-Яa-zA-Z]+""".r
  def args = """.*""".r
}
