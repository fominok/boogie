import scala.util.parsing.combinator.RegexParsers

class CoolParse(id: String, lastMsgs: Map[String, String]) extends RegexParsers {
  val learnPattern = """(^.*?)\s+(.*$)""".r
  def expr: Parser[AbstractCmd] = greeting ~> opt(""",\s*""".r) ~> (statement | diceShortcut)
  def greeting = """(?>[БбвВ]уги|[bBwW]oogie|б|b|\.)""".r
  def statement: Parser[AbstractCmd] = cmd ~ args ^^ {
    case ("yolo" | "йоло") ~ args => YoloCommand(id, args)
    case ("help" | "хелп") ~ _ => HelpCommand(id)
    case "запили" ~ args => FeatureCommand(id, args)
    case ("свич" | "switch") ~ args => SwitchCommand(id, args, lastMsgs(id))
    case ("учи" | "learn") ~ args => args match {
      case learnPattern(key, phrase) => LearnCommand(id, key, phrase)
      case _ => WrongCommand()
    }
    case x ~ _ => UserCommand(id, x)
  }
  def diceShortcut: Parser[AbstractCmd] = diceValue ~ "d" ~ diceValue ~ opt("""\s+""".r) ~ args ^^ {
    case left ~ _ ~ right ~ _ ~ args => DiceCommand(id, left, right, args)
  }
  def diceValue = """\d+""".r
  def cmd = """[а-яА-Яa-zA-Z]+""".r
  def args = """.*""".r
}
