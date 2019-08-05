package JSON

import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.annotation.tailrec

sealed trait JState
case object readName extends JState
case object readJObj extends JState
case object readString extends JState
case object readArray extends JState
case object readJNum extends JState
case object readColon extends JState
case object readQuoforName extends JState
case object readQuoforJObj extends JState



trait JSON
object JSON{
  case object JNull extends JSON // not implemented
  case class JNumber(get: Double) extends JSON
  case class JString(get:String) extends JSON
  case class JBool(get: Boolean)extends JSON // not implemented
  case class JArray(get: mutable.IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  /**
    *
    * @param get the structured JSON objected to print out
    * @param space the number of spaces to add in front for the sake of typesetting with default(as initial) value ""
    * @return JSON object as a string
    */
  def toString(get:JSON, space:String = ""):String = get match {
    case JNull => "Null"
    case JNumber(x) => x.toString+","
    case JBool(x) => x.toString+","
    case JString(x) => "\""+x+"\"" +","
    case JArray(arr) => {
      var s:String = "\n" + space + "[\n"
      for(jo <- arr) s = s.concat(space + toString(jo, space + " ") + "\n")
      s = s.concat(space + "]")
      s
    }
    case JObject(fm) => {
      var s:String = "\n" +  space + "{\n"
      fm.foreach(p=>{s = s.concat(" "+ space + "\""+p._1+"\"" + ":" + toString(p._2, space + " ") + "\n")})
      s = s.concat(space + "},")
      s
    }
  }

  /****
    * Not all json objects are considered(JSNULL and JSBOOL)
    * But the framework is already set up
    * @param s the unstructured json object
    * @return structured json object
    */

  def toJObject(s:String):JSON = {

    def map(input:(JState, Int, String, JSON, JSON)):(JSON, Int)
    =
      input match {
        case (state: readName.type, index, input, x, g) => {
          if (s(index) == '\"') {
            g match{
              case JObject(o)=>map(readColon, index + 1, input, x, g)
              case JArray(arr)=>map(readArray, index + 1, "", x, g)
            }
          } else { // space is the other case
            map(state, index + 1, input + s(index), x, g)
          }
        }
        case (state: readColon.type, index, input, x, g) => {
          if (s(index) == ':') {
            map(readQuoforJObj, index + 1, input, x, g)
          } else { // space is the other case.
            map(state, index + 1, input, x, g)
          }
        }
        case (state: readQuoforName.type, index, input, x, g) => s(index) match {
          case '\"' => map(readName, index + 1, input, JString(""), g)
          case '}'=>g match{
            case JObject(g)=>(JObject(g ++ Map(input->x)), index)
            case JArray(arr)=>(JArray(arr ++ mutable.IndexedSeq(x)), index)
          }
          case _=> map(state, index + 1, input, x, g)

        }
        case (state: readQuoforJObj.type, index, input, x, g) => s(index) match {
          case '\"' => {map(readString, index + 1, input, JString(""), g)}
          case '{' => {
            val tup = map(readQuoforName, index + 1, "", JNull, JObject(Map()))
            val nextIndex: Int = tup._2 + 1
            val subJObj = tup._1
            if(nextIndex < s.length) {
              g match {
                case JObject(t) => {
                  map(readJObj, nextIndex, input, subJObj, g)
                }
                case JArray(arr) => map(readArray, nextIndex, "", subJObj, g)
              }
            }else{
              tup
            }
          }
          case '[' => {
            val tup = map(readArray, index + 1, "", JNull, JArray(mutable.IndexedSeq()))
            val nextIndex: Int = tup._2
            val subObject = tup._1
            g match {
              case JObject(t) => map(readJObj, nextIndex, input, tup._1, g)
              case JArray(arr) => map(readArray, nextIndex, "", JNull, JArray(arr ++ mutable.IndexedSeq(JObject(Map(input -> subObject)))))
            }
          }
          case c => {
            if (c.isDigit) map(readJNum, index, input, JNumber(0), g)
            else map(state, index + 1, input, x ,g )
          }
        }
        // issue:need to consider the case when this object is stored in a JArray.
        case (state: readJObj.type, index, input, x, JObject(g)) => s(index) match {
          // push back and return
          case '}' => (JObject(g ++ Map(input -> x)), index)
          case ',' => map(readQuoforName, index + 1, "", JNull, JObject(g ++ Map(input -> x)))
          case _ => map(state, index + 1, input, x, JObject(g))
        }
        case (state: readJObj.type, index, input, x, JArray(arr)) => s(index) match {
          case ',' => map(readArray, index, "", JNull, JArray(arr))
          case _=>map(readArray, index, input, x, JArray(arr))
        }
        case (state: readString.type, index, input, JString(x), g) => s(index) match {
          case '\"' => {
            g match{
              case JObject(o)=>map(readJObj, index + 1, input, JString(x), g)
              case JArray(arr)=>map(readArray, index + 1, input, JString(x), g)
            }
          }
          case c => map(state, index + 1, input, JString(x + c), g)
        }
        case (state: readJNum.type, index, input, JNumber(n), g) => {
          if (s(index).isDigit) map(state, index + 1, input, JNumber(10 * n + s(index).toString.toInt), g)
          else g match {
            case JObject(o)=>map(readJObj, index, input, JNumber(n), g)
            case JArray(arr)=>map(readArray, index, input, JNumber(n), g)
          }
        }
        case (state: readArray.type, index, input, x, JArray(g)) =>s(index) match {
          case ']' => (JArray(g ++ mutable.IndexedSeq(x)), index)
          case ',' => map(readArray, index + 1, "", JNull, JArray(g ++ mutable.IndexedSeq(x)))
          case '\"' => map(readString, index + 1, "", JString(""), JArray(g))
          case '{' => map(readQuoforJObj, index, "", JObject(Map()), JArray(g))
          case x => {
            if (x.isDigit) map(readJNum, index, "", JNumber(0), JArray(g))
            else map(state, index + 1, "", JNumber(x), JArray(g))
          }
        }
        case (a, index, c, d, e) => {
          map(a, index + 1, c, d, e)
        }
      }
    map(readQuoforJObj, 0, "", JNull,JObject(Map()))._1
  }
}



