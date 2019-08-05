package JSON
import JSON._
import scala.collection.mutable

object JSmain {
  def main(args:Array[String])={
    val John = JObject(Map("firstName"->JString("John"),
                           "lastName"->JString("Smith"),
                           "sex"->JString("male"),
                           "age"->JNumber(25),
                           "address"->JObject(Map("streetAdress"->JString("21 2nd Street"),
                                                  "city"->JString("NYC"))),
                           "phone"->JArray(        mutable.IndexedSeq(JObject(Map("type"->JString("Home"), "Number"->JNumber(75845455))),
                             JObject(Map("type"->JString("Office"), "Number"->JNumber(12345678))))       )))
    val x = JSON.toString(John)

    val JS = "{\"Name\":\"haha\", \"JSON\":{\"Name\":\"haha\", \"Nanba\":3334, \"Newtype\":{\"NUmber\":21, \"Naism\":123, \"inf\":{\"kanade\":342}, \"hellow\":255}, \"asf\":\"asdf\"}, \"num\":123}"

    val test = "{\"bad idea\":\"ggo\"}"
    val array = "{\"array\":[\"John\", \"haha\", 12, {\"name\":\"Kanade\", \"set\": [\"aa\", \"bb\", 54]}]}"


    val test3 = "{\"items\":[{\"it1\":\"1\",\"it2\":\"2\"}, {\"it3\":\"3\", \"it4\":\"4\"}]}"

    val test2 = " {\"N1\": \"1\",\"N2\":\"2\", \"N3\": 3,  \"N4\": {  \"N5\": \"5\",\n  \"N7\": \"8\",\n  \"state\": \"Paschimbanga\",\n  \"postalCode\": \"713102\" }, \"f\":[{ \"type\": \"personal\",\"number\": \"09832209761\"},{ \"type\": \"fax\",\n  \"number\": \"91-342-2567692\"}]}"

    val example = JSON.toJObject(test2)

    val Another = "{\n  \"name\":\"John\",\n  \"age\":30,\n  \"cars\": [{ \"name\":\"Ford\", \"models\":[ \"Fiesta\", \"Focus\", \"Mustang\"]},\n    { \"name\":\"BMW\", \"models\":[ \"320\", \"X3\", \"X5\"]},\n    { \"name\":\"Fiat\", \"models\":[ \"500\", \"Panda\"]}]}"

    val test4 = "{\"glossary\": {\"title\": \"example glossary\",\n\t\t\"GlossDiv\": {\"title\": \"S\",\"GlossList\": {\"GlossEntry\": {\"ID\": \"SGML\",\"SortAs\": \"SGML\",\"GlossTerm\": \"Standard Generalized Markup Language\",\"Acronym\": \"SGML\",\"Abbrev\": \"ISO 8879:1986\",\"GlossDef\": {\"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",\"GlossSeeAlso\": [\"GML\", \"XML\"]},\"GlossSee\": \"markup\"}}}}}"
    //println(example)
    //println(JSON.toString(example))
    //println(JSON.toJObject(Another))
    println(JSON.toString(JSON.toJObject(test4)))
    //println(JSON.toString(JSON.toJObject(Another)))
  }
}
