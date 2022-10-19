package hashtablesandhashfunctions

import java.util.Scanner

object PhoneBook {

  // Uses direct addressing: so we create an array with the size
  // of the maximum possible phone numbers
  val phoneBook = new Array[String](10000000)

  def main(args: Array[String]) = {

    val s = new Scanner(System.in)
    val queryCount = s.nextInt()

    for (_ <- 0 until queryCount) {

      val query = s.next()
      query match {
        case "add" => add(s.nextInt(), s.next())
        case "del" => delete(s.nextInt())
        case "find" => find(s.nextInt())
      }
    }

  }

  def delete(number: Int): Unit =
    phoneBook(number) = null

  def add(number: Int, name: String): Unit =
    phoneBook(number) = name

  def find(number: Int): Unit =
    if (phoneBook(number) == null) println("not found")
    else println(phoneBook(number))

}
