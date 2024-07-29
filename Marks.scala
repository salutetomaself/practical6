import scala.io.StdIn.readLine
import scala.util.Try

object MarksApp extends App {

  // Function to validate input
  def validateInput(name: String, marks: String, totalMarks: String): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty"))
    } else if (!Try(marks.toInt).isSuccess || !Try(totalMarks.toInt).isSuccess) {
      (false, Some("Marks and total possible marks must be integers"))
    } else {
      val marksInt = marks.toInt
      val totalMarksInt = totalMarks.toInt
      if (marksInt < 0 || totalMarksInt <= 0) {
        (false, Some("Marks must be positive and total possible marks must be greater than zero"))
      } else if (marksInt > totalMarksInt) {
        (false, Some("Marks cannot exceed total possible marks"))
      } else {
        (true, None)
      }
    }
  }

  // Function to get student info with retry
  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var isValid = false
    var studentInfo: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'D')

    while (!isValid) {
      println("Enter student's name:")
      val name = readLine()

      println("Enter marks obtained:")
      val marks = readLine()

      println("Enter total possible marks:")
      val totalMarks = readLine()

      val validation = validateInput(name, marks, totalMarks)
      isValid = validation._1

      if (!isValid) {
        println(s"Invalid input: ${validation._2.getOrElse("Unknown error")}")
      } else {
        val marksInt = marks.toInt
        val totalMarksInt = totalMarks.toInt
        val percentage = (marksInt.toDouble / totalMarksInt) * 100
        val grade = percentage match {
          case p if p >= 90 => 'A'
          case p if p >= 75 => 'B'
          case p if p >= 50 => 'C'
          case _ => 'D'
        }
        studentInfo = (name, marksInt, totalMarksInt, percentage, grade)
      }
    }
    studentInfo
  }

  // Function to print student record
  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Student Name: $name")
    println(s"Marks Obtained: $marks")
    println(s"Total Possible Marks: $totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
  }

  // Main function to demonstrate the functionality
  val studentRecord = getStudentInfoWithRetry()
  printStudentRecord(studentRecord)
}
