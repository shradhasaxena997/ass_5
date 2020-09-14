
case class Marks(subjectId: Int, studentId: Long, marksObtained: Float)
case class Student(id: Int, name: String)

object DataSet {

  //Students Details
  val s1 = Student(1, "ABCDE")
  val s2 = Student(2, "EFGHI")
  val s3 = Student(3, "JKLMN")
  val s4 = Student(4, "OPQRS")
  val s5 = Student(5, "TUVWX")

  //Marks Details Student s1
  val s1m1 = Marks(1, 1, 95)
  val s1m2 = Marks(2, 1, 95)
  val s1m3 = Marks(3, 1, 95)
  val s1m4 = Marks(4, 1, 95)
  val s1m5 = Marks(5, 1, 95)

  //Marks Details Student s2
  val s2m1 = Marks(1, 2, 97)
  val s2m2 = Marks(2, 2, 97)
  val s2m3 = Marks(3, 2, 97)
  val s2m4 = Marks(4, 2, 97)
  val s2m5 = Marks(5, 2, 97)

  //Marks Details Student s3
  val s3m1 = Marks(1, 3, 90)
  val s3m2 = Marks(2, 3, 80)
  val s3m3 = Marks(3, 3, 90)
  val s3m4 = Marks(4, 3, 95)
  val s3m5 = Marks(5, 3, 60)

  //Marks Details Student s4
  val s4m1 = Marks(1, 4, 80)
  val s4m2 = Marks(2, 4, 75)
  val s4m3 = Marks(3, 4, 70)
  val s4m4 = Marks(4, 4, 60)
  val s4m5 = Marks(5, 4, 70)

  //Marks Details Student s5
  val s5m1 = Marks(1, 5, 50)
  val s5m2 = Marks(2, 5, 80)
  val s5m3 = Marks(3, 5, 90)
  val s5m4 = Marks(4, 5, 70)
  val s5m5 = Marks(5, 5, 95)

  //Marks List
  val listOfs1 = List(s1m1, s1m2, s1m3, s1m4, s1m5)
  val listOfs2 = List(s2m1, s2m2, s2m3, s2m4, s2m5)
  val listOfs3 = List(s3m1, s3m2, s3m3, s3m4, s3m5)
  val listOfs4 = List(s4m1, s4m2, s4m3, s4m4, s4m5)
  val listOfs5 = List(s5m1, s5m2, s5m3, s5m4, s5m5)

  //Lists of Data's
  val studentList = List(DataSet.s1, DataSet.s2, DataSet.s3, DataSet.s4, DataSet.s5)
  val marksList = List(DataSet.listOfs1, DataSet.listOfs2, DataSet.listOfs3, DataSet.listOfs4, DataSet.listOfs5)
}



object Methods {

  def passFailChecker(subjectId: Int, criteria: Float, flag: String): String = {
    val marks = DataSet.marksList.flatMap(x => x.map(y => y))

    flag match {
      case "Pass" | "pass" => marks.filter(_.subjectId == subjectId)
        .partition(_.marksObtained >= criteria)._1.size.toString
      case "Fail" | "fail" => marks.filter(_.subjectId == subjectId)
        .partition(_.marksObtained < criteria)._1.size.toString
      case _ => "Please Provide the flag as Pass or Fail"
    }
  }

  def topperLoserChecker(subjectId: Int, count: Int, flag: String): Any = {
    val marks = DataSet.marksList.flatMap(x => x.map(y => y))

    flag match {
      case "top" | "Top" => {
        val toppers = marks.filter(_.subjectId == subjectId).sortBy(_.marksObtained).reverse.slice(0, count)
        //val toppersId = toppers.map(top => top.studentId)

        toppers.foreach(topper => {
          DataSet.studentList.foreach(student => if (topper.studentId == student.id)
            println(student.name + " " + topper.marksObtained)
          )
        })
      }

      case "bottom" | "Bottom" => {
        val losers = marks.filter(_.subjectId == subjectId).sortBy(_.marksObtained).slice(0, count)
        //val losersId = losers.map(loser => loser.studentId)
        losers.foreach(loser => {
          DataSet.studentList.foreach(student => if (loser.studentId == student.id)
            println(student.name + " " + loser.marksObtained)
          )
        })
      }

      case _ => {
        println("Please Provide the flag as Top or Bottom")
      }
    }
  }

  def toperLoserOverall(flag: String, count: Int): Any = {

    val marks = DataSet.marksList.flatMap(x => x.map(y => y))
    val result = average(marks)

    flag match {

      case "top" | "Top" => {
        val list = result.toList.sortBy(_._2).reverse.slice(0, count)
        list.foreach(lis => {
          DataSet.studentList.foreach(student => if (lis._1 == student.id)
            println(student.name + " " + lis._2)

          )
        })
      }

      case "bottom" | "Bottom" => {
        val list = result.toList.sortBy(_._2).slice(0, count)
        list.foreach(lis => {
          DataSet.studentList.foreach(student => if (lis._1 == student.id)
            println(student.name + " " + lis._2)
          )
        })
      }

      case _ => {
        "Please Provide the correct Parameters"
      }
    }
  }

  def schlorshipChecker(percentage: Float, goodScholar: Int, noScholar: Int): Any = {

    val marks = DataSet.marksList.flatMap(x => x.map(y => y))
    val result = average(marks)

    val partitionMap = result.partition(_._2 >= percentage)
    val scholorStudentsKeys = partitionMap._1.map(x => x._1).toList
    val noScholorStudentsKeys = partitionMap._2.map(x => x._1)

    DataSet.studentList.foreach(student => {
      if (scholorStudentsKeys.contains(student.id)) {
        println(student.name + " " + goodScholar.toString)
      }
      else {
        println(student.name + " " + noScholar.toString)
      }
    })
  }

  def passFailChecker(flag: String, percentage: Long): Any = {

    val marks = DataSet.marksList.flatMap(x => x.map(y => y))
    val result = average(marks)

    flag match {
      case "pass" | "Pass" => {
        val passStudents = result.filter(_._2 >= percentage).toList.sortBy(_._2).reverse
        passStudents.foreach(pass => {
          DataSet.studentList.foreach(student => {
            if (pass._1 == student.id)
              println(student.name + " " + pass._2)
          })
        })
      }

      case "fail" | "Fail" => {
        val failStudents = result.filter(_._2 < percentage).toList.sortBy(_._2)
        failStudents.foreach(fail => {
          DataSet.studentList.foreach(student => {
            if (fail._1 == student.id)
              println(student.name + " " + fail._2)
          })
        })
      }

      case _ => {
        println("Please provide the correct parameters")
      }
    }
  }

  def aboveNintyFive():Any={

    val marks = DataSet.marksList.flatMap(x => x.map(y => y))
    val result = average(marks).toList.filter(_._2>=95)
    result.foreach(res=>{
      DataSet.studentList.foreach(student=>{
        if(res._1==student.id)
          println(student.name + " " +res._2)
      })
    })
  }

  def reportCard():Any={
    val marks = DataSet.marksList.flatMap(x => x.map(y => y))
    val result = average(marks).toList.sortBy(_._1)

    DataSet.studentList.foreach(stud=>{
      print(stud.name+"   ")
      result.foreach(res=>{
        marks.foreach(mark=>{
          if(stud.id==res._1&&stud.id==mark.studentId)
            print(mark.marksObtained+"  ")
        })
        if(res._1 == stud.id)
        print(res._2)
      })
      println()
    })


  }

  private def average(list: List[Marks]): Map[Long, Float] = {

    var totalMarks: Float = 0
    var averageNumberOfStudents: scala.collection.mutable.Map[Long, Float] = scala.collection.mutable.Map[Long, Float]()

    DataSet.studentList.foreach(student => {
      list.groupBy(_.studentId == student.id)(true).foreach(mark => totalMarks += mark.marksObtained)
      averageNumberOfStudents = averageNumberOfStudents + ((student.id.toLong) -> (totalMarks.toFloat / 5))
      totalMarks = 0
    }
    )
    averageNumberOfStudents.toMap
  }
 def main(args:Array[String]){
    passFailChecker(scala.io.StdIn.readInt(),scala.io.StdIn.readInt(),scala.io.StdIn.readLine())
    topperLoserChecker(scala.io.StdIn.readInt(),scala.io.StdIn.readInt(),scala.io.StdIn.readLine())
     toperLoserOverall(scala.io.StdIn.readLine(),scala.io.StdIn.readInt())
     schlorshipChecker(scala.io.StdIn.readInt(),scala.io.StdIn.readInt(), scala.io.StdIn.readInt())
     passFailChecker(scala.io.StdIn.readLine(),scala.io.StdIn.readInt())
     aboveNintyFive()
     reportCard()
  }
 
}

