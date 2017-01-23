// FULLY IMMUTABLE CODE (no var, no while/do while , no break/continue , and every function has return type other then Unit)


/* PROBLEM STATEMENT

Every Student has some marks associated with it. Student details contains its id and name.
And for Marks, there are subjectId, studentId and number of marks a student scored.

Following are the requirements which is required to gain from above scenario (i.e. Student and marks)

1)
Input:- (subjectId, percentage, pass/fail)
Output:- for input pass, evaluate that how much students(id, name) are passed in the inputted subjectId
for input fail, evaluate that how much students(id, name) are failed in the inputted subjectId
Note:- percentage is the input which defines the minimum passing criteria
e.g. 
Pass count: 15
Fail count: 10

2)
Input:- (subjectId, count, top/bottom)
Output:- based on the last input(top/bottom), output the students details who have scored max/min in that subjectId
e.g. 
input: 1 5 top
output: 
Kunal 85
Himanshu 84
Geetika 83
Anmol 82
Mahesh 81

3)
Input:-
(top/bottom, count)
OutPut:-
Overall top/least scorer based on all the subjects score, fetch students name
count- input defines that how much students name are to be printed on console
e.g.
input: top 2

output:
Himanshu 75%
Geetika 74%


4)
Input:-
(percentage, good_scholarship, normal_or_no_scholarship)
Output:- two groups of students with the amount of scholarship
e.g.
input: 85% 2000 500
output: 
Kunal 2000
Himanshu 500
Geetika 2000
Mahesh 500

5)
Input:-
(pass/fail, percentage)
count and print the number of students and all names who are passed/fail,
Pass or fail would be decided by percentage input field.
e.g.
input: fail 30
output: 
Kunal 28%
Himanshu 29%

6) Find the student(s) who have scored 95% or above and print its details.
input: 95%
output:
Kunal 95%
Himanshu 96%
Geetika 97%

7) For every student, find its marks in detail (just like detailed Report card of a student.)
Note:- must use groupBy method of List
input: reportcard
output:
Kunal 75 70 80 75 75%
Himanshu 74 70 81 75 75%
Geetika 70 70 85 75 75%
*/

case class Student(id: Long, name:String)
case class Marks(subjectid: Int, studentid : Long, marksobtained: Int)
class ResultCount(sL : List[Student], mL : List[Marks]){
 

	 def Count(id:Long,cent:Int,res:String):Int = {  
	 
	     val intermediate  =  mL map(x => getId(x,id,cent))
	     val passList = intermediate filter{_>0}
	     val  failList = intermediate filter{_<0}
             println(s"\n\n______ Count of Students $res in subject id: $id ________")
	     if(res == "pass") passList.size
	     else failList.size

	 }

	 def getId(m : Marks,id:Long,cent:Int):Long = {

	  if(m.subjectid==id && m.marksobtained > cent)m.studentid
	  else if(m.subjectid==id && m.marksobtained < cent) -1
	  else 0   
	    
	  }


	 def subjectMeritList(id:Long,till:Int,res:String):List[String] = {  
	 
	     val intermediate  =  mL map(x => ProcessMerit(x,id))
	     val intermediate2 = intermediate.filter{_ != Marks(0,0,0)}
	     val mysorted =intermediate2.sortWith(_.marksobtained < _.marksobtained) 
	     if(till <= intermediate2.size){

		           val generatedMerit  =  mysorted map( x =>

				  (for {
				        iterator <- 0 to sL.size-1
				        
				  }yield if(sL(iterator).id == x.studentid) sL(iterator).name +" "+x.marksobtained  else "" ).toList


			 )

		      val merit = generatedMerit.flatten.filter{_!=""}.reverse 
		      println(s"\n______ Merit List of subject id $id _____________________")
                       processRequest(merit,res,till)
		     }else{
			 List("Error: trying to access more strudents then available in merit list")
		      }

		     

	 }

	 def ProcessMerit(m : Marks,id:Long):Marks=if(m.subjectid==id)m else Marks(0,0,0)

	 def overallMeritList(res:String,till:Int):List[String] = { 

		 val resultNamePair= sL map(x => 

                 x.name -> calculatePercentage((for {
				     iterator <- 0 to mL.size-1         
		                     }yield if(mL(iterator).studentid == x.id) mL(iterator).marksobtained else -1).toList.filter{_ != -1}))
		 
	         val sorted = resultNamePair.sortBy(_._2) 
        
                 val meritIncreasingOrder = sorted map(x => x._1+" "+x._2+"%")
                 val merit = meritIncreasingOrder.reverse
                 if(till <= merit.size){
		 println(s"\n______ Merit List of all subjects _____________________")
                  processRequest(merit,res,till)
                 }
                 else{
			 List("Error: trying to access more strudents then available in merit list")
		     }

	 }

         def calculatePercentage(list:List[Int]):Double = { 
 
             val total:Double = list.foldLeft(0)(_ + _) 
             (total*100)/500
         }

         def processRequest(list:List[String],option:String,to:Int):List[String] = { 
 
		 list map(println(_))
		 if(option == "top") s"Top $to students are ---------->" :: list.dropRight(list.size - to) 
                 else if(option=="bottom")s"Bottom $to students are ---------->" :: list.reverse.dropRight(list.size - to) 
                 else List("not a valid operation")
         }

         def scholarship(criteria:Double,goodScholarship:Int,normalScholarship:Int):List[String] = {


		 val scholarshipNamePair= sL map(x => 
                 // invoking function calculateScholarship taking 1st argument from for comprehension and rest from this function
                 x.name -> calculateScholarship((for {
				     iterator <- 0 to mL.size-1         
		                     }yield if(mL(iterator).studentid == x.id) mL(iterator).marksobtained else -1).toList.filter{
                                                                            _ != -1},criteria,goodScholarship,normalScholarship))

                val scholarshipResult = scholarshipNamePair map(x => x._1+" "+x._2)
		println(s"\n_____________ Scholarship results _____________________\n")
                scholarshipResult
        }

         def calculateScholarship(list:List[Int],criteria:Double,goodScholarship:Int,normalScholarship:Int):Int = { 
 
             val total:Double = list.foldLeft(0)(_ + _) 
             val percentage = (total*100)/500
             if(percentage >= criteria )goodScholarship else normalScholarship

         }

         def countResult(option:String,criteria:Int):List[String]  = { 

                 println(s"\n_____ Count of Student $option with Percentage scored ____\n")
 		 val resultNamePair= sL map(x => 

                 x.name -> calculatePercentage((for {
				     iterator <- 0 to mL.size-1         
		                     }yield if(mL(iterator).studentid == x.id) mL(iterator).marksobtained else -1).toList.filter{_ != -1}))
		         
                 val result = resultNamePair map(x => 
                option  match   {
                       case "pass"=> if(x._2>=criteria) x._1+" "+x._2+"%" else ""
                       case "fail"=> if(x._2<criteria) x._1+" "+x._2+"%"  else ""
                       case _=>"Invalid choice"
                       })
                   

                 result.filter{ _ != ""}.filter{ _ != "Invalid choice"}

         }

         def scoredAbove(criteria:Int):List[String]  = { 

                 println(s"\n__________  Student(s) who scored above $criteria % __________\n")
 		 val resultNamePair= sL map(x => 

                 x.name -> calculatePercentage((for {
				     iterator <- 0 to mL.size-1         
		                     }yield if(mL(iterator).studentid == x.id) mL(iterator).marksobtained else -1).toList.filter{_ != -1}))
		         
                 val result = resultNamePair map(x =>  if(x._2>=criteria) x._1+" "+x._2+"%" else "")
                   
                   

                 result.filter{ _ != ""}.filter{ _ != "Invalid choice"}

         }
        def getReportCard(option:String):List[String]  = { 
    
                 if(option == "Reportcard"){
                 println(s"\n__ Reportcard, groupBy name starting with 'p' on Top __\n")
 		 val resultNamePair= sL map(x => 

                x.name -> generateReport((for {
				     iterator <- 0 to mL.size-1         
		                     }yield if(mL(iterator).studentid == x.id) mL(iterator).marksobtained else -1).toList.filter{_ != -1}))

                 /*****************************************
                 *                                        *
                 *      GROUPBY  name starts with 'P'     *
                 *                                        *
                 ******************************************/
                 val nameStartingWithCharP = resultNamePair.groupBy( _._1.charAt(0) == 'p')

	
                  val top= nameStartingWithCharP(true) map(x => getString(x))
                  val bottom= nameStartingWithCharP(false) map(x => getString(x))
                  top ::: bottom 
                 }
                 else{

                  List("Error: Invalid option passed as argument")
                 }


         }

         def generateReport(list:List[Int]):List[Any] = { 
 
             val total:Double = list.foldLeft(0)(_ + _) 
             list:+"   "+(total*100)/500+"%"
  
         }

         def getString(x:(String, List[Any])):String ={

               x._1 + " " + (for{
                          iterator <- 0 to x._2.size-1

                   }yield x._2(iterator)).mkString(" ")
          }

}
object OverallResult extends App{
 
val studentList = List(Student(1,"sonu"),Student(2,"prashant"),Student(3,"pranjut"),Student(4,"bhavya"),Student(5,"Ankit"),Student(6,"Shubhra"),Student(7,"Anmol"),Student(8,"Shivangi"),Student(9,"Mahesh"),Student(10,"Ashish"))
val marksList = List(Marks(1,1,95),Marks(2,1,96),Marks(3,1,98),Marks(4,1,96),Marks(5,1,96),Marks(1,2,56),Marks(2,2,10),Marks(3,2,20),Marks(4,2,0),Marks(5,2,53),Marks(1,3,56),Marks(2,3,34),Marks(3,3,45),Marks(4,3,32),Marks(5,3,92),Marks(1,4,44),Marks(2,4,23),Marks(3,4,55),Marks(4,4,77),Marks(5,4,44),Marks(1,5,90),Marks(2,5,96),Marks(3,5,91),Marks(4,5,78),Marks(5,5,49),Marks(1,6,76),Marks(2,6,89),Marks(3,6,25),Marks(4,6,78),Marks(5,6,90),Marks(1,7,46),Marks(2,7,39),Marks(3,7,45),Marks(4,7,72),Marks(5,7,92),Marks(1,8,54),Marks(2,8,56),Marks(3,8,55),Marks(4,8,77),Marks(5,8,44),Marks(1,9,66),Marks(2,9,34),Marks(3,9,55),Marks(4,9,62),Marks(5,9,12),Marks(1,10,48),Marks(2,10,43),Marks(3,10,55),Marks(4,10,77),Marks(5,10,44))
 
 val student = new ResultCount(studentList,marksList)
 val count = student.Count(5, 50, "fail")// Count of Students fail in subject id: 5
 println("\n\n")
 println(s"Count: $count")

 val merit_in_subject = student.subjectMeritList(2, 2, "top")
  merit_in_subject map(println(_))//Top 2 students in meritlist of subjects id 2
  println("_______________________________________________________\n") 
 
 val overallMerit = student.overallMeritList("top",2) // Top 2 students in meritlist of all subjects
  overallMerit map(println(_))
  println("_______________________________________________________\n")
 

 val scholarshipList = student.scholarship(50,2000,500) // Scholarship results good(above 50%) -> 2000 , normal -> 500
 scholarshipList map(println(_))
 println("_______________________________________________________\n")

 val Result = student.countResult("fail",50) //Count of Student fail with Percentage scored below 50
 val total = Result.size
 println(s"Count : $total")
 println("------------------>\n")
 Result map(println(_))
 println("_______________________________________________________\n")

 val detail = student.scoredAbove(95)//students scored above given %
 detail map(println(_))
  println("_______________________________________________________\n")

 val Report = student.getReportCard("Reportcard")//("Reportcard") ordered by students whose name starts with 'p' at top
 Report map(println(_))
 println("_______________________________________________________\n")

}
