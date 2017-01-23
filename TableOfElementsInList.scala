//- print the table of each element in the List

class PrintTable(list:List[Int]) {


       list map(x => println(getTable(x)))
     
        def getTable(element:Int):String={

              (for{
                   iterator <- 1 to 10
                 }yield element*iterator).mkString(" ") //return yield as a string                       

        }
}


object TableOfElementsInList extends App{

new  PrintTable(List(1,2,3,4,5,6)) // passing list as class parameter
     

}
