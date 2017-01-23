/*aggregate the contents of two lists of same size into a single list
List(1,2) and List("a", "b") results List(List(1, "a"), List(2, "b"))*/

class AggregateIt(list1:List[Int],list2:List[String]) {

 

           val aggregate =   for{
                          iterator <- 0 to list1.size-1
                          }yield list1(iterator)+" "+list2(iterator)  

            aggregate.toList map(println( _ ))                       

     
}


object AggregateList extends App{

new  AggregateIt(List(1,2,3,4,5,6),List("a","b","c","d","e","f")) // passing lists as class parameter
     

}
