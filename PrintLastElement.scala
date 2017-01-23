//Find the last element of list with its index value(dont use inbuilt methods to extract last element directly)

class ExtractLastWithIndex(list:List[Int]) {


     val lastIndex   = list.size-1
     val lastElement = list(lastIndex)

     println(s"last Index   of list is : $lastIndex")  
     println(s"last element of list is : $lastElement")      
}


object PrintLastElement extends App{

new  ExtractLastWithIndex(List(1,2,3,4,5,6)) // passing list as class parameter
     

}
