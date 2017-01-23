//- find sum and multiplication of the list (dont use inbuilt methods)

class PrintTable(list:List[Int]) {

   def actionPerform(choice:String):Double={

	 choice match {                                   //matches with choices

                      case "+" => produceSum(list)
                      case "*" => produceProduct(list)
                      case _   => 0.0

		      } 


   }

   def produceSum(updatedList:List[Int]):Double={

          if(updatedList.isEmpty) 0
          else
          updatedList.head +  produceSum(updatedList.tail)     //recursion 
    }   

   def produceProduct(updatedList:List[Int]):Double={

          if(updatedList.isEmpty) 1
          else
          updatedList.head *  produceProduct(updatedList.tail) //recursion
    } 
   
}


object SumAndProduct extends App{

val preparedList = new  PrintTable(List(1,2,3,4,5,6)) // passing list as class parameter    
val sum = preparedList.actionPerform("+") // to perform addition
val product = preparedList.actionPerform("*") // to perform multiplication
println("\n\n---------------------------------------------------------------\n")
println(s"Sum of all the elements in the list provided is : $sum ")
println(s"Product of all the elements in list provided is : $product")
println("\n---------------------------------------------------------------\n\n")
}
