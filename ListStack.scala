//- implement Stack and Queue using Lists

class Createstack{// class having defenitions for Stack operations

 def push(element:Int,previous:List[Int]):List[Int]= previous :+element
 
 def pop(previous:List[Int]):List[Int]={(for{iterator <- 0 to previous.size-2}yield previous(iterator)).toList}

 def Top(previous:List[Int]):Int=previous.head
 
 def IsEmpty(previous:List[Int]):Boolean=previous.isEmpty
}

class CreateQueue{ // class having defenitions for queue operations

 def enqueue(element:Int,previous:List[Int]):List[Int]= previous :+element
 
 def dequeue(previous:List[Int]):List[Int]=previous.tail 

 def peek(previous:List[Int]):Int=previous.head
 
 def IsEmpty(previous:List[Int]):Boolean=previous.isEmpty
}


object ListStack extends App{

  val mystack = new Createstack
  val emptystack = List[Int]()
  val res1 = mystack.push(10,emptystack)
  println("\n\n_________________ Stack Operations ________________")
  println("\nstack push 10")
  println(s"Current Stack : $res1\n")
  println("stack push 20")
  val res2 = mystack.push(20,res1)
  println(s"Current Stack : $res2\n")
  println("stack push 30")
  val res3 = mystack.push(30,res2)
  println(s"Current Stack : $res3\n")
  println("stack pop")
  val res4 = mystack.pop(res3) 
  println(s"Current Stack : $res4\n")
  println("top element of the Stack")
  val res5 = mystack.Top(res4) 
  println(s"Current Stack Top : $res5\n")
  println("Is this Stack empty??")
  val res6 = mystack.IsEmpty(res4) 
  println(s"Status : $res6")
  println("\n___________________________________________________\n")

  println("_________________ Queue Operations ________________")

  val myQueue = new CreateQueue
  val emptyQueue = List[Int]()
  val res7 = myQueue.enqueue(30,emptystack)
  println("\nenqueue 30")
  println(s"Current Queue : $res7\n")
  println("enqueue 50")
  val res8 = myQueue.enqueue(40,res7)
  println(s"Current Queue : $res8\n")
  println("enqueue 60")
  val res9 = myQueue.enqueue(50,res8)
  println(s"Current Queue : $res9\n")
  println("dequeue")
  val res10 = myQueue.dequeue(res9) 
  println(s"Current Queue : $res10\n")
  println("peek element of the Queue")
  val res11 = myQueue.peek(res10) 
  println(s"Current Queue Peek: $res11\n")
  println("Is this Queue empty??")
  val res12 = myQueue.IsEmpty(res10) 
  println(s"Status : $res12")
  println("\n___________________________________________________\n\n")

}
