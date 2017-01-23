class Quick{
	def sorting(lst: List[Int]): List[Int]= lst match{
		
		case Nil => Nil
		case head :: Nil => lst
		case _ =>
			val pivot = lst.head
			val (before, after) = lst.tail.partition(_ < pivot)
			sorting(before) ++ (pivot :: sorting(after))

	}

}

object QuickSort extends App{

	val Quickobj = new Quick
	val sortedList = Quickobj. sorting(List(19,111,25,53,7,31,61,46))
	println(sortedList)

}
