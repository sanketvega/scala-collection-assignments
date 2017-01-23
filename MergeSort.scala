class MSort{

//case class(1eft: List[Int], right:List[Int])

def merge(left : List[Int], right : List[Int]):List[Int] = 
	(left, right) match {
		case(Nil, _) => right      					//when left list is empty and right list has elements
		case(_, Nil) => left						//when right list empty and left list has elements
		case(lefthead:: lefttail, righthead::righttail) =>				//when both the list has atleast one element each

			if(lefthead < righthead) lefthead:: merge(lefttail, right)  		
			else righthead :: merge (left, righttail)
	}



def sorting(lst : List[Int]) : List[Int] = lst match{
	case Nil => lst
	case h :: Nil => lst
	case _ =>
	    val (left,right) = lst.splitAt(lst.length/2)
	    merge(sorting(left), sorting(right))



}

}


object MergeSort extends App{

	val MSortobj = new MSort
	val sortedList = MSortobj. sorting(List(19,111,25,53,7,31,61,46))
	println(sortedList)

}
