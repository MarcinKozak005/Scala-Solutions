object P02_ListsStrings {

    // Lists

    def P01ListLargest01(list : List[Int]) : Int = list.max

    def P01ListLargest02(list : List[Int]) : Int = {
        var maxElem = list(0)
        for (e <- list) {
            maxElem = if (e > maxElem) e else maxElem
        }
        maxElem
    }

    def P02ListReverse01(list : List[Int]) : List[Int] = list.reverse

    def P02ListReverse02(list : List[Int]) : List[Int] = 
        list match {
            case Nil => Nil
            case x :: tail => P02ListReverse02(tail) :+ x
        }

    def P03Contains01(list : List[Int], elem : Int) : Boolean = list.contains(elem)

    def P03Contains02(list : List[Int], elem : Int) : Boolean = 
        list match {
            case Nil => false
            case x :: tail => (x == elem)
        }

    // Arrays

    def P01ArrayLargest01(array : Array[Int]) : Int = array.max
    
    def P01ArrayLargest02(array : Array[Int]) : Int = {
        var maxElem = array(0)
        for (e <- array){
            maxElem = if (e > maxElem) e else maxElem
        }
        maxElem
    }

    def P02ArrayReverse01(array : Array[Int]) : Array[Int] = array.reverse

    def P02ArrayReverse02(array : Array[Int]) : Array[Int] = {
        var tmp : Int = 0;
        for (i <- 0 until (array.length+1)/2 ) {
            tmp = array(i)
            array(i) = array((array.length-1) - i)
            array((array.length-1) - i) = tmp
        }
        array
    }

    def P03ArrayContains01(array : Array[Int], elem : Int) : Boolean = array.contains(elem)

    def P03ArrayContains02(array : Array[Int], elem : Int) : Boolean = {
        var result = false
        for (e <- array) result = result || (e==elem)
        result
    }

    def P03ArrayContains03(array : Array[Int], elem : Int) : Boolean = {
        array.filter(_==elem).length != 0
    }

    def main(args: Array[String]) : Unit = {
        println("Lists")
        val list = List(3, 2, 1, 32, 11, 23)
        println(f"P1 L 01: ${P01ListLargest01(list)}")
        println(f"P1 L 02: ${P01ListLargest02(list)}")
        println(f"P2 L 01: ${P02ListReverse01(list)}")
        println(f"P2 L 02: ${P02ListReverse02(list)}")
        println(f"P3 L 01: ${P03Contains01(list,2)}")
        println(f"P3 L 02: ${P03Contains02(list,2)}")
        
        println("\nArrays")
        var array = Array(3, 2, 1, 32, 11, 23)
        println(f"P1 A 01: ${P01ArrayLargest01(array)}")
        println(f"P1 A 02: ${P01ArrayLargest02(array)}")
        println(f"P2 A 01: ${P02ArrayReverse01(array).mkString(" ")}")
        println(f"P2 A 02: ${P02ArrayReverse02(array).mkString(" ")}")
        println(f"P3 A 01: ${P03ArrayContains01(array,2)}")
        println(f"P3 A 02: ${P03ArrayContains02(array,2)}")
        println(f"P3 A 03: ${P03ArrayContains03(array,2)}")
        
    }
}