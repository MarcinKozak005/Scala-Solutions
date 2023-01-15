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

    def P03ListContains01(list : List[Int], elem : Int) : Boolean = list.contains(elem)

    def P03ListContains02(list : List[Int], elem : Int) : Boolean = 
        list match {
            case Nil => false
            case x :: tail => (x == elem)
        }

    def P04ListOddPositions01(list: List[Int]) : List[Int] = {
        var result = List[Int]()
        for (i <- 0 until list.length){
            if (i%2 == 1) {
                result = result :+ list(i)
            }
        }
        result
    }

    def P04ListOddPositions02(list: List[Int]) : List[Int] = {
        def takeElem(list: List[Int]) : List[Int] = list match {
            case Nil => Nil
            case e :: tail => e +: dontTakeElem(tail)
        }
        def dontTakeElem(list: List[Int]) : List[Int] = list match {
            case Nil => Nil
            case e :: tail => takeElem(tail)
        }
        dontTakeElem(list)
    }

    def P05ListRunningTotal01(list: List[Int]) : Int = list.sum
    
    def P05ListRunningTotal02(list: List[Int]) : Int = list.foldLeft(0)(_+_)
    
    def P05ListRunningTotal03(list: List[Int]) : Int = list.reduce(_+_)

    def P07Sum01(list: List[Int]) : Int = {
        var result = 0
        for (i <- 0 until list.length) result += list(i)
        result
    }

    def P07Sum02(list: List[Int]) : Int = {
        var result = 0
        for (e <- list) result += e
        result
    }

    def P07Sum03(list: List[Int]) : Int = {
        var i = 0
        var result = 0
        while(i < list.length){
            result += list(i)
            i += 1
        }
        result
    }

    def P07Sum04(list: List[Int]) : Int = {
        def sumElem(list: List[Int]) : Int = {
            list match {
                case Nil => 0
                case x :: tail => x + sumElem(tail)
            }
        }
        sumElem(list)
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

    def P04ArrayOddPositions01(array: Array[Int]) = {
        var result = Array[Int]()
        for (i <- 0 until array.length){
            if (i%2 == 1){
                result = result :+ array(i) 
            }
        }
        result
    }

    def P05ArrayRunningTotal01(array: Array[Int]) : Int = {
        var result = 0
        for (e <- array) result += e
        result
    }

    def P05ArrayRunningTotal02(array: Array[Int]) : Int = {
        // In Java such construct does not work
        var result = 0
        array.foreach(result+=_)
        result
    }

    // Strings

    def P06IsPalindrome01(str: String): Boolean = str.equals(str.reverse)
    
    def P06IsPalindrome02(str: String): Boolean = {
        var result = true
        for (i <- 0 until str.length) {
            result = result && str(i) == str(str.length-1 -i) 
        }
        result
    }

    def main(args: Array[String]) : Unit = {
        println("Lists")
        val list = List(3, 2, 1, 32, 11, 23)
        println(f"P1 L 01: ${P01ListLargest01(list)}")
        println(f"P1 L 02: ${P01ListLargest02(list)}")
        println(f"P2 L 01: ${P02ListReverse01(list)}")
        println(f"P2 L 02: ${P02ListReverse02(list)}")
        println(f"P3 L 01: ${P03ListContains01(list,2)}")
        println(f"P3 L 02: ${P03ListContains02(list,2)}")
        println(f"P4 L 01: ${P04ListOddPositions01(list)}")
        println(f"P4 L 02: ${P04ListOddPositions02(list)}")
        println(f"P5 L 01: ${P05ListRunningTotal01(list)}")
        println(f"P5 L 02: ${P05ListRunningTotal02(list)}")
        println(f"P5 L 03: ${P05ListRunningTotal03(list)}")
        println(f"P7 L 01: ${P07Sum01(list)}")
        println(f"P7 L 02: ${P07Sum02(list)}")
        println(f"P7 L 03: ${P07Sum03(list)}")
        println(f"P7 L 04: ${P07Sum04(list)}")
        
        println("\nArrays")
        val array = Array(3, 2, 1, 32, 11, 23)
        println(f"P1 A 01: ${P01ArrayLargest01(array)}")
        println(f"P1 A 02: ${P01ArrayLargest02(array)}")
        println(f"P2 A 01: ${P02ArrayReverse01(array).mkString(" ")}")
        println(f"P2 A 02: ${P02ArrayReverse02(array).mkString(" ")}")
        println(f"P3 A 01: ${P03ArrayContains01(array,2)}")
        println(f"P3 A 02: ${P03ArrayContains02(array,2)}")
        println(f"P3 A 03: ${P03ArrayContains03(array,2)}")
        println(f"P4 A 01: ${P04ArrayOddPositions01(array).mkString(" ")}")
        println(f"P5 A 01: ${P05ArrayRunningTotal01(array)}")
        println(f"P5 A 02: ${P05ArrayRunningTotal02(array)}")

        println("\nStrings")
        println(f"P6 S 01: ${P06IsPalindrome01("eye")}")
        println(f"P6 S 02: ${P06IsPalindrome02("eye")}")
        
    }
}