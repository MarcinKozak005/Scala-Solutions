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

    // on the website it's called: on_all
    def P08ListOnAll01(list: List[Int], fun: Int => Int) : List[Int] = list.map(fun)

    def P09ListConcatenate01(list1: List[Any], list2: List[Any]) : List[Any] = list1 ::: list2

    def P10ListAlternatingMerge01(list1 : List[Any], list2: List[Any]) : List[Any] = {
        def takeFromSecond(list1 : List[Any], list2: List[Any]) : List[Any] = list2 match {
            case Nil => Nil
            case x::tail => x::takeFromFirst(list1,tail)
        }
        def takeFromFirst(list1 : List[Any], list2: List[Any]) : List[Any] = list1 match {
            case Nil => Nil
            case x::tail => x::takeFromSecond(tail, list2)
        }
        takeFromFirst(list1, list2)
    }

    def P10ListAlternatingMerge02(list1 : List[Any], list2: List[Any]) : List[Any] = list1 zip list2 flatten {case (a,b) => List(a,b)}

    def P11ListMergeSorted01(list1: List[Int], list2: List[Int]) : List[Int] = {
        if (list1 == Nil) {
            list2
        } else if (list2 == Nil) {
            list1
        } else {
            if (list1(0) <= list2(0)) {
                list1(0) +: P11ListMergeSorted01(list1.slice(1,list1.length), list2)   
            } else {
                list2(0) +: P11ListMergeSorted01(list1, list2.slice(1,list2.length))
            } 
        }
    }

    def P12ListRotate01(list: List[Any], k: Int) : List[Any] = {
        val shift = k % list.length
        shift match {
            case 0 => list
            case x if x > 0 => P12ListRotate01(list.drop(1) :+ list(0),x-1) 
        }
    }

    def P12ListRotate02(list: List[Any], k: Int) : List[Any] = list.drop(k) ::: list.take(k)

    def P13ListFibonacci01() : List[Int] = {
        def extendFibonacci(l: List[Int], n: Int) : List[Int] = {
            if (n == 0){
                l
            }
            else 
            {
                extendFibonacci(l :+ (l(l.length-1) + l(l.length-2)), n-1)
            }
            
        }
        extendFibonacci(List(1,1),98)
    }

    def P14ListSplitNumber(number: Int) : List[Int] = number.toString.map(x => x.toInt - 48).toList

    def P15List(list1: List[Int], list2: List[Int], f: (Int,Int) => Int) : List[Int] = {
        P14ListSplitNumber(
            f(
                list1.map(_.toString).mkString.toInt,
                list2.map(_.toString).mkString.toInt
            )
        )
    }

    def P16ListChangeBase(list: List[Int], base1: Int, base2: Int) : List[Int] = {
        P14ListSplitNumber(
            java.lang.Long.toString(
                java.lang.Long.parseLong(list.map(_.toString).mkString,base1), 
                base2
            ).mkString.toInt
        )
    }

    def P17ListSelectionSort(list: List[Int]):List[Int] = list match {
        case Nil => Nil
        case _ => {
            val minimal = list.min
            val indexOfMinimal = list.indexOf(minimal)
            minimal +: P17ListSelectionSort(list.slice(0,indexOfMinimal) ::: list.slice(indexOfMinimal+1, list.length))
        }
    }

    def P17ListInsertionSort(list: List[Int]): List[Int] = {
        def insertionSortHelper(list: List[Int]):List[Int] = {
            val pos = list.length - 1
            var i = list.indexWhere(elem => elem>list(pos))
            if(i>=0) {
                (list.slice(0,i) :+ list(pos)) ::: list.slice(i,pos) ::: list.slice(pos+1, list.length)
            } else list
        }
        var result = list
        for(i <- 0 to list.length){
            result =  insertionSortHelper(result.slice(0,i+1)) ::: result.slice(i+1,result.length)
        }
        result
    }

    def P18ListBinarySearch(list: List[Int], elem: Int): Boolean = {
        def P18ListBinarySearchHelper(list: List[Int], elem: Int, start: Int, end: Int): Boolean = start + (end-start) / 2 match {
                case middle if list(middle) > elem && end-start!=0 => P18ListBinarySearchHelper(list, elem, start, middle-1)
                case middle if list(middle) < elem && end-start!=0 => P18ListBinarySearchHelper(list, elem, middle+1, end)
                case middle if list(middle) == elem => true
                case _ => false
        }
        P18ListBinarySearchHelper(list, elem, 0, list.length-1)
    }

    def P19ListStringsInFrame(list: List[String]): Unit = {
        val maxLength = list.map(s => s.length).max
        println("*"*(maxLength + 4))
        list.foreach(s => println(f"* ${s}"+ (" "*(maxLength-s.length)) +" *"))
        println("*"*(maxLength + 4))
    }

    // Capital letters cases (names, sentence beginnings) skipped
    def P20ListPigLatin(text: String): String = {
        text.split(" ").map(word => word.substring(1,word.length).concat(word(0).toLower.toString).concat("ay")).mkString(" ")
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

    // on the website it's called: on_all
    def P08ArrayOnAll01(array: Array[Int], fun: Int => Int) : Array[Int] = {
        val result = new Array[Int](array.length)
        for (i <- 0 until array.length) {
            result(i) = fun(array(i))
        }
        result
    }

    def P09ArrayConcatenate01(array1: Array[Any], array2: Array[Any]) : Array[Any] = array1 ++ array2

    def P10ArrayAlternatingMerge01(array1: Array[Any], array2: Array[Any]) : Array[Any] = {
        val result = new Array[Any](array1.length + array2.length)
        for (i <- 0 until array1.length + array2.length){
            if (i % 2 == 0){
                result(i) = array1(i / 2)
            }
            else{
                result(i) = array2((i-1) / 2)
            }
        }
        result
    }

    def P11ArrayMergeSorted01(array1: Array[Int], array2: Array[Int]) : Array[Int] = {
        val result = new Array[Int](array1.length + array2.length)
        var i = 0
        var j = 0
        while (i < array1.length && j < array2.length){
            if(array1(i) <= array2(j)){
                result(i+j) = array1(i)
                i+=1
            } else {
                result(i+j) = array2(j)
                j+=1
            }
        }
        while(i < array1.length){
            result(i+j) = array1(i)
            i+=1
        }
        while(j < array2.length){
            result(i+j) = array2(j)
            j+=1
        }
        result
    }

    def P12ArrayRotate01(array: Array[Int], k: Int) : Array[Int] = {
        val shift = k % array.length
        Array.concat(array.slice(shift,array.length), array.slice(0,shift))
    }

    def P14ArraySplitNumber(number: Int) : Array[Int] = number.toString.map(x => x.toString.toInt).toArray

    def P15Array(array1: Array[Int], array2: Array[Int], f: (Int,Int) => Int) : Array[Int] = {
        P14ArraySplitNumber(f(array1.map(_.toString).mkString.toInt, array2.map(_.toString).mkString.toInt))
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

    // Other

    def P13OtherFibonacci01() : Unit = {
        print(f"1 1")
        var x_1 = 1
        var x_2 = 1
        for (i <- 3 to 101){
            print(f" ${x_2 + x_1}")
            x_1 += x_2
            x_2 = x_1 - x_2
        }
    }

    def main(args: Array[String]) : Unit = {
        println("Lists")
        val list = List(3, 2, 1, 32, 11, 23)
        println(f"P01 L 01: ${P01ListLargest01(list)}")
        println(f"P01 L 02: ${P01ListLargest02(list)}")
        println(f"P02 L 01: ${P02ListReverse01(list)}")
        println(f"P02 L 02: ${P02ListReverse02(list)}")
        println(f"P03 L 01: ${P03ListContains01(list,2)}")
        println(f"P03 L 02: ${P03ListContains02(list,2)}")
        println(f"P04 L 01: ${P04ListOddPositions01(list)}")
        println(f"P04 L 02: ${P04ListOddPositions02(list)}")
        println(f"P05 L 01: ${P05ListRunningTotal01(list)}")
        println(f"P05 L 02: ${P05ListRunningTotal02(list)}")
        println(f"P05 L 03: ${P05ListRunningTotal03(list)}")
        println(f"P07 L 01: ${P07Sum01(list)}")
        println(f"P07 L 02: ${P07Sum02(list)}")
        println(f"P07 L 03: ${P07Sum03(list)}")
        println(f"P07 L 04: ${P07Sum04(list)}")
        println(f"P08 L 01: ${P08ListOnAll01(List.range(1,21), x => x*x)}")
        println(f"P09 L 01: ${P09ListConcatenate01(List('a','b','c'),List(1,2,3))}")
        println(f"P10 L 01: ${P10ListAlternatingMerge01(List('a','b','c'),List(1,2,3))}")
        println(f"P10 L 02: ${P10ListAlternatingMerge02(List('a','b','c'),List(1,2,3))}")
        println(f"P11 L 01: ${P11ListMergeSorted01(List(1,4,6),List(2,3,5))}")
        println(f"P12 L 01: ${P12ListRotate01(list,2)}")
        println(f"P12 L 02: ${P12ListRotate02(list,2)}")
        println(f"P13 L 01: ${P13ListFibonacci01()}")
        println(f"P14 L 01: ${P14ListSplitNumber(2342)}")
        println(f"P15 L Part01: ${P15List(List(3,2,1),List(1,2,3), _+_)}")
        println(f"P15 L Part02: ${P15List(List(3,2,1),List(1,2,3), _-_)}")
        println(f"P15 L Part03: ${P15List(List(3,2,1),List(1,2,3), _*_)}")
        println(f"P16 L 01: ${P16ListChangeBase(List(1,2,3),10,5)}")
        println(f"P17 L SelectionSort: ${P17ListSelectionSort(List(5,3,2,4,1))}")
        println(f"P17 L InsertionSort: ${P17ListInsertionSort(list)}")
        println(f"P18 L 01: ${P18ListBinarySearch(list.sorted, 100)}")
        println(f"P19 L StringsInFrame: "); P19ListStringsInFrame(List("Hello", "World", "in", "a", "frame"))
        println(f"P20 L Pig Latin: ${P20ListPigLatin("The quick brown fox")}")

        println("\nArrays")
        val array = Array(3, 2, 1, 32, 11, 23)
        println(f"P01 A 01: ${P01ArrayLargest01(array)}")
        println(f"P01 A 02: ${P01ArrayLargest02(array)}")
        println(f"P02 A 01: ${P02ArrayReverse01(array).mkString(" ")}")
        println(f"P02 A 02: ${P02ArrayReverse02(array).mkString(" ")}")
        println(f"P03 A 01: ${P03ArrayContains01(array,2)}")
        println(f"P03 A 02: ${P03ArrayContains02(array,2)}")
        println(f"P03 A 03: ${P03ArrayContains03(array,2)}")
        println(f"P04 A 01: ${P04ArrayOddPositions01(array).mkString(" ")}")
        println(f"P05 A 01: ${P05ArrayRunningTotal01(array)}")
        println(f"P05 A 02: ${P05ArrayRunningTotal02(array)}")
        println(f"P08 A 01: ${P08ArrayOnAll01(List.range(1,21).toArray, x => x*x).mkString(" ")}")
        println(f"P09 A 01: ${P09ArrayConcatenate01(Array('a','b','c'),Array(1,2,3)).mkString(" ")}")
        println(f"P10 A 01: ${P10ArrayAlternatingMerge01(Array('a','b','c'),Array(1,2,3)).mkString(" ")}")
        println(f"P11 A 01: ${P11ArrayMergeSorted01(Array(1,4,6),Array(2,3,5)).mkString(" ")}")
        println(f"P12 A 01: ${P12ArrayRotate01(array,2).mkString(" ")}")
        println(f"P14 A 01: ${P14ArraySplitNumber(2342).mkString(" ")}")

        println("\nStrings")
        println(f"P06 S 01: ${P06IsPalindrome01("eye")}")
        println(f"P06 S 02: ${P06IsPalindrome02("eye")}")

        println("\nOther")
        println(f"P13 O 01: "); P13OtherFibonacci01() // Int overflow
        println(f"P15 A Part01: ${P15Array(Array(3,2,1),Array(1,2,3), _+_).mkString}")
        println(f"P15 A Part02: ${P15Array(Array(3,2,1),Array(1,2,3), _-_).mkString}")
        println(f"P15 A Part03: ${P15Array(Array(3,2,1),Array(1,2,3), _*_).mkString}")
    }
}