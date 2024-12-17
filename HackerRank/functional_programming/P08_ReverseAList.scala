def f(arr:List[Int]):List[Int] = {
    if (arr.length == 0) List()
    else f(arr.tail) :+ arr.head
}