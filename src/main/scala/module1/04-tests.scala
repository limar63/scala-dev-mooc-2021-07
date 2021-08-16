package module1

import module1.opt.Option
import module1.List.MyList

object OptionTests {
  def startTesting: Unit = {
    //Test of isEmpty
    assert(!Option.Some(5).isEmpty)
    assert(Option.None.isEmpty)
    //test of get
    assert(Option.Some(5).get == 5)
    var exceptionCheck = false
    try {
      Option.None.get
      exceptionCheck = true
    }
    catch {
      case _: Throwable =>
    } finally {
      if (exceptionCheck) throw new java.lang.AssertionError("assertion failed")
    }
    //test of getOrElse
    assert(Option.Some(5).getOrElse("Empty option") == 5)
    assert(Option.None.getOrElse("Empty option") == "Empty option")

    //Как-то даже и не знаю как ассертить метод который юнит возвращает

    //Test of zip
    assert(Option.Some(5).zip(Option.Some(10)) == Option.Some((5, 10)))
    assert(Option.Some(5).zip(Option.None) == Option.None)
    assert(Option.None.zip(Option.Some(10)) == Option.None)
    //test of filter
    assert(Option.Some(5).filter(_ == 5) == Option.Some(5))
    assert(Option.Some(5).filter(_ == 4) == Option.None)
    assert(Option.None.filter(_ == 5) == Option.None)
    println("Testing of the data type \"Option\" is completed successfully")
  }
}

object ListTesting {
  def startTesting: Unit = {
    //test of ::
    assert((5 :: MyList.Nil) == MyList.Cons(5, MyList.Nil))
    assert((10 :: MyList.Cons(5, MyList.Nil)) ==
      MyList.Cons(10, MyList.Cons(5, MyList.Nil)))
    //test of apply() constructor. After this point, apply will be used for asserted lists
    assert(MyList(0, 1, 2, 3, 4, 5) == MyList.Cons(0,
      MyList.Cons(1, MyList.Cons(2, MyList.Cons(3,
        MyList.Cons(4, MyList.Cons(5, MyList.Nil)))))))
    assert(MyList() == MyList.Nil)
    //test of mkString
    assert(MyList(0, 1, 2, 3, 4, 5).mkString() == "0,1,2,3,4,5")
    assert(MyList(0, 1, 2, 3, 4, 5).mkString(" * ") == "0 * 1 * 2 * 3 * 4 * 5")
    assert(MyList().mkString() == "")
    //test of foldLeft
    assert(MyList(0, 1, 2, 3, 4, 5).foldLeft(0)(_ + _) == 15)
    assert(MyList[Int]().foldLeft(0: Int)(_ + _) == 0)
    assert(MyList(0, 1, 2, 3, 4, 5).foldLeft(MyList.Nil: MyList[Int])(
      (m, n) => n :: m) == MyList(5, 4, 3, 2, 1, 0))
    //test of foldRight
    assert(MyList(0, 1, 2, 3, 4, 5).foldRight(0)(_ + _) == 15)
    assert(MyList[Int]().foldRight(0: Int)(_ + _) == 0)
    assert(MyList(0, 1, 2, 3, 4, 5).foldRight(MyList.Nil: MyList[Int])(
      (m, n) => n :: m) == MyList(0, 1, 2, 3, 4, 5))
    //test of reverse
    assert(MyList(0, 1, 2, 3, 4, 5).reverse == MyList(5, 4, 3, 2, 1, 0))
    assert(MyList().reverse == MyList())
    //test of map
    assert(MyList(0, 1, 2, 3, 4, 5).map(_ + 2) == MyList(2, 3, 4, 5, 6, 7))
    assert(MyList[Int]().map(_ + 2) == MyList())
    assert(MyList(0, 1, 2, 3, 4, 5).map(a => a.toString) == MyList("0", "1", "2", "3", "4", "5"))
    //test of tailMap
    assert(MyList(0, 1, 2, 3, 4, 5).tailMap(_ + 2) == MyList(2, 3, 4, 5, 6, 7))
    assert(MyList[Int]().tailMap(_ + 2) == MyList())
    assert(MyList(0, 1, 2, 3, 4, 5).tailMap(a => a.toString) == MyList("0", "1", "2", "3", "4", "5"))
    //test of filter
    assert(MyList(0, 1, 2, 3, 4, 5).filter(_ > 3) == MyList(4, 5))
    assert(MyList(0, 1, 2, 3, 4, 5).filter(_ > 5) == MyList())
    assert(MyList[Int]().filter(_ < 5) == MyList())
    //test of incList
    assert(MyList.incList(MyList(0, 1, 2, 3, 4, 5)) == MyList(1, 2, 3, 4, 5, 6))
    assert(MyList.incList(MyList[Int]()) == MyList())
    //test of shoutString
    assert(MyList.shoutString(MyList("a", "b", "c", "d", "e", "f")) ==
      MyList("a!", "b!", "c!", "d!", "e!", "f!"))
    assert(MyList.shoutString(MyList[String]()) == MyList())
    assert(MyList(0, 1, 2, 3, 4, 5).toString == "(0, 1, 2, 3, 4, 5)")
    //test of toString
    assert(MyList().toString == "()")
    println("Testing of the data type \"List\" is completed successfully")
  }
}