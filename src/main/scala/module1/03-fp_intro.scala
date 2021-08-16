package module1

import java.util.UUID
import scala.annotation.tailrec
import java.time.Instant


/**
 * referential transparency
 */
object referential_transparency {


  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification {
    case class Email(email: String, text: Html) extends Notification

    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService {
    def sendNotification(notification: Notification): Unit

    def createNotification(abiturient: Abiturient): Notification
  }

  trait AbiturientService {

    def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient
  }

  class AbiturientServiceImpl(notificationService: NotificationService) extends AbiturientService {

    override def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient = {
      val abiturient = Abiturient(UUID.randomUUID().toString(), abiturientDTO.email, abiturientDTO.fio)
      notificationService.sendNotification(Notification.Email(abiturient.email, "Some message"))
      abiturient
    }

    def registerAbiturient2(uuid: UUID, abiturientDTO: AbiturientDTO): Abiturient = {
      val abiturient = Abiturient(uuid.toString(), abiturientDTO.email, abiturientDTO.fio)
      abiturient
    }

  }
}


// recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n) {
      _n *= i
      i += 1
    }
    _n
  }

  def factRec(n: Int): Int =
    if (n == 1) 1
    else n * factRec(n - 1)

  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(n: Int, accum: Int): Int =
      if (n == 1) accum
      else loop(n - 1, n * accum)

    loop(n, 1)
  }


  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */

  def fib(n: Int): Int = ???

}

object hof {

  def printFactorialResult(r: Int) = println(s"Factorial result is ${r}")

  def printFibonacciResult(r: Int) = println(s"Fibonacci result is ${r}")

  def printResult[T](r: T, funcName: String) = println(s"$funcName result is ${r}")

  def printRunningTimeFunc1[A, B](a: A)(f: A => B): Unit = {
    val current = Instant.now().toEpochMilli()
    f(a)
    val current2 = Instant.now().toEpochMilli()
    println(current2 - current)
  }


  // Follow type implementation
  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def sum(x: Int, y: Int): Int = x + y

  val r: Int => Int = partial(1, sum)

}


/**
 * Реализуем тип Option
 */


object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // Animal
  // Dog extend Animal
  // Option[Dog] Option[Animal]

  sealed trait Option[+T] {
    def isEmpty: Boolean = this match {
      case Option.Some(_) => false
      case Option.None => true
    }

    def get: T = this match {
      case Option.Some(v) => v
      case Option.None => throw new Exception("Get on empty Option")
    }

    def getOrElse[TT >: T](b: TT): TT = this match {
      case Option.Some(v) => v
      case Option.None => b
    }

    def map[B](f: T => B): Option[B] = this match {
      case Option.Some(v) => Option.Some(f(v))
      case Option.None => Option.None
    }

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Option.Some(v) => f(v)
      case Option.None => Option.None
    }

    def mapByFlatMap[B](f: T => B): Option[B] = flatMap[B](_ => this.flatMap(A => Option.Some(f(A))))

    def printIfAny: Unit = this match {
      case Option.Some(v) => println(v)
      case Option.None => println("Option is empty")
    }

    def zip[B](givenOption: Option[B]): Option[(T, B)] = (this, givenOption) match {
      case (Option.Some(a), Option.Some(b)) => Option.Some((a, b))
      case _ => Option.None
    }

    def filter(predicate: T => Boolean): Option[T] = this match {
      case Option.Some(v) => if (predicate(v)) Option.Some(v) else Option.None
      case Option.None => Option.None
    }

  }

  object Option {
    case class Some[+T](v: T) extends Option[T]

    case object None extends Option[Nothing]
  }

}

object List {
  sealed trait MyList[+T] {
    def ::[A >: T](givenElement: A): MyList[A] = MyList.Cons[A](givenElement, this)

    def mkString(separator: String = ","): String = this match {
      case MyList.Cons(head, MyList.Nil) => s"$head"
      case MyList.Cons(head, tail) => s"$head$separator" ++ tail.mkString(separator)
      case MyList.Nil => ""
    }

    def foldLeft[B](accum: B)(f: (B, T) => B): B = this match {
      case MyList.Cons(head, tail) => tail.foldLeft(f(accum, head))(f)
      case MyList.Nil => accum
    }

    def foldRight[B](accum: B)(f: (B, T) => B): B = this match {
      case MyList.Cons(head, tail) => f(tail.foldRight(accum)(f), head)
      case MyList.Nil => accum
    }

    def reverse: MyList[T] = this.foldLeft(MyList.Nil: MyList[T])((m, n) => n :: m)

    def map[B](f: T => B): MyList[B] = this match {
      case MyList.Cons(head, tail) => f(head) :: tail.map(f)
      case MyList.Nil => MyList.Nil
    }

    def tailMap[B](f: T => B): MyList[B] = {
      @tailrec
      def tailMapImplementation(f: T => B)(accum: MyList[B] = MyList.Nil, currentTail: MyList[T]): MyList[B] = currentTail match {
        case MyList.Cons(head, currentTail) => tailMapImplementation(f: T => B)(f(head) :: accum, currentTail)
        case MyList.Nil => accum.reverse
      }

      tailMapImplementation(f)(currentTail = this)
    }

    def filter(f: T => Boolean): MyList[T] = this match {
      case MyList.Cons(head, tail) if f(head) => head :: tail.filter(f)
      case MyList.Cons(_, tail) => tail.filter(f)
      case MyList.Nil => MyList.Nil
    }

    override def toString: String = {
      @tailrec
      def toStringWithoutParenthesis(accum: String = "", parsed: MyList[T]): String = parsed match {
        case MyList.Cons(head, MyList.Nil) => accum.reverse + head.toString
        case MyList.Cons(head, tail) => toStringWithoutParenthesis(" ," + head.toString + accum, tail)
        case MyList.Nil => ""
      }

      "(" + toStringWithoutParenthesis(parsed = this) + ")"

    }
  }

  object MyList {
    case class Cons[+T](head: T, tail: MyList[T]) extends MyList[T]

    case object Nil extends MyList[Nothing]

    def apply[T](args: T*): MyList[T] = args.foldRight(Nil: MyList[T])((m, n) => m :: n)

    def incList(listOfInts: MyList[Int]): MyList[Int] = listOfInts.map(_ + 1)

    def shoutString(listOfStrings: MyList[String]): MyList[String] = listOfStrings.map(_ ++ "!")

  }

}

