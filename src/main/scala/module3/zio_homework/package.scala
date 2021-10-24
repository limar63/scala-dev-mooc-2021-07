package module3

import pureconfig.error.ConfigReaderException
import zioConcurrency.printEffectRunningTime
import zio.{ Has, RIO, Task, URIO, ZIO, ZLayer, clock}
import zio.clock.Clock
import zio.console._
import zio.duration.durationInt
import zio.random._
import java.util.concurrent.TimeUnit
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */




  type GuessGameEnv = Random with Console

  def didHeGuessed(guess: Int, rand: Int): Task[String] = {
    if (guess == rand) ZIO.succeed("You got it right, buddy!")
    else ZIO.succeed("Nah, it was " + rand + ", better luck next time")
  }

  lazy val guessProgram: RIO[GuessGameEnv, Unit] = for {
    console <- ZIO.environment[Console].map(_.get)
    _ <- console.putStrLn("Bahai chislo ot 1 do 3-h")
    guess <- console.getStrLn.map(_.toInt)
    random <- ZIO.environment[Random].map(_.get)
    numb <- random.nextIntBetween(1, 4)
    result <- didHeGuessed(guess, numb)
  } yield println(result)

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R, E, A](eff: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] =
    eff.flatMap(x => if (condition(x)) doWhile(eff)(condition) else eff)
/*
  var i: Int = 20
  lazy val testLoop: RIO[Any, Unit] = for {
    _ <- doWhile[Any, Throwable, Int](ZIO.effect({println("Test"); i = i - 1; i}))(_ > 5)
  } yield ()
*/
  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault: Task[config.AppConfig] = config.load.catchSome {
    case _ : ConfigReaderException[config.AppConfig] => Task(config.AppConfig("Default appname", "Default.url"))
  }

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайным образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: URIO[Clock with Random, Int] = for {
    random <- ZIO.environment[Random].map(_.get)
    _ <- ZIO.sleep(1 seconds)
    number <- random.nextIntBetween(0, 11)
  } yield number


  /**
   * 4.2 Создайте коллекцию из 10 выше описанных эффектов (eff)
   */
  def replicateMList[R, A](n: Int)(effect: URIO[R, A]): URIO[R, List[A]] = effect.flatMap(
    x => if (n != 0) replicateMList(n - 1)(effect).map(x :: _) else URIO.succeed(Nil)
  )

  lazy val effects: List[URIO[Clock with Random, Int]] = for {
    x <- URIO.replicate(10)(eff).toList
  } yield x
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

    def summOfIter(list: List[URIO[Clock with Random, Int]]): URIO[Clock with Random, Int] = list match {
      case ::(head, next) => head.flatMap(x => summOfIter(next).map(y => x + y))
      case Nil => URIO(0)
    }
    
  lazy val app: ZIO[Clock with Console with Random, Nothing, Int] = for {
    y <- printEffectRunningTime(summOfIter(effects))
  } yield y



  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */
/*
  def concurrentSumm(effectList: List[URIO[Clock with Random, Int]]): ZIO[Clock with Random, Nothing, Int] = for {
    x <- ZIO.foreach(effectList)(_.fork)
    y <- ZIO.foreach(x)(_.join)
  } yield y.sum
*/



  lazy val appSpeedUp: URIO[Clock with Console with Random, Int] = for {
    x <- printEffectRunningTime(URIO.collectAllPar(effects).map(_.sum))
  } yield x


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */


  object EffectTime {

    type PrintEffectTimeEnv = Has[EffectTime.Service]

    trait Service {
      def executeAndGetRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A]
    }

    object Service {
      val live: Service = new Service {
        override def executeAndGetRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A] = {
          for {
            start <- clock.currentTime(TimeUnit.SECONDS)
            z <- zio
            end <- clock.currentTime(TimeUnit.SECONDS)
            _ <- putStrLn(s"Running time: ${end - start} sec")
          } yield z
        }
      }
    }

    val live: ZLayer[Any, Nothing, Has[Service]] = ZLayer.succeed(Service.live)

    def executeAndGetRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[PrintEffectTimeEnv with Clock with Console with R, E, A] =
      ZIO.accessM(_.get executeAndGetRunningTime zio)
  }


   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg: ZIO[Clock with Console with Random with EffectTime.PrintEffectTimeEnv, Nothing, Int] = for {
    seeTime <- ZIO.environment[EffectTime.PrintEffectTimeEnv].map(_.get)
    appResult <- seeTime.executeAndGetRunningTime(appSpeedUp)
  } yield appResult

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp: ZIO[Clock with Console with Random, Nothing, Unit] = for {
    programResult <- appWithTimeLogg.provideSomeLayer[Clock with Console with Random](EffectTime.live)
  } yield println(programResult)
  
}
