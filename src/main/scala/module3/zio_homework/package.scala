package module3

import zio.{Has, Schedule, Task, ULayer, URIO, ZIO, ZLayer}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.Console.println
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  lazy val readInt: ZIO[Console, Throwable, Int] =
    for {
      input <- getStrLn
      number <- ZIO.effect(input.toInt)
    } yield number

  lazy val readIntOrRetry: ZIO[Console, Nothing, Int] =
    readInt <> (putStrLn("It's not an integer. Try again") *> readIntOrRetry)

  def validateIntByRange(num: Int, range: Range): ZIO[Console, Nothing, Int] =
    if (range contains num) ZIO.succeed(num)
    else putStrLn("This range doesn't contain the num. Try again") *> readIntOrRetry


  def printResult(userNum: Int, randomNum: Int): URIO[Console, Unit] = {
    if (userNum == randomNum) putStrLn("You are right")
    else putStrLn("Nope, wrong")
  }

  lazy val guessProgram: ZIO[Random with Console, IOException, Unit] = for {
    _ <- putStrLn("Type any number from 1 to 3")
    userNumber <- readIntOrRetry
    validUserNumber <- validateIntByRange(userNumber, 1 to 3)
    randomNumber <- nextIntBetween(1, 3)
    _ <- printResult(validUserNumber, randomNumber)
  } yield ()


  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R, E, A](effect: ZIO[R, E, A])(f: A => Boolean): ZIO[R, E, A] = {
    effect.flatMap(k => {
      if(f(k)) effect
      else doWhile(effect)(f)
    })
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault = ???


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */


  lazy val eff: ZIO[Random with Clock, Nothing, Int] = for {
    _ <- ZIO.sleep(1 seconds)
    randomNum <- nextIntBetween(0, 10)
  } yield randomNum

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: List[ZIO[Random with Clock, Nothing, Int]] = List.fill(10)(eff)

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  import zioConcurrency.printEffectRunningTime
  lazy val app: ZIO[Clock with Console with Random, Nothing, Unit] = for {
    list <- printEffectRunningTime(ZIO.collectAll(effects))
    _    <- putStrLn(list.sum.toString)
  } yield ()

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp: ZIO[Clock with Console with Random, Nothing, Unit] = for {
    list <- printEffectRunningTime(ZIO.collectAllPar(effects))
    _    <- putStrLn(list.sum.toString)
  } yield ()


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */


   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg = ???

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = ???
  
}
