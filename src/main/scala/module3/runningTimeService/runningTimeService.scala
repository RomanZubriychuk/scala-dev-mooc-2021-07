package module3

import zio.{Has, ULayer, ZIO, ZLayer}
import zio.clock.{Clock, currentTime}
import zio.console.{Console, putStrLn}

import java.util.concurrent.TimeUnit

package object runningTimeService {
  type RunningTimeService = Has[RunningTimeService.Service]

  object RunningTimeService {
    trait Service {
      def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A]
    }

    object Service {
      val live: Service = new Service {
        def printEffectRunningTime[R, E, A](effect: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A] = for{
          start <- currentTime(TimeUnit.SECONDS)
          z <- effect
          end <- currentTime(TimeUnit.SECONDS)
          _ <- putStrLn(s"Running time: ${end - start}")
        } yield z
      }
    }

    val live: ULayer[Has[Service]] = ZLayer.succeed(Service.live)

    def printEffectRunningTime[R, E, A](effect: ZIO[R, E, A]): ZIO[RunningTimeService with Clock with Console with R, E, A] = ZIO.accessM(_.get.printEffectRunningTime(effect))
  }
}
