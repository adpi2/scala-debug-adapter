package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.Logger
import scala.util.Success
import scala.util.Failure
import scala.util.Try
import java.util.concurrent.CompletableFuture

private[debugadapter] object ScalaExtension {

  implicit class TryExtension[T](x: Try[T]) {
    def warnFailure(logger: Logger, message: String): Option[T] = x match {
      case Success(value) => Some(value)
      case Failure(e) =>
        logger.warn(s"$message: ${e.getClass.getSimpleName} ${e.getMessage}")
        None
    }

    def toCompletableFuture: CompletableFuture[T] = {
      val future = new CompletableFuture[T]()
      x match {
        case Success(value) => future.complete(value)
        case Failure(e) => future.completeExceptionally(e)
      }
      future
    }
  }

  implicit class SeqTryExtension[T](xs: Seq[Try[T]]) {
    def traverse: Try[Seq[T]] =
      xs.foldLeft(Try(Seq.empty[T])){ (acc, t) =>
        for {
          xs <- acc
          x <- t
        } yield x +: xs
      }
  }

  implicit class OptionExtension[T](opt: Option[T]) {
    def toTry(message: String): Try[T] = {
      opt match {
        case Some(value) => Success(value)
        case None => Failure(new Exception(message))
      }
    }
  }
}
