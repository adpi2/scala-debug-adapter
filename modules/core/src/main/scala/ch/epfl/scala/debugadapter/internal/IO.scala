package ch.epfl.scala.debugadapter.internal

import java.net.URI
import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.nio.file.Path
import java.util
import scala.util.Try
import scala.util.control.NonFatal
import scala.util.Failure
import scala.util.Success
import java.nio.file.Files

private[debugadapter] object IO {
  def withinJarFile[T](absolutePath: Path)(f: FileSystem => T): Try[T] = try {
    val tempFolder = Files.createTempDirectory("scala-debug-adapter")
    val copy = tempFolder.resolve("copy.zip")
    TimeUtils.logTime("copy") {
      Files.copy(absolutePath, copy)
    }
    val uri = URI.create(s"jar:${copy.toUri}")
    val fileSystem = FileSystems.newFileSystem(uri, new util.HashMap[String, Any])
    try Success(f(fileSystem))
    finally fileSystem.close()
  } catch {
    case NonFatal(exception) => Failure(exception)
    case zipError: util.zip.ZipError => Failure(zipError)
  }
}
