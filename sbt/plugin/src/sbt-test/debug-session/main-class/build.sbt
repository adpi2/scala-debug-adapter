import scala.concurrent.ExecutionContext
import ch.epfl.scala.debug.testing.TestDebugClient

val checkDebugSession = inputKey[Unit]("Check the main class debug session")

scalaVersion := "2.12.12"
checkDebugSession := {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath

  val client = TestDebugClient.connect(uri)
  try {
    client.initialize()
    client.launch()
    
    client.setBreakpoints(source, Array(3, 5, 9))
    client.configurationDone()
    
    val threadId = client.stopped.threadId
    
    client.continue(threadId)
    client.stopped
    
    client.continue(threadId)
    client.stopped
    
    client.continue(threadId)
    client.exited
    client.terminated
  } finally {
    client.close()
  }
}