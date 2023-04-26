package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.DebugConfig
import ch.epfl.scala.debugadapter.DebugTools
import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.Logger
import com.microsoft.java.debug.core.DebugSettings
import com.microsoft.java.debug.core.adapter.{StepFilterProvider => _, _}
import com.microsoft.java.debug.core.protocol.Types
import com.sun.jdi._
import io.reactivex.subjects.PublishSubject;

import java.util
import java.util.Collections
import io.reactivex.Observable
import io.reactivex.Observer
import io.reactivex.subjects.Subject
import scala.collection.immutable

private[debugadapter] object DebugAdapter {

  /**
   * Since Scala 2.13, object fields are represented by static fields in JVM byte code.
   * See https://github.com/scala/scala/pull/7270
   */
  DebugSettings.getCurrent.showStaticVariables = true

  def context(debuggee: Debuggee, tools: DebugTools, logger: Logger, config: DebugConfig): IProviderContext = {
    TimeUtils.logTime(logger, "Configured debugger context") {
      val context = new ProviderContext
      val classEntries = debuggee.classEntries
      val distinctEntries = classEntries
        .groupBy(e => e.name)
        .map { case (name, group) =>
          if (group.size > 1) logger.warn(s"Found duplicate entry $name in debuggee ${debuggee.name}")
          group.head
        }
        .toSeq
      val sourceLookUp = SourceLookUpProvider(distinctEntries, logger)

      context.registerProvider(
        classOf[IHotCodeReplaceProvider],
        HotCodeReplaceProvider(sourceLookUp, debuggee.classesToUpdate, logger, config.testMode)
      )
      context.registerProvider(classOf[IVirtualMachineManagerProvider], VirtualMachineManagerProvider)
      context.registerProvider(classOf[ISourceLookUpProvider], sourceLookUp)
      context.registerProvider(
        classOf[IEvaluationProvider],
        EvaluationProvider(debuggee, tools, sourceLookUp, logger, config)
      )
      context.registerProvider(classOf[ICompletionsProvider], CompletionsProvider)
      context.registerProvider(
        classOf[IStepFilterProvider],
        StepFilterProvider(debuggee, tools, sourceLookUp, logger, config.testMode)
      )
      context
    }
  }

  object CompletionsProvider extends ICompletionsProvider {
    override def codeComplete(
        frame: StackFrame,
        snippet: String,
        line: Int,
        column: Int
    ): util.List[Types.CompletionItem] = Collections.emptyList()
  }

  object VirtualMachineManagerProvider extends IVirtualMachineManagerProvider {
    def getVirtualMachineManager: VirtualMachineManager =
      Bootstrap.virtualMachineManager
  }
}
