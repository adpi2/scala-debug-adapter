package ch.epfl.scala.debugadapter.internal.evaluator

import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider
import com.sun.jdi._

import java.util.concurrent.CompletableFuture
import scala.collection.JavaConverters._
import scala.tools.nsc.ExpressionCompiler
import scala.util.Try
import ch.epfl.scala.debugadapter.Logger
import scala.util.control.NonFatal

class Evaluator(logger: Logger) {
  def evaluate(
    expression: String,
    thread: ThreadReference,
    frame: StackFrame
  )(sourceLookUpProvider: ISourceLookUpProvider): CompletableFuture[Value] = {
    val vm = thread.virtualMachine()
    val thisObject = Option(frame.thisObject())

    val location = frame.location()
    val sourcePath = location.sourcePath()
    val line = location.lineNumber()
    val fqcn = location.declaringType().name()

    val uri = sourceLookUpProvider.getSourceFileURI(fqcn, sourcePath)
    val content = sourceLookUpProvider.getSourceContents(uri)

    var error: Option[String] = None
    val result = try {
       for {
        classLoader <- vm.allClasses().asScala.find(_.classLoader() != null).map(_.classLoader()).flatMap(JdiClassLoader(_, thread))
        variables = frame.visibleVariables().asScala
        variableNames <- Some(variables.map(_.name()).map(vm.mirrorOf).toList)
        variableValues <- Some(variables.map(frame.getValue).flatMap(value => boxIfNeeded(value, classLoader, thread)).toList)
        fields = thisObject.map(_.referenceType().fields().asScala).getOrElse(List())
        fieldNames = fields.map(_.name()).map(vm.mirrorOf).toList
        fieldValues = thisObject.map(thiz => fields.map(field => thiz.getValue(field)).flatMap(value => boxIfNeeded(value, classLoader, thread))).getOrElse(List())
        thisObjectName = thisObject.map(_ => vm.mirrorOf("$this"))
        names = variableNames ++ fieldNames ++ thisObjectName.map(Seq(_)).getOrElse(Seq())
        values <- Some(variableValues ++ fieldValues ++ thisObject.map(Seq(_)).getOrElse(Seq()))
        systemClass <- classLoader.loadClass("java.lang.System")
        classPath <- systemClass
          .invokeStatic("getProperty", List(vm.mirrorOf("java.class.path")))
          .map(_.toString)
          .map(_.drop(1).dropRight(1)) // remove quotation marks
        expressionCompiler <- Some(ExpressionCompiler(classPath, line, expression, names.map(_.value()).toSet))
        _ = logger.info(expressionCompiler.dir.toString)
        expressionClassPath = expressionCompiler.dir.toUri.toString
        _ <- expressionCompiler.compile(content, errorMessage => error = Some(errorMessage))
        _ = logger.info("compiled successfully")
        url <- classLoader
          .loadClass("java.net.URL")
          .flatMap(_.newInstance(List(vm.mirrorOf(expressionClassPath))))
        _ = logger.info("java.net.URL loaded successfully")
        urls <- JdiArray("java.net.URL", 1, classLoader, thread)
        _ <- urls.setValue(0, url.reference)
        urlClassLoader <- classLoader
          .loadClass("java.net.URLClassLoader")
          .flatMap(_.newInstance(List(urls.reference)))
          .map(_.reference.asInstanceOf[ClassLoaderReference])
          .flatMap(JdiClassLoader(_, thread))
        _ = logger.info("java.net.URLClassLoader created successfully")
        expressionClassFqcn = ExpressionCompiler.expressionClassFqcn(fqcn)
        _ = logger.info(s"fqcn = $fqcn")
        _ = logger.info(s"expressionClassFqcn = $expressionClassFqcn")
        expressionClass <- urlClassLoader.loadClass(ExpressionCompiler.expressionClassFqcn(fqcn))
        _ = logger.info("expressionClass loaded")
        expression <- expressionClass.newInstance(List())
        _ = logger.info("expressionClass created")
        namesArray <- JdiArray("java.lang.String", names.size, classLoader, thread)
        _ = logger.info(s"namesArray created successfuly")
        valuesArray <- JdiArray("java.lang.Object", values.size, classLoader, thread) // add boxing
        _ = logger.info(s"valuesArray created successfuly")
        _ <- Some(namesArray.setValues(names))
        _ <- Some(valuesArray.setValues(values))
        _ = logger.info(s"values set")
        result <- expression.invoke("evaluate", List(namesArray.reference, valuesArray.reference))
      } yield {
        logger.info(s"expression evaluated")
        result
      }
    } catch {
      case NonFatal(cause) =>
        logger.error(cause.getMessage())
        logger.trace(cause)
        None
    }

    result match {
      case Some(value) =>
        CompletableFuture.completedFuture(value)
      case None => error match {
        case Some(message) =>
          throw new Exception(message)
        case None =>
          throw new Exception("Unable to evaluate the expression")
      }
    }
  }

  private def boxIfNeeded(value: Value, classLoader: JdiClassLoader, thread: ThreadReference): Option[Value] = value match {
    case value: BooleanValue =>
      JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value: CharValue =>
      JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value: DoubleValue =>
      JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value: FloatValue =>
      JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value: IntegerValue =>
      JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value: LongValue =>
      JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value: ShortValue =>
      JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value => Some(value)
  }

  def invokeMethod(
    thisContext: ObjectReference,
    methodName: String,
    signature: String,
    args: Array[Value],
    thread: ThreadReference,
    invokeSuper: Boolean
  ): CompletableFuture[Value] = {
    val result = for {
      obj <- Try(new JdiObject(thisContext, thread)).toOption
      result <- obj.invoke(methodName, signature, if (args == null) List() else args.toList)
    } yield result

    result match {
      case Some(value) =>
        CompletableFuture.completedFuture(value)
      case None =>
        throw new Exception("Unable to evaluate the expression")
    }
  }
}
