package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.{ArrayReference, ObjectReference, ReferenceType, ThreadReference, Value}

import scala.util.Try
import scala.collection.JavaConverters._

object JdiArray {
  def apply(arrayType: String, arraySize: Int, classLoader: JdiClassLoader, thread: ThreadReference): Option[JdiArray] = {
    val vm = thread.virtualMachine()
    for {
      classClass <- classLoader.loadClass("java.lang.Class")
      intClass <- classClass.invoke("getPrimitiveClass", List(vm.mirrorOf("int")))
      arrayClass <- classLoader.loadClass("java.lang.reflect.Array")
      newInstanceMethod <- Try(arrayClass.invoke("getMethod", List(vm.mirrorOf("newInstance"), classClass.reference, intClass)))
        .toOption
        .flatten
        .map(_.asInstanceOf[ObjectReference])
        .map(new JdiObject(_, thread))
      arrayTypeClass <- classLoader.loadClass(arrayType)
      integerValue <- JdiPrimitive.boxed(arraySize, classLoader, thread)
      array <- newInstanceMethod
        .invoke("invoke", List(arrayClass.reference, arrayTypeClass.reference, integerValue.reference))
        .map(_.asInstanceOf[ArrayReference])
        .map(new JdiArray(_, thread))
    } yield array
  }
}

class JdiArray(override val reference: ArrayReference, thread: ThreadReference) extends JdiObject(reference, thread) {
  def setValue(index: Int, value: Value): Option[Unit] =
    Try(reference.setValue(index, value)).toOption

  def setValues(values: List[Value]): Option[Unit] =
    Try(reference.setValues(values.asJava)).toOption
}