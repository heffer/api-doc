package no.samordnaopptak.json

import play.api.libs.json._

class JsonException(val message: String) extends Exception(message)
class JsonMergeObjectsException(message: String) extends JsonException(message)
class JsonIllegalConversionException(message: String) extends JsonException(message)
class JsonParseException(message: String) extends JsonException(message)

trait JValue {

  private var visitedKeys: Set[String] = Set()

  def pp() = Json.prettyPrint(this.asJsValue)

  override def toString =
    "JsonUtil.Json(\n"+pp()+"\n)"

  def asDouble = asNumber.toDouble
  def asInt = asNumber.toInt
  def asLong = asNumber.toLong

  protected def error =
    throw new JsonException(s"""Trying to access something illegal in JValue (see backtrace). value: ${pp()}""")

  protected def illegalConversionError =
    throw new JsonIllegalConversionException(s"""Can not convert Json object to this type. value: ${pp()}""")

  def asJsValue: JsValue
  def asJsObject: JsObject = illegalConversionError
  def asJsArray: JsArray = illegalConversionError
  def asMap: Map[String, JValue] = illegalConversionError
  def asArray: List[JValue] = illegalConversionError
  def asString: String = illegalConversionError
  def asNumber: BigDecimal = illegalConversionError
  def asBoolean: Boolean = illegalConversionError

  def isArray: Boolean = false
  def isObject: Boolean = false
  def isNumber: Boolean = false
  def isBoolean: Boolean = false
  def isString: Boolean = false
  def isNull: Boolean = false

  def isDefined: Boolean = true
  def hasKey(key: String): Boolean = illegalConversionError  // it might be undefined even if it has key. (i.e. when the value is null)
  def keys: Set[String] = illegalConversionError
  def size: Int = illegalConversionError

  def ++(other: JValue): JObject = illegalConversionError

  def asLongArray = asArray.map(_.asLong)
  def asIntArray = asArray.map(_.asInt)
  def asDoubleArray = asArray.map(_.asDouble)
  def asStringArray = asArray.map(_.asString)
  def asBooleanArray = asArray.map(_.asBoolean)

  def asOption[R](command: JValue => R): Option[R] =
    Some(command(this))

  def getOrElse[R](command: JValue => R, orElseValue: R): R =
    command(this)

  def apply(key: String) =
    asMap.get(key) match {
      case Some(json) => {
        visitedKeys = visitedKeys + key
        json
      }
      case None =>
        JUndefined(key, this)
    }

  def apply(index: Int) =
    try{
      asArray(index)
    }catch{
      case e: java.lang.IndexOutOfBoundsException => throw new JsonException("index "+index+" not found in "+pp()+" ("+e.getMessage()+")")
    }

  def validateRemaining(ignoreKeys: Set[String]): Unit = {
    val visitedKeys = this.visitedKeys ++ ignoreKeys
    val diff = asMap.keys.toSet.diff(visitedKeys)

    if (!diff.isEmpty)
      throw new JsonParseException(s"""Unknown field(s): ${diff.mkString("\"", "\", \"", "\"")}""")
  }

  def validateRemaining(ignoreKeys: String*): Unit =
    validateRemaining(ignoreKeys.toSet)
}


case class JNumber(value: BigDecimal) extends JValue{
  override def asNumber = value
  override def isNumber = true
  override def asJsValue = JsNumber(value)
}

case class JString(value: String) extends JValue{
  override def asString = value
  override def isString = true
  override def asJsValue = JsString(value)
}

case class JBoolean(value: Boolean) extends JValue{
  override def asBoolean = value
  override def isBoolean = true
  override def asJsValue = JsBoolean(value)
}

case class JObject(value: Map[String, JValue]) extends JValue{
  override def asMap: Map[String, JValue] = value
  override def isObject = true
  override def asJsValue = asJsObject
  override def asJsObject = JsObject(
    value.map{
      case (k: String, v: JValue) => k -> v.asJsValue
    }
  )
  override def size = value.size
  override def keys = value.keys.toSet
  override def hasKey(key: String) = keys.contains(key)

  override def ++(other: JValue): JObject = {
    val otherValue = other.asMap
    val result = value ++ otherValue

    if (result.size != value.size+otherValue.size)
      throw new JsonMergeObjectsException("JObject.++: objects intersects :" + value.keys.toSet.intersect(otherValue.keys.toSet))

    JObject(result)
  }
}

object JObject{
  def apply(value: List[(String, JValue)]): JObject = JObject(value.toMap)
}

case class JArray(value: List[JValue]) extends JValue{
  override def asArray = value
  override def isArray = true
  override def asJsValue = asJsArray
  override def asJsArray = JsArray(value.map(_.asJsValue))
  override def size = value.size
/*
  override def ++(other: JValue): JObject =
    JArray(value ++ other.asArray)
 */
}

case class JUndefined(key: String, parent: JValue) extends JValue{
  override def pp() = "Undefined key '"+key+"' in " + parent.pp()
  override def error = throw new JsonException("""Trying to access key """"+key+"""", which is not found in """+parent)
  override def asOption[R](command: JValue => R): Option[R] = None
  override def getOrElse[R](command: JValue => R, orElseValue: R): R = orElseValue
  override val isDefined = false
  override def asJsValue = JsNull
}

object JNull extends JValue {
  override def asJsValue = JsNull
  override def isNull = true
  override def asOption[R](command: JValue => R): Option[R] = None
  override def getOrElse[R](command: JValue => R, orElseValue: R): R = orElseValue
  override val isDefined = false
}



object J {

  private def jsValueToJValue(value: JsValue): JValue =
    value match {
      case j: JValue   => j
      case j: JsNumber => JNumber(j.value)
      case j: JsString => JString(j.value)
      case j: JsBoolean => JBoolean(j.value)
      case j: JsObject => JObject(
        j.value.map{
          case (k: String, v: JsValue) => k -> jsValueToJValue(v)
        }.toMap
      )
      case j: JsArray => JArray(j.value.map(jsValueToJValue(_)).toList)
      case `JsNull` => JNull
    }

  def apply(a: Any): JValue =
    a match {
      case value: JValue => value
      case value: JsValue =>  jsValueToJValue(value)
      case value: BigDecimal =>  JNumber(value)
      case value: Int =>  JNumber(value)
      case value: Long =>  JNumber(value)
      case value: Float =>  JNumber(value)
      case value: Double =>  JNumber(value)
      case value: String => JString(value)
      case value: Boolean => JBoolean(value)
      case value: Map[_,_] => JObject(
        value.asInstanceOf[Map[String,Any]].map{
          case (k: String, v: Any) => k -> apply(v)
        }
      )
      case value: Seq[_] => JArray(value.map(apply(_)).toList)
      case `None` => JNull
      case Some(value) => apply(value)
      case _ if a==null => JNull
      case value: Json.JsValueWrapper => apply(Json.arr(value)(0).get)
      case _ => throw new Exception(s"""Unable to convert "$a" to JValue. Class: ${a.getClass}""")
    }

  def obj(vals: (String, Any)*): JObject = {
    apply(vals.toMap).asInstanceOf[JObject]
  }

  def arr(vals: Any*): JArray =
    apply(vals.map(apply)).asInstanceOf[JArray]

  def parse(jsonString: String): JValue =
    try{
      jsValueToJValue(play.api.libs.json.Json.parse(jsonString))
    }catch{
      case e: Throwable => throw new JsonParseException(s"""Could not parse "$jsonString": ${e.getMessage()}""")
    }

  def parseAndValidate[R](jsonString: String, ignore: Set[String] = Set(), allowedKeys: Set[String] = Set())(command: JValue => R): R = {
    val json = parse(jsonString)

    if (allowedKeys != Set() && json.isObject)
      json.asMap.keys.foreach(key =>
        if (!allowedKeys.contains(key))
          throw new JsonParseException(s"""Key "$key" is not supported when parsing json string""")
      )

    val ret = command(json)
    json.validateRemaining(ignore)
    ret
  }

  // Same as parseAndValidate, except that it doesn't validate that all fields are read.
  def parseIt[R](jsonString: String)(command: JValue => R): R =
    command(parse(jsonString))

  def flattenJsObjects(objs: Seq[JsObject]): JsObject =
    if (objs.isEmpty)
      Json.obj()
    else
      objs.head ++ flattenJsObjects(objs.tail)

  /*
  def flattenJsObjects(objs: JsObject*): JsObject =
    flattenJsObjects(objs.toSeq)
   */

  def flattenJObjects(objs: Seq[JValue]): JObject =
    if (objs.isEmpty)
      obj()
    else
      objs.head ++ flattenJObjects(objs.tail)
}

