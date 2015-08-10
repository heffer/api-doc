package no.samordnaopptak.apidoc


import no.samordnaopptak.json._

import no.samordnaopptak.test.TestByAnnotation.Test

/*
 The internal format, close to the textual representation.
 */

object ApiDocParser{

  private def getIndentLength(line: String) =
    line.prefixLength(_==' ')

  @Test(code="""
    self.getEnumArgs("String") === (List(),0)
    self.getEnumArgs("String(query)") === (List(),0)
    self.getEnumArgs("Array String(query)") === (List(),0)
    self.getEnumArgs("Enum() String(query)") === (List(),6)
    self.getEnumArgs("Enum(1,2,3) String") === (List("1","2","3"),11)
    self.getEnumArgs("Enum(2,65,9) Int(query)") === (List("2","65","9"), 12)
  """)
  private def getEnumArgs(typetypetype: String): (List[String],Int) = {
    if (!typetypetype.startsWith("Enum ") && !typetypetype.startsWith("Enum("))
      return (List(),0)

    val startpos = typetypetype.indexOf('(')
    val endpos = typetypetype.indexOf(')')

    /*
    println("typetypetype: "+typetypetype)
    println("startpos: "+startpos)
    println("endpos: "+endpos)
     */

    val argsstring = typetypetype.substring(startpos+1,endpos)
    val args = argsstring.split(",").map(_.trim).filter(_ != "").toList
    //println("args: "+args)

    (args,endpos+1)
  }


  @Test(code="""
    self.findUriParms("/api/v1/acl/{service}")        === List("service")
    self.findUriParms("/api/v1/acl/{service}/{hest}") === List("service", "hest")
    self.findUriParms("/api/v1/acl/")                 === List()
    self.findUriParms("/api/v1/acl")                  === List()
  """)
  private def findUriParms(autoUri: String): List[String] =
    if (autoUri=="")
      List()
    else if (autoUri.startsWith("{")) {
      val next = autoUri.indexOf('}')
      autoUri.substring(1, next) :: findUriParms(autoUri.drop(next+1))
    } else
      findUriParms(autoUri.drop(1))



  @Test(code="""
      self.parseScalaTypeSignature("test.lib.User(+a,-b)") === ("test.lib.User", Set("a"), Set("b"))
    """)
  private def parseScalaTypeSignature(signature: String): (String, Set[String], Set[String]) = {

    val leftParPos = signature.indexOf('(')
    val rightParPos = signature.indexOf(')')

    if(leftParPos== -1 && rightParPos!= -1)
      throw new Exception("Malformed line: "+signature)
    if(leftParPos!= -1 && rightParPos== -1)
      throw new Exception("Malformed line: "+signature)
    if(leftParPos > rightParPos)
      throw new Exception("Malformed line: "+signature)

    if(leftParPos == -1) {

      (signature, Set(), Set())

    } else {

      val className = signature.take(leftParPos).trim
      val argsString = signature.substring(leftParPos+1, rightParPos).trim

      if (argsString=="") {

        (className, Set(), Set())

      } else {
        val modifiedFields = argsString.split(",").toList.map(_.trim)

        val addedFields = modifiedFields.filter(_.startsWith("+")).map(_.drop(1)).toSet
        val removedFields = modifiedFields.filter(_.startsWith("-")).map(_.drop(1)).toSet

        if (addedFields.size+removedFields.size != modifiedFields.size)
          throw new Exception("Malformed line: "+signature+". One or more modifier fields does not start with '-' or '+'")

        (className, addedFields, removedFields)
      }
    }
  }


  @Test(code="""
    instance.testTypeInfo("Array String (header)").type_ === "String"
    instance.testTypeInfo("Array String (header)").isArray === true
    instance.testTypeInfo("Array String (header)").paramType === "header"

    instance.testTypeInfo("Enum(a,b) String (header)").type_ === "String"
    instance.testTypeInfo("Enum(a,b) String (header)").isEnum === true
    instance.testTypeInfo("Enum(a,b) String (header)").paramType === "header"

    instance.testTypeInfo("Enum(2,65,9) Int(query)").type_ === "Int"
    instance.testTypeInfo("Enum(2,65,9) Int(query)").optional === false
    instance.testTypeInfo("Enum(2,65,9) Int(query,optional)").optional === true

    instance.testTypeInfo("String(header)").optional === false
    instance.testTypeInfo("String (header, optional)").optional === true
    instance.testTypeInfo("String(optional)").optional === true
    instance.testTypeInfo("String( optional)").optional === true
    instance.testTypeInfo("String").optional === false
  """)
  def testTypeInfo(typetypetype: String) =
    TypeInfo("", typetypetype)

  case class TypeInfo(val parmName: String, val typetypetype: String){                             // typetypetype = "Array String (header)"
    val (enumArgs,enumSize)  = getEnumArgs(typetypetype)
    val isArray      = typetypetype.startsWith("Array")
    val isEnum       = enumArgs.size > 0
    val typetype     = if (isArray) typetypetype.drop(6).trim else if (isEnum) typetypetype.drop(enumSize).trim else typetypetype                     // typetype = "String (header)"

    val leftParPos   = typetype.indexOf('(')
    val rightParPos  = typetype.indexOf(')')

    if (leftParPos>=0 && rightParPos == -1)
      throw new Exception(s"""Syntax error: Missing right paranthesis in "$parmName $typetypetype"""")

    if (leftParPos == -1 && rightParPos>=0)
      throw new Exception(s"""Syntax error: Missing left paranthesis in "$parmName $typetypetype"""")

    val hasTypeOptions = leftParPos != -1

    val type_        = if (hasTypeOptions) typetype.take(leftParPos).trim else typetype.trim           // type_ = "String"
    val typeOptions  = if (hasTypeOptions)
                          typetype.substring(leftParPos+1, rightParPos).split(',').map(_.trim).toSet
                       else
                         Set[String]()

    val optional     = typeOptions.contains("optional")

    val paramTypes   = typeOptions - "optional"

    if (paramTypes.size >= 2)
      throw new Exception(s"""Syntax error: Too many parameter options in "$parmName $typetypetype" ($paramTypes)""")

    val paramType    = if (paramTypes.size == 1)                                                           // paramType = "header"
                          paramTypes.head
                       else if (parmName=="body")
                         "body"
                       else
                         "path"
    /*
    println("parmName: "+parmName)
    println("typtyptyp: "+typetypetype)
     */

    if ( ! Set("body", "path", "query", "header", "formData").contains(paramType))
      throw new Exception(s""""$paramType" is not a valid paramameter type. It must be either "body", "path", "query", "header", or "formData". See https://github.com/wordnik/swagger-core/wiki/Parameters""")
  }

  private case class Raw(key: String, elements: List[String]) {
    def plus(element: String) =
      Raw(key, elements ++ List(element))


    private def getParameters(): JObject =
      JObject(
        elements.map(element => {
          if (element=="...")
            "..." -> J.obj(
              "type" -> "etc.",
              "isArray" -> false,
              "isEnum" -> false,
              "enumArgs" -> J.arr(),
              "required" -> false
            )
          else {
            val nameLength = element.indexOf(':', 0)
            if(nameLength == -1)
              throw new Exception(s"Syntax error for element '$element'. (Missing ':')")
            val name = element.substring(0,nameLength)
            val rest = element.drop(nameLength+1).trim.split("<-")
            val typeInfo = TypeInfo(name, rest(0).trim)
            val comment = if (rest.length==1) "" else rest(1).trim

            name -> J.obj(
              "type" -> typeInfo.type_,
              if (comment=="")
                "noComment" -> true
              else
                "comment" -> comment,
              "isArray" -> typeInfo.isArray,
              "isEnum" -> typeInfo.isEnum,
              "enumArgs" -> typeInfo.enumArgs,
              "paramType" -> typeInfo.paramType,
              "required"  -> !typeInfo.optional
            )
          }
        })
      )


    /*
     User: test.lib.User(+a,-b)
     ...
     
     ->

     User -> {...}
     */
    private def parseDataType(line: String): JObject = {
      val parameters = getParameters()
      val fieldNames = parameters.keys.toList.toSet

      val (dataTypeName, signature) = if (line.endsWith(":")) {

        val signature = key.dropRight(1).trim
        val splitPos = line.indexOf('(')

        if (splitPos== -1)
          (signature, signature+"()")
        else
          (line.take(splitPos).trim, signature)

      } else {

        val splitPos = line.indexOf(':')
        val dataTypeName = line.take(splitPos).trim
        val signature = line.drop(splitPos+1).trim

        (dataTypeName, signature)
      }

      if (signature != "!") {
        val (className, addedFields, removedFields) = parseScalaTypeSignature(signature)
        ApiDocValidation.validateDataTypeFields(className, dataTypeName, fieldNames, addedFields, removedFields)
      }

      J.obj(
        dataTypeName -> parameters
      )
    }

    def replace_leading_underscores(line: String): String =
      if (line(0)=='_')
        "&nbsp;" + replace_leading_underscores(line.drop(1))
      else
        line

    def getApidoc(): JObject = {
      if (key.startsWith("GET ") || key.startsWith("POST ") || key.startsWith("PUT ") || key.startsWith("DELETE ") || key.startsWith("PATCH ") || key.startsWith("OPTIONS ")) {

        if (!elements.isEmpty)
          throw new Exception(s"""Elements for "$key" are not empty: $elements""")

        val pos = key.indexOf(' ')
        val method = key.substring(0,pos).trim
        val uri = key.drop(pos).trim
        val uriParms = findUriParms(uri)

        J.obj(
          "method" -> method,
          "uri"    -> uri,
          "uriParms" -> uriParms
        )
      }

      else if (key=="DESCRIPTION")
        J.obj(
          "shortDescription" -> elements.head,
          "longDescription"  -> (if (elements.length==1) "" else elements.tail.map(replace_leading_underscores).mkString("<br>"))
        )

      else if (key=="PARAMETERS")
        J.obj(
          "parameters" -> getParameters()
        )

      else if (key=="ERRORS")
        J.obj(
          "errors" -> JArray(
            elements.map(element => {
              val code = element.substring(0,4).trim.toInt
              val description = element.drop(4).trim
              J.obj(
                "code" -> code,
                "message" -> description
              )})
          ))

      else if (key=="RESULT") {
        if (elements.length!=1)
          throw new Exception(s"Malformed RESULT elements (more or less than 1): $elements.")

        val splitted = elements(0).trim.split("<-").map(_.trim)

        val typeInfo = TypeInfo("", splitted(0))
        val comment = if (splitted.length==1) "" else splitted(1)

        J.obj(
          "result" -> J.obj(
            "type" -> typeInfo.type_,
            "comment" -> comment,
            "isArray" -> typeInfo.isArray,
            "isEnum" -> typeInfo.isEnum,
            "enumArgs" -> typeInfo.enumArgs
          )
        )
      }

      else if (key.contains(":"))
        J.obj(
          "datatype" -> parseDataType(key)
        )

      else
        throw new Exception(s"""Unknown key: "$key"""")
    }
  }


  private def parseRaw(
    lines: List[String],
    current: Raw,
    result: List[Raw],
    mainIndentLength: Int
  ): List[Raw] =

    if (lines.isEmpty)
      current::result

    else {
      val indentLength = getIndentLength(lines.head)

      val line = lines.head.trim

      if (indentLength < mainIndentLength)
        throw new Exception(s"""Bad indentation for line "$line"""")

      else if (indentLength > mainIndentLength)
        parseRaw(lines.tail, current.plus(line), result, mainIndentLength)

      else
        parseRaw(lines.tail, Raw(line, List()), current::result, mainIndentLength)
    }

  private def parseRaw(apidoc: String): List[Raw] = {
    val lines = apidoc.split("\n").filter(line => line.trim.length>0).toList // All non-empty lines.
    val indentLength = getIndentLength(lines.head)
    val line = lines.head.trim

    parseRaw(lines.tail, Raw(line, List()), List(), indentLength)
  }

  // public because of testing.
  def getRaw(apidoc: String): JObject = {
    val raws = parseRaw(apidoc)
    JObject(raws.map(raw => raw.key -> JArray(raw.elements.map(JString))))
  }


  def getJson(apidoc: String): JObject = {
    val raws = parseRaw(apidoc)
    var ret = J.obj()
    raws.reverse.map(_.getApidoc).foreach(apidoc =>
      if ( ! apidoc.keys.contains("datatype"))
        ret = ret ++ apidoc
    )
    ApiDocValidation.validateJson(ret)
    ret
  }


  def getDataTypes(apidoc: String): JObject =
    J.flattenJObjects(
      parseRaw(apidoc)
        .reverse
        .map(raw => raw.getApidoc())
        .filter(_("datatype").isDefined)
        .map(_("datatype"))
    )

}
