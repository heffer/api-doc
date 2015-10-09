package no.samordnaopptak.apidoc

import org.reflections.Reflections
import org.reflections.scanners.SubTypesScanner
import play.api.Play.current
import scala.collection.JavaConverters._
import no.samordnaopptak.test.TestByAnnotation.Test


object AnnotationHelper{
  @Test(code="""
     self.hasSameUri("/api/v1/acl", "/api/v1/acl")   === true
     self.hasSameUri("/1api/v1/acl", "/api/v1/acl")  =/= true
     self.hasSameUri("/api/v1/acl", "/api/v1/acl" )  === true
     self.hasSameUri("/1api/v1/acl", "/api/v1/acl" ) =/= true
     self.hasSameUri("/api/v1/acl", "/api/v1/acl2" ) =/= true
     self.hasSameUri("/api/v1/acl", "/api/v1"      ) =/= true

     self.hasSameUri("/1api/v1/acl/{service}", "/api/v1/acl/$service<[^/]+>") =/= true

     self.hasSameUri("/api/v1/acl/{service}", "/api/v1/acl/$service<[^/]+>" ) === true

     self.hasSameUri("/api/v1/acl",          "/api/v1/acl/$service<[^/]+> " ) =/= true
     self.hasSameUri("/api/v1/acl",          "/api/v1/acl/$service<[^/]+> " ) =/= true

     self.hasSameUri("/api/v1/acl",          "/api/v1/acl"                  ) === true
     self.hasSameUri("/api/v1/acl",          "/api/v1/acl/"                 ) === true
     self.hasSameUri("/api/v1/acl",          "/api/v1/acl"                  ) === true
     self.hasSameUri("/api/v1/acl",          "/api/v1/acl/"                 ) === true
     self.hasSameUri("/api/v1/acl/{service}", "/api/v1/acl/"                ) =/= true
     self.hasSameUri("/api/v1/acl/{service}", "/api/v1/acl"                 ) =/= true

      // one parameter in the middle of the uri:
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp" ) === true
     self.hasSameUri("/api/v1/acl/{service}/hepp/", "/api/v1/acl/$service<[^/]+>/hepp/" ) === true
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp/" ) === true
     self.hasSameUri("/api/v1/acl/{service}/hepp/", "/api/v1/acl/$service<[^/]+>/hepp" ) === true

     self.hasSameUri("/api/v1/acl/{service}/hepp2", "/api/v1/acl/$service<[^/]+>/hepp" ) === false
     self.hasSameUri("/api/v1/acl/{service2}/hepp", "/api/v1/acl/$service<[^/]+>/hepp" ) === false
     self.hasSameUri("/api/v1/acl2/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp" ) === false
     self.hasSameUri("/2api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp2" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service<[^/]+>/hepp/2" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl/$service2<[^/]+>/hepp/" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api/v1/acl2/$service<[^/]+>/hepp/" ) === false
     self.hasSameUri("/api/v1/acl/{service}/hepp", "/api2/v1/acl/$service<[^/]+>/hepp/" ) === false

     // two parameters in the middle of the uri:
     self.hasSameUri("/api/v1/acl/{service}/{hepp}", "/api/v1/acl/$service<[^/]+>/$hepp<[^/]+>" ) === true
     self.hasSameUri("/api/v1/acl/{service}/gakk/{hepp}", "/api/v1/acl/$service<[^/]+>/gakk/$hepp<[^/]+>" ) === true
  """)
  private def hasSameUri(autoUri: String, confUri: String): Boolean = {
    val autos = autoUri.split("/")
    val confs = RoutesHelper.getAutoUriFromConfUri(confUri).split("/")

    autos.size == confs.size &&
    autos.zip(confs).forall(g => {
      val auto = g._1
      val conf = g._2
      auto == conf
    })
  }

  class MissingMethodException(val message: String) extends Exception(message)
  class MethodMismatchException(val message: String) extends Exception(message)
  class UriMismatchException(val message: String) extends Exception(message)


  private def hasAnnotation(method: java.lang.reflect.Method) = {
    val annotations = method.getAnnotations()
    annotations.exists(_.isInstanceOf[no.samordnaopptak.apidoc.ApiDoc])
  }


  def hasMethodAnnotation(className: String, methodName: String) = {
    val class_ = play.api.Play.classloader.loadClass(className)

    class_.getDeclaredMethods().exists(
      method => method.getName()==methodName && hasAnnotation(method)
    )
  }

  def getMethodAnnotation(className: String, methodName: String) = {
    val class_ = play.api.Play.classloader.loadClass(className)

    val method =
      class_.getDeclaredMethods().find(
        method => (method.getName()==methodName && hasAnnotation(method))
      ).get

    val rightAnnotation =
      method.getAnnotations().find(
        _.isInstanceOf[no.samordnaopptak.apidoc.ApiDoc]
      ).get

    rightAnnotation.asInstanceOf[no.samordnaopptak.apidoc.ApiDoc]
  }

  def hasClassAnnotation(className: String): Boolean = try {
    val class_ = play.api.Play.classloader.loadClass(className)

    class_.getAnnotations.exists(_.isInstanceOf[no.samordnaopptak.apidoc.ApiDoc])
  } catch {
    case _: NoClassDefFoundError   => false
    case _: ClassNotFoundException => false
  }

  def getClassAnnotation(className: String) = {
    val cleanClassName = if (className.endsWith("[]")) className.dropRight(2) else className
    val class_ = play.api.Play.classloader.loadClass(cleanClassName)

    class_.getAnnotations().find {
      _.isInstanceOf[no.samordnaopptak.apidoc.ApiDoc]
    }.map(_.asInstanceOf[no.samordnaopptak.apidoc.ApiDoc])
  }

  def expandIncludes(doc: String, alreadyIncluded: scala.collection.mutable.Set[String]): String = {
    val lines = doc.split("\n")
    lines.map { line =>
      val trimmed = line.trim

      if (trimmed.startsWith("INCLUDE ")) {

        val pathAndMethod = trimmed.drop("INCLUDE ".size).trim

        if (alreadyIncluded.contains(pathAndMethod)) {

          "\n"

        } else {

          val lastDot = pathAndMethod.lastIndexOf('.')
          val className = pathAndMethod.take(lastDot)
          val methodName = pathAndMethod.drop(lastDot+1)

          /*
           println(line)
           println(pathAndMethod)
           println(className)
           println(methodName)
           */

          alreadyIncluded.add(pathAndMethod)

          getMethodAnnotationDoc(className, methodName, alreadyIncluded)
        }

      } else {

        line + "\n"

      }

    }.mkString
  }

  def getMethodAnnotationDoc(className: String, methodName: String, alreadyIncluded: scala.collection.mutable.Set[String]) = {
    val annotation = getMethodAnnotation(className, methodName)
    expandIncludes(annotation.doc, alreadyIncluded)
  }

  def validate(routeEntries: List[RouteEntry]): Unit = {
    val alreadyIncluded = scala.collection.mutable.Set[String]()

    routeEntries.foreach(routeEntry => {

      if (!hasMethodAnnotation(routeEntry.scalaClass, routeEntry.scalaMethod))
        throw new MissingMethodException(s"Missing ApiDoc for ${routeEntry.scalaClass}.${routeEntry.scalaMethod} (Make sure the Class is annotated, and not the corresponding Object)")

      val doc = getMethodAnnotationDoc(routeEntry.scalaClass, routeEntry.scalaMethod, alreadyIncluded)
      val json = ApiDocParser.getJson(doc)
      val jsonMethod = json("method").asString
      val jsonUri = json("uri").asString

      if (jsonMethod != routeEntry.restMethod)
        throw new MethodMismatchException(
          s"Conflicting REST method declared in the autodoc and in conf/routes for ${routeEntry.scalaClass}.${routeEntry.scalaMethod}.\n" +
          s"autodoc: $jsonMethod. conf/routes: ${routeEntry.restMethod}"
        )

      if (!hasSameUri(jsonUri, routeEntry.uri)) {
        throw new UriMismatchException(
          s"Conflicting uri declared in the autodoc and in conf/routes for ${routeEntry.scalaClass}.${routeEntry.scalaMethod}.\n" +
          s"autodoc: $jsonUri. conf/routes: ${routeEntry.uri}"
        )
      }
    }
    )
  }

  def getClassAnnotationDoc(className: String) = getClassAnnotation(className).map(_.doc).getOrElse("")

  private def rewriteFieldType(canonicalFieldTypeName: String, knownFields: String Map String): String = canonicalFieldTypeName match {
    case array if array endsWith "[]"                     => "Array " + rewriteFieldType(array dropRight 2, knownFields)
    case optional if optional startsWith "scala.Option"   => "String(optional)" // FIXME consider other types, too
    case "java.lang.String"                               => "String"
    case "int"                                            => "Integer"
    case "long"                                           => "Long"
    case "boolean"                                        => "Boolean"
    case knownType if (knownFields isDefinedAt knownType) => knownFields(knownType)
    case otherType                                        => otherType
  }

  def expandClassAnnotationDoc(apiDoc: String, resultAliasesToClasses: String Map String) = {
    val classesToResultAliases = resultAliasesToClasses map (_.swap)

    if (apiDoc.split("\n").length == 1) {
      val resultAlias = apiDoc.split(":")(0).trim

      val className = resultAliasesToClasses(resultAlias)
      val fields = play.api.Play.classloader.loadClass(className).getDeclaredFields.toSeq
      val fieldDefinition = fields.map(f => (f.getName, rewriteFieldType(f.getType.getCanonicalName, classesToResultAliases))).toMap

      s"    ${resultAlias}: ${className}\n" + fields.map(f => s"      ${f.getName}: ${fieldDefinition(f.getName)}").mkString("\n")
    } else {
      apiDoc
    }
  }

  def getApiDocsFromAnnotations(routeEntries: List[RouteEntry] = RoutesHelper.getRouteEntries(), searchClassesInsidePackage: String = ""): List[String] = {

    val routeEntriesWithoutApiDocs = routeEntries.filter(routeEntry => hasMethodAnnotation(routeEntry.scalaClass, routeEntry.scalaMethod))
    validate(routeEntriesWithoutApiDocs)

    val alreadyIncluded = scala.collection.mutable.Set[String]()
    val alreadyIncludedClasses = scala.collection.mutable.Set[String]()

    val reflections = new Reflections(searchClassesInsidePackage, new SubTypesScanner(false))
    val classAnnotations = reflections.getAllTypes.asScala.filter(hasClassAnnotation).toSet
    val resultAliasesToClasses = classAnnotations.map { className => getClassAnnotationDoc(className).trim.split(":")(0) -> className }.toMap

    val apiDocs = routeEntriesWithoutApiDocs.map { routeEntry =>
      val methodAnnotation = getMethodAnnotationDoc(routeEntry.scalaClass, routeEntry.scalaMethod, alreadyIncluded)
      val classApiDocs = (classAnnotations -- alreadyIncludedClasses).map(getClassAnnotationDoc).map(expandClassAnnotationDoc(_, resultAliasesToClasses)).mkString("\n")
      classAnnotations.foreach(alreadyIncludedClasses.add)

      methodAnnotation + classApiDocs
    }

    apiDocs
  }
}
