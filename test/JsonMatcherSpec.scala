package test.lib

import org.specs2.mutable._

import play.api.libs.json._
import play.api.test.Helpers._

import no.samordnaopptak.apidoc.JsonMatcher._


class JsonMatcherSpec extends Specification {
  "Json matcher" should {


    // empty tests

    "match empty obj" in {
      matchJson(Json.obj(),Json.obj())
    }

    "match non-empty objs" in {
      matchJson(Json.obj(),Json.obj("name" -> "value")) should throwA[JsonMatcherException]
    }

    "match empty array" in {
      matchJson(Json.arr(),Json.arr())
    }

    "match non-empty array" in {
      matchJson(Json.arr(),Json.arr(Json.arr())) should throwA[JsonMatcherException]
    }



    // simple obj tests

    "match obj with string" in {
      matchJson(
        Json.obj("a" -> "b"),
        Json.obj("a" -> "b")
      )
    }

    "match obj with diff key string" in {
      matchJson(
        Json.obj("a" -> "b"),
        Json.obj("c" -> "b")
      ) should throwA[JsonMatcherException]
    }

    "match obj with diff value string" in {
      matchJson(
        Json.obj("a" -> "b"),
        Json.obj("a" -> "c")
      ) should throwA[JsonMatcherException]
    }



    // simple array tests

    "match obj with array" in {
      matchJson(
        Json.obj("a" -> Json.arr()),
        Json.obj("a" -> Json.arr())
      )
    }

    "match obj with array" in {
      matchJson(
        Json.obj("a" -> Json.arr()),
        Json.obj("a" -> Json.arr())
      )
    }

    "match something else" in {
      matchJson(
        Json.obj("data" -> Json.arr(Json.obj("name" -> "ARole"))),
        Json.obj("data" -> Json.arr(Json.obj("name" -> "ARole")))
      )
    }



    // simple array/obj mix tests

    "match obj with array doesnt match" in {
      matchJson(
        Json.obj(),
        Json.arr()
      ) should throwA[JsonMatcherException]
    }



    // other fields, object

    "match object with other fields" in {
      matchJson(
        Json.obj(___allowOtherFields),
        Json.obj()
      )
    }

    "match object with other fields 2" in {
      matchJson(
        Json.obj(___allowOtherFields),
        Json.obj("a" -> "b")
      )
    }

    "match object with other fields 3" in {
      matchJson(
        Json.obj("hepp" -> "aiai", ___allowOtherFields),
        Json.obj("hepp" -> "aiai", "b" -> "c")
      )
    }

    "match object with diff fields, containing others" in {
      matchJson(
        Json.obj("hepp" -> "aiai", ___allowOtherFields),
        Json.obj("b" -> "c")
      ) should throwA[JsonMatcherException]
    }



    // other fields, array

    "match array with other fields 1" in {
      matchJson(
        Json.arr(___allowOtherValues),
        Json.arr()
      )
    }

    "match array with other fields 2" in {
      matchJson(
        Json.arr(___allowOtherValues),
        Json.arr("a")
      )
    }

    "match arr with other fields 3" in {
      matchJson(
        Json.arr("hepp", ___allowOtherValues),
        Json.arr("hepp", "b")
      )
    }

    "match array with diff fields, containing others" in {
      matchJson(
        Json.arr("hepp", ___allowOtherValues),
        Json.arr("b")
      ) should throwA[JsonMatcherException]
    }



    // num field, array

    "match array with num field" in {
      matchJson(
        Json.arr(___numElements, 0),
        Json.arr()
      )
    }


    "fail match array with num field, when too few elements" in {
      matchJson(
        Json.arr(___numElements, 1),
        Json.arr()
      ) should throwA[JsonMatcherException]
    }

    "fail match array with num field, when too many elements" in {
      matchJson(
        Json.arr(___numElements, 1),
        Json.arr("a","b","c")
      ) should throwA[JsonMatcherException]
    }


    "match array with num field 2" in {
      matchJson(
        Json.arr("hello",___numElements, 1),
        Json.arr("hello")
      )
    }

    "match array with num field 2b" in {
      matchJson(
        Json.arr(___numElements, 1),
        Json.arr("hello")
      )
    }

    "fail match array with num field 2" in {
      matchJson(
        Json.arr("hello", ___numElements, 3),
        Json.arr("hello1", "hello2", "hello3")
      ) should throwA[JsonMatcherException]
    }
 
    "match array with num field 3" in {
      matchJson(
        Json.arr(___numElements, 1, "hello"),
        Json.arr("hello")
      )
    }

    "match array with num field 4" in {
      matchJson(
        Json.arr("a", ___numElements, 2, "hello"),
        Json.arr("a", "hello")
      )
    }

    "match array with num field 4b" in {
      matchJson(
        Json.arr(___numElements, 4),
        Json.arr("a", "hello", "b", "c")
      )
    }


    // num field, object

    "match object with num field" in {
      matchJson(
        Json.obj(___numElements -> 0),
        Json.obj()
      )
    }

    "match object with num field 2" in {
      matchJson(
        Json.obj("hello" -> 5,___numElements -> 1),
        Json.obj("hello" -> 5)
      )
    }

    "match object with num field 2b" in {
      matchJson(
        Json.obj(___numElements -> 1),
        Json.obj("hello" -> 5)
      )
    }

    "fail match object with num field, too few" in {
      matchJson(
        Json.obj(___numElements -> 1),
        Json.obj()
      ) should throwA[JsonMatcherException]
    }

    "fail match object with num field, too many" in {
      matchJson(
        Json.obj(___numElements -> 0),
        Json.obj("hello" -> 5)
      ) should throwA[JsonMatcherException]
    }

    "fail match array with wrong order" in {
      matchJson(
        Json.arr(2,3),
        Json.arr(3,2)
      ) should throwA[JsonMatcherException]

      matchJson(
        Json.arr(2,3,___allowOtherValues),
        Json.arr(3,2)
      ) should throwA[JsonMatcherException]

      matchJson(
        Json.arr(),
        Json.arr(3)
      ) should throwA[JsonMatcherException]
    }

    "match array with wrong order, but legal anyway because of ___allowOtherValues" in {
      matchJson(
        Json.arr(2,3,___allowOtherValues),
        Json.arr(9,2,3)
          //     ^
          //     ignored
      )
    }


    // array and objects tests

    "match arrays and objects, etc." in {
      matchJson(
        Json.obj("data"      -> Json.arr(Json.obj("name"          -> "ARole",
                                                  "numUsers"      -> 1,
                                                  "actions"       -> Json.arr(___allowOtherValues),
                                                   ___numElements -> 4),
                                         ___numElements, 5),
                 "hitsTotal" -> 1),
        Json.obj("data"      -> Json.arr(Json.obj("name"     -> "ARole",
                                                  "numUsers" -> 1,
                                                  "somethingelse" -> Json.arr("hello"),
                                                  "actions"  -> Json.arr("a","b",Json.obj("a" -> "b"))),
                                         "d","e","f",Json.arr("f","g")),
                 "hitsTotal" -> 1)
      )
    }
  }
}