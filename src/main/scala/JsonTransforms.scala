import cc.spray.json._

object JsonTransforms {
  implicit def jsonTransforms(v: JsValue) = new {
    def transform[A: JsonReader, B: JsonWriter](field: String, fn: A => B) = {
      val result = fn(v.asJsObject.fields(field).convertTo[A])
      JsObject(v.asJsObject.fields + (field -> result.toJson))
    }
    def rename(from: String, to: String) =
      JsObject(v.asJsObject.fields + (to -> v.asJsObject.fields(from)) - from)

    def add[A: JsonWriter](fieldValues: (String, A)*) =
      JsObject(v.asJsObject.fields ++ (fieldValues.map { fv => (fv._1 -> fv._2.toJson) }))

    def remove(field: String) = JsObject(v.asJsObject.fields - field)

    def apply(key: String) = v.asJsObject.fields(key)
  }
}
