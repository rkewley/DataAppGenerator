package tools

object FormFieldImplicitsGenerator {
  
  def generateFormFieldImplicits = """
package controllers

import play.api.data.format._

object FormFieldImplicits {
  // Code merged into future Play release
  //
  implicit def doubleFormat = new Formatter[Double] {
    def bind(key: String, data: Map[String, String]) = Right(data(key).toDouble)
    def unbind(key: String, value: Double) = Map(key -> value.toString)
  }
}
"""
  def main(args: Array[String]) {
	val implicitsString = generateFormFieldImplicits
    DataAppGenerator.writeStringToFile("autogen/FormFieldImplicits.scala", implicitsString)
	
  }
}
