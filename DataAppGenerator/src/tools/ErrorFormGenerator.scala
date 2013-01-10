package tools

object ErrorFormGenerator {
  
  def errorForm = {"""
@(errors: String, referrer: String)

<div>
	<p>@errors<p>
	<p>Originating page: @referrer<p>
</div>
 <a href="#" onclick="history.go(-1)">Return to Form</a>  
"""
  }
  
  def main(args: Array[String]) {
	val errorFormString = errorForm
    DataAppGenerator.writeStringToFile("autogen/formError.scala.html", errorFormString)
	
  }  

}