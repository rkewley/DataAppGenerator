    
 package models
    
 case class MdlLink (
 	vidLink : Long,
	vDescription : String,
	vLink : String
    )  {
    
      def this() = this(0, "", "")

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vidLink.toString -> vidLink.toString
    
      def compare(a: MdlLink, b: MdlLink) = a.vidLink.compareTo(b.vidLink)
}
    