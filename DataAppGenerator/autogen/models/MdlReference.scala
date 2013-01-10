    
 package models
    
 case class MdlReference (
 	vidReference : Long,
	vTitle : String,
	vText : String,
	vLink : String
    )  {
    
      def this() = this(0, "", "", "")

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vidReference.toString -> vidReference.toString
    
      def compare(a: MdlReference, b: MdlReference) = a.vidReference.compareTo(b.vidReference)
}
    