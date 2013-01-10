    
 package models
    
 case class MdlPicture (
 	vidPicture : Long,
	vHyperlink : String,
	vAlternateText : String,
	vCaption : String
    )  {
    
      def this() = this(0, "", "", "")

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vidPicture.toString -> vidPicture.toString
    
      def compare(a: MdlPicture, b: MdlPicture) = a.vidPicture.compareTo(b.vidPicture)
}
    