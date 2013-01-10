    
 package models
    
 case class MdlTopicPicture (
 	vidTopicPicture : Long,
	vTopic : Long,
	vHyperlink : String,
	vAlternateText : String,
	vCaption : String
    )  {
    
      def this() = this(0, 0, "", "", "")

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vidTopicPicture.toString -> vidTopicPicture.toString
    
      def compare(a: MdlTopicPicture, b: MdlTopicPicture) = a.vidTopicPicture.compareTo(b.vidTopicPicture)
}
    