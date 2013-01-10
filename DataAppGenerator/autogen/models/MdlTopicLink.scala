    
 package models
    
 case class MdlTopicLink (
 	vidTopicLink : Long,
	vTopic : Long,
	vLink : String,
	vDescription : String
    )  {
    
      def this() = this(0, 0, "", "")

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vidTopicLink.toString -> vidTopicLink.toString
    
      def compare(a: MdlTopicLink, b: MdlTopicLink) = a.vidTopicLink.compareTo(b.vidTopicLink)
}
    