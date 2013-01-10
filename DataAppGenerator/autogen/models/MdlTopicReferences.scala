    
 package models
    
 case class MdlTopicReferences (
 	vidTopicReferences : Long,
	vTopic : Long,
	vReference : Long
    )  {
    
      def this() = this(0, 0, 0)

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vidTopicReferences.toString -> vidTopicReferences.toString
    
      def compare(a: MdlTopicReferences, b: MdlTopicReferences) = a.vidTopicReferences.compareTo(b.vidTopicReferences)
}
    