    
 package models
    
 case class MdlTopics (
 	vTopicsIndex : Long,
	vTopic : String,
	vTopicShortDescription : String,
	vTopicDetailedDescription : String
    )  {
    
      def this() = this(0, "", "", "")

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vTopicsIndex.toString -> vTopicsIndex.toString
    
      def compare(a: MdlTopics, b: MdlTopics) = a.vTopicsIndex.compareTo(b.vTopicsIndex)
}
    