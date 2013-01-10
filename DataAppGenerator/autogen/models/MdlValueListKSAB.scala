    
 package models
    
 case class MdlValueListKSAB (
 	vKSAB : String
    )  {
    
      def this() = this("")

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vKSAB.toString -> vKSAB.toString
    
      def compare(a: MdlValueListKSAB, b: MdlValueListKSAB) = a.vKSAB.compareTo(b.vKSAB)
}
    