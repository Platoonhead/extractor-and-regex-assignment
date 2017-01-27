object URL{

	def apply(protocol:String,domain:String,path:String,params:Map[String,String]):String = {

		 val keys    = params.keys.toList
		 val values  = params.values.toList
		 val keyValuePairs = (for{iterator <- 0 to keys.size-1}yield "&"+keys(iterator)+"="+values(iterator)).mkString
		 val parameters    = keyValuePairs.replaceFirst("&","") // remove 1st "&" from string created 
		 protocol+"://"+domain+path+"?"+parameters
	}

	def unapply(url:String):Option[(String , String,String,Map[String,String])] = {
          
            val regex = """(https?)://([a-zA-Z0-9.]*)\/([a-zA-Z0-9/]*)\?([a-zA-Z0-9&=]*)""".r //regx used to split URL

            url match {
		         case regex (protocol, domain,path,params) =>{val paramFragments = params.split('&').toList
                                                                     val generatedMap = for{  // creating param map
                                                                     iterator <-  paramFragments
                                                                     }yield iterator.split('=')(0) -> iterator.split('=')(1)
                                                                     Option(protocol,domain,path,generatedMap.toMap)}
		         case _ => None
                      }
	}
}

object Q1 extends App{

 val url = "https://aws.amazon.co.in/console/home/abc?state=hash&isauthcode=true&code=112&bac=234&ghggj=2425"
 println("\n\n________________Extractor Says________________________\n")
 val result = URL.unapply(url) //UNAPPLY
 if(result != None){
                    val fragmentsOfUrl = result.toList(0)
                    println("Protocol--------> :"+fragmentsOfUrl._1)
                    println("Domain----------> :"+fragmentsOfUrl._2)
                    println("Path------------> :"+"/"+fragmentsOfUrl._3)
                    println("Parameters are--> :"+fragmentsOfUrl._4)
		    val preparedUrl    = URL.apply(fragmentsOfUrl._1,fragmentsOfUrl._2,fragmentsOfUrl._3,fragmentsOfUrl._4) //APPLY
                    println("\n\n______________Generated URL FROM EXTRACTS____________")
		    println("\n\n"+preparedUrl)
                   }

else println("URL not in correct format , hint: Must be missing some characters")
println("\n______________________________________________________\n\n")

}
