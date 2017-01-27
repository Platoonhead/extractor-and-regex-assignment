object URL{

   def apply(protocol:String,domain:String,path:String,params:Map[String,String]):String = {

    "protocol is " + protocol + " in domain " +domain+ "@ path"+path+ " with parameters "+params

   }

   def unapply(url:String):Option[(String , String, String, Map[String, String])] = {

      //basic implementation of Url parsing using unapply  
      try{
      val urlparts = url split "://"           
      val urlparts1 = urlparts(1) split '/'
      val parts = urlparts1(2) split '?'
      val path ="/"+urlparts1(1) +"/"+ parts(0)
      val fragments = parts(1) split '&'
      val mymap = for{
		i <- fragments
      }yield i.split('=')(0) -> i.split('=')(1)
      Some(urlparts(0),urlparts1(0), path, mymap.toMap)
      }
      catch{
        case ex :Exception => println(ex)    
	None 
       } 
   }

}

object Q1Basic extends App{
	val protocol = "https"

	val domain = "aws.amazon.com"

	val path ="/console/home"

	val params = Map("state" -> "hash", "isauthcode" -> "true", "code"-> "112")

	println(URL.apply(protocol, domain, path, params))
	val url = "https://aws.amazon.com/console/home?state=hash&isauthcode=true&code=112"
	println(URL.unapply(url))

}
