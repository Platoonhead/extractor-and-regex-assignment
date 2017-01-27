object EMAIL{

	def unapply(url:String):Option[(String , String)] = {
          
            val regex = """([a-zA-Z0-9]+)@([a-zA-Z.]+)""".r //regx used to split Email (a@b.c)

            url match {
		         case regex (user, domain) => Option(user,domain)
		         case _ => None
                      }

	}
}

object Q2 extends App{

 val email = "knol@knoldus.com"
 println("\n\n________________EMAIL Extractor Says__________________\n")
 val result = EMAIL.unapply(email) //UNAPPLY
 if(result != None){
                    val fragmentsOfUrl = result.toList(0)
                    println("user------------> :"+fragmentsOfUrl._1)
                    println("Domain----------> :"+fragmentsOfUrl._2)
                   }

else println("Email not in correct format, hint: Must be in a@b.c format")
println("\n______________________________________________________\n\n")

}
