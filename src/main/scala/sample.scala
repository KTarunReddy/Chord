import java.security.MessageDigest

/**
 * @author KALYAN
 */
object sample extends App{

      
      val input : String = "11"
      val md = MessageDigest.getInstance("SHA-1")
      val hexString= md.digest(input.getBytes("UTF-8")).map("%02x".format(_)).mkString
      System.out.println(hexString)
      var node : BigInt = BigInt(hexString.trim(), 16);
      System.out.println(node)
    
  }