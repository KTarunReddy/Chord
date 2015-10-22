import akka.actor.ActorRef
import java.security.MessageDigest

/**
 * @author KALYAN
 */
class Finger(start: BigInt, range: Range, var node:ActorRef) {
  
  var HASH_TYPE = "SHA-1"
  
  def getStart(): BigInt = {
    return this.start
  }
  
  def getRange(): Range = {
    return this.range
  }
  
  def getNode(): ActorRef = {
    return this.node
  }
  
  def getHash():BigInt ={
    return getHash(node.toString());
  }
  
  def setNode(newNode:ActorRef):Unit ={
    this.node=newNode
  }

  def print:String={
    return ("Start: %s, End: %s, Node: %s".format(start,range.getEnd,getHash()))
  }
  
  def getHash(input:String): BigInt = {
    val md = MessageDigest.getInstance(HASH_TYPE)
    val hashString = BigInt(md.digest(input.getBytes("UTF-8")).map("%02x".format(_)).mkString,16)
    return hashString
  } 
  
}