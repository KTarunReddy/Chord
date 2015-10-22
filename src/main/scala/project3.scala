
import akka.actor.{ ActorRef, ActorSystem, Props, Actor }

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.math.BigInt

import java.security.MessageDigest

//Messages
sealed trait ChordMessage

case class Initialize(nodeName:String, nodeHash:BigInt, isFirstNode:Boolean) extends ChordMessage
case class findFinger(node:ActorRef, i:Int, start:BigInt) extends ChordMessage
case class updateFinger(before:BigInt, i:Int, node:ActorRef, nodeHash:BigInt) extends ChordMessage
case class foundFinger(i:Int, successor:ActorRef) extends ChordMessage

object project3 extends App{
  
    /* 
       *  numNodes are the total number of nodes(actors) with which network is built 
       *  numRequests are the total number of requests made by each actor
    */
    var numNodes = 0
    var numRequests = 0
    
    var ID_LENGTH = 160
    var HASH_TYPE = "SHA-1"
    
    if(args.length != 2){
      println(" !!! Invalid Input !!!\n "+
              " Input should be of type: project3.scala numNodes numRequests ")
      System.exit(1)
    }
    else{
      numNodes = args(0).toInt
      numRequests = args(1).toInt
      
      println("numNodes:" + numNodes + " -- numRequests:" + numRequests)
      chord_protocol(numNodes, numRequests)
    }
  
    var chordNodes = new ArrayBuffer[ActorRef]()
        
    def chord_protocol(numNodes : Int, numRequests : Int){   
      val system = ActorSystem("project3")
      
      createNetwork("node1", system)
      
      for (i <- 1 until numNodes-1) {
        joinNetwork("node"+i, system)
      }
    }
      
    def createNetwork(nodeName : String, sys : ActorSystem) {
      
      System.out.println(node + " is the first node in network")
      
      val nodeHash = getHash(nodeName)
      val node = sys.actorOf(Props(new ChordNode(numRequests, nodeName, nodeHash, "create")));
      var isFirstNode : Boolean = true
      
      node ! Initialize(nodeName, nodeHash, isFirstNode)
      Thread.sleep(1000)
       
    }
    
    def joinNetwork(node : String, sys : ActorSystem) {
      
      System.out.println(node + " wants to join the network")
      val nodeHash = getHash(node)
      chordNodes += sys.actorOf(Props(new ChordNode(numRequests, node, nodeHash, "join")));
    }
    
    def leaveNetwork(node : String) {
    
      System.out.println(node + " wants to leave the network")
      
    }
    
    def getHash(input:String): BigInt = {
        val md = MessageDigest.getInstance(HASH_TYPE)
        val hashString = BigInt(md.digest(input.getBytes("UTF-8")).map("%02x".format(_)).mkString.trim(),16)
        return hashString
    }
    
    class ChordNode (numRequests : Int, node : String, nodeHash : BigInt, opType : String) extends Actor{
  
        var isExists : Boolean = false
        var predecessor = self
        var successor = self 
        var fingerTable = new Array[Finger](ID_LENGTH)
        
        for(i <- 0 until ID_LENGTH-1) {
          val start = (nodeHash + BigInt(2).pow(i-1)) % (BigInt(2).pow(ID_LENGTH))
          val end = (nodeHash + BigInt(2).pow(i)) % (BigInt(2).pow(ID_LENGTH))
          val range= new Range(true, start, end, false)
        
          fingerTable(i)= new Finger(start, range, self)
        }
  
        def InitializeFinger() {  
            System.out.println("Started Finger Initialization");
            
            fingerTable(0).setNode(successor)
      
            for(i<-0 until ID_LENGTH-1){
                val range = new Range(true, nodeHash, fingerTable(i).getHash(), true)
      
                if(range.isInclude(fingerTable(i+1).getStart())) {
                    fingerTable(i).setNode(fingerTable(i-1).getNode())
                }else{
                  if(exist!=null)
                    exist ! findFinger(self, i, fingerTable(i).getStart())
                }
            }
        }
  
        def updateOthers():Unit = {
          for(i <- 0 to ID_LENGTH-1) {
            val position=(nodeHash - BigInt(2).pow(i)+BigInt(2).pow(ID_LENGTH)+1)%BigInt(2).pow(ID_LENGTH)
            successor ! updateFinger(position, i, self, nodeHash)
          }
        }
    
        def getHash(input:String): String = {
            val md = MessageDigest.getInstance(HASH_TYPE)
            val hexString= md.digest(input.getBytes("UTF-8")).map("%02x".format(_)).mkString
            return hexString
        } 
    
        def closestPreceedingFinger(id:BigInt): ActorRef = {   
            val range=new Range(false, nodeHash, id, false)
          
            for(i <- ID_LENGTH-1 to 0 by -1){
              if(range.isInclude(fingerTable(i).getHash()))
                return fingerTable(i).node;
            }
            return self;
          }
    
        override def receive: Receive = {
    
          case Initialize(nodeName:String, nodeHash:BigInt, isFirstNode : Boolean)=>{
              
              
          }
            
          case findFinger(node:ActorRef, i:Int, start:BigInt)=>{
                
                val range = new Range(false, nodeHash, fingerTable(0).getHash(), true)
              
                if(range.isInclude(start)){
                  node ! foundFinger(i,successor)
                }else{
                  val target = closestPreceedingFinger(start)
                  target ! findFinger(node,i,start)
                }
            }
        
            case foundFinger(i:Int, successor:ActorRef)=>{
                this.fingerTable(i).setNode(successor)
            }
        
            case updateFinger(before:BigInt,i:Int,node:ActorRef,nodeHash:BigInt) => { 
        
                if(node != self) {
              
                  val range1 = new Range(false, nodeHash, fingerTable(0).getHash(), true)
                  
                  if (range1.isInclude(before)) { 
                
                      val range2=new Range(false, nodeHash, fingerTable(i).getHash(), false)
                                
                      if(range2.isInclude(nodeHash)){
                  fingerTable(i).setNode(node)
                  predecessor ! updateFinger(nodeHash, i, node, nodeHash)
                }
              }else{
                val target = closestPreceedingFinger(before)
                target ! updateFinger(before,i,node,nodeHash)
              }
            }
          }
      }
  }
}  


