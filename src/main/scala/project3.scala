
import akka.actor.{ ActorRef, ActorSystem, Props, Actor }

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import java.security.MessageDigest

//Messages
sealed trait ChordMessage

case class InitializeFingerTable() extends ChordMessage

object project3 extends App{
  
    /* 
       *  numNodes are the total number of nodes(actors) with which network is built 
       *  numRequests are the total number of requests made by each actor
    */
    var numNodes = 0
    var numRequests = 0
    
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
  
    def chord_protocol(numNodes : Int, numRequests : Int){
      
      val system = ActorSystem("project3")
      
      val master = system.actorOf(Props(new Master(numNodes, numRequests, system)));
      
      master ! "start"
    }
}  

class Master (numNodes : Int, numRequests : Int, system :ActorSystem) extends Actor{

    /*
     *  
     */
    
    var chordNodes = new ArrayBuffer[ActorRef]()
        
    println("Started building network")
    
    for (i <- 0 until numNodes-1) {
        chordNodes += system.actorOf(Props(new ChordNode(numNodes, numRequests, i)), name = "Node" + i)
    }
    
    println("Completed building network")
    
    var keyList = Array(0,3,4,7)
    var keyHashList = Array[String]()
    var nodeMatchList = Array[BigInt]()
    
    for (j <- 0 until keyList.length-1) {
        
      var keyHash : String = getSHA1(keyList(j).toString())
      keyHashList = keyHashList :+ keyHash
              
      var node : BigInt = BigInt(keyHashList(j).trim(), 16);
      nodeMatchList = nodeMatchList :+ node
      
    }
    
    def getKeyIdSuccessor(input : BigInt) : BigInt =
    {
       for (k <- 0 until chordNodes.size-1)
       {
         
       }
    } 
    
    def getSHA1(input:String): String =
    {
        val md = MessageDigest.getInstance("SHA-1")
        val hexString= md.digest(input.getBytes("UTF-8")).map("%02x".format(_)).mkString
        return hexString
    }
    
    def receive = {
      case "start" =>
        
    }
  }

class ChordNode (numNodes : Int, numRequests : Int, identifier : Int) extends Actor{
  
    var predecessor : String = null
    var successors = new ArrayBuffer[String]
    

    
    var nodeHash : String = getHash(self.path.name)
    //var keyHash : String = getHash(key)
    
    var DEFAULT_METHOD =  "SHA-1"
  
    def receive = {
    
      case "abc" => {
        
        }
    }
    
    def InitializeFinger() {  
      
      System.out.println("Started Finger Initialization");
      
      if (successors.isEmpty)
      {
        //successors += keyHash
      }
    }
  
    def findPredecessor {
    
    }
  
    def findNodeSuccessor {
    
    }
    
    def findIdSuccessor {
      
    }
  
    def updateFinger {
    
    } 
    
    def getHash(input:String): String =
    {
        val md = MessageDigest.getInstance(DEFAULT_METHOD)
        val hexString= md.digest(input.getBytes("UTF-8")).map("%02x".format(_)).mkString
        return hexString
    }    
 
}