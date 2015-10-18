
import akka.actor.{ ActorRef, ActorSystem, Props, Actor }

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

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
    
    for (i <- 0 until numNodes) {
       //chordNodes = system.actorOf(Props(new ChordNode(numNodes, numRequests, i)), name = "Node"+i)
    }
    
    def receive = {
      case "start" =>
        
       
    }
  }

  class ChordNode (numNodes : Int, numRequests : Int, identifier : Int){
      
    def initFingerTable{
      
    }
    
    def findPredecessor{
      
    }
    
    def findSuccessor{
      
    }
    
    def updateFingerTable{
      
    }
    
  }