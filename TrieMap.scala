import scala.annotation.tailrec


sealed trait Trie[T] {
  def put(key:String,value:T)
  def get(key:String):Option[T]

}

class TrieMap[T] extends Trie[T] {

  case class Leaf(leafStr:String, leafVal:T)
  private val mChars = new Array[AnyRef](256)
  private var mPrefixVal:Option[T] = None

  def isEmpty: Boolean = {
    ! ( mPrefixVal.isDefined || mChars.exists(_!= null) )
  }

  def put(key:String,value:T): Unit ={

    if(key.length > 0) {
      val c = key.charAt(0)
      mChars(c) match {
        case null =>
          mChars(c) = Leaf(key, value)
        case child:TrieMap[T] =>
          child.put(key.substring(1), value)
        case leaf:Leaf =>
          val branch = new TrieMap[T]()
          branch.put(key.substring(1), value)
          branch.put(leaf.leafStr.substring(1), leaf.leafVal)
          mChars(c) = branch
      }
    }else{
      mPrefixVal = Some(value)
    }
  }

  @tailrec final def get(key:String):Option[T] = {

    if (key.length > 0) {
      val c = key.charAt(0)
      mChars(c) match {
        case null => None
        case child: TrieMap[T] =>
          child.get(key.substring(1))
        case leaf: Leaf =>
          if (key == leaf.leafStr)
            Some(leaf.leafVal)
          else
            None
      }
    }else{
      mPrefixVal
    }
  }



}

object test extends App {

  val trieMap = new TrieMap[Int]
  trieMap.put("Jav",0)
  trieMap.put("Java",1)
  trieMap.put("Javac",2)
  println("the value of Java is " + trieMap.get("Java"))

}
