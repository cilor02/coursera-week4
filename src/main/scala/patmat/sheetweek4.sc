package patmat


object sheetweek4 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  
    def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
  {
    def smallest (freqs: List[(Char, Int)]):(Char,Int) =
    {
      freqs.foldLeft(freqs.head)((a,b)=>if(a._2 < b._2) a else b)
    }
    
    def remove(lst:List[(Char,Int)], tple:Tuple2[Char,Int]):List[(Char,Int)] =
    {
      lst match
      {
        case Nil => Nil
        case (tple._1,tple._2)::tail => lst.tail
        case _ => lst.head::remove (lst.tail,tple)
      }
    }
    
    freqs match
    {
      case Nil => Nil
      case head::Nil => List(new Leaf(head._1,head._2))
      case head::tail => val small = smallest(freqs);List(new Leaf(small._1,small._2)):::makeOrderedLeafList(remove(freqs,small))
    }
  }                                               //> makeOrderedLeafList: (freqs: List[(Char, Int)])List[patmat.sheetweek4.Leaf]
                                                  //| 



    def remove(lst:List[(Char,Int)], tple:Tuple2[Char,Int]):List[(Char,Int)] =
    {
      lst match
      {
        case Nil => Nil
        case (tple._1,tple._2)::tail => lst.tail
        case _ => lst.head::remove (lst.tail,tple)
      }
    }                                             //> remove: (lst: List[(Char, Int)], tple: (Char, Int))List[(Char, Int)]
    
    
    val codes = List[(Char,Int)] (('a',2),('b',11),('z',31),('x',5),('f',7),('m',22),('W',91),('A',1))
                                                  //> codes  : List[(Char, Int)] = List((a,2), (b,11), (z,31), (x,5), (f,7), (m,2
                                                  //| 2), (W,91), (A,1))
    
   
    println(remove(codes,('a',4)))                //> List((a,2), (b,11), (z,31), (x,5), (f,7), (m,22), (W,91), (A,1))
        def smallest (freqs: List[(Char, Int)]):(Char,Int) =
    {
      freqs.foldLeft(freqs.head)((a,b) => if (a._2 > b._2) b else a )
    }                                             //> smallest: (freqs: List[(Char, Int)])(Char, Int)
    
    println(makeOrderedLeafList(codes))           //> List(Leaf(A,1), Leaf(a,2), Leaf(x,5), Leaf(f,7), Leaf(b,11), Leaf(m,22), Le
                                                  //| af(z,31), Leaf(W,91))
}