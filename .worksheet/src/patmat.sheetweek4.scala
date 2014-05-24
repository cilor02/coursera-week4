package patmat


object sheetweek4 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(80); 
  println("Welcome to the Scala worksheet")
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree;$skip(893); 
  
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
  };System.out.println("""makeOrderedLeafList: (freqs: List[(Char, Int)])List[patmat.sheetweek4.Leaf]""");$skip(250); 



    def remove(lst:List[(Char,Int)], tple:Tuple2[Char,Int]):List[(Char,Int)] =
    {
      lst match
      {
        case Nil => Nil
        case (tple._1,tple._2)::tail => lst.tail
        case _ => lst.head::remove (lst.tail,tple)
      }
    };System.out.println("""remove: (lst: List[(Char, Int)], tple: (Char, Int))List[(Char, Int)]""");$skip(113); 
    
    
    val codes = List[(Char,Int)] (('a',2),('b',11),('z',31),('x',5),('f',7),('m',22),('W',91),('A',1));System.out.println("""codes  : List[(Char, Int)] = """ + $show(codes ));$skip(44); 
    
   
    println(remove(codes,('a',4)));$skip(143); 
        def smallest (freqs: List[(Char, Int)]):(Char,Int) =
    {
      freqs.foldLeft(freqs.head)((a,b) => if (a._2 > b._2) b else a )
    };System.out.println("""smallest: (freqs: List[(Char, Int)])(Char, Int)""");$skip(45); 
    
    println(makeOrderedLeafList(codes))}
}
