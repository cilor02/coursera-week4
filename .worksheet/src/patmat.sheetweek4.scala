package patmat


object sheetweek4 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(80); 
  println("Welcome to the Scala worksheet")
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree;$skip(347); 
  
    // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match
  {
    case l:Leaf => l.weight
    case f:Fork => weight(f.left) + weight(f.right)
  };System.out.println("""weight: (tree: patmat.sheetweek4.CodeTree)Int""");$skip(146); 

  def chars(tree: CodeTree): List[Char] = tree match
  {
    case l:Leaf => List(l.char)
    case f:Fork => chars(f.left) ::: chars(f.right)
  };System.out.println("""chars: (tree: patmat.sheetweek4.CodeTree)List[Char]""");$skip(137); 

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right));System.out.println("""makeCodeTree: (left: patmat.sheetweek4.CodeTree, right: patmat.sheetweek4.CodeTree)patmat.sheetweek4.Fork""");$skip(362); 

  def times(chars: List[Char]): List[(Char, Int)] =
  {
    def timesMap(chars: List[Char],map:Map[Char,Int]):Map[Char, Int] =
    {
       chars match
       {
         case Nil => map
         case _ => timesMap(chars.tail,map.+(chars.head ->(map.getOrElse(chars.head, 0) + 1)))
       }
    }
    val map = Map[Char,Int]()
    timesMap(chars,map).toList
  };System.out.println("""times: (chars: List[Char])List[(Char, Int)]""");$skip(707); 
  
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
    
    println(makeOrderedLeafList(codes));$skip(75); 
    
      def singleton(trees: List[CodeTree]): Boolean = trees.size == 1;System.out.println("""singleton: (trees: List[patmat.sheetweek4.CodeTree])Boolean""");$skip(672); 
    
     def combine(trees: List[CodeTree]): List[CodeTree] =
  {
    
   def insert(trees:List[CodeTree],codeTree: CodeTree): List[CodeTree] =
   {
     val newTrees:List[CodeTree] = trees match
     {
       case Nil => trees ::: List[CodeTree](codeTree)
       case head::tail => if(weight(head) > weight(codeTree)) List[CodeTree](codeTree):::trees else List[CodeTree](trees.head):::insert(trees.tail,codeTree)
     }
     newTrees
   }
   
   val newTrees = trees match
  {
    case head::tail if (singleton(trees)) => trees
    case head::tail => insert( trees.drop(2), makeCodeTree(head,tail.head))
    case head::Nil => trees
    case _ =>trees
  }
   newTrees
 };System.out.println("""combine: (trees: List[patmat.sheetweek4.CodeTree])List[patmat.sheetweek4.CodeTree]""");$skip(323); 
   
  def until(singleTreeCheck: (List[CodeTree])=>Boolean, aggregator: ( List[CodeTree])=> List[CodeTree])(trees:  List[CodeTree]): List[CodeTree] =
  {
    trees match
    {
      case Nil =>Nil
      case head::tail => if(singleTreeCheck(trees)) trees else until(singleTreeCheck,aggregator){aggregator(trees)}
    }
  }
                type Bit = Int;System.out.println("""until: (singleTreeCheck: List[patmat.sheetweek4.CodeTree] => Boolean, aggregator: List[patmat.sheetweek4.CodeTree] => List[patmat.sheetweek4.CodeTree])(trees: List[patmat.sheetweek4.CodeTree])List[patmat.sheetweek4.CodeTree]""");$skip(566); 
            
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] =
  {
    def extractLeaf (tree: CodeTree, root: CodeTree, bits: List[Bit]): List[Char]  =
    {
    tree match
    {
      case f:Fork => if(bits.head == 0) extractLeaf(f.left,root,bits.tail) else extractLeaf(f.right,root,bits.tail)
      case l:Leaf => List(l.char) ::: ( if(bits.head == 0) decode(root,bits.tail) else decode(root,bits.tail))
    }
  }
    bits match
    {
      case Nil => Nil
      case _   => extractLeaf(tree,tree,bits)
    }
    
    
  };System.out.println("""decode: (tree: patmat.sheetweek4.CodeTree, bits: List[patmat.sheetweek4.Bit])List[Char]""");$skip(71); 

  
  val root = until(singleton,combine)(makeOrderedLeafList(codes));System.out.println("""root  : List[patmat.sheetweek4.CodeTree] = """ + $show(root ));$skip(88); val res$0 = 
  times("This function creates a code tree which is optimal to encode the text".toList);System.out.println("""res0: List[(Char, Int)] = """ + $show(res$0));$skip(16); 
  println(root);$skip(47); 
  println(combine(makeOrderedLeafList(codes)));$skip(122); 
  // val combined = combine(makeOrderedLeafList(codes))
   println(combine(combine(combine(makeOrderedLeafList(codes)))))}
}
