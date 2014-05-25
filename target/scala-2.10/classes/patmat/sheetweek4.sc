package patmat


object sheetweek4 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  
    // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match
  {
    case l:Leaf => l.weight
    case f:Fork => weight(f.left) + weight(f.right)
  }                                               //> weight: (tree: patmat.sheetweek4.CodeTree)Int

  def chars(tree: CodeTree): List[Char] = tree match
  {
    case l:Leaf => List(l.char)
    case f:Fork => chars(f.left) ::: chars(f.right)
  }                                               //> chars: (tree: patmat.sheetweek4.CodeTree)List[Char]

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
                                                  //> makeCodeTree: (left: patmat.sheetweek4.CodeTree, right: patmat.sheetweek4.Co
                                                  //| deTree)patmat.sheetweek4.Fork

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
  }                                               //> times: (chars: List[Char])List[(Char, Int)]
  
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
    
      def singleton(trees: List[CodeTree]): Boolean = trees.size == 1
                                                  //> singleton: (trees: List[patmat.sheetweek4.CodeTree])Boolean
    
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
 }                                                //> combine: (trees: List[patmat.sheetweek4.CodeTree])List[patmat.sheetweek4.Co
                                                  //| deTree]
   
  def until(singleTreeCheck: (List[CodeTree])=>Boolean, aggregator: ( List[CodeTree])=> List[CodeTree])(trees:  List[CodeTree]): List[CodeTree] =
  {
    trees match
    {
      case Nil =>Nil
      case head::tail => if(singleTreeCheck(trees)) trees else until(singleTreeCheck,aggregator){aggregator(trees)}
    }
  }                                               //> until: (singleTreeCheck: List[patmat.sheetweek4.CodeTree] => Boolean, aggre
                                                  //| gator: List[patmat.sheetweek4.CodeTree] => List[patmat.sheetweek4.CodeTree]
                                                  //| )(trees: List[patmat.sheetweek4.CodeTree])List[patmat.sheetweek4.CodeTree]
                                                  //| 
                type Bit = Int
            
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
    
    
  }                                               //> decode: (tree: patmat.sheetweek4.CodeTree, bits: List[patmat.sheetweek4.Bit
                                                  //| ])List[Char]

  
  val root = until(singleton,combine)(makeOrderedLeafList(codes))
                                                  //> root  : List[patmat.sheetweek4.CodeTree] = List(Fork(Fork(Leaf(z,31),Fork(L
                                                  //| eaf(m,22),Fork(Leaf(b,11),Fork(Leaf(f,7),Fork(Fork(Leaf(A,1),Leaf(a,2),List
                                                  //| (A, a),3),Leaf(x,5),List(A, a, x),8),List(f, A, a, x),15),List(b, f, A, a, 
                                                  //| x),26),List(m, b, f, A, a, x),48),List(z, m, b, f, A, a, x),79),Leaf(W,91),
                                                  //| List(z, m, b, f, A, a, x, W),170))
  times("This function creates a code tree which is optimal to encode the text".toList)
                                                  //> res0: List[(Char, Int)] = List((e,9), (s,3), (x,1), (n,3), (T,1), (t,8), (u
                                                  //| ,1), (f,1), (a,3), (m,1), (i,5), ( ,12), (l,1), (p,1), (c,5), (h,4), (r,2),
                                                  //|  (w,1), (o,5), (d,2))
  println(root)                                   //> List(Fork(Fork(Leaf(z,31),Fork(Leaf(m,22),Fork(Leaf(b,11),Fork(Leaf(f,7),Fo
                                                  //| rk(Fork(Leaf(A,1),Leaf(a,2),List(A, a),3),Leaf(x,5),List(A, a, x),8),List(f
                                                  //| , A, a, x),15),List(b, f, A, a, x),26),List(m, b, f, A, a, x),48),List(z, m
                                                  //| , b, f, A, a, x),79),Leaf(W,91),List(z, m, b, f, A, a, x, W),170))
  println(combine(makeOrderedLeafList(codes)))    //> List(Fork(Leaf(A,1),Leaf(a,2),List(A, a),3), Leaf(x,5), Leaf(f,7), Leaf(b,1
                                                  //| 1), Leaf(m,22), Leaf(z,31), Leaf(W,91))
  // val combined = combine(makeOrderedLeafList(codes))
   println(combine(combine(combine(makeOrderedLeafList(codes)))))
                                                  //> List(Leaf(b,11), Fork(Leaf(f,7),Fork(Fork(Leaf(A,1),Leaf(a,2),List(A, a),3)
                                                  //| ,Leaf(x,5),List(A, a, x),8),List(f, A, a, x),15), Leaf(m,22), Leaf(z,31), L
                                                  //| eaf(W,91))
}