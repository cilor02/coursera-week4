package patmat

object TestHuffman extends App {
  val huffman = Huffman
 //  val codes = List[(Char,Int)] (('a',2),('b',11),('z',31),('x',5),('f',7),('m',22),('W',91),('A',1))
//   println(huffman.combine(huffman.combine(huffman.makeOrderedLeafList(codes))))
//val text = "This function creates a code tree which is optimal to encode the text"
//  println(huffman.createCodeTree(text.toList))
 println(huffman.decodedSecret)
 println(huffman.decode(huffman.createCodeTree("tweet".toList),List(0,1,1,1,1,0)))
 
 println(huffman.encode(huffman.createCodeTree("tweet".toList))("tweet".toList))
 
 println(huffman.convert(huffman.createCodeTree("decodedSecret".toList)))
 
 println(huffman.convert(huffman.createCodeTree("huffmanestcool".toList)))
 
 val tree = huffman.createCodeTree("decodedSecret".toList)
 println(huffman.quickEncode(tree)("Sodded".toList))
}