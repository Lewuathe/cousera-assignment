package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times of list of char") {
    val l = List('a', 'a', 'b', 'c', 'd', 'a', 'b')
    assert(times(l) == List(('a', 3), ('b', 2), ('c', 1), ('d', 1)))
  }

  test("list is sorted by frequency") {
    val l = List(('a', 3), ('b', 2), ('c', 1), ('d', 1))
    assert(makeOrderedLeafList(l) == List(Leaf('c', 1), Leaf('d', 1), Leaf('b', 2), Leaf('a', 3)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of some leaf list that should be preserved") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('f', 6))
    assert(combine(leaflist) === List(Leaf('x',4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5), Leaf('f',6)))
  }

  test("create code tree") {
    val l = List('a', 'b', 'a', 'c', 'a', 'd', 'a', 'b')
    assert(createCodeTree(l) == Fork(Fork(Fork(Leaf('c',1),Leaf('d',1),List('c', 'd'),2),Leaf('b',2),List('c', 'd', 'b'),4),Leaf('a',4),List('c', 'd', 'b', 'a'),8))
  }

  test("decode french code") {
    assert(decodedSecret.mkString == "huffmanestcool")
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, encode(t2)("abd".toList)) === "abd".toList)
    }
  }

  test("look up table with codeBits") {
    val table: CodeTable = List(('a', List(0)), ('b', List(0,1)), ('c', List(0,1,0)))
    assert(codeBits(table)('a') == List(0))
    assert(codeBits(table)('b') == List(0,1))
    assert(codeBits(table)('c') == List(0,1,0))
  }

  test("create code table") {
    new TestTrees {
      assert(convert(t1) == List(('a', List(0)), ('b', List(1))))
      assert(convert(t2) == List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
    }
  }

  test("quick encode") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, quickEncode(t2)("abd".toList)) === "abd".toList)
      assert(decode(frenchCode, quickEncode(frenchCode)("huffmanestcool".toList)) === "huffmanestcool".toList)
    }
  }
}
