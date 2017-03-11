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
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("makeCodeTree") {
    new TestTrees {
      assert(makeCodeTree(
        makeCodeTree(Leaf('a', 2), Leaf('b', 3)),
        Leaf('d', 4)
      ) === t2)
    }
  }


  test("times") {
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("makeOrderedLeafList for some frequency table 2") {
    assert(makeOrderedLeafList(List(('a', 2), ('b', 1), ('c', 3))) === List(Leaf('b',1), Leaf('a',2), Leaf('c',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


//  test("createCodeTree") {
//    new TestTrees {
//      assert((string2Chars _ andThen createCodeTree)("ababdadab") === t2)
//    }
//  }

  test("decode") {
    assert(decode(frenchCode, secret) === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  test("encode") {
    assert(encode(frenchCode)(List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')) === secret)
  }

  test("quickEncode") {
    assert(quickEncode(frenchCode)(List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')) === secret)
  }

  test("mergeCodeTables") {
    assert(
      mergeCodeTables(
        List(
          ('a', List(0)),
          ('b', List(1))
        ),
        List(
          ('c', List(0)),
          ('d', List(1))
        )
      ) === List(
        ('a', List(0)),
        ('b', List(1)),
        ('c', List(0)),
        ('d', List(1))
      )
    )
  }

  test("convert") {
    new TestTrees {
      assert(convert(t1) === List(
        ('a', List(0)),
        ('b', List(1))
      ))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
