package com.phasmid.laScala

import org.scalatest.{FlatSpec, Inside, Matchers}

/**
  *
  * @author scalaprof
  */
class BinaryTreeSpec extends FlatSpec with Matchers with Inside {
  behavior of "toString"
  it should "form string from single node" in {
    Node('a').toString shouldBe "T(a . .)"
  }
  it should "form string from 3 levels" in {
    Node('a',
      Node('b', Node('d'), Node('e')),
      Node('c', Empty, Node('f', Node('g'), Empty))).toString shouldBe "T(a T(b T(d . .) T(e . .)) T(c . T(f T(g . .) .)))"
  }

  behavior of "depthFirst (result)"
  it should "perform DFS with single node" in {
    Node("a").depthFirst(identity).toString() shouldBe "List(a)"
  }
  it should "perform DFS with 2 levels (1)" in {
    Node("a",Node("b"),Empty).depthFirst(identity).toString() shouldBe "List(b, a)"
  }
  it should "perform DFS with 2 levels (2)" in {
    Node("a",Node("b"),Node("c")).depthFirst(identity).toString() shouldBe "List(b, c, a)"
  }
  it should "perform DFS from 3 levels" in {
    Node('a',
      Node('b', Node('d'), Node('e')),
      Node('c', Empty, Node('f', Node('g'), Empty))).depthFirst(identity).toString() shouldBe "List(d, e, b, g, f, c, a)"
  }
  behavior of "depthFirst (function)"
  it should "perform DFS with single node" in {
    val sb = new StringBuilder
    Node("a").depthFirst(s => sb.append(s))
    sb.toString() shouldBe "a"
  }
  it should "perform DFS with 2 levels (1)" in {
    val sb = new StringBuilder
    Node("a",Node("b"),Empty).depthFirst(s => sb.append(s))
    sb.toString() shouldBe "ba"
  }
  it should "perform DFS with 2 levels (2)" in {
    val sb = new StringBuilder
    Node("a",Node("b"),Node("c")).depthFirst(s => sb.append(s))
    sb.toString() shouldBe "bca"
  }
  it should "perform DFS from 3 levels" in {
    val sb = new StringBuilder
    Node('a',
      Node('b', Node('d'), Node('e')),
      Node('c', Empty, Node('f', Node('g'), Empty))).depthFirst(s => sb.append(s))
    sb.toString() shouldBe "debgfca"
  }

//  behavior of "depthFirstSearch"
//  it should "perform DFS with single node" in {
//    val dfs = DepthFirstSearch(Node("a"))
//    dfs.postOrder shouldBe List("a")
//    dfs.preOrder shouldBe List("a")
//    dfs.revPostOrder shouldBe List("a")
//  }
//  it should "perform DFS with 2 levels (1)" in {
//    val dfs = DepthFirstSearch(Node("a",Node("b"),Empty))
//    dfs.postOrder shouldBe List("b", "a")
//    dfs.preOrder shouldBe List("a", "b")
//    dfs.revPostOrder shouldBe List("a", "b")
//  }
//  it should "perform DFS with 2 levels (2)" in {
//    val dfs = DepthFirstSearch(Node("a",Node("b"),Node("c")))
//    dfs.postOrder shouldBe List("b", "c", "a")
//    dfs.preOrder shouldBe List("a", "c", "b")
//    dfs.revPostOrder shouldBe List("a", "c", "b")
//  }
//  it should "perform DFS from 3 levels" in {
//    val dfs = DepthFirstSearch(Node('a',
//      Node('b', Node('d'), Node('e')),
//      Node('c', Empty, Node('f', Node('g'), Empty))))
//    dfs.postOrder shouldBe List('d', 'e', 'b', 'g', 'f', 'c', 'a')
//    dfs.preOrder shouldBe List('a', 'c', 'f', 'g', 'b', 'e', 'd')
//    dfs.revPostOrder shouldBe List('a', 'c', 'f', 'g', 'b', 'e', 'd')
//  }
}
