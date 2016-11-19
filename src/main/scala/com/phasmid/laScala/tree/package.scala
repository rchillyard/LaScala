package com.phasmid.laScala

/**
  * Created by scalaprof on 11/12/16.
  */
package object tree {
  type NodeSeq[A] = Seq[Node[A]]

//  type TreeBuilder[A] = (Node[A], Node[A]) => TreeLike[A]
//
//  type LeafBuilder[A] = (A) => Node[A]
}
