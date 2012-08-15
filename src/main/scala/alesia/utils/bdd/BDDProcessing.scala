/*
 * Copyright 2012 Roland Ewald
 *  
 * Licensed under the Apache License, Version 2.0 (the "License"); 
 * you may not use this file except in compliance with the License. 
 * You may obtain a copy of the License at 
 *  
 *  http://www.apache.org/licenses/LICENSE-2.0
 *  
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 * See the License for the specific language governing permissions and 
 * limitations under the License. 
 */
package alesia.utils.bdd

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/** Implementation of algorithms for managing, processing, and creating binary decision diagrams.
 *
 *  Most implementations are from D. E. Knuth's "The Art of Computer Programming", vol. 4-1 (p. 70 et sqq.).
 *
 *  @see BinaryDecisionNode
 *
 *  @author Roland Ewald
 */
object BDDProcessing {

  /** Evaluate node for given input. */
  @tailrec
  def evaluate(node: BinaryDecisionNode, input: Array[Boolean]): Boolean = node match {
    case node: BDDNode => if (input(node.variable)) evaluate(node.high, input) else evaluate(node.low, input)
    case TrueNode => return true
    case FalseNode => return false
  }

  /** Evaluate an instruction array for a given input. */
  def evaluate(instructions: Array[BranchInstruction], input: Array[Boolean]): Boolean = {
    checkBDDDimension(instructions.size)
    @tailrec
    def evaluate(instructions: Array[BranchInstruction], input: Array[Boolean], currentIndex: Int): Boolean = instructions(currentIndex) match {
      case i: BranchInstr => if (input(i.variable)) evaluate(instructions, input, i.highIndex) else evaluate(instructions, input, i.lowIndex)
      case t: TrueNodeInstruction => true
      case f: FalseNodeInstruction => false
    }
    evaluate(instructions, input, instructions.size - 1)
  }

  /** Count BDD solutions (input variable combinations for which f(x_1, ..., x_n) = true).
   *  Corresponds to algorithm 7.1.4.C (p. 75, TAOCP - see above).
   */
  def countSolutions(bdd: Array[BranchInstruction]): Int = {
    checkBDDDimension(bdd.size)

    //Contains number of solutions (ones) for the bead corresponding to the branch instruction with the same index
    val counter = new Array[Int](bdd.size)

    //Computes the number of combinations between the index of a node and the given node with variable index v_k
    def combinations(idx: Int, v_k: Int) = counter(idx) * (1 << (bdd(idx).variable - v_k - 1))

    //C1
    counter(1) = 1
    for (k <- 2 until bdd.size) {
      val v = bdd(k).variable
      counter(k) = combinations(bdd(k).lowIndex, v) + combinations(bdd(k).highIndex, v) //C2
    }

    combinations(bdd.size - 1, -1) // Second argument needs to be -1 because variable indices start with 0
  }

  /** Reduce BDD if necessary. It is assumed the BDD is already ordered,
   *  i.e. children of a node related to v_i may only be related to variables v_{i+1}, ..., v_n.
   *  Corresponds to algorithm 7.1.4.R (p. 85, TAOCP - see above).
   *
   *  The algorithm is pretty tricky an involves a lot of pointer arithmetic on linked lists.
   */
  def reduce(bdd: Array[BranchInstruction]): Array[BranchInstruction] = {
    checkBDDDimension(bdd.size)

    // Maximal variable index (false & true node have variable index v_{n+1})
    val v_max = bdd(0).variable - 1

    // Current variable (of which the nodes shall be reduced) 
    var v = v_max //index of current node

    // Current root of the sub-tree to be reduced
    var root = bdd.size - 1

    // Auxiliary variables
    var p, q, r, s = 0

    // Auxiliary data per branch instruction
    val aux = new Array[Int](bdd.size)

    // Auxiliary data to list all nodes that refer to the same variable:
    val head = new Array[Int](v_max + 1)
    for (v <- bdd(root).variable to v_max)
      head(v) = -1

    // Index-based access to l_k, v_k, and h_k of the I_k
    val low = for (i <- bdd) yield i.lowIndex
    val variable = for (i <- bdd) yield i.variable
    val high = for (i <- bdd) yield i.highIndex

    // Top of stack for deleted nodes
    var avail = -1

    /** Initializes the algorithm by doing a depth-first search with bit-wise tricks in the aux array (step R1 in TAOCP).
     *  The high branches are searched first, with a pointer to the remaining low-index node to be visited stored in the auxiliary data
     *  (basically that's all pointer arithmetic on linked lists).
     *
     *  The method uses the bitwise complement (~0 = -1, ~1=-2, ~~x=x) etc. to (re)store pointers.
     *
     *  During the visit to all reachable nodes, linked lists of nodes that all
     *  query the same variable are generated, pointers to the start node indices are stored (again, as complements) in the head array.
     *
     *  Example for unreduced median3 function (see TAOCP, vol. 4-1, p. 71, eq. 7.1.4-(2)):
     *
     *  Before:
     *  root = 8
     *  low:  [0, 1, 0, 0, 2, 0, 1, 5, 4]
     *  var:  [3, 3, 2, 2, 1, 2, 2, 1, 0]
     *  high: [0, 1, 0, 1, 3, 1, 1, 6, 7]
     *  aux:  [-1, -1, 0, 0, 0, 0, 0, 0, -1]
     *  head: [-1, -1, -1]
     *
     *  After:
     *  low:  [0, 1, 0, 0, 2, 0, 1, 5, 4]
     *  var:  [3, 3, 2, 2, 1, 2, 2, 1, 0]
     *  high: [0, 1, 0, 1, 3, 1, 1, 6, 7]
     *  aux:  [-1, -1, -4, -6, -8, -7, -1, -1, -1]
     *  head: [-9, -5, -3]
     *
     *  Low, var, high remain unchanged.  Head contains the pointers to the start nodes of the linked lists for each variable, e.g. for
     *  the third variable it contains '-3', so that the first node that queries this variable has index ~(-3) = 2. The aux array for the
     *  variable with index 2 contains '-4', so the second element in that list is the node with index ~(-4) = 3, and so on. All nodes with
     *  aux = 0 could not be reached at all.
     */
    def init() = {

      // Mark root and bottom nodes as visited
      aux(0) = -1
      aux(1) = -1
      aux(root) = -1

      // Start from root
      s = root
      while (s != 0) {
        p = s //p is the current node
        s = ~aux(p) //contains pointer to previous node (or zero, in first iteration and if root node is reached again)
        aux(p) = head(variable(p)) //store pointer to next element in node list (-1 denotes Nil)
        head(variable(p)) = ~p //put in pointer to this element as first element in list
        if (aux(low(p)) >= 0) { //check if low branch has been visited
          aux(low(p)) = ~s //store previous node on aux storage for next node (as negative number)
          s = low(p) //go to this node
        }
        if (aux(high(p)) >= 0) { //check if high branch has been visited
          aux(high(p)) = ~s //store 'previous' (could be low(p)) node on aux storage for next node (as negative number)
          s = high(p) //go to this node
        }
      }
    }

    /** Do a bucket sort. This links together all nodes with the same low-index but differing high-indices (those were both are identical are removed as
     * duplicates). The lists start at ~aux(x), where x is the low-index being equal. R3 in TAOCP. */
    def bucketSort() = {
      p = ~head(v) //get index of first linked list element containing all nodes that refer to variable v
      s = 0 //index complement of previous element (i.e. 0 is -1 is Nil)
      while (p != 0) {
        val p2 = ~aux(p) //get pointer to next element in list, store in auxiliary variable
        q = high(p) //get high-branch node of current node
        if (low(q) < 0) { //if high-branch node has already been deleted, let high-branch of current node point to the non-deleted duplicate node (stored as complement in low array)  
          high(p) = ~low(q)
        }
        q = low(p) //get low-branch node of current node
        if (low(q) < 0) { //if low-branch node has already been deleted, let low-branch of current node point to the non-deleted duplicate node (stored as complement in low array)
          low(p) = ~low(q)
          q = low(p)
        }
        if (q == high(p)) { //if low(p) == high(p), this node is obsolete and can be deleted
          low(p) = ~q //store the index complement of the duplicated non-deleted node
          aux(p) = 0 //remove auxiliary data
          high(p) = avail //push p into the stack 
          avail = p
        } else if (aux(q) >= 0) { //if q already points to a list, add p to this linked list 
          aux(p) = s
          s = ~q
          aux(q) = ~p 
        } else { // if q is negative, copy the aux information of the node it points to and set pointer to this node
          aux(p) = aux(~aux(q))
          aux(~aux(q)) = p
        }
        p = p2 //go to next element in list, until end is reached (-1 in aux, ~(-1) == 0) 
      }

      cleanUpBucketSort()
    }

    /** Cleans up aux array after bucket sort is finished. R4 in TAOCP. */
    def cleanUpBucketSort() = {
      r = ~s //r is the index to clean up
      s = 0
      while (r >= 0) {
        q = ~aux(r) //read pointer to first node
        aux(r) = 0 //reset auxiliary storage
        if (s == 0) { //store pointer to first node in s
          s = q
        } else { //store other pointer in aux
          aux(p) = q
        }
        p = q
        while (aux(p) > 0) //go to end of linked list
          p = aux(p)
        r = ~aux(p)
      }
    }

    /** Removes duplicate nodes. R7 in TAOCP (except for the loop, which is realized in the main algorithm). */
    def removeDuplicates() {
      r = high(q)
      if (aux(r) >= 0) {
        aux(r) = ~q
      } else { //node q is a duplicate 
        low(q) = aux(r) //store index complement of duplicate non-deleted node
        high(q) = avail //push q into the stack
        avail = q
      }
      q = aux(q) //go to next node
    }

    /** Cleans up after duplicates have been removed, sets the auxiliary data of these nodes to 0.
     *  R8 in TAOCP (except for the loop, which is realized in the main algorithm).
     */
    def cleanUpAfterDuplicateRemoval() {
      if (low(p) >= 0) { //reset auxiliary data in case node p has not been marked as deleted
        aux(high(p)) = 0
      }
      p = aux(p) //go to next node
    }

    /** Checks whether algorithm is done. R9 in TAOCP (return code is used in main loop to achieve same behavior as the GOTOs).*/
    def checkDone(): Int = {
      if (p != 0)
        0
      else if (v > variable(root)) {
        v = v - 1
        1
      } else {
        if (low(root) < 0) {
          root = ~low(root)
        }
        -1
      }
    }

    /** Create BDD from arrays. After the algorithm is finished, the negative fields in the low array correspond to deleted nodes,
     *  their complement points at the equivalent non-deleted node. As the pointer structure has been updated accordingly as well,
     *  we can also just walk through the tree recursively, as is done below.
     */
    def createReducedBDD(idx: Int): BinaryDecisionNode = idx match {
      case 0 => FalseNode
      case 1 => TrueNode
      case n => BDDNode(variable(n), createReducedBDD(low(n)), createReducedBDD(high(n)))
    }

    //Main algorithm:

    init() //R1
    //R2 - Loop on v    
    aux(0) = 0
    aux(1) = 0
    var loopDecision = 0 //Represents the jump-logic (in absence of GOTOs... :)
    while (v >= 0 && loopDecision != -1) {
      loopDecision = 0

      //R3+R4
      bucketSort()

      //R5
      p = s
      if (p == 0)
        loopDecision = checkDone() // R9
      else
        q = p

      while (loopDecision == 0) {
        s = low(p) //R6 - examine a bucket
        removeDuplicates() //R7
        while (q != 0 && low(q) == s) //
          removeDuplicates()
        cleanUpAfterDuplicateRemoval() //R8
        while (p != q)
          cleanUpAfterDuplicateRemoval()
        loopDecision = checkDone() // R9
      }
    }

    //Create BDD starting from root node
    createReducedBDD(bdd.size - 1)
  }

  /** Checks dimension of BDD.*/
  private[this] def checkBDDDimension(size: Int) =
    require(size > 1, "A valid BDD consists of at least two branch instructions, but there are only " + size + " present.")
}