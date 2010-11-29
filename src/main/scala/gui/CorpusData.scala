package gui

import scala.collection.mutable._
import corpus2.Driver._
import corpus2._
import java.awt.{ Color }


object CorpusData
{

  //keep track of documents that have been added or deleted to the navigate panel
  val d2c = Map.empty[Document,CorpusData]

  var currentCorpusNodes = d2c.values.toBuffer

  

  def makeCorpusNodes(corpus: Corpus) : CorpusData = {

    val rootNode = CorpusData("BNC")

    for (i <- 0 until corpus.documents.size) {

       val doc = documents(corpus.documents(i))
       rootNode.children += CorpusData(doc.code, doc, 0, doc.sentences.size - 1)
     }
  
     rootNode
    
  }

  def makeDummyCorpusDatas = {
    for (i <- 0 until 10) {
     // currentCorpusNodes.append(CorpusData("A" + i, i * 10, ((i+1) * 10) - 1))
    }
  }


  def toggleLeafNode(node: CorpusData) {
    println("in toggleLeafNode... node was " + node.isSelected + "... ")
    node.isSelected = !node.isSelected
    if (node.isSelected == false) {
      currentCorpusNodes -= node
    } else {
      currentCorpusNodes += node
    }
  }

  def toggleNode(node: CorpusData) {
    
    if (node.isLeaf == true) {
      toggleLeafNode(node)
    }

    // if (d2c.contains(doc)) {
    //   d2c.remove(doc)
    // }
    // else {
    //   val cd = CorpusData(doc, doc.code, 0, doc.sentences.size)
    //   //println(cd)
    //   //currentCorpusNodes.append(cd)
    //   println(cd)
    //   d2c.put(doc, cd)
    // }
    // //currentCorpusNodes.append(cd)
    // currentCorpusNodes = d2c.values.toBuffer
    Main.navigatePanel.initializeRangeSettings

  }

  def apply(name: String, doc: Document, rawStartIdx: Int, rawEndIdx: Int) : CorpusData = {

    val c = CorpusData(name)
    c.document = doc
    c.rawStartIdx = rawStartIdx
    c.rawEndIdx = rawEndIdx
    c.isLeaf = true
    c
    
  }
}


case class CorpusData(name: String) {
  var document: Document = null 
  var rawStartIdx: Int = -1
  var rawEndIdx: Int = -1
  var startIdx  = -1
  var endIdx  = -1
  var isLeaf = false
  var isSelected = false

  implicit def dblToFloat(dbl: Double) = dbl.toFloat
  
  var color = new Color(math.random, math.random, math.random) 

  var children = new ListBuffer[CorpusData]
  
  override def toString(): String = {
    //return document.code + ", " + rawStartIdx + ", " + rawEndIdx + " / " + startIdx + ", " + endIdx
    return name

  }

  //returns the idx of the sentence within this CorpusData's document by transforming the navigate panel's representation of it
  def idxToRawIdx(idx : Int) : Int = {
    return (idx - startIdx)
  }

}
