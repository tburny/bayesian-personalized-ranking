package de.burnynet

import breeze.linalg.DenseMatrix
import java.lang.Math._

/**
 *
 * @author tobi
 *
 */

class kNNModel(userItems:DenseMatrix[Int]) extends BPRModel(userItems) {

  // only works once, thats enough ;)
  lazy val correlationMatrix = {
    val m = DenseMatrix.zeros[Double](numItems, numItems)
    m.foreachKey { case (i,j) => m(i,j) = cosine(i,j)}
    m
  }

  def deriveXUIJ(u:Int, i:Int, j:Int) : DenseMatrix[Double] = {
    // TODO check correctness
    val ui = userItems(::,u)
    // the gradient is positive for a positive rating and negative for a negative one
    val items = itemMatrix(ui)
    val matrix = DenseMatrix.zeros[Double](items.rows, items.cols)
    matrix.foreachKey { case (m, n) => matrix(m,n) = items(m, n) }
    matrix
  }

  // TODO correlation matrix looks weird!
  def xWithUserIndex(u:Int, i:Int) = {
    val positiveRatings =
      positiveItemRatingsByUser(u) - i
    // sum over all l € positiveRatings where l != i add c_il
    positiveRatings.foldLeft(0.0) ((a,l) =>a + correlationMatrix(i,l))
  }

  def cosine(i:Int, j:Int) = {
    if (i == j) 1.0
    else {
      val ui = usersWhichRatedItemPositive(i)
      val uj = usersWhichRatedItemPositive(j)
      if (ui.isEmpty || uj.isEmpty) 0.0
      // if its empty, the size of the intersection is 0 and 0 divided by anything = 0
      else (ui intersect(uj)).size / sqrt(ui.size * uj.size)
    }
  }

}