package de.burnynet.bpr

import breeze.linalg.{DenseVector, DenseMatrix}
import java.lang.Math._

/**
 * A model for de.burnynet.bpr.ModelLearner
 * @author tobi
 *
 */
abstract class BPRModel(userItems:DenseMatrix[Int]) {
  val numItems = userItems.rows
  val numUsers = userItems.cols

  lazy val Ds = {
    (for (i <- 0 until numUsers) yield dsForUser(i)).flatten
  }

  val correlationMatrix: DenseMatrix[Double]

  def deriveXUIJ(u:Int, i:Int, j:Int) : DenseMatrix[Double]
  def xWithUserIndex(u:Int, i:Int) : Double

  // See p. 457
  def xUIJ(u:Int, i:Int, j:Int) = xWithUserIndex(u,i) - xWithUserIndex(u,j)

  // I_u +
  /**
   * Returns the set I_u+ which contains the indices of all items that the user positively rated
   * @param u The user index
   * @return
   */
  def positiveItemRatingsByUser(u:Int) = {
    val row = userItems(::,u)
    (for (x <- 0 until row.length if row(x) > 0) yield x).toSet
  }

  // U_i +
  def usersWhichRatedItemPositive(i:Int) : Set[Int] = {
    // Transpose so we do not end up in a matrix -.-
    val row: DenseVector[Int] = userItems.t(::,i)
    (for (j <- 0 until row.length if (row(j)) > 0) yield j).toSet
  }

  def itemMatrix(d:DenseVector[Int]) : DenseMatrix[Int] = {
    val matrix = DenseMatrix.zeros[Int](d.length, d.length)
    matrix.foreachKey { case (i, j) => matrix(i,j) = compare(d(j), d(i)) }
    matrix
  }

  /**
   *
   * @param i Rating of first item
   * @param j Rating of second item
   * @return 1, if the user prefers item j over i, -1 if the user prefers i over j and 0 if nothing can be said about
   *         the preference
   */
  def compare(i:Int, j:Int) : Int = {
    if (j > i) 1
    else if (i>j) -1
    else 0
  }

  /**
   * Returns the set D_s which contains all triples (u,i,j) where user u prefers item i over item j
   * @param u user index
   * @return
   */
  def dsForUser(u:Int) = {
    val userPreferences = itemMatrix(userItems(::, u))
    for (
      i <- 0 until userPreferences.rows;
      j <- 0 until userPreferences.cols
      if (userPreferences(i,j) > 0)) yield (u,i,j)
  }

}
