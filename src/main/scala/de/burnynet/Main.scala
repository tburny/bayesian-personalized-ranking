package de.burnynet

import breeze.linalg.{DenseVector, DenseMatrix}
import Math.{sqrt,pow}
import scala.util.Random

/**
 *
 * @author tobi
 *
 */
object Main extends App {

  // our user X ratings matrix. Rows: Users, columns: Items
  val userItems =
    DenseMatrix (
      (0,1,1,0), // alice
      (1,1,0,0)  // tom
    ).t // transpose so the breeze lib returns vectors for users on (::, 0)

  val model : BPRModel = new kNNModel(userItems)


  val numItems = userItems.rows
  val numUsers = userItems.cols

  val alice = userItems(::,0)
  val alicePreferences = model.itemMatrix(alice)
  println(alicePreferences)

  // println(model.correlationMatrix)

  println()
  val tom = userItems(::,0)
  val tomPreferences = model.itemMatrix(tom)


  println("DS")
  println(model.Ds)

  println(model.positiveItemRatingsByUser(0))
  println(model.positiveItemRatingsByUser(1))

  println(model.usersWhichRatedItemPositive(2))
  println(model.usersWhichRatedItemPositive(1))




  for(u <- 0 until numUsers) {
    val m = DenseMatrix.zeros[Double](numItems, numItems)
    m.foreachKey {case (i,j) => m(i,j) = model.xUIJ(u,i,j)}
    println("x_uij for user " + u)
    println(m)
  }

  sep("LearnBPR")
  //values taken from Recommender101:
  val alpha = 0.05
  val learnedModel = ModelLearner.learnBpr(0.1, 0, model)
  println(truncate(learnedModel))


  def sep(s:String) {
    println("\n================" + s + "================")
  }

  def truncate(m:DenseMatrix[Double]) = {
    m.foreachKey { case (i,j) => m(i,j) = (math rint m(i,j) * 100) / 100}
    m
  }
}



