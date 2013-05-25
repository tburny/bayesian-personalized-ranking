package de.burnynet

import breeze.linalg.DenseMatrix
import java.lang.Math._
import scala.util.Random

/**
 *
 * @author tobi
 *
 */
object ModelLearner {

  // Use symbols as in the paper, thats more readable
  def learnBpr(α :Double, λ:Double, model: BPRModel) = {
    def recurse(ds:List[(Int,Int,Int)], Θ: DenseMatrix[Double]) : DenseMatrix[Double] = {
      ds match {
        // TODO check this
        case List() => Θ
        // Maybe not just: Define convergence as "all items picked from ds"
        case (u,i,j) :: xs =>
          // e ^ - x_uij
          val exp_X_uij = exp(-model.xUIJ(u,i,j))
          // Θ = Θ +  α * (e^-x_uij / (1+ e^-x_uij ) * d/dΘ (x_uij) + Θ * λ)
          recurse(xs, Θ) + (((model.deriveXUIJ(u,i,j) :* (exp_X_uij / (1+exp_X_uij))) + (Θ :* λ)) * α)
      }

    }

    val Θ = model.correlationMatrix.copy
    val rndDs = Random.shuffle(model.Ds).toList

    recurse(rndDs, Θ)
  }

}
