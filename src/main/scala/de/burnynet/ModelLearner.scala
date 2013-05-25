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
    val Θ = model.correlationMatrix.copy
    // randomize which items of D_s are picked first
    val rndDs = Random.shuffle(model.Ds).toList
    // just do it for all items.
    // See e.g. /org/recommender101/recommender/extensions/bprmf/BPRMFRecommender.java:212 in Recommender101
    rndDs.foldLeft(Θ) {
      case (akku, (u,i,j)) =>
        // e ^ - x_uij
        val exp_X_uij = exp(-model.xUIJ(u,i,j))
        // Θ = Θ +  α * (e^-x_uij / (1+ e^-x_uij ) * d/dΘ (x_uij) + Θ * λ)
        akku + (((model.deriveXUIJ(u,i,j) :* (exp_X_uij / (1+exp_X_uij))) + (Θ :* λ)) * α)
    }
  }

}
