package com.linearregression

import breeze.linalg.{*, DenseMatrix, DenseVector, csvread, csvwrite, inv, norm, sum}
import breeze.numerics.{pow, round}
import breeze.stats.mean

import java.io.File
import scala.util.Random

object Main {
  class LinearRegression(Normalize: Boolean = true) {
    private var W: DenseVector[Double] = DenseVector()
    var IsFitted: Boolean = false

    def Fit(X: DenseMatrix[Double], Y: DenseVector[Double], CV: Int = 3): Map[String, Double] = {
      var XCopy: DenseMatrix[Double] = X.copy
      var CVResults: Map[String, Double] = Map[String, Double]()
      if (!this.IsFitted) {
        if (this.Normalize) {
          XCopy = XCopy(::, *).map(Col => Col / norm(Col))
        }
        if (CV > 1) {
          val FoldSize = round(XCopy.rows / CV)
          var Indices = (0 until XCopy.rows).toList
          Indices = Random.shuffle(Indices)
          for (i <- 0 until CV) {
            var foldIndices: List[Int] = List[Int]()
            if (i < CV - 1) {
              foldIndices = Indices.slice(i * FoldSize, (i + 1) * FoldSize)
            } else {
              foldIndices = Indices.slice((CV - 1) * FoldSize, XCopy.rows)
            }
            val trainIndices = Indices.toSet.diff(foldIndices.toSet).toList
            val XFold = XCopy(trainIndices, ::).toDenseMatrix
            val YFold = Y(trainIndices).toDenseVector
            var CurW: DenseVector[Double] = DenseVector()
            CurW = inv(XFold.t * XFold) * XFold.t * YFold
            val YPred = XCopy(foldIndices, ::).toDenseMatrix * CurW
            val YTrue = Y(foldIndices).toDenseVector
            val Score = this.getRSquared(YTrue, YPred)
            println(s"Fold_${i}, Score: ${Score}")
            CVResults += (s"Fold_${i}" -> Score)
          }
        }
        this.W = inv(XCopy.t * XCopy) * XCopy.t * Y
        this.IsFitted = true
        CVResults
      } else {
        println("Already fitted.")
        CVResults
      }
    }

    def Predict(X: DenseMatrix[Double]): DenseVector[Double] = {
      if (!this.IsFitted) {
        println("Model is not fitted. Please call fit() before predict().")
      }
      if (this.Normalize) {
        X(::, *).map(Col => Col / norm(Col)) * this.W
      } else {
        X * this.W
      }
    }

    def getRSquared(YTrue: DenseVector[Double],
                    YPred: DenseVector[Double],
                    Adjusted: Boolean = false): Double = {
      val SsRes = sum(pow(YTrue - YPred, 2))
      val MeanTarget = mean(YTrue)
      val SsTot = sum(pow(YTrue - MeanTarget, 2))
      if (Adjusted) {
        val N = YTrue.length
        val P = this.W.length
        1 - (SsRes / (N - P - 1)) / (SsTot / (N - 1))
      } else {
        1 - SsRes / SsTot
      }
    }

    def score(X: DenseMatrix[Double],
              YTrue: DenseVector[Double],
              Adjusted: Boolean = false): Double = {
      val YPred = this.Predict(X)
      this.getRSquared(YTrue, YPred, Adjusted)
    }
  }

  def main(args: Array[String]): Unit = {
    println(args.mkString("Array(", ", ", ")"))
    val PathToTrainFile = args[0]
    val PathToTestFile = args[1]
    val PathToTestPredicts = args[2]
    var XYTrain = csvread(new File(PathToTrainFile), ';', skipLines = 1)
    var XYTest = csvread(new File(PathToTestFile), ';', skipLines = 1)
    val LinearRegression = new LinearRegression()
    // Returns Cross Validation Results
    val CVResults = LinearRegression.Fit(X = XYTrain(::, 1 to -1), Y = XYTrain(::, 0), CV = 5)
    val TestPredicts = LinearRegression.Predict(X = XYTest(::, 1 to -1))
    csvwrite(new File(PathToTestPredicts), TestPredicts.asDenseMatrix.t)
  }
}
