\name{CFF-package}
\alias{CFF-package}
\docType{package}
\title{
\packageTitle{CFF}
}
\description{
\packageDescription{CFF}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{CFF}
\packageIndices{CFF}
User-Based Collaborative Filtering Systems
}
\author{
\packageAuthor{CFF}

Maintainer: \packageMaintainer{CFF}
}
\references{
Kumar, P., Kumar, V., & Thakur, R. S. (2019). \emph{A new approach for rating prediction system using collaborative filtering}. Iran Journal of Computer Science, vol.2, no. 2, pp. 81-87.

Zhang, P., Zhang, Z., Tian, T., & Wang, Y. (2019). \emph{Collaborative filtering recommendation algorithm integrating time windows and rating predictions}. Applied Intelligence, vol. 49, no. 8, pp. 3146-3157.

Gadekula, S. K., Rao, U. P., Vyas, R. K., Dontula, A. L., & Gaikwad, S. V. (2019). \emph{Improved Pearson Similarity for Collaborative Filtering Recommendation System}. In 2019 6th International Conference on Computing for Sustainable Global Development (INDIACom), pp. 1047-1054, IEEE.

}
\keyword{Collaborative Filtering}
\examples{
ratings <- matrix(c(  2,    5,  NaN,  NaN,  NaN,    4,
                    NaN,  NaN,  NaN,    1,  NaN,    5,
                    NaN,    4,    5,  NaN,    4,  NaN,
                      4,  NaN,  NaN,    5,  NaN,  NaN,
                      5,  NaN,    2,  NaN,  NaN,  NaN,
                    NaN,    1,  NaN,    4,    2,  NaN),nrow=6,byrow=TRUE)
active_users <- c(1:dim(ratings)[2])

time_all <- c(rep(NaN, length(active_users)))

ratings3 <- ratings

for (ac in 1:length(active_users))
{
  cat("=========== user",active_users[ac], "==================", "\n","\n")
  ##1
  T1_start <- Sys.time()
  sim <- simple_similarity(ratings, max_score=5, min_score=1, ac)
  T1_end <- Sys.time()

  cat("    Similar Users =", sim$sim_index,                      "\n","\n")
  cat("Similarity Values =", sim$sim_x,                          "\n","\n")

  ##2
  T2_start <- Sys.time()
  ratings2 <- Score_replace(ratings, sim_index= sim$sim_index, ac)
  T2_end   <- Sys.time()

  cat(" Predicted Scores =", ratings2[,ac],                      "\n","\n")

  ##3
  T3_start <- Sys.time()
  predictedItems <- simple_predict(ratings, ratings2, ac)
  T3_end <- Sys.time()

  cat("  Predicted Items =", predictedItems,                    "\n","\n")

  ##4
  time_all[ac] <- (T1_end - T1_start) + (T2_end - T2_start) + (T3_end - T3_start)

  cat("             Time =", time_all[ac],                      "\n","\n")

  ##5
  ratings3[,ac] <- ratings2[,ac]
}

Mean_Time <- mean(time_all)

cat("=========== Mean Time ==================",                "\n","\n")

cat("         Mean Time =", Mean_Time,                         "\n","\n")

cat("       Full Matrix =",                                    "\n","\n")

print(ratings3)
}
