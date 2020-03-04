\name{simple_predict}
\alias{simple_predict}
\title{
Prediction Unseen Items For The Active User
}
\description{
In the predicted items list, items with more scores replace in top of the list.
}
\usage{
simple_predict(ratings, ratings2, ac)
}
\arguments{
  \item{ratings}{
  A rating matrix whose rows are items and columns are users.
  }
  \item{ratings2}{
  A matrix the size of the original user-item matrix in which the active user's empty elements are filled.
  }
  \item{ac}{
  The id of an active user as an integer (\eqn{1\le ac \le length of users}).
  }
}
\details{
Collaborative filtering is a recommender system for predicting the missing ratings that an active user might have given to an item. These ratings have been calculated and accumulate in a vector by this function.
}
\value{
  \item{predictedItems}{
  A sorted vector of predicted items based on the scores.
  }
}
\references{
Song, B., Gao, Y., & Li, X. M. (2020, January). \emph{Research on Collaborative Filtering Recommendation Algorithm Based on Mahout and User Model}. In Journal of Physics: Conference Series, Vol. 1437, no. 1, p. 012095, IOP Publishing.

Ramakrishnan, G., Saicharan, V., Chandrasekaran, K., Rathnamma, M. V., & Ramana, V. V. (2020). \emph{Collaborative Filtering for Book Recommendation System}. In Soft Computing for Problem Solving, pp. 325-338, Springer, Singapore.
}
\author{
Farimah Houshmand Nanehkaran

Maintainer: Farimah Houshmand Nanehkaran <hoshmandcomputer@gmail.com>
}
\examples{
ratings <- matrix(c(  2,    5,  NaN,  NaN,  NaN,    4,
                    NaN,  NaN,  NaN,    1,  NaN,    5,
                    NaN,    4,    5,  NaN,    4,  NaN,
                      4,  NaN,  NaN,    5,  NaN,  NaN,
                      5,  NaN,    2,  NaN,  NaN,  NaN,
                    NaN,    1,  NaN,    4,    2,  NaN),nrow=6,byrow=TRUE)

sim <- simple_similarity(ratings, max_score=5, min_score=1, ac=1)

ratings2 <- Score_replace(ratings, sim_index= sim$sim_index, ac=1)

predictedItems <- simple_predict(ratings, ratings2, ac=1)
}
\keyword{Prediction}

