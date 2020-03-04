\name{Score_replace}
\alias{Score_replace}
\title{
Replacing of Neighbor Users' Ratings on Non-Rated Items By The Active User
}
\description{
The ratings of each user that has more similar to the active user are directly replaced in his unseen items.
}
\usage{
Score_replace(ratings, sim_index, ac)
}
\arguments{
  \item{ratings}{
  A rating matrix whose rows are items and columns are users.
  }
  \item{sim_index}{
  Descending sorted indexes based on similarity to the active user who is a vector of integers.
  }
  \item{ac}{
  The id of an active user as an integer (\eqn{1\le ac \le length of users}).
  }
}
\details{
The unseen items of the active user are filled by the ratings of the similar users, respectively. Each element remains unchanged after one placement.
}
\value{
  \item{ratings2}{
  A matrix the size of the original user-item matrix in which the active user's empty elements are filled.
  }
}
\references{
Gadekula, S. K., Rao, U. P., Vyas, R. K., Dontula, A. L., & Gaikwad, S. V. (2019). \emph{Improved Pearson Similarity for Collaborative Filtering Recommendation System}. In 2019 6th International Conference on Computing for Sustainable Global Development (INDIACom), pp. 1047-1054, IEEE.
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
}
\keyword{replacement}
