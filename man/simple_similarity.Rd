\name{simple_similarity}
\alias{simple_similarity}
\title{
Finding Neighbor Users And Their Similarity Values
}
\description{
Steps of calculating the similarity of one user to an active user :

1- Calculating the difference between the desired user ratings with the active user in common items.

2- Calculating the similarity value for each common item.

3- Calculating the mean value of similarities.

}
\usage{
simple_similarity(ratings, max_score=5, min_score=1, ac)
}
\arguments{
  \item{ratings}{
  A rating matrix whose rows are items and columns are users.
  }
  \item{max_score}{
  The maximum range of ratings.
  }
  \item{min_score}{
  The minimum range of ratings.
  }
  \item{ac}{
  The id of an active user as an integer (\eqn{1\le ac \le length of users}).
  }
}
\details{
The similarity of the active user with other users is obtained by the following formulas :

\deqn{dif_{(u_i, j)}=|r_{(u_a, j)}-r_{(u_i, j)}|}

\deqn{sim_{dif_{(u_i, j)}}=\frac{-dif_{(u_i, j)}}{max_score-min_score}+1}

\deqn{sim_{(u_a, u_j)}=\frac{\sum_{j=1}^{N_j}sim_{(dif_{(u_i,j)})}}{N_j}}

j is the row number for the items and i is the column number for the users in the ratings matrix.

\eqn{u_i} is a i\emph{th} column user and \eqn{u_a} is an active user.

\eqn{r_{(u_a, j)}} is the rating of active user in the j\emph{th} row and \eqn{r_{(u_i, j)}} is the rating of the i\emph{th} user in the j\emph{th} row.

\eqn{dif_{(u_i, j)}} is the difference of the rating for the i\emph{th} user with the active user in the j\emph{th} row.

\eqn{sim_{dif_{(u_i, j)}}} is the similarity of the i\emph{th} user with the active user in the j\emph{th} row.

\eqn{sim_{(u_a, u_i)}} is the similarity of the user i, with the active user.

\eqn{N_j} is the number of common items.

For example, suppose active user ratings are: \{2, nan, 3, nan, 5\} and one user ratings are: \{3, 4, nan, nan, 1\} then for ratings between 1 and 5:

dif=\{1, nan, nan, nan, 4\} and

sim(dif)=\{\eqn{\frac{-1}{5-1}+1}, nan, nan, nan, \eqn{\frac{-4}{5-1}+1}\}=\{0.75, nan, nan, nan, 0\}

and mean of sim(dif) is sim=0.375.

}
\value{
An object of class \code{"simple_similarity"}, a list with components:
  \item{call}{
  The call used.
  }
  \item{sim_x}{
  Neighboring user similarity values in descending order.
  }
  \item{sim_index}{
  Number of columns for neighboring users in descending order of similarity.
  }
}
\references{
Mongia, A., & Majumdar, A. (2019). \emph{Matrix completion on multiple graphs: Application in collaborative filtering}. Signal Processing, vol. 165, pp. 144-148.

Hong, B., & Yu, M. (2019). \emph{A collaborative filtering algorithm based on correlation coefficient}. Neural Computing and Applications, vol. 31, no. 12, pp. 8317-8326.
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
                    NaN,    1,  NaN,    4,    2,  NaN),nrow=6,byrow=TRUE)#items*users

sim <- simple_similarity(ratings, max_score=5, min_score=1, ac=1)
}
\keyword{Similarity}
