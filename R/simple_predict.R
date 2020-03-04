#' @export

simple_predict <-
  function(ratings, ratings2, ac){

    #---------------------------------conditions
    #1 ratings
    if (missing(ratings) || !is.matrix(ratings)){
      stop("No ratings supplied.")
    }

    #2 ratings2
    if (missing(ratings2) || !is.matrix(ratings2)){
      stop("No ratings2 supplied.")
    }

    #3 ac
    if (missing(ac) || !is.numeric(ac) || any(ac<=0) || any(ac>dim(ratings)[2])){
      stop("No active_user specified.")
    }
    #---------------------------------conditions

    sort_ratings_ac <- sort(ratings2[,ac],na.last = TRUE,method = "radix",
                            decreasing = TRUE, index.return = TRUE)

    predictedItems_ix <- sort_ratings_ac$ix
    #predictedItems_x <- sort_ratings_ac$x

    k1 <- dim(ratings)[1]

    for(j in 1:dim(ratings)[1])
    {
      if(!is.na(ratings[predictedItems_ix[j],ac]))
      {
        predictedItems_ix[j] <- NaN

        k1 <- k1-1
      }
    }

    predictedItems <- c(rep(NaN,k1))

    k2 <- 0

    for(j in 1:dim(ratings)[1])
    {
      if(!is.na(predictedItems_ix[j]))
      {
        k2 <- k2+1

        predictedItems[k2] <- predictedItems_ix[j]
      }
    }

    return(predictedItems)
  }

