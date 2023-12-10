S <- read.csv('top_30.csv')
rownames(S) <- S[,1]
S <- S[,-1]
mr <- read.table('movie_ratings.txt', header = TRUE)
mr.names <- rownames(mr)
mr <- mr[,1]
names(mr) <- mr.names

myIBCF <- function(newuser){
  ## newuser: a 3706-by-1 vector of a new user's movie ratings
  ## Output: the top 10 recommended movies for this user
  #### It is possible that there are fewer than 10 predictions, provide a backup to fill in remaining spots
  #### Do not recommend a movie the user has already ranked
  
  ## movie.ratings will also need to be downloaded by the app
  # ratings <- read.csv('Rmat.csv'
  #                   , stringsAsFactors = FALSE
  #                   , header = TRUE)
  # mr <- colMeans(ratings, na.rm = TRUE)
  
  ## Load movie.ratings data object 
  unranked <- colnames(newuser[,is.na(newuser)]) ## Determine which movies haven't been ranked and thus can be used for recommendation
  recs <- rep(0, length(unranked))
  names(recs) <- unranked
  for (i in 1:length(unranked)){
    l <- unranked[i] 
    sl <- S[rownames(S) == l,]
    tmp.names <- colnames(sl[,!is.na(sl)])
    sl <- sl[!is.na(sl)]
    names(sl) <- tmp.names
    w <- as.matrix(newuser[,colnames(newuser) %in% names(sl)])
    w[,is.na(w)] <- 0
    rec.numerator <- sum(w * sl)
    rec.denom <- sum(sl[w != 0])
    recs[i] <- rec.numerator / rec.denom 
  }
  top.recs <- sort(recs, decreasing = TRUE) ## sort for finding ties
  if (sum(!is.na(top.recs)) >= 10){
    break.tie <- mr[which(names(mr) %in% names(top.recs))]
    top.recs2 <- cbind(top.recs, break.tie[names(top.recs)])
    top.recs2 <- top.recs2[
      order(top.recs2[,1], top.recs2[,2], decreasing = TRUE),]
    ret.top.recs <- top.recs2[1:10,1]
  } else if (sum(!is.na(top.recs)) > 0) { ## In case there are fewer than 10 non-NA recommendations
    ret.top.recs <- rep(0, 10)
    k <- sum(!is.na(top.recs))
    ret.top.recs[1:k] <- top.recs[!is.na(top.recs)]
    names(ret.top.recs)[1:k] <- names(top.recs[!is.na(top.recs)])
    names.exclude <- c(colnames(newuser[,!is.na(newuser)]), names(ret.top.recs))
    ret.top.recs[(k+1):10] <- sort(
      mr[!names(mr) %in% names.exclude]
      , decreasing = TRUE)[1:(10-k)]
    names(ret.top.recs)[(k+1):10] <- names(sort(
      mr[!names(mr) %in% names.exclude]
      , decreasing = TRUE))[1:(10-k)]
  } else { ## In case there are no non-NA recommendations
    ret.top.recs <- sample(mr, 10)
  }
  return (ret.top.recs)
}

newuser <- data.frame(matrix(NA, 1, ncol(S)))
colnames(newuser) <- colnames(S)