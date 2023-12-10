myurl = "https://liangfgithub.github.io/MovieData/"
## movies data
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)

### convert accented characters
movies$Title[73]
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
movies$Title[73]

### extract year
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))
movies$MovieID <- sapply('m', paste0, movies$MovieID)
movies$Genres <- sapply(sapply(lapply(movies$Genres, strsplit, split = "\\|"), unlist), unlist)
# movies$Genres <- lapply(movies$Genres, strsplit, split = "\\|")