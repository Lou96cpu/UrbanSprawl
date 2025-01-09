urban_sprawl <- function(matrix, urban_values) {

  n_rows <- nrow(matrix)
  n_cols <- ncol(matrix)

  new_conversions <- 0

  for (i in 1:n_rows) {
    for(j in 1:n_cols) {
      if (is.na(matrix[i, j]) || matrix[i, j] %in% urban_values) next

      neighbors <- c()
      if (i > 1) neighbors <- c(neighbors, matrix[i - 1, j])
      if (i < n_rows) neighbors <- c(neighbors, matrix[i + 1, j])
      if (j > 1) neighbors <- c(neighbors, matrix[i, j - 1])
      if (j < n_cols) neighbors <- c(neighbors, matrix[i, j + 1])

      neighbors <- neighbors[!is.na(neighbors)]

      if (any(neighbors %in% urban_values)) {
        matrix[i, j] <- sample(neighbors, 1) # changed to random neighbor value
        new_conversions <- new_conversions + 1
      }
    }
  }

  return(list(matrix, new_conversions = new_conversions))

}
