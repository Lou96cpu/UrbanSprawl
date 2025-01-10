urban_sprawl <- function(matrix, urban_values, p_current) {

  n_rows <- nrow(matrix)
  n_cols <- ncol(matrix)

  original_urban_count <- sum(getValues(land_use) %in% urban_values, na.rm = TRUE)

  new_conversions <- 0
  org_non_urban <- 0
  org_i <- c()
  org_j <- c()
  potential_pixel <- c()
  potential_neighbors <- c()

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
        potential_pixel <- c(potential_pixel, matrix[i, j])
        potential_neighbors <- c(potential_neighbors, neighbors)
        org_i <- c(org_i, i)
        org_j <- c(org_j, j)
        org_non_urban <- org_non_urban + 1

      }
    }
  }

  for (k in 1:org_non_urban) {
    num <- length(potential_pixel)
    tem <- sample(1:num, 1)

    matrix[org_i[tem], org_j[tem]] <- sample(potential_neighbors[tem], 1) # change to a random neighbor value
    new_conversions <- new_conversions + 1

    potential_pixel <- potential_pixel[-num]
    potential_neighbors <- potential_neighbors[-num]
    org_i <- org_i[-num]
    org_j <- org_j[-num]

    current_ratio <- new_conversions / original_urban_count

    if (current_ratio >= p_current) break

  }

  return(list(matrix = matrix, current_ratio = current_ratio))

}
