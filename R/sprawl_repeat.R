sprawl_ex <- function(matrix, urban_values, p) {

  total_conversions = 0
  updated_matrix = matrix

  repeat {

    result <- urban_sprawl(updated_matrix, urban_values, (p - total_conversions))
    updated_matrix <- result$matrix
    total_conversions <- total_conversions + result$current_ratio
    if (total_conversions >= p) break

  }

  updated_raster = raster(updated_matrix)

  return(list(updated_matrix = updated_matrix, updated_raster= updated_raster, current_ratio = total_conversions))

}
