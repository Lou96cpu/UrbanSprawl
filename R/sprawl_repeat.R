#' Repeat urban sprawl
#'
#' @param x A matrix containing land-use classes.
#' @param urban_values A numeric vector of values representing urban land use.
#' @param expansion Proportional increase in urban cells.
#' @return A list containing the updated matrix and expansion ratio.

sprawl_ex <- function(x, urban_values, expansion) {

  total_conversions = 0
  updated_matrix = x

  original_urban_count <- sum(
    x %in% urban_values,
    na.rm = TRUE
  )
     
  target_conversions <- ceiling(
    original_urban_count * expansion)

  repeat {

    result <- urban_sprawl(updated_matrix, urban_values, (target_conversions - total_conversions))
    updated_matrix <- result$matrix
    total_conversions <- total_conversions + result$expansion
    if (total_conversions >= p) break

  }

  updated_raster = raster(updated_matrix)

  return(list(updated_matrix = updated_matrix, updated_raster= updated_raster, current_ratio = total_conversions))

}
