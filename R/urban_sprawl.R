#' Simulate urban sprawl
#'
#' Expands urban land-use cells into neighbouring non-urban cells.
#'
#' @param x A matrix containing land-use classes.
#' @param urban_values A numeric vector of values representing urban land use.
#' @param expansion Proportional increase in urban cells.
#' @return A list containing the updated matrix and expansion ratio.
#' @export


urban_sprawl <- function(x,
                         urban_values,
                         expansion
                         ) 
{
  
  if (!is.matrix(x)) {
    stop("`x` must be a matrix.")
  }
  
  if (!is.numeric(urban_values) || length(urban_values) == 0) {
    stop("`urban_values` must be a non-empty numeric vector.")
  }
  
  if (!is.numeric(expansion) ||
      length(expansion) != 1 ||
      expansion < 0) {
    stop("`expansion` must be a single non-negative number.")
  }
  
  
  original_urban_count <- sum(
    x %in% urban_values,
    na.rm = TRUE
  )
  
  if (original_urban_count == 0) {
    stop("No urban cells were found.")
  }
  
  target_conversions <- ceiling(
    original_urban_count * expansion
  )
  
  conversions <- 0
  
  while (conversions < target_conversions) {
    
    candidates <- list()
    
    nr <- nrow(x)
    nc <- ncol(x)
    
    for (i in seq_len(nr)) {
      for (j in seq_len(nc)) {
        
        if (is.na(x[i, j]) ||
            x[i, j] %in% urban_values) {
          next
        }
        
        neighbours <- c()
        
        if (i > 1)
          neighbours <- c(neighbours, x[i - 1, j])
        
        if (i < nr)
          neighbours <- c(neighbours, x[i + 1, j])
        
        if (j > 1)
          neighbours <- c(neighbours, x[i, j - 1])
        
        if (j < nc)
          neighbours <- c(neighbours, x[i, j + 1])
        
        neighbours <- neighbours[
          !is.na(neighbours)
        ]
        
        urban_neighbours <- neighbours[
          neighbours %in% urban_values
        ]
        
        if (length(urban_neighbours) > 0) {
          
          candidates[[length(candidates) + 1]] <-
            list(
              i = i,
              j = j,
              neighbours = urban_neighbours
            )
        }
      }
    }
    
    if (length(candidates) == 0) {
      warning(
        "No additional cells are adjacent to urban cells."
      )
      break
    }
    
    selected <- sample(candidates, 1)[[1]]
    
    x[selected$i, selected$j] <-
      sample(selected$neighbours, 1)
    
    conversions <- conversions + 1
  }
  
  list(
    matrix = x,
    new_urban_cells = conversions,
    expansion = conversions / original_urban_count
  )
}
