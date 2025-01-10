export_updated <- function(matrix, raster, pathway) {

  original_crs <- raster::crs(raster)
  original_extent <- raster::extent(raster)

  updated_raster <- raster::raster(matrix)

  raster::crs(updated_raster) <- original_crs
  raster::extent(updated_raster) <- original_extent

  output_tif <- pathway
  raster::writeRaster(updated_raster, output_tif, overwrite = TRUE)
  cat("Updated land use raster saved to:", output_tif, "\n")

  par(mfrow = c(1, 2))
  sp::plot(raster, main = "Original Land Use")
  sp::plot(updated_raster, main = "Updated Land Use")

}
