make_plot <- function(dat, var, outline) {
  p <- ggplot()
  if (inherits(dat, "sf")) {
    
    p <- ggplot() +
      geom_sf(data = dat, col = "red") +
      geom_sf(data = outline, fill = NA, col = "black") +
      coord_sf()
    
  } else {
    
    fill_aes <- if (is.numeric(dat[[var]])) {
      scale_fill_gradientn(colours = terrain.colors(255))
    } else {
      scale_fill_discrete()
    }
    
    p <- ggplot(dat) +
      geom_tile(aes(x=x, y=y, fill = .data[[var]])) +
      fill_aes + geom_sf(data = outline, fill = NA, col="black")
}
  p
}