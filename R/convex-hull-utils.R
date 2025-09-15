#' Smooth a convex hull
#' by inserting circle segments
#'
#' @param p Set of points that form a convex hull, counterclockwise
#' @param arc Maximum arc between consecutive points (radians)
#' @param radius Radius of the circles around vertexes
#'
#' @returns Ser of points that form the smoothed hull
#' @export
#'
#' @examples
smooth_convex_hull <- function(p, arc = pi / 36, radius = NULL) {
  # default radius, 2% of max(width, height)
  if (is.null(radius)) {
    radius <- max(max(p$x) - min(p$x), max(p$y) - min(p$y)) * 0.02
  }
  
  # number of points
  n <- nrow(p)
  
  # vertex before and after
  bef <- c(n, 1:(n-1))
  aft <- c(2:n, 1)
  
  # directions from vertex perpendicular to before and after edges
  rbef <- atan2(p$y[bef] - p$y, p$x[bef] - p$x) + pi / 2
  raft <- atan2(p$y[aft] - p$y, p$x[aft] - p$x) - pi / 2
  
  # correct rbef > raft
  raft <- ifelse(rbef > raft, raft + 2 * pi, raft)
  
  # arc to cover
  dist <- raft - rbef
  
  # number of points
  ns <- ceiling(dist / arc)
  
  # initialize return frame
  s <- data.frame(x = numeric(), y = numeric())
  
  # main cycle
  for (v in 1:n) {
    # angles to points
    ang <- seq(rbef[v], raft[v], length.out = ns[v])
    
    s <- rbind(
      s,
      data.frame(
        x = p$x[v] + cos(ang) * radius,
        y = p$y[v] + sin(ang) * radius
      )
    )
  }
  
  return(s)
}