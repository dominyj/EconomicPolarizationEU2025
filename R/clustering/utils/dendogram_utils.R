# Interne Hilfsfunktion .get_k aus factoextra
get_k_internal <- function(dend, k = NULL, h = NULL) {
  if (!is.null(h)) {
    k <- length(unique(dendextend::cutree(dend, h = h)))
  }
  k
}

# Angepasste rect_dendrogram Funktion
custom_rect_dendrogram <- function (dend, k = NULL, h = NULL, k_colors = NULL, palette = NULL,
                                    rect_fill = FALSE, rect_lty = 2, lower_rect = -1.5, rect_width_offset = 3.5, ...)
{
  if (missing(k_colors) & !is.null(palette))
    k_colors <- palette
  prop_k_height <- 0.5
  if (!dendextend::is.dendrogram(dend))
    stop("x is not a dendrogram object.")
  k <- get_k_internal(dend, k, h)  # Hier verwenden wir unsere eigene get_k Funktion
  tree_heights <- dendextend::heights_per_k.dendrogram(dend)[-1]
  tree_order <- stats::order.dendrogram(dend)
  if (is.null(k))
    stop("specify k")
  if (k < 2) {
    stop(gettextf("k must be between 2 and %d", length(tree_heights)),
         domain = NA)
  }
  cluster <- dendextend::cutree(dend, k = k)
  clustab <- table(cluster)[unique(cluster[tree_order])]
  m <- c(0, cumsum(clustab))
  which <- 1L:k
  xleft <- ybottom <- xright <- ytop <- list()
  for (n in seq_along(which)) {
    next_k_height <- tree_heights[names(tree_heights) ==
                                    k + 1]
    if (length(next_k_height) == 0) {
      next_k_height <- 0
      prop_k_height <- 1
    }
    xleft[[n]] = m[which[n]] + 0.66
    ybottom[[n]] = lower_rect - rect_width_offset
    xright[[n]] = m[which[n] + 1] + 0.33
    ytop[[n]] <- tree_heights[names(tree_heights) == k] *
      prop_k_height + next_k_height * (1 - prop_k_height)
  }
  df <- data.frame(xmin = unlist(xleft), ymin = unlist(ybottom),
                   xmax = unlist(xright), ymax = unlist(ytop), stringsAsFactors = TRUE)

  # Hier ist der geänderte Teil für die Farbbehandlung
  color <- k_colors
  if (length(color) == 1 && color == "cluster")
    color <- "default"
  if (ggpubr:::.is_col_palette(color))
    color <- ggpubr:::.get_pal(color, k = k)
  else if (length(color) > 1 & length(color) < k) {
    color <- rep(color, k)[1:k]
  }

  if (rect_fill) {
    fill <- color
    alpha <- 0.2
  }
  else {
    fill <- "transparent"
    alpha <- 0
  }
  df$color <- color
  df$cluster <- as.factor(paste0("c", 1:k))
  ggpubr::geom_exec(geom_rect, data = df, xmin = "xmin", ymin = "ymin",
                    xmax = "xmax", ymax = "ymax", fill = fill, color = color,
                    linetype = rect_lty, alpha = alpha, ...)
}
