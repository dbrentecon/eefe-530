library(pacman)
p_load(dagitty, ggdag, ggplot2, dplyr, tibble, grid)


ggdag_eefe <- function(dag,
                       layout      = "fr",
                       label_mode  = c("inside", "outside", "name", "none"),
                       label_size  = 4.8,
                       node_size   = 12,
                       edge_width  = 1.4,
                       arrow_mm    = 6,
                       edge_shrink = 0.10,    # shorten edges so arrowheads are visible
                       obs_fill    = "black", # observed node fill
                       lat_fill    = "gray90", # latent node fill (NEW: makes latent obvious)
                       obs_outline = "black", # observed node outline
                       lat_outline = "gray90",# latent node outline
                       node_stroke = 1.1) {
  
  label_mode <- match.arg(label_mode)
  
  tidy_obj <- ggdag::tidy_dagitty(dag, layout = layout)
  td <- tidy_obj$data
  
  # Nodes = unique coordinates
  nodes <- td %>% dplyr::distinct(name, x, y)
  
  # Edges = rows with a destination
  edges <- td %>% dplyr::filter(!is.na(to))
  
  # ---- Parse latent node names from tidy_dagitty()$dag text (version-robust) ----
  .get_latent_names <- function(tidy_obj) {
    dag_txt <- paste(capture.output(tidy_obj$dag), collapse = "\n")
    lines <- strsplit(dag_txt, "\n", fixed = TRUE)[[1]]
    latent_lines <- grep("\\[.*\\blatent\\b.*\\]", lines, value = TRUE)
    latent_names <- sub("^\\s*([A-Za-z0-9_.]+)\\s*\\[.*$", "\\1", latent_lines)
    unique(trimws(latent_names))
  }
  
  # ---- Parse node labels from tidy_dagitty()$dag text ----
  .get_label_df <- function(tidy_obj) {
    dag_txt <- paste(capture.output(tidy_obj$dag), collapse = "\n")
    lines <- strsplit(dag_txt, "\n", fixed = TRUE)[[1]]
    
    node_lines <- grep("^\\s*[A-Za-z0-9_.]+\\s*\\[", lines, value = TRUE)
    
    nm <- sub("^\\s*([A-Za-z0-9_.]+)\\s*\\[.*$", "\\1", node_lines)
    
    lab <- ifelse(
      grepl('label="', node_lines, fixed = TRUE),
      sub('^.*label="([^"]+)".*$', "\\1", node_lines),
      NA_character_
    )
    
    tibble::tibble(name = trimws(nm), label = lab)
  }
  
  latent_names <- .get_latent_names(tidy_obj)
  label_df     <- .get_label_df(tidy_obj)
  
  nodes <- nodes %>%
    dplyr::mutate(latent = name %in% latent_names) %>%
    dplyr::left_join(label_df, by = "name") %>%
    dplyr::mutate(
      label = ifelse(is.na(label) | label == "", name, label),
      # NEW: explicit aesthetics to make latent obvious
      node_fill    = ifelse(latent, lat_fill, obs_fill),
      node_outline = ifelse(latent, lat_outline, obs_outline),
      text_col     = ifelse(latent, "black", "white")
    )
  
  # ---- Shorten edges so arrowheads don't overlap nodes ----
  edges <- edges %>%
    dplyr::mutate(
      dx = xend - x,
      dy = yend - y,
      d  = sqrt(dx^2 + dy^2),
      d  = ifelse(d == 0, NA_real_, d),
      
      x_new    = x    + edge_shrink * dx / d,
      y_new    = y    + edge_shrink * dy / d,
      xend_new = xend - edge_shrink * dx / d,
      yend_new = yend - edge_shrink * dy / d
    )
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = x_new, y = y_new, xend = xend_new, yend = yend_new),
      linewidth = edge_width,
      color = "black",
      lineend = "round",
      arrow = grid::arrow(length = grid::unit(arrow_mm, "mm"), type = "closed")
    ) +
    ggplot2::geom_point(
      data = nodes,
      ggplot2::aes(x = x, y = y),
      size = node_size,
      shape = 21,
      fill   = nodes$node_fill,
      color  = nodes$node_outline,
      stroke = node_stroke
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_void()
  
  # ---- Labels ----
  if (label_mode == "inside") {
    p <- p + ggplot2::geom_text(
      data = nodes,
      ggplot2::aes(x = x, y = y, label = label),
      color = nodes$text_col,
      fontface = "bold",
      size = label_size
    )
  } else if (label_mode == "name") {
    p <- p + ggplot2::geom_text(
      data = nodes,
      ggplot2::aes(x = x, y = y, label = name),
      color = nodes$text_col,
      fontface = "bold",
      size = label_size
    )
  } else if (label_mode == "outside") {
    p <- p + ggplot2::geom_text(
      data = nodes,
      ggplot2::aes(x = x, y = y, label = label),
      color = "black",
      fontface = "bold",
      size = label_size,
      nudge_y = 0.1
    )
  }
  
  p
}


g2 <- dagitty('dag {
  T [label="TOU_adopt"]
  Y [label="peak_kwh"]
  C [latent, label="C"]
  C -> T
  C -> Y
  T -> Y
}')

# Short variable names only (T, Y, C)
ggdag_eefe(g2, label_mode = "name")

# Full labels inside nodes
ggdag_eefe(g2, label_mode = "outside")



