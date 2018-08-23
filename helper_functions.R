library(ggplot2)
library(data.table)
library(png)
library(grid)

pull_turnip = function(turnip_dt, n = 1) {
  inds = sample(
    nrow(turnip_dt),
    size = n,
    replace = TRUE,
    prob = turnip_dt$percent
  )
  return(turnip_dt[inds,])
}

update_agg_history = function(agg_history_dt, new_pull_dt) {
  new_agg = agg_pulls(new_pull_dt)
  full_agg_dt = merge(agg_history_dt, new_agg, by = 'file', all.x = TRUE)
  full_agg_dt[is.na(N.y), N.y := 0]
  full_agg_dt[, N := N.x + N.y]
  full_agg_dt[, c('file', 'N')]
}

read_to_grob = function(turnip_file) {
  img = readPNG(paste0('images/', turnip_file))
  g = rasterGrob(img, interpolate = TRUE)
  return(g)
}

create_grob_list = function(turnip_files) {
  grob_list = lapply(turnip_files, read_to_grob)
  names(grob_list) = turnip_files
  return(grob_list)
}

bar_plot_agg_history = function(agg_history, grob_list, y_lim = NA) {
  if (is.na(y_lim)) {
    y_lim = max(agg_history$N)
  }
  bar_plot = ggplot(agg_history, aes(x = file, y = N, fill = file)) +
    geom_bar(stat = 'identity') +
    labs(
      x = '',
      y = 'Number of Pulls',
      title = paste0('Turnip Counts After ', sum(agg_history$N), ' Pulls')
    ) +
    ylim(-1, y_lim * 1.1) +
    guides(fill = FALSE) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  x_val = 1
  for (file_i in agg_history$file) {
    y_val = agg_history[file == file_i, N]
    bar_plot = bar_plot +
      annotation_custom(
        grob_list[[file_i]],
        xmin = x_val - 0.97,
        xmax = x_val + 1.03,
        ymin = y_val - y_lim * 0.08,
        ymax = y_val + y_lim * 0.08
      )
    x_val = x_val + 1
  }
  return(bar_plot)
}
