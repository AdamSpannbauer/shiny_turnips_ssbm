options(scipen=999)
library(data.table)
source('helper_functions.R')

# read in turnip odds
turnip_dt = fread('turnip_data.csv')
turnip_dt = turnip_dt[order(-percent),]
turnip_dt[, file := factor(file, file)]

# read in turnip images to be used by ggplot
grob_list = create_grob_list(turnip_dt$file)

# init storage for aggregated history
agg_history = data.table(file = factor(turnip_dt$file, turnip_dt$file),
                         N = 0)

n = 10000  # number of pulls per round
n_rounds = 15
for (i in seq(n_rounds)) {
  new_pull_dt = pull_turnip(turnip_dt, n = n)
  agg_history = update_agg_history(agg_history, new_pull_dt)
  agg_plot = bar_plot_agg_history(agg_history, grob_list, y_lim = n * n_rounds * 0.6)
  prefix = paste0(rep('z', i), collapse = '')  # ensure order of frames
  ggsave(paste0('frames/', prefix, 'frame_', i, '.png'), agg_plot)
}

# create gif (assumes imagemagick)
system('convert -delay 30 frames/*.png turnip_pull.gif')

# clear out frames dir
# system('rm -rf frames/*')