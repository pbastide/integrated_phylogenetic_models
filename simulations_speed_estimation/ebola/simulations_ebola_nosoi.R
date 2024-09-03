library(raster)
library(nosoi)
library(here)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(viridis)
library(phangorn)

################################################################################
## Utility functions to get realized average speed
################################################################################
# get distance on one edge
get_dist_haversine <- function(tree, ed, host_table, state_table) {
  parent_node <- tree@data[tree@data$node == ed[1], ]
  child_node <- tree@data[tree@data$node == ed[2], ]
  hosts_on_branch <- find_hosts(parent_node, child_node, host_table)
  dist_branch <- apply(hosts_on_branch, 1, get_dist_host, state_table = state_table)
  return(sum(dist_branch))
  # return(distHaversine(as.numeric(tree@data[tree@data$node == ed[1], c("state.x", "state.y")]),
  #                      as.numeric(tree@data[tree@data$node == ed[2][[1]], c("state.x", "state.y")]), r = 6371))
}
find_hosts <- function(parent_node, child_node, host_table) {
  current_host <- child_node$host
  current_time <- child_node$time
  host_chain <- NULL
  while(current_host != parent_node$host) {
    new_host <- host_table[host_table$hosts.ID == current_host, inf.by]
    new_time <- host_table[host_table$hosts.ID == current_host, inf.time]
    host_chain <- rbind(host_chain,
                        c(current_host, new_time, current_time))
    current_host <- new_host
    current_time <- new_time
  }
  host_chain <- rbind(host_chain,
                      c(current_host, parent_node$time, current_time))
  host_chain <- as.data.frame(host_chain)
  colnames(host_chain) <- c("host", "begin", "end")
  host_chain$begin <- as.numeric(host_chain$begin)
  host_chain$end <- as.numeric(host_chain$end)
  return(host_chain)
}
get_dist_host <- function(one_host, state_table) {
  host_states <- state_table[state_table$hosts.ID == one_host["host"] &
                               (state_table$time.to >= as.numeric(one_host["begin"]) | is.na(state_table$time.to)) &
                               state_table$time.from <= as.numeric(one_host["end"]), ]
  if (nrow(host_states) == 0) browser()
  if (nrow(host_states) == 1) return(0) # one line: no move
  dist_host <- 0
  for (i in 2:nrow(host_states)) {
    dist_host <- dist_host + geosphere::distHaversine(as.numeric(host_states[i-1, c("state.x", "state.y")]),
                                                      as.numeric(host_states[i, c("state.x", "state.y")]), r = 6371)
  }
  return(dist_host)
}
# sum over all edges
get_total_dist <- function(tree, host_table, state_table) {
  all_dist <- apply(tree@phylo$edge, 1, function(ed) get_dist_haversine(tree, ed, host_table, state_table))
  return(sum(all_dist))
}
# get average speed
get_average_speed <- function(tree, host_table, state_table) {
  total_dist <- get_total_dist(tree, host_table, state_table)
  total_time <- sum(tree@phylo$edge.length)
  return(total_dist / total_time)
}

################################################################################
## Set up simulation
## https://slequime.github.io/nosoi/articles/examples/ebola.html
################################################################################
## Data
data_dir <- here("simulations_speed_estimation/ebola/data_ebola")
data_file <- file.path(data_dir, "Africa_pop_2015.tif")
if (!file.exists(data_file)) { # download file if needed
  tmp_file <- file.path(data_dir, "Africa_pop_2015.zip")
  raster_url <- "https://github.com/slequime/nosoi/raw/master/docs/articles/examples/Africa_pop_2015.tif.zip"
  download.file(raster_url, tmp_file)
  data_file <- unzip(tmp_file, exdir = data_dir, overwrite = TRUE)[1]
  unlink(tmp_file)
}
e_WestAfrica_close <- extent(-18, -3, 3, 15) # WorldPop Africa raster
population_close <- crop(raster(data_file), e_WestAfrica_close)
e_WestAfrica <- extent(-18, 10, 3, 20) # WorldPop Africa raster
population <- crop(raster(data_file), e_WestAfrica)
start.pos <- c(-10.132, 8.561) # Guéckédou in Guinea

ggplot() + layer_spatial(population) + labs(x="", y="") +
  scale_fill_viridis(trans = "log1p",option="A",limits=c(0,NA),na.value="NA",
                     name="Population size", position="bottom",
                     breaks=c(6,60,600,6000,60000),labels=c(6,60,600,6000,60000))

geosphere::distHaversine(start.pos, start.pos + c(0.1, 0.1), r = 6371)

#pExit:
p_Exit_fct <- function(t, t_incub){
  t_recovery = 20 # days before recovery
  if (t <= t_incub) { p = 0 }
  if ((t > t_incub)&(t < (t_incub+t_recovery))) { p = 0.025 } # 50% chance to dye before recovery
  if (t > (t_incub+t_recovery)) { p = 1 }
  return(p)
}
#pMove:
p_Move_fct <- function(t, t_incub){
  if (t <= t_incub) { p = 1 }
  if (t > t_incub) { p = 0}
  return(p)}
#sdMove:
sdMove_fct <- function(t){0.1} # sd of 15.63696 km per move
#nContact:
n_contact <- function(t, current.env.value, host.count){
  if(host.count < current.env.value){return(3)}
  if(host.count >= current.env.value){return(0)}
}
#pTrans:
t_incub_fct <- function(x){
  t_incub <- c()
  while (length(t_incub) == 0)
  {
    t_incub = rnorm(x, mean=6.5, sd=2.5)
    t_incub = t_incub[(t_incub>=2)&(t_incub<=21)]
    if (length(t_incub) > 0)
    {
      if (length(t_incub) < x)
      {
        print(t_incub)
        t_incub <- c(t_incub, sample(t_incub,x-length(t_incub)))
      }
    }
  }
  return(t_incub)
}
proba <- function(t, t_incub) {
  if (t <= t_incub) { p = 0 }
  if (t > t_incub) { p = 0.025 }
  return(p)
}

################################################################################
## Simulate
################################################################################
set.seed(20240701)
for (n_sim in 1:100) {
  ninf <- 0
  i_sim <- 0
  while (ninf <= 100) {
    i_sim <- i_sim + 1
    print(i_sim)
    sim_ebola <- try(nosoiSim(
      type = "single", popStructure = "continuous",
      length = 365,
      max.infected = 10000,
      init.individuals = 1,
      init.structure = start.pos,
      structure.raster = population,

      #pExit:
      diff.pExit = FALSE,
      timeDep.pExit = FALSE,
      pExit = p_Exit_fct,
      param.pExit = list(t_incub = t_incub_fct),

      #pMove:
      diff.pMove = FALSE,
      timeDep.pMove = FALSE,
      pMove = p_Move_fct,
      param.pMove = list(t_incub = t_incub_fct),

      #sdMove:
      diff.sdMove = FALSE,
      timeDep.sdMove = FALSE,
      sdMove = sdMove_fct,
      param.sdMove = NA,
      attracted.by.raster = FALSE,

      #nContact:
      diff.nContact = TRUE,
      timeDep.nContact = FALSE,
      hostCount.nContact = TRUE,
      nContact = n_contact,
      param.nContact = NA,

      #pTrans:
      diff.pTrans = FALSE,
      timeDep.pTrans = FALSE,
      pTrans = proba,
      param.pTrans = list(t_incub = t_incub_fct),

      #Closing parameters:
      prefix.host = "H",
      print.progress = FALSE
    )
    )
    if (inherits(sim_ebola, "try-error")) {
      ninf <- 0
    } else {
      ninf <- sim_ebola$host.info.A$N.infected
    }
  }

  ################################################################################
  ## extract and save simulation results
  ################################################################################

  ## extract results
  host_ebola <- getTableHosts(sim_ebola)
  state_ebola <- getTableState(sim_ebola)
  tree_ebola <- getTransmissionTree(sim_ebola)

  ## sample
  active_hosts <- subset(host_ebola, active == 0)
  sum(!duplicated(active_hosts$current.in.x) | !duplicated(active_hosts$current.in.y))
  sampled_hosts <- sample(active_hosts$hosts.ID, 50)
  tree_ebola_sampled <- sampleTransmissionTreeFromExiting(tree_ebola, sampled_hosts)

  ## realized speed
  true_speed <- get_average_speed(tree_ebola_sampled, host_ebola, state_ebola) # km per day

  ## write data
  tree_sim <- tree_ebola_sampled@phylo
  data_pos <- tree_ebola_sampled@data[, c("host", "state.y", "state.x", "time")]
  data_pos <- data_pos[match(tree_sim$tip.label, data_pos$host), ]
  tree_sim$tip.label <- paste(tree_sim$tip.label, data_pos$time, sep = "_")
  tree_sim$node.label <- NULL
  data_pos$host <- paste(data_pos$host, data_pos$time, sep = "_")
  data_pos <- data_pos[, c("host", "state.y", "state.x")]
  colnames(data_pos) <- c("Name", "Lat", "Long")

  write.table(data_pos,
              file = file.path(data_dir, paste0(n_sim, "_sim_nosoi_coord.txt")),
              quote = FALSE, row.names = FALSE)
  ape::write.tree(tree_sim,
                  file = file.path(data_dir, paste0(n_sim, "_sim_nosoi_tree.tree")))
  write.table(true_speed,
              file = file.path(data_dir, paste0(n_sim, "_sim_nosoi_true_speed.txt")),
              quote = FALSE, row.names = FALSE, col.names = FALSE)

  ## write formated dates for phyrex
  phyrex_dates <- NULL
  for (tax in 1:length(tree_sim$tip.label)) {
    tax_name <- tree_sim$tip.label[tax]
    tax_date <- strsplit(tax_name, "_")[[1]][2]
    tax_date_string <- c(
      paste0("<clade id=\"clade_", tax, "\">"),
      paste0("\t<taxon value=\"", tax_name, "\"/>"),
      "</clade>",
      paste0("<calibration id=\"calib_", tax, "\">"),
      paste0("\t<lower>", tax_date, "</lower>"),
      paste0("\t<upper>", tax_date, "</upper>"),
      paste0("\t<appliesto clade.id=\"clade_", tax, "\"/>"),
      "</calibration>"
    )
    phyrex_dates <- c(tax_date_string, phyrex_dates)
  }
  write(phyrex_dates, file.path(data_dir, paste0(n_sim, "_sim_nosoi_phyrex_dates.txt")))

  ################################################################################
  ## Simulate sequences
  ################################################################################
  # kimura model
  # Q-matrix is such that rate is 1
  tstv <- 4
  # alpha <- tstv / (tstv + 1) # this is using tstv = alpha / (2 beta)
  # beta <- 1 / (tstv + 1) / 2
  alpha <- tstv / (tstv + 2) # this is using tstv = alpha / (beta)
  beta <- 1 / (tstv + 2)
  Q_matrix <- matrix(c(-1,   alpha, beta, beta,
                       alpha, -1,   beta, beta,
                       beta,  beta, -1,   alpha,
                       beta,  beta, alpha, -1   ),
                     ncol = 4, nrow = 4)
  colnames(Q_matrix) <- rownames(Q_matrix) <- c("a", "g", "c", "t")
  # mean of 0.1 substitution on each branch
  rate <- 0.1 / mean(tree_sim$edge.length)
  write.table(rate,
              file = file.path(data_dir, paste0(n_sim, "_sim_nosoi_true_rate.txt")),
              quote = FALSE, row.names = FALSE, col.names = FALSE)
  seq_sim <- simSeq(tree_sim,
                    l = 500,
                    Q = Q_matrix,
                    bf = c(0.25, 0.25, 0.25, 0.25),
                    rate = rate,
                    type = "DNA")
  write.phyDat(seq_sim,
               file = file.path(data_dir, paste0(n_sim, "_sim_nosoi_seq.txt")),
               format = "phylip")

  ################################################################################
  ## plot
  ################################################################################
  # Extracting node data from the tree (including long/lat coordinates):
  tree.node <- tidytree::as_tibble(tree_ebola_sampled)
  write.table(tree.node, file.path(data_dir, paste0(n_sim, "_sim_nosoi_treedata.txt")))
  # Joining table to get coordinates of starting and ending nodes:
  tree.links <- left_join(tree.node, tree.node, by = c("parent" = "node"), suffix = c(".to", ".from"))

  #plotting everything
  ggplot() +
    layer_spatial(population_close) + labs(x="", y="") +
    scale_fill_viridis(trans = "log1p",option="A",limits=c(0,NA),na.value="NA",
                       name="Population size", position="bottom",
                       breaks=c(6,60,600,6000,60000),labels=c(6,60,600,6000,60000)) +
    scale_color_viridis(option="D",limits=c(0,NA),na.value="white", name="time") +
    theme(panel.background = element_blank(), panel.ontop = FALSE,
          panel.grid.major = element_line(size=0.25, linetype="dashed", colour ="grey70"),
          axis.ticks=element_blank(), plot.caption=element_text(size=10)) +
    geom_segment(data=tree.links, aes(x = state.x.from, y = state.y.from, xend = state.x.to, yend = state.y.to),
                 color="gray50") +
    geom_point(data=tree.node, aes(x=state.x, y=state.y,color=time))
  ggsave(file = file.path(data_dir, paste0(n_sim, "_sim_nosoi_plot.pdf")))

}
