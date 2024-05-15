# Copyright (C) {2024} {PB, SG}
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

################################################################################
## Content:
#
# Run phyrex and beast analyses.
#
################################################################################

library(here)
here_dir <- here("phyrex_beast_comparison/fixed_tree")
setwd(here_dir)

datestamp_day <- format(Sys.time(), "%Y-%m-%d")
Nrep <- 5

###############################################################################
## phyrex run
###############################################################################
setwd(here(here_dir, "phyrex"))

time_phyrex_all <- NULL
for (sample_every in c(100, 1000, 10000)) {
  time_phyrex_all[[paste0(sample_every)]] <- rep(NA, Nrep)
  name_WNV <- paste0("WNV_", sample_every)
  for (irep in 1:Nrep) {
    name_irep <- paste0(name_WNV, "_phyrex_", irep, ".xml")
    xml_file <- readLines(paste0(name_WNV, "_phyrex.xml"))
    xml_file <- sub(paste0("output.file=\"", name_WNV, "\""), paste0("output.file=\"", name_WNV, "_", irep, "\""), xml_file)
    writeLines(xml_file, name_irep)
    time_phyrex_all[[paste0(sample_every)]][irep] <- system.time(system(paste0("phyrex --xml=./", name_irep)))[3]
    time_phyrex_all[[paste0(sample_every)]][irep] / 60
  }
}

setwd(here_dir)

saveRDS(time_phyrex_all, file = file.path(here_dir, "results", paste0(datestamp_day, "_result_timing_phyrex.RDS")))
# time_phyrex_all <- readRDS(file = file.path(here_dir, "results", paste0(datestamp_day, "_result_timing_phyrex.RDS")))

###############################################################################
## beast run
###############################################################################
setwd(here(here_dir, "beast"))

# path to beagle install
# can be set to "" if beagle is not installed
# beagle_path <- "-Djava.library.path=/opt/homebrew/Cellar/beagle/4.0.1/lib/"
beagle_path <- ""

time_beast <- rep(NA, Nrep)
for (irep in 1:Nrep) {
  name_all <- "WNV_FixedTree_IBM_ind_RMfix"
  name_irep <- paste0(name_all, "_", irep)
  xml_file <- readLines("WNV_FixedTree_IBM_ind_RMfix.xml")
  xml_file <- sub(name_all, name_irep, xml_file)
  writeLines(xml_file, paste0(name_irep, ".xml"))
  time_beast[irep] <- system.time(system(paste0("java ", beagle_path, " -jar beast.jar ", name_irep, ".xml")))[3]
  time_beast[irep] / 60
}

setwd(here_dir)

saveRDS(time_beast, file = file.path(here_dir, "results", paste0(datestamp_day, "_result_timing_beast.RDS")))
# time_beast <- readRDS(file = file.path(here_dir, "results", paste0(datestamp_day, "_result_timing_beast.RDS")))

###############################################################################
## read and format logs
###############################################################################
library(tracerer)

sample_every <- 10000
name_WNV <- paste0("WNV_", sample_every)
time_phyrex <- time_phyrex_all[[paste0(sample_every)]]

all_estim_phyrex <- all_estim_beast <- NULL
for (irep in 1:Nrep) {
  ## phyrex
  logphyrex <- read.table(here(here_dir, "phyrex", paste0(name_WNV, "_", irep, "_phyrex_stats_ibm.txt")), header = TRUE)
  # burning
  logphyrex <- logphyrex[(nrow(logphyrex) %/% 10 + 1):nrow(logphyrex), ]
  sample_interval_phyrex <- logphyrex$sample[2] - logphyrex$sample[1]

  ## beast
  logbeast <- read.table(here(here_dir, "beast", paste0("WNV_FixedTree_IBM_ind_RMfix_", irep, ".log")), header = TRUE)
  # burning
  logbeast <- logbeast[(nrow(logbeast) %/% 10 + 1):nrow(logbeast), ]
  sample_interval_beast <- logbeast$state[2] - logbeast$state[1]

  ###############################################################################
  ## ESS
  get_estimates_ess <- function(vec, sample_interval) {
    return(c(calc_ess(vec, sample_interval), mean(vec), calc_hpd_interval(vec, proportion = 0.95)))
  }

  estim_phyrex <- apply(logphyrex, 2, function(vec) get_estimates_ess(vec, sample_interval_phyrex))
  estim_phyrex <- data.frame(parameter = colnames(estim_phyrex),
                            ess = estim_phyrex[1, ],
                            esspersec = estim_phyrex[1, ] / time_phyrex[irep],
                            mean = estim_phyrex[2, ],
                            HMDmin = estim_phyrex[3, ],
                            HMDmax = estim_phyrex[4, ],
                            time = time_phyrex[irep],
                            nitetations = max(logphyrex$sample),
                            irep = irep)
  all_estim_phyrex <- rbind(all_estim_phyrex, estim_phyrex)

  estim_beast <- apply(logbeast, 2, function(vec) get_estimates_ess(vec, sample_interval_beast))
  estim_beast <- data.frame(parameter = colnames(estim_beast),
                            ess = estim_beast[1, ],
                            esspersec = estim_beast[1, ] / time_beast[irep],
                            mean = estim_beast[2, ],
                            HMDmin = estim_beast[3, ],
                            HMDmax = estim_beast[4, ],
                            time = time_beast[irep],
                            nitetations = max(logbeast$state),
                            irep = irep)
  all_estim_beast <- rbind(all_estim_beast, estim_beast)
}

mean_estim_phyrex <- aggregate(. ~  parameter, data = all_estim_phyrex, mean)
mean_estim_beast <- aggregate(. ~ parameter, data = all_estim_beast, mean)

write.csv(all_estim_phyrex, file = file.path(here_dir, "results", paste0(datestamp_day, "_all_estim_phyrex.csv")))
write.csv(all_estim_beast, file = file.path(here_dir, "results", paste0(datestamp_day, "_all_estim_beast.csv")))
write.csv(mean_estim_phyrex, file = file.path(here_dir, "results", paste0(datestamp_day, "_mean_estim_phyrex.csv")))
write.csv(mean_estim_beast, file = file.path(here_dir, "results", paste0(datestamp_day, "_mean_estim_beast.csv")))
