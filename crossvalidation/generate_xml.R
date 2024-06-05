library(here)

setwd(here("crossvalidation"))

## read file
xml_base_name <- list.files(full.names = TRUE)
xml_base_name <- xml_base_name[grepl("wnv_config_[0-9].*[0-9]\\.xml", xml_base_name)]

xml_base <- readLines(xml_base_name, warn = FALSE)
model_line <- grepl("<spatialmodel name=\"ibm\"", xml_base)

## IBM file
writeLines(xml_base, "wnv_config_ibm.xml")

## IOU file
xml_iou <- xml_base
xml_iou[model_line] <- "<spatialmodel name=\"iou\" rw.prior.distrib=\"flat\" rw.prior.mean=\"1.\" sampling=\"detection\" integrateAncestralLocations=\"true\" distance.type=\"greatcircle\" observational.model=\"no\">"
writeLines(xml_iou, "wnv_config_iou.xml")

## RRW file
xml_rrw <- xml_base
xml_rrw[model_line] <- "<spatialmodel name=\"rrw+gamma\" rw.prior.distrib=\"flat\" rw.prior.mean=\"1.\" sampling=\"detection\" integrateAncestralLocations=\"true\" distance.type=\"greatcircle\" observational.model=\"no\">"
writeLines(xml_rrw, "wnv_config_rrw.xml")

