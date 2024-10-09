library(here)
here_dir <- here("wnv_prediction")

################################################################################
## Extract data and compute scores
################################################################################

## All Counties
library(sf)
counties <- st_read(file.path(here_dir, "cb_2018_us_county_500k.shp"), quiet=T)

threshold <- 0.5

## Extract incidence and prediction for all years
all_inc_and_pred <- NULL
all_scores <- NULL
all_scores_th <- NULL
for (year in 2000:2007) {
  inc_and_pred <- data.frame(County = counties$GEOID)
  ## true incidence
  true_inc <- read.table(file.path(here_dir, year, "IBM", paste0("wnv_incidence_", year, ".csv")), header = T, sep = ",")
  inc_and_pred$Incidence <- NA
  inc_and_pred$Incidence <- true_inc$Incidence[match(inc_and_pred$County, true_inc$County)]
  inc_and_pred$label <- (inc_and_pred$Incidence >= 1) + 0
  ## predictions
  score <- NULL
  score_th <- NULL
  for (method in c("rrw", "ibm")) {
    pred_method <- read.table(file.path(here_dir, year, method, paste0("wnv_prediction_", year, ".csv")), header = T, sep = ",")
    ## Empirical Cumulative Distribution Function
    ee <- ecdf(pred_method$Incidence)
    inc_and_pred[[method]] <- NA
    inc_and_pred[[method]] <- pred_method$Incidence[match(inc_and_pred$County, pred_method$County)]
    ## Replace NA with 0 (no observation)
    inc_and_pred[is.na(inc_and_pred)] <- 0
    ## normalize counts with ecdf
    inc_and_pred[[paste0(method, "_norm")]] <- ee(inc_and_pred[[method]])
    # scores
    score <- rbind(score, c(sum(inc_and_pred[[method]] > 0 & inc_and_pred$Incidence > 0) / sum(inc_and_pred$Incidence > 0),
                            sum(inc_and_pred[[method]] == 0 & inc_and_pred$Incidence == 0) / sum(inc_and_pred$Incidence == 0),
                            method,
                            year))
    # scores
    score_th <- rbind(score_th, c(sum(inc_and_pred[[paste0(method, "_norm")]] > threshold & inc_and_pred$Incidence > 0) / sum(inc_and_pred$Incidence > 0),
                                  sum(inc_and_pred[[paste0(method, "_norm")]] <= threshold & inc_and_pred$Incidence == 0) / sum(inc_and_pred$Incidence == 0),
                                  method,
                                  year))
  }
  score <- as.data.frame(score)
  colnames(score) <- c("Sensitivity", "Specificity", "method", "year")
  score_th <- as.data.frame(score_th)
  colnames(score_th) <- c("Sensitivity", "Specificity", "method", "year")
  # store
  inc_and_pred$year <- year
  all_inc_and_pred <- rbind(all_inc_and_pred, inc_and_pred)
  all_scores <- rbind(all_scores, score)
  all_scores_th <- rbind(all_scores_th, score_th)
}
all_scores <- tidyr::pivot_longer(as.data.frame(all_scores), cols = c("Sensitivity", "Specificity"))
all_scores$value <- as.numeric(all_scores$value)
all_scores_th <- tidyr::pivot_longer(as.data.frame(all_scores_th), cols = c("Sensitivity", "Specificity"))
all_scores_th$value <- as.numeric(all_scores_th$value)

summary(all_inc_and_pred)

################################################################################
## Plot
################################################################################
ibm.col = rgb(220/255,13/255,10/255,alpha=0.6);
rrw.col = rgb(10/255,168/255,109/255,alpha=0.6);

## ROC curves
library(ROCR)
proc <- function(this_year) {
  par(mar = c(3.2, 3.2, 1, 0.1))
  inc_and_pred <- subset(all_inc_and_pred, year == this_year)
  # ibm
  pred <- prediction(inc_and_pred$ibm_norm, inc_and_pred$label)
  perf <- performance(pred,"tpr","fpr")
  plot(perf, col = ibm.col, lwd = 3, xlab = "", ylab = "", asp = 1)
  # title(xlab = "False positive rate", mgp = c(2, 0, 0))
  # title(ylab = "True positive rate", mgp = c(2, 0, 0))
  # rrw
  pred <- prediction(inc_and_pred$rrw_norm, inc_and_pred$label)
  perf <- performance(pred,"tpr","fpr")
  plot(perf, col = rrw.col, lwd = 3,
       add = TRUE)
  # line
  abline(a = 0, b = 1, lty = 2, lwd = 2)
  # title
  title(paste0(this_year))
  # legend
  if (this_year == 2000) legend("bottomright", legend = c("ibm", "rrw"), fill = c(ibm.col, rrw.col), border = FALSE, box.lty = 0)
}

library(cowplot)
library(ggplot2)
plot_all_roc <- plot_grid(ggdraw(function() proc(2000)), ggdraw(function() proc(2001)), ggdraw(function() proc(2002)), ggdraw(function() proc(2003)),
                          ggdraw(function() proc(2004)), ggdraw(function() proc(2005)), ggdraw(function() proc(2006)), ggdraw(function() proc(2007)),
                          # labels = paste0("(", letters[1:8], ")"),
                          label_size = 12, nrow = 2) +
  draw_label("False positive rate", x = 0.5, y = 0, vjust = -0.5, angle = 0) +
  draw_label("True positive rate", x= 0, y = 0.5, vjust = 1.5, angle = 90)

plot_all_roc

twocolumnwidth <- 10
ggsave(filename = file.path(here_dir, "roc_curves.pdf"),
       plot = plot_all_roc,
       width = twocolumnwidth,
       height = twocolumnwidth / 2,
       unit = "in")

## TPR and FPR with threshold 0
psens <- ggplot(all_scores, aes(x = year, y = value, colour = method)) +
  geom_point() +
  geom_line(aes(group = interaction(method))) +
  facet_wrap(vars(name)) +
  theme_bw() +
  xlab("") +
  scale_colour_manual(breaks = c("ibm", "rrw"), values = c(ibm.col, rrw.col), name = element_blank()) +
  theme(text = element_text(size = 10),
        # title = element_text(size = 7),
        # panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.01, 0.01),
        legend.justification = c("left", "bottom"),
        legend.key.size = unit(10, 'pt'),
        strip.placement = "outside",
        strip.background = element_blank(),
        # plot.margin = unit(c(0.1,0.1,-0.31,-0.5), "cm"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )

twocolumnwidth <- 10
ggsave(filename = file.path(here_dir, "sensitivity_specificity.pdf"),
       plot = psens,
       width = twocolumnwidth / 2,
       height = twocolumnwidth / 4,
       unit = "in")

## TPR and FPR with threshold
psens <- ggplot(all_scores_th, aes(x = year, y = value, colour = method)) +
  geom_point() +
  geom_line(aes(group = interaction(method))) +
  facet_wrap(vars(name)) +
  theme_bw() +
  xlab("") +
  scale_colour_manual(breaks = c("ibm", "rrw"), values = c(ibm.col, rrw.col), name = element_blank()) +
  theme(text = element_text(size = 10),
        # title = element_text(size = 7),
        # panel.grid.minor = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.01, 0.01),
        legend.justification = c("left", "bottom"),
        legend.key.size = unit(10, 'pt'),
        strip.placement = "outside",
        strip.background = element_blank(),
        # plot.margin = unit(c(0.1,0.1,-0.31,-0.5), "cm"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )

twocolumnwidth <- 10
ggsave(filename = file.path(here_dir, "sensitivity_specificity_threshold.pdf"),
       plot = psens,
       width = twocolumnwidth / 2,
       height = twocolumnwidth / 4,
       unit = "in")
