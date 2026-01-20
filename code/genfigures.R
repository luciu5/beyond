rm(list = ls())
basedir <- file.path(Sys.getenv("HOME"), "Projects", "beyond")
# setwd(basedir)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(scales)
simpath <- file.path(basedir, "data")
load(file.path(simpath, "AllSims.RData"))


relleverage <- sort(unique(res_mkt$relleveragePre), decreasing = TRUE)


res_mkt <- mutate(res_mkt,
  Merger = factor(merger, levels = c("vertical", "up", "down", "both"), labels = c("Vertical", "Upstream", "Downstream", "Integrated")),
  Cost = factor(mc,
    levels = c("constant", "linprod", "lincons", "conslin", "consparty", "linparty"),
    labels = c(
      "Constant",
      "Linear", "Linear/Constant", "Constant/Linear", "Party Constant", "Party Linear" # "Quadratic","Linear/Quadratic","Quadratic/Linear","Linear/Constant","Constant/Linear"
    )
  )
) %>%
  mutate(
    cvtrun = ifelse(cv / mktrev.pre * 100 > 200, 200, cv / mktrev.pre * 100),
    cvtrun = ifelse(cv / mktrev.pre * 100 < -200, -200, cvtrun),
    hhipostcut = cut(hhipost, seq(1600, 10000, 200), right = FALSE, dig.lab = 5),
    hhideltacut = cut(hhidelta, seq(0, 2500, 100), right = FALSE, dig.lab = 5),
    Cost = reorder(Cost, -cvtrun, median, na.rm = TRUE),
    up = as.numeric(as.character(up)),
    down = as.numeric(as.character(down)),
    vert = as.numeric(as.character(vert)),
    up = factor(ifelse(merger == "both", up - 2, up)),
    down = factor(ifelse(merger == "both", down - 2, down)),
    vert = factor(ifelse(merger == "both", vert - 2, vert))
  )


with(res_mkt, prop.table(table(hhipost > 1800 & hhidelta > 100)))
with(res_mkt, table(hhipostcut, hhideltacut))
with(res_mkt, table(hhipostcut, hhd))
with(res_mkt, table(hhd, hhideltacut))
with(res_mkt, table(up, down, vert))


res_mkt <- filter(res_mkt, hhipost > 1800 & hhidelta >= 100) %>% mutate(
  hhipostcut = droplevels(hhipostcut),
  hhideltacut = droplevels(hhideltacut)
)


seq_palette <- RColorBrewer::brewer.pal(name = "YlGnBu", n = 8)[c(4, 6, 8)]

omit_label <- function(labels, n = 3) {
  sapply(seq_along(labels), function(i) if ((i - 1) %% n == 0) labels[i] else "")
}


boxfun <- function(x, probs = c(.05, .25, .5, .75, .95)) {
  r <- quantile(x, probs, na.rm = TRUE)
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(r)
}


psummary_deltahhi.bw <- ggplot(
  filter(
    res_mkt, # hhidelta <=2500 &
    Cost == "Constant" & nestParm == "0" &
      !is.na(hhipostcut) &
      # !is.na(hhideltacut) &
      (vert %in% c("0", "1", "3"))
  ) %>% mutate(hhd = factor(hhd)),
  aes(y = cvtrun, x = hhd)
) +
  stat_summary(fun.data = boxfun, geom = "boxplot", position = "dodge") +
  # coord_cartesian(ylim=c(-20,25))+
  # scale_y_continuous(breaks=seq(-20,25,5))+
  scale_x_discrete(labels = omit_label) +
  scale_color_manual(values = seq_palette) +
  facet_grid(vert ~ Merger, scale = "free_y") +
  theme_bw() +
  xlab("Change in HHI") +
  ylab("Consumer Harm (%)") +
  geom_hline(yintercept = 0, color = "goldenrod", linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  ggtitle("Consumer harm v. HHI change, by merger type and # integrated incumbents") +
  labs(
    colour = "Retail Game:",
    subtitle = "Harm reported as a percentage of pre-merger market revenues. Negative values denote harm."
  )


psummary_hhi.bw <- ggplot(
  filter(
    res_mkt, # hhipost <=5000 &
    Cost == "Constant" & nestParm == "0" &
      !is.na(hhipostcut) &
      !is.na(hhideltacut) &
      vert %in% c("0", "1", "2")
  ),
  aes(
    y = cvtrun, x = hhipostcut
    # ,color=vert
  )
) +
  # ))+ #geom_boxplot() +
  stat_summary(fun.data = boxfun, geom = "boxplot", position = "dodge") +
  # coord_cartesian(ylim=c(-40,10))+
  # scale_y_continuous(breaks=seq(-40,10,5))+
  scale_x_discrete() +
  scale_color_manual(values = seq_palette) +
  facet_grid(vert ~ Merger, scale = "free_y") +
  theme_bw() +
  xlab("Post-merger HHI") +
  ylab("Consumer Harm (%)") +
  geom_hline(yintercept = 0, color = "goldenrod", linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  ggtitle("Consumer harm v. post-merger HHI, by merger type and # integrated incumbents") +
  labs(
    colour = "Retail Game:",
    subtitle = "Harm reported as a percentage of pre-merger market revenues. Negative values denote harm."
  )


psummary_heat <- ggplot(
  filter(
    res_mkt, # abs(cv/mktrev.pre*100)<=80 &
    hhidelta <= 2500 &
      nestParm == "0"
  ) %>%
    filter(!is.na(hhipostcut) & !is.na(hhideltacut)) %>%
    group_by(Merger, hhipostcut, hhideltacut) %>%
    summarise(median_value = median(cvtrun), .groups = "drop"),
  aes(x = hhideltacut, y = hhipostcut, fill = median_value)
) +
  scale_x_discrete(labels = omit_label) +
  scale_y_discrete(labels = omit_label) +
  facet_grid(~Merger, scale = "free_y") +
  geom_tile(color = "grey80") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    name = "Median CV"
  ) +
  labs(
    x = "HHI Change",
    y = "Post-merger HH",
    title = "Median Consumer Harm by HHI Change, Post-merger HHI"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

delast <- ggplot(
  filter(
    res_mkt,
    mktElast > -3 &
      vert == 0
  ),
  aes(x = mktElast)
) +
  geom_histogram(bins = 100) +
  facet_grid(~Merger, scale = "free_y") +
  theme_bw()

tapply(res_mkt$mktElast, res_mkt$Merger, quantile, probs = c(.01, 0.25, 0.5, 0.75, .99))

pfreq <- ggplot(
  res_mkt %>% filter(hhidelta < 1e3 & hhipost < 1e4),
  aes(x = hhidelta, y = hhipost)
) +
  facet_grid(~Merger, scale = "free_x") +
  geom_point(shape = ".") +
  geom_hline(yintercept = 1800, color = "goldenrod") +
  geom_vline(xintercept = 100, color = "goldenrod") +
  theme_bw()

pfreq_heat <- ggplot(
  res_mkt %>%
    group_by(Merger, hhipostcut, hhideltacut) %>%
    summarise(freq = n(), .groups = "drop"),
  aes(x = hhideltacut, y = hhipostcut, fill = freq)
) +
  scale_x_discrete(labels = omit_label) +
  scale_y_discrete(labels = omit_label) +
  facet_grid(~Merger, scale = "free_y") +
  geom_tile(color = "grey80") +
  scale_fill_gradient(
    low = "white", high = "red",
    name = "Frequency"
  ) +
  labs(
    x = "HHI Change",
    y = "Post-merger HH",
    title = "Median Consumer Harm by HHI Change, Post-merger HHI"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#
# ## box and whisker results for Incumbent
#

pvertincumb.bw <- ggplot(res_mkt, aes(
  y = cvtrun,
  x = vert # color=Cost
  # ,color=Outcome
)) +
  stat_summary(fun.data = boxfun, geom = "boxplot", position = "dodge") +
  # coord_cartesian(ylim=c(-60,55))+
  scale_y_continuous(breaks = seq(-100, 100, 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "goldenrod") +
  theme_bw() +
  scale_colour_tableau("Color Blind") +
  theme(legend.position = "bottom") +
  facet_grid(~Merger, scales = "free", labeller = label_context) +
  xlab("# Vertically Integrated Firms") +
  ylab("Outcome (%)") +
  labs(colour = "Outcome:") +
  labs(
    title = "The Distributions  of Merger Outcomes as the Number of Integrated Firms Increases",
    subtitle = "Outcomes are reported as a percentage of pre-merger total expenditures."
  )


psummary.bw <- ggplot(res_mkt, aes(
  y = cvtrun,
  # avgpricedelta/mktrev.pre*100,
  x = Merger # ,color=Cost
)) +
  stat_summary(fun.data = boxfun, geom = "boxplot", position = "dodge") +
  # coord_cartesian(ylim=c(-60,55))+
  scale_y_continuous(breaks = seq(-100, 100, 5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "goldenrod") +
  theme_bw() +
  scale_colour_tableau("Color Blind") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  # scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  # theme_tufte(ticks=FALSE) +
  # geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  # facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  # facet_grid(~Merger,scales="free",labeller = "label_context")+
  xlab("Change in Surplus") +
  ylab("Outcome (%)") +
  # ylab("Avg. Downstream Price Change (%)")+
  # ylab("Share-Weighted Downstream Price Change")+
  # geom_text(data=ann_text,label="Wholesale advantage")
  # labs(colour="Cost:")+
  labs(
    title = "The Distributions  of Merger Outcomes",
    subtitle = "Outcomes are reported as a percentage of pre-merger total expenditures."
    # subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
    # caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )

pprofits <- ggplot(
  data = filter(profitableMarkets, mc == "constant" & preset == "none") %>% group_by(merger, vert) %>%
    summarize(`Profitable Mergers (%)` = sum(isprofitable) / sum(n) * 100) %>% ungroup() %>%
    mutate(Merger = factor(merger, levels = c("vertical", "up", "down", "both"), labels = c("Vertical", "Upstream", "Downstream", "Integrated"))),
  aes(x = vert, y = `Profitable Mergers (%)`, color = Merger)
) +
  geom_point() +
  geom_line(aes(group = Merger)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Percentage of Profitable Mergers, By # of Integrated Firms" # ,
    # subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
  )

pprofits_barg <- ggplot(
  data = filter(profitableMarkets, mc == "constant" & preset == "none" & vert %in% c(0, 1, 2, 4, 6)) %>% group_by(merger, barg, vert) %>%
    summarize(`Profitable Mergers (%)` = sum(isprofitable) / sum(n) * 100) %>% ungroup() %>%
    mutate(Merger = factor(merger, levels = c("vertical", "up", "down", "both"), labels = c("Vertical", "Upstream", "Downstream", "Integrated"))),
  aes(x = factor(barg, labels = MASS::fractions(relleverage)), y = `Profitable Mergers (%)`, color = vert)
) +
  facet_grid(~Merger) +
  geom_point() +
  geom_line(aes(group = vert)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Percentage of Profitable Mergers, By Bargaining Parameter" # ,
    # subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(name = "YlGnBu", n = 9)[c(4, 5, 6, 8, 10) - 1]) +
  geom_vline(xintercept = 5, linetype = "dashed", color = "black") +
  xlab("Relative Bargaining Power")

pprofits_barg <- pprofits_barg + geom_segment(
  aes(x = x, xend = xend, y = y, yend = y),
  color = "black", arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "closed"), size = 1, show.legend = FALSE,
  data = data.frame(
    x = 5.1, y = 15, xend = 9,
    # Outcome=factor( "Consumer" ,levels=unique(res_mkt.logit$Outcome)),
    Merger = factor("Integrated", levels = c("Vertical", "Upstream", "Downstream", "Integrated"))
  )
) +
  geom_text(aes(x = x, y = y),
    color = "black", label = "equal power", angle = 90,
    data = data.frame(
      x = 4.5, y = 40, # Outcome=factor( "Consumer" ,levels=unique(res_mkt.logit$Outcome)),
      Merger = factor("Integrated", levels = c("Vertical", "Upstream", "Downstream", "Integrated"))
    ), size = 3.5
  ) +
  geom_text(aes(x = x, y = y),
    color = "black", label = "more retailer power",
    data = data.frame(
      x = 6.75, y = 20, # Outcome=factor( "Consumer" ,levels=unique(res_mkt.logit$Outcome)),
      Merger = factor("Integrated", levels = c("Vertical", "Upstream", "Downstream", "Integrated"))
    ), size = 2.5
  )


pbargincumbent.bw <- ggplot(
  res_mkt %>%
    filter(
      !is.na(hhipostcut) &
        !is.na(hhideltacut) &
        vert %in% c("0", "1", "2")
    ),
  aes(
    y = cvtrun,
    x = factor(barg, labels = MASS::fractions(relleverage)), color = vert
  )
) +
  # geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data = boxfun, geom = "boxplot", position = "dodge") +
  # coord_cartesian(ylim=c(-50,50))+
  scale_y_continuous(breaks = seq(-100, 100, by = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 5, linetype = "dotted") +
  theme_bw() + # scale_colour_tableau('Color Blind')+
  # scale_color_brewer(palette = "PuRd",direction=-1)+
  scale_color_manual(values = seq_palette) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  # scale_x_discrete(labels=rev(levels(res_mkt$relleveragePre)))+
  # scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  # theme_tufte(ticks=FALSE) +
  # geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  # facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Merger, labeller = "label_context") +
  xlab("Relative Bargaining Power") +
  ylab("Outcome (%)") +
  # ylab("Avg. Downstream Price Change (%)")+
  # ylab("Share-Weighted Downstream Price Change")+
  # geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour = "# Integrated Firms:") +
  labs(
    title = "How Changing Bargaining Strength Affects Consumer Surplus, By Merger",
    subtitle = "Outcomes are reported as a percentage of pre-merger total expenditures."
    # subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
    # caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )


## Output results


png(file.path(basedir, "output", "surplussum_hhi.png"), width = 13, height = 13, units = "in", res = 300)
print(psummary_hhi.bw)
dev.off()


png(file.path(basedir, "output", "surplussum_deltahhi.png"), width = 13, height = 13, units = "in", res = 300)
print(psummary_deltahhi.bw)
dev.off()

png(file.path(basedir, "output", "surplussum.png"), width = 10, height = 7, units = "in", res = 300)
print(psummary.bw)
dev.off()

png(file.path(basedir, "output", "hhidist.png"), width = 13, height = 13, units = "in", res = 300)
print(pfreq)
dev.off()


png(file.path(basedir, "output", "CVbargincumbentBW.png"), width = 10, height = 7, units = "in", res = 300)
print(pbargincumbent.bw)
dev.off()

png(file.path(basedir, "output", "pprofits.png"), width = 7, height = 7, units = "in", res = 300)
print(pprofits)
dev.off()

# png(file.path(basedir, "output", "pprofits_barg.png"), width = 10, height = 7, units = "in", res = 300)
# print(pprofits_barg)
# dev.off()
