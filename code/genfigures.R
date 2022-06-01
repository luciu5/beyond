rm(list=ls())
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(scales)
simpath <- "./data"
load(file.path(simpath,"AllSims.RData"))


## Box and whisker plots for FirmCount Experiment
#res.firms <- mutate(ungroup(res.firms),up=factor(up,levels=as.character(seq(min(nufirms),max(nufirms),1))),
#                              Retailers=factor(Retailers,levels=as.character(seq(min(ndfirms),max(ndfirms),1))))
#res.barg <- filter(res.barg, !relleveragePre %in% c("9","18"))
#res.barg <- mutate(ungroup(res.barg),
#                    Retailers=factor(Retailers,levels=as.character(seq(min(ndfirms),max(ndfirms),1))),
#                    relleveragePre=factor(relleveragePre,levels=as.character(seq(0.2,3,.2))
#)

relleverage=sort(unique(res.nests$relleveragePre),decreasing = TRUE)


## Only include constant cost sims
res.nests.allcost  <- mutate(ungroup(res.nests.long),
                                            Merger=factor(merger,levels=c("vertical","up","down","both"),labels=c("Vertical","Upstream","Downstream","Integrated")),
                                            Cost=factor(mc,
                                                        levels=c("constant","linprod","lincons","conslin","consparty","linparty"),
                                                        labels=c("Constant"
                                                                    ,"Linear","Linear/Constant","Constant/Linear","Party Constant","Party Linear"# "Quadratic","Linear/Quadratic","Quadratic/Linear","Linear/Constant","Constant/Linear"
                                                                    )
                                                        ),
                                            Cost=reorder(Cost,-Outcome_value/mktrev.pre,median,na.rm=TRUE)) %>% select(-mc)


res.nests.logit <- res.nests.long <- res.nests.allcost %>% filter(Cost=="Constant")


res.nest_diagonal.allcost  <- ungroup(res.nest_all.long) %>% filter(preset != "none") %>% mutate(
                             Merger=factor(merger,levels=c("vertical","up","down","both"),labels=c("Vertical","Upstream","Downstream","Integrated")),
                             Cost=factor(mc,
                                         levels=c("constant","linprod","lincons","conslin","consparty","linparty"),
                                         labels=c("Constant"
                                                  ,"Linear","Linear/Constant","Constant/Linear","Party Constant","Party Linear"# "Quadratic","Linear/Quadratic","Quadratic/Linear","Linear/Constant","Constant/Linear"
                                         )
                             ),
                             Cost=reorder(Cost,-Outcome_value/mktrev.pre,median,na.rm=TRUE)) %>% select(-mc)



#res.nests.logit <- filter(as.data.frame(res.nests.logit),nestParm == "0" & Retailers !="1" & Wholesalers !="1") %>%
#  mutate_if(is.factor,droplevels)
#partdata <- filter(partdata,up!="1" & down !="1" )%>%
#  mutate_if(is.factor,droplevels)

seq_palette <- RColorBrewer::brewer.pal(name="YlGnBu",n=8)[c(4,6,8)]

boxfun <- function(x,probs=c(.05,.25,.5,.75,.95)){

  r <- quantile(x, probs,na.rm=TRUE)
  names(r) <- c("ymin","lower","middle","upper","ymax")
  return(r)
}



psummary_cost.bw <-  ggplot(filter(res.nests.allcost,Outcome %in% c("Consumer","Total") & !Cost %in% c("Constant/Linear","Linear/Constant")) %>%
                              mutate(isParty=factor(grepl("Party",as.character(Cost)),labels=c("All","Party")),
                                     Cost=reorder(gsub("Party ","",as.character(Cost)),ifelse((Outcome=="Consumer") & (Merger=="Vertical"),-Outcome_value/mktrev.pre,NA),quantile,probs=.5,na.rm=TRUE),

                                     ),
                            aes(y=Outcome_value/mktrev.pre*100,
                                            #avgpricedelta/mktrev.pre*100,
                                            x=Merger,color=Cost
)
) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-60,55))+
  scale_y_continuous(breaks=seq(-100,100,5))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(isParty~Outcome,scales="fixed",labeller = "label_context")+
  xlab("Change in Surplus")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  #labs(colour="Cost:")+
  labs(title =  "The Distributions  of Consumer and Total Surplus\nFor Different Cost Structures",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )


#
# ## box and whisker results for Incumbent
#

pvertincumb.bw <-  ggplot(res.nests.logit %>% filter(Outcome %in% c("Consumer","Total") #& Cost=="Constant"
                                                     ), aes(y=Outcome_value/mktrev.pre*100,
                                           #avgpricedelta/mktrev.pre*100,
                                           x=vert#color=Cost
                                           #,color=Outcome
                                           )
                                           ) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-60,55))+
  scale_y_continuous(breaks=seq(-100,100,10))+
  geom_hline(yintercept=0,linetype="dashed",color="goldenrod")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(Outcome~Merger,scales="free",labeller = label_context)+
  xlab("# Incumbent Vertically Integrated Firms")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Outcome:")+
  labs(title =  "The Distributions  of Merger Outcomes as the Number of Integrated Firms Increases",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures.\nHorizontal mergers occur between a vertically integrated and unintegrated firm."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )

pvertincumb_c.bw <-  ggplot(res.nests.logit %>% filter(Outcome %in% c("Consumer") #& Cost=="Constant"
) %>% mutate(vert=as.numeric(as.character(vert)),
        vert=ifelse(merger=="both",vert-2,vert),
        vert=factor(vert)), aes(y=Outcome_value/mktrev.pre*100,
       #avgpricedelta/mktrev.pre*100,
       x=vert#color=Cost
       #,color=Outcome
)
) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-60,55))+
  scale_y_continuous(breaks=seq(-100,100,10))+
  geom_hline(yintercept=0,linetype="dashed",color="goldenrod")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Merger,scales="free",labeller = label_context)+
  xlab("# Incumbent Vertically Integrated Firms")+
  ylab("Consumer Surplus (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Outcome:")+
  labs(title =  "The Distribution of Consumer Surplus Changes as the Number of Integrated Firms Increases",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures.\nHorizontal mergers occur between a vertically integrated and unintegrated firm."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )

pvertincumb_t.bw <-  ggplot(res.nests.logit %>% filter(Outcome %in% c("Total") #& Cost=="Constant"
) %>% mutate(vert=as.numeric(as.character(vert)),
             vert=ifelse(merger=="both",vert-2,vert),
             vert=factor(vert)), aes(y=Outcome_value/mktrev.pre*100,
       #avgpricedelta/mktrev.pre*100,
       x=vert#color=Cost
       #,color=Outcome
)
) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-60,55))+
  scale_y_continuous(breaks=seq(-100,100,10))+
  geom_hline(yintercept=0,linetype="dashed",color="goldenrod")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Merger,scales="free",labeller = label_context)+
  xlab("# Incumbent Vertically Integrated Firms")+
  ylab("Consumer Surplus (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Outcome:")+
  labs(title =  "The Distribution of Total Surplus Changes as the Number of Integrated Firms Increases",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures.\nHorizontal mergers occur between a vertically integrated and unintegrated firm."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )



pvertincumb_updown.bw <-  ggplot(res.nests.logit %>% filter(Outcome %in% c("Consumer","Total") & Merger %in% c("Downstream","Upstream") #& Cost=="Constant"
), aes(y=Outcome_value/mktrev.pre*100,
       #avgpricedelta/mktrev.pre*100,
       x=vert,#color=Cost
       color=Outcome
)
) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-60,55))+
  scale_y_continuous(breaks=seq(-100,100,10))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Merger,scales="free",labeller = label_context)+
  xlab("# Pre-merger Vertically Integrated Firms")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Outcome:")+
  labs(title =  "The Distributions of Upstream and Downstream Merger Outcomes\nas the Number of Integrated Firms Increases",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures.\nHorizontal mergers occur between a vertically integrated and unintegrated firm."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )

pvertincumb_vertint.bw <-  ggplot(res.nests.logit %>% filter(Outcome %in% c("Consumer","Total") & Merger %in% c("Vertical","Integrated") #& Cost=="Constant"
), aes(y=Outcome_value/mktrev.pre*100,
       #avgpricedelta/mktrev.pre*100,
       x=vert,#color=Cost
       color=Outcome
)
) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-60,55))+
  scale_y_continuous(breaks=seq(-100,100,10))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Merger,scales="free",labeller = label_context)+
  xlab("# Pre-merger Vertically Integrated Firms")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Outcome:")+
  labs(title =  "The Distributions of Vertical and Integrated Merger Outcomes\nas the Number of Integrated Firms Increases",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures.\nHorizontal mergers occur between a vertically integrated and unintegrated firm."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )


pup_vertincumb.bw <-  ggplot(filter(res.nests.logit,Merger=="Upstream" & Outcome=="Consumer"), aes(y=Outcome_value/mktrev.pre*100,
                                                                                                     #avgpricedelta/mktrev.pre*100,
                                                                                                     x=vert,color=Cost
)
) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-60,55))+
  scale_y_continuous(breaks=seq(-100,100,5))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(Retailers~Wholesalers,scales="free_x",labeller = label_both)+
  xlab("# Pre-merger Vertically Integrated Firms")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Cost:")+
  labs(title =  "The Distribution of Consumer Harm from Upstream Mergers as the Number of Pre-merger Integrated Firms Increases",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )

pdown_vertincumb.bw <-  ggplot(filter(res.nests.logit,Merger=="Downstream" & Outcome=="Consumer"), aes(y=Outcome_value/mktrev.pre*100,
                                                                                                   #avgpricedelta/mktrev.pre*100,
                                                                                                   x=vert,color=Cost
)
) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-60,55))+
  #scale_y_continuous(breaks=seq(-100,100,5))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(Retailers~Wholesalers,scales="free_x",labeller = label_both)+
  xlab("# Pre-merger Vertically Integrated Firms")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Cost:")+
  labs(title =  "The Distribution of Consumer Harm from Downstream Mergers as the Number of Pre-merger Integrated Firms Increases",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )




pvert_vertincumb.bw <-  ggplot(filter(res.nests.logit,Merger=="Vertical" & Outcome=="Consumer"), aes(y=Outcome_value/mktrev.pre*100,
                                               #avgpricedelta/mktrev.pre*100,
                                               x=vert,color=Cost
)
) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-60,55))+

  #scale_y_continuous(breaks=seq(-100,100,5))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = label_both)+
  facet_grid(Retailers~.,scales="free_x",labeller = label_both)+
  xlab("# Pre-merger Vertically Integrated Firms")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Cost:")+
  labs(title =  "The Distribution of Consumer Harm from Vertical Mergers as the Number of Pre-merger Integrated Firms Increases",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )

pboth_vertincumb.bw <-  ggplot(filter(res.nests.logit,Merger=="Integrated" & Outcome=="Consumer"), aes(y=Outcome_value/mktrev.pre*100,
                                                                                                     #avgpricedelta/mktrev.pre*100,
                                                                                                     x=vert,color=Cost
)
) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-60,55))+

  #scale_y_continuous(breaks=seq(-100,100,5))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = label_both)+
  facet_grid(Retailers~Wholesalers,scales="free_x",labeller = label_both)+
  xlab("# Pre-merger Vertically Integrated Firms")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Cost:")+
  labs(title =  "The Distribution of Consumer Harm From Integrated Mergers as the Number of Pre-merger Integrated Firms Increases",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )

psummary.bw <-  ggplot(res.nests.logit, aes(y=Outcome_value/mktrev.pre*100,
                                            #avgpricedelta/mktrev.pre*100,
                                            x=Merger#,color=Cost
)
) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-60,55))+
  scale_y_continuous(breaks=seq(-100,100,5))+
  geom_hline(yintercept=0,linetype="dashed",color="goldenrod")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(legend.position="bottom",axis.text.x = element_text(angle = 45,hjust=1))+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Outcome,scales="free",labeller = "label_context")+
  xlab("Change in Surplus")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  #labs(colour="Cost:")+
  labs(title =  "The Distributions  of Merger Outcomes",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )

pprofits <- ggplot(data=filter(profitableMarkets,mc=="constant" & preset=="none") %>% group_by(merger,vert) %>%
                     summarize(`Profitable Mergers (%)`=sum(isprofitable)/sum(n)*100)%>% ungroup() %>%
                     mutate(Merger=factor(merger,levels=c("vertical","up","down","both"),labels=c("Vertical","Upstream","Downstream","Integrated"))),
                   aes(x=vert,y=`Profitable Mergers (%)`,color=Merger)
) + geom_point()+geom_line(aes(group=Merger)) +theme_bw() +theme(legend.position = "bottom") +
  labs(title =  "Percentage of Profitable Mergers, By # of Integrated Firms"#,
      # subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
  )

pprofits_barg <- ggplot(data=filter(profitableMarkets,mc=="constant" & preset=="none" & vert %in% c(0,1,2,4,6)) %>% group_by(merger,barg,vert) %>%
                     summarize(`Profitable Mergers (%)`=sum(isprofitable)/sum(n)*100)%>% ungroup() %>%
                     mutate(Merger=factor(merger,levels=c("vertical","up","down","both"),labels=c("Vertical","Upstream","Downstream","Integrated"))),
                   aes(x=factor(barg,labels=MASS::fractions(relleverage)),y=`Profitable Mergers (%)`,color=vert)
) + facet_grid(~Merger) + geom_point()+geom_line(aes(group=vert)) +theme_bw() +theme(legend.position = "bottom") +
  labs(title =  "Percentage of Profitable Mergers, By Bargaining Parameter"#,
       #subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
  ) + scale_color_manual(values = RColorBrewer::brewer.pal(name="YlGnBu",n=9)[c(4,5,6,8,10)-1]) +
geom_vline(xintercept=5,linetype="dashed",color="black") +xlab("Relative Bargaining Power")

pprofits_barg <- pprofits_barg + geom_segment(
    aes(x=x,xend=xend,y=y,yend=y),color="black",arrow=arrow(length=unit(0.3,"cm"),ends="last",type="closed"),size=1, show.legend = FALSE,
    data=data.frame(x=5.1,y=15,xend=9
                    ,#Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                    Merger=factor("Integrated", levels=c("Vertical","Upstream","Downstream","Integrated")))) +
  geom_text(aes(x=x,y=y),color="black",label="equal power",angle=90,
            data=data.frame(x=4.5,y=40,#Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                            Merger=factor("Integrated", levels=c("Vertical","Upstream","Downstream","Integrated"))),size=3.5) +
  geom_text(aes(x=x,y=y),color="black",label="more retailer power",
            data=data.frame(x=6.75,y=20,#Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                            Merger=factor("Integrated", levels=c("Vertical","Upstream","Downstream","Integrated"))),size=2.5)






pnests.bw <-  ggplot(filter(ungroup(res.nest_all.long),Outcome %in% c("Consumer","Total")) %>%
                       rename(Merger=merger) %>%
                       mutate(Merger=factor(Merger,levels=c("up","down","vertical"),
                                            labels=c("Upstream","Downstream","Vertical"))),
                     aes(y=Outcome_value/mktrev.pre*100,
                                  #avgpricedelta/mktrev.pre*100,
                                  x=nestParm,color=Cost)) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-100,20))+
  #scale_y_continuous(breaks=seq(-100,20,10))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_wrap(Merger~Outcome,scales="free",labeller = "label_context",ncol=2)+
  xlab("Nesting Parameter")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Cost:")+
  labs(title =  "How Changing the Nesting Parameter Affects Merger Outcomes",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )




pfirmsup_all.bw <- ggplot(filter(res.nests.logit,#Wholesalers != "2" &
                                 merger =="up"), aes(y=Outcome_value/mktrev.pre*100,
                                                                  #avgpricedelta/mktrev.pre*100,
                                                                  x=Wholesalers,color=Cost)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-45,20))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  scale_y_continuous(breaks=seq(-45,20,5))+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Outcome,scales="free",labeller = "label_context")+
  xlab("Number of Wholesalers")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Cost:")+
  labs(title =  "How Changing the Number of Wholesalers Affects Surplus\n in a Merger Among Wholesalers",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )








pfirmsdown_all.bw <- ggplot(filter(ungroup(res.nests.logit),merger =="down") , aes(y=Outcome_value/mktrev.pre*100,
                                                 #avgpricedelta/mktrev.pre*100,
                                                 x=Retailers,color=Cost)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-45,20))+
  scale_y_continuous(breaks=seq(-55,25,5))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Outcome,scales="free",labeller = "label_context")+
  xlab("Number of Retailers")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Cost:")+
  labs(title =  "How Changing the Number of Retailers Affects Surplus\n in a Merger Among Retailers",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )



pfirmsvert_all.bw <- ggplot(filter(ungroup(res.nests.logit),merger =="vertical" & Wholesalers != "1")%>%
                              mutate(Retailers=factor(Retailers, labels=paste0("Retailers: ", levels(Retailers)))), aes(y=Outcome_value/mktrev.pre*100,
                                                                     #avgpricedelta/mktrev.pre*100,
                                                                     x=Wholesalers,color=Cost)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-25,25))+
  geom_hline(yintercept=0,linetype="dashed",color="red")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(Outcome~Retailers,scales="free_y",labeller = "label_context")+
  xlab("Number of Wholesalers")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Cost:")+
  labs(title =  "How Changing the Number of Wholesalers and Retailers Affects Surplus\n in a Vertical Merger",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )



pfirmsvert_wholesalers.bw <- ggplot(filter(ungroup(res.nests.logit),merger =="vertical"  & Wholesalers != "1")%>%
                                      mutate_if(is.factor,droplevels)%>%
                              mutate(Retailers=factor(Retailers, labels=paste0("Retailers: ", levels(Retailers)))), aes(y=Outcome_value/mktrev.pre*100,
                                                                                                                        #avgpricedelta/mktrev.pre*100,
                                                                                                                        x=Wholesalers,color=Cost)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-62.5,60))+
  scale_y_continuous(breaks=seq(-100,100,5))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Outcome,scales="free_y",labeller = "label_context")+
  xlab("Number of Wholesalers")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Cost:")+
  labs(title =  "How Changing the Number of Wholesalers Affects Surplus\n in a Vertical Merger",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )


pfirmsvert_retailers.bw <- ggplot(filter(ungroup(res.nests.logit),merger =="vertical"  & Retailers != "1")%>%
                                    mutate_if(is.factor,droplevels)#%>%
                                      #mutate(Retailers=factor(Retailers, labels=paste0("Retailers: ", levels(Retailers))))
                                  , aes(y=Outcome_value/mktrev.pre*100,
                                                                                                                                #avgpricedelta/mktrev.pre*100,
                                                                                                                                x=Retailers,color=Cost)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-62.5,60))+
  scale_y_continuous(breaks=seq(-100,100,5))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.barg$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Outcome,scales="free_y",labeller = "label_context")+
  xlab("Number of Retailers")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Cost:")+
  labs(title =  "How Changing the Number of Retailers Affects Surplus\n in a Vertical Merger",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )


pbargnoboth.bw <- ggplot(filter(res.nests.logit,Outcome %in% c("Consumer","Total") & merger !="both" & vert %in% c("0","1","4")), aes(y=Outcome_value/mktrev.pre*100,
                                                                                               #avgpricedelta/mktrev.pre*100,
                                                                                               x=factor(barg,labels=MASS::fractions(relleverage)),color=vert)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-60,60))+
  scale_y_continuous(breaks=seq(-100, 100, by=10) ) +
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_vline(xintercept=5,linetype="dotted")+
  theme_bw()+#scale_colour_tableau('Color Blind')+
  #scale_color_brewer(palette = "PuRd",direction=-1)+
  scale_color_manual(values = seq_palette)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.nests$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(Outcome~Merger,scales="free",labeller = "label_context")+
  xlab("Relative Bargaining Power")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="# Integrated Firms:")+
  labs(title =  "How Changing Bargaining Strength Affects Consumer and Total Surplus, By Merger",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )


pbargnoboth.bw <- pbargnoboth.bw + geom_segment(
  aes(x=x,xend=xend,y=y,yend=y),color="black",arrow=arrow(length=unit(0.3,"cm"),ends="last",type="closed"),size=1, show.legend = FALSE,
  data=data.frame(x=5.1,y=-50,xend=9
                  ,Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                   Merger=factor("Vertical", levels=unique(res.nests.logit$Merger)))) +
  geom_text(aes(x=x,y=y),color="black",label="equal power",angle=90,
            data=data.frame(x=4.5,y=30,Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                                                        Merger=factor("Vertical", levels=unique(res.nests.logit$Merger))),size=3.5) +
  geom_text(aes(x=x,y=y),color="black",label="more retailer power",
            data=data.frame(x=7.5,y=-40,Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                            Merger=factor("Vertical", levels=unique(res.nests.logit$Merger))),size=3.5)


pbargincumbent.bw <- ggplot(filter(res.nests.logit,Outcome %in% c("Consumer","Total") &
                                  ((merger !="both" & vert %in% c("0","1","4")) | (merger =="both" & vert %in% c("2","3","6")))) %>%
                            mutate(vert=as.numeric(as.character(vert)),
                                   vert=ifelse(merger=="both",vert-2,vert),
                                   vert=factor(vert)),
                           aes(y=Outcome_value/mktrev.pre*100,
                           #avgpricedelta/mktrev.pre*100,
                          x=factor(barg,labels=MASS::fractions(relleverage)),color=vert)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-50,50))+
  scale_y_continuous(breaks=seq(-100, 100, by=10) ) +
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_vline(xintercept=5,linetype="dotted")+
  theme_bw()+#scale_colour_tableau('Color Blind')+
  #scale_color_brewer(palette = "PuRd",direction=-1)+
  scale_color_manual(values = seq_palette)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.nests$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(Outcome~Merger,labeller = "label_context")+
  xlab("Relative Bargaining Power")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="# Integrated Firms:")+
  labs(title =  "How Changing Bargaining Strength Affects Consumer and Total Surplus, By Merger",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )


pbargincumbent.bw <- pbargincumbent.bw + geom_segment(
  aes(x=x,xend=xend,y=y,yend=y),color="black",arrow=arrow(length=unit(0.3,"cm"),ends="last",type="closed"),size=1, show.legend = FALSE,
  data=data.frame(x=5.1,y=-50,xend=9
                  ,Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                  Merger=factor("Vertical", levels=unique(res.nests.logit$Merger)))) +
  geom_text(aes(x=x,y=y),color="black",label="equal power",angle=90,
            data=data.frame(x=4.5,y=30,Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                            Merger=factor("Vertical", levels=unique(res.nests.logit$Merger))),size=3.5) +
  geom_text(aes(x=x,y=y),color="black",label="more retailer power",
            data=data.frame(x=7.5,y=-40,Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                            Merger=factor("Vertical", levels=unique(res.nests.logit$Merger))),size=3.5)



pbargincumbent_c.bw <- ggplot(filter(res.nests.logit,Outcome %in% c("Consumer") &
                                     ((merger !="both" & vert %in% c("0","1","4")) | (merger =="both" & vert %in% c("2","3","6")))) %>%
                              mutate(vert=as.numeric(as.character(vert)),
                                     vert=ifelse(merger=="both",vert-2,vert),
                                     vert=factor(vert)),
                            aes(y=Outcome_value/mktrev.pre*100,
                                #avgpricedelta/mktrev.pre*100,
                                x=factor(barg,labels=MASS::fractions(relleverage)),color=vert)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-50,50))+
  scale_y_continuous(breaks=seq(-100, 100, by=10) ) +
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_vline(xintercept=5,linetype="dotted")+
  theme_bw()+#scale_colour_tableau('Color Blind')+
  #scale_color_brewer(palette = "PuRd",direction=-1)+
  scale_color_manual(values = seq_palette)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.nests$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Merger,labeller = "label_context")+
  xlab("Relative Bargaining Power")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="# Integrated Firms:")+
  labs(title =  "How Changing Bargaining Strength Affects Consumer Surplus, By Merger",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )


pbargincumbent_c.bw <- pbargincumbent_c.bw + geom_segment(
  aes(x=x,xend=xend,y=y,yend=y),color="black",arrow=arrow(length=unit(0.3,"cm"),ends="last",type="closed"),size=1, show.legend = FALSE,
  data=data.frame(x=5.1,y=-50,xend=9
                  ,Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                  Merger=factor("Vertical", levels=unique(res.nests.logit$Merger)))) +
  geom_text(aes(x=x,y=y),color="black",label="equal power",angle=90,
            data=data.frame(x=4.5,y=30,Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                            Merger=factor("Vertical", levels=unique(res.nests.logit$Merger))),size=3.5) +
  geom_text(aes(x=x,y=y),color="black",label="more retailer power",
            data=data.frame(x=7,y=-44,Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                            Merger=factor("Vertical", levels=unique(res.nests.logit$Merger))),size=2.5)



pbargincumbent_t.bw <- ggplot(filter(res.nests.logit,Outcome %in% c("Total") &
                                       ((merger !="both" & vert %in% c("0","1","4")) | (merger =="both" & vert %in% c("2","3","6")))) %>%
                                mutate(vert=as.numeric(as.character(vert)),
                                       vert=ifelse(merger=="both",vert-2,vert),
                                       vert=factor(vert)),
                              aes(y=Outcome_value/mktrev.pre*100,
                                  #avgpricedelta/mktrev.pre*100,
                                  x=factor(barg,labels=MASS::fractions(relleverage)),color=vert)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-50,50))+
  scale_y_continuous(breaks=seq(-100, 100, by=10) ) +
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_vline(xintercept=5,linetype="dotted")+
  theme_bw()+#scale_colour_tableau('Color Blind')+
  #scale_color_brewer(palette = "PuRd",direction=-1)+
  scale_color_manual(values = seq_palette)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.nests$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Merger,labeller = "label_context")+
  xlab("Relative Bargaining Power")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="# Integrated Firms:")+
  labs(title =  "How Changing Bargaining Strength Affects Total Surplus, By Merger",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )


pbargincumbent_t.bw <- pbargincumbent_t.bw + geom_segment(
  aes(x=x,xend=xend,y=y,yend=y),color="black",arrow=arrow(length=unit(0.3,"cm"),ends="last",type="closed"),size=1, show.legend = FALSE,
  data=data.frame(x=5.1,y=-12,xend=9
                  ,Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                  Merger=factor("Vertical", levels=unique(res.nests.logit$Merger)))) +
  geom_text(aes(x=x,y=y),color="black",label="equal power",angle=90,
            data=data.frame(x=4.5,y=20,Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                            Merger=factor("Vertical", levels=unique(res.nests.logit$Merger))),size=3.5) +
  geom_text(aes(x=x,y=y),color="black",label="more retailer power",
            data=data.frame(x=7.5,y=-10,Outcome=factor( "Consumer" ,levels=unique(res.nests.logit$Outcome)),
                            Merger=factor("Vertical", levels=unique(res.nests.logit$Merger))),size=2.5)



pbargdiagonal.bw <- ggplot(filter(res.nest_diagonal.allcost ,Outcome %in% c("Consumer","Total")  & preset=="diag0" & vert %in% c("0","1","4")), aes(y=Outcome_value/mktrev.pre*100,
                                                                                                                                      #avgpricedelta/mktrev.pre*100,
                                                                                                                                      x=factor(barg,labels=MASS::fractions(relleverage)),color=vert)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-50,10))+
  scale_y_continuous(breaks=seq(-100, 100, by=10) ) +
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_vline(xintercept=5,linetype="dotted")+
  theme_bw()+#scale_colour_tableau('Color Blind')+
  #scale_color_brewer(palette = "PuRd",direction=-1)+
  scale_color_manual(values = seq_palette)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.nests$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(Outcome~Merger,scales="free",labeller = "label_context")+
  xlab("Relative Bargaining Power")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="# Integrated Firms:")+
  labs(title =  "How Changing non-Party  Bargaining Strength in Diagonal Mergers Affects Consumer and Total Surplus, By Merger",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures.\nRelative bargaining power for parties' integrated product equal to 1."

       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )


pbargdiagonal.bw <- pbargdiagonal.bw + geom_segment(
  aes(x=x,xend=xend,y=y,yend=y),color="black",arrow=arrow(length=unit(0.3,"cm"),ends="last",type="closed"),size=1, show.legend = FALSE,
  data=data.frame(x=5.1,y=-50,xend=9
                  ,Outcome=factor( "Consumer" ,levels=unique(res.nest_diagonal.allcost$Outcome)),
                  Merger=factor("Vertical", levels=unique(res.nest_diagonal.allcost$Merger)))) +
  geom_text(aes(x=x,y=y),color="black",label="equal power",angle=90,
            data=data.frame(x=4.5,y=30,Outcome=factor( "Consumer" ,levels=unique(res.nest_diagonal.allcost$Outcome)),
                            Merger=factor("Vertical", levels=unique(res.nest_diagonal.allcost$Merger))),size=3.5) +
  geom_text(aes(x=x,y=y),color="black",label="more retailer power",
            data=data.frame(x=7.5,y=-40,Outcome=factor( "Consumer" ,levels=unique(res.nest_diagonal.allcost$Outcome)),
                            Merger=factor("Vertical", levels=unique(res.nest_diagonal.allcost$Merger))),size=3.5)


pbargup_all.bw <- ggplot(filter(res.nests.logit,merger =="up" & vert %in% c("0","1","4")), aes(y=Outcome_value/mktrev.pre*100,
                                                         #avgpricedelta/mktrev.pre*100,
                                                         x=factor(barg,labels=MASS::fractions(relleverage)),color=vert)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-45,20))+
  scale_y_continuous(breaks=seq(-45, 20, by=5) ) +
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_vline(xintercept=5,linetype="dotted")+
  theme_bw()+#scale_colour_tableau('Color Blind')+
  #scale_color_brewer(palette = "PuRd",direction=-1)+
  scale_color_manual(values = seq_palette)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.nests$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Outcome,scales="free",labeller = "label_context")+
  xlab("Relative Bargaining Power")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="# Integrated Firms:")+
  labs(title =  "How Changing Bargaining Strength Affects Surplus\n in a Merger Among Wholesalers",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )


pbargup_all.bw <- pbargup_all.bw + geom_segment(
  aes(x=x,xend=xend,y=y,yend=y),color="black",arrow=arrow(length=unit(0.3,"cm"),ends="last",type="closed"),size=1, show.legend = FALSE,
  data=data.frame(x=5.1,y=-40,xend=9
                  ,Outcome=factor( "Wholesaler" ,levels=unique(res.nests.logit$Outcome)))) +
  geom_text(aes(x=x,y=y),color="black",label="equal power",angle=90,
            data=data.frame(x=4.5,y=-10,Outcome=factor( "Wholesaler" ,levels=unique(res.nests.logit$Outcome))),size=3.5) +
  geom_text(aes(x=x,y=y),color="black",label="more\n retailer\n power",
            data=data.frame(x=7.5,y=-30,Outcome=factor( "Wholesaler" ,levels=unique(res.nests.logit$Outcome))),size=3)





pbargdown_all.bw <- ggplot(filter(res.nests.logit,merger =="down" & vert %in% c("0","1","4")), aes(y=Outcome_value/mktrev.pre*100,
                                                                  #avgpricedelta/mktrev.pre*100,
                                                                  x=factor(barg,labels=MASS::fractions(relleverage)),color=vert)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-47.5,22.5))+
  scale_y_continuous(breaks=seq(-60, 30, by=10) ) +
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_vline(xintercept=5,linetype="dotted")+
  theme_bw()+
  #scale_colour_tableau('Color Blind')+
  scale_color_manual(values = seq_palette)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.nests$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.nests$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Outcome,scales="free",labeller = "label_context")+
  xlab("Relative Bargaining Power")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="# Integrated Firms:")+
  labs(title =  "How Changing Bargaining Strength Affects Outcomes\n in a Merger Among Retailers",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )



pbargdown_all.bw <- pbargdown_all.bw + geom_segment(
  aes(x=x,xend=xend,y=y,yend=y),color="black",arrow=arrow(length=unit(0.3,"cm"),ends="last",type="closed"),size=1, show.legend = FALSE,
  data=data.frame(x=5.1,y=-45,xend=9.5
                  ,Outcome=factor( "Wholesaler" ,levels=unique(res.nests.logit$Outcome)))) +
  geom_text(aes(x=x,y=y),color="black",label="equal power",angle=90,
            data=data.frame(x=4.7,y=-20,Outcome=factor( "Wholesaler" ,levels=unique(res.nests.logit$Outcome))),size=3.5) +
  geom_text(aes(x=x,y=y),color="black",label="more\n retailer\n power",
            data=data.frame(x=7,y=-35,Outcome=factor( "Wholesaler" ,levels=unique(res.nests.logit$Outcome))),size=3)







pbargvert_all.bw <- ggplot(filter(res.nests.logit,merger =="vertical" & vert %in% c("0","1","4")), aes(y=Outcome_value/mktrev.pre*100,
                                                                      #avgpricedelta/mktrev.pre*100,
                                                                      x=factor(barg,labels=MASS::fractions(relleverage)),color=vert)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  scale_y_continuous(breaks=seq(-40,35,5))+
  coord_cartesian(ylim=c(-40,35))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_vline(xintercept=5,linetype="dotted")+
  theme_bw()+
  #scale_colour_tableau('Color Blind')+
  scale_color_manual(values = seq_palette)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.nests$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Outcome,scales="free",labeller = "label_context")+
  xlab("Relative Bargaining Power")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="# Integrated Firms:")+
  labs(title =  "How Changing Bargaining Strength Affects Surplus\n in a Vertical Merger",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )


pbargvert_all.bw <- pbargvert_all.bw + geom_segment(
  aes(x=x,xend=xend,y=y,yend=y),color="black",arrow=arrow(length=unit(0.3,"cm"),ends="last",type="closed"),size=1, show.legend = FALSE,
  data=data.frame(x=5.1,y=-25,xend=7.5
                  ,Outcome=factor( "Wholesaler" ,levels=unique(res.nests.logit$Outcome)))) +
  geom_text(aes(x=x,y=y),color="black",label="equal power",angle=90,
            data=data.frame(x=4.7,y=15,Outcome=factor( "Wholesaler" ,levels=unique(res.nests.logit$Outcome))),size=3.5) +
  geom_text(aes(x=x,y=y),color="black",label="more\n retailer\n power",
            data=data.frame(x=6.5,y=-20,Outcome=factor( "Wholesaler" ,levels=unique(res.nests.logit$Outcome))),size=3)





pbargboth_all.bw <- ggplot(filter(res.nests.logit,Merger =="Integrated"  & !Outcome %in% c("Retailer","Wholesaler") & vert != "5" & vert !="3"), aes(y=Outcome_value/mktrev.pre*100,
                                                                             #avgpricedelta/mktrev.pre*100,
                                                                             x=factor(barg,labels=MASS::fractions(relleverage)),color=vert)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  scale_y_continuous(breaks=seq(-40,35,5))+
  coord_cartesian(ylim=c(-40,35))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_vline(xintercept=5,linetype="dotted")+
  theme_bw()+
  #scale_colour_tableau('Color Blind')+
  scale_color_manual(values = seq_palette)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.nests$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Outcome,scales="free",labeller = "label_context")+
  xlab("Relative Bargaining Power")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="# Integrated Firms:")+
  labs(title =  "How Changing Bargaining Strength Affects Surplus\n in an Integrated Merger",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )


pbargboth_all.bw <- pbargboth_all.bw + geom_segment(
  aes(x=x,xend=xend,y=y,yend=y),color="black",arrow=arrow(length=unit(0.3,"cm"),ends="last",type="closed"),size=1, show.legend = FALSE,
  data=data.frame(x=5.1,y=-25,xend=7.5
                  ,Outcome=factor( "Total" ,levels=unique(res.nests.logit$Outcome)))) +
  geom_text(aes(x=x,y=y),color="black",label="equal power",angle=90,
            data=data.frame(x=4.7,y=15,Outcome=factor( "Total" ,levels=unique(res.nests.logit$Outcome))),size=3.5) +
  geom_text(aes(x=x,y=y),color="black",label="more\n retailer\n power",
            data=data.frame(x=6.5,y=-20,Outcome=factor( "Total" ,levels=unique(res.nests.logit$Outcome))),size=3)


pbargboth_diagonal.bw <- ggplot(filter(res.nest_diagonal.allcost,Merger =="Integrated" & vert != "5" & vert !="3"), aes(y=Outcome_value/mktrev.pre*100,
                                                                                                         #avgpricedelta/mktrev.pre*100,
                                                                                                         x=factor(barg,labels=MASS::fractions(relleverage)),color=vert)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  scale_y_continuous(breaks=seq(-40,35,5))+
  coord_cartesian(ylim=c(-40,35))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_vline(xintercept=5,linetype="dotted")+
  theme_bw()+
  #scale_colour_tableau('Color Blind')+
  scale_color_manual(values = seq_palette)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  #scale_x_discrete(labels=rev(levels(res.nests$relleveragePre)))+
  #scale_x_discrete(drop=FALSE,labels=ifelse(levels(res.barg$relleveragePre) %in% as.character(round(relleveragePre,1)),levels(res.barg$relleveragePre),""))+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(Outcome~Retailers+Wholesalers,scales="free_y",labeller = "label_context")+
  facet_grid(~Outcome,scales="free",labeller = "label_context")+
  xlab("Relative Bargaining Power")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="# Integrated Firms:")+
  labs(title =  "How Changing Party Bargaining Strength Affects Surplus\n in an Integrated Merger",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures.\nRelative bargaining power for non-parties equal to 1."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )


pbargboth_diagonal.bw <- pbargboth_diagonal.bw + geom_segment(
  aes(x=x,xend=xend,y=y,yend=y),color="black",arrow=arrow(length=unit(0.3,"cm"),ends="last",type="closed"),size=1, show.legend = FALSE,
  data=data.frame(x=5.1,y=-25,xend=7.5
                  ,Outcome=factor( "Wholesaler" ,levels=unique(res.nest_all.long$Outcome)))) +
  geom_text(aes(x=x,y=y),color="black",label="equal power",angle=90,
            data=data.frame(x=4.7,y=15,Outcome=factor( "Wholesaler" ,levels=unique(res.nest_all.long$Outcome))),size=3.5) +
  geom_text(aes(x=x,y=y),color="black",label="more\n retailer\n power",
            data=data.frame(x=6.5,y=-20,Outcome=factor( "Wholesaler" ,levels=unique(res.nest_all.long$Outcome))),size=3)


## Output results

png("output/surplussum.png",width = 10, height = 7, units = "in", res=300)
print(psummary.bw)
dev.off()

png("output/surplussum_cost.png",width = 10, height = 7, units = "in", res=300)
print(psummary_cost.bw)
dev.off()

png("output/CVvertincumbBW.png",width = 10, height = 7, units = "in", res=300)
print(pvertincumb.bw)
dev.off()

png("output/CVvertincumbBW_consumer.png",width = 10, height = 7, units = "in", res=300)
print(pvertincumb_c.bw)
dev.off()

png("output/CVvertincumbBW_total.png",width = 10, height = 7, units = "in", res=300)
print(pvertincumb_t.bw)
dev.off()

png("output/CVvertincumb_updownBW.png",width = 10, height = 7, units = "in", res=300)
print(pvertincumb_updown.bw)
dev.off()

png("output/CVvertincumb_vertincBW.png",width = 10, height = 7, units = "in", res=300)
print(pvertincumb_vertint.bw)
dev.off()


png("output/down_vertincumbBW.png",width = 7, height = 7, units = "in", res=300)
print(pdown_vertincumb.bw)

dev.off()
png("output/up_vertincumbBW.png",width = 10, height = 7, units = "in", res=300)
print(pup_vertincumb.bw)
dev.off()

png("output/both_vertincumbBW.png",width = 10, height = 7, units = "in", res=300)
print(pboth_vertincumb.bw)
dev.off()

png("output/vert_vertincumbBW.png",width = 10, height = 7, units = "in", res=300)
print(pvert_vertincumb.bw)
dev.off()

png("output/CVfirmsupBW.png",width = 10, height = 7, units = "in", res=300)
print(pfirmsup_all.bw)
dev.off()
png("output/CVfirmsdownBW.png",width = 10, height = 7, units = "in", res=300)
print(pfirmsdown_all.bw)
dev.off()
png("output/CVfirmsvertBW.png",width = 7, height = 7, units = "in", res=300)
print(pfirmsvert_all.bw)
dev.off()

png("output/CVfirmsvert_retailBW.png",width = 10, height = 7, units = "in", res=300)
print(pfirmsvert_retailers.bw)
dev.off()


png("output/CVfirmsvert_wholeBW.png",width = 10, height = 7, units = "in", res=300)
print(pfirmsvert_wholesalers.bw)
dev.off()


png("output/CVbargupBW.png",width = 10, height = 7, units = "in", res=300)
print(pbargup_all.bw)
dev.off()
png("output/CVbargdownBW.png",width = 10, height = 7, units = "in", res=300)
print(pbargdown_all.bw)
dev.off()
png("output/CVbargvertBW.png",width = 10, height = 7, units = "in", res=300)
print(pbargvert_all.bw)
dev.off()
png("output/CVbargbothBW.png",width = 10, height = 7, units = "in", res=300)
print(pbargboth_all.bw)
dev.off()

png("output/CVbargbothdiagonalBW.png",width = 10, height = 7, units = "in", res=300)
print(pbargboth_diagonal.bw)
dev.off()

png("output/CVbargnobothBW.png",width = 10, height = 7, units = "in", res=300)
print(pbargnoboth.bw)
dev.off()

png("output/CVbargincumbent.png",width = 10, height = 7, units = "in", res=300)
print(pbargincumbent.bw)
dev.off()

png("output/CVbargincumbent_consumer.png",width = 10, height = 7, units = "in", res=300)
print(pbargincumbent_c.bw)
dev.off()

png("output/CVbargincumbent_total.png",width = 10, height = 7, units = "in", res=300)
print(pbargincumbent_t.bw)
dev.off()

png("output/CVbargdiagonalBW.png",width = 10, height = 7, units = "in", res=300)
print(pbargdiagonal.bw)
dev.off()

png("output/pprofits.png",width = 7, height = 7, units = "in", res=300)
print(pprofits)
dev.off()
png("output/pprofits_barg.png",width = 10, height = 7, units = "in", res=300)
print(pprofits_barg)
dev.off()
