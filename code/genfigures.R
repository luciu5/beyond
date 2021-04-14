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

res.nests.logit <- res.nests.long <- mutate(ungroup(res.nests.long),
                                            Merger=factor(merger,levels=c("down","up","vertical","both"),labels=c("Downstream","Upstream","Vertical","Integrated")),
                                            Cost=factor(mc,labels=c("Constant"
                                                                    ,"Linear"#,"Quadratic","Linear/Quadratic","Quadratic/Linear","Linear/Constant","Constant/Linear"
                                                                    )
                                                        ),
                                            Cost=reorder(Cost,-Outcome_value/mktrev.pre,median,na.rm=TRUE)) %>% select(-mc)

#res.nests.logit <- filter(as.data.frame(res.nests.logit),nestParm == "0" & Retailers !="1" & Wholesalers !="1") %>%
#  mutate_if(is.factor,droplevels)
#partdata <- filter(partdata,up!="1" & down !="1" )%>%
#  mutate_if(is.factor,droplevels)

boxfun <- function(x,probs=c(.05,.25,.5,.75,.95)){

  r <- quantile(x, probs,na.rm=TRUE)
  names(r) <- c("ymin","lower","middle","upper","ymax")
  return(r)
}



#
# ## box and whisker results for Incumbent
#

pvertincumb.bw <-  ggplot(res.nests.logit %>% filter(vert!= "5" & vert !="6"), aes(y=Outcome_value/mktrev.pre*100,
                                           #avgpricedelta/mktrev.pre*100,
                                           x=vert,color=Cost
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
  facet_grid(Merger~Outcome,scales="free",labeller = label_context)+
  xlab("# Pre-merger Vertically Integrated Firms")+
  ylab("Outcome (%)")+
  #ylab("Avg. Downstream Price Change (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  #geom_text(data=ann_text,label="Wholesale advantage")
  labs(colour="Cost:")+
  labs(title =  "The Distributions  of Merger Outcomes as the Number of Integrated Firms Increases",
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

pbargup_all.bw <- ggplot(filter(res.nests.logit,merger =="up" & vert != "3" & vert !="4"), aes(y=Outcome_value/mktrev.pre*100,
                                                         #avgpricedelta/mktrev.pre*100,
                                                         x=factor(barg,labels=MASS::fractions(relleverage)),color=vert)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-45,20))+
  scale_y_continuous(breaks=seq(-45, 20, by=5) ) +
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_vline(xintercept=5,linetype="dotted")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
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





pbargdown_all.bw <- ggplot(filter(res.nests.logit,merger =="down" & vert != "3" & vert !="4"), aes(y=Outcome_value/mktrev.pre*100,
                                                                  #avgpricedelta/mktrev.pre*100,
                                                                  x=factor(barg,labels=MASS::fractions(relleverage)),color=vert)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-47.5,22.5))+
  scale_y_continuous(breaks=seq(-60, 30, by=10) ) +
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_vline(xintercept=5,linetype="dotted")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
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







pbargvert_all.bw <- ggplot(filter(res.nests.logit,merger =="vertical" & vert != "3" & vert !="4" ), aes(y=Outcome_value/mktrev.pre*100,
                                                                      #avgpricedelta/mktrev.pre*100,
                                                                      x=factor(barg,labels=MASS::fractions(relleverage)),color=vert)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  scale_y_continuous(breaks=seq(-40,35,5))+
  coord_cartesian(ylim=c(-40,35))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_vline(xintercept=5,linetype="dotted")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
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





pbargboth_all.bw <- ggplot(filter(res.nests.logit,Merger =="Integrated" & vert != "3" & vert !="4"), aes(y=Outcome_value/mktrev.pre*100,
                                                                             #avgpricedelta/mktrev.pre*100,
                                                                             x=factor(barg,labels=MASS::fractions(relleverage)),color=vert)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  scale_y_continuous(breaks=seq(-40,35,5))+
  coord_cartesian(ylim=c(-40,35))+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_vline(xintercept=5,linetype="dotted")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
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
                  ,Outcome=factor( "Wholesaler" ,levels=unique(res.nests.logit$Outcome)))) +
  geom_text(aes(x=x,y=y),color="black",label="equal power",angle=90,
            data=data.frame(x=4.7,y=15,Outcome=factor( "Wholesaler" ,levels=unique(res.nests.logit$Outcome))),size=3.5) +
  geom_text(aes(x=x,y=y),color="black",label="more\n retailer\n power",
            data=data.frame(x=6.5,y=-20,Outcome=factor( "Wholesaler" ,levels=unique(res.nests.logit$Outcome))),size=3)


## Output results

png("output/surplussum.png",width = 10, height = 7, units = "in", res=300)
print(psummary.bw)
dev.off()


png("output/CVvertincumbBW.png",width = 10, height = 7, units = "in", res=300)
print(pvertincumb.bw)
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

