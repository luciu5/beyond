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

res.nests.logit <- res.nests.long <- mutate(ungroup(res.nests.long),Merger=factor(merger,levels=c("down","up","vertical"),labels=c("Downstream","Upstream","Vertical")))
#res.nests.logit <- filter(as.data.frame(res.nests.logit),nestParm == "0" & Retailers !="1" & Wholesalers !="1") %>%
#  mutate_if(is.factor,droplevels)
#partdata <- filter(partdata,up!="1" & down !="1" )%>%
#  mutate_if(is.factor,droplevels)

boxfun <- function(x,probs=c(.05,.25,.5,.75,.95)){

  r <- quantile(x, probs,na.rm=TRUE)
  names(r) <- c("ymin","lower","middle","upper","ymax")
  return(r)
}

png("verthhicv.png",height=750,width=600)
print(ggplot(
  filter(res.nests,
  #,merger=="vertical"# &
  nestParm == "0"  ) %>%mutate(hhipostcut=cut(hhipost,seq(1000,10000,500),right=FALSE,dig.lab=5)) %>% filter(!is.na(hhipostcut))
    #) #%>% select(type,hhipre,hhipost,hhidelta,cv,relleveragePre,relmarginPre)
  #res.nests
,aes(y=cv/mktrev.pre*100,x=hhipostcut)) + #,color=type
    # ))+ #geom_boxplot() +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-40,10))+
  scale_y_continuous(breaks=seq(-40,10,5))+
  facet_grid(merger~type,scale="free_y") +
  theme_bw() + xlab("Post-merger HHI") + ylab("Consumer Harm (%)") + geom_hline(yintercept=0,linetype="dashed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
  ggtitle("Vertical Merger Consumer Harm, by Post-merger HHI") +
  labs(colour="Retail Game:"
       ,subtitle="Outcomes are reported as a percentage of pre-merger market revenues. Negative values denote harm."))
dev.off()


png("relmargin_cv.png",height=750,width=600)
print(ggplot(
  filter(ungroup(res.nests),
         merger=="vertical"&
         #nestParm == "0" &
           isMarket ) %>% mutate(relMarginPreCut=cut(relmarginPre,quantile(relmarginPre, probs=seq(0,1,length=11), na.rm=TRUE), dig.lab=1, include.lowest = TRUE))%>%
    filter(!is.na(relMarginPreCut)) %>% mutate(relMarginPreCut=factor(relMarginPreCut))
  #) #%>% select(type,hhipre,hhipost,hhidelta,cv,relleveragePre,relmarginPre)
  #res.nests
  ,aes(y=cv/mktrev.pre*100,x=relMarginPreCut)) + #,color=type
    # ))+ #geom_boxplot() +
    stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
    coord_cartesian(ylim=c(-30,10))+
    scale_y_continuous(breaks=seq(-80,40,5))+
    facet_grid(nestParm~type,scale="free_y",labeller = "label_context") +
    theme_bw() + xlab("Pre-merger share-weighted wholesaler/retailer margin") + ylab("Consumer Harm (%)") + geom_hline(yintercept=0,linetype="dashed",color="red") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
    ggtitle("Vertical Merger Consumer Harm, by Post-merger HHI") +
    labs(colour="Retail Game:"
         ,subtitle="Outcomes are reported as a percentage of pre-merger market revenues. Negative values denote harm.")
  )
dev.off()

res.nests <- filter(res.nests,merger=="vertical" & nestParm=="0")
relmargins_poly_lm <- lapply(c("Bertrand","2nd") ,function(x){lm(cv~poly(relmarginPre,3),data=res.nests[res.nests$type==x,])})
hhi_poly_lm <- lapply(c("Bertrand","2nd") ,function(x){lm(cv~I(hhipost)+ I(hhipost^2)+I(hhipost^3),data=res.nests[res.nests$type==x,])})
hhidelta_poly_lm <- lapply(c("Bertrand","2nd") ,function(x){lm(cv~I(hhidelta)+ I(hhidelta^2)+I(hhidelta^3),data=res.nests[res.nests$type==x,])})

png("verthhisdeltacv.png",height=750,width=600)
print(ggplot(
  #filter(res.nests
  #,merger=="vertical"# & nestParm == "0"
  # ) #%>% select(type,hhipre,hhipost,hhidelta,cv,relleveragePre,relmarginPre)
  filter(res.nests,hhidelta<=4200 & nestParm == "0")
  ,aes(y=cv/mktrev.pre*100,x=cut(hhidelta,seq(0,4200,200),right=FALSE,dig.lab=5)#,color=type
  ))+ #geom_boxplot() +
    stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
    #coord_cartesian(ylim=c(-40,10))+
    #scale_y_continuous(breaks=seq(-40,10,5))+
    facet_grid(merger~type,scale="free_y") +
    theme_bw() + xlab("Delta HHI") + ylab("Consumer Harm (%)") + geom_hline(yintercept=0,linetype="dashed") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="bottom")+
    ggtitle("Vertical Merger Consumer Harm, by Changes in  HHI") +
    labs(colour="Retail Game:"
         ,subtitle="Outcomes are reported as a percentage of pre-merger market revenues. Negative values denote harm."))
dev.off()



png("verthhicv_small.png",height=600,width=750)
print(ggplot(
  filter(res.nests, hhipost<=3000 & hhipost>=1800 &  nestParm == "0")
  #,merger=="vertical"# & nestParm == "0"
  # ) #%>% select(type,hhipre,hhipost,hhidelta,cv,relleveragePre,relmarginPre)
  #res.nests
  ,aes(y=cv/mktrev.pre*100,x=cut(hhipost,seq(1000,3000,200),right=FALSE,dig.lab=5)#,color=type
  ))+ #geom_boxplot() +
    stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
    coord_cartesian(ylim=c(-10,5))+
    scale_y_continuous(breaks=seq(-25,5,5))+
    facet_grid(merger~.,scale="free") +
    theme_bw() + xlab("Post-merger HHI") + ylab("Consumer Harm (%)") + geom_hline(yintercept=0,linetype="dashed") +
    ggtitle("Vertical Merger Consumer Harm, by Post-merger HHI") +
    labs(colour="Retail Game:"
         ,subtitle="Outcomes are reported as a percentage of pre-merger market revenues. Negative values denote harm."))
dev.off()


#
# ## box and whisker results for Nests experiment
#

psummary.bw <-  ggplot(res.nests.logit, aes(y=Outcome_value/mktrev.pre*100,
                                           #avgpricedelta/mktrev.pre*100,
                                           x=Merger,color=type
                                           )
                                           ) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-60,55))+
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
  labs(colour="Retail Game:")+
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
                                  x=nestParm,color=type)) +
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
  labs(colour="Retail Game:")+
  labs(title =  "How Changing the Nesting Parameter Affects Merger Outcomes",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )




pfirmsup_all.bw <- ggplot(filter(res.nests.logit,#Wholesalers != "2" &
                                 merger =="up"), aes(y=Outcome_value/mktrev.pre*100,
                                                                  #avgpricedelta/mktrev.pre*100,
                                                                  x=Wholesalers,color=type)) +
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
  labs(colour="Retail Game:")+
  labs(title =  "How Changing the Number of Wholesalers Affects Surplus\n in a Merger Among Wholesalers",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )








pfirmsdown_all.bw <- ggplot(filter(ungroup(res.nests.logit),merger =="down") , aes(y=Outcome_value/mktrev.pre*100,
                                                 #avgpricedelta/mktrev.pre*100,
                                                 x=Retailers,color=type)) +
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
  labs(colour="Retail Game:")+
  labs(title =  "How Changing the Number of Retailers Affects Surplus\n in a Merger Among Retailers",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )



pfirmsvert_all.bw <- ggplot(filter(ungroup(res.nests.logit),merger =="vertical" & Wholesalers != "1")%>%
                              mutate(Retailers=factor(Retailers, labels=paste0("Retailers: ", levels(Retailers)))), aes(y=Outcome_value/mktrev.pre*100,
                                                                     #avgpricedelta/mktrev.pre*100,
                                                                     x=Wholesalers,color=type)) +
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
  labs(colour="Retail Game:")+
  labs(title =  "How Changing the Number of Wholesalers and Retailers Affects Surplus\n in a Vertical Merger",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )



pfirmsvert_wholesalers.bw <- ggplot(filter(ungroup(res.nests.logit),merger =="vertical"  & Wholesalers != "1")%>%
                                      mutate_if(is.factor,droplevels)%>%
                              mutate(Retailers=factor(Retailers, labels=paste0("Retailers: ", levels(Retailers)))), aes(y=Outcome_value/mktrev.pre*100,
                                                                                                                        #avgpricedelta/mktrev.pre*100,
                                                                                                                        x=Wholesalers,color=type)) +
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
  labs(colour="Retail Game:")+
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
                                                                                                                                x=Retailers,color=type)) +
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
  labs(colour="Retail Game:")+
  labs(title =  "How Changing the Number of Retailers Affects Surplus\n in a Vertical Merger",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for downstream mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )

pbargup_all.bw <- ggplot(filter(res.nests.logit,merger =="up"), aes(y=Outcome_value/mktrev.pre*100,
                                                         #avgpricedelta/mktrev.pre*100,
                                                         x=factor(barg,labels=MASS::fractions(relleverage)),color=type)) +
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
  labs(colour="Retail Game:")+
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





pbargdown_all.bw <- ggplot(filter(res.nests.logit,merger =="down"), aes(y=Outcome_value/mktrev.pre*100,
                                                                  #avgpricedelta/mktrev.pre*100,
                                                                  x=factor(barg,labels=MASS::fractions(relleverage)),color=type)) +
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
  labs(colour="Retail Game:")+
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







pbargvert_all.bw <- ggplot(filter(res.nests.logit,merger =="vertical" ), aes(y=Outcome_value/mktrev.pre*100,
                                                                      #avgpricedelta/mktrev.pre*100,
                                                                      x=factor(barg,labels=MASS::fractions(relleverage)),color=type)) +
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
  labs(colour="Retail Game:")+
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


##Create plots for partial  analysis


#filter(partdata,part %in% c("cvdiff","totalDiff")) %>% group_by(Merger,Model,part) %>%
#dplyr::summarise(p25=quantile(diff,.25),p50=quantile(diff,.5),p75=quantile(diff,.75),p05=quantile(diff,.05),p95=quantile(diff,.95))

ppartial.bw <- ggplot(filter(partdata,

                             part %in% c("cvdiff" ,
                                         #"downPSDiff",
                                         "totalDiff"
                                         #, "upPSDiff"
                                         )) %>%
                        mutate(part=factor(part,levels=c("cvdiff" ,#"downPSDiff", "upPSDiff",
                                                         "totalDiff" ),
                                           labels=c("Consumer",#"Retailer","Wholesaler",
                                                    "Total"))
                               ),
  #aes(y=diff/mktrev.pre*100,x=part,color=Model)) +
  aes(y=diff*100,x=part,color=Model)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
coord_cartesian(ylim=c(-100,160)) +
  scale_y_continuous(breaks=seq(-100, 160, by=20) ) +
  #scale_y_continuous(breaks=seq(-5, 30, by=5) ) +
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(
    #axis.text.x = element_text(angle = 0, hjust = 1),
    legend.position="bottom")+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  facet_grid(~Merger,scales="free",labeller = "label_both")+
  xlab("Change in Surplus")+
  ylab("Outcome Difference (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  labs(colour="Retail Game:")+
  labs(title =   "Difference Between Outcomes in the Full Model vs. the Partial Model",
       subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       #subtitle="Differences are reported as a percentage of the full model outcomes."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for vertical mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )



ppartial_up.bw <- ggplot(filter(partdata,
                             Merger=="Upstream" &
                             part %in% c("cvdiff" ,
                                         #"downPSDiff",
                                         "totalDiff"
                                         #, "upPSDiff"
                             )) %>%
                        mutate(part=factor(part,levels=c("cvdiff" ,#"downPSDiff", "upPSDiff",
                                                         "totalDiff" ),
                                           labels=c("Consumer",#"Retailer","Wholesaler",
                                                    "Total"))
                        ),
                      #aes(y=diff/mktrev.pre*100,x=part,color=Model)) +
                      aes(y=diff*100,x=part,color=Model)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-100,160)) +
  scale_y_continuous(breaks=seq(-100, 160, by=20) ) +
  #scale_y_continuous(breaks=seq(-5, 30, by=5) ) +
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(
    #axis.text.x = element_text(angle = 0, hjust = 1),
    legend.position="bottom")+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(~Merger,scales="free",labeller = "label_both")+
  xlab("Change in Surplus")+
  ylab("Outcome Difference (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  labs(colour="Retail Game:")+
  labs(title =   "Difference Between Outcomes in Upstream Mergers:\n Full Model vs. the Partial Model",
       #subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       subtitle="Differences are reported as a percentage of the full model outcomes."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for vertical mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )
ppartial_down.bw <- ggplot(filter(partdata,
                                  Merger=="Downstream" &
                                    part %in% c("cvdiff" ,
                                                #"downPSDiff",
                                                "totalDiff"
                                                #, "upPSDiff"
                                    )) %>%
                             mutate(part=factor(part,levels=c("cvdiff" ,#"downPSDiff", "upPSDiff",
                                                              "totalDiff" ),
                                                labels=c("Consumer",#"Retailer","Wholesaler",
                                                         "Total"))
                             ),
                           #aes(y=diff/mktrev.pre*100,x=part,color=Model)) +
                           aes(y=diff*100,x=part,color=Model)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  coord_cartesian(ylim=c(-100,160)) +
  scale_y_continuous(breaks=seq(-100, 160, by=20) ) +
  #scale_y_continuous(breaks=seq(-5, 30, by=5) ) +
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(
    #axis.text.x = element_text(angle = 0, hjust = 1),
    legend.position="bottom")+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  #facet_grid(~Merger,scales="free",labeller = "label_both")+
  xlab("Change in Surplus")+
  ylab("Outcome Difference (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  labs(colour="Retail Game:")+
  labs(title =   "Difference Between Outcomes in Downstream Mergers:\nFull Model vs. the Partial Model",
       #subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       subtitle="Differences are reported as a percentage of the full model outcomes."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for vertical mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )

ppartial_price.bw <- ggplot(filter(partdata,

                             part %in% c("avgdownpricedelta.part","avguppricedelta.part"
                             )) %>%
                        mutate(part=factor(part ,levels=c("avgdownpricedelta.part","avguppricedelta.part"),
                                           labels=c("Retail Price","Wholesale Price"))
                        ),
                      aes(y=diff*100,x=part,color=Model)) +
  #geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data=boxfun, geom="boxplot",position="dodge")+
  #coord_cartesian(ylim=c(-100,160)) +
  #scale_y_continuous(breaks=seq(-100, 160, by=20) ) +
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  theme_bw()+scale_colour_tableau('Color Blind')+ theme(
    #axis.text.x = element_text(angle = 0, hjust = 1),
    legend.position="bottom")+
  #theme_tufte(ticks=FALSE) +
  #geom_tufteboxplot(median.type = "line", whisker.type = 'line') +
  facet_grid(~Merger,scales="free",labeller = "label_both")+
  xlab("Change in Surplus")+
  ylab("Outcome Difference (%)")+
  #ylab("Share-Weighted Downstream Price Change")+
  labs(colour="Retail Game:")+
  labs(title =   "Difference Between Outcomes in the Full Model vs. the Partial Model",
       #subtitle="Outcomes are reported as a percentage of pre-merger total expenditures."
       subtitle="Differences are reported as a percentage of the full model outcomes."
       #subtitle = "1st and 2nd score auctions yields radically different predictions for vertical mergers,\n but similar predictions for upstream mergers",
       #caption ="outMargin = 25\nshareOutDown = .15\nmcshare.up =.25\nmcshare.down = .1\nnfirms.up = 3"
  )





## res =300 is 300 dots per inch. this should be high resolution!
png("output/ppartial.png",width = 10, height = 7, units = "in", res=300)
print(ppartial.bw)
dev.off()
png("output/ppartial_up.png",width = 6, height = 6, units = "in", res=300)
print(ppartial_up.bw)
dev.off()
png("output/ppartial_down.png",width = 6, height = 6, units = "in", res=300)
print(ppartial_down.bw)
dev.off()
             #
# png("output/ppartial_firm.png",width = 10, height = 7, units = "in", res=300)
# print(ppartial_firm.bw)
# dev.off()

png("output/surplussum.png",width = 10, height = 7, units = "in", res=300)
print(psummary.bw)
dev.off()

png("output/CVnestsBW.png",width = 7, height = 10, units = "in", res=300)
print(pnests.bw)
dev.off()

## Output results

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
#
# pdf("output/CVnestsBW.pdf",width = 10, height = 7)
# print(pnestsup.bw)
# print(pnestsdown.bw)
# print(pnestsvert.bw)
# dev.off()
#

png("output/CVbargupBW.png",width = 10, height = 7, units = "in", res=300)
print(pbargup_all.bw)
dev.off()
png("output/CVbargdownBW.png",width = 10, height = 7, units = "in", res=300)
print(pbargdown_all.bw)
dev.off()
png("output/CVbargvertBW.png",width = 10, height = 7, units = "in", res=300)
print(pbargvert_all.bw)
dev.off()
