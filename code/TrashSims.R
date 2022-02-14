rm(list=ls())

library(antitrust)
library(kableExtra)
options(knitr.kable.NA = '')

library(dplyr)
library(tidyr)
library(ggplot2)


## Load Data:

combined_data <- read.csv("./doc/trash/combined_data.csv",sep=",")
paired_share <- read.csv("./doc/trash/paired_share.csv",sep=",")

# simdata <- data.frame(
#   #`Name`= c('ANT:ASO','CIG:ASO','AET:ASO','UT:ASO','OT:ASO'),
#   `Pre-merger Owner`= c('Anthem','Cigna','Aetna','United','Other'),
#   `Post-merger Owner`= c('Anthem','Anthem','Aetna','United','Other'),
#   #`Prices ($/unit)`= c(24.4,NA,NA,NA,NA),
#   `Quantities`= c(22.35,6.3,8.6,17.19,2.87),
#   `Margins ($/unit)`= c(19.5,NA,NA,NA,NA),
#   `Post-merger Cost Changes($/unit)`= c(-15.7,-4.4,0,0,0),
#   nests=factor(c(rep(FALSE,2),rep(TRUE,3))),
#   check.names = FALSE,
#   stringsAsFactors = FALSE
# ) 


simdata <- data.frame(
  #`Name`= c('ANT:ASO','CIG:ASO','AET:ASO','UT:ASO','OT:ASO'),
  `Pre-merger Owner`= c('Anthem','Cigna','Aetna','United','Other'),
  `Post-merger Owner`= c('Anthem','Anthem','Aetna','United','Other'),
  `Prices ($/unit)`= c(4356,NA,NA,NA,NA),
  `Quantities`= c(10.53,2.97,4.05,8.10,1.35),
  `Margins ($/unit)`= c(239.58,NA,NA,NA,NA),
  `Post-merger Cost Changes($/unit)`= c(-84.90,-505.05,0,0,0),
  nests=factor(c(rep(FALSE,2),rep(TRUE,3))),
  check.names = FALSE,
  stringsAsFactors = FALSE
) 
largeFirmElast <- -.09

## Run Simulation: 
simres_noeff <- auction2nd.logit.alm(
  ownerPre = simdata$`Pre-merger Owner`,
  ownerPost = simdata$`Post-merger Owner`,
  prices = simdata$`Prices ($/unit)`,
  shares = simdata$`Quantities` / sum( simdata$`Quantities` ) ,
  margins = simdata$`Margins ($/unit)`,
  mcDelta = rep(0,nrow(simdata)),
  mktElast = largeFirmElast,
 insideSize = sum(simdata$`Quantities`),
  labels = simdata$`Pre-merger Owner` #normIndex = 5
) 


simres_noeff_known <- auction2nd.logit(
  ownerPre = simdata$`Pre-merger Owner`,
  ownerPost = simdata$`Post-merger Owner`,
  prices = simdata$`Prices ($/unit)`,
  shares = simdata$`Quantities` / sum( simdata$`Quantities` )*.94,#.977 ,
  margins = simdata$`Margins ($/unit)`,
  mcDelta = rep(0,nrow(simdata)),
  insideSize = sum(simdata$`Quantities`),
  labels = simdata$`Pre-merger Owner` #normIndex = 5
) 


simres_noeff_noout <- auction2nd.logit(
  ownerPre = simdata$`Pre-merger Owner`,
  ownerPost = simdata$`Post-merger Owner`,
  prices = simdata$`Prices ($/unit)`,
  shares = simdata$`Quantities` / sum( simdata$`Quantities` ) ,
  margins = simdata$`Margins ($/unit)`,
  mcDelta = rep(0,nrow(simdata)),
  insideSize = sum(simdata$`Quantities`),
  labels = simdata$`Pre-merger Owner` #normIndex = 5
) 

simres_medical <- auction2nd.logit.alm(
  ownerPre = simdata$`Pre-merger Owner`,
  ownerPost = simdata$`Post-merger Owner`,
  prices = simdata$`Prices ($/unit)`,
  shares = simdata$`Quantities` / sum( simdata$`Quantities` ) ,
  margins = simdata$`Margins ($/unit)` ,
  mcDelta =  simdata$`Post-merger Cost Changes($/unit)`,
  mktElast = largeFirmElast,
   insideSize = sum(simdata$`Quantities`),
  labels = simdata$`Pre-merger Owner`#, normIndex = 5
  ) 

## Run nested versions, using shares outside share
# simres_noeff_nests <- auction2nd.logit.nests(
#   ownerPre = simdata$`Pre-merger Owner`,
#   ownerPost = simdata$`Post-merger Owner`,
#   nests = simdata$nests,
#   prices = simdata$`Prices ($/unit)`,
#   shares = (simdata$`Quantities` / sum( simdata$`Quantities` ))*simres_noeff@shareInside ,
#   margins = simdata$`Margins ($/unit)`,
#   mcDelta = rep(0,nrow(simdata)),
#   #mktElast = largeFirmElast,
#   insideSize = sum(simdata$`Quantities`),
#   labels = simdata$`Pre-merger Owner` #normIndex = 5
# ) 

# simres_medical_nests <- auction2nd.logit.nests(
#   ownerPre = simdata$`Pre-merger Owner`,
#   ownerPost = simdata$`Post-merger Owner`,
#   prices = simdata$`Prices ($/unit)`,
#   shares = (simdata$`Quantities` / sum( simdata$`Quantities` ))*simres_noeff@shareInside ,
# 
#   margins = simdata$`Margins ($/unit)` ,
#   nests = simdata$nests,
#   mcDelta =  simdata$`Post-merger Cost Changes($/unit)`,
#   #mktElast = largeFirmElast,
#   insideSize = sum(simdata$`Quantities`),
#   labels = simdata$`Pre-merger Owner`#, normIndex = 5
# ) 


## create inputs for merger simulation
ownerPreUp=rep("MonHosp",nrow(simdata))

sharesFull <- simdata$`Quantities`/ sum( simdata$`Quantities` ) * 
  simres_noeff@shareInside
  #.94
  #0.91 
div <- tcrossprod(1/(1-sharesFull),sharesFull)*sharesFull
diag(div) <- -sharesFull
div <- as.vector(div)


simres_vert <- NULL

exploreVerts <- function(bargparm= 0.8260953,hospMarkup=0.33){
  
marginBargUp <- as.vector(
  solve(
  matrix(1, ncol=nrow(simdata), nrow=nrow(simdata)) * div
  ) %*% 
  ((1 - bargparm)/bargparm * diag(nrow(simdata))*div) %*%
  calcMargins(simres_noeff)
)
 # return((marginBargUp[1] - 555.5820896)^2)
  #}
  

priceBargUp <- marginBargUp/hospMarkup

## Run supply chain simulation
simres_vert <<- vertical.barg(supplyDown = "2nd", 
  sharesDown = sharesFull,
  pricesDown=simdata$`Prices ($/unit)`,
  marginsDown=simdata$`Margins ($/unit)`/simdata$`Prices ($/unit)`,
  ownerPreDown=simdata$`Pre-merger Owner`,
  ownerPostDown=simdata$`Post-merger Owner`,
  pricesUp=priceBargUp,
  marginsUp=marginBargUp,
  ownerPreUp=ownerPreUp,
  ownerPostUp=ownerPreUp,
  labels = simdata$`Pre-merger Owner`,
  insideSize=simres_noeff@insideSize
)
#simres_vert@up@bargpowerPre <- simres_vert@up@bargpowerPost <- rep(bargparm,length(sharesFull)) 
#   
# simres_vert <- auction2nd.logit.alm(
#   ownerPre = simdata$`Pre-merger Owner`,
#   ownerPost = simdata$`Post-merger Owner`,
#   prices = simdata$`Prices ($/unit)`,
#   shares = simdata$`Quantities` / sum( simdata$`Quantities` ) ,
#   margins = simdata$`Margins ($/unit)`,
#   mcDelta = simdata$`Post-merger Cost Changes($/unit)`,
#   mktElast = largeFirmElast,
#   insideSize = sum(simdata$`Quantities`),
#   labels = simdata$Name #normIndex = 5
# ) 



## Summarize results



## Summary Tab Results:
simsum <- list(
  noeff=summary(simres_noeff, revenue = FALSE, levels = TRUE, market=FALSE)[,-1] ,
  medical=summary(simres_medical, revenue = FALSE, levels = TRUE, market=FALSE)[,-1] ,
vert=summary(simres_vert, revenue = FALSE, levels = TRUE, market=FALSE)[,-1] 
)

simsum_mkt <- list(
  noeff=summary(simres_noeff, market=TRUE,revenue = FALSE, levels = TRUE)[c(1,2,5,6,7)],
  medical=summary(simres_medical, market=TRUE,revenue = FALSE, levels = TRUE)[c(1,2,5,6,7)] ,
  vert=summary(simres_vert, market=TRUE,revenue = FALSE, levels = TRUE)
)

names(simsum_mkt$vert) <- gsub("Down\\s*","",names(simsum_mkt$vert),perl=TRUE)
names(simsum_mkt$vert) <- gsub("^Price Change","Industry Price Change",names(simsum_mkt$vert),perl=TRUE)

simsum_mkt <- bind_rows(simsum_mkt,.id="Model")
colnames(simsum_mkt) <- gsub("Industry","Down",colnames(simsum_mkt),perl=TRUE)
colnames(simsum_mkt) <- gsub("^Producer Benefit","Down Producer Benefit",colnames(simsum_mkt),perl=TRUE)

#simsum_mkt_long <- pivot_longer(simsum_mkt,-Model,names_to="Outcome",
   #          values_to="Value") %>% arrange(Outcome,Model)
simsum_mkt_long <- gather(simsum_mkt,-Model,key="Outcome",
                                         value="Value") %>% arrange(Outcome,Model)

#ggplot(simsum_mkt_long,aes(y=Model,x=Value)) + 
#  facet_grid(~Outcome,scales="free_x") + geom_dotplot() 

simsum_array <- array(dim=c(2, ncol(simsum$medical),3,nrow(simsum$medical)),
      dimnames=list(level=c("up","down"),
                           outcome=colnames(simsum$medical),
                           model=names(simsum),
                           firm=rownames(simsum$medical)
                          )
)


simsum_array["down",names(simsum$medical)!="mcDelta","noeff",] <- t(simsum$noeff)

simsum_array["down",,"medical",] <- t(simsum$medical)


simsum_array["up",names(simsum$medical)!="mcDelta","vert",] <- 
  t(simsum$vert[
                    ,grepl("Up|shares|output|isParty",names(simsum$vert))])

simsum_array["down",names(simsum$medical)!="mcDelta","vert",] <- 
  t(simsum$vert[
    ,grepl("Down|shares|output|isParty",names(simsum$vert))])

simsum <- as.data.frame.table(simsum_array)

#simsum <- pivot_wider(simsum,names_from = outcome, values_from = Freq)
simsum <- spread(simsum,key= outcome, value= Freq)

simsum <- simsum[(simsum$level != "up" | !is.na(simsum$pricePre)) 
                 #&
                  #simsum$firm %in% c("Anthem","Cigna") 
                 ,] %>%  
  arrange(level,model,firm)


return(list(mkt=simsum_mkt,firm=simsum))
}


## Output data

print(kable(select(simdata,-nests, - `Post-merger Owner`) %>% 
              mutate(Quantities=round(Quantities/sum(Quantities)*100)) %>%
              rename("Owner"=`Pre-merger Owner`),format = "latex",
            booktabs = TRUE,  
            caption = "Anthem/Cigna Merger Simulation Inputs"#,label = "siminputs"
            )
      ,digits=2)

## output simulation results

res_82_33 <- exploreVerts()

mkt_82_33 <- res_82_33$mkt %>% select(Model,#7,#3,
                                      4,5,8,6)
mkt_82_33 <- gather(mkt_82_33,key="key",value="value",-Model) %>%
  mutate( Model=factor(Model,
                       levels=c("noeff","medical","vert"),
                       labels = c("None","Medical","Vertical"))
          ,
          key=gsub("\\(.*","",key,perl=TRUE),
          key=factor(key,levels=rev(unique(key)))
          ) %>% 
  filter(key != "Difference ")
                                                                   

#labelcol=rep("black",12)
#labelcol[c(2)] <- "white"

mktplot_82_33 <- ggplot(data= mkt_82_33
                         ,
                         aes(x=key,y=value,fill=Model)) +
  #facet_wrap(~key,scales = "free_x") + 
  geom_bar(stat="identity", position=position_dodge()) + theme_bw() +
  theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1)) + 
  xlab("") + ylab("Equilibrium Level Changes (Millions $)") + geom_hline(yintercept = 0,linetype="dashed")+ coord_flip() +
  scale_fill_grey(start = .9, end = .1) +  geom_text(aes(label=round(value),hjust = ifelse(value >= 0, 0, 1)), position=position_dodge(width=0.9),color="black",size=2)


data_82_33 <- 
  tidyr::gather(res_82_33$firm,key="key",value="value",-level,-model,-firm) %>%
  mutate(firm=factor(firm,levels=rev(levels(firm))),
         model=factor(model,labels = c("None","Medical","Vertical"))) %>%
  filter(key %in% c("outputDelta","priceDelta")) %>%
  mutate(key=factor(key,levels=c("priceDelta","outputDelta"),
                    labels=c("Price","Output")),
         level=factor(level,levels=c("up","down"),labels=c("Up","Down"))) %>%
  rename(Efficiency=model)

data_82_33 <- rbind(data_82_33,
                    data.frame(level="Up",Efficiency="Medical",firm="Anthem",key="Price",value=simdata$`Post-merger Cost Changes($/unit)`[1]),
                    data.frame(level="Up",Efficiency="Medical",firm="Cigna",key="Price",value=simdata$`Post-merger Cost Changes($/unit)`[2])
)

firmplot_82_33 <- ggplot(data= data_82_33
,
       aes(x=firm,y=value, fill=level)) +
       facet_grid(key~Efficiency,scales = "free_y") + geom_bar(stat="identity", position=position_dodge())  + 
  xlab("Insurer") + ylab("Equilibrium Level Changes") + geom_hline(yintercept = 0,linetype="dashed") + coord_flip()+ scale_fill_grey(start = .9, end = .1) +
  geom_text(aes(label=
                  ifelse(abs(value)>0,round(value,1),NA),
                   hjust=ifelse(sign(value)>0,1,0),color=ifelse(level=="Down","white","black")),
                  position=position_dodge(width=.9),size =3) + theme_bw() +
  theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))

# 
# png(filename="./output/AnthemCignaSimsFirm.png" ,res = 250, units="in"
#     ,width = 4, height = 4 )
# 
# print(firmplot_82_33)
# 
# dev.off()
# 

#png(filename="./output/AnthemCignaSimsMkt.png" ,res = 200
#    ,width = 1000, height = 800
#)
#print(mktplot_82_33)
#dev.off()
ggsave(plot=firmplot_82_33,file="./output/AnthemCignaSimsFirm.png" ,dpi=300,units="in",width=5,height=4)
ggsave(plot=mktplot_82_33,file="./output/AnthemCignaSimsMkt.png" ,dpi=300,units="in",width=6,height=4)
# 
# print(
#   kable(mutate_if(res_82_33$firm,is.numeric,round,digits=2) ,format = "latex",
#             booktabs = TRUE,
#             caption = "Anthem/Cigna Merger Simulation Results",label = "siminputs") %>%
#     column_spec(1, bold=T) %>%
#     collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle")
#       ,digits=2)


print(kable(tidyr::spread(data_82_33,key=key,value=value)%>% mutate(Efficiency=factor(Efficiency,c("None","Medical","Vertical")),
                                                                    level=factor(level,c("Up","Down"),labels=c("Upstream","Downstream")),
                                                                    firm=factor(firm,c("Anthem","Cigna","United","Aetna","Other")))%>% arrange(Efficiency,level,firm),digits=2,format = "latex",
                   booktabs = TRUE,
                   caption = "Anthem/Cigna Merger Simulation Results",label = "siminputs") %>%
      column_spec(1) %>%
             collapse_rows(columns = 1:2, latex_hline = "major")
               ,digits=2)
