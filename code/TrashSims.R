rm(list=ls())

library(antitrust)
library(kableExtra)
options(knitr.kable.NA = '')

library(dplyr)
library(tidyr)
library(ggplot2)


## Load Data:

library(readr)
paired_data_4_22 <- read_csv("~/Projects/bargaining_convex/trash/paired_data_4_22.csv")

disposal_price_republic <-  96 # source: 4/26 correspondence with Peggy

simdata <- filter(paired_data_4_22,collection_paired_share>0) %>%
  mutate(collection_paired_share=collection_paired_share/sum(collection_paired_share),
         disposal_dollar_margin=ifelse(disposal_firm_name=="Republic",
                                       disposal_percent_margin*disposal_price_republic,
                                       disposal_dollar_margin),
         disposal_price=ifelse(disposal_firm_name=="Republic" &
                                 collection_firm_name=="Republic",
                               disposal_price_republic - disposal_dollar_margin,disposal_price),
         barg=ifelse(disposal_firm_name==collection_firm_name,1,0.7)
         ) %>%
  group_by(collection_firm_name) %>%
  mutate(Name=interaction(disposal_firm_name,collection_firm_name,sep=":",drop=TRUE),
         firmShare=sum(collection_paired_share),
         ownerPreUp=disposal_firm_name,
         ownerPostUp=ifelse(ownerPreUp=="Santek","Republic",ownerPreUp),
         ownerPreDown=collection_firm_name,
         ownerPostDown=ifelse(ownerPreDown=="Santek","Republic",ownerPreDown)) %>% ungroup()%>%
  mutate(
         alpha_2nd=log(1-firmShare)/(firmShare*collection_dollar_margin),
         marginDown_2nd=log(1-firmShare)/(firmShare*max(alpha_2nd,na.rm=TRUE)),
         alpha_bert=-1/((1-firmShare)*collection_dollar_margin),
         marginDown_bert=-1/((1-firmShare)*max(alpha_bert,na.rm=TRUE))
  )



## create inputs for merger simulation
ownerPreUpMat=model.matrix(~-1+ownerPreUp,data=simdata)
ownerPreUpMat=tcrossprod(ownerPreUpMat)
ownerPreDownMat=model.matrix(~-1+ownerPreDown,data=simdata)
ownerPreDownMat=tcrossprod(ownerPreDownMat)

sharesNoOut <- with(simdata,collection_paired_share)
div <- tcrossprod(1/(1-sharesNoOut),sharesNoOut)*sharesNoOut
diag(div) <- -sharesNoOut

vertFirms <- intersect(simdata$ownerPreUp,simdata$ownerPreDown)


minD <- function(alpha,is2nd=FALSE,margins=FALSE){

  nprods <- nrow(simdata)

  marginsUp <- simdata$disposal_dollar_margin
  marginsDown <- simdata$collection_dollar_margin
  sharesDown   <- simdata$collection_paired_share

  ownerDownMatVertical <- matrix(0,nrow=nprods,ncol=nprods)

  #alpha <- theta[1]
  #b <- theta[-1]
  #b <- b[as.numeric(id)]


  #b[simdata$ownerPreUp == simdata$ownerPreDown] <- 1
  b <- simdata$barg

  for( v in vertFirms){

    vertrows <- simdata$ownerPreUp != v  & simdata$ownerPreDown == v
    ownerPreUpMat[vertrows, simdata$ownerPreUp == v] <- -(1-b[vertrows])/b[vertrows]
  }


  ownerBargDownVert  <-  ownerPreDownMat  * (1-b)/b

  for( v in vertFirms){

    vertrows <-  simdata$ownerPreUp == v  & simdata$ownerPreDown != v

    ## only change downstream matrix when firms are playing Bertrand
    if(!is2nd){ownerDownMatVertical[simdata$ownerPreDown == v, vertrows] <- 1}



    ownerBargDownVert [vertrows, simdata$ownerPreDown == v] <- -1

  }


  #down@ownerPre <- ownerPreDownMat

  #if(is2nd) mval <- log(sharesDown) - log(idxShare) - alpha*(pricesUp - idxPrice)
  #else{mval <- log(sharesDown) - log(idxShare) - alpha*(pricesDown - idxPrice)}

  #down@slopes <- list(alpha = alpha,
  #                    meanval = mval
  #)



  if(is2nd) marginsCandDown <- simdata$marginDown_2nd * simdata$alpha_2nd[1]/alpha
  else{marginsCandDown <- simdata$marginDown_bert * simdata$alpha_bert[1]/alpha}



    elast <-  -alpha*tcrossprod(sharesDown)
    diag(elast) <- alpha*sharesDown + diag(elast)
    elast.inv <- try(solve(ownerPreDownMat * elast),silent=TRUE)
    if(any(class(elast.inv) == "try-error")){elast.inv <- MASS::ginv(ownerPreDownMat * elast)}

    upMarginPart <-  solve(ownerPreUpMat * div) %*% (ownerBargDownVert * div)
    marginsCandUp <- solve(diag(nprods) + (upMarginPart %*% elast.inv %*%  (ownerDownMatVertical* elast)))
    marginsCandUp <- drop(marginsCandUp %*% upMarginPart %*% marginsCandDown)

    if(!is2nd){
    marginsCandDown <- marginsCandDown - elast.inv %*% ( (ownerDownMatVertical * elast) %*% (marginsCandUp) )

  }

  #depVar <- as.vector((ownerPreUpMat  * div) %*% marginsUp)
  #regressor <- as.vector( ( ownerBargDownVert  * div) %*% marginsCandDown)

  err <- c(#depVar - regressor,
    marginsDown - marginsCandDown)

  if(!margins) return(sum((err)^2,na.rm = TRUE))
  else{return(list(up=marginsCandUp,down=as.vector(marginsCandDown)))}
  }

minAlpha_bert <- optimise(minD,c(-1e6,0))$minimum
minAlpha_2nd <- optimise(minD,c(-1e6,0),is2nd=TRUE)$minimum
predMargins_bert=minD(minAlpha_bert,margins=TRUE)
predMargins_2nd=minD(minAlpha_2nd,is2nd=TRUE,margins=TRUE)

 simdata <-
          mutate(simdata,
          alpha_bert=minAlpha_bert,
          disposal_cost_2nd=disposal_price- predMargins_2nd$up,
          disposal_cost_bert=disposal_price - predMargins_bert$up) %>% group_by(disposal_firm_name) %>%
          mutate(
          disposal_price_2nd=ifelse(is.na(disposal_price) & disposal_firm_name==collection_firm_name,
                                    max(disposal_cost_2nd,na.rm=TRUE),
                                    disposal_price),
          disposal_price_bert=ifelse(is.na(disposal_price) & disposal_firm_name==collection_firm_name,
                                    max(disposal_cost_bert,na.rm=TRUE),
                                    disposal_price)) %>% ungroup() %>%
   mutate(
          priceDown_2nd=predMargins_2nd$down + `collection cost` + disposal_price_2nd,

          priceDown_bert=predMargins_bert$down + `collection cost` + disposal_price_bert
         ) %>% ungroup()




## Run Simulation:


simres_noeff <- with(simdata,
                           logit(
  ownerPre = ownerPreDown,
  ownerPost = ownerPostDown,
  prices = priceDown_bert,
  shares = collection_paired_share,
  margins = disposal_percent_margin,
  mcDelta = rep(0,nrow(simdata)),
  insideSize = sum(disposal_volume),
  labels = as.character(Name)
))


simres_noeff_2nd <- with(simdata,
                     auction2nd.logit(
                       ownerPre = ownerPreDown,
                       ownerPost = ownerPostDown,
                       prices = priceDown_bert,
                       shares = collection_paired_share,
                       margins = disposal_percent_margin,
                       mcDelta = rep(0,nrow(simdata)),
                       insideSize = sum(disposal_volume),
                       labels = as.character(Name)
                     ))



simres_vert <- NULL

marginUp_percent_bert <- with(simdata,marginUp_bert/disposal_price_bert)
marginUp_percent_bert[1] <- NA

marginUp_percent_2nd <- with(simdata,marginUp_2nd/disposal_price_2nd)
marginUp_percent_2nd[1] <- NA

simres_vert <<- with(simdata,
                     vertical.barg(supplyDown = "bertrand",
                              sharesDown = collection_paired_share,
                              pricesDown=priceDown_bert,
                              marginsDown=disposal_percent_margin,
                              ownerPreDown=ownerPreDown,
                              ownerPostDown=ownerPostDown,
                              pricesUp=disposal_price_bert,
                              marginsUp=marginUp_percent_bert,
                              ownerPreUp=ownerPreUp,
                              ownerPostUp=ownerPostUp,
                              labels = as.character(Name),
                              insideSize=sum(disposal_volume)
                              #,constrain="pair"
))

simres_vert@up@bargpowerPre[simres_vert@up@bargpowerPre==0.5] <- 0.3
simres_vert@up@bargpowerPost[simres_vert@up@bargpowerPost==0.5] <- 0.3
simres_vert@up@bargpowerPost["Santek:Republic"] <- 1
simres_vert@down@slopes$alpha <- simdata$alpha_bert[1]
simres_vert@down@slopes$meanval <- with(simdata,log(collection_paired_share) - log(collection_paired_share[1]) - simres_vert@down@slopes$alpha *(priceDown_bert - priceDown_bert[1]))
simres_vert <- ownerToMatrix(simres_vert, preMerger = TRUE)
simres_vert <- ownerToMatrix(simres_vert, preMerger = FALSE)
mcPre <- calcMC(simres_vert, TRUE)
mcPost <- calcMC(simres_vert, FALSE)
simres_vert@down@mcPre <- mcPre$down
simres_vert@down@mcPost <- mcPost$down
simres_vert@up@mcPre <- mcPre$up
simres_vert@up@mcPost <- mcPost$up
simres_vertsPre <- calcPrices(simres_vert, preMerger = TRUE)
simres_vert@down@pricePre <- simres_vertsPre$down
simres_vert@up@pricePre <- simres_vertsPre$up
simres_vertsPost <- calcPrices(simres_vert, preMerger = FALSE)
simres_vert@down@pricePost <- simres_vertsPost$down
simres_vert@up@pricePost <- simres_vertsPost$up


simres_vert_2nd <<- with(simdata,
                     vertical.barg(supplyDown = "2nd",
                                   sharesDown = collection_paired_share,
                                   pricesDown=priceDown_2nd,
                                   marginsDown=disposal_percent_margin,
                                   ownerPreDown=ownerPreDown,
                                   ownerPostDown=ownerPostDown,
                                   pricesUp=disposal_price_2nd,
                                   marginsUp=marginUp_percent_2nd,
                                   ownerPreUp=ownerPreUp,
                                   ownerPostUp=ownerPostUp,
                                   labels = as.character(Name),
                                   insideSize=sum(disposal_volume)
                                   #,constrain="pair"
                     ))

exploreVerts <- function(bargparm= 0.8260953,hospMarkup=0.33){

marginBargUp <- as.vector(
  solve(
  ownerPreUpMat * div
  ) %*%
  ((1 - bargparm)/bargparm * ownerPreDownMat *div) %*%
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
