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
         # Peggy suggests assuming Santek and WasteConn have the same upstream margin as Republic
         # and use that margin to recover costs
         disposal_price=ifelse(disposal_firm_name=="Republic",
                               disposal_price_republic - disposal_dollar_margin,disposal_price),
         barg=ifelse(disposal_firm_name==collection_firm_name,1,0.5),
         disposal_dollar_margin=ifelse(disposal_firm_name %in% c("Santek","WasteConn"),
                                       max(disposal_percent_margin,na.rm=TRUE)*disposal_price,
                                       disposal_dollar_margin)
         ) %>%
  group_by(disposal_firm_name) %>%
  mutate(
    disposal_dollar_margin=max(disposal_dollar_margin,na.rm=TRUE),
    disposal_price=ifelse(is.na(disposal_price),max(disposal_price,na.rm=TRUE) - disposal_dollar_margin,disposal_price),
    disposal_cost= ifelse(disposal_firm_name==collection_firm_name,disposal_price, disposal_price-disposal_dollar_margin),
    disposal_dollar_margin=ifelse(disposal_firm_name==collection_firm_name,0,disposal_dollar_margin)) %>%
  group_by(collection_firm_name) %>%
  mutate(

         Name=interaction(disposal_firm_name,collection_firm_name,sep=":",drop=TRUE),
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

for (v in vertFirms){
  ## set integrated margin disagreement payoff to 0
  ownerPreUpMat[simdata$ownerPreUp==v & simdata$ownerPreDown!=v
            , simdata$ownerPreUp==v & simdata$ownerPreDown==v]=0
  ## constrain upstream integrated margin to zero
  ownerPreUpMat[simdata$ownerPreUp==v & simdata$ownerPreDown==v
            , simdata$ownerPreUp==v & simdata$ownerPreDown!=v]=0

}

sharesNoOut <- with(simdata,collection_paired_share)
div <- tcrossprod(1/(1-sharesNoOut),sharesNoOut)*sharesNoOut
diag(div) <- -sharesNoOut

vertFirms <- intersect(simdata$ownerPreUp,simdata$ownerPreDown)


minD <- function(barg,is2nd=FALSE,margins=FALSE){

  nprods <- nrow(simdata)

  marginsUp <- simdata$disposal_dollar_margin
  marginsDown <- simdata$collection_dollar_margin
  sharesDown   <- simdata$collection_paired_share

  ownerDownMatVertical <- matrix(0,nrow=nprods,ncol=nprods)

if(!is2nd)  alpha <- simdata$alpha_bert[1]
else{ alpha <- simdata$alpha_2nd[1]}

  b <- rep(1,nprods)
  #b <- theta[-1]
  #b <- b[as.numeric(id)]


  #b[simdata$ownerPreUp != simdata$ownerPreDown] <- barg
  b[simdata$ownerPreUp=="Santek" & simdata$ownerPreDown!="Santek"] <- barg[1]
  b[simdata$ownerPreUp=="WasteConn" & simdata$ownerPreDown!="WasteConn"] <- barg[2]

  #b[simdata$ownerPreUp!="Republic" & simdata$ownerPreDown=="Republic"] <- barg[1]
  #b[simdata$ownerPreUp!="WM_ADS" & simdata$ownerPreDown=="WM_ADS"] <- barg[2]
  #b[simdata$ownerPreUp!="Regional" & simdata$ownerPreDown=="Regional"] <- barg[3]


  #b <- simdata$barg

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

  depVar <- as.vector((ownerPreUpMat  * div) %*% marginsUp)
  regressor <- as.vector( ( ownerBargDownVert  * div) %*% marginsCandDown)

  err <- c(depVar - regressor,
    marginsDown - marginsCandDown)

  if(!margins) return(sum((err)^2,na.rm = TRUE))
  else{return(list(up=marginsCandUp,down=as.vector(marginsCandDown)))}
  }


## same bargaininag parameter for all non-integrated options
#minBarg_bert <- optimise(minD,c(0,1))
#minBarg_2nd <- optimise(minD,c(0,1),is2nd=TRUE)

## same bargaininag parameter for retailers
#minBarg_bert <- optim(rep(.5,3),minD,method="L-BFGS-B",lower=rep(0,3),upper=rep(1,3))
#minBarg_2nd <- optim(rep(.5,3),minD,method="L-BFGS-B",lower=rep(0,3),upper=rep(1,3),is2nd=TRUE)

## same bargaininag parameter for wholesalers
minBarg_bert<- optim(rep(.5,2),minD,method="L-BFGS-B",lower=rep(0,2),upper=rep(1,2))
minBarg_2nd <- optim(rep(.5,2),minD,method="L-BFGS-B",lower=rep(0,2),upper=rep(1,2),is2nd=TRUE)


predMargins_bert=minD(minBarg_bert$par,margins=TRUE)
predMargins_2nd=minD(minBarg_2nd$par,is2nd=TRUE,margins=TRUE)

 simdata <-
          mutate(simdata,
          marginUp_2nd=predMargins_2nd$up,
          marginUp_bert=predMargins_bert$up,
          marginDown_2nd=predMargins_2nd$down,
          marginDown_bert=predMargins_bert$down,
          disposal_cost_2nd=disposal_price- marginUp_2nd,
          disposal_cost_bert=disposal_price - marginUp_bert) %>% group_by(disposal_firm_name) %>%
          mutate(
          disposal_price_2nd=ifelse(is.na(disposal_price) & disposal_firm_name==collection_firm_name,
                                    max(disposal_cost_2nd,na.rm=TRUE),
                                    disposal_price),
          disposal_price_bert=ifelse(is.na(disposal_price) & disposal_firm_name==collection_firm_name,
                                    max(disposal_cost_bert,na.rm=TRUE),
                                    disposal_price)) %>% ungroup() %>%
   mutate(
          priceDown_2nd=predMargins_2nd$down + `collection cost` + disposal_price_2nd,

          priceDown_bert=predMargins_bert$down + `collection cost` + disposal_price_bert,
          priceDown_repmargin_2nd=predMargins_2nd$down + `collection cost` + disposal_price,

          priceDown_repmargin_bert=predMargins_bert$down + `collection cost` + disposal_price,
         ) %>% ungroup()




## Run Simulation:

 marginsDown_bert_prop <- with(simdata,marginDown_bert/priceDown_repmargin_bert)
 #marginsDown_bert_prop[-1] <- NA

simres_noeff <- with(simdata,
                           logit(
  ownerPre = ownerPreDown,
  ownerPost = ownerPostDown,
  prices = priceDown_repmargin_bert,
  shares = collection_paired_share,
  margins = marginsDown_bert_prop,
  mcDelta = rep(0,nrow(simdata)),
  insideSize = sum(disposal_volume),
  labels = as.character(Name)
))


simres_noeff_2nd <- with(simdata,
                     auction2nd.logit(
                       ownerPre = ownerPreDown,
                       ownerPost = ownerPostDown,
                       prices = priceDown_repmargin_2nd,
                       shares = collection_paired_share,
                       margins = marginDown_2nd,
                       mcDelta = rep(0,nrow(simdata)),
                       insideSize = sum(disposal_volume),
                       labels = as.character(Name)
                     ))



simres_vert <- NULL

marginUp_percent_bert <- with(simdata,marginUp_bert/disposal_price_bert)
#marginUp_percent_bert[1] <- NA

marginUp_percent_2nd <- with(simdata,marginUp_2nd/disposal_price_2nd)
#marginUp_percent_2nd[1] <- NA

simres_vert <<- with(simdata,
                     vertical.barg(supplyDown = "bertrand",
                              sharesDown = collection_paired_share,
                              pricesDown=priceDown_repmargin_bert,
                              marginsDown=marginsDown_bert_prop,
                              ownerPreDown=ownerPreDown,
                              ownerPostDown=ownerPostDown,
                              pricesUp=disposal_price,
                              marginsUp=disposal_dollar_margin/disposal_price +.0001,
                              ownerPreUp=ownerPreUp,
                              ownerPostUp=ownerPostUp,
                              labels = as.character(Name),
                              insideSize=sum(disposal_volume)
                              ,constrain="wholesaler"
))


## bargaining parameters are not being recovered properly. Figure out why
simres_vert@up@bargpowerPre[simdata$disposal_firm_name=="Santek" & simdata$collection_firm_name!="Santek"] <- minBarg_bert$par[1]
simres_vert@up@bargpowerPost[simdata$disposal_firm_name=="Santek" & simdata$collection_firm_name!="Santek"] <- minBarg_bert$par[1]
simres_vert@up@bargpowerPre[simdata$disposal_firm_name=="WasteConn" & simdata$collection_firm_name!="WasteConn"] <- minBarg_bert$par[2]
simres_vert@up@bargpowerPost[simdata$disposal_firm_name=="WasteConn" & simdata$collection_firm_name!="WasteConn"] <- minBarg_bert$par[2]
simres_vert@up@bargpowerPost["Santek:Republic"] <- 1

simres_vert@down@slopes$alpha <- simdata$alpha_bert[1]
simres_vert@down@slopes$meanval <- log(simres_vert@down@shares) - log(simres_vert@down@shares)[1]  - simres_vert@down@slopes$alpha *(simres_vert@down@prices - simres_vert@down@prices[1])
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



## Output data

sink("./doc/TrashData.tex")
print(kable(select(simdata,disposal_firm_name,
                   collection_firm_name,
                   disposal_volume,
                   disposal_cost,
                   disposal_dollar_margin,
                   collection_dollar_margin,
                   priceDown_repmargin_2nd
) %>%

              rename("Disposal Firm"=disposal_firm_name,
                     "Collection Firm"=collection_firm_name,
                     "Volume (000s)"=disposal_volume,
                     "Disposal Price"=disposal_cost,
                     "Disposal Margin ($)"=disposal_dollar_margin,
                     "Collection Margin ($)"=collection_dollar_margin,
                     "Collection Price"=priceDown_repmargin_2nd
                     ) %>%
  mutate(`Volume (000s)`=`Volume (000s)`/1e3) %>%
  mutate(across(where(is.numeric),round)),
format = "latex",
            booktabs = TRUE,
            caption = "Republic/Santek Merger Simulation Inputs"#,label = "siminputs"
            ) %>% kable_styling(latex_options = "scale_down")
      ,digits=2)

sink()
#
# mktplot_82_33 <- ggplot(data= mkt_82_33
#                          ,
#                          aes(x=key,y=value,fill=Model)) +
#   #facet_wrap(~key,scales = "free_x") +
#   geom_bar(stat="identity", position=position_dodge()) + theme_bw() +
#   theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.text.y = element_text(angle = 45, hjust = 1)) +
#   xlab("") + ylab("Equilibrium Level Changes (Millions $)") + geom_hline(yintercept = 0,linetype="dashed")+ coord_flip() +
#   scale_fill_grey(start = .9, end = .1) +  geom_text(aes(label=round(value),hjust = ifelse(value >= 0, 0, 1)), position=position_dodge(width=0.9),color="black",size=2)
#
#
# data_82_33 <-
#   tidyr::gather(res_82_33$firm,key="key",value="value",-level,-model,-firm) %>%
#   mutate(firm=factor(firm,levels=rev(levels(firm))),
#          model=factor(model,labels = c("None","Medical","Vertical"))) %>%
#   filter(key %in% c("outputDelta","priceDelta")) %>%
#   mutate(key=factor(key,levels=c("priceDelta","outputDelta"),
#                     labels=c("Price","Output")),
#          level=factor(level,levels=c("up","down"),labels=c("Up","Down"))) %>%
#   rename(Efficiency=model)
#
# data_82_33 <- rbind(data_82_33,
#                     data.frame(level="Up",Efficiency="Medical",firm="Anthem",key="Price",value=simdata$`Post-merger Cost Changes($/unit)`[1]),
#                     data.frame(level="Up",Efficiency="Medical",firm="Cigna",key="Price",value=simdata$`Post-merger Cost Changes($/unit)`[2])
# )

vert_sum <- summary(simres_vert)
vert_sum$name=rownames(vert_sum)

vert_sum <- separate(vert_sum,name,sep=":",into=c("Disposal","Collector")) %>%
  pivot_longer(c(priceUpPre ,priceUpPost , priceDownPre, priceDownPost, sharesPre, sharesPost)) %>%
  mutate(Level=ifelse(grepl("Up",name),"Disposal","Collection"),
         Pre=ifelse(grepl("Pre",name),"Pre-merger","Post-merger"),
         name=gsub("Pre|Post|Up|Down","",name)) %>%
  rename(Effect=name) %>%
  pivot_wider(values_from=value,names_from=Pre) %>%
  mutate(`Change (%)`=(1-`Pre-merger`/`Post-merger`)*100,
         Effect=factor(Effect, labels=c("Prices","Shares")),
          Level=factor(Level,levels=rev(sort(unique(Level))))) %>%
  select(-priceUpDelta,-priceDownDelta,-outputDelta,-isParty) %>%
  arrange(Level,Effect,Disposal,desc(`Change (%)`)) %>%
  mutate(across(where(is.numeric),round)) %>% relocate(Level,Effect)


sink("./doc/TrashSims.tex")
kable(vert_sum,format = "latex",
      booktabs = TRUE,
      caption = "Republic/Santex Simulation Effects") #%>%
  #collapse_rows(1:2,row_group_label_position="stack")
sink()

# firmplot <- ggplot(data=  vert_sum
# ,
#        aes(x=firm,y=value, fill=level)) +
#        facet_grid(key~Efficiency,scales = "free_y") + geom_bar(stat="identity", position=position_dodge())  +
#   xlab("Insurer") + ylab("Equilibrium Level Changes") + geom_hline(yintercept = 0,linetype="dashed") + coord_flip()+ scale_fill_grey(start = .9, end = .1) +
#   geom_text(aes(label=
#                   ifelse(abs(value)>0,round(value,1),NA),
#                    hjust=ifelse(sign(value)>0,1,0),color=ifelse(level=="Down","white","black")),
#                   position=position_dodge(width=.9),size =3) + theme_bw() +
#   theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))

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
