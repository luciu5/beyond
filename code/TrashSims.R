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
  #group_by(disposal_firm_name) %>%
  #mutate(disposal_volume=ifelse(disposal_firm_name != collection_firm_name,mean(disposal_volume[disposal_firm_name != collection_firm_name]),disposal_volume)) %>%
  ungroup() %>%
  mutate(#collection_paired_share=collection_paired_share/sum(collection_paired_share),
         collection_firm_name=gsub("_","-",collection_firm_name),
         collection_paired_share=disposal_volume/sum(disposal_volume),
         disposal_dollar_margin=ifelse(disposal_firm_name=="Republic",
                                       disposal_percent_margin*disposal_price_republic,
                                       disposal_dollar_margin),
         # Peggy suggests assuming Santek and WasteConn have the same upstream margin as Republic
         # and use that margin to recover costs
         disposal_price=ifelse(disposal_firm_name=="Republic",
                               disposal_price_republic - disposal_dollar_margin,disposal_price),
         barg=ifelse(disposal_firm_name==collection_firm_name,1,0.6),
         disposal_dollar_margin=ifelse(disposal_firm_name %in% c("Santek","WasteConn"),
                                       max(disposal_percent_margin,na.rm=TRUE)*disposal_price,
                                       disposal_dollar_margin)
         ) %>%
  group_by(disposal_firm_name) %>%
  mutate(
    disposal_dollar_margin=max(disposal_dollar_margin,na.rm=TRUE),
    disposal_price=ifelse(is.na(disposal_price),max(disposal_price,na.rm=TRUE) - disposal_dollar_margin,disposal_price),
    disposal_cost= ifelse(disposal_firm_name==collection_firm_name,disposal_price, disposal_price-disposal_dollar_margin),
    disposal_dollar_margin=ifelse(disposal_firm_name==collection_firm_name,0,disposal_dollar_margin)#,
    #disposal_dollar_margin=ifelse(min(collection_paired_share) == collection_paired_share | disposal_dollar_margin==0,disposal_dollar_margin,NA)
    ) %>%
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

#disposal HHI
hhiPreUp <- HHI(owner=ownerPreUpMat,shares=simdata$collection_paired_share)
#collection HHI
hhiPreDown <- HHI(owner=ownerPreDownMat,shares=simdata$collection_paired_share)
#WM-ADS/Regional HHI Delta
hhiDeltawmads_regional <- tapply(simdata$collection_paired_share*100,simdata$collection_firm_name,sum,na.rm=TRUE)
hhiDeltawmads_regional <- 2*prod(hhiDeltawmads_regional[c("Regional","WM-ADS")])
#WM-ADS/Santek HHI Delta
hhiDeltawmads_santek <- tapply(simdata$collection_paired_share*100,simdata$collection_firm_name,sum,na.rm=TRUE)
hhiDeltawmads_santek <- 2*prod(hhiDeltawmads_santek[c("Santek","WM-ADS")])
#WasteConn/Santek HHI Delta
hhiDeltaUpwasteconn_santek <- tapply(simdata$collection_paired_share*100,simdata$disposal_firm_name,sum,na.rm=TRUE)
hhiDeltaUpwasteconn_santek  <- 2*prod(hhiDeltaUpwasteconn_santek [c("Santek","WasteConn")])
hhiDeltaDownwasteconn_santek <- tapply(simdata$collection_paired_share*100,simdata$collection_firm_name,sum,na.rm=TRUE)
hhiDeltaDownwasteconn_santek  <- 2*prod(hhiDeltaDownwasteconn_santek [c("Santek","WasteConn")])


vertFirms <- intersect(simdata$ownerPreUp,simdata$ownerPreDown)

for (v in vertFirms){
  ## set integrated margin disagreement payoff to 0
  ownerPreUpMat[simdata$ownerPreUp==v & simdata$ownerPreDown!=v
            , simdata$ownerPreUp==v & simdata$ownerPreDown==v]=0
  ## constrain upstream integrated margin to zero
  ownerPreUpMat[simdata$ownerPreUp==v & simdata$ownerPreDown==v
            , simdata$ownerPreUp==v & simdata$ownerPreDown!=v]=0

}






minD <- function(theta,is2nd=FALSE,margins=FALSE,nests,nestParm=0,relevant){

  alpha <- theta[1]
  theta <- theta[-1]
  barg <- theta
  nprods <- nrow(simdata)


  pricesDown <- simdata$disposal_price
  marginsUp <- simdata$disposal_dollar_margin
  marginsDown <- simdata$collection_dollar_margin
  sharesDown   <- simdata$collection_paired_share

  ownerDownMatVertical <- matrix(0,nrow=nprods,ncol=nprods)

#if(!is2nd)  alpha <- simdata$alpha_bert[1]
#else{ alpha <- simdata$alpha_2nd[1]}

  b <- rep(1,nprods)
  #b <- theta[-1]
  #b <- b[as.numeric(id)]


  #b[simdata$ownerPreUp != simdata$ownerPreDown] <- barg
  if(missing(relevant)){
  b[simdata$ownerPreUp=="Santek" & simdata$ownerPreDown!="Santek"] <- barg[1]
  b[simdata$ownerPreUp=="WasteConn" & simdata$ownerPreDown!="WasteConn"] <- barg[2]
  }
  else{ b[relevant] <- barg}
  #b[simdata$ownerPreUp!="Republic" & simdata$ownerPreDown=="Republic"] <- barg[1]
  #b[simdata$ownerPreUp!="WM_ADS" & simdata$ownerPreDown=="WM_ADS"] <- barg[2]
  #b[simdata$ownerPreUp!="Regional" & simdata$ownerPreDown=="Regional"] <- barg[3]


  #b <- simdata$barg

  for( v in vertFirms){

    vertrows <- simdata$ownerPreUp != v  & simdata$ownerPreDown == v
    ownerPreUpMat[vertrows, simdata$ownerPreUp == v] <- -(1-b[vertrows])/b[vertrows]
  }

  ## set integrated margin disagreement payoff to 0,
  ## constrain upstream integrated margin to zero

  for(n in which(simdata$ownerPreUp==simdata$ownerPreDown)){
    ownerPreUpMat[n,-n] <- ownerPreUpMat[-n,n] <- 0
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


  if(!missing(nests)){
    nestMat <- tcrossprod(model.matrix(~-1+nests))
  sharesBetween <- as.vector(tapply(sharesDown, nests, sum))
  sharesBetween <- sharesBetween[nests]
  sharesWithin <- sharesDown/sharesBetween


  elastDiff.down <- -alpha*tcrossprod(sharesDown)
  elastSame.down <- -alpha*tcrossprod(sharesDown*(1 + nestParm[nests] /( 1-nestParm[nests] )*(1/sharesBetween)),sharesDown)

  elast <- elastSame.down*nestMat + elastDiff.down*(1-nestMat)
  diag(elast) <- alpha/(1-nestParm[nests])*sharesDown*(1-nestParm[nests]*sharesWithin - (1-nestParm[nests])*sharesDown)
  elast.inv <- solve(ownerPreDownMat * elast)
  marginsCandDown = - as.vector(elast.inv %*% sharesDown)  #compute downstream margins


  divSameNest <- (1-sharesWithin)^nestParm[nests]*(1 - sharesBetween + sharesBetween * (1- sharesWithin)^(1-nestParm[nests]) )
  divSameNest <-  tcrossprod((1/divSameNest - 1), sharesDown)
  ##    matrix(shares * (1/divSameNest - 1), nrow = nprods,ncol=nprods,byrow=TRUE)
  divDiffNest <- sharesBetween * (1 - (1- sharesWithin)^(1-nestParm[nests]) )
  divDiffNest <-  1/divDiffNest - 1
  divDiffNest <-  tcrossprod(1/divDiffNest, sharesDown)
  ##   matrix(shares/divDiffNest, nrow=nprods, ncol=nprods, byrow=TRUE)

  div <- divSameNest * nestMat + divDiffNest*(1-nestMat)
  div[is.na(div)] <- 0
  diag(div) <- -sharesDown





  }


  else{


    div <- tcrossprod(1/(1-sharesDown),sharesDown)*sharesDown
    diag(div) <- -sharesDown


    elast <-  -alpha*tcrossprod(sharesDown)
    diag(elast) <- alpha*sharesDown + diag(elast)
    elast.inv <- try(solve(ownerPreDownMat * elast),silent=TRUE)
    if(any(class(elast.inv) == "try-error")){elast.inv <- MASS::ginv(ownerPreDownMat * elast)}
  }


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
minBarg_bert<- optim(c(-1,rep(.6,2)),minD,method="L-BFGS-B",lower=c(-1e3,rep(.1,2)),upper=c(-1e-4,rep(1,2)))
minBarg_2nd <- optim( c(-1,rep(.6,2)),minD,method="L-BFGS-B",lower=c(-1e3,rep(.1,2)),upper=c(-1e-4,rep(1,2)),is2nd=TRUE)


predMargins_bert=minD(minBarg_bert$par,margins=TRUE)
predMargins_6_bert=minD(c(minBarg_bert$par[1],.6,.6),margins=TRUE)
predMargins_8_bert=minD(c(minBarg_bert$par[1],.8,.8),margins=TRUE)
predMargins_9_bert=minD(c(minBarg_bert$par[1],.9,.9),margins=TRUE)
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
#                           logit(
sim(
  supply="bertrand",
  demand="Logit",
 ownerPre = ownerPreDown,
  ownerPost = ownerPostDown,
 demand.param=list(alpha=alpha_bert[1],
                   meanval=log(collection_paired_share)-log(collection_paired_share)[1] - alpha_bert[1]*(priceDown_repmargin_bert- priceDown_repmargin_bert[1])),
  prices = priceDown_repmargin_bert,
  #shares = collection_paired_share,
  #margins = marginsDown_bert_prop,
  mcDelta = rep(0,nrow(simdata)),
  insideSize = sum(disposal_volume),
  labels = as.character(Name)
))

simres_noeff_up <- with(simdata,
  #                   logit(
  sim(
    supply="bertrand",
    demand="Logit",
    demand.param=list(alpha=alpha_bert[1],
                      meanval=log(collection_paired_share)-log(collection_paired_share)[1] - alpha_bert[1]*(priceDown_repmargin_bert- priceDown_repmargin_bert[1])),
                       ownerPre = ownerPreUp,
                       ownerPost = ownerPostUp,
                       prices = priceDown_repmargin_bert,
                   #    shares = collection_paired_share,
                   #     margins = marginsDown_bert_prop,
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



marginUp_percent_bert <- with(simdata,marginUp_bert/disposal_price_bert)
#marginUp_percent_bert[1] <- NA

marginUp_percent_2nd <- with(simdata,marginUp_2nd/disposal_price_2nd)
#marginUp_percent_2nd[1] <- NA

simres_vert <- with(simdata,
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
                              ,constrain="wholesaler",
                              chain_level="full"
))




## bargaining parameters are not being recovered properly. Figure out why

#minBarg_bert$par[2:3] <- 0.9
# simres_vert@up@bargpowerPre[simdata$disposal_firm_name=="Santek" & simdata$collection_firm_name!="Santek"] <- minBarg_bert$par[2]
# simres_vert@up@bargpowerPost[simdata$disposal_firm_name=="Santek" & simdata$collection_firm_name!="Santek"] <- minBarg_bert$par[2]
# simres_vert@up@bargpowerPre[simdata$disposal_firm_name=="WasteConn" & simdata$collection_firm_name!="WasteConn"] <- minBarg_bert$par[3]
# simres_vert@up@bargpowerPost[simdata$disposal_firm_name=="WasteConn" & simdata$collection_firm_name!="WasteConn"] <- minBarg_bert$par[3]
# simres_vert@up@bargpowerPost["Santek:Republic"] <- 1
#
# simres_vert@down@slopes$alpha <- simdata$alpha_bert[1]
# simres_vert@down@slopes$meanval <- log(simres_vert@down@shares) - log(simres_vert@down@shares)[1]  - simres_vert@down@slopes$alpha *(simres_vert@down@prices - simres_vert@down@prices[1])
# simres_vert <- ownerToMatrix(simres_vert, preMerger = TRUE)
# simres_vert <- ownerToMatrix(simres_vert, preMerger = FALSE)
# mcPre <- calcMC(simres_vert, TRUE)
# mcPost <- calcMC(simres_vert, FALSE)
#
# ## Set Santek:Santek marginal costs equal to the calibrated costs of other Santek Products
# mcPre$up["Santek:Santek"] <- mcPre$up["Santek:Regional"]
# mcPost$up["Santek:Santek"] <- mcPost$up["Santek:Regional"]
#
# simres_vert@down@mcPre <- mcPre$down
# simres_vert@down@mcPost <- mcPost$down
# simres_vert@up@mcPre <- mcPre$up
# simres_vert@up@mcPost <- mcPost$up

simres_vert_nodown <- simres_vert_noup <- simres_vert



simres_vert_nodown@chain_level="wholesaler"
simres_vert_noup@chain_level="retailer"

simres_vertsPre <- calcPrices(simres_vert, preMerger = TRUE)
simres_vert@down@pricePre <- simres_vertsPre$down
simres_vert@up@pricePre <- simres_vertsPre$up
simres_vertsPost <- calcPrices(simres_vert, preMerger = FALSE)
simres_vert@down@pricePost <- simres_vertsPost$down
simres_vert@up@pricePost <- simres_vertsPost$up

simres_vertsPre <- calcPrices(simres_vert_noup, preMerger = TRUE)
simres_vert_noup@down@pricePre <- simres_vertsPre$down
simres_vert_noup@up@pricePre <- simres_vertsPre$up
simres_vertsPost <- calcPrices(simres_vert_noup, preMerger = FALSE)
simres_vert_noup@down@pricePost <- simres_vertsPost$down
simres_vert_noup@up@pricePost <- simres_vertsPost$up

simres_vertsPre <- calcPrices(simres_vert_nodown, preMerger = TRUE)
simres_vert_nodown@down@pricePre <- simres_vertsPre$down
simres_vert_nodown@up@pricePre <- simres_vertsPre$up
simres_vertsPost <- calcPrices(simres_vert_nodown, preMerger = FALSE)
simres_vert_nodown@down@pricePost <- simres_vertsPost$down
simres_vert_nodown@up@pricePost <- simres_vertsPost$up







cat("Market results Full model:\n")
summary(simres_vert,market=TRUE)
cat("Market results Downstream Only model:\n")
summary(simres_vert_noup,market=TRUE)
cat("Market results Upstream Only model:\n")
summary(simres_vert_nodown,market=TRUE)
#
# simres_vert_2nd <- with(simdata,
#                      vertical.barg(supplyDown = "2nd",
#                                    sharesDown = collection_paired_share,
#                                    pricesDown=priceDown_2nd,
#                                    marginsDown=disposal_percent_margin,
#                                    ownerPreDown=ownerPreDown,
#                                    ownerPostDown=ownerPostDown,
#                                    pricesUp=disposal_price_2nd,
#                                    marginsUp=marginUp_percent_2nd,
#                                    ownerPreUp=ownerPreUp,
#                                    ownerPostUp=ownerPostUp,
#                                    labels = as.character(Name),
#                                    insideSize=sum(disposal_volume)
#                                    #,constrain="pair"
#                      ))
#


## Output data

sink("./doc/TrashData.tex")
print(kable(select(simdata,disposal_firm_name,
                   collection_firm_name,
                   disposal_volume,
                   disposal_price,
                   disposal_dollar_margin,
                   collection_dollar_margin,
                   priceDown_repmargin_2nd
) %>%

              rename("Disposal Firm"=disposal_firm_name,
                     "Collection Firm"=collection_firm_name,
                     "Volume"=disposal_volume,
                     "Disposal Price"=disposal_price,
                     "Disposal Margin"=disposal_dollar_margin,
                     "Collection Margin"=collection_dollar_margin,
                     "Collection Price"=priceDown_repmargin_2nd
                     ) %>%
  mutate(`Volume`=`Volume`/1e3) %>%
  mutate(across(where(is.numeric),round)),
            format = "latex",
            align = c("r","r",rep("c",5)),
            booktabs = TRUE,
            caption = "Republic/Santek Merger Simulation Inputs. Volume is reported in thousands of pounds, while prices and margins are reported in dollars.",label = "trashdata"
            ) %>% kable_styling(latex_options = "scale_down") %>% collapse_rows(1, latex_hline = "major")
      ,digits=2)

sink()


vert_sum <- summary(simres_vert)
vertnoup_sum <- summary(simres_vert_noup)
vertnodown_sum <- summary(simres_vert_nodown)

base_sum <- summary(simres_noeff)
up_sum <- summary(simres_noeff_up)

base_sum$name=rownames(base_sum)
vert_sum$name=rownames(vert_sum)
vertnoup_sum$name=rownames(vertnoup_sum)
vertnodown_sum$name=rownames(vertnodown_sum)
up_sum$name=rownames(up_sum)


vert_sum <- separate(vert_sum,name,sep=":",into=c("Disposal","Collector")) %>%
  pivot_longer(c(priceUpPre ,priceUpPost , priceDownPre, priceDownPost, sharesPre, sharesPost)) %>%
  mutate(Level=ifelse(grepl("Up",name),"Disposal","Collection"),
         Pre=ifelse(grepl("Pre",name),"Pre-merger","Post-merger"),
         name=gsub("Pre|Post|Up|Down","",name)) %>%
  rename(Effect=name) %>%
  pivot_wider(values_from=value,names_from=Pre) %>%
  mutate(`Change (%)`=(`Post-merger`/`Pre-merger` - 1)*100,
         Effect=factor(Effect, labels=c("Prices","Shares")),
          Level=factor(Level,levels=rev(sort(unique(Level))))) %>%
  select(-priceUpDelta,-priceDownDelta,-outputDelta,-isParty) %>%
  arrange(Level,Effect,Disposal,desc(`Change (%)`)) %>%
  mutate(across(where(is.numeric),round)) %>% relocate(Level,Effect)

vertnoup_sum <- separate(vertnoup_sum,name,sep=":",into=c("Disposal","Collector")) %>%
  pivot_longer(c(priceUpPre ,priceUpPost , priceDownPre, priceDownPost, sharesPre, sharesPost)) %>%
  mutate(Level=ifelse(grepl("Up",name),"Disposal","Collection"),
         Pre=ifelse(grepl("Pre",name),"Pre-merger","Post-merger"),
         name=gsub("Pre|Post|Up|Down","",name)) %>%
  rename(Effect=name) %>%
  pivot_wider(values_from=value,names_from=Pre) %>%
  mutate(`Change (%)`=(`Post-merger`/`Pre-merger` - 1)*100,
         Effect=factor(Effect, labels=c("Prices","Shares")),
         Level=factor(Level,levels=rev(sort(unique(Level))))) %>%
  select(-priceUpDelta,-priceDownDelta,-outputDelta,-isParty) %>%
  arrange(Level,Effect,Disposal,desc(`Change (%)`)) %>%
  mutate(across(where(is.numeric),round)) %>% relocate(Level,Effect)

vertnodown_sum <- separate(vertnodown_sum,name,sep=":",into=c("Disposal","Collector")) %>%
  pivot_longer(c(priceUpPre ,priceUpPost , priceDownPre, priceDownPost, sharesPre, sharesPost)) %>%
  mutate(Level=ifelse(grepl("Up",name),"Disposal","Collection"),
         Pre=ifelse(grepl("Pre",name),"Pre-merger","Post-merger"),
         name=gsub("Pre|Post|Up|Down","",name)) %>%
  rename(Effect=name) %>%
  pivot_wider(values_from=value,names_from=Pre) %>%
  mutate(`Change (%)`=(`Post-merger`/`Pre-merger` - 1)*100,
         Effect=factor(Effect, labels=c("Prices","Shares")),
         Level=factor(Level,levels=rev(sort(unique(Level))))) %>%
  select(-priceUpDelta,-priceDownDelta,-outputDelta,-isParty) %>%
  arrange(Level,Effect,Disposal,desc(`Change (%)`)) %>%
  mutate(across(where(is.numeric),round)) %>% relocate(Level,Effect)

base_sum <- separate(base_sum,name,sep=":",into=c("Disposal","Collector")) %>%
  pivot_longer(c( pricePre, pricePost, sharesPre, sharesPost)) %>%
  mutate(Level=ifelse(grepl("Up",name),"Disposal","Collection"),
         Pre=ifelse(grepl("Pre",name),"Pre-merger","Post-merger"),
         name=gsub("Pre|Post|Up|Down","",name)) %>%
  rename(Effect=name) %>%
  pivot_wider(values_from=value,names_from=Pre) %>%
  mutate(`Change (%)`=(`Post-merger`/`Pre-merger` - 1)*100,
         Effect=factor(Effect, labels=c("Prices","Shares")),
         Level=factor(Level,levels=rev(sort(unique(Level))))) %>%
  select(-priceDelta,-outputDelta,-isParty) %>%
  arrange(Level,Effect,Disposal,desc(`Change (%)`)) %>%
  mutate(across(where(is.numeric),round)) %>% relocate(Level,Effect)



up_sum <- separate(up_sum,name,sep=":",into=c("Disposal","Collector")) %>%
  pivot_longer(c( pricePre, pricePost, sharesPre, sharesPost)) %>%
  mutate(Level=ifelse(grepl("Up",name),"Disposal","Collection"),
         Pre=ifelse(grepl("Pre",name),"Pre-merger","Post-merger"),
         name=gsub("Pre|Post|Up|Down","",name)) %>%
  rename(Effect=name) %>%
  pivot_wider(values_from=value,names_from=Pre) %>%
  mutate(`Change (%)`=(`Post-merger`/`Pre-merger` - 1)*100,
         Effect=factor(Effect, labels=c("Prices","Shares")),
         Level=factor(Level,levels=rev(sort(unique(Level))))) %>%
  select(-priceDelta,-outputDelta,-isParty) %>%
  arrange(Level,Effect,Disposal,desc(`Change (%)`)) %>%
  mutate(across(where(is.numeric),round)) %>% relocate(Level,Effect)


compare <- bind_rows(
  mutate(vert_sum, Model="Full"),
  mutate(vertnoup_sum, Model="Downstream-only"),
  mutate(vertnodown_sum, Model="Upstream-only")
)

sink("./doc/TrashSims.tex")
kable(vert_sum,format = "latex",
      booktabs = TRUE,
      caption = "Republic/Santex Simulation Effects") #%>%
  #collapse_rows(1:2,row_group_label_position="stack")
sink()

vert_sum <- mutate(vert_sum,Name=interaction(Disposal,Collector,drop=TRUE,sep="/"),
                    Name=reorder(Name,`Post-merger`* as.numeric(Effect=="Prices") * as.numeric(Level=="Collection")))

firmplot <- ggplot(data=  vert_sum  %>%
         mutate(Change=`Post-merger` - `Pre-merger`)%>%
         pivot_longer(c(`Pre-merger` ,`Post-merger`),names_to = "Type",values_to = "value") %>%
         mutate(Type=factor(Type,levels=c("Pre-merger","Post-merger")),
                Name=factor(as.character(Name))) %>%
         filter(!grepl("Change",Type)) %>% arrange(Name),
aes(x=Name,y=value,fill=Type,label=value)) +
  facet_grid(~Level+Effect,scales = "free_x") + geom_bar(stat="identity",
                                                             position=position_dodge2(reverse=TRUE)
                                                             #position="stack"
                                                         )  +
  xlab("Disposal/Collector") + ylab("Equilibrium Levels") + #geom_hline(yintercept = 0,linetype="dashed") +
  scale_fill_brewer(type="qual",palette = "Paired") +#scale_fill_grey(start = .9, end = .1) +
  geom_text( #color=ifelse(Type=="Post-merger","white","black"),
                hjust=1.5, position=position_dodge2(width=.9,reverse=TRUE),size =3) +  coord_flip()+ theme_bw() +
  theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(limits=rev)


compare <- compare %>% mutate(Name=interaction(Disposal,Collector,drop=TRUE,sep="/"),
           Name=reorder(Name,`Post-merger`*as.numeric(Model=="Full")*as.numeric(Effect=="Prices") * as.numeric(Level=="Collection"))) %>%
  mutate(Change=`Post-merger` - `Pre-merger`) %>%
  pivot_longer(c(`Pre-merger` ,`Post-merger`),names_to = "Type",values_to = "value") %>%
  mutate(Type=factor(Type,levels=c("Pre-merger","Post-merger")),
         Model=factor(Model,levels=c("Full","Downstream-only","Upstream-only"))) %>%
  filter(!grepl("Change",Type))

compareplot <- ggplot(data=  filter(compare,Effect!="Shares" &
                                      !(Type=="Pre-merger" & Model!="Full") &
                                      !(Level=="Disposal" & Model=="Downstream-only") &
                                      !(Level=="Collection" & Model =="Upstream-only")) %>%
                        mutate(Model=ifelse(Type=="Pre-merger","Pre-merger",as.character(Model)),
                               Model=factor(Model,levels=c("Pre-merger","Full","Upstream-only","Downstream-only")),
                               Name=factor(as.character(Name))),
                   aes(x=Name,y=value,fill=Model,label=value)) +
  facet_grid(~Level,scales = "free_x") + geom_bar(stat="identity",
                                                         position=position_dodge2(reverse = TRUE)
                                                         #position="stack"
  )  +
  xlab("Disposal/Collector") + ylab("Equilibrium Prices") + #geom_hline(yintercept = 0,linetype="dashed") +
  scale_fill_brewer(type="seq",palette = "YlGnBu",direction=-1) +#scale_fill_grey(start = .9, end = .1) +
  geom_text( #color=ifelse(Type=="Post-merger","white","black"),
    hjust=1.5, position=position_dodge2(width=.9,reverse=TRUE),size =3) +  coord_flip()+ theme_bw() +
  theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill="")+
  scale_x_discrete(limits=rev)

compareplot_noup <- ggplot(data=  filter(compare,Type!="Pre-merger" & !(Level=="Disposal" & Effect=="Prices") &
                                      Model!="Upstream-only"),
                      aes(x=Name,y=value,fill=Model,label=value)) +
  facet_grid(~Level+Effect,scales = "free_x") + geom_bar(stat="identity",
                                                         position=position_dodge()
                                                         #position="stack"
  )  +
  xlab("Disposal/Collector") + ylab("Equilibrium Post-merger Levels") + #geom_hline(yintercept = 0,linetype="dashed") +
  scale_fill_brewer(type="qual",palette = "Paired") +#scale_fill_grey(start = .9, end = .1) +
  geom_text( #color=ifelse(Type=="Post-merger","white","black"),
    hjust=1.5, position=position_dodge(width=.9),size =3) +  coord_flip()+ theme_bw() +
  theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))


 png(filename="./output/TrashSimsFirm.png" ,res = 250, units="in"
     ,width = 6, height = 6 )

 print(firmplot)

 dev.off()


 png(filename="./output/TrashSimsCompare.png" ,res = 250, units="in"
     ,width = 6, height = 6 )

 print(compareplot_noup)

 dev.off()

 png(filename="./output/TrashSimsCompareAll.png" ,res = 250, units="in"
     ,width = 6, height = 6 )

 print(compareplot)

 dev.off()





 mergersweep <- function(thissim,acquirer=c("Republic","Santek","WasteConn","WM-ADS"),
                         target=c("Republic","Santek","WasteConn","WM-ADS","Regional"),
                         unintegrated=c("Base","Vertical","Up","Down"),
                         partial=FALSE){


   aquirer <- match.arg(acquirer)
   target <- match.arg(target)
   unintegrated <- match.arg(unintegrated)

   if(acquirer==target){return(NULL)}
   # if((acquirer=="Republic" && target=="Santek") ||
   #     target=="Republic" && acquirer=="Santek"){return(thissim)}


   if(unintegrated !="Base"){

   smallestBarg <- min(thissim@up@bargpowerPre)
   #thissim@up@bargpowerPre[thissim@up@ownerPre==thissim@down@ownerPre] <- smallestBarg
   thissim@up@bargpowerPre <- rep(smallestBarg,length(thissim@up@bargpowerPre))
   thissim@up@bargpowerPost<-   thissim@up@bargpowerPre
   }

   thissim@up@ownerPost <- thissim@up@ownerPre
   thissim@down@ownerPost <- thissim@down@ownerPre


   if(unintegrated=="Up"){
     isAcquired <- thissim@up@ownerPost==target
     thissim@up@ownerPost[isAcquired] <- acquirer
     thissim@down@ownerPre <- paste0(thissim@down@ownerPre,"Ind")
     thissim@down@ownerPost <- paste0(thissim@down@ownerPost,"Ind")
     if(partial){
       isIntegratedPost <- isIntegratedPre <- thissim@down@ownerPre == paste0(acquirer,"Ind") & thissim@up@ownerPre == acquirer
       thissim@down@ownerPre[isIntegratedPre] <- acquirer
       thissim@down@ownerPost[isIntegratedPost] <- acquirer
       thissim@up@bargpowerPre[isIntegratedPre ] <- 1
       thissim@up@bargpowerPost[isIntegratedPost] <- 1
     }
   }
   else if(unintegrated=="Down"){
     isAcquired <- thissim@down@ownerPost==target
     thissim@down@ownerPost[isAcquired] <- acquirer
     thissim@up@ownerPre <- paste0(thissim@up@ownerPre,"Ind")
     thissim@up@ownerPost <- paste0(thissim@up@ownerPost,"Ind")
     if(partial){
       isIntegratedPost <-isIntegratedPre <- thissim@up@ownerPre == paste0(acquirer,"Ind") & thissim@down@ownerPre == acquirer
       thissim@up@ownerPre[isIntegratedPre] <- acquirer
       thissim@up@ownerPost[isIntegratedPost] <- acquirer
       thissim@up@bargpowerPre[isIntegratedPre] <- 1
       thissim@up@bargpowerPost[isIntegratedPost] <- 1
     }
   }
   else if(unintegrated=="Vertical"){
     isAcquired <-  thissim@up@ownerPost==acquirer & thissim@down@ownerPost==target
     thissim@down@ownerPost[isAcquired] <- acquirer
     thissim@down@ownerPre <- paste0(thissim@down@ownerPre,"Ind")
     thissim@down@ownerPost[!isAcquired] <- paste0(thissim@down@ownerPost[!isAcquired],"Ind")
     thissim@up@bargpowerPost[isAcquired] <- 1

   }
   else if(unintegrated=="Base"){
     thissim@up@ownerPost[thissim@up@ownerPost==target] <- acquirer
     thissim@down@ownerPost[thissim@down@ownerPost==target] <- acquirer
     thissim@up@bargpowerPre[thissim@up@ownerPre==thissim@down@ownerPre] <- 1
     thissim@up@bargpowerPost[thissim@up@ownerPost==thissim@down@ownerPost] <- 1
   }

   preVert <- thissim@up@ownerPre == thissim@down@ownerPre
   postVert <- thissim@up@ownerPost == thissim@down@ownerPost

   isVerticalMerger <- any(!preVert & postVert)
   isHorizontal <- !isVerticalMerger

   isUpHorz <- ifelse(!isTRUE(all.equal(thissim@up@ownerPre,thissim@up@ownerPost,check.attributes=FALSE)),TRUE,FALSE)
   isUpstream <- isHorizontal & isUpHorz


   thissim@isHorizontal=isHorizontal
   thissim@isUpstream=isUpstream


   thissim <- ownerToMatrix(thissim, preMerger = TRUE)
   thissim <- ownerToMatrix(thissim, preMerger = FALSE)

   # if(unintegrated!="Base"){
   #   mcPre <- calcMC(thissim, TRUE)
   #   mcPost <- calcMC(thissim, FALSE)
   #
   #   thissim@down@mcPre <- mcPre$down
   #   thissim@down@mcPost <- mcPost$down
   #   thissim@up@mcPre <- mcPre$up
   #   thissim@up@mcPost <- mcPost$up
   # }

   thissimsPre <- calcPrices(thissim, preMerger = TRUE)
   thissim@down@pricePre <- thissimsPre$down
   thissim@up@pricePre <- thissimsPre$up
   thissimsPost <- calcPrices(thissim, preMerger = FALSE)
   thissim@down@pricePost <- thissimsPost$down
   thissim@up@pricePost <- thissimsPost$up



   return(thissim)

 }


 mergercases_base <- data.frame(acquirer=c(rep("Republic",4),rep("Santek",3),rep("WasteConn",2),"WM-ADS"),
                           target=c("Santek","WasteConn","WM-ADS","Regional","WasteConn","WM-ADS","Regional","WM-ADS","Regional","Regional")
 )

 mergercases <-  expand_grid(acquirer=c("Republic","Santek","WasteConn","WM-ADS"),
                             target=c("Republic","Santek","WasteConn","WM-ADS","Regional","WasteConn")) %>%
   filter(acquirer!=target & !(acquirer=="WM-ADS" & target!="Regional"))

 mergercases <- expand_grid(mergercases,
                            #unintegrated="Base"
                            unintegrated=c("Base","Vertical","Up","Down"),
                            partial=c(FALSE,TRUE)
                            #,stringsAsFactors = FALSE
 ) %>%
   distinct() %>%
   filter(!(unintegrated %in% c("Base","Vertical") & partial) &
            !((target%in% c("Regional","WM-ADS") |acquirer=="WM-ADS") & (!unintegrated %in% c("Base","Down","Vertical") | partial)) &
            !(acquirer=="Republic" & unintegrated=="Vertical") &
            !(((acquirer=="Santek" & target=="WasteConn")|(target=="Santek" & acquirer=="WasteConn")) & unintegrated=="Vertical")&
            !(target=="Republic" & !unintegrated %in% c("Base","Vertical") & !partial))



mergercases <- left_join(mergercases,mutate(mergercases_base,unintegrated="Base",keep=TRUE))
mergercases <- filter(mergercases,unintegrated!="Base" | !is.na(keep)) %>% select(-keep)

 mkt_mergersweep <- mapply(
   function(x,y,z,p){
     res=mergersweep(simres_vert,acquirer=x,target=y,unintegrated = z,partial=p)
     thisfirmsum =summary(res,market=FALSE,revenue=FALSE)
     sumres = summary(res,market=TRUE)
     sumres$mktSize <- with(thisfirmsum,sum(priceDownPre*sharesPre*res@down@mktSize/100))
     sumres$Acquirer=x
     sumres$Target=y
     sumres$Unintegrated=z
     sumres$Partial=ifelse(p,"Partial ","")

     thisfirmsum$mktSize <- with(thisfirmsum,sum(priceDownPre*sharesPre*res@down@mktSize/100))
     thisfirmsum$Acquirer=x
     thisfirmsum$Target=y
     thisfirmsum$Unintegrated=z
     thisfirmsum$Product=rownames(thisfirmsum)
     thisfirmsum$Partial=ifelse(p,"Partial ","")
     thisfirmsum$alpha=res@down@slopes$alpha
     thisfirmsum$mcDown=calcMC(res,preMerger=TRUE)$down
     thisfirmsum$mcUp=calcMC(res,preMerger=TRUE)$up
     thisfirmsum$bargPre=res@up@bargpowerPre
     thisfirmsum$bargPost=res@up@bargpowerPost

     return(list(sum=sumres,firm=thisfirmsum))

   },
   mergercases$acquirer ,
   mergercases$target,
   mergercases$unintegrated,
   mergercases$partial,
   SIMPLIFY=FALSE)





 mkt_mergersweep_firm <- bind_rows(lapply(mkt_mergersweep,function(x){return(x$firm)}))
 mkt_mergersweep <- bind_rows(lapply(mkt_mergersweep,function(x){return(x$sum)}))

 mkt_mergersweep <-  mutate(mkt_mergersweep,across(contains("$"),~.x/mktSize*100)) %>%
   pivot_longer(cols=`Up Price Change (%)`:`Difference ($)` ) %>%
   mutate(name=gsub("\\s*\\(.*","",name,perl=TRUE),
          name=gsub("Up\\s*(Producer)?\\s*","Disposal ",name,perl=TRUE),
          name=gsub("Down\\s*(Producer)?\\s*","Collection ",name,perl=TRUE),
          name=factor(name),
          name=relevel(name,"Consumer Harm"),
          Unintegrated=factor(Unintegrated,levels=c("Base","Down","Vertical","Up")),
          Unintegrated=interaction(Partial,Unintegrated,drop = TRUE,sep=""),
          Unintegrated=reorder(Unintegrated,ifelse(Acquirer=="Republic" & Target=="Santek" & name=="Consumer Harm",-value,NA),max,na.rm=TRUE),
          Acquirer=factor(Acquirer),
          Target=factor(Target,levels=c("Republic","Santek","WasteConn","WM-ADS","Regional")),
   )


 trashfakemergerplot <- ggplot(data=filter(mkt_mergersweep,Acquirer!="WM-ADS"& #Target !="Regional" &#Acquirer=="Republic" & Target=="Santek" &
                                                     name %in% c("Consumer Harm","Disposal Benefit","Collection Benefit")),aes(x=Unintegrated,y=value,color=name,group=name))+
   geom_point()+geom_line()+ facet_grid(Target~Acquirer,scales="free") +theme_bw()+ylab("% of Pre-merger Expenditures")+ xlab("Merger") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title=element_blank(),legend.position="bottom") +geom_hline(yintercept=0,linetype="dashed")

ggsave("./output/trashfakemerger.png",trashfakemergerplot,height = 7,width = 7)


trashfakemergerplotalt <- ggplot(data=filter(mkt_mergersweep,Acquirer!="WM-ADS"& #Target !="Regional" &#Acquirer=="Republic" & Target=="Santek" &
                                            name %in% c("Consumer Harm","Disposal Benefit","Collection Benefit")) %>%
                                              mutate(Merger=paste(Acquirer,Target,sep=":"),
                                                     Merger=reorder(Merger,ifelse(name=="Consumer Harm" & Unintegrated=="Base",-value,NA),max,na.rm=TRUE),
                                                     Unintegrated=relevel(Unintegrated,"Base")),aes(x=Merger,y=value,color=name,group=name))+
  geom_point()+geom_line()+ facet_grid(~Unintegrated,scales="free") +theme_bw()+ylab("% of Pre-merger Expenditures")+ xlab("Merger") +  scale_x_discrete(limits=rev) + coord_flip()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title=element_blank(),legend.position="bottom") +geom_hline(yintercept=0,linetype="dashed")

ggsave("./output/trashfakemergeralt.png",trashfakemergerplotalt,height = 7,width = 7)


trashinterestingmergerbar <- ggplot(data=filter(mkt_mergersweep,Unintegrated=="Base" &
                                                  name %in% c("Consumer Harm","Disposal Benefit","Collection Benefit")) %>% rename(Type=name) %>%
                                      mutate(Merger=paste(Acquirer,Target,sep="/") ) %>%
                                      filter(Merger %in% c("Republic/Santek","Santek/WasteConn","Santek/WM-ADS", "WM-ADS/Regional")),aes(y=Merger,x=value,fill=Type)) +
  #facet_grid(~Type,scales = "fixed") +
  geom_bar(stat="identity",#fill="lightgrey",
                                                         position=position_dodge()
                                                         #position="stack"
  )  +
   xlab("% of Pre-merger Expenditures") + geom_vline(xintercept = 0,linetype="dashed",color="goldenrod") +
  scale_fill_brewer(type="qual",palette = "YlGnBu",direction=-1) +#scale_fill_grey(start = .9, end = .1) +
  geom_text(aes(label=round(value,1)),hjust=1.1,color="black",
   position=position_dodge(width=.9),size =3) +
  theme_bw() +
  theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(limits=rev)

ggsave(filename="./output/trashinterestingmergerbar.png",trashinterestingmergerbar,width = 6,height=4)

mkt_mergersweep_firm$Product <- rownames(mkt_mergersweep_firm )
mkt_mergersweep_firm$Product <- gsub("\\..*$","",mkt_mergersweep_firm$Product,perl=TRUE)
mkt_mergersweep_firm$Product <- gsub(":","/",mkt_mergersweep_firm$Product,perl=TRUE)

trashinterestingfirmbar <- ggplot(data= filter(mkt_mergersweep_firm,Unintegrated=="Base" ) %>%
                                    mutate(Merger=paste(Acquirer,Target,sep="/")) %>%
                                    filter(Merger %in% c(#"Republic/Santek",
                                      "Santek/WasteConn","Santek/WM-ADS", "WM-ADS/Regional")) %>%
                                    select(-priceUpDelta,-priceDownDelta,-outputDelta,-isParty) %>%
                                  pivot_longer(c(priceUpPre ,priceUpPost , priceDownPre, priceDownPost, sharesPre, sharesPost)) %>%
                                    mutate(Level=ifelse(grepl("Up",name),"Disposal","Collection"),
                                           Pre=ifelse(grepl("Pre",name),"Pre-merger","Post-merger"),
                                           name=gsub("Pre|Post|Up|Down","",name)) %>%
                                    rename(Effect=name) %>%
                                    pivot_wider(values_from=value,names_from=Pre) %>%
                                    mutate(`Change (%)`=(`Post-merger`/`Pre-merger` - 1)*100,
                                           Effect=factor(Effect, labels=c("Prices","Shares")),
                                           Level=factor(Level,levels=rev(sort(unique(Level))))) %>%
                                    #arrange(Level,Effect,Disposal,desc(`Change (%)`)) %>%
                                    mutate(across(where(is.numeric),round)) %>% relocate(Level,Effect) %>%
                                    mutate( Product=factor(Product)) %>%
                                    pivot_longer(c(`Pre-merger`,`Post-merger`#,`Change (%)`
                                                   )) %>% rename(Type=name) %>%
                                    mutate(Type=factor(Type,levels=c("Pre-merger","Post-merger"))),
                   aes(x=Product,y=value,fill=Type,label=value)) +
  facet_grid(Merger~Level+Effect,scales = "free_x") + geom_bar(stat="identity",
                                                         position=position_dodge2(reverse = TRUE)
                                                         #position="stack"
  )  +
  xlab("Disposal/Collector") + ylab("Equilibrium Levels") + #geom_hline(yintercept = 0,linetype="dashed") +
  scale_fill_brewer(type="qual",palette = "Paired") +#scale_fill_grey(start = .9, end = .1) +
  geom_text( #color=ifelse(Type=="Post-merger","white","black"),
    hjust=1.5, position=position_dodge2(width=.9,reverse=TRUE),size =3) +  coord_flip()+ theme_bw() +
  theme(legend.position="bottom",axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(limits=rev)

ggsave(filename="./output/trashinterestingfirmbar.png",trashinterestingfirmbar,width=7,height=7)
write_csv(mkt_mergersweep,"./output/mkt_mergersweep.csv",na="")
write_csv(mkt_mergersweep_firm,"./output/mkt_mergersweep_firm.csv",na="")

# mkt_interesting <- filter(mkt_mergersweep,
#                           (Acquirer=="Republic" & Target =="Santek" & Unintegrated %in% c("Base","Up","Down","Vertical")) |
#                             (Acquirer=="Santek" & Target =="WasteConn" & Unintegrated %in% c("Base")) |
#                                (Acquirer=="Santek" & Target =="WM-ADS" & Unintegrated %in% c("Base")   ) )
#
# mkt_interesting_firm <- filter(mkt_mergersweep_firm,
#                                Acquirer=="Republic" & Target =="Santek" & Unintegrated %in% c("Base","Up","Down","Vertical") |
#   (Acquirer=="Santek" & Target =="WasteConn" & Unintegrated %in% c("Base")) |
#   (Acquirer=="Santek" & Target =="WM-ADS" & Unintegrated %in% c("Base")   )
# ) %>%
#   mutate(Unintegrated=factor(Unintegrated,levels=c("Base","Down","Vertical","Up")),
#          Unintegrated=interaction(Partial,Unintegrated,drop = TRUE,sep=""))


# trashinterestingplot <- ggplot(data=na.omit(filter(mkt_interesting,
#                                                     name %in% c("Consumer Harm","Disposal Benefit","Collection Benefit"))),aes(x=interaction(Acquirer,Target,sep=":",drop=TRUE),y=value,color=name,group=name))+
#   geom_point()+geom_line()+ facet_grid(~Unintegrated,scales="free_x") +theme_bw()+ylab("% of Pre-merger Expenditures")+ xlab("Merger") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title=element_blank(),legend.position="bottom") +geom_hline(yintercept=0,linetype="dashed")
#
# ggsave("./output/trashinterestingmerger.png",trashinterestingplot,height = 7,width = 7)
# write_csv(mkt_interesting,"./output/mkt_interesting.csv",na="")
# write_csv(mkt_interesting_firm,"./output/mkt_interesting_firm.csv",na="")


 bargsweep <- function(thissim,barg=0.5,who=c("Santek:Republic","WasteConn:Republic","All","Base")){

   who <- match.arg(who)

   if(who=="Base"){return(thissim)}

   relevantPre <- switch(who,
                         "Santek:Republic"=names(thissim@up@bargpowerPre)=="Santek:Republic",
                         "WasteConn:Republic"=names(thissim@up@bargpowerPre)=="WasteConn:Republic",
                         #All=thissim@up@bargpowerPre !=1
                         All=thissim@up@ownerPre!=thissim@down@ownerPre
   )

   relevantPost <- switch(who,
                          "Santek:Republic"=names(thissim@up@bargpowerPost)=="Santek:Republic",
                          "WasteConn:Republic"=names(thissim@up@bargpowerPost)=="WasteConn:Republic",
                          #All=thissim@up@bargpowerPost !=1
                          All=thissim@up@ownerPost!=thissim@down@ownerPost
   )

   ## fix bargaining power parameters at barg and solve for new alpha
   minBarg_bert<- optimize(function(x){minD(c(x,barg),relevant = relevantPre)},lower=-1e4,upper=-1e-4)
   thissim@down@slopes$alpha <- minBarg_bert$minimum
   thissim@down@slopes$meanval <- log(thissim@down@shares) - log(thissim@down@shares)[1] -  thissim@down@slopes$alpha*(simres_vert@down@prices - simres_vert@down@prices[1])
   thissim@up@bargpowerPre[relevantPre] <- barg
   thissim@up@bargpowerPost[relevantPost] <- barg
   thissim <- ownerToMatrix(thissim, preMerger = TRUE)
   thissim <- ownerToMatrix(thissim, preMerger = FALSE)

   mcPre <- calcMC(thissim, TRUE)
   mcPost <- calcMC(thissim, FALSE)

   thissim@down@mcPre <- mcPre$down
   thissim@down@mcPost <- mcPost$down
   thissim@up@mcPre <- mcPre$up
   thissim@up@mcPost <- mcPost$up

   thissimsPre <- calcPrices(thissim, preMerger = TRUE)
   thissim@down@pricePre <- thissimsPre$down
   thissim@up@pricePre <- thissimsPre$up
   thissimsPost <- calcPrices(thissim, preMerger = FALSE)
   thissim@down@pricePost <- thissimsPost$down
   thissim@up@pricePost <- thissimsPost$up

   return(thissim)

 }


 mkt_bargsweep <- lapply(c("Santek:Republic","WasteConn:Republic","All","Base"),
                         function(x){
                           res_bargsweep=lapply(seq(0.1,0.9,0.1), function(z){bargsweep(simres_vert,barg=z,who=x)})
                           names(res_bargsweep ) <- seq(0.1,0.9,0.1)


                           res=bind_rows(lapply(res_bargsweep,
                                                function(z){
                                                  thisfirmsum =summary(z,market=FALSE,revenue=FALSE)
                                                  thissum = summary(z,market=TRUE)

                                                  thissum$mktSize <- with(thisfirmsum,sum(priceDownPre*sharesPre*z@down@mktSize/100))
                                                  thissum$alpha <- z@down@slopes$alpha
                                                  #isMkt <- try(HypoMonTest(z,1:nrow(thisfirmsum)),silent=TRUE)
                                                  #if(class(isMkt)=="try-error") isMkt <- NA
                                                  #thissum$isMkt <- isMkt
                                                  return(thissum)
                                                }
                           )
                           ,.id="barg")

                           if(x=="Base"){
                             res$barg=NA
                             res=unique(res)
                           }
                           return(res)
                         })





 names(mkt_bargsweep) <- c("Santek:Republic","WasteConn:Republic","All","Base")
 mkt_bargsweep <- bind_rows(mkt_bargsweep,.id="scenario")

 mkt_bargsweep <-  mutate(mkt_bargsweep,across(contains("$"),~.x/mktSize*100)) %>%
   pivot_longer(cols=`Up Price Change (%)`:`Difference ($)` ) %>%
   mutate(name=gsub("\\s*\\(.*","",name,perl=TRUE),
          name=gsub("Up\\s*(Producer)?\\s*","Disposal ",name,perl=TRUE),
          name=gsub("Down\\s*(Producer)?\\s*","Collection ",name,perl=TRUE),
          name=factor(name),
          name=relevel(name,"Consumer Harm")
   )

 trashbargplot <- ggplot(data=na.omit(filter(mkt_bargsweep,scenario=="All" & name %in% c("Consumer Harm"#,"Disposal Benefit","Collection Benefit"
                                                                       ))),aes(x=barg,y=value,
                                                                                                                                      #color=scenario,
                                                                                                                                      group=scenario
                                                                                                                                      #color=name,group=name
                                                                                                                                      ))+
   geom_point()+geom_line()+
   #facet_grid(name~.,scales = "free") +
   theme_bw()+ylab("% of Pre-merger Expenditures")+ xlab("Bargaining Power") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title=element_blank(),legend.position="bottom") +
   geom_hline(data=mkt_bargsweep %>%
                filter(scenario=="Base" & name %in% c("Consumer Harm"#,"Disposal Benefit","Collection Benefit"
                                                      ))
              ,aes(yintercept = value),linetype="dashed") +# labs(title="The Effect of Changing Bargaining Power on Simulation Outcomes",
                                                                                       #subtitle="Dashed horizontal lines depict base case.")
 scale_color_brewer(type="seq",palette = "YlGnBu",direction=-1) +
   scale_y_continuous(breaks=c(round((mkt_bargsweep %>%filter(scenario=="Base" & name %in% c("Consumer Harm")))$value,1),seq(-10,20,5)))

 ggsave("./output/trashbarg.png",trashbargplot,height = 7,width = 7)


 elastsweep <- function(thissim,shareOut=0,priceOut=0,multiplier=1){


   if(shareOut==0){return(thissim)}



   thissim@down@normIndex <- NA
   thissim@down@shareInside <- 1-shareOut
   thissim <- calcSlopes(thissim)
   #thissim@down@slopes$alpha <- multiplier*thissim@down@slopes$alpha
   #thissim@down@slopes$meanval <- log(thissim@down@shares) - log(shareOut) -  thissim@down@slopes$alpha*(simres_vert@down@prices - priceOut)


   thissim <- ownerToMatrix(thissim, preMerger = TRUE)
   thissim <- ownerToMatrix(thissim, preMerger = FALSE)

   mcPre <- calcMC(thissim, TRUE)
   mcPost <- calcMC(thissim, FALSE)

   thissim@down@mcPre <- mcPre$down
   thissim@down@mcPost <- mcPost$down
   thissim@up@mcPre <- mcPre$up
   thissim@up@mcPost <- mcPost$up

   thissimsPre <- calcPrices(thissim, preMerger = TRUE)
   thissim@down@pricePre <- thissimsPre$down
   thissim@up@pricePre <- thissimsPre$up
   thissimsPost <- calcPrices(thissim, preMerger = FALSE)
   thissim@down@pricePost <- thissimsPost$down
   thissim@up@pricePost <- thissimsPost$up

   return(thissim)

 }


 mkt_elastsweep <- lapply(seq(0,0.9,0.05),
                          function(x){
                            res_elastsweep=elastsweep(simres_vert,shareOut = x)})


 names(mkt_elastsweep ) <- seq(0,0.9,0.05)


 mkt_elastsweep=bind_rows(lapply(mkt_elastsweep,
                                 function(z){
                                   thisfirmsum =summary(z,market=FALSE,revenue=FALSE)
                                   thissum = summary(z,market=TRUE)
                                   thissum$mktSize <- with(thisfirmsum,sum(priceDownPre*sharesPre*z@down@mktSize/100))
                                   thissum$mktElast <- elast(z,market=TRUE)
                                   thissum$alpha <- z@down@slopes$alpha
                                   isMkt <- try(HypoMonTest(z,1:nrow(thisfirmsum),ssnip=.05),silent=TRUE)
                                   if(class(isMkt)=="try-error") isMkt <- NA
                                   thissum$isMkt <- isMkt
                                   return(thissum)
                                 }
 )
 ,.id="shareOut")



 mkt_elastsweep <-  mutate(mkt_elastsweep,across(contains("$"),~.x/mktSize*100)) %>%
   pivot_longer(cols=`Up Price Change (%)`:`Difference ($)` ) %>%
   mutate(name=gsub("\\s*\\(.*","",name,perl=TRUE),
          name=gsub("Up\\s*(Producer)?\\s*","Disposal ",name,perl=TRUE),
          name=gsub("Down\\s*(Producer)?\\s*","Collection ",name,perl=TRUE),
          name=factor(name),
          name=relevel(name,"Consumer Harm")
   )

 mkt_elastsweep$shareElast <- factor(paste0(as.numeric(mkt_elastsweep$shareOut)*100,"\n","(",abs(round(mkt_elastsweep$mktElast,1)),")"))
 mkt_elastsweep$shareElast <- relevel( mkt_elastsweep$shareElast, ref="5\n(0.3)")
 mkt_elastsweep$shareElast <- relevel( mkt_elastsweep$shareElast, ref= "0\n(0)")

 trashelastplot <- ggplot(data=na.omit(filter(mkt_elastsweep,name %in% c("Consumer Harm"
                                                                         #,"Disposal Benefit","Collection Benefit"
                                                                         )
                                                )
                                              ),aes(x=shareElast,y=value,#color=name,
                                                    group=name
                                                    ))+
   geom_point()+geom_line() +theme_bw()+ylab("% of Pre-merger Expenditures")+ xlab("% Outside Share (Market Elasticity)") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title=element_blank(),legend.position="bottom") +
   #labs(title="The Effect of Changing Outside Share on Simulation Outcomes",

   #subtitle="Outside share of 0 depicts base case.") +
   scale_color_brewer(type="seq",palette = "YlGnBu",direction=-1) +
   scale_x_discrete( breaks=levels(mkt_elastsweep$shareElast)[seq(1,length(levels(mkt_elastsweep$shareElast)),by=2)])

ggsave("./output/trashelast.png",trashelastplot,width = 7,height=7)

trashelastplot_alt <- ggplot(data=na.omit(filter(mkt_elastsweep,name %in% c("Consumer Harm","Disposal Benefit","Collection Benefit"))),aes(x=factor(abs(round(as.numeric(mktElast),2))),y=value,color=name,group=name))+
  geom_point()+geom_line() +theme_bw()+ylab("% of Pre-merger Expenditures")+ xlab("Elasticity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title=element_blank(),legend.position="bottom") + labs(title="The Effect of Changing Outside Share on Simulation Outcomes",
                                                                                                                        subtitle="Outside share of 0 depicts base case.")+
  scale_color_brewer(type="seq",palette = "YlGnBu",direction=-1)




nestsweep <- function(thissim,nestParm=1, nests=c("integrated","landfill")){

  nests <- match.arg(nests)


  if(nestParm==1){return(thissim)}


  nests <- switch(nests,
                  integrated= with(simdata,factor(ifelse(collection_firm_name == disposal_firm_name,"Integrated","Unintegrated" ))),
                  landfill = factor(simdata$ownerPreUp)
                  )

  sigma=rep(nestParm,nlevels(nests))
  names(sigma) <- levels(nests)

  #nestCnt      <- as.numeric(table(nests))
  #isSingletonNest <- nestCnt==1
  #sigma[isSingletonNest] <- 0

  upNest <- thissim@up
  upNest@pricePre <-upNest@pricePost <- numeric()
  barg <- unique(upNest@bargpowerPre)
  barg <- barg[barg!=1]
  barg <- barg[1:min(2,length(barg))]
  downNest <-
    new("LogitNests",
        prices=thissim@down@prices, shares=thissim@down@shares,
        margins=thissim@down@margins,
        normIndex=thissim@down@normIndex,
        ownerPre=simdata$ownerPreDown,
        ownerPost=simdata$ownerPostDown,
        insideSize=thissim@down@insideSize,
        mktSize=thissim@down@mktSize,
        mcDelta=thissim@down@mcDelta,
        nests=nests,
        subset=thissim@down@subset,
        priceOutside=thissim@down@priceOutside,
        priceStart=thissim@down@priceStart,
        shareInside=ifelse(isTRUE(all.equal(sum(thissim@down@shares),1,check.names=FALSE,tolerance=1e-3)),1,sum(thissim@down@shares)),
        labels=thissim@down@labels)



  ##save upstream and downstream data
  thissim_nests <- new("VertBargBertLogitNests",
                                      up = upNest,
                                      down = downNest,
                                      constrain ="global",
                                      chain_level="full",
                                      supplyDown="bertrand",
                                      isHorizontal=thissim@isHorizontal,
                                      isUpstream=thissim@isUpstream
  )


  #thissim_nests@down@slopes <- thissim@down@slopes

  minParm<- optim(c(-1,rep(.6,2)),minD,nests=nests,nestParm=1-sigma,method="L-BFGS-B",lower=c(-1e3,rep(.1,2)),upper=c(-1e-4,rep(1,2)))
  #minParm<- optimize(function(x){minD(c(x,rep(barg,2)),nests=nests,nestParm=1-sigma,)},lower=-1e4,upper=-1e-4)
  #sigma[isSingletonNest] <- 1
  thissim_nests@down@slopes <- list(sigma=sigma)


  #names(thissim_nests@down@slopes$sigma) <- levels(nests)
  #
  # meanval <-
  #    log(thissim_nests@down@shares)- log(shareOut) -
  #          thissim_nests@down@slopes$alpha*(thissim_nests@down@prices - thissim_nests@down@priceOutside) +
  #          (thissim_nests@down@slopes$sigma-1)*log(sharesNests)
  #
  #
  #       names(meanval)   <- object@labels


  shares <- thissim_nests@down@shares
  prices <- thissim_nests@down@prices
  alpha <- thissim_nests@down@slopes$alpha <- minParm$par[1]
  sharesBetween <- as.vector(tapply(shares,nests,sum)[nests])

  if(is.na(thissim_nests@down@normIndex)){
    shareOut <- 1 - thissim_nests@down@shareInside
    priceOut <- thissim_nests@down@priceOutside
  }
  else{
    idx <- thissim_nests@down@normIndex
    shareOut <- shares[idx]
    shareOutBetween <- sharesBetween[idx]
    priceOut <- prices[idx]
    nestOut <- sigma[nests]
    nestOut <- nestOut[idx]
  }

  #thissim_nests@down@slopes$meanval=(1-nestParm[nests])*(log(shares)-log(shareOut))- alpha*(prices - priceOut) + nestParm[nests]*(log(sharesBetween) - log(shareOut))
  meanval <-
    sigma[nests]*(log(shares)  - log(shareOut)) -
    alpha*(prices/sigma[nests] - priceOut/nestOut) -
    ((sigma[nests]-1)*log(sharesBetween) - (nestOut-1)*log(shareOutBetween))

  thissim_nests@down@slopes$meanval <- meanval


  thissim_nests<- ownerToMatrix(thissim_nests, preMerger = TRUE)
  thissim_nests <- ownerToMatrix(thissim_nests, preMerger = FALSE)
  mcPre <- calcMC(thissim_nests, TRUE)
  mcPost <- calcMC(thissim_nests, FALSE)



  thissim_nests@down@mcPre <-  mcPre$down
  thissim_nests@down@mcPost <- mcPost$down

  thissim_nests@up@mcPre <-  mcPre$up
  thissim_nests@up@mcPost <- mcPost$up

pricePre  <- calcPrices(thissim_nests,preMerger=TRUE)
thissim_nests@down@pricePre <- pricePre$down
thissim_nests@up@pricePre <- pricePre$up


pricePost <- calcPrices(thissim_nests,preMerger=FALSE)
thissim_nests@down@pricePost <-pricePost$down
thissim_nests@up@pricePost <- pricePost$up


  return(thissim_nests)

}


nestcases <- expand_grid(nestParm=seq(0.1,1,0.1),nest=c("integrated"
                                                        ,"landfill"
                                                        )) #%>%
  #filter(!(nestParm==1 & nest=="landfill"))


mkt_nestsweep <- mapply(
  function(x,y){
    res=nestsweep(simres_vert,nestParm=x,nest=y)
    thisfirmsum =summary(res,market=FALSE,revenue=FALSE)
    sumres = summary(res,market=TRUE)
    sumres$mktSize <- with(thisfirmsum,sum(priceDownPre*sharesPre*res@down@mktSize/100))
    sumres$alpha=res@down@slopes$alpha
    sumres$nestParm=x
    sumres$nest=y

    thisfirmsum$mktSize <- with(thisfirmsum,sum(priceDownPre*sharesPre*res@down@mktSize/100))
    thisfirmsum$nestParm=x
    thisfirmsum$nest=y
    thisfirmsum$alpha=res@down@slopes$alpha
    thisfirmsum$mcDown=calcMC(res,preMerger=TRUE)$down
    thisfirmsum$mcUp=calcMC(res,preMerger=TRUE)$up
    thisfirmsum$barg=res@up@bargpowerPre

    return(list(sum=sumres,firm=thisfirmsum))

  },
  nestcases$nestParm ,
  nestcases$nest,
  SIMPLIFY=FALSE)


mkt_nestsweep_firm <- bind_rows(lapply(mkt_nestsweep,function(x){return(x$firm %>% mutate(product=rownames(x$firm)))}))
mkt_nestsweep <- bind_rows(lapply(mkt_nestsweep,function(x){return(x$sum)}))

mkt_nestsweep <-  mutate(mkt_nestsweep,across(contains("$"),~.x/mktSize*100)) %>%
  pivot_longer(cols=`Up Price Change (%)`:`Difference ($)` ) %>%
  mutate(name=gsub("\\s*\\(.*","",name,perl=TRUE),
         name=gsub("Up\\s*(Producer)?\\s*","Disposal ",name,perl=TRUE),
         name=gsub("Down\\s*(Producer)?\\s*","Collection ",name,perl=TRUE),
         name=factor(name),
         name=relevel(name,"Consumer Harm"),
         nestParm=factor(1-nestParm)
         )


mkt_nestsweep$nest <- factor(mkt_nestsweep$nest)
mkt_nestsweep$nest <- factor(mkt_nestsweep$nest,levels=levels(mkt_nestsweep$nest),labels=stringr::str_to_title(levels(mkt_nestsweep$nest)))
trashnestplot <- ggplot(data=filter(mkt_nestsweep,
                                            #nest=="All" &
                                            name %in% c("Consumer Harm"
                                                        #,"Disposal Benefit","Collection Benefit"
                                                        )),aes(x=nestParm,y=value,color=nest,
                                                               group=nest))+
  geom_point()+geom_line()+
  #facet_grid(~nest) +
theme_bw()+ylab("% of Pre-merger Expenditures")+ xlab("Nesting Parameter") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title=element_blank(),legend.position="bottom") +geom_hline(yintercept=0,linetype="dashed") + scale_color_brewer(type="seq",palette = "YlGnBu",direction=-1)

ggsave("./output/trashnest.png",trashnestplot,height = 7,width = 7)
mkt_nestsweep_firm$nestParm <- 1-mkt_nestsweep_firm$nestParm
write_csv(mkt_nestsweep_firm,"./output/mkt_nestsweep_firm.csv",na="")

