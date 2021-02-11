
## load classes and methods from bargaining_calibrate
source("../bargaining_calibrate/MonteCarlo.R")

## modify methods and functions to allow for upstream and downstream linear marginal costs
simMarket.1st <- function(mkt,...){


  nprod.down <- mkt$down$nproducts
  nprod.up   <- mkt$up$nproducts
  sharesDown <- mkt$down$shares
  shareOutDown <- mkt$down$shareOut
  ownerPre.up <- mkt$up$ownerPre
  ownerPre.down <- mkt$down$ownerPre
  mcshare.up <- mkt$up$mcshare
  mcshare.down <- mkt$down$mcshare
  bargparm <- mkt$up$bargparm
  outMargin <- mkt$down$outMargin

  M <- mkt$down$M

  nests <- mkt$down$nests
  nestMat=mkt$down$nestMat
  alpha    <- -1/(mkt$down$outMargin*(1-shareOutDown))

  nestParm <- mkt$down$nestParm

  sharesBetween.down <- as.vector(tapply(sharesDown,nests,sum)[nests])
  sharesWithin.down <- sharesDown/sharesBetween.down

    elastDiff.down <- -alpha*tcrossprod(sharesDown)
    elastSame.down <- -alpha*tcrossprod(sharesDown*(1 + nestParm[nests] /( 1-nestParm[nests] )*(1/sharesBetween.down)),sharesDown)
    elast.down <- elastSame.down*nestMat + elastDiff.down*(1-nestMat)
    diag(elast.down) <- alpha/(1-nestParm[nests])*sharesDown*(1-nestParm[nests]*sharesWithin.down - (1-nestParm[nests])*sharesDown)
    downMargin = - as.vector(solve(ownerPre.down * elast.down) %*% sharesDown)  #compute downstream margins


    ## Use upstream FOC, diversions, and downstream margins  to solve for upstream margins

    #div <- tcrossprod(1/(1-sharesDown),sharesDown)
    #diag(div) <- -1

    div <- diversions(sharesDown,nests, nestParm,nestMat)

    upMargin <-   as.vector(solve(ownerPre.up * div) %*% ( (1-bargparm)/bargparm *((ownerPre.down * div) %*% downMargin)))


  upMC <- mcshare.up*upMargin
  upPrices <- upMargin + upMC

  ## Use downstream margin and upstream prices to solve for downstream prices
  downMC <- mcshare.down*downMargin
  downPrices <- upPrices + downMargin + downMC

  ##compute downstream mean valuations (outside good has mc of 0)
  meanvalDown=(1-nestParm[nests])*(log(sharesDown)-log(shareOutDown))-alpha*(downPrices - outMargin) + nestParm[nests]*(log(sharesBetween.down) - log(shareOutDown))

  ## create a container to store upstream and downstream data
  mkt<-list(up=c(mkt$up,list(price=upPrices,margin=1-upMC/upPrices, mcParm = sharesDown*M/upMC)),
            down=c(mkt$down,list(alpha=alpha,price=downPrices,
                                 margin=1-(upPrices + downMC)/downPrices, mcParm =sharesDown*M/downMC, meanval=meanvalDown)),
            vertical = mkt$vertical
  )

  class(mkt)<-"1st"
  return(mkt)


}


FOC.1st<-function(priceCand,mkt,preMerger=TRUE,  subset=rep(TRUE,length(mkt$down$meanval)),useEst = FALSE, level = "all"){

  if(useEst){
    stop("calibration routine for nesting parameter not yet written!")
    alpha <- mkt$est$alpha
    meanval <- mkt$est$meanval[subset]
    bargparm <- mkt$est$bargparm[subset]
    mcUp <- mkt$est$mcUp[subset]
    mcDown <- mkt$est$mcDown[subset]

  }

  else{
    alpha <- mkt$down$alpha
    meanval <- mkt$down$meanval[subset]
    bargparm <- mkt$up$bargparm[subset]
    mcParmUp <- mkt$up$mcParm[subset]
    mcParmDown <- mkt$down$mcParm[subset]
    nestParm <- mkt$down$nestParm
  }


  M <- mkt$down$M

  nprods <- nrow(mkt$down$ids[subset,])
  outMargin <- mkt$down$outMargin

  nests <- mkt$down$nests[subset]
 nestMat=mkt$down$nestMat[subset,subset]



  v <- mkt$vertical

  if(preMerger){
    ownerUp <- mkt$up$ownerPre[subset,subset]
    ownerDown <- mkt$down$ownerPre[subset,subset]
  }
  else{

    ownerUp <- mkt$up$ownerPost[subset,subset]
    ownerDown <- mkt$down$ownerPost[subset,subset]
  }

  if(level == "all"){
    priceCandUp= rep(NA, 1,nprods)[subset]
    priceCandUp <- priceCand[1:length(priceCandUp)]
    priceCandDown <- priceCand[-(1:length(priceCandUp))]
  }
  else if (level =="down"){
    priceCandUp = mkt$up$price[subset]
    priceCandDown = priceCand
  }
  else if (level =="up"){
    priceCandDown = mkt$down$price[subset]
    priceCandUp = priceCand
  }


  shareCandDown  <- calcShares(mkt,priceCandDown - outMargin, mval=meanval, nests=nests, useEst = useEst)

  shareCandBetween.down <- as.vector(unname(tapply(shareCandDown,nests,sum)[nests]))
  shareCandWithin.down <- shareCandDown/shareCandBetween.down


  mcUp <- shareCandDown*M/mcParmUp
  mcDown <- shareCandDown*M/mcParmDown

  elastDiff.down <- -alpha*tcrossprod(shareCandDown)
  elastSame.down <- -alpha*tcrossprod(shareCandDown*(1 + nestParm[nests] /( 1-nestParm[nests] )*(1/shareCandBetween.down)),shareCandDown)
  elast <- elastSame.down*nestMat + elastDiff.down*(1-nestMat)
  diag(elast) <- alpha/(1-nestParm[nests])*shareCandDown*(1-nestParm[nests]*shareCandWithin.down - (1-nestParm[nests])*shareCandDown)
  elast.inv <- solve(ownerDown * elast)
  marginsDownCand = - as.vector(elast.inv %*% shareCandDown)  #compute downstream margins


  #div <- tcrossprod(1/(1-shareCandDown),shareCandDown)
  #diag(div) <- -1

  div <- diversions(shareCandDown, nests, nestParm,nestMat)

  if(length(v) > 0 && !preMerger){
    marginsDownCand <-  marginsDownCand - elast.inv %*% ( (v$ownerPost.down * elast) %*% (priceCandUp-mcUp) )
    #upFOC <- (ownerUp * div) %*% (priceCandUp-mcUp) - (v$ownerPostLambda.down * div) %*% marginsDownCand
    #marginsUpCand <- as.vector((v$ownerPostLambda.down * div) %*% marginsDownCand)
    marginsUpCand <- as.vector(solve(ownerUp * div) %*% (v$ownerPostLambda.down * div) %*% marginsDownCand)

    #upFOC <- as.vector((ownerUp * div) %*% (priceCandUp-mcUp)) - marginsUpCand
    upFOC <-  priceCandUp-mcUp - marginsUpCand
}
  else{

    #marginsUpCand <-    as.vector(solve(ownerUp * div) %*% ((1-bargparm)/bargparm *(( ownerDown * div) %*% marginsDownCand)))
    marginsUpCand <-    as.vector((( ownerDown * div) %*% marginsDownCand))

    upFOC<- as.vector(bargparm *((ownerUp * div) %*% (priceCandUp-mcUp))  -  (1-bargparm) * marginsUpCand)


  }


  #upFOC <-  priceCandUp-mcUp - marginsUpCand
  #upFOC <-  as.vector(bargparm *((ownerUp * div) %*% (priceCandUp-mcUp))) - (1-bargparm) * marginsUpCand
  downFOC <- priceCandDown - priceCandUp - mcDown - marginsDownCand


  if(level == "all"){thisFOC= c(downFOC,upFOC)}
  else if (level == "down"){thisFOC = downFOC}
  else if (level == "up"){thisFOC = upFOC}

return(thisFOC)
}



## Define calcPrices method for 2nd, 1st

calcPrices.default <- function(mkt,preMerger=TRUE,priceStartUp=mkt$up$price,
                           priceStartDown=mkt$down$price,
                           subset=rep(TRUE,nrow(mkt$down$ids)),
                           useEst = FALSE,level = c("all","up","down")){

  level <- match.arg(level)
  ## compute equlibrium prices given model parameters
  require(nleqslv,quietly=TRUE)

  ## Find price changes that set FOCs equal to 0
  if(level == "all"){
    priceStart <- c(priceStartUp[subset],priceStartDown[subset])
  }
  else if (level =="up"){
    priceStart <- c(priceStartUp[subset])
}
  else if (level =="down"){
    priceStart <- c(priceStartDown[subset])
  }
    minResult   <- try(nleqslv(priceStart,FOC,mkt=mkt,preMerger=preMerger, subset=subset, useEst=useEst, level = level)$x,silent=TRUE)

    minResultUp <- rep(NA, length(priceStartUp))
    minResultDown <- rep(NA, length(priceStartDown))

  if(class(minResult) != "try-error"){

    if(level == "all"){
      minResultUp[subset] <- minResult[1:length(priceStartUp[subset])]
      minResultDown[subset] <- minResult[-(1:length(priceStartUp[subset]))]
    }
    else if (level == "up"){
      minResultUp[subset] <- minResult
      minResultDown[subset] <- mkt$down$price[subset]
    }
    else if (level == "down"){
      minResultUp[subset] <- mkt$up$price[subset]
      minResultDown[subset] <- minResult
    }

  }
  return(list(up=minResultUp,down=minResultDown))

}



PS.default <- function(mkt,upPrices,downPrices, useEst = FALSE,market=FALSE,ratio=FALSE,party=FALSE){


  if(useEst){
    stop("not implemented")
    mcDown = mkt$est$mcDown
    mcUp = mkt$est$mcUp
  }
  else{
    mcParmDown = mkt$down$mcParm
    mcParmUp = mkt$up$mcParm
  }


  nestParm  <- mkt$down$nestParm
  nests <- mkt$down$nests

  quantities<-calcQuantities(mkt,downPrices- mkt$down$outMargin)


  mcDown <- quantities/mcParmDown
  mcUp <- quantities/mcParmUp

  psDown <- (downPrices-upPrices - mcDown)*quantities
  psUP <- (upPrices - mcUp) * quantities

  if(party){
    isParty <- mkt$down$isParty
    psUP <- psUP[isParty]
    psDown <- psDown[isParty]
    quantities <- quantities[isParty]
  }


  if(ratio){
     return(weighted.mean(psUP/psDown,quantities, na.rm=TRUE))
  }


  if(market){

    psDown <- sum(psDown,na.rm=TRUE)
    psUP <- sum(psUP, na.rm=TRUE)
  }


  return(list(down=psDown,up=psUP))

}






calcPricesHypoMon.default <-
    function(mkt,prodIndex=1:nrow(mkt$down$ids)){


    mcParm       <- mkt$down$mcParm[prodIndex]
    priceFixed <- mkt$down$price
    mkt$down$M

    calcMonopolySurplus <- function(priceCand){

      priceFixed[prodIndex] <- priceCand #keep prices of products not included in HM fixed at premerger levels

      sharesCand          <- calcShares(mkt,priceFixed -  mkt$down$outMargin)

      mc <- sharesCand*M/mcParm

      surplus             <- (priceCand-mc)*sharesCand[prodIndex]

      return(sum(surplus,na.rm=TRUE))
    }


    maxResult <- optim(mkt$down$price[prodIndex],calcMonopolySurplus,
                       method = "L-BFGS-B",lower = 0,
                       control=list(fnscale=-1))

    pricesHM <- maxResult$par

    #priceDelta <- pricesHM/pricePre[prodIndex] - 1
    #names(priceDelta) <- mkt@labels[prodIndex]

    return(pricesHM)

  }








summary.1st <-function(mkt, market =FALSE, vertical = ifelse(length(mkt$vertical)>0, TRUE,FALSE), useEst = FALSE,level = c("all","up","down")){


  if(useEst){
    mcDown = mkt$est$mcDown
    mcUp = mkt$est$mcUp
  }
  else{
    mcParmDown = mkt$down$mcParm
    mcParmUp = mkt$up$mcParm
  }

  M <- mkt$down$M
  nestParm <- mkt$down$nestParm
  nests <- mkt$down$nests

  pre <- calcPrices(mkt,preMerger=TRUE, useEst = useEst)
  post <- calcPrices(mkt,preMerger=FALSE, useEst = useEst)
  shares.pre <- calcShares(mkt, pre$down - mkt$down$outMargin, useEst = useEst)
  shares.post <- calcShares(mkt, post$down - mkt$down$outMargin, useEst = useEst)
  shareInside = sum(shares.pre,na.rm=TRUE)
  nsDown <- NA

  thislevel <- ifelse(!isTRUE(all.equal(mkt$up$ownerPre,mkt$up$ownerPost)),"up","down")
  pre.part <- list(up=rep(NA,length(pre$up)),down=rep(NA,length(pre$up)))
  #shares.pre.part <- pre.part <- pre.part[[thislevel]]
  post.part <- list(up=rep(NA,length(post$up)),down=rep(NA,length(post$up)))
  #shares.post.part <- post.part <- post.part[[thislevel]]

  if(vertical){
    subNS.up <- mkt$vertical$ownerPostNoSupply.up
    subNS.down <- mkt$vertical$ownerPostNoSupply.down
    post.ns.up <- calcPrices(mkt,preMerger=FALSE,subset=!subNS.up)
    post.ns.down <- calcPrices(mkt,preMerger=FALSE,subset=!subNS.down)
    shares.post.ns.up <- calcShares(mkt, post.ns.up$down -  mkt$down$outMargin, useEst =useEst )
    shares.post.ns.down <- calcShares(mkt, post.ns.down$down -  mkt$down$outMargin, useEst = useEst)

    isVup <- mkt$down$ids$up.firm==1
    isVDown <- mkt$down$ids$down.firm==1

    mcUp.ns.up <- shares.post.ns.up*M/mcParmUp
    mcDown.ns.up <- shares.post.ns.up*M/mcParmDown

    mcUp.ns.down <- shares.post.ns.down*M/mcParmUp
    mcDown.ns.down <- shares.post.ns.down*M/mcParmDown

    mcUp <- shares.post*M/mcParmUp
    mcDown <- shares.post*M/mcParmDown


    surplus.ns.up <- sum(((post.ns.up$up - mcUp.ns.up )*shares.post.ns.up)[isVup],na.rm=TRUE) +
      sum(((post.ns.up$down - post.ns.up$up - mcDown.ns.up )*shares.post.ns.up)[isVDown],na.rm=TRUE)

    surplus.ns.down <- sum(((post.ns.down$up - mcUp.ns.down )*shares.post.ns.down)[isVup],na.rm=TRUE) +
      sum(((post.ns.down$down - post.ns.down$up - mcDown.ns.down )*shares.post.ns.down)[isVDown],na.rm=TRUE)

    surplus.all <- sum(((post$up - mcUp )*shares.post)[isVup],na.rm=TRUE) +
      sum(((post$down - post$up - mcDown )*shares.post)[isVDown],na.rm=TRUE)

    surplus <- c(ns.down=surplus.ns.down, ns.up=surplus.ns.up, all = surplus.all)
    surplus.test <- which.max(surplus)

    if(names(surplus.test) == "ns.down"){
      post <- post.ns.down
      shares.post <- shares.post.ns.down
      nsDown <- TRUE
    }
    else if (names(surplus.test) == "ns.up") {
      post <- post.ns.up
      shares.post <- shares.post.ns.up
      nsDown <- FALSE
    }


  }

  else{

    thislevel <- ifelse(!isTRUE(all.equal(mkt$up$ownerPre,mkt$up$ownerPost)),"up","down")

    pre.part <- calcPrices(mkt,preMerger=TRUE, useEst = useEst,level=thislevel)
    shares.part.pre <- calcShares(mkt, pre.part$down - mkt$down$outMargin, useEst = useEst)
    #pre.part= pre.part[[thislevel]]
    #shares.part.pre <- shares.part.pre[[thislevel]]
    #pre.part$down <- pre.part$up



    post.part <- calcPrices(mkt,preMerger=FALSE,  useEst = useEst,level = thislevel)
    shares.part.post <- calcShares(mkt, post.part$down - mkt$down$outMargin, useEst = useEst)
    #post.part= post.part[[thislevel]]
    #shares.part.post <- shares.part.post[[thislevel]]
    #post.part$down <- pre.part$up

  }


  ## for upstream bertrand mergers, assume that
  if(thislevel == "up"){partcosts <- post.part$up - pre.part$up }
  else{ partcosts     <- rep(0,length(post.part$up))}

  res <- data.frame(upPricePre= pre$up,
                    upPricePost=post$up,
                    downPricePre= pre$down,
                    downPricePost=post$down,
                    shares.pre= shares.pre,
                    shares.post=shares.post,
                    idDown = mkt$down$ids$down.firm,
                    idUp=mkt$down$ids$up.firm,
                    nsDown=nsDown,
                    alpha = mkt$down$alpha,
                    nestParm=mkt$down$nestParm[1],
                    mktElastPre = mkt$down$alpha * sum(pre$down*shares.pre/shareInside,na.rm=TRUE) * (1 - shareInside),
                    cv = CV(mkt, pre$down  , post$down ),
                    cv.part = CV(mkt, pre.part$down  , post.part$down + partcosts ),
                    relmarginPre = PS(mkt,pre$up,pre$down,ratio=TRUE),
                    relmarginPartyPre = PS(mkt,pre$up,pre$down,party=TRUE,ratio = TRUE),
                    upPSPre = PS(mkt,pre$up,pre$down,market = FALSE)$up,
                    downPSPre = PS(mkt,pre$up,pre$down,market = FALSE)$down,
                    upPSPost = PS(mkt,post$up,post$down,market = FALSE)$up,
                    downPSPost = PS(mkt,post$up,post$down,market = FALSE)$down,
                    upPSPost.part = PS(mkt,post.part$up,post.part$down,market = TRUE)$up,
                    downPSPost.part = PS(mkt,post.part$up,post.part$down,market = TRUE)$down,
                    upPricePost.part=post.part$up,
                    downPricePost.part=post.part$down,
                    #upPriceDelta.part = mean(1 - post.part$up/post$up,na.rm=TRUE),
                    #downPriceDelta.part = mean(1 - post.part$down/post$down,na.rm=TRUE)
                    upPriceDelta.part = sum((1 - post.part$up/post$up)*shares.post,na.rm=TRUE),
                    downPriceDelta.part = sum((1 - post.part$down/post$down)*shares.post,na.rm=TRUE)
                    #priceDelta.part= 1 - sum(shares.part.post*post.part) / sum(shares.post*post[[thislevel]])
                    #cv.part=CV(mkt,pre.part,post.part)
                    )

  if(market){

    #res <- sum(post$down*shares.post,na.rm=TRUE) - sum(pre$down*shares.pre,na.rm=TRUE) #CV in 2nd score w/o efficiencies
    res <-  unique(res$cv)

  }

  return(res)
}



