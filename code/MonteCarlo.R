
## load classes and methods from bargaining_calibrate
source("../bargaining_calibrate/code/MonteCarlo.R")




## Create constructor function to generate markets
## For Logit Demand and  a given downstream supply side
## Extends bargainig_calibrate to allow for different vertical configurations
market<-function(nfirms.down=3, # # of downstream firms
                 nprods.down=1, # # number of downstream products
                 supply.down=c("1st","2nd"), # whether downstream is a 2nd score or 1st score auction
                 M=1, # market size
                 nfirms.up=nfirms.down, # # of upstream firms
                 nprods.up=nprods.down, # # of upstream products
                 nfirms.vert=0, # number of integrated firms pre-merger
                 bargparm=runif(nfirms.down*nfirms.up, .25,.75), # bargaining parameters for each upstream/downstream firm pair
                 bargpreset=c("none","party","rival"), # fix some bargaining parameters
                 # none: do not modify bargparm
                 # party: set party bargparm to 0.5, allow others to vary
                 # rival: set non-parties to 0.5, allow others to vary
                 mcshare.up=runif(nfirms.up*nprods.up), # share of upstream costs
                 mcshare.down=runif(nfirms.down*nprods.down), # share of wholesale price
                 ownerPost = c("up","down","vertical"), # simulate an upstream horizontal merger, downstream horizontal merger,
                 # or a vertical merger
                 nests, # specify nesting structure. default is merging parties in same nest
                 nestParm = 0, # single nesting parameter. Default is flat Logit
                 isNestedTogether=TRUE, # specify if parties are in the same nest (TRUE) or different nests (FALSE)
                 outMargin=runif(1,1,1e2),
                 shareOutDown=runif(1,.01,.1), # outside good margin
                 shareParm=rep(1,nfirms.down*nprods.down*nfirms.up*nprods.up), # parameter for dirichlet (share) distribution (default is uniform)
                 largeMerger=FALSE, # force the firms with the largest market shares to merge,
                 ...){

  #demand <- match.arg(demand)
  ownerPost <- match.arg(ownerPost)
  supply.down <- match.arg(supply.down)
  bargpreset <- match.arg(bargpreset)


  if(any(is.na(nestParm) | nestParm >=1 | nestParm<0)){stop("nestParm must be >=0 and <1")}
  if(length(nestParm) > 2){stop("nestParm must be length less than or equal to 2")}

  #  if(nprods.down %% nfirms.down !=0 ||
  #     nprods.up %% nfirms.up !=0 ){
  #    stop(" The number of products (both upstream and downstream) must be evenly distributed across firms")
  #  }

  require(bayesm,quietly=TRUE)

  ids <- expand.grid(up.firm=as.character(1:nfirms.up),up.products=as.character(1:nprods.up),
                     down.firm=as.character(1:nfirms.down),down.products=as.character(1:nprods.down),
                     stringsAsFactors = TRUE)

  nprods <- nrow(ids)

  isJoint <-  isParty <- switch(ownerPost,
                                up = ids$up.firm == 1 | ids$up.firm == 2,
                                down = ids$down.firm == 1 | ids$down.firm == 2,
                                vertical= ids$up.firm == 1 | ids$down.firm == 1,
                                both=ids$up.firm == 1 | ids$up.firm == 2)

  if(ownerPost=="vertical"){isJoint <-  ids$up.firm == 1 & ids$down.firm == 1}

  ## create nesting structures for different merger types
  if( missing(nests)){
    if(isNestedTogether){
      if(ownerPost %in% c("up","both")) nests <- ids$up.firm == 1 | ids$up.firm == 2
      else if(ownerPost == "down")      nests <- ids$down.firm == 1 | ids$down.firm == 2
      else if (ownerPost == "vertical") nests <- ids$up.firm == 1 | ids$down.firm == 1
    }

    else{
      if(ownerPost %in% c("up","both")) nests <- ids$up.firm == 1 | ids$up.firm == 3
      else if(ownerPost == "down")      nests <- ids$down.firm == 1 | ids$down.firm == 3
      else if (ownerPost == "vertical") nests <- ids$up.firm == 1 | ids$down.firm == 2
    }


    nests <- droplevels(factor(nests,levels=c(TRUE,FALSE)))

  }

  else if ( length(nests) != nrow(ids) | any(is.na(nest))){stop(" 'nests' length must be equal total number of products and cannot contain NAs")}

  if(nlevels(nests) >1 ){nestMat <- tcrossprod(model.matrix(~-1+nests))}
  else{nestMat <- matrix(1,nrow=nrow(ids),ncol=nrow(ids))}


  #nestParm  <- c(nestParm,0)
  nestParm  <- rep(nestParm,2)




  vertical=list()
  bargparm <- bargparm[interaction(ids$up.firm,ids$down.firm)]
  mcshare.up <- mcshare.up[interaction(ids$up.firm,ids$up.products)]
  mcshare.down <- mcshare.down[interaction(ids$down.firm,ids$down.products)]

  bargparm <- switch(bargpreset,
                     none=bargparm,
                     party=ifelse(isJoint,0.5,bargparm),
                     rival=ifelse(!isJoint,0.5,bargparm)
  )

  ## when OwnerPost equals "vertical"
  ## simulate a vertical merger between U1 and D1
  if(ownerPost  %in%  c("vertical") || nfirms.vert>0){

    if (nfirms.vert>0) vertFirms <-2:(nfirms.vert + 1)
    else{vertFirms=NULL}

    #if(!(ownerPost  %in%  c("vertical")) && min(nfirms.down,nfirms.up) >2 ) vertFirms <- vertFirms+1

    for (v in vertFirms){

      bargparm[ids$up.firm == v & ids$down.firm == v] <- 1
    }

    bargparmPost <- bargparm
    if(ownerPost  %in%  c("vertical")){ bargparmPost[1] <- 1 }# all bargaining power resides with the retailer in a vertical  merger
    else if(ownerPost  %in%  c("up")){ bargparmPost[ids$up.firm == 1 & ids$down.firm == 2] <- 1 } # post-merger, u1 gives d1 a discount
    else if(ownerPost  %in%  c("down")){ bargparmPost[ids$up.firm == 2 & ids$down.firm == 1] <- 1 } #post-merger u2 gives d1 a discount

    }

  sharesDown  <- as.vector(rdirichlet(shareParm)) # generate inside shares

  if(largeMerger){
    sharesDown <- sort(sharesDown, decreasing = TRUE)
    if (ownerPost =="down") {sharesDown <- sharesDown[order(ids$up.firm,ids$down.firm)]}  #assign first two upstream products largest share
  }
  sharesDown <- sharesDown * (1-shareOutDown)

  if(nlevels(ids$up.firm)>1){
    ownerPre.up <- model.matrix(~0+up.firm,data=ids)
    ownerPre.up <- tcrossprod(ownerPre.up)
  }
  else{ownerPre.up <- matrix(1,nrow=nrow(ids),ncol=nrow(ids))}

  if(nlevels(ids$down.firm)>1){
    ownerPre.down <- model.matrix(~0+down.firm,data=ids)
    ownerPre.down <- tcrossprod(ownerPre.down)
  }
  else{ownerPre.down <- matrix(1,nrow=nrow(ids),ncol=nrow(ids))}


  ownerPost.down <- ownerPre.down
  ownerPost.up <- ownerPre.up


  ## when ownerPost equals either "up" or "both",
  ## simulate a merger between  upstream firms 1 and 2
  if(ownerPost  %in%  c("up","both") ){

    ownerPost.up[ids$up.firm == 1 , ids$up.firm == 2] <- 1
    ownerPost.up[ids$up.firm == 2 , ids$up.firm == 1] <- 1

  }


  ## when ownerPost equals either "down" or "both",
  ## simulate a merger between  downstream firms 1 and 2
  if(ownerPost %in%  c("down","both") ){

    ownerPost.down[ids$down.firm == 1 , ids$down.firm == 2] <- 1
    ownerPost.down[ids$down.firm == 2 , ids$down.firm == 1] <- 1
  }



  ##ownership matrices for vertical mergers
  if(ownerPost  %in%  c("vertical") || nfirms.vert >0){



    ownerDownMatVerticalPre <- matrix(0,nrow=nprods,ncol=nprods)
    #ownerBargUpVertPre<- ownerPre.up

    for( v in vertFirms){

      ## integrated retailer bargains with other wholesalers
      vertrowsDown <-ids$up.firm  != v  & ids$down.firm  == v

      ownerPre.up[vertrowsDown, ids$up.firm == v] <- -(1-bargparm[vertrowsDown])/bargparm[vertrowsDown]
      ownerPost.up[vertrowsDown, ids$up.firm == v] <- -(1-bargparmPost[vertrowsDown])/bargparmPost[vertrowsDown]

      #ownerBargUpVertPre[vertrowsDown, ids$up.firm  == v] <- -(1-bargparm[vertrowsDown])/bargparm[vertrowsDown]
    }



    ownerVertPre.down  <-  ownerPre.down  * (1-bargparm)/bargparm
    ownerVertPost.down  <-  ownerPost.down  * (1-bargparmPost)/bargparmPost

    for( v in vertFirms){

      vertrowsUp <-  ids$up.firm == v  & ids$down.firm != v

      ## only change downstream matrix when firms are playing Bertrand
      #if(supply.down=="1st"){ownerDownMatVerticalPre[ids$down.firm  == v, vertrows] <- 1}
      ownerDownMatVerticalPre[ids$down.firm  == v, vertrowsUp] <- 1
      #ownerDownMatVertical[owner.down == v, !vertrows] <- 0


      ownerVertPre.down [vertrowsUp, ids$down.firm  == v] <- -1
      ownerVertPost.down[vertrowsUp, ids$down.firm  == v] <- -1
    }



    ownerDownMatVerticalPost <- ownerDownMatVerticalPre
    #ownerBargUpVertPost <-  ownerBargUpVertPre

    if(ownerPost  %in%  c("vertical")){
      vertrowsDown <- ids$up.firm != 1  & ids$down.firm == 1

    ## integrated retailer bargains with other wholesalers
    ownerPost.up[vertrowsDown, ids$up.firm == 1] <- -(1-bargparmPost[vertrowsDown])/bargparmPost[vertrowsDown]

    vertrowsUp <-  ids$up.firm == 1  & ids$down.firm != 1

    #ownerPostVertical.down <- matrix(0,nrow= nrow(ids),ncol = nrow(ids))
    ownerDownMatVerticalPost[ids$down.firm ==1, vertrowsUp] <- 1

    #ownerPostLambda.down <- ownerPost.down * (1-bargparmPost)/bargparmPost

    ## integrated wholesaler bargains with other retailers
    ownerVertPost.down[vertrowsUp, ids$down.firm == 1] <- -1

    vertFirms <- c(1,vertFirms)
    }
    else if(ownerPost  %in%  c("up")){

    vertrowsDown <- ids$up.firm != 1  & ids$down.firm == 2

    ## integrated retailer bargains with other wholesalers
    ownerPost.up[vertrowsDown, ids$up.firm == 1] <- -(1-bargparmPost[vertrowsDown])/bargparmPost[vertrowsDown]

    vertrowsUp <-  ids$up.firm == 1  & ids$down.firm != 2

    #ownerPostVertical.down <- matrix(0,nrow= nrow(ids),ncol = nrow(ids))
    ownerDownMatVerticalPost[ids$down.firm ==2, vertrowsUp] <- 1

    #ownerPostLambda.down <- ownerPost.down * (1-bargparmPost)/bargparmPost

    ## integrated wholesaler bargains with other retailers
    ownerVertPost.down[vertrowsUp, ids$down.firm == 2] <- -1
    }

    else if(ownerPost  %in%  c("down")){

    vertrowsDown <- ids$up.firm != 2  & ids$down.firm == 1

    ## integrated retailer bargains with other wholesalers
    ownerPost.up[vertrowsDown, ids$up.firm == 2] <- -(1-bargparmPost[vertrowsDown])/bargparmPost[vertrowsDown]

    vertrowsUp <-  ids$up.firm == 2  & ids$down.firm != 1

    #ownerPostVertical.down <- matrix(0,nrow= nrow(ids),ncol = nrow(ids))
    ownerDownMatVerticalPost[ids$down.firm ==1, vertrowsUp] <- 1

    #ownerPostLambda.down <- ownerPost.down * (1-bargparmPost)/bargparmPost

    ## integrated wholesaler bargains with other retailers
    ownerVertPost.down[vertrowsUp, ids$down.firm == 1] <- -1
    }


    #vertical$bargparmPre <- bargparm
    vertical$bargparmPost <- bargparmPost

    vertical$ownerDownMatPre <- ownerDownMatVerticalPre
    vertical$ownerDownMatPost <- ownerDownMatVerticalPost

    #vertical$ownerPost.down <- ownerPostVertical.down
    #vertical$ownerPostLambda.down <-  ownerPostLambda.down

    #vertical$ownerBargUpPre <- ownerBargUpVertPre
    #vertical$ownerBargUpPost <- ownerBargUpVertPost

    vertical$ownerVertPre.down <- ownerVertPre.down
    vertical$ownerVertPost.down <- ownerVertPost.down



    #vertical$ownerPostNoSupply.up <-  ids$up.firm == 1  & ids$down.firm != 1
    #vertical$ownerPostNoSupply.down <- ids$up.firm == 1  & ids$down.firm == 1


    vertical$vertFirms <- vertFirms


  }




  ## create a container to store upstream and downstream data
  mkt<-list(up=list(nfirms=nfirms.up,nproducts=nprods.up,ownerPre=ownerPre.up, ownerPost=ownerPost.up,
                    bargparm=bargparm,mcshare=mcshare.up),
            down=list(nfirms=nfirms.down,nproducts=nprods.down,shares=sharesDown,
                      shareOut=shareOutDown,M=M,outMargin=outMargin,ids=ids,isParty=isParty,mcshare=mcshare.down,
                      ownerPre=ownerPre.down,ownerPost=ownerPost.down, nests=nests,nestParm=nestParm, nestMat=nestMat),
            vertical = vertical)



  class(mkt)<-supply.down

  mkt<-simMarket(mkt,...)

  return(mkt)
}
calcMC <- function (mkt,...)  {
  UseMethod("calcMC", mkt)
}

calcMC.default <- function (mkt,...)  {


  # return constant marginal cost

  up <- mkt$up$mcParm
  down <- mkt$down$mcParm

  return(list(up=up,down=down))


}

calcMC.linprod <- function (mkt,share)  {

  # return linear marginal costs, product level
  M <- mkt$down$M
  up <- mkt$up$mcParm
  down <- mkt$down$mcParm

  return(list(up=share*M/up,down=share*M/down))

}

calcMC.quadprod <- function (mkt,share)  {

  # return linear marginal costs, product level
  M <- mkt$down$M
  up <- mkt$up$mcParm
  down <- mkt$down$mcParm

  return(list(up=(share*M)^2/up,down=(share*M)^2/down))

}

calcMC.quadlin <- function (mkt,share)  {

  # return linear marginal costs, product level
  M <- mkt$down$M
  up <- mkt$up$mcParm
  down <- mkt$down$mcParm

  return(list(up=(share*M)^2/up,down=(share*M)/down))

}


calcMC.linquad <- function (mkt,share)  {

  # return linear marginal costs, product level
  M <- mkt$down$M
  up <- mkt$up$mcParm
  down <- mkt$down$mcParm

  return(list(up=(share*M)/up,down=(share*M)^2/down))

}



calcMC.lincons <- function (mkt,share)  {

  # return linear marginal costs, product level
  M <- mkt$down$M
  up <- mkt$up$mcParm
  down <- mkt$down$mcParm

  return(list(up=(share*M)/up,down=down))

}



calcMC.conslin <- function (mkt,share)  {

  # return linear marginal costs, product level
  M <- mkt$down$M
  up <- mkt$up$mcParm
  down <- mkt$down$mcParm

  return(list(up=up,down=(share*M)/down))

}

calcMC.linfirm <- function (mkt,share)  {


  M <- mkt$down$M
  up <- mkt$up$mcParm
  down <- mkt$down$mcParm
  ownerUpPre <- mkt$up$ownerPre
  ownerDownPre<- mkt$down$ownerPre


    return(list(up=as.vector(ownerUpPre%*%share)*M/up,down=as.vector(ownerDownPre%*%share)*M/down))


}




## modify methods and functions to allow for upstream and downstream linear marginal costs
simMarket.1st <- function(mkt,cost_type=c("constant","linprod","quadprod","linquad","quadlin","lincons","conslin","linfirm"),...){

  cost_type <- match.arg(cost_type)

  v <- mkt$vertical

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

  nprods <- nrow(mkt$down$ids)

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

    elast.inv <- solve(ownerPre.down * elast.down)
    downMargin = - as.vector(elast.inv %*% sharesDown)  #compute downstream margins


    ## Use upstream FOC, diversions, and downstream margins  to solve for upstream margins

    #div <- tcrossprod(1/(1-sharesDown),sharesDown)
    #diag(div) <- -1

    div <- diversions(sharesDown,nests, nestParm,nestMat)


    if(length(v) > 0 ){

      upMarginPart <-  solve(ownerPre.up * div) %*% (v$ownerVertPre.down * div)
      upMargin <- solve(diag(nprods) + (upMarginPart %*% elast.inv %*%  (v$ownerDownMatPre * elast.down)))
      upMargin <- drop(upMargin %*% upMarginPart %*% downMargin)

      #print(upMargin)
      downMargin <-  drop(downMargin  - elast.inv %*% ( (v$ownerDownMatPre * elast.down) %*% upMargin ))
    }

    else{
      upMargin <-   drop(solve(ownerPre.up * div) %*% ( (1-bargparm)/bargparm *((ownerPre.down * div) %*% downMargin)))

       #print(upMargin)
      }

  upMC <- mcshare.up*upMargin
  upPrices <- upMargin + upMC

  ## Use downstream margin and upstream prices to solve for downstream prices
  downMC <- mcshare.down*downMargin
  downPrices <- upPrices + downMargin + downMC

  ##compute downstream mean valuations (outside good has mc of 0)
  meanvalDown=(1-nestParm[nests])*(log(sharesDown)-log(shareOutDown))-alpha*(downPrices - outMargin) + nestParm[nests]*(log(sharesBetween.down) - log(shareOutDown))


  if(cost_type=="constant"){mcParm <- list(up=upMC,down=downMC)}
  else if (cost_type=="linprod"){{mcParm <- list(up= sharesDown*M/upMC,down=sharesDown*M/downMC)}}
  else if (cost_type=="quadprod"){{mcParm <- list(up= (sharesDown*M)^2/upMC,down=(sharesDown*M)^2/downMC)}}
  else if (cost_type=="linquad"){{mcParm <- list(up= (sharesDown*M)/upMC,down=(sharesDown*M)^2/downMC)}}
  else if (cost_type=="quadlin"){{mcParm <- list(up= (sharesDown*M)^2/upMC,down=(sharesDown*M)/downMC)}}
  else if (cost_type=="linfirm"){{mcParm <- list(up= as.vector(ownerPre.up%*%sharesDown)*M/upMC,down=as.vector(ownerPre.down%*%sharesDown)*M/downMC)}}
  else if (cost_type=="lincons"){{mcParm <- list(up= (sharesDown*M)/upMC,down=downMC)}}
  else if (cost_type=="conslin"){{mcParm <- list(up= upMC,down=(sharesDown*M)/downMC)}}

  ## create a container to store upstream and downstream data
  mkt<-list(up=c(mkt$up,list(price=upPrices,margin=1-upMC/upPrices, mcParm = mcParm$up)),
            down=c(mkt$down,list(alpha=alpha,price=downPrices,
                                 margin=1-(upPrices + downMC)/downPrices, mcParm =mcParm$down, meanval=meanvalDown)),
            vertical = mkt$vertical
  )

  class(mkt)<-c("1st",cost_type)
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

    if(length(v)>0){


     ownerBargDown <- v$ownerVertPre.down[subset,subset]
     ownerDownMat <- v$ownerDownMatPre[subset,subset]

    }
  }


  else{

    ownerUp <- mkt$up$ownerPost[subset,subset]
    ownerDown <- mkt$down$ownerPost[subset,subset]

    if(length(v)>0){


      ownerBargDown <- v$ownerVertPost.down[subset,subset]
      ownerDownMat <- v$ownerDownMatPost[subset,subset]
      bargparm <-  mkt$vertical$bargparmPost[subset]

    }
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


  mc <- calcMC(mkt,share = shareCandDown)

  mc$up <- mc$up[subset]
  mc$down <- mc$down[subset]

  elastDiff.down <- -alpha*tcrossprod(shareCandDown)
  elastSame.down <- -alpha*tcrossprod(shareCandDown*(1 + nestParm[nests] /( 1-nestParm[nests] )*(1/shareCandBetween.down)),shareCandDown)
  elast <- elastSame.down*nestMat + elastDiff.down*(1-nestMat)
  diag(elast) <- alpha/(1-nestParm[nests])*shareCandDown*(1-nestParm[nests]*shareCandWithin.down - (1-nestParm[nests])*shareCandDown)
  elast.inv <- solve(ownerDown * elast)
  marginsDownCand = - as.vector(elast.inv %*% shareCandDown)  #compute downstream margins


  #div <- tcrossprod(1/(1-shareCandDown),shareCandDown)
  #diag(div) <- -1

  div <- diversions(shareCandDown, nests, nestParm,nestMat)



  if(length(v) > 0) {

    upMarginPart <-  solve(ownerUp * div) %*% (ownerBargDown * div)
    marginsUpCand <- solve(diag(nprods) + (upMarginPart %*% elast.inv %*%  (ownerDownMat * elast)))
    marginsUpCand <- drop(marginsUpCand %*% upMarginPart %*% marginsDownCand)


    marginsDownCand <-  drop(marginsDownCand - elast.inv %*% ( (ownerDownMat * elast) %*% marginsUpCand ))
    #upFOC <- (ownerUp * div) %*% (priceCandUp-mcUp) - (v$ownerPostLambda.down * div) %*% marginsDownCand
    #marginsUpCand <- as.vector((v$ownerPostLambda.down * div) %*% marginsDownCand)
    #marginsUpCand <- as.vector(solve(ownerUp  * div) %*% (ownerBargDown * div) %*% marginsDownCand)



    #upFOC <- as.vector((ownerUp * div) %*% (priceCandUp-mcUp)) - marginsUpCand
    upFOC <-  priceCandUp-mc$up - marginsUpCand
}
  else{

    #marginsUpCand <-    as.vector(solve(ownerUp * div) %*% ((1-bargparm)/bargparm *(( ownerDown * div) %*% marginsDownCand)))
    marginsUpCand <-    as.vector((( ownerDown * div) %*% marginsDownCand))

    upFOC<- as.vector(((ownerUp * div) %*% (priceCandUp-mc$up))  -  (1-bargparm)/bargparm * marginsUpCand)


  }


  #upFOC <-  priceCandUp-mcUp - marginsUpCand
  #upFOC <-  as.vector(bargparm *((ownerUp * div) %*% (priceCandUp-mcUp))) - (1-bargparm) * marginsUpCand
  downFOC <- priceCandDown - priceCandUp - mc$down - marginsDownCand


  if(level == "all"){thisFOC= c(downFOC,upFOC)}
  else if (level == "down"){thisFOC = downFOC}
  else if (level == "up"){thisFOC = upFOC}

return(thisFOC)
}




PS.default <- function(mkt,upPrices,downPrices, useEst = FALSE,market=FALSE,ratio=FALSE,party=FALSE){


  if(useEst){
    stop("not implemented")

  }




  nestParm  <- mkt$down$nestParm
  nests <- mkt$down$nests

  quantities<-calcQuantities(mkt,downPrices- mkt$down$outMargin)

  mc <- calcMC(mkt,quantities/mkt$down$M)

  mcDown <- mc$down
  mcUp <- mc$up

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



    priceFixed <- mkt$down$price
    mkt$down$M

    calcMonopolySurplus <- function(priceCand){

      priceFixed[prodIndex] <- priceCand #keep prices of products not included in HM fixed at premerger levels

      sharesCand          <- calcShares(mkt,priceFixed -  mkt$down$outMargin)

      mc <- calcMC(mkt,sharesCand)$down[prodIndex]

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








summary.1st <-function(mkt, market =FALSE, vertical = ifelse(length(mkt$vertical)>0 & mkt$vertical$vertFirms==1, TRUE,FALSE), useEst = FALSE,level = c("all","up","down")){


  if(useEst){
    mcDown = mkt$est$mcDown
    mcUp = mkt$est$mcUp
  }


  vertFirms <- mkt$vertical$vertFirms

  M <- mkt$down$M
  nestParm <- mkt$down$nestParm
  nests <- mkt$down$nests

  pre <- calcPrices(mkt,preMerger=TRUE, useEst = useEst)
  post <- calcPrices(mkt,preMerger=FALSE, useEst = useEst)
  shares.pre <- calcShares(mkt, pre$down - mkt$down$outMargin, useEst = useEst)
  shares.post <- calcShares(mkt, post$down - mkt$down$outMargin, useEst = useEst)
  shareInside = sum(shares.pre,na.rm=TRUE)

  thislevel <- ifelse(!isTRUE(all.equal(mkt$up$ownerPre,mkt$up$ownerPost)),"up","down")

  isVert <- mkt$down$ids$down.firm %in% vertFirms &
    as.numeric(mkt$down$ids$down.firm)==as.numeric(mkt$down$ids$up.firm)



  res <- data.frame(isVert=isVert,
                    upPricePre= pre$up,
                    upPricePost=post$up,
                    downPricePre= pre$down,
                    downPricePost=post$down,
                    shares.pre= shares.pre,
                    shares.post=shares.post,
                    idDown = mkt$down$ids$down.firm,
                    idUp=mkt$down$ids$up.firm,
                    alpha = mkt$down$alpha,
                    nestParm=mkt$down$nestParm[1],
                    mktElastPre = mkt$down$alpha * sum(pre$down*shares.pre/shareInside,na.rm=TRUE) * (1 - shareInside),
                    cv = CV(mkt, pre$down  , post$down ),
                    relmarginPre = PS(mkt,pre$up,pre$down,ratio=TRUE),
                    relmarginPartyPre = PS(mkt,pre$up,pre$down,party=TRUE,ratio = TRUE),
                    upPSPre = PS(mkt,pre$up,pre$down,market = FALSE)$up,
                    downPSPre = PS(mkt,pre$up,pre$down,market = FALSE)$down,
                    upPSPost = PS(mkt,post$up,post$down,market = FALSE)$up,
                    downPSPost = PS(mkt,post$up,post$down,market = FALSE)$down
                    )

  if(market){

    #res <- sum(post$down*shares.post,na.rm=TRUE) - sum(pre$down*shares.pre,na.rm=TRUE) #CV in 2nd score w/o efficiencies
    res <-  unique(res$cv)

  }

  return(res)
}



