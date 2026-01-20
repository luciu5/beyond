rm(list = ls())
# suppress library warnings and maskings
# options(conflicts.policy = list(error = FALSE, warn = FALSE))
library(bayesm, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(compiler, quietly = TRUE)
library(readr, quietly = TRUE)
library(data.table, quietly = TRUE)

## suppress dplyr info
options(dplyr.summarise.inform = FALSE)

# get command line arguments
args <- commandArgs(trailingOnly = TRUE)


lineno <- as.numeric(args[3])
thisrun <- read.csv(text = args[1:2])


basedir <- file.path(Sys.getenv("HOME"), "Projects", "beyond")
loadcmp(file.path(basedir, "code", "MonteCarlo.Rc"))
simpath <- file.path(basedir, "data", "batch")

# Get SLURM info, task id to index saved results and set seed
# task_id <- Sys.getenv("SLURM_ARRAY_TASK_ID") %>% as.numeric()
# task_id=1
## set seed

set.seed(3141 * 100 + as.numeric(thisrun$syseed))


genMkts <- function(x, y, z, h, t, m, n, b, c, a, large = FALSE) {
  thismkt <- try(
    market(
      supply.down = t,
      nfirms.down = y,
      nfirms.up = x,
      nfirms.vert = as.numeric(as.character(z)),
      hhidelta = h,
      outMargin = runif(1, 2, 20),
      # outMargin=5,
      nestParm = n,
      shareOutDown = 0.15,
      mcshare.up = rep(0.25, x * y),
      mcshare.down = rep(.25, x * y),
      bargparm = rep(b, x * y),
      ownerPost = m,
      M = 1, cost_type = as.character(c),
      largeMerger = large,
      preference = TRUE,
      bargpreset = as.character(a)
    ),
    silent = TRUE
  )


  if (any(class(thismkt) == "try-error")) {
    return(list(
      summary = thismkt,
      res = thismkt
    ))
  }

  vertFirms <- thismkt$vertical$vertFirms
  isVert <- length(vertFirms) > 0


  shareOutDown <- thismkt$down$shareOut
  M <- thismkt$down$M
  outMargin <- thismkt$down$outMargin
  # thismkt <- calcSlopes(thismkt,constrain="global");

  isNegMCUp <- any(thismkt$up$mc < 0)
  isNegMCDown <- any(thismkt$ip$mc < 0)

  mcParmUp <- thismkt$up$mcParm
  mcParmDown <- thismkt$down$mcParm

  thissum <- try(summary(thismkt, market = FALSE), silent = TRUE)
  if (class(thissum) == "try-error") {
    return(list(
      summary = thissum,
      res = thissum
    ))
  }
  isMarket <- try(HypoMonTest(thismkt), silent = TRUE)
  if (class(isMarket) == "try-error") isMarket <- NA
  thissum$idVert <- thissum$idDown
  thissum$idVert[thissum$idUp == 1] <- 1

  # for(v in vertFirms){
  #   thissum$idVert[thissum$idUp == v] <- v
  # }

  sumshares.pre <- sum(thissum$shares.pre, na.rm = TRUE)
  sumshares.post <- sum(thissum$shares.post, na.rm = TRUE)

  mktrev.pre <- sum(thissum$downPricePre * thissum$shares.pre, na.rm = TRUE) * M

  thisres <- with(thissum, data.frame(
    type = t,
    down = y,
    up = x,
    vert = z,
    hhd = h,
    nestParm = n,
    merger = m,
    barg = b,
    preset = a,
    mc = c,
    M = M,
    outMargin = outMargin,
    mktElast = mktElastPre[1],
    cv = cv[1],
    isMarket = isMarket,
    mktrev.pre = mktrev.pre,
    relmarginPre = relmarginPre[1],
    relmarginPartyPre = relmarginPartyPre[1],
    upPSPre = sum(upPSPre, na.rm = TRUE),
    upPSPost = sum(upPSPost, na.rm = TRUE),
    downPSPre = sum(downPSPre, na.rm = TRUE),
    downPSPost = sum(downPSPost, na.rm = TRUE),
    isProfitable.up = ifelse(isVert,
      sum(upPSPost[idUp %in% 1:2], downPSPost[idDown %in% 2], -upPSPre[idUp %in% 1:2], -downPSPre[idDown %in% 2], na.rm = TRUE) > 0,
      sum(upPSPost[idUp %in% 1:2], -upPSPre[idUp %in% 1:2], na.rm = TRUE) > 0
    ),
    isProfitable.down = ifelse(isVert,
      sum(upPSPost[idUp %in% 2], downPSPost[idDown %in% 1:2], -upPSPre[idUp %in% 2], -downPSPre[idDown %in% 1:2], na.rm = TRUE) > 0,
      sum(downPSPost[idDown %in% 1:2], -downPSPre[idDown %in% 1:2], na.rm = TRUE) > 0
    ),
    isProfitable.vert = sum(downPSPost[idDown == 1], upPSPost[idUp == 1], -downPSPre[idDown == 1], -upPSPre[idUp == 1], na.rm = TRUE) > 0,
    isProfitable.both = sum(downPSPost[idDown %in% 1:2], upPSPost[idUp %in% 1:2], -downPSPre[idDown %in% 1:2], -upPSPre[idUp %in% 1:2], na.rm = TRUE) > 0,
    avgpricepre.up = sum(thissum$upPricePre * shares.pre / sumshares.pre, na.rm = TRUE),
    avgpricepre.down = sum(thissum$downPricePre * shares.pre / sumshares.pre, na.rm = TRUE),
    # avgPartyCostParmUp.up = weighted.mean(mcParmUp[idUp %in% 1:2],shares.pre[idUp %in% 1:2] ),
    # avgPartyCostParmDown.down = weighted.mean(mcParmDown[idDown %in% 1:2],shares.pre[idDown %in% 1:2]),
    # avgPartyCostParmUp.vert = weighted.mean(mcParmUp[idUp ==1],shares.pre[idUp == 1]),
    # avgPartyCostParmDown.vert = weighted.mean(mcParmDown[idDown ==1],shares.pre[idDown == 1]),
    # avgpricedelta = sum(thissum$downPricePost*shares.post,na.rm=TRUE) + outMargin*(1-sumshares.post ) - sum(thissum$downPricePre*shares.pre/sumshares.pre,na.rm=TRUE) - outMargin*(1-sumshares.pre ),
    avgpricedelta = sum(thissum$downPricePost * shares.post / sumshares.post, na.rm = TRUE) - sum(thissum$downPricePre * shares.pre / sumshares.pre, na.rm = TRUE),
    isNegMCUp = isNegMCUp,
    isNegMCDown = isNegMCDown,
    hhidelta.down = 2 * prod(tapply(shares.pre / sumshares.pre * 100, idDown, sum, na.rm = TRUE)[1:2]),
    hhipre.down = sum(tapply(shares.pre / sumshares.pre * 100, idDown, sum)^2, na.rm = TRUE),
    hhidelta.up = 2 * prod(tapply(shares.pre / sumshares.pre * 100, idUp, sum, na.rm = TRUE)[1:2]),
    hhipre.up = sum(tapply(shares.pre / sumshares.pre * 100, idUp, sum)^2, na.rm = TRUE),
    hhipost.vert = sum(tapply(shares.pre / sumshares.pre * 100, idVert, sum)^2, na.rm = TRUE)
  ))

  thisres$hhidelta <- with(thisres, ifelse(merger == "up", hhidelta.up,
    ifelse(merger == "vertical", hhipost.vert - hhipre.down, hhidelta.down)
  ))
  thisres$hhipre <- with(thisres, ifelse(merger != "up", hhipre.down, hhipre.up))
  thisres$hhipost <- with(thisres, ifelse(merger == "vertical", hhipost.vert, hhipre + hhidelta))
  # thisres$hhipost.vert <-thisres$hhidelta.up <- thisres$hhidelta.down <- thisres$hhipre.up <- thisres$hhipre.down <- NULL

  return(list(
    # mkt=thismkt,
    summary = thissum,
    res = unique(thisres)
  ))
}

repMkts <- function(x, y, z, h, t, m, n, b, c, a, large = FALSE) {
  replicate(thisrun$samples, genMkts(x, y, z, h, t, m, n, b, c, a, large), simplify = "list")
}


results <- repMkts(
  thisrun$up, thisrun$down, thisrun$vert, thisrun$hhidelta, thisrun$type, thisrun$merger, thisrun$nestParm,
  thisrun$barg, thisrun$mc, thisrun$preset
)

colnames(results) <- 1:thisrun$samples

resMkt <- lapply(results["res", ], function(x) {
  x <- x[!sapply(x, class) %in% "try-error"]
})

resFirm <- lapply(results["summary", ], function(x) {
  x <- x[!sapply(x, class) %in% "try-error"]
})


resMkt <- data.table::rbindlist(resMkt, idcol = "sample")
resFirm <- data.table::rbindlist(resFirm, idcol = "sample")

resMkt <- mutate(resMkt,
  up = ifelse(merger == "both", up + 2, up),
  down = ifelse(merger == "both", down + 2, down),
  up = factor(up),
  down = factor(down),
  # vert=ifelse(merger=="vertical",vert+1,vert),
  vert = ifelse(merger == "both", vert + 2, vert),
  vert = factor(vert), nestParm = factor(nestParm),
  hhd = factor(hhd),
  preset = factor(preset),
  relleveragePre = (1 - barg) / barg, barg = factor(barg),
  type = ifelse(type == "1st", "Bertrand", "2nd"), type = factor(type, levels = c("Bertrand", "2nd")),
  Retailers = down
)


resFirm$Grp <- resMkt$Grp <- thisrun$Grp


write_csv(resMkt, file = file.path(simpath, paste0("ResMkt", "_", lineno, ".csv")), na = "")
write_csv(resFirm, file = file.path(simpath, paste0("ResFirm", "_", lineno, ".csv")), na = "")
