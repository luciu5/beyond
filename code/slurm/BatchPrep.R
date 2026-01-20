rm(list=ls())
library(bayesm,quietly = TRUE)
library(dplyr,quietly = TRUE)
library(tidyr,quietly = TRUE)
library(compiler,quietly=TRUE)
library(readr,quietly=TRUE)
library(rando)
cmpfile("./code/MonteCarlo.R")


chunk=2
maxcores=300


batchdir <- "./data/batch"
simpath <- "./data"
file.remove(file.path(".","code","bargaining_convex_slurm.out"))
files_to_delete <- dir(path=batchdir ,pattern="*.csv")
file.remove(file.path(batchdir, files_to_delete))





samples <- 5e2 #1e3
ndfirms <- seq(1,15,2)
nufirms <- ndfirms
#nvfirms <- unique(c(0,ndfirms-1))
nvfirms <- c(0,1,2,3)
nestParm <- c(0)
type=c("1st")
merger=c("up","down", "vertical","both")
bargparm <- seq(.1,.9,.1)
assym <- c("none"#,"diag1","diag0"
)
mc=c("constant"#,"linprod","lincons","conslin","consparty","linparty"#, "quadprod","linquad","quadlin","linfirm"
)
multiplier=1:1 # run nMergers over multiplier number of CPUS
res.nests_plan <- expand_grid(Grp=multiplier,samples,
                              up=ndfirms,
                              down =nufirms ,
                              vert=nvfirms,
                              nestParm=nestParm,
                              barg = bargparm,
                              type=type,
                              merger=merger,
                              mc=mc,
                              preset=assym)

#res.nests_plan <- inner_join(res.nests_plan,landfill_eff)
#res.nests <- (filter(res.nests, down==1))
res.nests_plan <- mutate(res.nests_plan, type=as.character(type),
                         merger=as.character(merger)) %>%
  filter(!(merger %in% c("up") & (up ==1))) %>%
  filter(!(merger %in% c("down") & (down ==1))) %>%
  filter(vert < up & vert<down) %>%
  filter(7 >= up | 7 >= down) %>%
  filter(!((down == 1 & up ==1))) %>%
  filter(!(preset %in% c("diag1","diag0") & mc !="constant"))

res.nests_plan <- mutate(res.nests_plan,syseed=replicate(n(),gen_seed()))


write_csv(res.nests_plan,file="./data/simjobs.csv")


## request up to maxcores, but no more than needed to run
cores=ceiling(min(nrow(res.nests_plan)/chunk,maxcores))



## write source code for file

cat(file="./code/runsims.sh",

sprintf('#!/bin/bash
#SBATCH --job-name=bargaining_convex
#SBATCH --array=1-%s%s
#SBATCH --ntasks=1
#SBATCH --mem=1000m
#SBATCH	--mail-type=begin
#SBATCH	--mail-type=end
#SBATCH	--mail-type=fail
#SBATCH	--mail-user=charles.s.taragin@frb.gov
#SBATCH -o bargaining_convex_slurm.out
#SBATCH --open-mode=append

rm ./slurm/output_log.txt
cd `pathf $SLURM_SUBMIT_DIR`


#Set the number of runs that each SLURM task should do
PER_TASK=%s

# Calculate the starting and ending values for this task based
# on the SLURM task and the number of runs per task.
START_NUM=$(( ($SLURM_ARRAY_TASK_ID - 1) * $PER_TASK + 1 ))
END_NUM=$(( $SLURM_ARRAY_TASK_ID * $PER_TASK ))
header=$(head -1  ../data/simjobs.csv)
nlines=$(wc -l  < ../data/simjobs.csv)

# Print the task and run range
echo This is task $SLURM_ARRAY_TASK_ID, which will do runs $START_NUM to $END_NUM

# Run the loop of runs for this task.
for (( run=$START_NUM; run<=$END_NUM; run++ )); do
  lineno=$(($run + 1))
  thisrun=$(head -n $lineno ../data/simjobs.csv | tail -n 1)
  if  [[ -z "$thisrun" || $lineno -gt $nlines ]];
  then
    break;
  fi
  echo This is SLURM task $SLURM_ARRAY_TASK_ID, run number $run >> ./slurm/output_log.txt

#echo "$lineno: $thisrun"
srun Rscript ${HOME}/Projects/bargaining_convex/code/slurm/BatchSims.R "$header" "$thisrun" "$lineno"
done
',
        plyr::round_any(nrow(res.nests_plan)/chunk,chunk,f=ceiling),paste0("%",cores),chunk
        )
)
