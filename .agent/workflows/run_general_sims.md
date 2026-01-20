---
description: Run the general Monte Carlo market simulations
---

1. Open the terminal.
2. Navigate to the code directory: `cd code`
3. Run the simulation script: `Rscript RunSims.R`

Note: This script may take a significant amount of time as it utilizes parallel processing (`makeCluster`). It generates `SimsNests.RData` in the data directory and a summary table in `doc/sumtable.tex`.
