# Beyond Horizontal and Vertical: Measuring the Welfare Effects of Complex Integration
## Project Status: JEMS Submission Pivot

### **Strategic Objective**
Refurbish the paper for submission to *Journal of Economics & Management Strategy (JEMS)*.
*   **Pivot:** Shift from a purely simulation-based paper to a theoretical contribution ("The Impossibility of Symmetric Efficiency") supported by empirical simulations.
*   **Key Insight:** In complex mergers, the legacy integrated product faces strictly positive upward pricing pressure (Proposition 1), exploring the "Zone of Harm" where this outweighs the efficiency gains of the acquired product.

### **Current State**
1.  **Theory Section (`VerticalArrangements.tex`):**
    *   **Completed.** Replaced the detailed Nash Bargaining derivation with a streamlined "Asymmetric Pricing Pressure" framework.
    *   Key components: `theory_revision.tex` (new theory text) and `VerticalArrangements_backup.tex` (archived original).

2.  **Codebase (`d:/Projects/beyond/code`):**
    *   **`RunSims.R`:** **READY**. Configured to run the main simulation grid (HHI vs. Market Share). Pathing updated to `beyond` directory.
    *   **`TrashSims.R`:** **READY**. Generates the specific case study figures (`Trash...` pngs). Pathing updated.
    *   **`slurm/Batch...R`:** **READY**. Scripts for large-scale cluster runs (`BatchPrepHHI.R`, `BatchSims.R`, `BatchAssemble.R`).

### **Next Steps for Agents/User**
1.  **Run Simulations (Critical for Figures):**
    *   Execute `RunSims.R` to generate the heatmap data.
    *   Execute `TrashSims.R` to generate the case study figures (`TrashSimsFirm.png`, `trashelast.png`, `trashnest.png`, `trashbarg.png`). *These are currently missing and causing LaTeX warnings.*
2.  **Generate Figures:**
    *   Use the output from `RunSims.R` to create the "Zone of Harm" heatmap.
3.  **Finalize Document:**
    *   Recompile `VerticalArrangements.tex` once figures are generated in `doc/output/`.
    *   Verify the Appendix contains the necessary mathematical proofs (moved from main text).

### **Notes for Handover**
*   **R Environment:** The necessary packages (`bayesm`, `antitrust`, `dplyr`, `ggplot2`) should be installed in the global R library, not locally in the `code` folder.
*   **Paths:** All scripts assume `basedir <- file.path(Sys.getenv("HOME"), "Projects", "beyond")`. Ensure this matches the execution environment.
