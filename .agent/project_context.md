# Project Context: Beyond "Horizontal" and "Vertical"

## Overview
This project investigates the welfare effects of "complex" mergers—those involving both horizontal and vertical aspects—using a simulation model. It specifically focuses on the solid waste management industry, applying the framework to the 2021 Republic/Santek merger in Chattanooga, TN.

## Key Goals
- Analyze the welfare effects of complex integration (mergers with horizontal and vertical components).
- Compare "complex" mergers to purely horizontal or vertical ones.
- Simulate the Republic/Santek merger to estimate consumer harm vs. efficiency gains (Elimination of Double Marginalization - EDM).

## Repository Structure
- `code/`: R scripts for simulations.
    - `RunSims.R`: General simulation script. **Note: It attempts to load `./data` and `MonteCarlo.R` which implies conflicting working directories. The script expects to be in `code` to find `MonteCarlo.R`, but `data` is in the project root.**
    - `TrashSims.R`: Specific simulation for the Republic/Santek merger (Chattanooga application).
    - `MonteCarlo.R`: Core simulation logic (sourced by RunSims.R).
    - `slurm/`: Scripts for batch processing on a cluster. **Note: `BatchSims.R` contains hardcoded paths to `~/Projects/bargaining_convex/data/batch` which may not match the local environment.**
- `doc/`: Documentation and paper drafts.
    - `VerticalArrangements.tex`: The main research paper.
- `data/`: Data inputs for the simulations.
- `output/`: Generated paths and figures.

## Key Technologies
- **Language**: R
- **Libraries**: `bayesm`, `dplyr`, `antitrust` (custom/specific package), `ggplot2`.
- **Methodology**: Bertrand competition downstream, Nash Bargaining upstream.

## Known Issues
- **Path Resolution**: Scripts like `RunSims.R` and `BatchSims.R` have hardcoded relative or absolute paths that may not match the current folder structure (`d:\Projects\beyond`).
  - `RunSims.R` defines `simpath <- "./data"`, but if run from `code` (required for `MonteCarlo.R`), it should likely be `../data`.
  - `BatchSims.R` references `~/Projects/bargaining_convex/data/batch`.
