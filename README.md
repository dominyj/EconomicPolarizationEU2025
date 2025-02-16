# Economic Polarization in the European Union

## Overview
This project reproduces the analysis and graphics from the paper:
**"Economic Polarization in the European Union: Development Models in the Race for the Best Location"** (2025) by Jonas Dominy, Claudius Gräbner-Radkowitsch, Philipp Heimberger, and Jakob Kapeller.

The study investigates economic polarization in the EU by analyzing income divergence and the development of heterogeneous economic models. It classifies EU economies into core, periphery, workbench economies, and financial hubs.

## Installation
To use this project, first clone the repository and restore the required R packages using `renv`:

```r
renv::restore()
```

If `renv` is not installed, you can install it with:

```r
install.packages("renv")
```

## Usage
Run the following script in R to reproduce all figures from the paper:

```r
source("00_run_analysis.R")
```

### Data
- **`data_original/`** contains the original datasets to avoid overwriting.
- **`data/`** is used for analysis.

## Authors
- **Jonas Dominy** 
- [**Claudius Gräbner-Radkowitsch**](https://claudius-graebner.com/)
- [**Philipp Heimberger**](https://wiiw.ac.at/philipp-heimberger-s-1138.html)
- [**Jakob Kapeller**](https://jakob-kapeller.org/)


