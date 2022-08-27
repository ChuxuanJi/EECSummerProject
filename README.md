# Influence of chemical and warming stressors on freshwater microbial function

## Description

This directory contains R scripts and programs for **"Influence of chemical and warming stressors on freshwater microbial function"**, Chuxuan Ji's summer project of EEC MRes.

## Languages

R, Bash, Tex

## Dependencies

For some scripts in this directory, packages [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), [lmerTest](https://cran.r-project.org/web/packages/lmerTest/index.html), [ggpubr](https://cran.r-project.org/web/packages/ggpubr/index.html), [EMAtools](https://cran.r-project.org/web/packages/EMAtools/index.html), [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html), [data.table](https://cran.r-project.org/web/packages/data.table/index.html), [ggrepel](https://cran.r-project.org/web/packages/ggrepel/index.html), [RColorBrewer](https://cran.r-project.org/web/packages/RColorBrewer/index.html), [gridExtra](https://cran.r-project.org/web/packages/gridExtra/index.html), [gcookbook](https://cran.r-project.org/web/packages/gcookbook/index.html), [ggpattern](https://cran.r-project.org/web/packages/ggpattern/index.html), [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html), [MuMIn](https://cran.r-project.org/web/packages/MuMIn/index.html), [lattice](https://cran.r-project.org/web/packages/lattice/index.html), [BiocManager](https://cran.r-project.org/web/packages/BiocManager/index.html), [car](https://cran.r-project.org/web/packages/car/index.html) are required. 
Please run the following script in **R/RStudio** for package installation: 
```R
install.packages(c("ggplot2", "lmerTest", "ggpubr", "EMAtools", "dplyr", "data.table", "ggrepel", "RColorBrewer", "gridExtra", "gcookbook", "ggpattern", "reshape2", "RColorBrewer", "MuMIn", "lattice", "BiocManager", "car"))
```


[LaTeX](https://www.latex-project.org/) installation is also required. Please run following **bash** script in Linux Terminal for installation:
```bash
sudo apt install texlive-full texlive-fonts-recommended texlive-pictures texlive-latex-extra imagemagick
```
## Installation

To use scripts in this directory clone the repository and run.

```bash
git clone https://github.com/ChuxuanJi/EECSummerProject.git/
```

## Project structure and Usage 

### Code:

- **3Year_Barplots.R:** An R script for plotting 2010-2022 ATP, MicroResp, leaf decomposition barplots.

- **ATP2022_barplot.R:** An R script for plotting 2022 ATP barplots.

- **BOD2022_barplot.R:** An R script for plotting 2022 BOD barplots.

- **FlowCytometry_cellcount.R:** An R script for calculating cell counts in water samples and plotting cell count plots.

- **LinearMixedModel2022.R:** An R script for fitting linear mixed models of 2022 ATP, MicroResp, BOD and decomposition.

- **LinearMixedModel_3Year.R:** An R script for fitting linear mixed models of 2019-2022 ATP, MicroResp and leaf decomposition.

- **Mesocosm20192022_TemperaturePlots.R:** An R script for plotting 2019-2022 mean month temperature plots.

- **Mesocosm_TemperaturePlots.R:** An R script for plotting 2019-2022 mean daily temperature plots.

- **MicroResp_2022_barplot.R:** An R script for plotting 2022 MicroResp barplots.

- **Regression2022_plot.R:** An R script for plotting regression models of 2022 ATP, MicroResp, BOD and decomposition.

- **TeaBag2022_2022_barplot.R:** An R script for plotting 2022 tea bag decomposition barplots.


<br/>

### Data: 

- **2022Data:** All data about ATP, MicroResp, enzyme, BOD, flow cytometer and decomposition in 2022.

- **ThreeYearData:** All data about ATP, MicroResp and leaf decomposition in 2019-2022.

- **Mesocosm Temperature:** All mesocosm temperature data from 2019.1 to 2022.6.


<br/>


### Sandbox: 

- **Enzymes:** Summary of 2022 enzyme assays data.

- **Wettex:** Summary of 2022 Wettex decomposition data.

<br/>


## Author name and contact

Name: Chuxuan Ji

Email: chuxuan.ji21@imperial.ac.uk