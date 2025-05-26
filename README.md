# StormBase: An R Package for Identification of Water Input (Rainfall)–Runoff Events

**StormBase**  is an R package designed to objectively identify rainfall (or other liquid water input)–runoff events. It supports event detection and automatic baseflow separation, making it a valuable tool for hydrologists and water resources engineers.

The core algorithm was developed by Giani et al. (2022) in MATLAB using the Detrending Moving-average Cross-Correlation Analysis (DMCA). Key innovations of this method include:

* No need to manually separate baseflow from streamflow, avoiding subjective parameter choices typical of digital filters or peak-over-threshold methods.

* A wide temporal window is used to search for catchment response times, allowing for objective attribution of rainfall (input) and corresponding stormflow (output) events.

* Events are not limited to single peaks — the algorithm identifies event cores and determines the start and end of each input and output event.

For more technical details regarding this method, please refer to Giani et al. (2020, 2022). The original MATLAB code was translated into efficient R functions and packaged to ensure broader accessibility and usability within the open-source R ecosystem. This algorithm can have a major contribution for flood studies and engineering designs for stormwater management. For instance, Sharif & Ameli (2025) demonstrated that many rain-dominated, flood-prone catchments exhibit linear time-invariant (LTI) behavior in stormflow generation, questioning the necessity of complex models to estimate stormflow volumes.

---

## Installation

As the package is not on CRAN, you can install the package with `devtools` package.

``` r 
# If you don't have devtools, run the below line first:
# install.packages("devtools")
devtools::install_github("h-sharif/StormBase",
                         build_vignettes = TRUE)
```

## 🚀 Quick Start

Load the package and explore a sample dataset:
```r
library(StormBase)

# Sample data
head(test_catchment)
```
### Baseflow Extraction

```r
baseflow <- baseflow_extractor(
  water_input = test_catchment$precipitation_mmd,
  flow = test_catchment$streamflow_mmd,
  is_daily = TRUE
)

plot(baseflow, type = "l", ylab = "Baseflow (mm/day)",
     xlab = "Time")
```
### Rainfall–Runoff Event Identification

```r
events_df <- event_identifier(
  datetime = test_catchment$Date,
  water_input = test_catchment$precipitation_mmd,
  flow = test_catchment$streamflow_mmd, 
  # Bellow are default values (more details are in ?event_identifier)
  is_daily = TRUE, # running with daily data
  Rmin = 0.02, 
  Lmax = 100,
  baseflow_flag = TRUE, # provided flow is the total streamflow, so it asks to
  # calculate and subtract baseflow values for runoff volume and runoff ratio
  # calculations
  exclude_na = TRUE # it removes events with missed streamflow values since
  # those values are treated as 0 for volume calculations (na.rm = TRUE).
)

head(events)
```

## 📁 Example Dataset

StormBase includes an example dataset: `test_catchment`.

```r
data("test_catchment")
str(test_catchment)
```

## 📄 Vignette

A full worked example is provided in the package vignette.

**In R:**

```r
vignette("study_case", package = "StormBase")
```

**Online (via GitHub Pages)**: [Study Case Vignette (HTML)](https://h-sharif.github.io/StormBase/study_case.html)

## 📚 Documentation

Use the help system in R:

```r
?baseflow_extractor
?event_identifier
```

## 🤝 Contributions
Contributions are welcome! Feel free to:

* Report bugs or request features via GitHub Issues
* Submit pull requests for bug fixes, new features, or improvements
* Improve documentation or add new examples