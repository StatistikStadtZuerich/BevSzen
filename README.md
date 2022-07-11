# BevSzen

*Population scenarios for the city of Zurich*

This [R](https://www.r-project.org/) code is used to generate various population development scenarios for the city of Zurich, Switzerland. The underlying models are based on official population and residential statistics as well as capacity and construction reserve calculations. Documentation can be found [here](5_Documentation).

All necessary data is [Open Data](https://data.stadt-zuerich.ch/) except the projects as well as the capacity and reserves data.

## Usage:

It is best to use an RStudio project (use included or [create your own](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects)) to ensure correct working directories etc.

Source [0001_model_control_flow.r](1_Code/0000_General) and run `run_scen()` with the desired modules.

Example: future mortlity rates ("dea" for death), middle scenario

``` r
run_scen( scenarios = "middle", modules = "dea")
```

How to execute the entire model:

``` r
run_scen( scenarios = c("lower", "middle", "upper"), modules = c("all"))
```

**Caveat**: Projects and Capacity and reserves data is currently not publicly available and therefore the respective modules cannot be run. We are working on the open data availability of the respective datasets and hope to be able to provide them soon.

## Documentation:

[Documentation (german)](https://www.stadt-zuerich.ch/prd/de/index/statistik/themen/bevoelkerung/bevoelkerungsentwicklung/bevoelkerungsszenarien.html#dokumentation)

## Author:

[Statistik Stadt Zürich](mailto:statistik@zuerich.ch)
