# BevSzen

*Population scenarios for the city of Zurich*

This [R](https://www.r-project.org/) code is used to generate various population development scenarios for the city of Zurich, Switzerland. The underlying models are based on official population and residential statistics as well as capacity and construction reserve calculations. Documentation can be found [here](5_Documentation).

All necessary data is [Open Data](https://data.stadt-zuerich.ch/) except the capacity and reserves data.

## Usage:

Source [0001_model_control_flow.r](1_Code/0000_General) and run `run_scen()` with the desired modules. E.g.:

``` r
run_scen( scenarios = c("lower", "middle", "upper"), modules = c("all"))
```

**Caveat**: Capacity and reserves data is currently not available and the respective modules cannot be run. We are working on the open data availability of the respective dataset and hope to be able to provide it soon.

## Author:

[Statistik Stadt Zürich](mailto:statistik@zuerich.ch)
