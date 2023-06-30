# BevSzen

*Population scenarios for the city of Zurich*

This [R](https://www.r-project.org/) code is used to generate various population development scenarios for the city of Zurich, Switzerland. The underlying models are based on official population and residential statistics as well as capacity and construction reserve calculations. Documentation can be found [here](5_Documentation). [Quarto](https://quarto.org/) is used to generate output plots in the form of a HTML book.

All necessary data is [Open Data](https://data.stadt-zuerich.ch/) except the projects as well as the capacity and reserves data.

## Renv

### To users:

We use a [`renv`](https://rstudio.github.io/renv/articles/renv.html) enabled RStudio project to manage the local environment with libraries in order to ensure reproducible results, correct working directories etc. You can use the included one or [create your own](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects).\
When you pull this repository, make sure to restart your R session (this is done automatically if you open this as an RStudio project). R will read the project specific `.Rprofile` file and thereby activate `renv`.\
You can also activate it manually by using `renv::activate()`. This might lead to the download of many (potentially older) packages.\
Keep in mind that you will be working in this specific environment as long as you're not switching to another project or manually deactivate renv (`renv::deactivate()`).

### To contributors:

If a new package is required, use `renv::install("new package")`. For updates, use `renv::update("update_package")`.\
Then add this to the lock file by running `renv::snapshot()` and commit the changes in renv.lock. A good overview can be found [here](https://rstudio.github.io/renv/articles/renv.html)

## Usage

### To run the population scenarios:

Source [0001_model_control_flow.r](1_Code/0000_General) to run the whole model and create plots.

This runs `init()` from [general_init.R](1_Code/0000_General) to source all necessary functions and initialize variables. Then it runs `render_book()` to run the whole model and create output plots as a Quarto HTML document.

You may want to only run the model (or parts thereof) alone without plotting by running `run_scen()` with the desired modules.

How to execute the entire model incl. plotting:

``` r
render_book(cache_refresh = TRUE)
```

Example: future mortlity rates ("dea" for death), middle scenario

``` r
run_scen( scenarios = "middle", modules = "dea")
```

**Caveat**: Projects and Capacity and reserves data is currently not publicly available and therefore the respective modules cannot be run. We are working on the open data availability of the respective datasets and hope to be able to provide them soon.

[variables.yml](2_Data/3_Parameter) defines all paths, labels etc. and may be adjusted to your liking. The file [parameter.csv](2_Data/3_Parameter) contains all valid parameters for the scenario like thresholds etc.

A log file is being created at [2_Data/6_Log](2_Data/6_Log).

## Documentation:

[Documentation (german)](https://www.stadt-zuerich.ch/prd/de/index/statistik/themen/bevoelkerung/bevoelkerungsentwicklung/bevoelkerungsszenarien.html#dokumentation)

## Author:

[Statistik Stadt Zürich](mailto:statistik@zuerich.ch)
