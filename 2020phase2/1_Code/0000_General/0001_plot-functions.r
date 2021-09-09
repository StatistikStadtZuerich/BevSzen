#-------------------------------------------------------------------
#generic plot function
#
# Kind of a ggplot wrapper to obtain similar and uniform plots
# with less code
#
#bad/rok, July 2021
#-------------------------------------------------------------------
#
# @params:
# data: main tibble
# geom: a char vector with the names of the desired geom. Default is geom_line.
#       if "", then geom must be defined in quotes parameter (see below)
# aes_x: the grouping variable for x as string
# aes_y: the grouping variable for y as string
# aes_col: the grouping variable for colour as string.
#          Is converted to a factor if it is continuous
# aes_ltyp: the grouping variable for linetype as string
# aes_alpha: the alpha value for a geom as string
# aes_size: the size value for a geom as string
# labs_x, labs_y, labs_col, labs_ltyp, labs_alpha, labs_size:
#         labels for the respective variables
# i_x: vector of vertical intercepts on x axis
# i_y: vector of horizontal intercepts on y axis
# scale_x: set the x scale. Default ist pretty_breaks for continuous scale
# scale_y: set the y scale. Default is none. "pretty" for pretty_breaks, "log"
#          for a log10 transformation, a vector for limits.
#          Achieve "expand_limits(y = 0)" with c(0, NA)
# breaks: vector with breaks for scale if desired.
# fix_col: desired length of a colour vector selected from predefined colours
#          is ignored (and therefore unnecessary) if aes_col is set
# fix_size: desired size of geom (i.e. of line or points)
#          is ignored (and therefore unnecessary) if aes_size is set
# grid: if set, creates a facet grid with grid[1] on LHS
#       and grid[2] on RHs of formula
# gridscale: use to define the scale attribute in facet_grid
# wrap: if set, creates a facet wrap with this value on RHs of formula
# ncol: argument cols for facet_grid, ncol for facet_wrap
# title: title for the whole chart, as object or string
#        (esp. in case of multipage plots)
# name: if set, stores graphics under this name. otherwise to graphics device
# file_type: select file type for storing. Default is pdf
# width: width of output file
# height: height of output file
# multi: apply the plot on this list recursively and
#         combine the plots on multipage document
# multif: run the plot recursively with this expression in tidy format
#         (usually a filter, e.g.: "filter(year == x)".
#         Defaults for uni_d/uni_o in multi are handled and can be omitted here
# quotes: this argument can be used to hand over any quoted ggplot arguments
#         (e.g. additional geoms)
#         if used, make sure that an eventually colliding parameter is set to ""
#         can be used for a single quote ("quote(call)") or multiple in form
#         of a list ("c(quote(call1)), quote(call2))")
# quotes_top: same as quotes but is inserted at the very top of
#             the plot building statementd
# angle:  angle of text on y axis (in degrees)

## nota bene:
## all parameters must be added to the recursive function call at the very end!

#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import magrittr

sszplot <- function(data,
                    geom = c("line"),
                    aes_x,
                    aes_y,
                    aes_col = NULL,
                    aes_ltyp = NULL,
                    aes_alpha = NULL,
                    aes_size = NULL,
                    labs_x = NULL,
                    labs_y = NULL,
                    labs_col = NULL,
                    labs_ltyp = NULL,
                    labs_alpha = NULL,
                    labs_size = NULL,
                    i_x = NULL,
                    i_y = NULL,
                    scale_x = "cont_pretty",
                    scale_y = NULL,
                    fix_col = 1,
                    fix_size = NULL,
                    breaks = NULL,
                    grid = NULL,
                    wrap = NULL,
                    ncol = NULL,
                    gridscale = NULL,
                    title = NULL,
                    name = NULL,
                    file_type = "pdf",
                    width = 7,
                    height = 4,
                    multi = NULL,
                    multif = NULL,
                    quotes = NULL,
                    quotes_top = NULL,
                    angle = NULL) {

  # some elementary checks (but not too sophisticated)
  stopifnot(!is.null(data) && !is.null(aes_x))
  stopifnot(!geom == "" || !is.null(quotes))


  #-------------------------------------------------------------------
  #### prep work ####
  #-------------------------------------------------------------------

  ## checks location of current file and saves plots in folder
  ## "3_Results/(first part of this file name)/[name].[file_type]", where
  ## (first part of this file name) means e.g. 0100_birth.
  ## creates the folder first if missing

  # determine output path from file name (for saving)
  current_file <- regmatches(this.path(),
                             regexpr("[a-zA-Z0-9._-]+$", this.path()))

  # reduce to the nnnn_xyz part of the file name (e.g. 0100_birth)
  sub_path <- regmatches(current_file,
                         regexpr("[0-9]{4}_[a-zA-Z0-9]+", current_file))

  # handle folder hierarchy only on first two of nnnn (e.g. 0110_birth is converted to 0100_birth)
  sub_path <- sub("[0-9]{2}_", "00_", sub_path)

  # create target path and create corresponding folder if not existing yet
  target <- paste(res_path, sub_path, sep = "/")
  if (!file.exists(paste(getwd(), target, sep = "/")))
    dir.create(paste(getwd(), target, sep = "/"),
               recursive = TRUE)
  target <- paste(target, paste0(name, ".", file_type), sep = "/")


  ## building of the plot is only needed if we are not in multipage mode
  if (is.null(multi)) {
    ## colours
    # special palettes are defined: a default with changed order and one especially
    # for time specific plots, where the colour depends on a time attribute
    # (usually year). Colours are then "rainbow" for the scenario years and grey
    # for all past years (except when only past years are shown).
    # This must be calculated dynamically

    # if aes_col is set, selects as many colors as needed by grouping variable aes_x
    # from the predefined color palette col_6
    # if not, selects entry from predefined colour palette (but in adapted order)
    # with given index fix_col (default 1)

    col_palette <- col_6[c(1, 3, 4, 5, 6, 2)]

    if (is.null(aes_col))
      ifelse(fix_col > length(col_palette),
              fix_col <- col_palette[1],
              fix_col <- col_palette[fix_col])
    else {
      if (identical(aes_col, "origin"))
        fix_col <- col_o
      else
        if (aes_col %in% c("year", "month", "week", "day")) {
          alltimes <- select(data, all_of(aes_col)) %>% unique %>% nrow
          oldtimes <- select(data, all_of(aes_col)) %>% filter(. < szen_begin) %>% unique %>% nrow
          maxtime <- select(data, all_of(aes_col)) %>% max()

          if (maxtime > szen_begin)
            col_time <- c(rep(col_grey, oldtimes),
                          colorRampPalette(col_6[1:5])(alltimes - oldtimes))
          else
            col_time <- colorRampPalette(col_6[1:5])(alltimes)

          fix_col <- col_time
        }
      else
        if (identical(aes_col, "sex"))
          fix_col <- col_s
      else
        if (identical(aes_col, "region"))
          fix_col <- col_r
      else
        fix_col <- col_palette[1:count(unique(data[aes_col]))$n]

      # if aes_col is a continuous variable, it has to be converted to a factor (discrete scale)
      if (is.numeric(eval(str2lang(paste0("data$", aes_col)))))
        aes_col <- as.factor(eval(str2lang(paste0("data$", aes_col))))
    }

    ## aesthetics stuff

    # Need to use aes_string to build it up and then make the class "uneval"
    aest <- aes_string(x = aes_x, y = aes_y)
    if (!is.null(aes_col))
      aest <- c(aest, aes_string(colour = aes_col))
    if (!is.null(aes_ltyp))
      aest <- c(aest, aes_string(linetype = aes_ltyp))
    if (!is.null(aes_alpha))
      aest <- c(aest, aes_string(alpha = aes_alpha))
    if (!is.null(aes_size))
      aest <- c(aest, aes_string(size = aes_size))
    class(aest) <- "uneval"

    # set axis labels to aes_x and aes_y if they are not explicitly given
    if (is.null(labs_x)) labs_x <- aes_x
    if (is.null(labs_y)) labs_y <- aes_y

    #-------------------------------------------------------------------
    #### build the plot ####
    #-------------------------------------------------------------------
    # default plot
    res <- ggplot(data) +
      neutral

    # add quoted arguments at the very top if desired. This may be needed to achieve the right layering
    # order of multiple geoms when one geom contains a separate aes_col param while the other does not.
    # The argument may contain a single quote or a list of quotes.
    # they must be handled separately
    if (!is.null(quotes_top)) {
      if (is.call(quotes_top))
        res <- res +
          eval(quotes_top)
      else {
        for (i in seq_len(length(quotes_top))) {
          res <- res +
            eval(quotes_top[[i]])
        }
      }
    }
    # add aesthetics except if geom is "": then it is expected that the aesthetic
    # are handed to the function in a quoted geom statement,
    # i.e. quotes = quote(geom_line(x = ...)).
    if (!identical(geom, ""))
      res <- res + aest

    # add vertical lines at i_x if i_x is set
    if (!is.null(i_x)) {
      if (identical(i_x, "5"))
        i_x <- data$year[data$year %% 5 == 0]

      res <- res +
        geom_vline(xintercept = i_x,
                   col = col_grey,
                   linetype = "dashed")
    }

    # add horizontal lines at i_y if i_y is set
    if (!is.null(i_y)) {
      for (i in seq_len(length(i_y)))
        res <- res +
          geom_hline(yintercept = i_y[i],
                     col = col_grey,
                     linetype = i)
    }

    # add geom; if none was specified, uses line chart as default
    # distinction dependent upon the colour grouping variable: if that is empty,
    # colour can be set inside geom (fixed value). Same is true for size:
    # if aes_size is not set, the fix_size will be taken into account
    if (!is.null(aes_size)) fix_size <- NULL
    if (is.null(fix_size))
      fix_size <- ""
    else
      fix_size <- paste("size = ", fix_size)

    geomfix <- ""
    if (is.null(aes_col)) geomfix <- paste0("colour = fix_col",
                                         if_else(nchar(fix_size) == 0, "", ","))
    if (is.null(aes_size)) geomfix <- paste0(geomfix, fix_size)


    if ("line" %in% geom)  res <- res + eval(str2expression(paste0("geom_line(", geomfix, ")")))
    if ("point" %in% geom) res <- res + eval(str2expression(paste0("geom_point(", geomfix, ")")))


    # add labels if labs_x and _y are set (labs_x is same as aes_x per default;
    # if you want none, then you need to use labs_x = "")
    if (is.null(labs_col)) labs_col <- ""
    if (is.null(labs_ltyp)) labs_ltyp <- ""
    if (is.null(labs_x)) labs_x <- ""
    if (is.null(labs_y)) labs_y <- ""
    if (is.null(labs_alpha)) labs_alpha <- ""
    if (is.null(labs_size)) labs_size <- ""

    res <- res + labs(x = labs_x,
                      y = labs_y,
                      colour = labs_col,
                      linetype = labs_col,
                      alpha = labs_alpha)

    # add continuous x scale with pretty breaks if scale_x is set
    if (!is.null(scale_x)) {
      if (startsWith(scale_x, "cont")) {
        if (endsWith(scale_x, "pretty"))
          res <- res +
            scale_x_continuous(breaks = pretty_breaks())
      }
      # preparation if a non-continuous non-pretty scale should be needed
      else if (startsWith(scale_x, "disc")) {

      }
    }

    # add continuous y scale if scale_y is set
    # can be either pretty_breaks, log10 or defined by limits
    if (!is.null(scale_y)) {
      if (identical(scale_y, "pretty"))
        res <- res + scale_y_continuous(breaks = pretty_breaks()) else
          if (identical(scale_y, "log"))
            res <- res + scale_y_continuous(trans = "log10") else
              res <- res + scale_y_continuous(limits = scale_y,
                                              breaks = if (!is.null(breaks)) breaks else pretty_breaks())
    }


    # add colour scale with fix_col if aes_col is set
    # (if colour is no grouping variable, fix_col is already applied in the geom statement)
    if (!is.null(aes_col))
      res <- res +
        scale_colour_manual(values = fix_col)


    # add title if title is set. If title is not set but mode is multipage,
    # a default value is created: the name of the x argument to the function
    if (!is.null(title))
      res <- res +
        ggtitle(as.character(title))

    # add facet grid or wrap if either grid or wrap is set
    if (!is.null(grid)) {
      # labeller for columnns should be 'label_both', but not if category is year/district/sex/region
      if (grid[2] %in% colnames(data) &&
          !grid[2] %in% c("year", "district", "sex", "region", "origin"))
        gridlab <- str2lang(paste0("labeller(", grid[2], " = label_both)"))
      else if (grid[1] %in% colnames(data) &&
               !grid[1] %in% c("year", "district", "sex", "region", "origin"))
        gridlab <- str2lang(paste0("labeller(", grid[1], " = label_both)"))
      else
        gridlab <- "label_value"

      res <- res +
        facet_grid(as.formula(paste(grid[1], "~", grid[2])),
                   cols = ncol,
                   scale = gridscale,
                   labeller = eval(gridlab))

    } else if (!is.null(wrap)) {
      # labeller for columnns should be 'label_both', but not if category is year/district/sex/region
      if (wrap %in% colnames(data) && !wrap %in% c("year", "district", "sex", "region"))
        gridlab <- str2lang(paste0("labeller(", wrap, " = label_both)"))
      else
        gridlab <- "label_value"

      res <- res +
        facet_wrap(as.formula(paste("~", wrap)),
                   ncol = ncol,
                   scale = gridscale,
                   labeller = eval(gridlab))
    }

    # change text angle
    if (!is.null(angle))
      res <- res +
      theme(axis.text.x = element_text(angle = angle, vjust = 0.5, hjust = 1))

    # add quoted arguments. The argument may contain a single quote or a list of quotes.
    # they must be handled separately
    if (!is.null(quotes)) {
      if (is.call(quotes))
        res <- res +
          eval(quotes)
      else {
        for (i in seq_len(length(quotes))) {
          res <- res +
            eval(quotes[[i]])
        }
      }
    }

    # in case alpha is used as aesthetics argument, make sure the respective labs are not transparent
    if (!is.null(aes_alpha))
      res <- res +
      guides(alpha = "none")
  }

  #-------------------------------------------------------------------
  # handle plot output and recursive function call in case of multipage
  #-------------------------------------------------------------------
  ## plot to file if name is set, otherwise to graphics device
  # (file type per default pdf)
  if (!is.null(multi)) {

    # set default title for multipage docs
    if (is.null(title))
      title <- "as.character(x)"

    # plot to pdf if a name is provided
    if (!is.null(name))
      pdf(target, width = width, height = height)

    # set default for multif parameter: the x value for the lapply function
    # relies on uni_d being the districts, uni_o the origins
    if (is.null(multif)) {
      if (identical(multi, uni_d)) multif <- "filter(district == x)"
      if (identical(multi, uni_o)) multif <- "filter(origin == x)"
    }
    stopifnot(!is.null(multi) && !is.null(multif))

    # if multiple similar plots have to be generated with one plot per page (via lapply):
    # data term is built on the fly from original data parameter together with multif:
    # data %>% <multif>, e.g. data %>% filter(district == x)
    lapply(multi, function(x) {
      sszplot(data = eval(str2lang(paste("data %>% ", multif))),
              geom = geom,
              aes_x = aes_x,
              aes_y = aes_y,
              aes_col = aes_col,
              aes_ltyp = aes_ltyp,
              aes_alpha = aes_alpha,
              aes_size = aes_size,
              labs_x = labs_x,
              labs_y = labs_y,
              labs_col = labs_col,
              labs_ltyp = labs_ltyp,
              labs_alpha = labs_alpha,
              labs_size = labs_size,
              i_x = i_x,
              i_y = i_y,
              scale_x = scale_x,
              scale_y = scale_y,
              fix_col = fix_col,
              fix_size = fix_size,
              breaks = breaks,
              grid = grid,
              wrap = wrap,
              ncol = ncol,
              gridscale = gridscale,
              title = eval(str2expression(title)),
              quotes = quotes,
              quotes_top = quotes_top,
              angle = angle)
    }
    )
    if (!is.null(name))
      dev.off()
  }
  else {
  if (!is.null(name))
    ggsave(target,
           plot = res,
           width = width,
           height = height)
  else
    print(res)
  }


  # TODO:
  #     simple long format function?
  #     parameter for colour palette if not default ?

}
