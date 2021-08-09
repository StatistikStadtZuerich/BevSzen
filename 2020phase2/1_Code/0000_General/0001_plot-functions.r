#-------------------------------------------------------------------
#generic plot function
#
# Kind of a ggplot wrapper to obtain similar and uniform plots. 
#
#rok/bad, July 2021
#-------------------------------------------------------------------
# 
# @params:
# data: main tibble
# geom: a char vector with the names of the desired geom
# aes_x: the grouping variable for x
# aes_y: the grouping variable for y
# aes_col: the grouping variable for colour
# aes_ltyp: the grouping variable for linetype
# labs_x, labs_y, labs_col, labs_ltyp: labels for the respective variables
# i_x: vector of vertical intercepts on x axis
# i_y: vector of horizontal intercepts on y axis
# scale_x: set the x scale. Default ist pretty_breaks for continuous scale
# scale_y: set the y scale. Default is none. Choose "pretty" for pretty_breaks,
#   a vector for limits (default with pretty_breaks)
# breaks: vector with breaks for scale if desired.
# fix_col: desired length of a colour vector selected from predefined colours
#          is ignored (and therefore unnecessary) if aes_col is set
# grid: if set, creates a facet grid with grid[1] on LHS and grid[2] on RHs of formula
# wrap: if set, creates a facet wrap with this value on RHs of formula
# col: argument cols for facet_grid, ncol for facet_wrap
# title: title for the whole chart
# name: if set, stores graphics under this name. otherwise to graphics device
# file_type: select file type for storing. default is pdf
# width: width of output file
# height: height of output file
# multi: apply the plot on this list recursively and combine the plots on multipage document
#         --> this is not working yet!!! (code removed)

sszplot <- function(data, geom = c("line", "point"),
                    aes_x, aes_y, aes_col = NULL, aes_ltyp = NULL,
                    labs_x = NULL, labs_y = NULL, labs_col = NULL, labs_ltyp = NULL,
                    i_x = NULL, i_y = NULL,
                    scale_x = "cont_pretty", scale_y = NULL, fix_col = 1, breaks = NULL,
                    grid = NULL, wrap = NULL, col = NULL,
                    title = NULL,
                    name = NULL, file_type = "pdf", width = 7, height = 4,
                    multi = NULL){
  
  stopifnot(!is.null(data) && !is.null(aes_x))
  
  #### prep work ####
  # determine output path from file name (for saving)
  current_file <- regmatches(this.path(), regexpr("[a-zA-Z0-9._-]+$", this.path()))
  # reduce to the nnnn_xyz part of the file name (e.g. 0100_birth)
  sub_path <- regmatches(current_file, regexpr("[0-9]{4}_[a-zA-Z0-9]+", current_file))
  # handle folder hierarchy only on first two of nnnn (e.g. 0110_birth is converted to 0100_birth)
  sub_path <- sub("[0-9]{2}_", "00_", sub_path)
  # create target path and create corresponding folder if not existing yet
  target <- paste(res_path, sub_path, sep = "/")
  if (!file.exists(paste(getwd(), target, sep = "/")))
    dir.create(paste(getwd(), target, sep = "/"), recursive = TRUE)
  target <- paste(target, paste0(name, ".", file_type), sep = "/")
  
  # theme  -- this does not work yet (no effect; should apply a default line colour)
  theme_ssz <- theme(line = element_line(colour = col_6[1]))
  
  # colours
  # if aes_col is set, selects as many colors as needed by grouping variable aes_x
  # from the predefined color palette col_6
  # if not, selects entry from predefined colour palette with given index fix_col (default 1)
  col_time <- c(rep(col_grey, length(uniy_bir_base)),
                colorRampPalette(col_6[1:5])(length(uniy_szen)))
  
  if (is.null(aes_col))
    fix_col <- col_6[fix_col] else
      if (identical(aes_col, "origin"))
        fix_col <- col_o else
      if (grepl("year", aes_col))
        fix_col <- col_time else
          fix_col <- colorRampPalette(col_6)(count(unique(data[aes_col]))$n)
  
  
  # aesthetics stuff. Need to use aes_string to build it up and then make the class "uneval"
  aest <- aes_string(x = aes_x, y = aes_y)
  if(!is.null(aes_col))
    aest <- c(aest, aes_string(colour = aes_col))
  if (!is.null(aes_ltyp))
    aest <- c(aest, aes_string(linetype = aes_ltyp))
  class(aest) <- "uneval"
  
  # set axis labels to aes_x and aes_y if they are not explicitly given
  if (is.null(labs_x)) labs_x <- aes_x
  if (is.null(labs_y)) labs_y <- aes_y

    
  #### build the plot ####
  # default plot      
  res <- ggplot(data) +
    aest +
    neutral
  
  # add geom; if none was specified, uses line chart with dots as default
  # distinction dependent upon the colour grouping variable: if that is empty,
  # colour can be set inside geom (fixed value). Unfortunately, this is currently
  # implemented with a cumbersome duplication for all geoms
  if(!is.null(aes_col)) {
    if("line" %in% geom) res <- res + geom_line()
    if("point" %in% geom) res <- res + geom_point()
  } else {
    if("line" %in% geom) res <- res + geom_line(colour = fix_col)
    if("point" %in% geom) res <- res + geom_point(colour = fix_col)
  }
  
  # add labels if labs_x and _y are set (labs_x is same as aes_x per default;
  # if you want none, then you need to use labs_x = "")
  if (is.null(labs_col)) labs_col = ""
  if (is.null(labs_ltyp)) labs_ltyp = ""
  if (is.null(labs_x)) labs_x = ""
  if (is.null(labs_y)) labs_y = ""
  
  res <- res + labs(x = labs_x, y = labs_y, colour = labs_col, linetype = labs_col)
  
  # add vertical lines at i_x if i_x is set
  if (!is.null(i_x)){
    if (identical(i_x,"5"))
      i_x <- data$year[data$year %% 5 == 0]
    
    res <- res + 
      geom_vline(xintercept = i_x, col = col_grey, linetype = "dashed")
  }
  
  # add horizontal lines at i_y if i_y is set
  if (!is.null(i_y)){
    for (i in 1:length(i_y))
      res <- res + 
        geom_hline(yintercept = i_y[i], col = col_grey, linetype = i)
  }
  
  # add continuous x scale with pretty breaks if scale_x is set
  if (!is.null(scale_x)){
    if (startsWith(scale_x, "cont")) {
      if (endsWith(scale_x, "pretty"))
        res <- res +
          scale_x_continuous(breaks = pretty_breaks())
    }
    else if (startsWith(scale_x, "disc")) {}
  }
  
  # add continuous y scale if scale_y is set
  # can be either pretty_breaks or defined by limits
  if (!is.null(scale_y)){
    if (identical(scale_y, "pretty"))
      res <- res + scale_y_continuous(breaks = pretty_breaks()) else
        res <- res + scale_y_continuous(limits = scale_y,
                                        breaks = if(!is.null(breaks)) breaks else pretty_breaks())
  }
  
  # add colour scale with fix_col if fix_col is set
  if (!is.null(fix_col)){
    res <- res +
      scale_colour_manual(values = fix_col)
  }
  
  # add title if title is set
  if (!is.null(title)){
    res <- res +
      ggtitle(as.character(title))
  }
  
  # add facet grid or wrap if either grid or wrap is set
  if (!is.null(grid)){
    res <- res +
      facet_grid(as.formula(paste(grid[1], "~", grid[2])), cols = col)
  } else if (!is.null(wrap)){
    res <- res +
      facet_wrap(as.formula(paste("~", wrap)), ncol = col)
  }
  
  # plot to file if name is setotherwise to graphics device
  # (file type per default pdf)
  if (!is.null(name)) {
      if (is.null(multi))
        ggsave(target, plot = res, width = width, height = height) else {
  ## this is work in progress, not working yet        
          pdf(target, width = 13, height = 8)
    
          lapply(multi, sszplot)
          
          dev.off()
        }
    
  } else {
    print(res)
  }
  

  
  #TODO:
  #     plot multi pages
  #     other geoms?
  #     colors if not indicated?
  #     different scales?
  
}
