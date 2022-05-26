#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '#
#' 
#' Progam Name: 00. General Setup.R
#' Author: Dimitri Lyulik
#' Date: 05/05/2021
#' Description: Set up custom funcitons and load useful libraries
#'
#'
#'
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '#


# 00 - User Defined Functions ---------------------------------------------

d.copy <- function(x, rn = F, cn = T) {
  
  # This function copies a data frame to your clipboard so you can paste it into excel
  # Function parameters are:
  #   x  = dataframe you wish to copy
  #   rn = include row names if set to TRUE
  #   cn = include column names if set to TRUE
  
  write.table(x, "clipboard-16384", sep="\t", row.names=rn, col.names=cn)
}

d.paste <- function () {
  
  # This function assigns a copied excel table to a variable in R. 
  # There are no function parameters. Use this as follows:
  #   1. copy a table in excel to clipbard
  #   2. run the following line in R:
  #      df <- d.paste()
  
  f <- file(description = "clipboard", open = "r")
  df <- read.table(f, sep = "\t", header = TRUE)
  rownames(df) <- NULL
  setDT(df)
  close(f)
  return(df)
}

d.connect.to.databases <- function(Server = NULL, Database = NULL) {
  
  # This function initiates standard SQL connection strings and places them into global evnironment.
  # There is an option to also create a custom connection string if you specify:
  #  - the server name; and
  #  - the database name
  # See below for connection strings names. Example:
  # Connect_to_Database()
  # 
  # df <- sqlQuery(channel = Channel_BI, "SELECT * FROM [BeazleyIntelligenceDataSets].[Report].[SectionCombinedView])
  
  
  require(RODBC)
  
  # BeazleyIntelligenceDataSets
  Channel_BI <<- RODBC::odbcDriverConnect(paste0(
    'Driver={SQL Server};
                  MultiSubnetFailover=Yes;
                  Server={DBS-p9m-BeazleyIntelligenceDataSetsRO-PRD,12440};
                  Database={BeazleyIntelligenceDataSets};
                  Trusted_Connection=Yes'))
  
  # GroupActuaryReserving_inputs
  Channel_Reserving_Inputs <<- RODBC::odbcDriverConnect(paste0(
    'Driver={SQL Server};
                  MultiSubnetFailover=Yes;
                  Server={DBS-2qg-GroupActuaryReserving_inputs-PRD,13020};
                  Database={GroupActuaryReserving_inputs};
                  Trusted_Connection=Yes'))
  
  # Template Rater
  Channel_Template_Rater <<- RODBC::odbcDriverConnect(paste0(
    'Driver={SQL Server};
                  MultiSubnetFailover=Yes;
                  Server={DBS-p3t-TemplateRater01-PRD};
                  Database={TemplateRater01};
                  Trusted_Connection=Yes'))
  
  # Template Rater - Marine
  Channel_Template_Rater_Marine <<- RODBC::odbcDriverConnect(paste0(
    'Driver={SQL Server};
                  MultiSubnetFailover=Yes;
                  Server={DBS-p3t-TemplateRater01-PRD};
                  Database={MarineHullRater};
                  Trusted_Connection=Yes'))
  
  # BBR
  Channel_BBR <<- RODBC::odbcDriverConnect(paste0(
    'Driver={SQL Server};
                  MultiSubnetFailover=Yes;
                  Server={DBS-93v-BeazleyBreachResponse-PRD};
                  Database={BeazleyBreachResponse};
                  Trusted_Connection=Yes'))
  
  # A Rich Cyber database
  Channel_Cyber <<- RODBC::odbcDriverConnect(paste0(
    'Driver={SQL Server Native Client 11.0};
                  MultiSubnetFailover=Yes;
                  Server={DBS-gs6-ExternalDataRepository-PRD,1433};
                  Database={ExternalDataRepository};
                  Trusted_Connection=Yes'))
  
    # A Rich Cyber database
  Channel_Safeguard <<- RODBC::odbcDriverConnect(paste0(
    'Driver={SQL Server Native Client 11.0};
                  MultiSubnetFailover=Yes;
                  Server={DBS-l39-Safeguard-PRD,12770};
                  Database={Safeguard};
                  Trusted_Connection=Yes'))
  
    # HPL
  Channel_HPL <<- RODBC::odbcDriverConnect(paste0(
    'Driver={SQL Server Native Client 11.0};
                  MultiSubnetFailover=Yes;
                  Server={DBS-fy7-HPL-PRD,13170};
                  Database={HPL};
                  Trusted_Connection=Yes'))
  
  
  # Custom Connection
  if (!(is.null(Server) & is.null(Database))) {
    
    Conn.String <- paste0(
      'Driver={SQL Server};
                  MultiSubnetFailover=Yes;
                  Server={', Database, '};
                  Database={', Server,'};
                  Trusted_Connection=Yes')
    
    Channel_Custom <<- RODBC::odbcDriverConnect(Conn.String)
  }
}

d.download.SQL.data <- function(channel = NULL,  out = NULL,  SQLstring = NULL) {
  
  if(is.null(channel)) {
    stop("channel required - see Connect_to_Databases for details")
  }
  
  if(is.null(out)) {
    stop("Save location required")
  }
  
  if(is.null(SQLstring)) {
    stop("SQL Query required - as text")
  }
  
  require(RODBC)
  require(data.table)
  
  res <- data.table(sqlQuery(channel = channel, query = SQLstring))
  fwrite(res, out)
  cat("output written to:\n", gsub(pattern = "/", replacement = "\\\\", out))
  
  return(res)
  
}


d.mean.with.char <- function(x){
  
  xnum <- as.numeric(x)
  
  if(any(!is.na(xnum))) {
    as.character(mean(xnum, na.rm=TRUE))
  } else {
    x[1]
  }
}


d.numeric.to.string <- function(x){
  
  xnum<- as.numeric(as.character(x))
  
  if(!any(is.na(xnum)) & !is.factor(x)) {
    xnum
  } else {
    x
  }
}

d.data.summary <- function(
  data = NULL
  , max_levels = 60
  , input_file = NULL
  , excel_output = FALSE
  , output_location = NULL
  , summary_name = NULL
  , breaks_in = "Sturges"
  , hist_trim = 1) {
  
  #' 
  #' This function Summarises an input dataset. It produces 7 summaries:
  #' -- data dimensions
  #' -- Numeric Summary (mins, max, mean, quantiles, NA counts, zero counts)
  #' -- Character / factor (Count of unique levels and prop relative to total rows)
  #' -- Long Character / factor. These fields are skipped (variable name and number of unique levels)
  #' -- Date Summary (min, max, time frame in days and years)
  #' -- Sample of 15 rows
  #' -- Histogram summary
  #' 
  #' Parameters:
  #' @data = input dataset
  #' @max_levels = maximum amount of unique levels allowable to include in character / factor summary
  #' @excel_outpt = TRUE / FALSE to write summaries to csvs
  #' @output_location = location of directory where output should be saved. enter a string
  #' @summary_name = name of summary output folder
  #' @breaks_in = suggested number of bins to use for histogram summary
  #' @hist_trim = trims the data which feeds into histogram summary to exclude bottom/top quantiles
  #' 
  #' Use funciton as:
  #' 
  # res <- d.data.summary(
  #     data = data
  #   , max_levels = 60
  #   , excel_output = TRUE
  #   , output_location = '\\\\bfl.local\\dfsroot\\Users\\lyuld\\My Documents\\0 - Actuarial\\0 - DL Working Environment'
  #   , summary_name = "Test summary 2"
  #   , breaks_in = 20
  #   , hist_trim = 1
  # )
  #'
  
  require(data.table)
  require(tidyverse)
  require(purrr)
  require(magrittr)
  require(stringr)
  
  if (excel_output == TRUE & (is.null(output_location) | is.null(summary_name))) {
    stop("Specify output location and summary name")
  }
  
  if(hist_trim > 1 | hist_trim < 0) {
    stop("hist_trim value must be between 0 and 1")
  }
  
  dt <- copy(data)
  setDT(dt)
  
  # fix names
  names(dt) <- gsub("[^[:alnum:]]", "_", names(dt))
  
  dt %<>% modify_if(is.factor, as.character)
  var_names <- names(dt)
  
  Summary_Long_Char <- data.frame()
  Summary_character <- data.frame()
  Summary_numeric <- data.frame()
  Summary_dttm <- data.frame()
  Summary_hist <- data.frame()
  
  for (i in 1:length(dt)) {
    
    # case where character variable
    if(class(dt[[var_names[i]]]) %in% c("character", "logical")) {
      
      temp_char <- dt %>% dplyr::group_by_(.dots = var_names[i]) %>% dplyr::summarise(n = n()) %>% arrange(desc(n))
      
      # max_levels catch 
      if (nrow(temp_char) > max_levels) {
        
        cat("\nRunning summary: ", var_names[i], "type = character -- summary skipped, too many levels")
        Summary_Long_Char <- rbind(Summary_Long_Char, data.frame(Variable = var_names[i], No_of_levels = nrow(temp_char)))
        next
        
      }
      
      cat("\nRunning summary: ", var_names[i], "type = character")
      temp_char <- cbind(data.frame(X = var_names[i]), temp_char)
      names(temp_char) <- c("Variable", "Level", "Count")
      var_char_total <- sum(temp_char$Count)
      temp_char$Prop <-  temp_char$Count / var_char_total
      
      Summary_character <- rbind(Summary_character, temp_char)
      
    } else if (class(dt[[var_names[i]]]) == "numeric" | class(dt[[var_names[i]]]) == "integer") {
      
      # numeric variables
      cat("\nRunning summary: ", var_names[i], "type = numeric")
      VAR <- dt %>% dplyr::select(var_names[i]) %>% unlist() %>% unname()
      
      temp_num = data.frame(
        Variable = var_names[i],
        Count = length(VAR),
        Count_Zero = sum(VAR == 0),
        Count_NA = sum(is.na(VAR)),
        Prop_Zero = sum(VAR == 0) / length(VAR),
        Prop_NA = sum(is.na(VAR)) / length(VAR),
        Sum = sum(VAR, na.rm = T),
        Mean = mean(VAR, na.rm = T),
        Std.Dev = sd(VAR, na.rm = T),
        Min = min(VAR, na.rm = T),
        Q_25 = quantile(VAR, na.rm = T, 0.25),
        Q_50 = quantile(VAR, na.rm = T, 0.50),
        Q_75 = quantile(VAR, na.rm = T, 0.75),
        Q_95 = quantile(VAR, na.rm = T, 0.95),
        Q_99 = quantile(VAR, na.rm = T, 0.99),
        Max = max(VAR, na.rm = T)
      )
      
      temp_num %<>% melt(id.vars = "Variable")
      names(temp_num) <- c("Variable", "Metric", "Value")
      
      Summary_numeric <- rbind(Summary_numeric, temp_num)
      
      # histogram summary
      # suppressWarnings(h <- hist(VAR, breaks = seq(min(VAR, na.rm = T), max(VAR, na.rm = T), length.out = breaks_in)))
      
      # trim histogram data to remove outliers as specified in function trim_var
      trim_lo <- 1 - hist_trim
      VAR <- VAR[!is.na(VAR)]
      VAR <- VAR[VAR > quantile(VAR, trim_lo, na.rm = T) & VAR < quantile(VAR, hist_trim, na.rm = T)]
      
      if (length(VAR > 0)) {
        
        # if the quantile trim completely removes all records, skip histogram summary
        suppressWarnings(h <- hist(VAR, breaks = breaks_in))
        hist_temp <- data.table(names = var_names[i], breaks = h$breaks[1:length(h$breaks)-1], count = h$counts)
        hist_temp$index  <- 1:nrow(hist_temp)
        hist_temp$key <- paste0(hist_temp$names, hist_temp$index)
        Summary_hist <- rbind(Summary_hist, hist_temp)
        
      }
      
    } else if (class(dt[[var_names[i]]])[1] %in% c("Date", "POSIXct")) {
      
      # date variables variables -- needs work to convert to excel dates
      cat("\nRunning summary: ", var_names[i], "type = Date")
      VARdt <- dt %>% dplyr::select(var_names[i]) 
      names(VARdt) <- "temp"
      
      VARdt <- VARdt[order(temp)]
      mindate <- min(VARdt$temp)
      maxdate <- max(VARdt$temp)
      datediff_yrs <- (maxdate - mindate)
      
      temp_dttm <- cbind("Minimum Date"=as.character(mindate),
                         "Maximum Date" = as.character(maxdate), 
                         "Difference in days" = as.character(datediff_yrs),
                         "Difference in years" = as.character(datediff_yrs / 365)
      )
      
      temp_dttm %<>% melt()
      temp_dttm$Var1 <- NULL
      
      temp_dttm <- cbind(var_names[i], temp_dttm)
      names(temp_dttm) <- c("Variable", "Metric", "Value")
      
      Summary_dttm <- rbind(Summary_dttm, temp_dttm)
      
    } else {
      
      cat("skipping ", var_names[i],"; summary not defined for class: ", class(dt[[var_names[i]]]))
      
    }
  }
  
  # create summary of data dimensions and other function inputs
  Summary_dimensions <- data.frame(Rows = dim(data)[1], Cols = dim(data)[2], max_levels = max_levels, breaks = breaks_in, TrimQ = hist_trim)
  
  # write output
  if(excel_output == TRUE) {
    
    if (!is.null(output_location)) {
      
      save_location <- paste0(output_location, "\\", summary_name, "\\")
      
      if (!dir.exists(save_location)) {
        dir.create(save_location)
      }
    }
    
    # add catches to force output
    
    if (nrow(Summary_Long_Char) == 0) {
      Summary_Long_Char <- data.table(ERROR = "No long character variables present")
    }
    
    if (nrow(Summary_character) == 0) {
      Summary_character <- data.table(ERROR = "No character variables present")
    }
    
    if (nrow(Summary_numeric) == 0) {
      Summary_numeric <- data.table(ERROR = "No numeric variables present")
    }
    
    if (nrow(Summary_dttm) == 0) {
      Summary_dttm <- data.table(ERROR = "No date variables present")
    }
    
    if (nrow(Summary_hist) == 0) {
      Summary_hist <- data.table(ERROR = "No numeric variables for histogram output")
    }
    
    
    tryCatch(fwrite(Summary_dimensions, paste0(save_location, "Summary_Dimensions.csv")), error = function(e) print("skipped"))
    tryCatch(fwrite(head(dt, 100), paste0(save_location, "Summary_Data_Sample.csv")), error = function(e) print("Sample - skipped"))
    tryCatch(fwrite(Summary_Long_Char, paste0(save_location, "Summary_long_char.csv")), error = function(e) print("Long Character - skipped"))
    tryCatch(fwrite(Summary_character, paste0(save_location, "Summary_character.csv")), error = function(e) print("Character - skipped"))
    tryCatch(fwrite(Summary_numeric, paste0(save_location, "Summary_numeric.csv")), error = function(e) print("Numeric - skipped"))
    tryCatch(fwrite(Summary_dttm, paste0(save_location, "Summary_dttm.csv")), error = function(e) print("DTTM - skipped"))
    tryCatch(fwrite(Summary_hist, paste0(save_location, "Summary_hist.csv")), error = function(e) print("Histogram - skipped"))
    
  }
  
  return(list(Num = Summary_numeric, Char = Summary_character, Char_long = Summary_Long_Char, date = Summary_dttm))
}


d.library <- function(package) {
  
  #' Description:
  #' Function checks if package exists
  #' if the package doesn't exist, first install the package and then load;
  #' otherwise just load the package
  #' 
  #' Variables:
  #' @package = name of package entered a string
  #' 
  #' Example:
  #' d.library('data.table')
  
  if(!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

d.search.pdf <- function (
  directory
  , keyword
  , dir_path_keyword = NULL
  , split_pdf = FALSE
  , surround_lines = FALSE
  , ignore_case = TRUE
  , remove_hyphen = TRUE
  , token_results = TRUE
  , convert_sentence = TRUE
  , split_pattern = '//p{WHITE_SPACE}{3,}'
  , full_names = TRUE
  , file_pattern = '.pdf'
  , recursive = FALSE
  , max_search = NULL
  , ...) 
{
  
  # SPECIFY DIRECTORY AND FILE NAMES
  files_dir <- list.files(path = directory, pattern = file_pattern, full.names = full_names, recursive = recursive)
  file_name <- list.files(path = directory, pattern = file_pattern, full.names = FALSE, recursive = recursive)
  
  if (!is.null(dir_path_keyword)) {
    
    # Search files for specific word in name and directory path
    dir.keep = grep(pattern = dir_path_keyword, ignore.case = ignore_case, x = files_dir)
    files_dir <- files_dir[dir.keep]
    file_name <- file_name[dir.keep]
    
  }
  

  error_table <- tibble(keyword = keyword, page_num = 0L, line_num = 0L, line_text = list('Skipped'), text_text = list('Skipped'))
  
  if (is.null(max_search)) {
    
    extract_table <- lapply(seq_along(files_dir), 
                            function(xx) {tryCatch({pdfsearch::keyword_search(files_dir[xx]
                                                                              , keyword = keyword
                                                                              , path = TRUE
                                                                              , split_pdf = split_pdf
                                                                              , surround_lines = surround_lines
                                                                              , ignore_case = ignore_case
                                                                              , remove_hyphen = remove_hyphen
                                                                              , token_results = token_results
                                                                              , convert_sentence = convert_sentence
                                                                              , split_pattern = split_pattern)
                            }, error = function(e)  NULL)
                            }
    )
    
  } else {
    
    files_dir <- files_dir[1:max_search]
    file_name <- file_name[1:max_search]
    
    extract_table <- lapply(seq_along(files_dir), 
                            function(xx) {tryCatch({pdfsearch::keyword_search(files_dir[xx]
                                                                              , keyword = keyword
                                                                              , path = TRUE
                                                                              , split_pdf = split_pdf
                                                                              , surround_lines = surround_lines
                                                                              , ignore_case = ignore_case
                                                                              , remove_hyphen = remove_hyphen
                                                                              , token_results = token_results
                                                                              , convert_sentence = convert_sentence
                                                                              , split_pattern = split_pattern)
                            }, error = function(e)  NULL)
                            }
    )
  }
  
  Result_Not_NULL <- c(1:length(files_dir))
  Result_NULL <- c()
  
  for (i in 1:length(files_dir)) {
    if (is.null(extract_table[[i]])) {
      Result_Not_NULL <- Result_Not_NULL[!Result_Not_NULL %in% i]
      Result_NULL <- c(Result_NULL, i)
    }
  }
  
  
  num_rows <- unlist(lapply(extract_table, nrow))
  result_out <- do.call('rbind', extract_table)
  
  ids <- data.frame(ID = rep(seq_along(files_dir[Result_Not_NULL]), num_rows)
                    ,pdf_full_path = rep(files_dir[Result_Not_NULL], num_rows)
                    ,pdf_name = rep(file_name[Result_Not_NULL], num_rows)
  )
  
  result_out <- cbind(ids, result_out)
  
  result_out[['token_text']] <- NULL
  result_out %<>% as_tibble()
  result_out[['line_text']] <- gsub('//s+', ' ', result_out[['line_text']])
  
  
  # Attach skipped PDFs
  result_null_out <- tibble(ID = rep(0, length(file_name[Result_NULL])),
                            pdf_full_path = files_dir[Result_NULL],
                            pdf_name = file_name[Result_NULL],
                            keyword = rep('NA', length(file_name[Result_NULL])),
                            page_num = rep(0, length(file_name[Result_NULL])),
                            line_num = rep(0, length(file_name[Result_NULL])),
                            line_text = rep('PDF HAS BEEN SKIPPED DUE TO ERRORS', length(file_name[Result_NULL]))
  )
  
  result_out <- rbind(result_null_out, result_out)
  
  
  
  return(result_out)
}


d.OHE.variable <- function(data_in, var_name_in) {
  
  setDT(data_in)
  
  var_name <- names(data_in[, ..var_name_in])
  var_levels <- data_in[[var_name_in]]
  temp_var <- paste(var_name, var_levels, sep = "_")
  data_in$temp_var <- temp_var
  data_in$temp_val <- 1
  
  data_in %<>% spread(key = temp_var, value = temp_val, fill = 0)
  return(data_in)
  
}


# GLM SPECIFIC

# plotdata = PCdata
# plotdata$target = plotdata$ClaimCount
# 
# plotdata$target = rlnorm(nrow(plotdata), 10, 2)
# plotdata %<>% mutate(target = ifelse(target < 500000, 0, target))
# plotdata$exposure = plotdata$earned.exposure
# 
# var = "Construction"
# target = "ClaimCount"
# exposure = "earned.exposure"

get.factor.count <- function(plotdata, var, bin_width = 1, sort_index = 2, graph = TRUE) {
  
  x  <- plotdata %>% dplyr::group_by_(.dots=var) %>% summarise(Exposure = sum(earned.exposure), Clm = sum(ClaimCount), freq = sum(ClaimCount) / sum(earned.exposure)) %>% data.frame()
  x <- x[order(-x[,sort_index]),]
  print(x)
  
  if (is.numeric(plotdata[[var]])) {
    
    p <- ggplot(plotdata, aes_string(var)) + geom_histogram(binwidth = bin_width, color = "darkblue", fill = "lightblue")
    
  } else {
    
    p <- ggplot(plotdata, aes_string(x = var)) + geom_bar(color = "darkblue", fill = "lightblue") 
    
  }
  
  if (graph == TRUE) {
    return(p)
  } else {
    return(x)
  }
  
}

get.factor.count.sev <- function(plotdata, var, target, bin_width = 1, sort_index = 2, graph = TRUE) {
  
  x  <- plotdata %>% dplyr::filter(target > 0) %>% dplyr::group_by_(.dots=var) %>% summarise(Clm = n(), ACS = sum(target) / n()) %>% data.frame()
  x <- x[order(-x[,sort_index]),]
  acs_rel <- x[which(x$Clm == max(x$Clm)), ]$ACS
  x$ACS_Rel <- round(x$ACS / acs_rel,3)
  
  print(x)
  
  if (is.numeric(plotdata[[var]])) {
    
    p <- ggplot(plotdata, aes_string(var)) + geom_histogram(binwidth = bin_width, color = "darkblue", fill = "lightblue")
    
  } else {
    
    p <- ggplot(plotdata, aes_string(x = var)) + geom_bar(color = "darkblue", fill = "lightblue") 
    
  }
  
  if (graph == TRUE) {
    return(p)
  } else {
    return(x)
  }
  
}

set.factor.ref.level <- function (x, ref) {
  
  PCdata[[x]] <- factor(PCdata[[x]], levels = unique(PCdata[[x]]))
  PCdata[[x]] <- relevel(PCdata[[x]], ref = ref)
  
  return(PCdata)
  
}

f.clean <- function(x) {str_replace_all(x, "[^[:alnum:]]", " ")}

f.require <- function(x) {
  
  new.packages <- x[!(x %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) install.packages(new.packages)
  
  tmp <- suppressWarnings(suppressMessages(lapply(x, require, character.only = TRUE, quietly = TRUE, warn.conflicts = F)))
  rm(tmp,x,new.packages)
  
}

f.band.variable <- function(data.in, 
                            variable.name, 
                            variable.name.out = paste0(variable.name,".banded"), 
                            break.levels) {
  
  #Set up new variable name
  variable.name.banded <- paste(variable.name,".banded",sep="")
  
  #Set up the dummy variable name to work on
  
  data.in$dummy.variable <- data.in[, variable.name, with = FALSE]
  
  #Work out what is the maximum number of decimal places in the banding list
  #Using "x<y+0.00000001 & x>y-0.00000001" instead of "x==y" is to avoid floating point errors
  
  var.decimals=0
  
  for(j in 1:length(break.levels)) {
    
    test<-{min(which(break.levels[j]*10^(0:20)<round(break.levels[j]*10^(0:20),digits=0)+0.00000001 &
                       break.levels[j]*10^(0:20)>round(break.levels[j]*10^(0:20),digits=0)-0.00000001 )) - 1}
    
    if (test>var.decimals) {var.decimals<-test}
    
  }
}

f.spline <- function(data, variable, breaks, prefix = NULL, suffix = NULL, names = NULL) {
  
  options(scipen = 999)  # No scientific notation
  
  data[, s.variable := get(variable)]
  
  numbering <- sprintf('%02d', c(1:length(breaks)))
  
  if (is.null(names)) {
    
    splines <- paste0(variable, '.', numbering, '.', breaks[2:length(breaks)])
    
    if (!is.null(prefix)) splines <- paste0(prefix,'.',splines)
    if (!is.null(suffix)) splines <- paste0(splines,'.', suffix)
    
  } else {
    
    splines <- names
    
  }
  
  for (i in 1:(length(breaks) - 1)) {
    
    data[, splines[i] := pmin(pmax(s.variable - breaks[i], 0), breaks[i+1] - breaks[i])]
    
  }
  
  return(data)
  
}

f.check.duplicates <- function(data, variable, return.dups = F) {
  
  data <- data.frame(data)
  
  if (class(data[, variable]) == 'factor') {
    n.dups <- nrow(data) - length(unique(levels(data[, variable])))
  } else {
    n.dups <- nrow(data) - nrow(unique(select_(data, .dots = variable)))
  }
  
  if (n.dups == 1) {
    print(paste0('There is ', n.dups, ' duplicate record on @', deparse(substitute(data))))
  } else {
    print(paste0('There are ', n.dups, ' duplicate records on @', deparse(substitute(data))))
  }
  
  if (return.dups == T) {
    
    t.frequency <- data.frame(table(data[, variable]))
    v.dups <- data[duplicated(data[, variable]), variable]
    rec.dups <- data[data[, variable] %in% v.dups,]
    
    return(rec.dups)
    
  } 
  
}

f.variable.types <- function(data) {
  
  types <- data.table(sapply(data, class))
  vars <- names(data)
  
  out <- data.table(vars, types)
  names(out) <- c('variable', 'type')
  
  print(table(out$type))
  cat("******************************************************************** \n")
  
  return(out)
  
}

f.count.na <- function(data, dir.out = paste0(getwd(),'/'), file.name = paste0("na.summ.", deparse(substitute(data))), write.file = TRUE) {
  
  na.count <- sapply(data, function(y) sum(length(which(is.na(y)))))
  na.count <- data.frame(na.count)
  na.count$na.count.prop <- na.count$na.count / nrow(data)
  
  if (write.file == FALSE) {
    
    return(na.count)
    
  } else {
    
    write.csv(na.count, file=paste0(dir.out, file.name,".csv"), row.names = TRUE)
    
  } 
  
}

f.xlsx <- function(x, excel.path, sheet.name = 'Sheet1', row.names = F, col.names = T) {
  
  if (file.exists(excel.path)) {
    
    wb <- xlsx::loadWorkbook(excel.path)
    xlsx::removeSheet(wb, sheetName = sheet.name)
    xlsx::saveWorkbook(wb, excel.path)
    write.xlsx(x, excel.path, sheetName = sheet.name, append = T, row.names = row.names, col.names = col.names)
    
  } else {
    
    write.xlsx(x, excel.path, sheetName = sheet.name, append = F, row.names = row.names, col.names = col.names)
    
  }
  
}

f.avm.table <- function(data,
                        target,
                        p.target,
                        exposure,
                        feature, 
                        secondary.feature = NULL, 
                        confidence.intervals,
                        numeric.resolution, 
                        numeric.resolution.type,
                        numeric.band.perc.low,
                        numeric.band.perc.high,
                        order.factors) 
{
  
  feature.class <- class(data[,get(feature)])
  unique.levels <- length(unique(data[,get(feature)]))
  dimensions <- c(feature, secondary.feature)
  
  setDT(data)
  data[, d.target := get(target)]
  data[, dp.target := get(p.target)]
  data[, error := d.target - dp.target]
  data[, d.exposure := 1]
  
  if (!is.na(exposure)) data[, d.exposure := earned.exposure]
  
  # Banding for numeric variables
  
  if (feature.class %in% c('numeric', 'integer') & unique.levels > numeric.resolution) {
    
    if (numeric.resolution.type == 'ntiles') {
      
      data[, d.feature := ntile(get(feature), numeric.resolution)]
      decile.summary <- data[, .(value = max(get(feature))), by = d.feature][order(value)]
      
      data <- merge(data, decile.summary, by = 'd.feature')
      data[, b.feature := value]
      
    } else {
      
      factor.min <- quantile(data[,get(feature)], numeric.band.perc.low, na.rm = T)
      factor.max <- quantile(data[,get(feature)], numeric.band.perc.high, na.rm = T)
      
      band.size <- (factor.max - factor.min) / (numeric.resolution - 1)
      
      options(scipen = 999)
      
      x <- expand.grid(c(1,2,5), 10^(-2:8))
      v.round.values <- x$Var1 * x$Var2
      
      v.diff <- abs(v.round.values - band.size)
      n.closest <- which(v.diff == min(v.diff)) 
      band.size <- v.round.values[n.closest]
      
      data[, b.feature := pmin(pmax(get(feature), factor.min), factor.max)]
      data[, b.feature := band.size * (floor(b.feature / band.size))]
      
    }
    
    dimensions <- c('b.feature', secondary.feature)
    
  } 
  
  
  # Create summary
  
  if (confidence.intervals == 'continuous') {
    
    
    s <- data[,list(actual     = sum(d.target) / sum(d.exposure)
                    , modelled = sum(dp.target) / sum(d.exposure)
                    , ci05      = sum(dp.target) / sum(d.exposure) - 1.96 * sd(error) / sqrt(sum(d.exposure))
                    , ci95      = sum(dp.target) / sum(d.exposure) + 1.96 * sd(error) / sqrt(sum(d.exposure))
                    , std       = sd(dp.target)
                    , exposure  = sum(d.exposure))
              , by        = dimensions]
    
    
  } else {
    
    
    s <- data[,list(actual     = sum(d.target) / sum(d.exposure)
                    , modelled = sum(dp.target) / sum(d.exposure)
                    , ci05      = sum(dp.target) / sum(d.exposure) - 1.96 * sqrt(sum(dp.target) / sum(d.exposure) * (1 - sum(dp.target) / sum(d.exposure))) / sqrt(sum(d.exposure))
                    , ci95      = sum(dp.target) / sum(d.exposure) + 1.96 * sqrt(sum(dp.target) / sum(d.exposure) * (1 - sum(dp.target) / sum(d.exposure))) / sqrt(sum(d.exposure))
                    , std       = sd(dp.target)
                    , exposure  = sum(d.exposure))
              , by        = dimensions]
    
  }
  
  # Rename things
  
  names(s)[1] <- 'feature1.level'
  s[, feature1 := feature]
  
  if (!is.null(secondary.feature)) {
    
    names(s)[2] <- 'feature2.level'
    s[, feature2 := secondary.feature]
    
  } else {
    
    s[, feature2.level := 'NA']
    s[, feature2 := 'NA']
    
  }
  
  setcolorder(s, c('feature1', 'feature2', 'feature1.level', 'feature2.level',
                   'actual', 'modelled', 'ci05', 'ci95', 'std', 'exposure'))
  
  # Re-order things
  
  if (feature.class %in% c('factor')) {
    
    if (order.factors == 'alphabetical') s <- s[order(feature1)]
    if (order.factors == 'modelled') s <- s[order(modelled)]
    if (order.factors == 'exposure') s <- s[order(exposure)]
    if (order.factors == 'modelled.desc') s <- s[order(-modelled)]
    if (order.factors == 'exposure.desc') s <- s[order(-exposure)]
    
  } else {
    
    s <- s[order(feature1.level)]
    
  }
  
  s[, feature1.level.text := paste(LETTERS, feature1.level)]
  s[, feature2.level.text := paste(LETTERS, feature2.level)]
  
  return(s)
  
}


f.avm.excel <- function(data,
                        target,
                        p.target,
                        exposure,
                        v.features,
                        secondary.feature = NULL,
                        confidence.intervals = 'binomial', #binomial continuous
                        clean.names = TRUE,
                        numeric.resolution = 15,
                        numeric.resolution.type = 'bands', #ntiles bands
                        numeric.band.perc.low = 0.01, 
                        numeric.band.perc.high = 0.99,
                        order.factors = 'modelled'
                        
) 
{
  
  
  d.results <- rbindlist(lapply(v.features, function(x) f.avm.table(data = data, 
                                                                    target = target, 
                                                                    p.target = p.target,
                                                                    exposure = exposure,
                                                                    feature = x,
                                                                    secondary.feature = NULL,
                                                                    confidence.intervals = confidence.intervals,
                                                                    numeric.resolution = numeric.resolution,
                                                                    numeric.resolution.type = numeric.resolution.type,
                                                                    numeric.band.perc.low = numeric.band.perc.low, 
                                                                    numeric.band.perc.high = numeric.band.perc.high,
                                                                    order.factors = order.factors)))
  
  
  
  if (!is.null(secondary.feature)) {
    
    d.results2 <- rbindlist(lapply(v.features, function(x) f.avm.table(data = data, 
                                                                       target = target, 
                                                                       p.target = p.target,
                                                                       exposure = exposure,
                                                                       feature = x,
                                                                       secondary.feature = secondary.feature,
                                                                       confidence.intervals = confidence.intervals,
                                                                       numeric.resolution = numeric.resolution,
                                                                       numeric.resolution.type = numeric.resolution.type,
                                                                       numeric.band.perc.low = numeric.band.perc.low, 
                                                                       numeric.band.perc.high = numeric.band.perc.high,
                                                                       order.factors = order.factors)))
    d.results <- rbind(results, results2)
    
  }
  
  d.features <- data.table(original.names = v.features,
                           clean.names = f.clean(v.features),
                           feature.order = c(1:length(v.features)))
  
  d.results <- merge(d.results, d.features, by.x = 'feature1', by.y = 'original.names', all = TRUE)
  
  
  n.oneways = nrow(d.results[feature2 == 'NA'])
  
  if (is.null(secondary.feature)) secondary.feature <- 'NULL'
  
  d.parameters <- data.table(data = deparse(substitute(data)),
                             n.features = length(v.features),
                             n.oneways = n.oneways,
                             n.twoways = nrow(d.results) - n.oneways,
                             secondary.feature = secondary.feature,
                             confidence.intervals = confidence.intervals,
                             clean.names = clean.names,
                             numeric.resolution = numeric.resolution,
                             numeric.resolution.type = numeric.resolution.type,
                             numeric.band.perc.low = numeric.band.perc.low,
                             numeric.band.perc.high = numeric.band.perc.high,
                             order.factors = order.factors)
  
  d.parameters <- data.table(parameters = names(d.parameters),
                             values = t(d.parameters)[,1])
  
  return(list(d.results, d.features, d.parameters))    
  
}

f.avm.generic <- function(model.name, 
                          model.group,
                          actual,
                          modelled,
                          data,
                          confidence.intervals,
                          model.object = paste0(model.name, '.rds'), 
                          outpath = dir.out.excel, 
                          features = NULL, 
                          outname = '00 AvM.csv',
                          avm.name = '00 AvM.rds',
                          verbose = T) {
  
  data[, n.modelled := get(modelled)]
  data[, n.actual := get(actual)]
  
  
  avm.od <- f.avm.excel(data,
                        target = 'n.actual',
                        p.target = 'n.modelled',
                        exposure = 'earned.exposure',
                        confidence.intervals = confidence.intervals,
                        v.features = features)
  
  
  result <- avm.od[[1]]
  result[, model := model.name]
  result[, model.group := model.group]
  
  
  if (file.exists(paste0(outpath, '/', avm.name))) {
    
    avm.data <- data.table(readRDS(paste0(outpath, '/', avm.name)))
    result.full <- rbind(result, avm.data[model != model.name])
    
  } else {
    
    result.full <- result
    
  }
  
  
  saveRDS(result.full, paste0(outpath, '/', avm.name))
  
  write.csv(result.full, paste0(outpath,'\\', outname), row.names = F)
  
  return(result)
  
}


# GBM SPECIFIC FUNCTIONS



d.GBM.split.modelling.data <- function(data_in, 
                                       training_split = 0.7, 
                                       precision = 0.01, 
                                       initial_seed = 12345) {
  
  data <- as.data.table(data_in)
  data[, Index := 1:.N]
  data_split = 2 
  
  while ((data_split < (1 - precision)) | (data_split > (1 + precision))) {
    
    set.seed(initial_seed)
    
    # set initial order
    data <- data[][order(Index)]
    
    # set order based on random uniform and split between training and validation
    data[, rand := runif(nrow(data))]
    data <- data[][order(rand)]
    
    # Split into training and validation
    tdata <- data[1:(training_split * nrow(data)), ]
    vdata <- data[(training_split * nrow(data)):nrow(data), ]
    
    # check that the split in data has the a similar overall average target
    data_split <- (sum(tdata[['target_variable']]) / sum(tdata$earned.exposure)) / (sum(vdata[["target_variable"]]) / sum(vdata$earned.exposure))
    
    print(initial_seed)
    print(data_split)
    
    initial_seed <- initial_seed + 1
    
  }
  
  tdata[, c("Index", "rand") := NULL]
  tdata[, dataset := "training"]
  
  vdata[, c("Index", "rand") := NULL]
  vdata[, dataset := "validation"]
  
  
  return(list("total" = rbind(tdata, vdata), "training" = tdata, "validation" = vdata))
  
}

d.GBM.overall <- function(m, 
                          data = data, 
                          model.name = model.name, 
                          model.group = model.group, 
                          target = peril,
                          outpath = dir_out_output,
                          best.iter.method = "test",
                          outname.overall = "01 GBM Overall.rds",
                          outname.var.imp = "02 GBM Variable Importance.rds",
                          outname.best.iter = "03 GBM Best Iteration.rds",
                          outname.model = "08 GBM Model Summary.rds"
) {
  
  # Overall Summary
  output_summ <- data %>% group_by(dataset) %>% summarise(target = sum(ClaimCount), prediction = sum(prediction), exposure = sum(earned.exposure), rows = n()) %>% data.table()
  output_summ <- cbind(data.table(model.group = model.group, model.name = model.name), output_summ)
  
  setDT(output_summ)
  output_summ[, key := paste0(model.group, model.name, 1:.N)]
  
  # Variable Importance
  output_var_imp <- data.table(model.group = model.group, model.name = model.name, Index = 1:nrow(summary(m))) 
  output_var_imp <- cbind(output_var_imp, summary(m))

  
  setDT(output_var_imp)
  output_var_imp[, key := paste0(model.group, model.name, 1:.N)]
  
  # best iteration plot
  
  best.iter <- gbm.perf(m, method = best.iter.method)
  best.iter.plot <- data.table(Index = 1:length(m$train.error), modeltrain.error = m$train.error, val.error = m$valid.error, max.y = max(m$valid.error), best.iter = best.iter )  
  best.iter.plot[1,1] <- 0
  
  output_best_iteration <- data.table(model.group = model.group, model.name = model.name, Index = seq(0,5000,100))
  output_best_iteration <- merge(output_best_iteration, best.iter.plot, all.x = T)
  output_best_iteration %<>% dplyr::select(model.group, model.name, Index, modeltrain.error, val.error, max.y, best.iter)
  
  setDT(output_best_iteration)
  output_best_iteration[, key := paste0(model.group, model.name, 1:.N)]
  
  # Model details
  if (is.null(var.monotone)) {
    var.monotone.t <- NA
  } else {
    var.monotone.t <- var.monotone
  }
  
  output_model <- data.table(Parameter = c('Best.iter', 'n.trees', 'var.monotone','n.minobsinnode','distribution','cv.folds','bag.fraction','train.fraction','shrinkage', 'interaction.depth', 'target.variable'),
                             ParValue  = c(best.iter.method, n.trees, var.monotone.t, n.minobsinnode, distribution, cv.folds, bag.fraction, train.fraction, shrinkage, interaction.depth, peril))
  
  
  output_model <- cbind(data.table(model.group = model.group, model.name = model.name), output_model)
  setDT(output_model)
  output_model[, key := paste0(model.group, model.name, 1:.N)]
  
  saveRDS(output_summ, paste0(outpath, outname.overall))
  saveRDS(output_var_imp, paste0(outpath, outname.var.imp))
  saveRDS(output_best_iteration, paste0(outpath, outname.best.iter))
  saveRDS(output_model, paste0(outpath, outname.model))
  
  return(list("Data_Summ" = output_summ, "Var_imp" = output_var_imp, "Best_iteration" = output_best_iteration, "Model_Details" = output_model))
  
}



d.GBM.PD.plots <- function (m, 
                            data = tdata,
                            model.name = model.name, 
                            model.group = model.group, 
                            best.iter.method = "test",
                            outpath = dir_out_output,
                            outname = "04 GBM PD.rds") {
  
  best.iter <- gbm.perf(m, method = best.iter.method)
  top_vars <- as.character(summary(m)[[1]])
  output <- data.table()
  
  for(i in 1:length(top_vars)) {
    
    varX <- data.table(model.group = model.group, model.name = model.name, Var = top_vars[i], plot.gbm(m, top_vars[i], best.iter, return.grid = T, type="response"))
    names(varX)[3:5] <- c("variable", "level", "pd")
    
    # attach exposure count
    if(is.factor(data[[top_vars[i]]])) {
      
      varX[, var.type := 'factor']
      data_dt <- data.table(data)
      varXCount <- data_dt[, .N, by=eval(top_vars[[i]])]
      names(varXCount) <- c("level", "density")
      varXb <- merge(varX, varXCount, by = "level")
      
      base_level = levels(data[[top_vars[i]]])[1]
      varXb[, relativity := pd / varX[level == base_level, pd]]
      
    }  else {
      
      varX[, var.type := 'continuous']
      varXDen <- density(data[[top_vars[i]]], na.rm = TRUE, n = 100, from = unlist(varX[1,4]), to = unlist(varX[100,4]))
      varXDenDF <- data.frame(Y=varXDen$y)
      varXb <- cbind(varX,varXDenDF)
      
      base_level = unname(unlist(varXb[varXb$Y == max(varXb$Y), 'pd']))
      varXb[, relativity := pd / base_level]
      names(varXb)[7] <- "density"
      
    }
    
    varXb %<>% dplyr::select(model.group, model.name, variable, var.type, level, pd, density, relativity)
    varXb$level <- as.character(paste0(varXb$level))
    output <- rbind(output, varXb)
    
    
  }
  
  setDT(output)
  
  output <- output[][order(model.group, model.name, variable, level)]
  output[, key := paste0(model.group, model.name, 1:.N)]
  
  saveRDS(output, paste0(outpath, outname))
  
  return(output)
  
}

d.GBM.interactions <- function(m, 
                               data,
                               model.name = model.name, 
                               model.group = model.group, 
                               outpath = dir_out_output,
                               outname = "05 GBM Var Interactions.rds") { 
  
  top_vars <- as.character(summary(m)[[1]])
  
  nx <- length(top_vars)
  v1 <- character()
  v2 <- character()
  H <- numeric()
  
  k <- 0
  
  for(i in 1:(nx-1)){
    
    for(j in (i+1):nx){
      k <- k+1
      print(k)
      v1[k] <- top_vars[i]
      v2[k] <- top_vars[j]
      
      H[k] <- interact.gbm(m, data = data, i.var=c(v1[k],v2[k]) )
    }
  }
  
  output <- data.frame(model.group = model.group, model.name = model.name, cbind(v1=v1, v2=v2, H=H))
  output$H <- as.numeric(as.character(output$H))
  output <- setDT(output)[order(-H)]
  output$Index = 1:nrow(output)
  
  output[, key := paste0(model.group, model.name, 1:.N)]
  
  saveRDS(output, paste0(dir_out_output, outname))
  
  return(output)
  
}

d.GBM.deciles <- function(m, 
                          vdata = data, 
                          model.name = model.name, 
                          model.group = model.group, 
                          outpath = dir_out_output,
                          outname = "06 GBM Deciles.rds") {
  
  vdata <- setDT(data)
  vdata <- vdata[dataset == 'validation', ]
  # temp <- setDT(vdata)[, decile := cut(prediction, quantile(prediction, probs=0:10/10), include.lowest=TRUE, labels=FALSE)]
  temp <- setDT(vdata)[order(prediction)]
  temp$rows <- 1:nrow(temp)
  totalrows <- nrow(temp)
  
  temp[, decile := ifelse(rows <= totalrows * 0.1 , 1, 
                   ifelse(rows <= totalrows * 0.2 , 2, 
                   ifelse(rows <= totalrows * 0.3 , 3, 
                   ifelse(rows <= totalrows * 0.4 , 4, 
                   ifelse(rows <= totalrows * 0.5 , 5, 
                   ifelse(rows <= totalrows * 0.6 , 6, 
                   ifelse(rows <= totalrows * 0.7 , 7, 
                   ifelse(rows <= totalrows * 0.8 , 8, 
                   ifelse(rows <= totalrows * 0.9 , 9, 10 
                                 )))))))))]
  #average by decile
  
  output_Avg <- temp[,mean(prediction),by=.(decile)]
  names(output_Avg) <- c("decile","Prediction")
  output_Avg <- output_Avg[order(output_Avg$decile)]
  
  output_Avg2 <- temp[,mean(ClaimCount),by=.(decile)]
  names(output_Avg2) <- c("decile1","Actual")
  output_Avg2 <- output_Avg2[order(output_Avg2$decile)]
  
  output_Count <- temp[,length(prediction),by=.(decile)]
  names(output_Count) <- c("decile2","Obs")
  output_Count <- output_Count[order(output_Count$decile)]
  
  output <- cbind(data.table(model.group = model.group, model.name = model.name), output_Avg,output_Avg2,output_Count)
  output$decile1 <- NULL
  output$decile2 <- NULL
  
  setDT(output)
  output[, key := paste0(model.group, model.name, decile)]
  
  saveRDS(output, paste0(outpath, outname))
  
  return(output)
  
}

d.GBM.RoC <- function(m, 
                      data = data, 
                      model.name = model.name, 
                      model.group = model.group, 
                      outpath = dir_out_output,
                      outname = "07 GBM RoC.rds") {
  
  # RoC
  vdata <- data[dataset == 'validation', ]
  predictions <- vdata$prediction
  Actuals <- pmin(vdata[[actual]], 1)
  
  GBM_Pred_testing <- ROCR::prediction(predictions, Actuals)
  GBM_ROC_test <- performance(GBM_Pred_testing, "tpr", "fpr")
  
  output <- data.table(model.group = model.group, model.name = model.name, fpr = unlist(GBM_ROC_test@x.values), tpr = unlist(GBM_ROC_test@y.values))
  output$Index <- 1:nrow(output)
  output$group <- as.numeric(cut(output$Index, 100))
  output %<>% group_by(model.group, model.name, group) %>% summarise(fpr = max(fpr), tpr = max(tpr))
  
  output$AB_line <- 1:nrow(output) / nrow(output)
  
  output$Index <- NULL
  output$group <- NULL
  output$Area <- gbm.roc.area(Actuals, predictions)
  
  setDT(output)
  output[, Index := 1:.N]
  output[, key := paste0(model.group, model.name, Index)]
  
  saveRDS(output, paste0(outpath, outname))
  
  # Confusion Matrix -- might need to think this through
  # predictions_b <- ifelse(predictions < 0.1, 0, 1)
  # conf_m <- confusionMatrix(Actuals, predictions_b)
  
  return(output)
  
}

d.GBM.data.compile <- function(outpath = dir_out_output) {
  
  # # GBM AvM
  # filenames <- c(NULL
  #                ,'00 GBM AvM'
  #                ,'01 GBM Overall'
  #                ,'02 GBM Variable Importance'
  #                ,'03 GBM Best Iteration'
  #                ,'04 GBM PD'
  #                ,'05 GBM Var Interactions'
  #                ,'06 GBM Deciles'
  #                ,'07 GBM RoC'
  # )
  # 
  # for (i in 1:length(filenames)) {
  #   
  #   filename_in <- filenames[i]
  #   
  #   if (file.exists(paste0(outpath, filename_in, ' - All.csv'))) {
  #     
  #     GBM.full <- as.data.frame(fread(paste0(dir_out_output, filename_in, ' - All.csv')))
  #     GBM <- as.data.frame(readRDS(paste0(dir_out_output, filename_in, '.rds')))
  #     
  #     GBM.full <- rbind(GBM, GBM.full[GBM.full$model.name != model.name, ])
  #     fwrite(GBM.full, paste0(outpath, filename_in, ' - All.csv'))
  #     
  #   } else {
  #     
  #     GBM <- as.data.frame(readRDS(paste0(dir_out_output, filename_in, '.rds')))
  #     GBM.full <- GBM
  #     fwrite(GBM.full, paste0(outpath, filename_in, ' - All.csv'))
  #     
  #   }
  
  #  for some reason doesnt work with loop above. says that excel is still open when trying to write
  
  filenames <- c(NULL
                 ,'00 GBM AvM'
                 ,'01 GBM Overall'
                 ,'02 GBM Variable Importance'
                 ,'03 GBM Best Iteration'
                 ,'04 GBM PD'
                 ,'05 GBM Var Interactions'
                 ,'06 GBM Deciles'
                 ,'07 GBM RoC'
  )
  
  
  # 00 GBM AVM
  if (file.exists(paste0(outpath, '00 GBM AvM - All.csv'))) {
    
    GBM.full.0 <- as.data.frame(fread(paste0(dir_out_output, '00 GBM AvM - All.csv')))
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '00 GBM AvM.rds')))
    GBM.full.0 <- rbind(GBM, GBM.full.0[GBM.full.0$model.name != model.name, ])
    
  } else {
    
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '00 GBM AvM.rds')))
    GBM.full.0 <- GBM
    
  }
  
  
  # 01 GBM Overall
  if (file.exists(paste0(outpath, '01 GBM Overall - All.csv'))) {
    
    GBM.full.1 <- as.data.frame(fread(paste0(dir_out_output, '01 GBM Overall - All.csv')))
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '01 GBM Overall.rds')))
    GBM.full.1 <- rbind(GBM, GBM.full.1[GBM.full.1$model.name != model.name, ])
    
  } else {
    
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '01 GBM Overall.rds')))
    GBM.full.1 <- GBM
    
  }
  
  
  # 02 GBM Variable Importance
  if (file.exists(paste0(outpath, '02 GBM Variable Importance - All.csv'))) {
    
    GBM.full.2 <- as.data.frame(fread(paste0(dir_out_output, '02 GBM Variable Importance - All.csv')))
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '02 GBM Variable Importance.rds')))
    GBM.full.2 <- rbind(GBM, GBM.full.2[GBM.full.2$model.name != model.name, ])
    
  } else {
    
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '02 GBM Variable Importance.rds')))
    GBM.full.2 <- GBM
    
  }
  
  
  # 03 GBM Best Iteration
  if (file.exists(paste0(outpath, '03 GBM Best Iteration - All.csv'))) {
    
    GBM.full.3 <- as.data.frame(fread(paste0(dir_out_output, '03 GBM Best Iteration - All.csv')))
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '03 GBM Best Iteration.rds')))
    GBM.full.3 <- rbind(GBM, GBM.full.3[GBM.full.3$model.name != model.name, ])
    
  } else {
    
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '03 GBM Best Iteration.rds')))
    GBM.full.3 <- GBM
    
  }
  
  
  # 04 GBM PD
  if (file.exists(paste0(outpath, '04 GBM PD - All.csv'))) {
    
    GBM.full.4 <- as.data.frame(fread(paste0(dir_out_output, '04 GBM PD - All.csv')))
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '04 GBM PD.rds')))
    GBM.full.4 <- rbind(GBM, GBM.full.4[GBM.full.4$model.name != model.name, ])
    
  } else {
    
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '04 GBM PD.rds')))
    GBM.full.4 <- GBM
    
  }
  
  
  # 05 GBM Var Interactions
  if (file.exists(paste0(outpath, '05 GBM Var Interactions - All.csv'))) {
    
    GBM.full.5 <- as.data.frame(fread(paste0(dir_out_output, '05 GBM Var Interactions - All.csv')))
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '05 GBM Var Interactions.rds')))
    GBM.full.5 <- rbind(GBM, GBM.full.5[GBM.full.5$model.name != model.name, ])
    
  } else {
    
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '05 GBM Var Interactions.rds')))
    GBM.full.5 <- GBM
    
  }
  
  
  # 06 GBM Deciles
  if (file.exists(paste0(outpath, '06 GBM Deciles - All.csv'))) {
    
    GBM.full.6 <- as.data.frame(fread(paste0(dir_out_output, '06 GBM Deciles - All.csv')))
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '06 GBM Deciles.rds')))
    GBM.full.6 <- rbind(GBM, GBM.full.6[GBM.full.6$model.name != model.name, ])
    
  } else {
    
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '06 GBM Deciles.rds')))
    GBM.full.6 <- GBM
    
  }
  
  tryCatch({
    # 07 GBM RoC
    if (file.exists(paste0(outpath, '07 GBM RoC - All.csv'))) {
      
      GBM.full.7 <- as.data.frame(fread(paste0(dir_out_output, '07 GBM RoC - All.csv')))
      GBM <- as.data.frame(readRDS(paste0(dir_out_output, '07 GBM RoC.rds')))
      GBM.full.7 <- rbind(GBM, GBM.full.7[GBM.full.7$model.name != model.name, ])
      
    } else {
      
      GBM <- as.data.frame(readRDS(paste0(dir_out_output, '07 GBM RoC.rds')))
      GBM.full.7 <- GBM
      
    }
  }, error = function(e) {"GBM ROC Skipped"; GBM.full.7 <<- data.table(error = "GBM.full.7 - no output")}
  )
  
  # 08 GBM Model Summary
  if (file.exists(paste0(outpath, '08 GBM Model Summary - All.csv'))) {
    
    GBM.full.8 <- as.data.frame(fread(paste0(dir_out_output, '08 GBM Model Summary - All.csv')))
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '08 GBM Model Summary.rds')))
    GBM.full.8 <- rbind(GBM, GBM.full.8[GBM.full.8$model.name != model.name, ])
    
  } else {
    
    GBM <- as.data.frame(readRDS(paste0(dir_out_output, '08 GBM Model Summary.rds')))
    GBM.full.8 <- GBM
    
  }
  
  Sys.sleep(1)
  
  
  # Write All
  fwrite(GBM.full.0, paste0(outpath, '00 GBM AvM - All.csv'))
  fwrite(GBM.full.1, paste0(outpath, '01 GBM Overall - All.csv'))
  fwrite(GBM.full.2, paste0(outpath, '02 GBM Variable Importance - All.csv'))
  fwrite(GBM.full.3, paste0(outpath, '03 GBM Best Iteration - All.csv'))
  fwrite(GBM.full.4, paste0(outpath, '04 GBM PD - All.csv'))
  fwrite(GBM.full.5, paste0(outpath, '05 GBM Var Interactions - All.csv'))
  fwrite(GBM.full.6, paste0(outpath, '06 GBM Deciles - All.csv'))
  fwrite(GBM.full.7, paste0(outpath, '07 GBM RoC - All.csv'))
  fwrite(GBM.full.8, paste0(outpath, '08 GBM Model Summary - All.csv'))
  
}


# 01 - Libraries ----------------------------------------------------------

d.library("tidyverse")
d.library("data.table")
d.library("magrittr")
d.library("BZLYRating")
d.library("readxl")
d.library("RODBC")
d.library("ggplot2")
d.library("rgdal") 
d.library("tidyr") 
d.library("scales")
d.library("viridis")
d.library("hrbrthemes")
d.library("directlabels")
d.library("ggplot2")
d.library("broom")
d.library("rmarkdown")
d.library("knitr")
d.library("stringr")
d.library("splines")

d.library('evir') 
d.library('quantmod') 

d.library('fitdistrplus')
d.library('actuar')

d.library('gbm')
d.library('caret')
d.library('ROCR')
d.library('corrplot')
d.library('Hmisc')
d.library('factoextra')
d.library('neuralnet')
library('BZLYUtil')
