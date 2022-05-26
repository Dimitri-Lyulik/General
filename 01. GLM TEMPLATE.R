#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '#
#' 
#' Progam Name: 01. GBM TEMPLATE.R
#' Author: Dimitri Lyulik
#' Date: 11/03/2022
#' Description: TEMPLAT SCRIPT FOR GBM MODELLING
#'
#'
#'
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '#
 

# 0. load Global Parameters ------------------------------------------------

{
  # Set up Parameters
  rm(list=ls()); gc()
  
  core_location <- ""
  model_data <- paste0(core_location, "\\Model_Data.RDS")
  dir_out_output <- paste0(core_location, "\\R Output\\")
  
  peril <- "target"
  model.group = "Model.Group"
  
  # load libraries and UDFs
  source(paste0("W:\\Finance\\Actuarial\\Pricing\\00 - General\\16 - Central Pricing Tools\\00 - R\\00. General Setup.R"))
  Year_end <- 2019
  
  
}


# 1. Data Load ------------------------------------------------------------

{
  PCdata <- readRDS(model_data)
  
  # 2. Data Manipulation ---------------------------------------------------
  
  PCdata %<>% filter(YOA <= Year_end)
  
  PCdata[, earned.exposure := BI_Term_Years]
  PCdata[, earned.exposure := ifelse(earned.exposure == 0, 1, earned.exposure)]
  PCdata$ClaimCount <- PCdata[[peril]]
  PCdata$target <- NULL
  
  # Attach standard factor groupungs and assign levels
  
  # Check there are no NAs
  for (i in 1:length(PCdata)) {
    if(is.factor(PCdata[[i]])) {
      print(names(PCdata)[i])
      print(PCdata[is.na(PCdata[[i]]), ])
    }
  }
  
  # check initial average frequency
  sum(PCdata$ClaimCount) / sum(PCdata$earned.exposure)
  
  glimpse(PCdata)
  
  # Check Factor distribution
  gf <- get.factor.count(PCdata, "TRANS_COMMODITY", bin_width = 1, graph = F)
  
  # Set Factor Levels
  PCdata <- set.factor.ref.level("TRANS_COMMODITY", "OIL") # set based on most common level
   
}

{ # Run All -- Exectute code from this line to build glm and generate outputs (change parameters below first)
  
  # 3. Run GLM and output ---------------------------------------------------
  {
    
    model.name = 'freq.1'
    model.type = 'freq'
    actual = 'ClaimCount'
    exposure = 'earned.exposure'
    data = PCdata
    setDT(data)
    
    # use this to obtain full list of variables; paste into excel, then paste into "PASTE HERE" below
    temp <- data.table(names(PCdata)); temp %<>% mutate(GLM.Features = paste0(", '", V1, "'")) %>% dplyr::select(GLM.Features) %>% d.copy(cn = F)
    
    # SELECT SPECIFIC FEATURES TO USE IN THE GLM
    features.glm<-c(NULL 
                    
                    # PASTE HERE
                    
    )
    
    # INCLUDE ALL FEATURES, MODELLED AND UNMODELLED TO ASSESS THE AVM IN EXCEL
    features.all<-c(NULL 
                    
                    # PASTE HERE

    )
    
    
  }
  
  # GLM FORMULA BASED ON THE ABOVE SELECTION. ONE WITHOUT SPLINES AND ONE WITH
  f <- formula(paste0(actual, ' ~ offset(log(',exposure,')) + ', paste(features.glm, collapse = ' + ')))
  m <- glm(f, data = data, family = poisson(link = "log"))
  summary(m)
  anova(m)
  
  
  # for export
  {
    
    p <- data.table(tidy(m, conf.int = F, exponentiate = F))
    data[, exposure := exposure]
    data[, prediction := predict(m, data, type = 'response')]
    confidence.intervals <- 'continuous'
    p <- data.table(tidy(m))
    p[, model := model.name]
    p[, model.group := model.group]
    
    #  ADD LEVERAGE PLOTS
    CD <- cooks.distance(m)
    CD <- CD[order(-CD)]
    
    p[, AIC := m$aic]
    p[, cooks.distance.top.N := CD[1:nrow(p)]]
    p[, formula := paste0(f[2], " ", f[1], " ", f[3])]
    
    glm.result <- f.avm.generic(model.name = model.name, 
                                model.group = model.group,
                                actual = actual, 
                                modelled = 'prediction', 
                                data = data, 
                                features = features.all,
                                confidence.intervals = confidence.intervals,
                                outpath = dir_out_output,
                                outname = '00 AvM.csv',
                                avm.name = '00 AvM.rds'
    )
    
    if (file.exists(paste0(dir_out_output, '01 GLM Parameters.rds'))) {
      
      glm.p <- readRDS(paste0(dir_out_output, '01 GLM Parameters.rds'))
      glm.p.full <- rbind(p, glm.p[model != model.name])
      
    } else {
      
      glm.p.full <- p
      
    }
    
    saveRDS(glm.p.full, paste0(dir_out_output, '01 GLM Parameters.rds'))
    write.csv(glm.p.full, paste0(dir_out_output, '01 GLM Parameters.csv'), row.names = F)
    
    
    # Remove previous combined output and re-save
    
    unlink(paste0(dir_out_output,"00 AvM.csv"))
    unlink(paste0(dir_out_output,"01 GLM Parameters.csv"))
    
    output.all <- list.files(paste0(dir_out_output), recursive = T, full.names = T)
    output.all.avm <- output.all[grep(pattern = "00 AvM.rds", output.all)]
    output.all.par <- output.all[grep(pattern = "01 GLM Parameters.rds", output.all)]
    
    avm.list <- data.table()
    param.list <- data.table()
    
    for(l in 1:length(output.all.avm)) {
      
      loc <- output.all.avm[l]
      file.in <- readRDS(loc)
      avm.list <- rbind(avm.list, file.in)
      
    }
    
    for(l in 1:length(output.all.par)) {
      
      loc <- output.all.par[l]
      file.in <- readRDS(loc)
      param.list <- rbind(param.list, file.in)
      
    }
    
    
    write.csv(avm.list, paste0(dir_out_output,'00 AvM - All.csv'), row.names = F)
    write.csv(param.list, paste0(dir_out_output,'01 GLM Parameters - All.csv'), row.names = F)
    
  }
}
