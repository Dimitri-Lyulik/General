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


PCdata <- readRDS(model_data)


# 2. Data Manipulation ---------------------------------------------------

PCdata %<>% filter(YOA <= Year_end)
PCdata %<>% filter(TRANS_TRANSIT_FLAG == "YES") %>% data.table()

PCdata[, earned.exposure := BI_Term_Years]
PCdata[, earned.exposure := ifelse(earned.exposure == 0, 1, earned.exposure)]
PCdata$ClaimCount <- PCdata[[peril]]
PCdata$ClaimCount <- ifelse(PCdata$ClaimCount > 0, 1, 0)
PCdata$target <- NULL

# Attach standard factor groupungs and assign levels

# Check there are not NAs
for (i in 1:length(PCdata)) {
  if(is.factor(PCdata[[i]])) {
    print(names(PCdata)[i])
    print(PCdata[is.na(PCdata[[i]]), ])
  }
}


# Check Factor distribution
glimpse(PCdata)


# Set Factor Levels
PCdata <- set.factor.ref.level("TRANS_COMMODITY", "OIL") # set based on most common level
PCdata <- set.factor.ref.level("TRANS_Est_Value_Band", "a. 0-100m") # set based on most common level (this is also the lowest band)
PCdata <- set.factor.ref.level("TRANS_Deductible_2", "MEDIUM") # set based on most common level (this is also expected to be neutral)
PCdata <- set.factor.ref.level("TRANS_PACKAGING", "CONTAINERISED") # set based on most common level
PCdata <- set.factor.ref.level("TRANS_VOYAGE", "MEDIUM") # set based on most common level (this is also expected to be neutral)
PCdata <- set.factor.ref.level("TRANS_SURV_PRESENT", "NO") # set based on most common level
PCdata <- set.factor.ref.level("TRANS_VESSEL", "AVERAGE") # set based on most common level (this is also expected to be neutral)
PCdata <- set.factor.ref.level("TRANS_TYPE_OF_COVER", "ICC A") # set based on most common level (this is also expected to be neutral)



{ # RUN FROM THIS LINE TO BUILD GBM AND GENERAT OUTPUTS

  # 3. Run GBM and output ---------------------------------------------------
  {
    model.name = 'freq.GBM.1'
    model.type = 'freq'
    actual = 'ClaimCount'
    exposure = 'earned.exposure'
    confidence.intervals <- 'continuous'
    
    n.trees = 5000               # Number of Trees (weak learners) to be modelled
    var.monotone = NULL          # force feature to be monotoncally increasing (e.g. insured value)
    n.minobsinnode = 150         # the minimum allowable number of observation for a terminal node. If the number of observations is smaller there wont be any additional splits
    distribution = "bernoulli"   # assumed error distribution
    cv.folds = 5                 # Number of cross-validation subsets for the model to create
    bag.fraction = 0.5           # the fraction of the training set observations randomly selected to propose the next tree in the expansion.
    train.fraction = 0.7         # The first train.fraction * nrows(data) observations are used to fit the gbm and the remainder are used for computing out-of-sample estimates of the loss function.
    shrinkage = 0.001            # learning rate / step-size function
    verbose = TRUE               # show intermediate output or not
    interaction.depth = 5  
    
    # Method to select optimal number of trees for prediction
    best.iter.method = "test"
    
    # split data into training a validation
    
    PCdata_split <- d.GBM.split.modelling.data(PCdata, 0.7, 0.01, 12345)
    data <- PCdata_split[["total"]]
    tdata <- PCdata_split[["training"]]
    vdata <- PCdata_split[["validation"]]
    
    tdata %<>% as.data.frame()
    vdata %<>% as.data.frame()
    
    
    # use this to obtain full list of variables; paste into excel, then paste into "PASTE HERE" below
    temp <- data.table(names(PCdata)); temp %<>% mutate(GLM.Features = paste0(", '", V1, "'")) %>% dplyr::select(GLM.Features) %>% d.copy(cn = F)
    
    features.gbm<-c(NULL 
                    # PASTE HERE
                    
                    # , 'TRANS_EST_ANN_TRANS_VALS'
                    , 'TRANS_Est_Value_Band'
                    , 'TRANS_Deductible_2'
                    , 'TRANS_PACKAGING'
                    , 'TRANS_VOYAGE'
                    , 'TRANS_SURV_PRESENT'
                    , 'TRANS_VESSEL'
                    # , 'TRANS_TYPE_OF_COVER'
                    # , 'TRANS_COMMODITY'
                    # , 1
    )
    
    features.all<-c(NULL 
                    # PASTE HERE
                    
                    , 'TRANS_EST_ANN_TRANS_VALS'
                    , 'TRANS_Est_Value_Band'
                    , 'TRANS_Deductible_2'
                    , 'TRANS_PACKAGING'
                    , 'TRANS_VOYAGE'
                    , 'TRANS_SURV_PRESENT'
                    , 'TRANS_VESSEL'
                    , 'TRANS_TYPE_OF_COVER'
                    , 'TRANS_COMMODITY'
                    # , 1
    )
    
    
    f <- formula(paste0('ClaimCount', ' ~ offset(log(', exposure, ')) + ', paste(features.gbm, collapse = ' + ')))
    
    # NO NEED TO EDIT ANYTHING BELOW
    
    m <- gbm(formula = f, 
              data = tdata[,c(features.gbm, 'ClaimCount', 'earned.exposure')],
              n.trees           = n.trees,              
              var.monotone      = var.monotone,
              n.minobsinnode    = n.minobsinnode,        
              distribution      = distribution,    
              cv.folds          = cv.folds,                
              bag.fraction      = bag.fraction,          
              train.fraction    = train.fraction,        
              shrinkage         = shrinkage,           
              verbose           = verbose,              
              interaction.depth = interaction.depth        
             )
    
    summary(m)
    
  }
  
  # For Export
  
  {
    
    # predict GBM
    best.iter <- gbm.perf(m, method = best.iter.method)
    data[, exposure := earned.exposure]
    data[, prediction := predict.gbm(m, newdata = data, n.trees = best.iter, type = "response") * earned.exposure]
    
    # AVM Result
    res0 <- f.avm.generic(model.name = model.name, 
                                model.group = model.group,
                                actual = actual, 
                                modelled = 'prediction', 
                                data = data, 
                                features = features.all,
                                confidence.intervals = confidence.intervals,
                                outpath = dir_out_output,
                                outname = '00 GBM AvM.csv',
                                avm.name = '00 GBM AvM.rds')
    
    # Overall data summary
    res1 <- d.GBM.overall(m, 
                  data = data, 
                  model.name = model.name, 
                  model.group = model.group, 
                  outpath = dir_out_output,
                  best.iter.method = best.iter.method,
                  outname.overall   = "01 GBM Overall.rds",
                  outname.var.imp   = "02 GBM Variable Importance.rds",
                  outname.best.iter = "03 GBM Best Iteration.rds",
                  outname.model = "08 GBM Model Summary.rds"
                  )
    
    
    
    # Partial Dependency Plots
    res2 <- d.GBM.PD.plots(m, 
                   data = tdata,
                   model.name = model.name, 
                   model.group = model.group, 
                   best.iter.method = best.iter.method,
                   outpath = dir_out_output,
                   outname = "04 GBM PD.rds")
    
    # gbm interactions
    res3 <- d.GBM.interactions(m, 
                       data = tdata,
                       model.name = model.name, 
                       model.group = model.group, 
                       outpath = dir_out_output,
                       outname = "05 GBM Var Interactions.rds")
      
    # deciles
    res4 <- d.GBM.deciles(m, 
                  vdata = vdata, 
                  model.name = model.name, 
                  model.group = model.group, 
                  outpath = dir_out_output,
                  outname = "06 GBM Deciles.rds")
    
    # ROC
    res5 <- d.GBM.RoC(m, 
              data = data, 
              model.name = model.name, 
              model.group = model.group, 
              outpath = dir_out_output,
              outname = "07 GBM RoC.rds")
    
    # compile all outputs
    d.GBM.data.compile(outpath = dir_out_output)
    
    
  } # end export chunk

} # end building model and export chunk
   
    
    
    
    