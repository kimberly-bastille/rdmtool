# This code runs the standard projections requested by the PDT for WHAM models.
# The terminal year of this stock assessment is 2023.
# Last tested using WHAM version: 1.0.9.9000
# GithubRef: devel
# GithubSHA1: 24dd1ab92d90aad2d9bb04dcc8e58f1a155def19

# Author: Charles Perretti (2024-NOV)
# Mod : Min-Yang Lee (2024-Nov)

# This code does 2 projections, but uses the first projection
# 1) Fmsy 2025-2027 (this is the projection in the MT report) which also produces the ofl in 2025
# 2) 75%Fmsy 2025-2027 (potential ABCs)


# The bioeconomic model needs a few parameters that go into the stock assessment.
# It also needs some parameters that come out of the stock assessment.
# Bridging (2024 removals) is set at 2105mt, following the groundfish PDT

############
# Parameters that come out of the stock assessment
# Weights at age
# Historical NAA comes out of the WHAM stock assessment.

######
# Parameters that come out of the projections
######
# Numbers at age (in the future).  There isn't a stochastic projection for
# Haddock NAA are assumed to be lognormally distributed with a mean and sd parameters.
# I use rlnorm() to generate a distribution
############ End description###################################################


###########Begin Housekeeping##################################################
# Check the version of wham, install wham if needed, load libraries. I like using the "pak" package for this.

# Install the version of wham that corresponds to the version used to do the stock assessment.
packageDescription("wham")$RemoteSha
#If the result of the previous command  is not "24dd1ab92d90aad2d9bb04dcc8e58f1a155def19", you will need to  to
# install the version of wham that was used to do the initial estimation.
#pak::pak("timjmiller/wham@24dd1ab92d90aad2d9bb04dcc8e58f1a155def19", ask=FALSE)
# You should also probably restart R before running the rest of the code.


library(wham)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(haven)

#Set paths and savefile names

BLAST_root<-file.path("//nefscfile","BLAST","READ-SSB-Lee-BLAST","cod_haddock_fy2025")

input_folder<-file.path(BLAST_root,"source_data","haddock","input")
output_folder<-file.path(BLAST_root,"source_data","haddock","output",Sys.Date())

dir.create(file.path(output_folder), showWarnings = FALSE)

FullProjectionsSaveFile<-"GOM_Haddock_Projections.rds"

ProjectedNAASaveFile<-"GOM_Haddock_projected_NAA_2024Assessment.dta"
HistoricalNAASaveFile<-"GOM_Haddock_historical_NAA_2024Assessment.dta"



# Which year do you want a projection for, How many projections? Set a seed.
YearProj<-2025
num_NAA_draws<-10000
set.seed(6)
###########End Housekeeping#####################################################





# Set some specifications ######################################################
bridge_year_catch <- 2105 #GOM haddock 2024 MT PDT-supplied catch
model_location <- file.path(input_folder,"mod_nola_dcpe_blls2.rds")
stock_name <- "GOM haddock"
model_name <- "2024MT"
################################################################################

# Load WAA projections (specific to GOM haddock) ###############################
waa_proj_ssb <-
  readxl::read_excel(file.path(input_folder,"waa_pred_2024-08-25.xlsx"),
                     sheet = "SSB WAA") %>%
  filter(YEAR %in% 2024:2027) %>%
  select(-YEAR)

waa_proj_catch <-
  readxl::read_excel(file.path(input_folder,"waa_pred_2024-08-25.xlsx"),
                     sheet = "Catch WAA") %>%
  filter(YEAR %in% 2024:2027) %>%
  select(-YEAR)

waa_input_blls <- array(dim = c(6,4,9)) #new wham wants the waa doubled for some reason
for(i in 1:9){ # the order of the sources matches input$data$waa_pointers
  waa_input_blls[,,i] <- rbind(t(waa_proj_catch[,i]), t(waa_proj_ssb[,i]), t(waa_proj_ssb[,i]),
                               t(waa_proj_catch[,i]), t(waa_proj_ssb[,i]), t(waa_proj_ssb[,i]))
}

# In theory, you shouldn't have to touch anything below here:

# Pull models to make projections ##############################################
mod <- readRDS(file = model_location)
# take a look at the version of WHAM used to generate the model.
mod$wham_commit
mod$wham_version

# Assign some short names to the models (can do more than one if desired)
mod$model_name <- model_name

mod_list <- list(mod)

# Set specs ####################################################################
set_specs <- function(mod, bridge_year_catch) {

  Fmsy <- exp(mod$rep$log_FXSPR_static)
  catch2025 <-
    project_wham(model = mod,
                 proj.opts = list(n.yrs = 2,
                                  proj_F_opt = c(5, 4),
                                  proj_Fcatch = c(bridge_year_catch, 0.75 * Fmsy)),
                 do.sdrep = F, MakeADFun.silent = T,
                 check.version = FALSE)$rep$
    pred_catch[mod$env$data$n_years_model + 2, ] %>%
    sum()


  proj.opts_list <-
    list(Model = rep(mod$model_name, times = 2),
         scenario    = c("(1) Fmsy (2025-2027)",                  #1
                         "(2) 0.75Fmsy (2025-2027)"               #2
                         ),
         n.yrs       = rep(list(4), times = 2),
         proj_R_opt  = rep(list(2), times = 2),
         proj_F_opt  = list(c(5, 4, 4, 4),  #1
                            c(5, 4, 4, 4)  #2
                            ),
         proj_Fcatch = list(c(bridge_year_catch, rep(Fmsy, 3)),                #1
                            c(bridge_year_catch, rep(0.75 * Fmsy, 3))         #2
                           ),
         proj_waa = list(waa_input_blls,
                         waa_input_blls)

         )
}

proj.opts_list2 <- map_df(mod_list, .f = set_specs, bridge_year_catch)


# Run projections ##############################################################
proj_list <- list()
mod_names <- map_df(mod_list, .f = function(x) data.frame(model_name = x$model_name))
for(i in 1:length(proj.opts_list2$n.yrs)) {

  print(paste0("Running projection: ", proj.opts_list2$Model[i],
               " ", proj.opts_list2$scenario[i]))

  mod_index <- which(mod_names$model_name == proj.opts_list2$Model[i])

  proj_list[[i]] <-
    project_wham(model = mod_list[[mod_index]],
                 proj.opts = list(n.yrs = proj.opts_list2$n.yrs[[i]],
                                  proj_R_opt  = proj.opts_list2$proj_R_opt[[i]],
                                  proj_F_opt  = proj.opts_list2$proj_F_opt[[i]],
                                  proj_Fcatch = proj.opts_list2$proj_Fcatch[[i]],
                                  proj_waa    = proj.opts_list2$proj_waa[[i]]
                                  ),
                 do.sdrep = T,
                 MakeADFun.silent = T,
                 check.version = FALSE)

  proj_list[[i]]$scenario <- proj.opts_list2$scenario[i]
  proj_list[[i]]$Model <- proj.opts_list2$Model[i]
}


# Create table of results ######################################################
proj_out <-
  map_df(proj_list, .f = function(x) {

    std <- list(TMB:::as.list.sdreport(x$sdrep, what = "Est", report = TRUE),
                TMB:::as.list.sdreport(x$sdrep, what = "Std", report = TRUE))

    logssb <- std[[1]]$log_SSB
    logssb_sd <- std[[2]]$log_SSB
    ssb <- exp(std[[1]]$log_SSB)[,1]
    ssb_90lo <- exp(logssb - qnorm(0.95) * logssb_sd)[,1]
    ssb_90hi <- exp(logssb + qnorm(0.95) * logssb_sd)[,1]


    out <-
      tibble(Model = x$Model,
             scenario = x$scenario,
             Year = x$years_full,
             `F`  = round(apply(exp(x$rep$log_FAA_tot), 1, max),2),
             SSB  = round(ssb, 1),
             `SSB CI (90% low)`  = round(ssb_90lo,1),
             `SSB CI (90% high)` = round(ssb_90hi,1),
             `Catch Fleet` = round(x$rep$pred_catch,1),
             `Catch (Total)` = rowSums(`Catch Fleet`)) %>%
  filter(Year >= max(x$years)) %>%
  rename(`Projection scenario` = scenario)

    return(out)
  })


################################################################################
################################################################################
# Save the full set of projections
 saveRDS(proj_list, file = file.path(output_folder,FullProjectionsSaveFile))
################################################################################
################################################################################



# Plot projections #############################################################
proj2plot <-
  proj_out %>%
  gather(variable, value, -Year, -`Projection scenario`, -Model)

ggplot(proj2plot %>% filter(variable %in% c("Catch (Total)", "SSB")),
       aes(x = Year, y = value, color = `Projection scenario`)) +
  {if(length(unique(proj2plot$Model)) > 1) geom_line(linetype = Model) else
    geom_line()} +
  geom_point() +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  ylab("Metric tons") +
  theme_bw() +
  ggtitle(paste0(stock_name, " projections"))

#ggsave(paste0("projections_for_PDT.png"), w = 9, h = 6)


# Show NAA projection uncertainty ######################################
# This is a pretty slick way to do things.The log_naa tibble ends up in tidy format (Years and then ages)
# x <- proj_list[[1]] # Grab the Fmsy projections
# log_naa_ind <- which(names(x$sdrep$value) == "log_NAA_rep")
# log_naa <-
#   tibble(Year = rep(x$years_full, x$input$data$n_ages),
#          Age  = rep(1:x$input$data$n_ages, each = length(x$years_full)),
#          log_NAA = x$sdrep$value[log_naa_ind],
#          log_NAA_sd = x$sdrep$sd[log_naa_ind],
#          log_NAA_95lo = log_NAA - 1.96 * log_NAA_sd,
#          log_NAA_95hi = log_NAA + 1.96 * log_NAA_sd,
#          NAA = exp(log_NAA),
#          NAA_95lo = exp(log_NAA_95lo),
#          NAA_95hi = exp(log_NAA_95hi))
#
# ggplot(log_naa %>% filter(Year >= 2010), aes(x = Year, y = NAA)) +
#   geom_point() +
#   geom_line() +
#   geom_ribbon(aes(ymin = NAA_95lo, ymax = NAA_95hi), alpha = 0.3) +
#   geom_vline(xintercept = 2023.5, linetype= 2) +
#   facet_wrap(~Age) +
#   theme_bw()


################################################################################
################################################################################
# Get historical and projected NAA
################################################################################
################################################################################
#This pulls objects out of the sdreport.
std1 <- list(TMB:::as.list.sdreport(proj_list[[1]]$sdrep, what = "Est", report = TRUE),
             TMB:::as.list.sdreport(proj_list[[1]]$sdrep, what = "Std", report = TRUE))
year<-proj_list[[1]]$years_full

# Extract the mean and std dev of log_NAA from the results.
# the 1st dimension of this array contains stock, the second contains region.
# This particular WHAM model only contained 1 stock and 1 region.
NAA_logmean<-std1[[1]]$log_NAA_rep[1,1,,]
NAA_logsd<-std1[[2]]$log_NAA_rep[1,1,,]

#column names
names<-paste0("age",1:ncol(NAA_logmean))

TerminalAssess<-tail(mod$years_full,1)


# Construct a dataframe of historical Numbers at Age
historical_NAA<-exp(NAA_logmean)
colnames(historical_NAA)<-names
historical_NAA<-as.data.frame(cbind(year,historical_NAA))

historical_NAA <- historical_NAA %>%
  dplyr::filter(year<YearProj)

write_dta(historical_NAA, path=file.path(output_folder,HistoricalNAASaveFile))



# Pick exactly 1 year. See the header.
RowPick<-which(year==YearProj)
stopifnot(length(RowPick)==1)




#extract just 1 row
NAA_logmean<-NAA_logmean[RowPick,]
NAA_logsd<-NAA_logsd[RowPick,]

stopifnot(length(NAA_logmean)==length(NAA_logsd))


# Simulate NAA
NAA<-list()
for (ageclass in 1:length(NAA_logmean)){
  NAA[[ageclass]]<-rlnorm(num_NAA_draws,NAA_logmean[ageclass],NAA_logsd[ageclass])
}

#smush the list to a Dataframe, give it nice names, add on the year and a replicate number.
NAA<-list2DF(NAA)
colnames(NAA)<-names
NAA <-NAA %>%
  mutate(replicate= row_number(),
         year=YearProj) %>%
  relocate(replicate,year)

write_dta(NAA, path=file.path(output_folder,ProjectedNAASaveFile))
