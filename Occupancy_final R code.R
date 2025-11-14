

############################################################################################################
#------------------------------------------ Packages & setup ------------------------------------------#
############################################################################################################

## Comments:

# This part loads all the R packages the script needs and then brings the dataset into R. 
# It prepares everything so the rest of the code can run smoothly. There is no big change from before, 
# except we now load all the packages at the start (installing them too if a new user doesn't have the right ones installed)
# and make sure R keeps text as text rather than automatically converting it into factors. This avoids annoying data 
# import issues later.


required_pkgs <- c("tidyr", "unmarked", "AICcmodavg", "ggplot2", "dplyr")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))

set.seed(42)  # Reproducibility

options(dplyr.summarise.inform = FALSE)



Occu_data = read.csv(
  "https://raw.githubusercontent.com/harryjobann/Pangolin/refs/heads/main/Joined_data.csv",
  stringsAsFactors = FALSE
) #ensures text columns stay as character type, not factors




############################################################################################################
#------------------------------------------ Recoding detection ------------------------------------------#
############################################################################################################


# Amended this slightly to turn the interview responses into numbers the model can use. 
# "YES" becomes 1, "NO" becomes 0, and anything else, such as "unknown" or blank, is left as missing. 
# In the original code, anything that was not "YES" was treated as a 0. This version is safer and more accurate because 
# uncertain answers are not automatically counted as definite "no" detections. Hope that makes sense.

# Safer binary recode. Keep unknowns as NA, not zero.
Occu_data <- dplyr::mutate(
  Occu_data,
  detection_binary = dplyr::case_when(
    detection == "YES" ~ 1L,
    detection == "NO"  ~ 0L,
    TRUE ~ NA_integer_
  )
)





############################################################################################################
#------------------------------------------ # Order and add visit index within site ------------------------------------------#
############################################################################################################

# This section groups interviews by site and gives each visit a number (1, 2, 3, and so on). 
# It helps the model know which observations belong to the same place. The logic is the same as before, 
# but we now use dplyr:: commands explicitly. This prevents confusion between packages that use the same function names 
# and makes the code easier to read and fix if something goes wrong.


Occu_data <- dplyr::arrange(Occu_data, grid.id) |>
  dplyr::group_by(grid.id) |>
  dplyr::mutate(survey_number = dplyr::row_number()) |>
  dplyr::ungroup()




############################################################################################################
#------------------------------------------ Detection history (y) ------------------------------------------#
############################################################################################################

# Here we reshape the data so that each site becomes one row and each visit becomes one column. 
# The values show whether pangolins were detected (1), not detected (0), or not recorded (NA). 
# This is the main structure that occupancy models in unmarked require. The approach is mostly the same as before, 
# but with a little added safeguard to make sure R does not drop the matrix shape if there is only one visit.

detection_wide <- tidyr::pivot_wider(
  Occu_data,
  id_cols = grid.id,
  names_from = survey_number,
  values_from = detection_binary,
  names_prefix = "survey"
)
y <- as.matrix(detection_wide[, -1, drop = FALSE])
rownames(y) <- detection_wide$grid.id


############################################################################################################
#------------------------------------------ Prepare the site covariates ------------------------------------------#
############################################################################################################

# This step takes one set of site characteristics per location, such as elevation and road length, 
# and ensures they are in the same order as the detection data. It then scales the values so all covariates 
# are measured on a similar scale. So instead of elevation ranging from 500 to 3,000 and road length from 0 to 10, 
# they’re both centred around 0 and spread across a similar range. Scaling the covariates seems to be quite common in 
# occupancy models because it add numerical stability (models can struggle to estimateparameters accurately if some 
# variables have very large numbers compared with others. Standardising them makes the optimisation easier and helps the
# model converge reliably)


# The other main change here is that we now explicitly join the covariates back to the
# detection data by the site ID. The old version assumed that the rows were already in the right order, which is risky 
# because unmarked matches rows by position, not by name.

site_covs_df <- Occu_data |>
  dplyr::distinct(grid.id, .keep_all = TRUE) |>
  dplyr::select(grid.id, mean.elevation, river.length.sum, road.length.sum,
                village.number, slope.mean) |>
  dplyr::right_join(detection_wide[, "grid.id", drop = FALSE], by = "grid.id") |>
  dplyr::select(-grid.id)

site_covs_df <- as.data.frame(scale(site_covs_df))




############################################################################################################
#------------------------------------------ Prepare the observed covariates ------------------------------------------#
############################################################################################################


# # This section cleans up the interview-level data. It groups "visit reasons" into a few simple categories, 
# standardises the effort variables like "total days per year" and "distance", and reshapes everything so that it 
# lines up with the detection matrix. It also converts the "season" variable into clear labels (season1, season2, season3) 
# and ensures that all columns share the same levels and reference category. The main difference is that we now scale the 
# continuous variables before reshaping, so a one-unit change means the same thing across all visits. We also realign
# everything with the detection table, which prevents mismatched rows.

# ---- 3. OBS covariates. Regroup, SCALE IN LONG, widen, align, set baselines ----
# Visit reason regrouping
Occu_data <- dplyr::mutate(
  Occu_data,
  visit.reason.regrouped = dplyr::case_when(
    visit.reason %in% c("hunting","trapping","fishing","frog_coll","jhum","rice","vegetable") ~ "subsistence",
    visit.reason %in% c("cardamom","ginger","orange_farm","pepper","palm","sugarcane","silkworm","rubber","tea") ~ "agriculture_horticulture",
    visit.reason %in% c("cane_coll","bamboo","etkum_coll","firewood_coll","fodder_coll","mushroom_coll","timber_coll","tuber_coll","toko_coll","ant_coll") ~ "forest_resource_collection",
    visit.reason %in% c("village","transit","FD_petrol","electricity","road_const","fencing","mithun","cow_grz") ~ "village_transit",
    TRUE ~ "other"
  ),
  # scale continuous obs covariates in long format
  total.day.per.year_sc = as.numeric(scale(total.day.per.year)),
  distance_sc           = as.numeric(scale(distance)),
  # make an explicit season factor from numeric 1,2,3
  season_factor         = factor(season, levels = c(1,2,3),
                                 labels = c("season1","season2","season3"))
)

# visit.reason wide, aligned to detection_wide
visit_reason_wide <- Occu_data |>
  dplyr::select(grid.id, survey_number, visit.reason.regrouped) |>
  tidyr::pivot_wider(names_from = survey_number, values_from = visit.reason.regrouped, names_prefix = "survey") |>
  dplyr::right_join(detection_wide[, "grid.id", drop = FALSE], by = "grid.id") |>
  dplyr::select(-grid.id)

# unify factor levels across columns, set "subsistence" as baseline if present
all_levels <- unique(na.omit(unlist(visit_reason_wide)))
visit_reason_wide <- as.data.frame(lapply(visit_reason_wide, function(col) {
  factor(col, levels = all_levels)
}))
if ("subsistence" %in% levels(visit_reason_wide[[1]])) {
  visit_reason_wide[] <- lapply(visit_reason_wide, function(col) stats::relevel(col, ref = "subsistence"))
}

# total.day.per.year_sc wide, aligned
day_wide <- Occu_data |>
  dplyr::select(grid.id, survey_number, total.day.per.year_sc) |>
  tidyr::pivot_wider(names_from = survey_number, values_from = total.day.per.year_sc, names_prefix = "survey") |>
  dplyr::right_join(detection_wide[, "grid.id", drop = FALSE], by = "grid.id") |>
  dplyr::select(-grid.id)

# distance_sc wide, aligned
distance_wide <- Occu_data |>
  dplyr::select(grid.id, survey_number, distance_sc) |>
  tidyr::pivot_wider(names_from = survey_number, values_from = distance_sc, names_prefix = "survey") |>
  dplyr::right_join(detection_wide[, "grid.id", drop = FALSE], by = "grid.id") |>
  dplyr::select(-grid.id)

# season wide, aligned, unify levels and set baseline
season_wide <- Occu_data |>
  dplyr::select(grid.id, survey_number, season_factor) |>
  tidyr::pivot_wider(names_from = survey_number, values_from = season_factor, names_prefix = "survey") |>
  dplyr::right_join(detection_wide[, "grid.id", drop = FALSE], by = "grid.id") |>
  dplyr::select(-grid.id)

season_levels <- unique(na.omit(unlist(season_wide)))
season_wide <- as.data.frame(lapply(season_wide, function(col) factor(col, levels = season_levels)))
season_wide[] <- lapply(season_wide, function(col) stats::relevel(col, ref = "season1"))



############################################################################################################
#------------------------------------------ Create unmarked data objects  ------------------------------------------#
############################################################################################################

#  Once everything is cleaned, we combine the detection data, site variables, and interview variables into two data objects 
# that the model can use. One object models detection using visit reason, and the other uses season instead. The process is the
# same as before, but now the data are ordered and aligned, so the model is reading the correct values for each site
# and visit. (Some of these changes might not be entirely necessary, depending on the cleanliness of the data, but it's good practice
# to introduce these kinds of safeguards anyway)


obsCovs_visit  <- list(
  visit.reason      = visit_reason_wide,
  total.day.per.year = day_wide,
  distance          = distance_wide
)
obsCovs_season <- list(
  season            = season_wide,
  total.day.per.year = day_wide,
  distance          = distance_wide
)

umf_visit  <- unmarkedFrameOccu(y = y, siteCovs = site_covs_df, obsCovs = obsCovs_visit)
umf_season <- unmarkedFrameOccu(y = y, siteCovs = site_covs_df, obsCovs = obsCovs_season)


############################################################################################################
#------------------------------------------ Fit the models ------------------------------------------#
############################################################################################################

# Here we run three occupancy models. The first model has no covariates and acts as a simple baseline. 
# The second model uses visit reason, total days, and distance to explain detection. 
# The third swaps visit reason for season. In all three models, occupancy is explained by site-level variables 
# such as elevation and slope. The difference from before is that we have added slope.mean into the formula, 
# which was collected but never used in the original script. 


m0       <- unmarked::occu(~ 1 ~ 1, data = umf_visit)

m_visit  <- unmarked::occu(
  ~ visit.reason + total.day.per.year + distance
  ~ mean.elevation + river.length.sum + road.length.sum + village.number + slope.mean,
  data = umf_visit
)

m_season <- unmarked::occu(
  ~ season + total.day.per.year + distance
  ~ mean.elevation + river.length.sum + road.length.sum + village.number + slope.mean,
  data = umf_season
)


############################################################################################################
#------------------------------------------ Summaries and predictions ------------------------------------------#
############################################################################################################

# This section prints the results of each model and then calculates the average probabilities of occupancy (ψ) and detection (p).
# For the simplest model, no extra input is needed. For the others, predictions are made at average conditions and at the 
# chosen baseline categories. The original code included a  placeholder that did not actually affect predictions.
# This version uses  input values that match the model setup.


summary(m0)
summary(m_visit)
summary(m_season)

# Intercept-only predictions. No newdata needed.
predict(m0, type = "state")  # ψ
predict(m0, type = "det")    # p

# Predictions at scaled means (0) and baseline levels
pred_occ_visit  <- predict(m_visit,  type = "state",
                           newdata = data.frame(mean.elevation=0, river.length.sum=0, road.length.sum=0,
                                                village.number=0, slope.mean=0))

pred_det_visit  <- predict(m_visit,  type = "det",
                           newdata = data.frame(
                             visit.reason = stats::relevel(factor("subsistence",
                                                                  levels = levels(visit_reason_wide[[1]])),
                                                           ref = "subsistence"),
                             total.day.per.year = 0, distance = 0))

pred_occ_season <- predict(m_season, type = "state",
                           newdata = data.frame(mean.elevation=0, river.length.sum=0, road.length.sum=0,
                                                village.number=0, slope.mean=0))

pred_det_season <- predict(m_season, type = "det",
                           newdata = data.frame(
                             season = stats::relevel(factor("season1",
                                                            levels = levels(season_wide[[1]])),
                                                     ref = "season1"),
                             total.day.per.year = 0, distance = 0))

pred_occ_visit; pred_det_visit; pred_occ_season; pred_det_season





############################################################################################################
#------------------------------------------ Model comparison ------------------------------------------#
############################################################################################################


# This part compares all three models using AIC to see which one fits the data best. 
# It also runs a goodness-of-fit test to check whether the model assumptions hold. Adding these tests makes the results 
# more reliable and lets you see whether the model explains the data well or if more variation needs to be accounted for.


AICcmodavg::aictab(list(m0 = m0, m_visit = m_visit, m_season = m_season))

# MacKenzie–Bailey GOF and overdispersion ĉ for the main models
set.seed(1)
gof_visit  <- AICcmodavg::mb.gof.test(m_visit,  nsim = 100)
gof_season <- AICcmodavg::mb.gof.test(m_season, nsim = 100)
gof_visit; gof_season

# If ĉ > 1, compute QAIC
# AICcmodavg::aictab(list(m_visit = m_visit, m_season = m_season), c.hat = gof_visit$chat)


############################################################################################################
#------------------------------------------ Plot ψ and p with confidence intervals ----------------------------------#
############################################################################################################

# Similarly as before, here we create a simple bar chart that shows the estimated occupancy and detection probabilities
# for both main models, including their 95% confidence intervals. This makes it easier to see and explain the results. 


plot_data <- data.frame(
  Model    = rep(c("Visit reason","Season"), each = 2),
  Type     = rep(c("Occupancy (ψ)","Detection (p)"), 2),
  Estimate = c(pred_occ_visit$Predicted, pred_det_visit$Predicted,
               pred_occ_season$Predicted, pred_det_season$Predicted),
  Lower    = c(pred_occ_visit$lower,     pred_det_visit$lower,
               pred_occ_season$lower,    pred_det_season$lower),
  Upper    = c(pred_occ_visit$upper,     pred_det_visit$upper,
               pred_occ_season$upper,    pred_det_season$upper)
)

ggplot2::ggplot(plot_data, ggplot2::aes(x = Type, y = Estimate, fill = Model)) +
  ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.7), width = 0.6) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = Lower, ymax = Upper),
                         position = ggplot2::position_dodge(width = 0.7), width = 0.2) +
  ggplot2::ylim(0, 1) +
  ggplot2::labs(title = "Occupancy and detection estimates with 95% CIs", y = "Probability", x = "") +
  ggplot2::theme_minimal() +
  ggplot2::theme(text = ggplot2::element_text(size = 12), legend.title = ggplot2::element_blank())



############################################################################################################
#------------------------------------------ Optional sensitivity analysis ----------------------------------#
############################################################################################################

# Finally, we count how many times each site was visited and filter out sites with fewer than three interviews. 
# You can rerun the same models on this subset to see if the results change. This part is new, but it is a useful 
# test because sites with only one or two visits provide very little information about detection. Running the models again 
# on sites with better coverage helps you check whether your findings are robust.


visits_per_site <- Occu_data |>
  dplyr::count(grid.id, name = "nvisits")

keep_sites <- dplyr::filter(visits_per_site, nvisits >= 3)$grid.id
Occu_data_ge3 <- dplyr::filter(Occu_data, grid.id %in% keep_sites)





#### TO DO (NOTES FOR MYSELF)


# incorporate comments on diff covariates (e.g. season - handling that appropriately atm?)
# go through analysis docs and reproduce results
# push data further 
# explore diff ways of using covariates



