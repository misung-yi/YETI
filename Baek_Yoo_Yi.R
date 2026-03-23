########### Build the YETI scoring formula based on training data
###################################################
# 0. Load training data
# : Please load your own training dataset
###################################################
# df_train <- read.csv("your_path/df_train.csv")

###################################################
# 1. Variable name standardization function
# : Function to standardize variable names
###################################################
std_name <- function(x){
  x <- tolower(trimws(x))
  x <- gsub("\\s+", "", x)
  x <- gsub("^(fear|fea)_?(\\d+)$", "fea\\2", x)
  x <- gsub("^(agg)_?(\\d+)$",  "agg\\2", x)
  x <- gsub("^(att|attach)_?(\\d+)$", "att\\2", x)
  x <- gsub("^(sep)_?(\\d+)$",  "sep\\2", x)
  x <- gsub("^(exc)_?(\\d+)$",  "exc\\2", x)
  x <- gsub("^(mis)_?(\\d+)$",  "mis\\2", x)
  x <- gsub("^(tra)_?(\\d+)$",  "tra\\2", x)
  x
}

names(df_train) <- vapply(names(df_train), std_name, character(1))
head(df_train)

###################################################
# 2. Define items based on the paper
# : Define the items used for each personality category
###################################################
item_list <- list(
  EI = c("exc63","exc67","att70","att71","mis91","mis92",
         "fea36","fea37","fea39","fea40","fea42","fea47"),
  
  TA = c("tra1","tra4","sep60",
         "mis74","mis75","mis76","mis77","mis78",
         "mis80","mis81","mis83","mis86","mis87","mis97"),
  
  SU = c("fea36","fea37","fea39","fea40","fea45","fea46","fea47",
         "fea52","fea53",
         "sep54","sep55","sep56","sep57","sep58","sep59",
         "att68","att69","att72","att73",
         "mis90","mis97"),
  
  BC = c("tra2","tra3","tra5","tra6","tra7","mis93")
)

###################################################
# 3. Number of PCs to retain
# : Number of principal components to use
###################################################
n_pc <- c(EI = 4, TA = 7, SU = 7, BC = 3)

###################################################
# 4. Set directions
###################################################
sign_list <- list(
  EI = c(PC1 = -1, PC2 = -1, PC3 =  1, PC4 = -1),
  TA = c(PC1 =  1, PC2 =  1, PC3 =  1, PC4 =  1, PC5 = -1, PC6 =  1, PC7 =  1),
  SU = c(PC1 =  1, PC2 = -1, PC3 =  1, PC4 = -1, PC5 = -1, PC6 =  1, PC7 = -1),
  BC = c(PC1 =  1, PC2 =  1, PC3 = -1)
)

###################################################
# 5. Function to build the scoring formula (model)
# : Function that creates the full YETI scoring formula using training data
###################################################
build_yeti_model <- function(df_train, item_list, n_pc, sign_list) {
  
  model <- lapply(names(item_list), function(trait) {
    # Repeat the same procedure for EI, TA, SU, and BC
    
    vars <- item_list[[trait]]
    # Retrieve the items to use
    k    <- n_pc[[trait]]
    
    dat <- na.omit(df_train[, vars, drop = FALSE])
    # Select only the required items from the training data
    pc  <- prcomp(dat, center = TRUE, scale. = TRUE)
    # Perform PCA after standardization
    
    rotation_k <- pc$rotation[, 1:k, drop = FALSE]
    colnames(rotation_k) <- paste0("PC", 1:k)
    # Store the rotation matrix (loadings) for the retained PCs
    
    pc_scores <- pc$x[, 1:k, drop = FALSE]
    # Store the PC scores themselves
    colnames(pc_scores) <- paste0("PC", 1:k)
    
    lambda <- pc$sdev[1:k]^2
    # Compute variances
    
    weight <- lambda / sum(lambda)
    # Compute weight values
    names(weight) <- paste0("PC", 1:k)
    
    sgn <- sign_list[[trait]][paste0("PC", 1:k)]
    beta <- weight * sgn
    
    list(
      vars     = vars,
      center   = pc$center,
      scale    = pc$scale,
      rotation = rotation_k,
      pc_mean  = colMeans(pc_scores),
      pc_sd    = apply(pc_scores, 2, sd),
      weight   = weight,
      sign     = sgn,
      beta     = beta
    )
  })
  
  names(model) <- names(item_list)
  model
}

###################################################
# 6. Build model
###################################################
yeti_model <- build_yeti_model(
  df_train  = df_train,
  item_list = item_list,
  n_pc      = n_pc,
  sign_list = sign_list
)

########### Calculate YETI scores when new data are provided
###################################################
# 0. Load new data
###################################################
# Please load your own dataset
# new_data <- read.csv("your_path/new_data.csv")

###################################################
# 1. Variable name standardization function
###################################################
std_name <- function(x){
  x <- tolower(trimws(x))
  x <- gsub("\\s+", "", x)
  x <- gsub("^(fear|fea)_?(\\d+)$", "fea\\2", x)
  x <- gsub("^(agg)_?(\\d+)$",  "agg\\2", x)
  x <- gsub("^(att|attach)_?(\\d+)$", "att\\2", x)
  x <- gsub("^(sep)_?(\\d+)$",  "sep\\2", x)
  x <- gsub("^(exc)_?(\\d+)$",  "exc\\2", x)
  x <- gsub("^(mis)_?(\\d+)$",  "mis\\2", x)
  x <- gsub("^(tra)_?(\\d+)$",  "tra\\2", x)
  x
}

names(new_data) <- vapply(names(new_data), std_name, character(1))

###################################################
# 2. Function to calculate the score for one trait
###################################################
calc_trait_score <- function(row_df, trait, model) {
  obj <- model[[trait]]
  
  # Return NA if required items are missing
  if (!all(obj$vars %in% names(row_df))) return(NA_real_)
  
  x <- as.numeric(row_df[1, obj$vars, drop = FALSE])
  names(x) <- obj$vars
  
  # Return NA if there are missing values
  if (any(is.na(x))) return(NA_real_)
  
  # Standardize items
  z_item <- (x - obj$center) / obj$scale
  
  # Reorder to match the PCA rotation order
  z_item <- z_item[rownames(obj$rotation)]
  
  # Convert to a 1-row matrix and compute PC scores
  z_item_mat <- matrix(z_item, nrow = 1)
  colnames(z_item_mat) <- rownames(obj$rotation)
  
  pc_raw <- z_item_mat %*% obj$rotation
  # Formula used to calculate principal component scores
  pc_raw <- as.numeric(pc_raw)
  names(pc_raw) <- colnames(obj$rotation)
  
  # Standardize PC scores
  z_pc <- (pc_raw - obj$pc_mean[names(pc_raw)]) / obj$pc_sd[names(pc_raw)]
  
  # Final score
  sum(obj$beta[names(z_pc)] * z_pc)
}

###################################################
# 3. Function to calculate scores for all individuals
###################################################
score_all_dogs <- function(df, model) {
  EI_score <- apply(df, 1, function(x) calc_trait_score(as.data.frame(t(x), stringsAsFactors = FALSE), "EI", model))
  TA_score <- apply(df, 1, function(x) calc_trait_score(as.data.frame(t(x), stringsAsFactors = FALSE), "TA", model))
  SU_score <- apply(df, 1, function(x) calc_trait_score(as.data.frame(t(x), stringsAsFactors = FALSE), "SU", model))
  BC_score <- apply(df, 1, function(x) calc_trait_score(as.data.frame(t(x), stringsAsFactors = FALSE), "BC", model))
  
  out <- df
  out$EI_score <- EI_score
  out$TA_score <- TA_score
  out$SU_score <- SU_score
  out$BC_score <- BC_score
  
  out
}

###################################################
# 4. Classification function using cutoff = 0
###################################################
classify_EI <- function(x) ifelse(is.na(x), NA, ifelse(x >= 0, "E", "I"))
classify_TA <- function(x) ifelse(is.na(x), NA, ifelse(x >= 0, "T", "A"))
classify_SU <- function(x) ifelse(is.na(x), NA, ifelse(x >= 0, "S", "U"))
classify_BC <- function(x) ifelse(is.na(x), NA, ifelse(x >= 0, "B", "C"))

###################################################
# 5. Function to generate the final YETI type
###################################################
make_yeti_type <- function(EI, TA, SU, BC) {
  ifelse(is.na(EI) | is.na(TA) | is.na(SU) | is.na(BC),
         NA,
         paste0(EI, TA, SU, BC))
}

###################################################
# 6. Calculate overall scores
###################################################
result <- score_all_dogs(new_data, yeti_model)

###################################################
# 7. Classification by trait
###################################################
result$EI_class <- classify_EI(result$EI_score)
result$TA_class <- classify_TA(result$TA_score)
result$SU_class <- classify_SU(result$SU_score)
result$BC_class <- classify_BC(result$BC_score)

###################################################
# 8. Generate final type
###################################################
result$YETI_type <- make_yeti_type(
  result$EI_class,
  result$TA_class,
  result$SU_class,
  result$BC_class
)

###################################################
# 9. Save results
###################################################
# Please specify your own file path when saving the results
# Example:
# write.csv(
#   result,
#   "your_path/new_data_scored.csv",
#   row.names = FALSE,
#   fileEncoding = "UTF-8"
# )

###################################################
# 10. Check
###################################################
head(result[, c("EI_score", "TA_score", "SU_score", "BC_score",
                "EI_class", "TA_class", "SU_class", "BC_class",
                "YETI_type")])

table(result$YETI_type, useNA = "ifany")