# Fit Huggins closed-pop capture-recapture model
# Use example mouse data to compare to example in Amstrup et al 2005 Handbook


# Jason Carlisle
# Wyoming Game and Fish Department

require(readxl)
require(dplyr)
require(mra)  # version 2.16.11


# Read data ----
# Mouse capture-recapture dataset, Table 4.2 (page 75) in Amstrup et al 2005
# 38 unique mice caught during 6 capture occasions, includes three covariates
# Note, age=semi-adult have been reclassified as age=adult as described on
# page 74 of Amstrup et al 2005

mouse <- read_excel("mouse.xlsx")


# Prep data ----
# Follows conventions of help doc of mra::F.huggins.estim
# Capture (encounter) history matrix
ch <- mouse %>%
  select(occ1,
         occ2,
         occ3,
         occ4,
         occ5,
         occ6) %>%
  as.matrix()

# Time index
ct <- as.factor(1:ncol(ch))
attr(ct, "nan") <- nrow(ch)

# Individual heterogeneity index
h <- 1:nrow(ch)
attr(h, "ns") <- ncol(ch)

# Covariate sex
(sex <- mouse %>%
  pull(sex) %>%
  rep(times = ncol(ch)) %>%
  matrix(nrow = nrow(ch),
            ncol = ncol(ch)))

(sexMale <- ifelse(sex == "m", 1, 0))

# Covariate age
(age <- mouse %>%
  pull(age) %>%
  rep(times = ncol(ch)) %>%
  matrix(nrow = nrow(ch),
         ncol = ncol(ch)))

(ageYoung <- ifelse(age == "y", 1, 0))

# Covariate weight
(weight <- mouse %>%
  pull(weight) %>%
  rep(times = ncol(ch)) %>%
  matrix(nrow = nrow(ch),
         ncol = ncol(ch)))



# Fit Huggins models ----
# Fits models using mra::F.huggins.estim
# Compare results to Table 4.3 (page 77) of Amstrup et al 2005

# Constant model (M0)
# Converges and matches textbook
(M0 <- F.huggins.estim(capture = ~ 1,
                      recapture = NULL,
                      histories = ch))

# Time varying model (Mt)
# Converges and matches textbook
(Mt <- F.huggins.estim(capture = ~ tvar(ct),
                      recapture = NULL,
                      histories = ch))

# Additive behavioral model (Mb)
# Converges and matches textbook
(Mb <- F.huggins.estim(capture = ~ 1,
                       recapture = ~ 1,
                       histories = ch))

# Individual effects model 
# Failed:  Message =  FAILURE: Likelihood evaluated too many times
(Mh <- F.huggins.estim(capture = ~ ivar(h),
                       recapture = NULL,
                       histories = ch))

# Time and Behavioral model (Mtb)
# Converges and matches textbook
(Mtb <- F.huggins.estim(capture = ~ tvar(ct),
                        recapture = ~ 1,
                        histories = ch))

# Covars
# ERROR
# Error in `[[<-.data.frame`(`*tmp*`, i, value = c(2L, 2L, 2L, 2L, 2L, 1L,  : 
#                                                    replacement has 228 rows, data has 38
(Mcovars <- F.huggins.estim(capture = ~ age + sex + weight,
                            recapture = ~ age + sex + weight,
                            histories = ch))

# END