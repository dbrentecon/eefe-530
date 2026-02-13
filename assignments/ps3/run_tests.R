# autograding/run_tests.R
# Minimal checks for PS2 Matching
# Usage (from repo root): Rscript autograding/run_tests.R

required <- c("att_exp","att_naive_psid","att_nn","att_ker","bal_nn")

missing <- required[!vapply(required, exists, logical(1), inherits = TRUE)]
if(length(missing)>0){
  stop("Missing required objects: ", paste(missing, collapse=", "))
}

vals <- mget(c("att_exp","att_naive_psid","att_nn","att_ker"))
if(any(!vapply(vals, is.numeric, logical(1)))) stop("ATT objects must be numeric.")

cat("All required objects exist and are numeric.\n")
print(vals)
