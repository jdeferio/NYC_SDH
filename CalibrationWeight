library("sampling")

# -------------------- Prepare Data Sets -----------------------
# --------- Assumes that you have already curated sample df to mirror size and structure of population df ----------

sample_data_path = "/Users/Documents/XXXXX.csv"
population_data_path = "/Users/Documents/XXXXX.csv"

output_file_path = "/Users/Documents/XXXXX.csv"

samp <- read.csv(sample_data_path, sep = ',')
pop <- read.csv(population_data_path, sep = ',')

# Converts samp and pop from dataframes to a matrices, samp_ and pop_
samp_ <- as.matrix(sapply(samp, as.numeric))
pop_ <- as.matrix(sapply(pop, as.numeric))

# Sets initial weights to 1 for the number of rows your dataset
d = rep(1, nrow(samp_))

total = list()
for (i in 1:ncol(pop_)) {
    total[i] = sum(pop_[,i])
}
total = unlist(total)


# -------------------- Calculate G Weights -----------------------

glist = list()
for (i in 1:ncol(pop_)) {
    glist[[i]] = calib(samp_[,i], d, total[i], method="logit")
}

# -------------------- Check Calibrated G Weights-----------------------

gcal_list = list()
for (i in 1:ncol(pop_)) {
    if(is.null(glist[[i]]))
        gcal_list[[i]] = rep(1, nrow(pop_))
    else gcal_list[[i]] = (c((glist[[i]]) %*% samp_[,i]))
}

# -------------------- Convert Glist to Matrix -----------------------

gmat = matrix(nrow = nrow(pop_), ncol = ncol(pop_))
for (i in 1:length(glist)) {
  if(is.null(glist[[i]])) {
    gmat[,i] = rep(1,nrow(pop_))
  } else {
    gmat[,i] = glist[[i]]
  }
}

# ------------------------ Apply New G Weights to Sample Data ------------------
samp_calib = samp_*gmat
write.csv(samp_calib, output_file_path)
