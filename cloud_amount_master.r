# cloud_amount_master.r

options(scipen=3)
require(doParallel)
registerDoParallel(cores = 3)

master_dir = "/home/jim/Dropbox/Projects/cloud_amount"
data_dir = "/media/jim/FAT323/Projects/cloud_amount"
# data_dir = "/home/jim/Dropbox/Python/cloud_amount/data"

# --------------------------------------------------------------

setwd(master_dir)
source("cloud_amount_data.r")

# --------------------------------------------------------------

setwd(master_dir)
source("cloud_amount_analysis.r")

# --------------------------------------------------------------