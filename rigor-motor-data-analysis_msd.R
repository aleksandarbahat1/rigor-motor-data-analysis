# # # # # # # # # # # # # # #
# RIGOR MOTOR DATA ANALYSIS #
# MSD                       #
# # # # # # # # # # # # # # #

#### Load packages ####

library(gtools)

#### Define MSD functions (msd_partial, msd_total) ####

msd_partial = function(x, n) {
  sqdist = diff(x, lag = n)^2
  return(mean(sqdist))
}

msd_total = function(x, y, n) {
  sqdist = diff(x, lag = n)^2 + diff(y, lag = n)^2
  return(mean(sqdist))
}

#### Write MSD data (in x and y directions, and total) to files ####

filelist = mixedsort(list.files("position"))

maxes_msd = list()

for (filename in filelist) {
  position = read.table(paste(getwd(), "/position/", filename, sep = ""))
  t = position[,1]
  x = position[,2]
  y = position[,3]
  
  n = (1:(length(t) - 1))
  msd_x = numeric(length(n))
  msd_y = numeric(length(n))
  msd_xy = numeric(length(n))
  for (k in n) {
    msd_x[k] = msd_partial(x, k)
    msd_y[k] = msd_partial(y, k)
    msd_xy[k] = msd_total(x, y, k)
  }

  msd_filename = paste("msd_", substring(filename, 10), sep = "")
  full_msd_filename = paste(getwd(), "/msd/", msd_filename, sep = "")
  msd = data.frame(n, msd_x, msd_y, msd_xy)
  maxes_msd[[msd_filename]] = c(max(c(msd_x[1:length(n)/3], msd_y[1:length(n)/3])),
                                max(msd_xy[1:length(n)/3]))

  write.table(msd, file = full_msd_filename, sep = "\t", col.names = F, row.names = F)
  write.table(maxes_msd, file = "maxes_msd.txt", sep = "\t", col.names = F, row.names = F)
}
