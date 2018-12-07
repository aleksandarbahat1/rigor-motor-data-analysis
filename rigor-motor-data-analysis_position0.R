# # # # # # # # # # # # # # #
# RIGOR MOTOR DATA ANALYSIS #
# POSITION0                 #
# # # # # # # # # # # # # # #

#### Load packages ####

library(gtools)

#### Read data and write position vs. time to text files (shifted to start at t,x,y=0) ####

filelist = mixedsort(list.files("data"))

maxes_position0 = list()

for (filename in filelist) {
  data = read.table(paste(getwd(), "/data/", filename, sep = ""), skip = 2)
  t = data[,2] - data[1,2]
  x0 = data[,3] - data[1,3]
  y0 = data[,4] - data[1,4]
  
  position0_filename = paste("position0_", substring(filename, 6), sep = "")
  full_position0_filename = paste(getwd(), "/position0/", position0_filename, sep = "")
  position0 = data.frame(t, x0, y0)
  maxes_position0[[position0_filename]] = max(c(abs(x0), abs(y0)))
  
  write.table(position0, file = full_position0_filename, sep = "\t", col.names = F, row.names = F)
  write.table(maxes_position0, file = "maxes_position0.txt", sep = "\t", col.names = F, row.names = F)
}
