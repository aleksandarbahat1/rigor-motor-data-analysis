# # # # # # # # # # # # # # #
# RIGOR MOTOR DATA ANALYSIS #
# POSITION                  #
# # # # # # # # # # # # # # #

#### Load packages ####

library(gtools)

#### Change coordinates, write new data to files ####

# Calculate best fit line (x+by+c=0) for position data for each file, i.e., "locate the microtubules"
# Change coordinates for each file so axes are parallel/perpendicular to microtubule
# Store resulting (b,c) pairs in list of vectors and transformed data in list of data frames

# Note: We model x as response to linear predictor y. Since most data sets are nearly "vertical"
#  (much thinner in x direction than y direction) this works pretty well. Trying to model y
#  as response to linear predictor x fails, because model becomes too sensitive to "clustering"
#  in certain regions, which is not related to geometry of MT.
# Future task: Find a more robust "direction-independent" way to do this!

filelist = mixedsort(list.files("position0"))

MTline_parameters = list()
maxes_position = list()

for (filename in filelist) {
  position0 = read.table(paste(getwd(), "/position0/", filename, sep = ""))
  t = position0[,1]
  x0 = position0[,2]
  y0 = position0[,3]
  
  MTline = lm(x0 ~ y0, data = position0)
  c = -MTline$coefficients[1]
  b = -MTline$coefficients[2]
  x = 1/sqrt(1+b^2) * (b*x0 - y0)
  y = 1/sqrt(1+b^2) * (x0 + b*y0 + c)
  
  position_filename = paste("position_", substring(filename, 11), sep = "")
  full_position_filename = paste(getwd(), "/position/", position_filename, sep = "")
  position = data.frame(t, x, y)
  MTline_parameters[[position_filename]] = c(b, c)
  maxes_position[[position_filename]] = max(c(abs(x), abs(y)))
  
  write.table(position, file = full_position_filename, sep = "\t", col.names = F, row.names = F)
  write.table(MTline_parameters, file = "MTline_parameters.txt", sep = "\t", col.names = F, row.names = F)
  write.table(maxes_position, file = "maxes_position.txt", sep = "\t", col.names = F, row.names = F)
}
