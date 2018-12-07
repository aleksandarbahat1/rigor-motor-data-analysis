# # # # # # # # # # # # # # #
# RIGOR MOTOR DATA ANALYSIS #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# To calculate MSDs or create plots, uncomment desired section  #
#  (make sure to choose which files to use before running)      #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#### Load packages ####

library(ggplot2)
library(gtools)

#### Read from files, store original data (data_frames, max_xyshift) ####

# Read data from each file
# Store t, x(t), y(t) in list of data frames

filelist = mixedsort(list.files("data"))

data_frames = list(length(filelist))
maxes_xyshift = numeric(length(filelist))

for (filenumber in (1:length(filelist))) {
  data = read.table(paste(getwd(), "/data/", filelist[filenumber], sep = ""), skip = 2)
  t = data[,2]
  x = data[,3]
  y = data[,4]
  x_shift = x - x[1]
  y_shift = y - y[1]

  data_frames[[filenumber]] = data.frame(t, x_shift, y_shift)
  maxes_xyshift[filenumber] = max(c(abs(x_shift), abs(y_shift)))
}

# Set scale for plots of original data (same for each file, to aid comparison)

max_xyshift = max(maxes_xyshift)
remove(maxes_xyshift)

#### Change coordinates, store new data (MTline_parameters_all, new_data_frames, new_max_xyshift) ####

# Calculate best fit line (x+by+c=0) for position data for each file, i.e., "locate the microtubules"
# Change coordinates for each file so axes are parallel/perpendicular to microtubule
# Store resulting (b,c) pairs in list of vectors and transformed data in list of data frames

# Note: We model x as response to linear predictor y. Since most data sets are nearly "vertical"
#  (much thinner in x direction than y direction) this works pretty well. Trying to model y
#  as response to linear predictor x fails, because model becomes too sensitive to "clustering"
#  in certain regions, which is not related to geometry of MT.
# Future task: Find a more robust "direction-independent" way to do this!

MTline_parameters_all = list(length(filelist))
new_data_frames = list(length(filelist))
new_maxes_xyshift = numeric(length(filelist))

for (filenumber in (1:length(filelist))) {
  data_frame = data_frames[[filenumber]]
  t = data_frame[[1]]
  x_shift = data_frame[[2]]
  y_shift = data_frame[[3]]
  
  MTline = lm(x_shift ~ y_shift, data = data_frame)
  c = -MTline$coefficients[1]
  b = -MTline$coefficients[2]
  
  new_x_shift = 1/sqrt(1+b^2) * (b*x_shift - y_shift)
  new_y_shift = 1/sqrt(1+b^2) * (x_shift + b*y_shift + c)
  
  MTline_parameters_all[[filenumber]] = c(b,c)
  new_data_frames[[filenumber]] = data.frame(t, new_x_shift, new_y_shift)
  new_maxes_xyshift[filenumber] = max(c(abs(new_x_shift), abs(new_y_shift)))
}

# Set scale for plots of transformed data (same for every file, to aid comparison)

new_max_xyshift = max(new_maxes_xyshift)
remove(new_maxes_xyshift)

#### Define MSD functions (msd_x, msd), store MSD data (msd_x_all, msd_y_all, msd_all, max_msd_xy, max_msd) ####

# # Function to calculate pathwise mean squared displacement in one coordinate
# 
# msd_x = function(X, n) {
#   sqdist = diff(X, lag = n)^2
#   return(mean(sqdist))
# }
# 
# # Function to calculate pathwise total mean squared displacement
# 
# msd = function(X, Y, n) {
#   sqdist = diff(X, lag = n)^2 + diff(Y, lag = n)^2
#   return(mean(sqdist))
# }
# 
# # Store MSD (in transformed x and y directions, and total) data for each file in separate lists of vectors
# 
# msd_x_all = list(length(filelist))
# msd_y_all = list(length(filelist))
# msd_all = list(length(filelist))
# maxes_msd_xy = numeric(length(filelist))
# maxes_msd = numeric(length(filelist))
# 
# for (filenumber in (1:length(filelist))) {
#   new_data_frame = new_data_frames[[filenumber]]
#   t = new_data_frame[[1]]
#   new_x_shift = new_data_frame[[2]]
#   new_y_shift = new_data_frame[[3]]
# 
#   msd_x_individual = numeric(length(t) - 1)
#   for (n in (1:(length(t) - 1))) {
#     msd_x_individual[n] = msd_x(new_x_shift, n)
#   }
#   msd_y_individual = numeric(length(t) - 1)
#   for (n in (1:(length(t) - 1))) {
#     msd_y_individual[n] = msd_x(new_y_shift, n)
#   }
#   msd_individual = numeric(length(t) - 1)
#   for (n in (1:(length(t) - 1))) {
#     msd_individual[n] = msd(new_x_shift, new_y_shift, n)
#   }
# 
#   msd_x_all[[filenumber]] = msd_x_individual
#   msd_y_all[[filenumber]] = msd_y_individual
#   msd_all[[filenumber]] = msd_individual
#   maxes_msd_xy[filenumber] = max(c(msd_x_individual[1:length(t)/3], msd_y_individual[1:length(t)/3]))
#   maxes_msd[filenumber] = max(msd_individual[1:length(t)/3])
# }
# 
# # Set scale for plots of MSD (same for every file, to aid comparison)
# 
# max_msd_xy = max(maxes_msd_xy)
# max_msd = max(maxes_msd)
# remove(maxes_msd_xy)
# remove(maxes_msd)

#### Choose which files to use for plots (plotfiles) ####

plotfiles = (1:24)

#### Plot original data, with microtubule line displayed ####

# for (filenumber in plotfiles) {
#   data_frame = data_frames[[filenumber]]
#   x_shift = data_frame[[2]]
#   y_shift = data_frame[[3]]
#   MTline_parameters_individual = MTline_parameters_all[[filenumber]]
#   b = MTline_parameters_individual[1]
#   c = MTline_parameters_individual[2]
# 
#   plot(x_shift, y_shift, type = "l",
#        xlim = c(-max_xyshift, max_xyshift), ylim = c(-max_xyshift, max_xyshift))
#   lines(x_shift, (-1/b)*x_shift - c/b, type = "l", col = "red",
#         xlim = c(-max_xyshift, max_xyshift), ylim = c(-max_xyshift, max_xyshift))
# 
#   # plot(x_shift, y_shift, type = "l", asp = 1)
#   # lines(x_shift, (-1/b)*x_shift - c/b, type = "l", col = "red", asp = 1)  # zoomed in
# }

#### Plot original data, with time represented by color, with microtubule line displayed ####

# for (filenumber in plotfiles) {
#   data_frame = data_frames[[filenumber]]
#   t = data_frame[[1]]
#   x_shift = data_frame[[2]]
#   y_shift = data_frame[[3]]
#   MTline_parameters_individual = MTline_parameters_all[[filenumber]]
#   b = MTline_parameters_individual[1]
#   c = MTline_parameters_individual[2]
#   
#   p = ggplot(data = data_frame, aes(x = x_shift, y = y_shift, color = t)) +
#     geom_point(size = 0.1, stroke = 0, shape = 16) +
#     scale_color_gradientn(colors = rainbow(3)) +
#     coord_cartesian(xlim = c(-max_xyshift, max_xyshift), ylim = c(-max_xyshift, max_xyshift)) +
#     geom_abline(color = "black", slope = -1/b, intercept = -c/b)
#   print(p)
#   
#   # p = ggplot(data = data_frame, aes(x = x_shift, y = y_shift, color = t)) +
#   #   geom_point(size = 0.1, stroke = 0, shape = 16) +
#   #   scale_color_gradientn(colors = rainbow(3)) +
#   #   geom_abline(color = "black", slope = -1/b, intercept = -c/b)
#   # print(p)  # zoomed in
# }

#### Plot original x(t) ####

# for (filenumber in plotfiles) {
#   data_frame = data_frames[[filenumber]]
#   t = data_frame[[1]]
#   x_shift = data_frame[[2]]
# 
#   plot(t, x_shift, type = "l", ylim = c(-max_xyshift, max_xyshift))
# 
#   # plot(t, x_shift, type = "l")  # zoomed in
# }

#### Plot original y(t) ####

# for (filenumber in plotfiles) {
#   data_frame = data_frames[[filenumber]]
#   t = data_frame[[1]]
#   y_shift = data_frame[[3]]
# 
#   plot(t, y_shift, type = "l", ylim = c(-max_xyshift, max_xyshift))
# 
#   # plot(t, y_shift, type = "l")  # zoomed in
# }

#### Plot transformed data, with microtubule line displayed ####

# for (filenumber in plotfiles) {
#   new_data_frame = new_data_frames[[filenumber]]
#   new_x_shift = new_data_frame[[2]]
#   new_y_shift = new_data_frame[[3]]
# 
#   plot(new_x_shift, new_y_shift, type = "l",
#        xlim = c(-new_max_xyshift, new_max_xyshift), ylim = c(-new_max_xyshift, new_max_xyshift))
#   lines(new_x_shift, 0*new_x_shift, type = "l", col = "red",
#         xlim = c(-new_max_xyshift, new_max_xyshift), ylim = c(-new_max_xyshift, new_max_xyshift))
#   
#   # plot(new_x_shift, new_y_shift, type = "l", asp = 1)
#   # lines(new_x_shift, 0*new_x_shift, type = "l", col = "red", asp = 1)  # zoomed in
# }

#### Plot transformed data, with time represented by color, with microtubule line displayed ####

# for (filenumber in plotfiles) {
#   new_data_frame = new_data_frames[[filenumber]]
#   t = new_data_frame[[1]]
#   new_x_shift = new_data_frame[[2]]
#   new_y_shift = new_data_frame[[3]]
# 
#   p = ggplot(data = new_data_frame, aes(x = new_x_shift, y = new_y_shift, color = t)) +
#     geom_point(size = 0.1, stroke = 0, shape = 16) +
#     scale_color_gradientn(colors = rainbow(3)) +
#     coord_cartesian(xlim = c(-new_max_xyshift, new_max_xyshift), ylim = c(-new_max_xyshift, new_max_xyshift)) +
#     geom_abline(color = "black", slope = 0, intercept = 0)
#   print(p)
# 
#   # p = ggplot(data = new_data_frame, aes(x = new_x_shift, y = new_y_shift, color = t)) +
#   #   geom_point(size = 0.1, stroke = 0, shape = 16) +
#   #   scale_color_gradientn(colors = rainbow(3)) +
#   #   geom_abline(color = "black", slope = 0, intercept = 0)
#   # print(p)  # zoomed in
# }

#### Plot transformed x(t) ####

# for (filenumber in plotfiles) {
#   new_data_frame = new_data_frames[[filenumber]]
#   t = new_data_frame[[1]]
#   new_x_shift = new_data_frame[[2]]
# 
#   plot(t, new_x_shift, type = "l", ylim = c(-new_max_xyshift, new_max_xyshift))
#   
#   # plot(t, new_x_shift, type = "l")  # zoomed in
# }

#### Plot transformed y(t) ####

# for (filenumber in plotfiles) {
#   new_data_frame = new_data_frames[[filenumber]]
#   t = new_data_frame[[1]]
#   new_y_shift = new_data_frame[[3]]
# 
#   plot(t, new_y_shift, type = "l", ylim = c(-new_max_xyshift, new_max_xyshift))
# 
#   # plot(t, new_y_shift, type = "l")  # zoomed in
# }

#### Plot distribution of transformed x positions (histogram, density, QQ) ####

# for (filenumber in plotfiles) {
#   new_data_frame = new_data_frames[[filenumber]]
#   new_x_shift = new_data_frame[[2]]
# 
#   hist(new_x_shift)
#   plot(density(new_x_shift))
#   qqnorm(new_x_shift)
# }

#### Plot distribution of transformed y positions (histogram, density, QQ) ####

# for (filenumber in plotfiles) {
#   new_data_frame = new_data_frames[[filenumber]]
#   new_y_shift = new_data_frame[[3]]
# 
#   hist(new_y_shift)
#   plot(density(new_y_shift))
#   qqnorm(new_y_shift)
# }

#### Plot distribution of transformed x increments (histogram, density, QQ) ####

# for (filenumber in plotfiles) {
#   new_data_frame = new_data_frames[[filenumber]]
#   new_x_shift = new_data_frame[[2]]
# 
#   hist(diff(new_x_shift))
#   plot(density(diff(new_x_shift)))
#   qqnorm(diff(new_x_shift))
# }

#### Plot distribution of transformed y increments (histogram, density, QQ) ####

# for (filenumber in plotfiles) {
#   new_data_frame = new_data_frames[[filenumber]]
#   new_y_shift = new_data_frame[[3]]
# 
#   hist(diff(new_y_shift))
#   plot(density(diff(new_y_shift)))
#   qqnorm(diff(new_y_shift))
# }

#### Plot MSD curve in transformed x direction ####

# for (filenumber in plotfiles) {
#   new_data_frame = new_data_frames[[filenumber]]
#   t = new_data_frame[[1]]
#   msd_x_individual = msd_x_all[[filenumber]]
# 
#   n = (1:(length(t) - 1))
#   delta = t[2] - t[1]
# 
#   plot(n*delta, msd_x_individual/1e06, type = "l",
#        xlim = c(0, length(t)*delta/3), ylim = c(0, max_msd_xy/1e06),
#        xlab = "time (s)", ylab = expression(paste("msd_x (", mu * m^{2}, ")")))
# 
#   # plot(n*delta, msd_x_individual/1e06, type = "l",
#   #      xlim = c(0, length(t)*delta/3),
#   #      xlab = "time (s)", ylab = expression(paste("msd_x (", mu * m^{2}, ")")))  # zoomed in
#   
#   # plot(log(n*delta), log(msd_x_individual/1e06), type = "l",
#   #      xlab = "log(time)", ylab = expression("log(msd_x)"))  # log-log
# }

#### Plot MSD curve in transformed y direction ####

# for (filenumber in plotfiles) {
#   new_data_frame = new_data_frames[[filenumber]]
#   t = new_data_frame[[1]]
#   msd_y_individual = msd_y_all[[filenumber]]
# 
#   n = (1:(length(t) - 1))
#   delta = t[2] - t[1]
# 
#   plot(n*delta, msd_y_individual/1e06, type = "l",
#        xlim = c(0, length(t)*delta/3), ylim = c(0, max_msd_xy/1e06),
#        xlab = "time (s)", ylab = expression(paste("msd_y (", mu * m^{2}, ")")))
# 
#   # plot(n*delta, msd_y_individual/1e06, type = "l",
#   #      xlim = c(0, length(t)*delta/3),
#   #      xlab = "time (s)", ylab = expression(paste("msd_y (", mu * m^{2}, ")")))  # zoomed in
#   
#   # plot(log(n*delta), log(msd_y_individual/1e06), type = "l",
#   #      xlab = "log(time)", ylab = expression("log(msd_y)"))  # log-log
# }

#### Plot total MSD curve ####

# for (filenumber in plotfiles) {
#   new_data_frame = new_data_frames[[filenumber]]
#   t = new_data_frame[[1]]
#   msd_individual = msd_all[[filenumber]]
# 
#   n = (1:(length(t) - 1))
#   delta = t[2] - t[1]
# 
#   plot(n*delta, msd_individual/1e06, type = "l",
#        xlim = c(0, length(t)*delta/3), ylim = c(0, max_msd/1e06),
#        xlab = "time (s)", ylab = expression(paste("msd (", mu * m^{2}, ")")))
# 
#   # plot(n*delta, msd_individual/1e06, type = "l",
#   #      xlim = c(0, length(t)*delta/3),
#   #      xlab = "time (s)", ylab = expression(paste("msd (", mu * m^{2}, ")")))  # zoomed in
# 
#   # plot(log(n*delta), log(msd_individual/1e06), type = "l",
#   #      xlab = "log(time)", ylab = expression("log(msd)"))  # log-log
# }

#### ****Averages of paths for each 'type'

#### ****Averages of MSDs for each 'type'

#### Notes ####

# modified_try1_3_5-1.txt, modified_try1_3_6-2.txt, modified_try1_3_6-3.txt, and modified_try1_3_10-4
#  have non-zero start times - is there a reason?
# Most, but not all, paths are oriented approximately vertically:
#  most seem to be <20 degrees from vertical;
#  path 12 is L-shaped;
#  path 22 is ~30 degrees from vertical;
#  path 23 is nearly horizontal.
# y's are approximately Gaussian except for paths 3, 8, 12, 13, 19, 23 (maybe 9, 21, and 24)

# Linear model works well for finding the microtubule for all paths except 12 and 23.
# However, as noted in the relevant section, this solution is "good"/workable but not the *best*.
# Try to find a method independent of the orientation of the data (or, equivalently, the microtubule).

# Paths have length(t) == 3000 except for...
# 7: 2371 (1-2371),  12: 2174 (827-3000),  14: 746 (1-746),  15: 1853 (1148-3000),
# 16: 522 (1993-2514),  22: 2263 (1-2263),  24: 1311 (1690-3000)
