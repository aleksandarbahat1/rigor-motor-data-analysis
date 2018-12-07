# # # # # # # # # # # # # # #
# RIGOR MOTOR DATA ANALYSIS #
# PLOT MSD                  #
# # # # # # # # # # # # # # #

#### Load packages ####

library(ggplot2)
library(gtools)

#### Plot MSD curve in transformed x direction ####

filelist = mixedsort(list.files("msd"))

max_msd = max(read.table("maxes_msd.txt")[1,])

for (filename in filelist) {
  msd = read.table(paste(getwd(), "/msd/", filename, sep = ""))
  n = msd[,1]
  msd_x = msd[,2]
  delta = 0.001

  plot_filename = paste("plot_msd_x_", substring(filename, 5, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_msd_x/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(n*delta, msd_x/1e06, type = "l", main = plot_filename,
       xlim = c(0, length(n)*delta/3), ylim = c(0, max_msd/1e06),
       xlab = "time (s)", ylab = expression(paste("msd_x (", mu * m^{2}, ")")))
  dev.off()

  plot_filename = paste("plot_msd_x_zoomed_", substring(filename, 5, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_msd_x/plot_msd_x_zoomed/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(n*delta, msd_x/1e06, type = "l", main = plot_filename,
       xlim = c(0, length(n)*delta/3),
       xlab = "time (s)", ylab = expression(paste("msd_x (", mu * m^{2}, ")")))
  dev.off()  # zoomed

  plot_filename = paste("plot_msd_x_loglog_", substring(filename, 5, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_msd_x/plot_msd_x_loglog/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(log(n*delta), log(msd_x/1e06), type = "l", main = plot_filename,
       xlab = "log(time)", ylab = expression("log(msd_x)"))
  dev.off()  # log-log
}

plot_filename = "plot_msd_x_samples.png"
full_plot_filename = paste(getwd(), "/plot_msd_x/", plot_filename, sep = "")
png(full_plot_filename)
plot(NULL, type = "l", main = plot_filename,
     xlim = c(0, 2999*delta/3), ylim = c(0, max_msd/1e06),
     xlab = "time (s)", ylab = expression(paste("msd_x (", mu * m^{2}, ")")))
for (filename in filelist) {
  msd = read.table(paste(getwd(), "/msd/", filename, sep = ""))
  n = msd[,1]
  msd_x = msd[,2]
  delta = 0.001

  if (length(n) == 2999) {
    lines(n*delta, msd_x/1e06)
  }
}
dev.off()  # all msd_x curves (for paths of "full" duration 3.000 seconds)

#### Plot MSD curve in transformed y direction ####

filelist = mixedsort(list.files("msd"))

max_msd = max(read.table("maxes_msd.txt")[1,])

for (filename in filelist) {
  msd = read.table(paste(getwd(), "/msd/", filename, sep = ""))
  n = msd[,1]
  msd_y = msd[,3]
  delta = 0.001

  plot_filename = paste("plot_msd_y_", substring(filename, 5, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_msd_y/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(n*delta, msd_y/1e06, type = "l", main = plot_filename,
       xlim = c(0, length(n)*delta/3), ylim = c(0, max_msd/1e06),
       xlab = "time (s)", ylab = expression(paste("msd_y (", mu * m^{2}, ")")))
  dev.off()

  plot_filename = paste("plot_msd_y_zoomed_", substring(filename, 5, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_msd_y/plot_msd_y_zoomed/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(n*delta, msd_y/1e06, type = "l", main = plot_filename,
       xlim = c(0, length(n)*delta/3),
       xlab = "time (s)", ylab = expression(paste("msd_y (", mu * m^{2}, ")")))
  dev.off()  # zoomed

  plot_filename = paste("plot_msd_y_loglog_", substring(filename, 5, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_msd_y/plot_msd_y_loglog/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(log(n*delta), log(msd_y/1e06), type = "l", main = plot_filename,
       xlab = "log(time)", ylab = expression("log(msd_y)"))
  dev.off()  # log-log
}

plot_filename = "plot_msd_y_samples.png"
full_plot_filename = paste(getwd(), "/plot_msd_y/", plot_filename, sep = "")
png(full_plot_filename)
plot(NULL, type = "l", main = plot_filename,
     xlim = c(0, 2999*delta/3), ylim = c(0, 0.006),
     xlab = "time (s)", ylab = expression(paste("msd_y (", mu * m^{2}, ")")))
for (filename in filelist) {
  msd = read.table(paste(getwd(), "/msd/", filename, sep = ""))
  n = msd[,1]
  msd_y = msd[,3]
  delta = 0.001

  if (length(n) == 2999) {
    lines(n*delta, msd_y/1e06)
  }
}
dev.off()  # all msd_y curves (for paths of "full" duration 3.000 seconds)

#### Plot total MSD curve ####

filelist = mixedsort(list.files("msd"))

max_msd = max(read.table("maxes_msd.txt")[2,])

for (filename in filelist) {
  msd = read.table(paste(getwd(), "/msd/", filename, sep = ""))
  n = msd[,1]
  msd_xy = msd[,4]
  delta = 0.001

  plot_filename = paste("plot_msd_", substring(filename, 5, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_msd/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(n*delta, msd_xy/1e06, type = "l", main = plot_filename,
       xlim = c(0, length(n)*delta/3), ylim = c(0, max_msd/1e06),
       xlab = "time (s)", ylab = expression(paste("msd (", mu * m^{2}, ")")))
  dev.off()

  plot_filename = paste("plot_msd_zoomed_", substring(filename, 5, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_msd/plot_msd_zoomed/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(n*delta, msd_xy/1e06, type = "l", main = plot_filename,
       xlim = c(0, length(n)*delta/3),
       xlab = "time (s)", ylab = expression(paste("msd (", mu * m^{2}, ")")))
  dev.off()  # zoomed

  plot_filename = paste("plot_msd_loglog", substring(filename, 5, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_msd/plot_msd_loglog/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(log(n*delta), log(msd_xy/1e06), type = "l", main = plot_filename,
       xlab = "log(time)", ylab = expression("log(msd)"))
  dev.off()  # log-log

  plot_filename = "plot_msd_samples.png"
  full_plot_filename = paste(getwd(), "/plot_msd/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(NULL, type = "l", main = plot_filename,
       xlim = c(0, 2999*delta/3), ylim = c(0, max_msd/1e06),
       xlab = "time (s)", ylab = expression(paste("msd (", mu * m^{2}, ")")))
  for (filename in filelist) {
    msd = read.table(paste(getwd(), "/msd/", filename, sep = ""))
    n = msd[,1]
    msd_xy = msd[,4]
    delta = 0.001

    if (length(n) == 2999) {
      lines(n*delta, msd_xy/1e06)
    }
  }
  dev.off()  # all msd curves (for paths of "full" duration 3.000 seconds)
}

U = net profit (payoff - cost)
U(cooperation) <=>? U(disassociation) <=>? U(conflict)