# # # # # # # # # # # # # # #
# RIGOR MOTOR DATA ANALYSIS #
# PLOT POSITION0            #
# # # # # # # # # # # # # # #

#### Load packages ####

library(ggplot2)
library(gtools)

#### Plot original data, with microtubule line displayed ####

filelist = mixedsort(list.files("position0"))

MTline_parameters = read.table("MTline_parameters.txt")
max_position0 = max(read.table("maxes_position0.txt"))

for (filename in filelist) {
  position0 = read.table(paste(getwd(), "/position0/", filename, sep = ""))
  t = position0[,1]
  x0 = position0[,2]
  y0 = position0[,3]
  filenumber = which(filelist == filename)
  b = MTline_parameters[1, filenumber]
  c = MTline_parameters[2, filenumber]

  plot_filename = paste("plot_position0_", substring(filename, 11, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_position0/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(x0, y0, type = "l", main = plot_filename,
       xlim = c(-max_position0, max_position0), ylim = c(-max_position0, max_position0))
  lines(x0, (-1/b)*x0 - c/b, type = "l", col = "red",
        xlim = c(-max_position0, max_position0), ylim = c(-max_position0, max_position0))
  dev.off()

  plot_filename = paste("plot_position0_zoomed_", substring(filename, 11, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_position0/plot_position0_zoomed/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(x0, y0, type = "l", main = plot_filename, asp = 1)
  lines(x0, (-1/b)*x0 - c/b, type = "l", col = "red", asp = 1)
  dev.off()  # zoomed
}

#### Plot original data, with time represented by color, with microtubule line displayed ####

filelist = mixedsort(list.files("position0"))

MTline_parameters = read.table("MTline_parameters.txt")
max_position0 = max(read.table("maxes_position0.txt"))

for (filename in filelist) {
  position0 = read.table(paste(getwd(), "/position0/", filename, sep = ""))
  t = position0[,1]
  x0 = position0[,2]
  y0 = position0[,3]
  filenumber = which(filelist == filename)
  b = MTline_parameters[1, filenumber]
  c = MTline_parameters[2, filenumber]

  plot_filename = paste("plot_position0_color_", substring(filename, 11, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_position0_color/", plot_filename, sep = "")
  png(full_plot_filename)
  p = ggplot(data = position0, aes(x = x0, y = y0, color = t)) +
    geom_point(size = 0.1, stroke = 0, shape = 16) +
    scale_color_gradientn(colors = rainbow(3)) +
    coord_cartesian(xlim = c(-max_position0, max_position0), ylim = c(-max_position0, max_position0)) +
    geom_abline(color = "black", slope = -1/b, intercept = -c/b) +
    labs(title = plot_filename)
  print(p)
  dev.off()

  plot_filename = paste("plot_position0_color_zoomed", substring(filename, 11, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_position0_color/plot_position0_color_zoomed/", plot_filename, sep = "")
  png(full_plot_filename)
  p = ggplot(data = position0, aes(x = x0, y = y0, color = t)) +
    geom_point(size = 0.1, stroke = 0, shape = 16) +
    scale_color_gradientn(colors = rainbow(3)) +
    geom_abline(color = "black", slope = -1/b, intercept = -c/b) +
    labs(title = plot_filename)
  print(p)
  dev.off()  # zoomed
}

#### Plot original x(t) ####

filelist = mixedsort(list.files("position0"))

max_position0 = max(read.table("maxes_position0.txt"))

for (filename in filelist) {
  position0 = read.table(paste(getwd(), "/position0/", filename, sep = ""))
  t = position0[,1]
  x0 = position0[,2]

  plot_filename = paste("plot_x0_", substring(filename, 11, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_x0/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(t, x0, type = "l", main = plot_filename, ylim = c(-max_position0, max_position0))
  dev.off()

  plot_filename = paste("plot_x0_zoomed_", substring(filename, 11, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_x0/plot_x0_zoomed/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(t, x0, type = "l", main = plot_filename)
  dev.off()  # zoomed
}

#### Plot original y(t) ####

filelist = mixedsort(list.files("position0"))

max_position0 = max(read.table("maxes_position0.txt"))

for (filename in filelist) {
  position0 = read.table(paste(getwd(), "/position0/", filename, sep = ""))
  t = position0[,1]
  y0 = position0[,3]

  plot_filename = paste("plot_y0_", substring(filename, 11, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_y0/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(t, y0, type = "l", main = plot_filename, ylim = c(-max_position0, max_position0))
  dev.off()

  plot_filename = paste("plot_y0_zoomed_", substring(filename, 11, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_y0/plot_y0_zoomed/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(t, y0, type = "l", main = plot_filename)
  dev.off()  # zoomed
}
