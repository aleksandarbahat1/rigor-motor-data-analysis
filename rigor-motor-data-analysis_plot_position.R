# # # # # # # # # # # # # # #
# RIGOR MOTOR DATA ANALYSIS #
# PLOT POSITION             #
# # # # # # # # # # # # # # #

#### Load packages ####

library(ggplot2)
library(gtools)

#### Plot transformed data, with microtubule line displayed ####

filelist = mixedsort(list.files("position"))

max_position = max(read.table("maxes_position.txt"))

for (filename in filelist) {
  position = read.table(paste(getwd(), "/position/", filename, sep = ""))
  t = position[,1]
  x = position[,2]
  y = position[,3]

  plot_filename = paste("plot_position_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_position/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(x, y, type = "l", main = plot_filename,
       xlim = c(-max_position, max_position), ylim = c(-max_position, max_position))
  lines(x, 0*x, type = "l", col = "red",
        xlim = c(-max_position, max_position), ylim = c(-max_position, max_position))
  dev.off()

  plot_filename = paste("plot_position_zoomed_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_position/plot_position_zoomed/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(x, y, type = "l", main = plot_filename, asp = 1)
  lines(x, 0*x, type = "l", col = "red", asp = 1)
  dev.off()  # zoomed
}

#### Plot transformed data, with time represented by color, with microtubule line displayed ####

filelist = mixedsort(list.files("position"))

max_position = max(read.table("maxes_position.txt"))

for (filename in filelist) {
  position = read.table(paste(getwd(), "/position/", filename, sep = ""))
  t = position[,1]
  x = position[,2]
  y = position[,3]

  plot_filename = paste("plot_position_color_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_position_color/", plot_filename, sep = "")
  png(full_plot_filename)
  p = ggplot(data = position, aes(x = x, y = y, color = t)) +
    geom_point(size = 0.1, stroke = 0, shape = 16) +
    scale_color_gradientn(colors = rainbow(3)) +
    coord_cartesian(xlim = c(-max_position, max_position), ylim = c(-max_position, max_position)) +
    geom_abline(color = "black", slope = 0, intercept = 0) +
    labs(title = plot_filename)
  print(p)
  dev.off()

  plot_filename = paste("plot_position_color_zoomed_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_position_color/plot_position_color_zoomed/", plot_filename, sep = "")
  png(full_plot_filename)
  p = ggplot(data = position, aes(x = x, y = y, color = t)) +
    geom_point(size = 0.1, stroke = 0, shape = 16) +
    scale_color_gradientn(colors = rainbow(3)) +
    geom_abline(color = "black", slope = 0, intercept = 0) +
    labs(title = plot_filename)
  print(p)
  dev.off()  #zoomed
}

#### Plot transformed x(t) ####

filelist = mixedsort(list.files("position"))

max_position = max(read.table("maxes_position.txt"))

for (filename in filelist) {
  position = read.table(paste(getwd(), "/position/", filename, sep = ""))
  t = position[,1]
  x = position[,2]

  plot_filename = paste("plot_x_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_x/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(t, x, type = "l", main = plot_filename, ylim = c(-max_position, max_position))
  dev.off()

  plot_filename = paste("plot_x_zoomed_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_x/plot_x_zoomed/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(t, x, type = "l", main = plot_filename)
  dev.off()  # zoomed
}

#### Plot transformed y(t) ####

filelist = mixedsort(list.files("position"))

max_position = max(read.table("maxes_position.txt"))

for (filename in filelist) {
  position = read.table(paste(getwd(), "/position/", filename, sep = ""))
  t = position[,1]
  y = position[,3]

  plot_filename = paste("plot_y_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_y/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(t, y, type = "l", main = plot_filename, ylim = c(-max_position, max_position))
  dev.off()

  plot_filename = paste("plot_y_zoomed_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_y/plot_y_zoomed/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(t, y, type = "l", main = plot_filename)
  dev.off()  # zoomed
}

#### Plot distribution of transformed x positions (histogram, density, QQ) ####

filelist = mixedsort(list.files("position"))

for (filename in filelist) {
  position = read.table(paste(getwd(), "/position/", filename, sep = ""))
  x = position[,2]

  plot_filename = paste("plot_density_x_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_distribution_x/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(density(x), main = plot_filename)
  dev.off()

  plot_filename = paste("plot_histogram_x_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_distribution_x/", plot_filename, sep = "")
  png(full_plot_filename)
  hist(x, main = plot_filename)
  dev.off()

  plot_filename = paste("plot_QQ_x_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_distribution_x/", plot_filename, sep = "")
  png(full_plot_filename)
  qqnorm(x, main = plot_filename)
  dev.off()
}

#### Plot distribution of transformed y positions (histogram, density, QQ) ####

filelist = mixedsort(list.files("position"))

for (filename in filelist) {
  position = read.table(paste(getwd(), "/position/", filename, sep = ""))
  y = position[,3]

  plot_filename = paste("plot_density_y_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_distribution_y/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(density(y), main = plot_filename)
  dev.off()

  plot_filename = paste("plot_histogram_y_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_distribution_y/", plot_filename, sep = "")
  png(full_plot_filename)
  hist(y, main = plot_filename)
  dev.off()

  plot_filename = paste("plot_QQ_y_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_distribution_y/", plot_filename, sep = "")
  png(full_plot_filename)
  qqnorm(y, main = plot_filename)
  dev.off()
}

#### Plot distribution of transformed x increments (histogram, density, QQ) ####

filelist = mixedsort(list.files("position"))

for (filename in filelist) {
  position = read.table(paste(getwd(), "/position/", filename, sep = ""))
  x = position[,2]

  plot_filename = paste("plot_density_diff_x_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_distribution_diff_x/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(density(diff(x)), main = plot_filename)
  dev.off()

  plot_filename = paste("plot_histogram_diff_x_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_distribution_diff_x/", plot_filename, sep = "")
  png(full_plot_filename)
  hist(diff(x), main = plot_filename)
  dev.off()

  plot_filename = paste("plot_QQ_diff_x_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_distribution_diff_x/", plot_filename, sep = "")
  png(full_plot_filename)
  qqnorm(diff(x), main = plot_filename)
  dev.off()
}

#### Plot distribution of transformed y increments (histogram, density, QQ) ####

filelist = mixedsort(list.files("position"))

for (filename in filelist) {
  position = read.table(paste(getwd(), "/position/", filename, sep = ""))
  y = position[,3]

  plot_filename = paste("plot_density_diff_y_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_distribution_diff_y/", plot_filename, sep = "")
  png(full_plot_filename)
  plot(density(diff(y)), main = plot_filename)
  dev.off()

  plot_filename = paste("plot_histogram_diff_y_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_distribution_diff_y/", plot_filename, sep = "")
  png(full_plot_filename)
  hist(diff(y), main = plot_filename)
  dev.off()

  plot_filename = paste("plot_QQ_diff_y_", substring(filename, 10, nchar(filename) - 4), ".png", sep = "")
  full_plot_filename = paste(getwd(), "/plot_distribution_diff_y/", plot_filename, sep = "")
  png(full_plot_filename)
  qqnorm(diff(y), main = plot_filename)
  dev.off()
}
