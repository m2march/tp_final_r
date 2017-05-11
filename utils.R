file.remove(list.files(pattern='plot.*.png'))

plot_counter = 0

save_plot <- function(plot_f) {
    filename <- sprintf('plot%02d.png', plot_counter)
    plot_counter <<- plot_counter + 1
    png(filename)
    plot_f()
    dev.off()
    png('out.png')
    plot_f()
    dev.off()
}
