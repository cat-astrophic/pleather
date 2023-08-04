#' @title pleather
#'
#' @description Performs the bootstrapped synthetic controls method using the Synth package
#'
#' @param dat A data.frame object ready to pass into Synth
#' @param treat.id The ID for the treated unit - this comes from the unit.var column of dat
#' @param iterations The number of times that pleather resamples from the donor pool and re-runs Synth
#' @param subsample The proportion of the donor pool to be randomly chosen for each iteration
#' @param pred.vars The names of the predictor variables for Synth
#' @param preds.op The operator to be used on the predictors - the default is 'mean'
#' @param dep.var The name of the dependent variable
#' @param unit.var The name of the column of dat that contains the ID for Synth
#' @param time.var The name of the column of dat that contains the time variable
#' @param pre.time The period of time before the treatment takes effect
#' @param opt.time The period of time over which Synth will optimize the pre-event fit
#' @param unit.var.name The name of the column of dat that contains the names of the units
#' @param plot.time The period of time to be plotted by Synth
#' @param synth.opt.method The optimization method for Synth to use - the default is All
#' @param plot.title The title for the ggplot created by pleather
#' @param plot.y The label for the y-axis of the ggplot
#' @param plot.x The label for the x-axis of the ggplot
#' @param plot.y.int The y-intercept for a horizontal line - do not include if unwanted
#' @param plot.x.int The x-intercept for a vertical line - do not include if unwanted
#' @param treated.name The name of the treated unit as you want it to appear in the ggplot legend
#' @param treated.color The color of the line for the treated unit in the ggplot - default is red4 - Go Hokies!
#' @param save.to The filepath for where you want the figure saved if desired
#'
#' @return A data.frame object which contains data on the means and standard deviations from the bootstrapped synthetic control method
#'
#' @examples
#' data(fake)
#' boot.df <- pleather(dat = fake, treat.id = 21, iterations = 50, subsample = .5, pred.vars = c('Effort', 'Funding'), preds.op = 'mean', dep.var = 'Output', unit.var = 'ID', time.var = 'Month', pre.time = c(1:24), opt.time = c(1:24), unit.var.name = 'Name', plot.time = c(1:36), synth.opt.method = 'All', plot.title = 'Example Using Bootstapped SCM', plot.y = 'Outpu', plot.x = 'Month', plot.x.int = 25, treated.name = 'Michael', treated.color = 'red4', save.to = 'pleather_example_figure.png')
#'
#' @export
#' @import Synth
#' @import ggplot2

pleather <- function (dat, treat.id, iterations, subsample, pred.vars, preds.op, dep.var, unit.var, time.var, pre.time, opt.time, unit.var.name, plot.time, synth.opt.method, plot.title, plot.y, plot.x, plot.y.int, plot.x.int, treated.name, treated.color, save.to) {

  # Setting a seed for replicability

  set.seed(42069)

  # Data storage for main loop

  plot.data <- c()

  # Main loop

  for (i in 1:iterations) {

    boot.vals <- c()

    # Subset controls with subsample

    control.ids <- sample(unique(dat[,unit.var])[which(unique(dat[,unit.var]) != treat.id)], floor(subsample * length(unique(dat[,unit.var])[which(unique(dat[,unit.var]) != treat.id)])), replace = FALSE)
    dataframe <- dat[which(dat[,unit.var] %in% c(control.ids, treat.id)),]

    # Create the data prep object for Synth

    synth.data.prep.object <- dataprep(foo = dataframe, predictors = pred.vars, predictors.op = preds.op, dependent = dep.var,
                                       unit.variable = unit.var, time.variable = time.var, treatment.identifier = treat.id,
                                       controls.identifier = control.ids, time.predictors.prior = pre.time, time.optimize.ssr = opt.time,
                                       unit.names.variable = unit.var.name, time.plot = plot.time)

    # Run Synth

    if (missing(synth.opt.method) == TRUE) {

      synth.output <- synth(data.prep.obj = synth.data.prep.object)

    } else {

      synth.output <- synth(data.prep.obj = synth.data.prep.object, optimxmethod = synth.opt.method)

    }

    # Store data

    for (i in min(dataframe[,time.var]):max(dataframe[,time.var])) {

      tmp <- dataframe[which(dataframe[,unit.var] != treat.id),]
      tmp <- tmp[which(tmp[,time.var] == i),]
      boot.vals <- c(boot.vals, sum(tmp[,dep.var] * synth.output$solution.w))

    }

    plot.data <- cbind(plot.data, boot.vals)

  }

  # Creating a data.frame for to make a figure with

  plot.data <- as.data.frame(plot.data)
  means <- rowMeans(plot.data)
  sds <- apply(plot.data, 1, sd)
  plot.df <- as.data.frame(cbind(means, sds, dat[which(dat[,unit.var] == treat.id), dep.var], unique(dataframe[,time.var])))
  colnames(plot.df) <- c('Mean', 'SD', 'Value', 'X')

  # Creating a figure with ggplot

  if (missing(treated.color) == TRUE) {

    treated.color = 'red4' # Go Hokies

  }

  p <- ggplot(data = plot.df, aes(x = X, y = Value)) +
    theme_bw() +
    ggtitle(plot.title) +
    ylab(plot.y) +
    xlab(plot.x) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_line(aes(y = Mean, col = 'Synthetic Control'), size = 2, alpha = 1) +
    geom_ribbon(aes(ymin = Mean - 2*SD, ymax = Mean + 2*SD), fill = 'lightgray') +
    geom_line(aes(y = Value, col = treated.name), size = 1, alpha = 1) +
    scale_color_manual(name = '', breaks = c(treated.name, 'Synthetic Control'), values = c(treated.color, 'lightgray'))

  if (missing(plot.x.int) == FALSE) {

    p <- p + geom_vline(xintercept = plot.x.int)

  }

  if (missing(plot.y.int) == FALSE) {

    p <- p + geom_hline(yintercept = plot.y.int)

  }

  if (missing(save.to) == FALSE) {

    png(save.to)
    print(p)
    dev.off()

  }

  print(p)

  return(plot.df)

}

