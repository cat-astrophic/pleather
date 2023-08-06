#' Artificial data created for this package
#'
#'
#' @docType data
#'
#' @usage data(fake)
#'
#' @format An object of class \code{'data.frame'}
#'
#' @keywords datasets
#'
#' @examples
#' data(fake)
#' boot.df <- pleather(dat = fake, treat.id = 21, iterations = 50, subsample = .5, pred.vars = c('Effort', 'Funding'), preds.op = 'mean', dep.var = 'Output', unit.var = 'ID', time.var = 'Month', pre.time = c(1:24), opt.time = c(1:24), unit.var.name = 'Name', plot.time = c(1:36), synth.opt.method = 'All', plot.title = 'Example Using Bootstapped SCM', plot.y = 'Output', plot.x = 'Month', plot.x.int = 24, treated.name = 'Michael', treated.color = 'red4', save.to = 'pleather_example_figure.png')

