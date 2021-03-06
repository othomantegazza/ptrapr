% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dot-plot.R
\name{panicle_tileplot}
\alias{panicle_tileplot}
\title{Plot the Output of `panicle_tibble()`}
\usage{
panicle_tileplot(pan_tbl, draw.plot = FALSE)
}
\arguments{
\item{pan_tbl}{A tibble, the output of `panicle_tibble()`}

\item{draw.plot}{Logical. Should the function draw a plot on the graphic device?
Defaults to `FALSE`.}
}
\description{
Returns a `ggplot2` object.
}
\details{
This is a small utility plot function. You can use it
on the output of `panicle_tibble()` to represent it as a
tileplot.

Although we provide this function as utility, we suggest that you design
your own plotting function, because only in this way you will reach the
versatlity required for exploratory data analysis. We provide ideas
on how to achieve this in the vignettes.

This tileplot is inspired by the plots in
https://www.nature.com/articles/s41598-018-30395-9
}
