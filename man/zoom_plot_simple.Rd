% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zoom_plot_resp.R
\name{zoom_plot_simple}
\alias{zoom_plot_simple}
\title{zoom_plot_simple}
\usage{
zoom_plot_simple(data, channel, marker, cut_start, cut_end, ...)
}
\arguments{
\item{data}{The ExpeData (".exp" ??? Sable Systems) file with the spectrograph information for oxygen, carbon dioxide, water vapor flow rate etc.}

\item{channel}{The specific channel, or data column from `extract_data` that is relevant for plotting.}

\item{marker}{The marker data that should be plotted.}

\item{cut_start}{Where you want to start the plotting from i.e. region you want to zoom in on}

\item{cut_end}{Where you want to end the plotting}

\item{...}{Additional arguments passed to plot}
}
\description{
Plots the relevant data as a function of time with the marker text plotted as well within a specified part of the trace
}
\author{
Daniel Noble ??? daniel.noble@anu.edu.au and Fonti Kar fonti.kar@gmail.com
}
