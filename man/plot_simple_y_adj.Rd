% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zoom_plot_resp.R
\name{plot_simple_y_adj}
\alias{plot_simple_y_adj}
\title{plot_simple_y_adj}
\usage{
plot_simple_y_adj(data, channel, marker, y_lim_min, y_lim_max, ...)
}
\arguments{
\item{data}{The ExpeData (".exp" ??? Sable Systems) file with the spectrograph information for oxygen, carbon dioxide, water vapor flow rate etc.}

\item{channel}{The specific channel, or data column from `extract_data` that is relevant for plotting.}

\item{marker}{The marker data that should be plotted.}

\item{...}{Additional arguments passed to plot}

\item{cut_start}{Where you want to start the plotting from i.e. region you want to zoom in on}

\item{cut_end}{Where you want to end the plotting}
}
\description{
Plots the relevant data as a function of time with the marker text plotted as well, adjusts y axes in case there is leakage and you want to truncate the y axis
}
\author{
Daniel Noble ??? daniel.noble@anu.edu.au and Fonti Kar fonti.kar@gmail.com
}
