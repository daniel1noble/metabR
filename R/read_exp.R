#    Copyright (C) 2013,  Thomas Foerster <foerster@sablesys.com>

#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    You should have received a copy of the GNU General Public License along
#    with this program; if not, write to the Free Software Foundation, Inc.,
#    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


# Read basic SSCF file
read.exp <- function(filename) {
  f <- file(filename, "rb")

  header <- read.header(f)
  
  # data block
  if (header$nsamp > 0) { # normally terminated file
    data <- readBin(f, numeric(), n=header$nchannel*header$nsamp, size=4)
    data <- matrix(data, nrow=header$nsamp, ncol=header$nchannel, byrow=T)

  } else { # cut-off file
    warning("Error: Number of samples indicates a truncated file.")
    return (NULL)
  }

  # extended metadata block in file versions > 1.0
  if (header$version > 1) {
    footer <- read.footer(f, header)
  } else {
    footer <- NULL
  }

  #TODO: catch exceptions/errors 
  close(f) 

  ### post processing
  result <- embellish.data(data, header, footer)
  result <- build.metadata(result, header, footer)
  
  class(result) <- c("SSCFFile", class(result))
  
  return(result)
}