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


### Helper functions (not part of the public API)

read.header <- function(f) {
  header <- list()
  
  header$magic <- readBin(f, numeric(), 1, size=4)
  header$version <- readBin(f, numeric(), 1, size=4)
  header$nchannel <- readBin(f, numeric(), 1, size=4)
  header$nsamp <- readBin(f, numeric(), 1, size=4)
  header$dt <- readBin(f, numeric(), 1, size=4)

  return(header)
}

read.footer <- function(f, header) {
  footer <- list()

                                        # aux channel data
  footer$aux <- list()
  for (ch in 1:header$nchannel) 
    footer$aux[[ch]] <- readBin(f, numeric(), 10, size=4)

                                        # date time of recording
  footer$date <- readBin(f, numeric(), 6, size=4)

                                        # remarks
  footer$remarks <- readChar(f, 240, useBytes=TRUE)

                                        # markers
  footer$nmarkers <- readBin(f, "int", 1, size=4)
  
  footer$markers <- list()
  if (footer$nmarkers > 0) {
    for (i in 1:footer$nmarkers) {
      val <- readBin(f, "int", 1, size=4)
      sample <- val %% 1E6
      code <- val %/% 1E6
      type="marker"
      
      if (code >= 128) {
        type <- "note"
        code <- code - 128
      }
    
      footer$markers[[i]] <- list(sample=sample, type=type, code=code)
    }
  }

  # note marker data
  footer$nnotes <- readBin(f, "int", 1, size=4)
  footer$notes <- list()
  
  if (footer$nnotes > 0) {  
    for (i in 1:footer$nnotes) {
      footer$notes[[i]] <- readChar(f, 256, useBytes=TRUE)
    }
  }

  #### SSCF version 2 extra data
  
  # redundant channel number
  footer$nchannel <- readBin(f, "int", 1, size=4)

  # read channel records
  #TODO check consistency (i.e. channel num matches header)
  footer$channelrecs <- list()
  
  if (footer$nchannel > 0) {
    for (i in 1:footer$nchannel) {
      title <- readChar(f, 24, useBytes=TRUE)
      units <- readChar(f, 16, useBytes=TRUE)
      instrument <- readChar(f, 32, useBytes=TRUE)
      add.1 <- readChar(f, 32, useBytes=TRUE)
      add.2 <- readChar(f, 32, useBytes=TRUE)
      num.transform <- readBin(f, "int", 1, size=4)
      color <- readBin(f, "int", 1, size=4)
      
      #TODO according to docs should be 30 4-byte floats
      # file seems to have 31 (as shown by hex editor)
      aux <- readBin(f, numeric(), 31, size=4)
      
      footer$channelrecs[[i]] <- list(title=title,
                                      units=units,
                                      instrument=instrument,
                                      add.1=add.1,
                                      add.2=add.2,
                                      tnum=num.transform,
                                      color=color,
                                      aux=aux)
    }
  } # footer nchannel>0

  # user variables
  footer$uservar <- readBin(f, "numeric", 50, size=4)
  
  # variable aliases
  footer$nbytes <- readBin(f, "int", 1, size=4)
  footer$valias <- ""
  if (footer$nbytes > 0)
    footer$valias <- readChar(f, footer$nbytes, useBytes=TRUE)
  
  # setup file info
  footer$setupbytes <- readBin(f, "int", 1, size=4)
  if (footer$setupbytes == 192) {
    footer$setupfile <- readChar(f, 64, useBytes=TRUE)
    footer$setuppath <- readChar(f, 128, useBytes=TRUE)
  }
  
  # Extended remarks
  footer$remark.bytes <- readBin(f, "int", 1, size=4)
  footer$eremarks <- ""
  if (footer$remark.bytes > 0)
    footer$eremarks <- readChar(f, footer$remark.bytes, useBytes=TRUE)
  
  return(footer)
}

embellish.data <- function(data, header, footer=NULL) {
  # encode timebase in data
  data <- ts(data, deltat=header$dt)

  # add channel names (optional)
  if (! is.null(footer)) {
    ch.names <- sapply(footer$channelrecs, "[[", "title")
    ch.names <- strip.trailing.ws(ch.names)
    colnames(data) <- ch.names
  }

  return(data)
}

build.metadata <- function(x, header, footer) {
  attr(x, "version") <- header$version
  attr(x, "nchannel") <- header$nchannel
  attr(x, "nsample") <- header$nsamp

  attr(x, "comment") <- paste(footer$remarks, footer$eremarks, sep="")
  attr(x, "marker") <- process.markers(footer, times=time(x))
  attr(x, "channelinfo") <- process.channelinfo(footer)
  attr(x, "uservar") <- process.uservars(footer)
  attr(x, "date") <- process.date(footer)
  
  path <- strip.trailing.ws(footer$setuppath)
  file <- strip.trailing.ws(footer$setupfile)

  if (length(file)>0) {
    if (length(path) > 0) {
      setup <- file.path(path, file)
    } else {
      setup <- file
    }
  } else {
    setup <- ""
  }
    
  attr(x, "setup") <- setup

  # debug only metadata
  if (getOption("debug", FALSE)) {
    attr(x, "rawheader") <- header
    attr(x, "rawfooter") <- footer
  }

  return(x)
}

process.markers <- function(footer, times) {
  m <- footer$markers
  n <- footer$notes

  if (length(m) == 0)
    return(list())
  
  convert.marker <- function(x) {
    switch(x$type,
           marker = {
             x$text <- to.ascii(x$code)
           },
           note = {
             x$text <- n[x$code]
           }
           )
    
    return(x)
  }

  ma <- lapply(m, convert.marker)
  
  samples <- sapply(ma, "[[", "sample")
  samples[samples == 0] <- NA
  
  codes <- sapply(ma, "[[", "code")
  texts <- sapply(ma, "[[", "text")
  types <- factor(sapply(ma, "[[", "type"), levels=c("marker", "note"))

  times <- times[samples+1] # samples are 0 based
  
  return(data.frame(sample=samples, time=times,
                    code=codes, type=types, text=texts,
                    stringsAsFactors=FALSE))
}

strip.trailing.ws <- function(x) {
  return(gsub("\\s+$", "", x, perl=TRUE))
}

process.channelinfo <- function(footer) {
  r <- footer$channelrecs

  strip.fun <- function(x) {
    x$title <- strip.trailing.ws(x$title)
    x$units <- strip.trailing.ws(x$units)
    x$instrument <- strip.trailing.ws(x$instrument)
    x$add.1 <- strip.trailing.ws(x$add.1)
    x$add.2 <- strip.trailing.ws(x$add.2)
    return(x)
  }

  color.conversion <- function(x) {
    blue <- x$color %/% 65536
    rg <- x$color %% 65536
    green <- rg %/% 256
    red <- rg %% 256    
    
    x$color <- rgb(red,green,blue, maxColorValue=255)
    return(x)
  }

  r <- lapply(r, strip.fun)
  r <- lapply(r, color.conversion)

  names(r) <- sapply(r, function(x) x$title)
  
  return(r)
}
  
process.uservars <- function(footer) {
  vals <- footer$uservar
  nams <- footer$valias
  
  # name 'em K1 .. K50 by default
  names(vals) <- paste("K", 1:length(vals), sep="")

  # now apply defined aliases
  n.assignments <- strsplit(nams, "\r\n")

  for (a in n.assignments) {
    if (length(a)) {
      elem <- strsplit(a, "=")[[1]]
      lhs <- elem[1]
      rhs <- elem[2]

      idx <- as.numeric(gsub("K", "", lhs))
      names(vals)[idx] <- rhs
    }
  }
    
  return(vals)
}

process.date <- function(footer) {
  d = footer$date
  
  return(ISOdatetime(d[3], d[2], d[1], d[4], d[5], d[6]))
}
