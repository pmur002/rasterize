\name{grid.rasterize}
\alias{grid.rasterize}
\alias{grid.rasterize.function}
\alias{grid.rasterize.character}
\alias{grid.rasterize.gPath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Rasterize \pkg{grid} output
}
\description{
  This function can be used to selectively rasterize components
  of a 'grid' image.
}
\usage{
grid.rasterize(x, ..., res = 72)
\method{grid.rasterize}{character}(x, ..., res = 72)
\method{grid.rasterize}{gPath}(x, merge=FALSE, redraw=TRUE, ..., res = 72)
\method{grid.rasterize}{function}(x, ..., res = 72)
}
\arguments{
  \item{x}{
    What to rasterize.  Can be a function (and the 'grid' output
    for that function will be rasterized).  Can be the name of a grob
    (or a gPath) and that grob will be rasterized.
  }
  \item{merge}{
    Whether to replace multiple grobs with a single raster (or replace
    each grob with its own raster).
  }
  \item{redraw}{
    Whether to redraw the scene after replacing the grob with a
    raster.
  }
  \item{\dots}{
    For the character and gPath methods,
    arguments passed on to \code{grid.grep}.
  }
  \item{res}{
    The raster resolution (dpi).
  }
}
\details{
  The rasterization occurs in the context of the current viewport.
  A temporary PNG device is opened, the current viewport tree (below
  the current viewport) is enforced on the temporary device and the specified
  function or grob(s) are drawn on the temporary device.
  The resulting raster is drawn within the current viewport.
  For the character and gPath methods, the raster replaces the grob
  that was rasterized.
}
\value{
  NULL
}
\author{
  Paul Murrell
}
\examples{
grid.newpage()
grid.rect(width=.8, height=.8)
grid.text("test", name="t")
grid.rasterize("t")
}
\keyword{ dplot }
