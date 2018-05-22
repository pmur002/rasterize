
grid.rasterize <- function(x, ..., res=72) {
    UseMethod("grid.rasterize")
}

generateRaster <- function(f, res) {
    ## Size of current viewport
    w <- convertWidth(unit(1, "npc"), "in", valueOnly=TRUE)
    h <- convertHeight(unit(1, "npc"), "in", valueOnly=TRUE)
    cvp <- current.vpTree(all=FALSE)
    ## Change current viewport to entire device
    if (inherits(cvp, "vpTree")) {
        cvp$parent$x <- unit(.5, "npc")
        cvp$parent$y <- unit(.5, "npc")
        cvp$parent$width <- unit(1, "npc")
        cvp$parent$height <- unit(1, "npc")
        cvp$parent$valid.just <- c(.5, .5)
        cvp$parent$angle <- 0
    } else {
        cvp$x <- unit(.5, "npc")
        cvp$y <- unit(.5, "npc")
        cvp$width <- unit(1, "npc")
        cvp$height <- unit(1, "npc")
        cvp$valid.just <- c(.5, .5)
        cvp$angle <- 0
    }
    dev <- dev.cur()
    pngfile <- tempfile("raster", fileext=".png")
    png(pngfile, width=round(w*res), height=round(h*res), res=res,
        bg="transparent")
    ## Recreate viewport tree on sub-device
    pushViewport(cvp)
    if (depth(cvp) > 1) {
        upViewport(depth(cvp)  -1 )
    }
    ## Call function to draw raster content
    tryCatch(f(),
             finally={ dev.off(); dev.set(dev) })
    readPNG(pngfile)
}

## Capture the output from a function in the current viewport
grid.rasterize.function <- function(x, ..., res=72) {
    raster <- generateRaster(x, res)
    grid.raster(raster)
}

## Rasterize an existing grob
## (replace an existing grob with a raster grob)
grid.rasterize.character <- function(x, ..., res=72) {
    grid.rasterize(gPath(x), ..., res=res)
}

replaceGrob <- function(grobPath, res, redraw) {
    grob <- grid.get(grobPath)
    raster <- generateRaster(function() { grid.draw(grob) }, res)
    grid.set(grobPath, rasterGrob(raster, name=grob$name),
             redraw=redraw)
}

grid.rasterize.gPath <- function(x, merge=FALSE, redraw=TRUE, ..., res=72) {
    grobPath <- grid.grep(x, ...)
    if (length(grobPath)) {
        if (inherits(grobPath, "gPath")) {
            ## Single grob to rasterize
            replaceGrob(grobPath, res, redraw)
        } else {
            ## Several grobs to rasterize
            if (merge) {
                ## Replace first grob with combination
                grobs <- lapply(grobPath, grid.get)
                grob <- gTree(children=do.call(gList, grobs),
                              name=grobs[[1]]$name)
                raster <- generateRaster(function() { grid.draw(grob) }, res)
                grid.set(grobPath[[1]], rasterGrob(raster, name=grob$name),
                         redraw=FALSE)
                ## Remove the others
                lapply(grobPath[-1], grid.remove, redraw=FALSE)
                if (redraw)
                    grid.refresh()
            } else {
                ## Rasterize individually
                lapply(grobPath, replaceGrob, res, FALSE)
                if (redraw)
                    grid.refresh()
            }
        }
    }
}
