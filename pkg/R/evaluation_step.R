############## ############## ############## ############## ##############
# Draw a line in the graphic
############## ############## ############## ############## ##############

draw.lineBox <- function(l, x=.5, y=.5, height, width, components, comp.height, comp.width, draw.index=FALSE, draw.names=FALSE, margin) {
  linevp <- viewport(x=x, y=y, width=width, height=height);
  pushViewport(linevp);

  for (i in 1:length(components)) {
    obj <- components[[i]];
    obj$x <- unit(margin, "mm") * i - unit(margin, "mm") + sum(comp.width[1:i]) - comp.width[i] * 0.5;
    obj$y <- comp.height[i] * 0.5 + unit(margin, "mm");

    # if (i < length(components)) { 
    #   grid.lines(
    #   x=unit(4, "mm") * i + sum(comp.width[1:i]),
    #   gp=gpar(lty="dashed")
    #   );
    # }
    grid.draw(obj);
  }

  popViewport();
}

lineBoxGrob <- function(l, draw.index=FALSE, draw.names=FALSE, margin=2) { 
  components <- list();
  comp.height <- vector();
  comp.width <- vector();
  for (i in 1:length(l)) {
    components[[i]] <- l[[i]];
    if (i == 1) {
      comp.height <- grobHeight(components[[i]]);
      comp.width <- grobWidth(components[[i]]);
    } else {
      comp.height <- unit.c(comp.height, grobHeight(components[[i]]));
      comp.width <- unit.c(comp.width, grobWidth(components[[i]]));
    }
  }
  height <- unit(margin, "mm") + max(comp.height)
  width <- unit(margin, "mm") * length(components) + sum(comp.width);

  grob(labels=l, components=components, height=height, width=width, comp.height=comp.height, comp.width=comp.width, x=.5, y=.5, margin=margin, cl="lineBox");
}

# level is the current level in the syntax tree, nlevel the total number of levels
# , level, nlevel, ...
drawDetails.lineBox <- function(x, recording) {
  draw.lineBox(x$labels, x$x, x$y, x$height, x$width, x$components, x$comp.height, x$comp.width, margin=x$margin);
}

xDetails.lineBox <- function(x, theta) {
  grobX(roundrectGrob(x=x$x, y=x$y, width=x$width, height=x$height), theta);
}

yDetails.lineBox <- function(x, theta) {
  grobY(rectGrob(x=x$x, y=x$y, width=x$width, height=x$height), theta);
}

grobWidth.lineBox <- function(x) {
  x$width
}

grobHeight.lineBox <- function(x) {
  x$height
}

