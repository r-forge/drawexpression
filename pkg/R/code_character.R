############## ############## ############## ############## ##############
# Draw a function name (and parenthesis, comma, operator)
############## ############## ############## ############## ##############

draw.functionText <- function(l, x=.5, y=.5, height, width) {
  functionTextVP <- viewport(x=x, y=y, width=width, height=height);
  pushViewport(functionTextVP);
  #grid.rect(x=x, y=y, width=width, height=height, gp=gpar(lty="longdash"));
  #grid.rect(gp=gpar(lty="longdash"));
  grid.text(l);
  popViewport();
}

functionTextBoxGrob <- function(l, x=.5, y=.5) { 
  height <- unit(1, "line");
  width <- stringWidth(l) + unit(1, "mm");
  grob(labels=l, height=height, width=width, x=x, y=y, cl="functionText");
}

drawDetails.functionText <- function(x, ...) {
  draw.functionText(x$labels, x$x, x$y, x$height, x$width);
}

xDetails.functionText <- function(x, theta) {
  grobX(roundrectGrob(x=x$x, y=x$y, width=x$width, height=x$height), theta);
}

yDetails.functionText <- function(x, theta) {
  grobY(rectGrob(x=x$x, y=x$y, width=x$width, height=x$height), theta);
}

grobWidth.functionText <- function(x) {
  x$width
}

grobHeight.functionText <- function(x) {
  x$height
}
