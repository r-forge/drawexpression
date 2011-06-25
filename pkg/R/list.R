############## ############## ############## ############## ##############
# Draw a list
############## ############## ############## ############## ##############

draw.listBox <- function(l, x=.5, y=.5, height, width, components, comp.height, comp.width, draw.index=FALSE, draw.names=FALSE, marginheight) {
  listvp <- viewport(x=x, y=y, width=width, height=height);
  pushViewport(listvp);

  # content
  content.height <- height - marginheight;
  content.vp <- viewport(
      x=width * .5,
      y=content.height * .5,
      width=width,
      height=content.height
      );
  pushViewport(content.vp);
  grid.rect(gp=gpar(lty="dashed"));
  for (i in 1:length(components)) {
    obj <- components[[i]];
    obj$x <- unit(4, "mm") * i - unit(2, "mm") + sum(comp.width[1:i]) - comp.width[i] * 0.5;
    obj$y <- comp.height[i] * 0.5 + unit(2, "mm");

    if (i < length(components)) { 
      grid.lines(
      x=unit(4, "mm") * i + sum(comp.width[1:i]),
      gp=gpar(lty="dashed")
      );
    }
    grid.draw(obj);
  }
  popViewport();

# names
  if (draw.names | draw.index) {
    margin.vp <- viewport(
        x=width * .5,
        y=content.height + marginheight * .5,
        width=width,
        height=marginheight
        );
    pushViewport(margin.vp);
    grid.rect(gp=gpar(fill="lightgray", lwd=0))
      if (draw.names) {
#y <- marginheight * .5;
        y <- marginheight ;
# if (draw.index) {
#   y <- (marginheight - unit(1, "lines")) * .5;
# }
        for (j in 1:length(l)) {
          x <- unit(4, "mm") * j - unit(2, "mm") + sum(comp.width[1:j]) - comp.width[j] * 0.5;
          grid.text(names(l)[j],
              y=y,
              x=x,
#hjust="center",
              vjust="bottom",
              just="right",
              rot=60
              );
        }
      }
    if (draw.index) {
      for (j in 1:length(l)) {
          x <- unit(4, "mm") * j - unit(2, "mm") + sum(comp.width[1:j]) - comp.width[j] * 0.5;
        grid.text(j,
            y=unit(1, "lines") * .5,
            x=x,
            just="right"
            );
      }
    }
    popViewport();
  }


  popViewport();
}

listBoxGrob <- function(l, x=.5, y=.5, draw.index=FALSE, draw.names=FALSE) { 

  if (!is.null(names(l))) {
    draw.index=TRUE;
    draw.names=TRUE;
  }

  components <- list();
  comp.height <- vector();
  comp.width <- vector();

  if (length(l) == 0) {
    stop("empty list");
  }
  for (i in 1:length(l)) {
    components[[i]] <- objectGrob(l[[i]]);
    if (i == 1) {
      comp.height <- grobHeight(components[[i]]);
      comp.width <- grobWidth(components[[i]]);
    } else {
      comp.height <- unit.c(comp.height, grobHeight(components[[i]]));
      comp.width <- unit.c(comp.width, grobWidth(components[[i]]));
    }
  }
  height <- unit(4, "mm") + max(comp.height)
  width <- unit(4, "mm") * length(components) + sum(comp.width);

  marginheight <- unit(0, "mm");
  if (draw.index) {
    marginheight <- marginheight + unit(1, "lines");
  }
  if (draw.names) {
    if (is.null(names(l))) {
      stop("Cannot draw names if the list has no column names.");
    }
    imax <- which.max(nchar(names(l)));
    max.name.length <- stringWidth(names(l)[imax]);
    marginheight <- marginheight + max.name.length;
  }
  height <- height + marginheight;

  grob(labels=l, components=components, height=height, width=width, comp.height=comp.height, comp.width=comp.width, x=x, y=y, draw.index=draw.index, draw.names=draw.names, cl="listBox", marginheight=marginheight);
}

drawDetails.listBox <- function(x, ...) {
  draw.listBox(x$labels, x$x, x$y, x$height, x$width, x$components, x$comp.height, x$comp.width, x$draw.index, x$draw.names, marginheight=x$marginheight);
}

xDetails.listBox <- function(x, theta) {
  grobX(roundrectGrob(x=x$x, y=x$y, width=x$width, height=x$height), theta);
}

yDetails.listBox <- function(x, theta) {
  grobY(rectGrob(x=x$x, y=x$y, width=x$width, height=x$height), theta);
}

grobWidth.listBox <- function(x) {
  x$width
}

grobHeight.listBox <- function(x) {
  x$height
}

