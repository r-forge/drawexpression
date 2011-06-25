############## ############## ############## ############## ##############
# Draw a vector
############## ############## ############## ############## ##############

draw.vectorBox <- function(vect, x=.5, y=.5, draw.index=FALSE, draw.names=FALSE, width, height) {
  len.vect <- length(vect);

  if (draw.names) {
    v.names <- names(vect);
    max.name.length <- which.max(nchar(v.names));
  }
  if (draw.index) {
    indexes <- 1:len.vect;
  }

  tablevp <- viewport(x=x, y=y, width=width, height=height);
  pushViewport(tablevp);
  #grid.rect(gp=gpar(lty="dashed"));
  grid.lines(y=unit(0, "lines"));
  grid.lines(y=unit(1, "lines"));
  #grid.lines(y=unit(2, "lines"), gp=gpar(lty="dashed"));

  for (i in 1:(len.vect)) {
    if (i == 1) { 
      grid.lines(x=unit(0, "npc"), y=unit(c(0,1), "lines"));
    } else { 
      grid.lines(x=sum(stringWidth(vect[1:(i-1)])) + unit(2, "mm") * (i-1), y=unit(c(0,1), "lines"));
    }
  }
  grid.lines(x=unit(1, "npc"), y=unit(c(0,1), "lines"));

  for (i in 1:(len.vect)) {
    grid.text(vect[i],
        y=unit(2.5, "mm"),
        x=unit(2, "mm") * (i-1) + unit(1, "mm") + sum(stringWidth(vect[1:i])),
        just="right"
        );
  }


#grid.rect(x = unit(0.5, "npc"), y = unit(0.5, "npc"),
#          width = unit(1, "npc"), height = unit(1, "npc"),

#  row.margin.vp <- viewport(
#      x=marginwidth * .5,
#      y=content.height * .5,
#      width=marginwidth,
#      height=content.height
#      );
#  pushViewport(row.margin.vp);

    if (draw.index) {
    grid.rect(x = unit(0.5, "npc"), y = unit(2.5, "mm") + unit(1, "lines"),
      width = width, height = unit(.9, "lines"), gp=gpar(fill="lightgray", lwd=0));
    }
    if (draw.names) {
      y <- unit(1, "lines") + 0.5 * stringWidth(v.names[max.name.length]);
      # unit(2.5, "mm") + 
      if (draw.index) {
        y <- y + unit(1, "lines");
      }    
    grid.rect(x = unit(0.5, "npc"), y = y,
      width = width, height = unit(.9, "lines"), gp=gpar(fill="lightgray", lwd=0));
    }

  for (i in 1:(len.vect)) {
    if (draw.index) {
      if (i == 1) { 
        grid.text(indexes[i],
            y=unit(2.5, "mm") + unit(1, "lines"),
            x=unit(1, "mm") + stringWidth(vect[i]) * .5,
            just=c("centre", "center")
            , gp=gpar(fill="lightgray")
            )
      } else {
        grid.text(indexes[i],
            y=unit(2.5, "mm") + unit(1, "lines"),
            x=unit(2, "mm") * (i-1) + unit(1, "mm") + sum(stringWidth(vect[1:(i-1)])) + stringWidth(vect[i]) * .5,
            just=c("centre", "center")
            , gp=gpar(fill="lightgray")
            )
      }
    }
    if (draw.names) {
      y <- unit(1, "lines") + 0.5 * stringWidth(v.names[max.name.length]);
      # unit(2.5, "mm") + 
      if (draw.index) {
        y <- y + unit(1, "lines");
      }    
      if (i == 1) { 
        grid.text(v.names[i],
            y=y,
            x=unit(1, "mm") + stringWidth(vect[i]) * .5,
            hjust="top",
            vjust="top",
           # just="right",
            rot=60
            );
      } else {
        grid.text(v.names[i],
            y=y,
            x=unit(2, "mm") * (i-1) + unit(1, "mm") + sum(stringWidth(vect[1:(i-1)])) + stringWidth(vect[i]) * .5,
            hjust="top",
            vjust="top",
           # just="right",
            rot=60
            );
      }
    }
  }
  popViewport();
  return(unit.c(height, width));
}

vectorBoxGrob <- function(v, x=.5, y=.5, draw.index=FALSE, draw.names=FALSE, a.factor=FALSE) { 

  if (!is.null(names(v))) {
    draw.index=TRUE;
    draw.names=TRUE;
  }

  height <- unit(1, "lines");
  if (draw.index) height <- height + unit(1, "lines");
  if (draw.names) {
    if (is.null(names(v))) stop("Cannot draw names if the vector has no names.");
    #max.name.length <- which.max(nchar(names(v)));
    #height <- height + stringWidth(names(v)[max.name.length]);
    height <- height + max(stringWidth(names(v)));
  }

  if (!a.factor) {
    if (is.character(v)) {
      n <- names(v);
      v <- paste("\"", v, "\"", sep="");
      names(v) <- n;
    }
  }
  width <- sum(stringWidth(v)) + unit(2, "mm") * length(v);

  grob(labels=v, x=x, y=y, width=width, height=height, draw.index=draw.index, draw.names=draw.names, cl="vectorBox");
}

drawDetails.vectorBox <- function(x, ...) {
  draw.vectorBox(x$labels, x$x, x$y, x$draw.index, x$draw.names, x$width, x$height);
}

xDetails.vectorBox <- function(x, theta) {
  grobX(roundrectGrob(x=x$x, y=x$y, width=x$width, height=x$height), theta);
}

yDetails.vectorBox <- function(x, theta) {
  grobY(rectGrob(x=x$x, y=x$y, width=x$width, height=x$height), theta);
}

grobWidth.vectorBox <- function(x) {
  return(x$width);
}

grobHeight.vectorBox <- function(x) {
  return(x$height);
}

