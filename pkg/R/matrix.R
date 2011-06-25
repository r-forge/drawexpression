############## ############## ############## ############## ##############
# Draw a matrix
############## ############## ############## ############## ##############

charactermatrix <- function(m) {
  if (is.character(m)) {
    d <- dim(m);
    dn <- dimnames(m);
    m <- paste("\"", m, "\"", sep="");
    dim(m) <- d;
    dimnames(m) <- dn;
  }
  return(m);
}

draw.matrixBox <- function(matrice, x=.5, y=.5, draw.index, draw.names, width, height, colwidth, marginwidth, marginheight) {

  #matrice <- charactermatrix(matrice);
  nr <- nrow(matrice);

  tablevp <- viewport(x=x, y=y, width=width, height=height);
  pushViewport(tablevp);

  # ------ the content of the matrix ---------

  content.width <- width - marginwidth;
  content.height <- height - marginheight;
  content.vp <- viewport(
      x=marginwidth + content.width * .5,
      y=content.height * .5,
      width=content.width,
      height=content.height
      );
  pushViewport(content.vp);

  grid.lines(x=unit(0, "npc"), y=unit(c(0, nr), "lines"));
  for (i in 1:ncol(matrice)) {
    grid.lines(x=sum(colwidth[1:i]) + unit(2, "mm") * i, y=unit(c(0, nr), "lines"));
  }

  grid.lines(y=unit(0, "lines"));
  for (i in 1:nrow(matrice)) {
    grid.lines(y=unit(i, "lines"));
  }

  for (i in 1:nrow(matrice)) {
    for (j in 1:ncol(matrice)) {
      grid.text(matrice[nrow(matrice) - (i-1), j],
          y=unit(i-1, "lines") + unit(2.5, "mm"),
          x=sum(colwidth[1:j]) + unit(2, "mm") * (j-1) + unit(2, "mm") * .5,
          just="right"
          );
    }
  }
  popViewport();

  # ------ the row margin: row names and row index

  row.margin.vp <- viewport(
      x=marginwidth * .5,
      y=content.height * .5,
      width=marginwidth,
      height=content.height
      );
  pushViewport(row.margin.vp);
  grid.rect(gp=gpar(fill="lightgray", lwd=0))
  if (draw.names) {
    for (i in 1:nrow(matrice)) {
      grid.text(rownames(matrice)[nrow(matrice) - (i-1)],
          y=unit(i-1, "lines") + unit(2.5, "mm"),
          x=unit(0, "npc"),
          just="left"
          );
    }
  }
  if (draw.index) {
    for (i in 1:nrow(matrice)) {
      grid.text(nrow(matrice) - i+1,
          y=unit(i-1, "lines") + unit(2.5, "mm"),
          x=marginwidth - unit(1, "mm"),
          just="right"
          );
    }
  }
  popViewport();

  # ------

  # ------ the col margin: col names and col index

  col.margin.vp <- viewport(
      x=marginwidth + content.width * .5,
      y=content.height + marginheight * .5,
      width=content.width,
      height=marginheight
      );
  pushViewport(col.margin.vp);
  grid.rect(gp=gpar(fill="lightgray", lwd=0))
  if (draw.names) {
    #y <- marginheight * .5;
    y <- marginheight ;
   # if (draw.index) {
   #   y <- (marginheight - unit(1, "lines")) * .5;
   # }
    for (j in 1:ncol(matrice)) {
      grid.text(colnames(matrice)[j],
          y=y,
          x=sum(colwidth[1:j]) + unit(2, "mm") * (j-1) + unit(2, "mm") * .5,
          #hjust="center",
          vjust="bottom",
          just="right",
          rot=60
          );
    }
  }
  if (draw.index) {
    for (j in 1:ncol(matrice)) {
      grid.text(j,
          y=unit(1, "lines") * .5,
          x=sum(colwidth[1:j]) + unit(2, "mm") * (j-1) + unit(2, "mm") * .5,
          just="right"
          );
    }
  }
  popViewport();

  # ------

  popViewport();
}

matrixBoxGrob <- function(m, x=.5, y=.5, draw.index=FALSE, draw.names=FALSE) { 

  m <- charactermatrix(m);
  if (!is.null(rownames(m)) & !is.null(colnames(m))) {
    draw.index=TRUE;
    draw.names=TRUE;
  }

  lchar <- nchar(m);
  ilonguest <- apply(nchar(m), 2, which.max);
  ncell <- nrow(m) * ncol(m);
  longest <- m[seq(0, ncell-1, nrow(m)) + ilonguest]
  colwidth <- stringWidth(longest);

  width <- sum(colwidth) + unit(2, "mm") * ncol(m);
  height <- unit(1, "lines") * nrow(m);

  marginwidth <- unit(0, "mm");
  if (draw.index) {
    marginwidth <- marginwidth + stringWidth(nrow(m)) + unit(1, "mm");
  }
  if (draw.names) {
    if (is.null(colnames(m)) | is.null(rownames(m))) {
      stop("Cannot draw names if the matrix has no row names.");
    }
    imax <- which.max(nchar(rownames(m)));
    marginwidth <- marginwidth + stringWidth(rownames(m)[imax]) + unit(1, "mm");
  }
  width <- width + marginwidth

  marginheight <- unit(0, "mm");
  if (draw.index) {
    marginheight <- marginheight + unit(1, "lines");
  }
  if (draw.names) {
    if (is.null(colnames(m))) {
      stop("Cannot draw names if the matrix has no column names.");
    }
    imax <- which.max(nchar(colnames(m)));
    max.name.length <- stringWidth(colnames(m)[imax]);
    marginheight <- marginheight + max.name.length;
  }
  height <- height + marginheight;

  grob(labels=m, x=x, y=y, draw.index=draw.index, draw.names=draw.names, width=width, height=height, cl="matrixBox", colwidth=colwidth, marginwidth=marginwidth, marginheight=marginheight);
}

drawDetails.matrixBox <- function(x, ...) {
  draw.matrixBox(x$labels, x$x, x$y, draw.index=x$draw.index, draw.names=x$draw.names, width=x$width, height=x$height, colwidth=x$colwidth, marginwidth=x$marginwidth, marginheight=x$marginheight);
}

xDetails.matrixBox <- function(x, theta) {
  grobX(roundrectGrob(x=x$x, y=x$y, width=x$width, height=x$height), theta);
}

yDetails.matrixBox <- function(x, theta) {
  grobY(rectGrob(x=x$x, y=x$y, width=x$width, height=x$height), theta);
}

grobWidth.matrixBox <- function(x) {
  x$width
}

grobHeight.matrixBox <- function(x) {
  x$height;
}

