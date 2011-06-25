## TODO

# "table" object doesn't work: table is numeric but not vector. table(x, y) give a matrice (is.matrix()->TRUE)
# dealing with formula object?
#   subset: argument as formula
# do something for the attributes of a vector
#  (eg. for regexp functions...)

# Add the original code on a first line?
# matrice : problem with column width when indices are two number length
# data frame and matrix share too much code

#I'm wondering if I shouldn't automatically shorten vectors and matrixes when
#they are two long. For instances, for vectors longer than six elements, add a
#"..." as sixth element. I will make clearer the emphasis on the steps of the
#evaluation of the expression.


# by Hadley Wickham <hadley@rice.edu> :

# It would be great if drawExpression distinguished between vectors and
# single row matrices:
# 
# library(drawExpression)
# 
# a <- 1:5
# b <- matrix(1:5, nrow = 1)
# drawExpression("list(a, b)")
# 
# # And arrays don't seem to work in a list:
# 
# c <- array(1:5, c(1, 5, 1))
# drawExpression("list(a, b, c)")
# 
# drawExpression("c") # works, but drawn in wrong direction
#
# Thanks for the drawExpression package - it looks really great.  A few comments:
# 
# * Have you thought about visualising the non-evaluated expressions?
# i.e. just show the parse tree?  I've attached the R code I use to draw
# them as text, but it would be much nicer to draw them as graphics.
# 
# * I think drawExpression should accept calls as well as strings - so
# you can do (e.g.) drawExpression(quote(a + b * c))
# 
# * When a matrix has row/column names, I think you should just show
# them, not show the indices as well.

debuging <- FALSE;

##
## The public method
##

drawExpression <- function (expr, draw.index=FALSE, draw.names=FALSE, filename=NULL) {
  if (mode(expr) != "character") {
    stop("expr must be a characters string");
  }

  if (length(expr) != 1) {
    stop("expr must be a characters string of length 1");
  }

  ## Build the syntax tree
  e <- parse(text=expr);
  if (mode(e) != "expression") {
    stop(paste(expr, "must be a parsable expression"));
  }

  ## Create an intermediary representation
  drawable <- .drawableTree(e[[1]], 1);

#print(drawable);
  plot.new();

  ## draw this representation with grid function
  .drawTree(drawable, filename=filename);

}

########################################################
########################################################

# ---------------------------------
#
# Walk recursively through the syntax tree and create and transform it into a
# tree ready for drawing. Some representations used in the parse tree are
# transformed into more traditionnal (and concrete) representation; for
# instance the leaves "+ 1 2" are reordened into the more classical "1 + 2".
#
# param
#
# call: the syntax tree.
# level: the level in the tree (1-based)
#
# value
#
# A list reflecting the parse tree.
#
# The list contains the following components:
# $eval = the result of the evaluation of the expression
# $type = a character string
# $level = the level in the parse tree (1 = root)
# $children = a list containing a list for each member of the expression. These inner list are
#            also structured with the component $eval, $type, $level, $children, and they are
#            produced by recursively calling .drawableTree.
#
# For instance, the expression  "c(1, 2) + 2" submitted to drawExpression()
# produce the following list (in version 1.0 of drawExpression):
#
# $eval
# [1] 3 4
# 
# $type
# [1] ""
# 
# $level
# [1] 1
# 
# $children
# $children[[1]]
# $children[[1]]$eval
# [1] 1 2
# 
# $children[[1]]$type
# [1] ""
# 
# $children[[1]]$level
# [1] 2
# 
# $children[[1]]$children
# $children[[1]]$children[[1]]
# $children[[1]]$children[[1]]$eval
# [1] "c"
# 
# $children[[1]]$children[[1]]$type
# [1] "special"
# 
# $children[[1]]$children[[1]]$level
# [1] 3
# 
# 
# $children[[1]]$children[[2]]
# $children[[1]]$children[[2]]$eval
# [1] "("
# 
# $children[[1]]$children[[2]]$type
# [1] "special"
# 
# $children[[1]]$children[[2]]$level
# [1] 3
# 
# 
# $children[[1]]$children[[3]]
# $children[[1]]$children[[3]]$eval
# [1] 1
# 
# $children[[1]]$children[[3]]$type
# [1] ""
# 
# $children[[1]]$children[[3]]$level
# [1] 3
# 
# 
# $children[[1]]$children[[4]]
# $children[[1]]$children[[4]]$eval
# [1] ","
# 
# $children[[1]]$children[[4]]$type
# [1] "special"
# 
# $children[[1]]$children[[4]]$level
# [1] 3
# 
# 
# $children[[1]]$children[[5]]
# $children[[1]]$children[[5]]$eval
# [1] 2
# 
# $children[[1]]$children[[5]]$type
# [1] ""
# 
# $children[[1]]$children[[5]]$level
# [1] 3
# 
# 
# $children[[1]]$children[[6]]
# $children[[1]]$children[[6]]$eval
# [1] ")"
# 
# $children[[1]]$children[[6]]$type
# [1] "special"
# 
# $children[[1]]$children[[6]]$level
# [1] 3
# 
# 
# 
# 
# $children[[2]]
# $children[[2]]$eval
# [1] "+"
# 
# $children[[2]]$type
# [1] "special"
# 
# $children[[2]]$level
# [1] 2
# 
# 
# $children[[3]]
# $children[[3]]$eval
# [1] 2
# 
# $children[[3]]$type
# [1] ""
# 
# $children[[3]]$level
# [1] 2
# ---------------------------------

.drawableTree <- function(call, level) {
  #mode(call) may be name, call, or primitive (numeric, etc.).
  l <- list();
  l$eval = eval(call, envir=parent.frame(2));
  l$type = "";
  l$level = level;
  lengthCall <- length(call);
  if (lengthCall > 1) {
    children = list();
    if (mode(eval(call[[1]])) == "function") {
      if (as.character(call[[1]]) == "[") {
        children[[1]] <- .drawableTree(call[[2]], level+1);
        children[[2]] <- makeOpenningBracket(level + 1);
        for (z in 3:length(call)) {
          if (z > 3) {
            children[[length(children) + 1]] <- makeComma(level+1);
          }
          if (as.character(call[[z]]) == "") {
          } else {
            children[[length(children) + 1]] <- .drawableTree(call[[z]], level+1);
          }
        }
        children[[length(children) + 1]] <- makeClosingBracket(level + 1);
      } else if (as.character(call[[1]]) == "$") {
        children[[1]] <- .drawableTree(call[[2]], level+1);
        children[[2]] <- makeDollar(level + 1);
        children[[3]] <- makeFunction(deparse(call[[3]]), level+1);
      } else if (as.character(call[[1]]) == "[[") {
        children[[1]] <- .drawableTree(call[[2]], level+1);
        children[[2]] <- makeOpenningDoubleBracket(level + 1);
        children[[3]] <- .drawableTree(call[[3]], level+1);
        children[[4]] <- makeClosingDoubleBracket(level + 1);
      } else if (
          (as.character(call[[1]]) == "<")
          || (as.character(call[[1]]) == ":")
          || (as.character(call[[1]]) == ">")
          || (as.character(call[[1]]) == "+")
          || (as.character(call[[1]]) == "-")
          || (as.character(call[[1]]) == "*")
          || (as.character(call[[1]]) == "/")
          || (as.character(call[[1]]) == "==")
          || (as.character(call[[1]]) == ">=")
          || (as.character(call[[1]]) == "<=")
          || (as.character(call[[1]]) == "&")
          || (as.character(call[[1]]) == "|")
          || (as.character(call[[1]]) == "!=")
          || (as.character(call[[1]]) == "$")
          ) {
        children[[1]] <- .drawableTree(call[[2]], level+1);
        children[[2]] <- makeOperator(as.character(call[[1]]), level + 1);
        children[[3]] <- .drawableTree(call[[3]], level+1);
      } else if (grepl("<-$", as.character(call[[1]]))) {
        children[[1]] <- makeFunction(deparse(call[[2]]), level+1);
        children[[2]] <- makeFunction(as.character(call[[1]]), level + 1);
        for (i in 3:lengthCall) {
          children[[i]] <- .drawableTree(call[[i]], level+1);
        }
      } else {
        #print(as.character(call[[1]]));
        children[[1]] <- makeFunction(as.character(call[[1]]), level + 1);
        children[[2]] <- makeOpenningParenthesis(level + 1);
        offset <- 1;
        for (i in 2:lengthCall) {

        #  print(as.character(call[[i]]));

# test if it is the name of a parameter (such as "from" in "seq(from=...)")
          paramName <- names(call[i]);
        #  print(paramName);
          if (! is.null(paramName)) {
            children[[i + offset]] <- makeParam(names(call[i]), level + 1);
            offset = offset + 1;
          }
# This argument is a function, handle it nicely.
        if (mode(eval(call[[i]])) == "function") {
          children[[i + offset]] <- makeFunction(as.character(call[[i]]), level + 1);
        } else {
          children[[i + offset]] <- .drawableTree(call[[i]], level+1);
        }
        #  children[[i + offset]] <- .drawableTree(call[[i]], level + 1);

# add a comma between argument
          if (i < lengthCall) {
            offset = offset + 1;
            children[[i + offset]] <- makeComma(level + 1);
          }
        }
        children[[length(children) + 1]] <- makeClosingParenthesis(level + 1);
      }
    } else {
      for (i in 1:lengthCall) {
        children[[i]] <- .drawableTree(call[[i]], level+1);
      }
    }
    l$children <- children;
  } else {
    l$children = NULL;
  }
  l;
}

########################################################
########################################################

.drawTree <- function(drawable, filename) {
  depth <- getMaxDepth(drawable);
  linesGrob <- vector(length=depth, mode="list");
  for (i in depth:1) {
    line <- list();
    line <- .getLineRec(line, drawable, i);
    lineGrob <- lineBoxGrob(line, i, depth);
    linesGrob[[i]] <- lineGrob;
  }

  heights <- numeric(length(linesGrob));
  widths <- numeric(length(linesGrob));
  for (i in 1:length(linesGrob)) {
    if (i == 1) {
      heights <- grobHeight(linesGrob[[1]]);
      widths <- grobWidth(linesGrob[[1]]);
    } else {
      heights <- unit.c(heights, grobHeight(linesGrob[[i]]));
      widths <- unit.c(widths, grobWidth(linesGrob[[i]]));
    }
  }
  totalHeight <- sum(heights);
  maxWidth <- max(widths);

  if(!is.null(filename)) {
    height_inches <- as.numeric(convertUnit(totalHeight, "inches"));
    width_inches <- as.numeric(convertUnit(maxWidth, "inches"));
    pdf(filename, height=height_inches + .2, width=width_inches + .2, onefile=TRUE);
  }

  listvp <- viewport(x=.5, y=.5, width=maxWidth, height=totalHeight);
  pushViewport(listvp);
  if (debuging) grid.rect(gp=gpar(lty="longdash"));
  for (i in length(linesGrob):1) {
    line <- linesGrob[[i]];
    line$y <- sum(heights[1:i]) - heights[i] * 0.5;
    grid.draw(line);
  }
  popViewport();

  if(!is.null(filename)) {
    dev.off();
  }
}

# A line is a list of representations of R syntax components: R object or
# pieces of syntax (function name, coma, operator...).
.getLineRec <- function(line, drawable, level) {
  if ((drawable$level < level) & (! is.null(drawable$children))) {
    for (i in 1:length(drawable$children)) {
      line <- .getLineRec(line, drawable$children[[i]], level);
    }
  } else {
    line <- c(line, list(drawLineComponent(drawable)));
  }
  return(line);
}

drawLineComponent <- function(drawable) {
  if (drawable$type == "special") {
    return(functionTextBoxGrob(drawable$eval));
  } else {
    if (is.atomic(drawable$eval)) {
      if (is.vector(drawable$eval)) {
        return(vectorBoxGrob(drawable$eval));
      } else if (is.matrix(drawable$eval)) {
        return(matrixBoxGrob(drawable$eval));
      } else if (is.array(drawable$eval)) {
        ## TODO good for tapply
        #return(listBoxGrob(as.list(drawable$eval)));
        ## TODO good for margin.table and table.
        ## not so good for table(): lost its name
        return(matrixBoxGrob(as.matrix(drawable$eval)));
      } else if (is.factor(drawable$eval)) {
        return(vectorBoxGrob(drawable$eval, a.factor=TRUE));
      } else {
        stop(paste("unknown case", drawable$eval));
      }
    } else if (is.data.frame(drawable$eval)) {
      return(dataframeBoxGrob(drawable$eval));
    } else if (is.list(drawable$eval)) {
      return(listBoxGrob(drawable$eval));
    } else if (is.function(drawable$eval)) {
      return(functionTextBoxGrob(drawable$eval));
      # TODO
    } else {
      stop(paste("Unknown type:", drawable$eval));
    }
  }
}

makeDefaultSpecial <- function(text, level) {
  l <- list();
  l$eval = text;
  l$type = "special";
  l$level = level;
  l$children = NULL;
  l;
}

makeOpenningDoubleBracket <- function(level) {
  makeDefaultSpecial("[[", level);
}

makeClosingDoubleBracket <- function(level) {
  makeDefaultSpecial("]]", level);
}

makeDollar <- function(level) {
  makeDefaultSpecial("$", level);
}

makeOpenningBracket <- function(level) {
  makeDefaultSpecial("[", level);
}

makeClosingBracket <- function(level) {
  makeDefaultSpecial("]", level);
}

makeOperator <- function(op, level) {
  makeDefaultSpecial(op, level);
}

makeOpenningParenthesis <- function(level) {
  makeDefaultSpecial("(", level);
}

makeClosingParenthesis <- function(level) {
  makeDefaultSpecial(")", level);
}

makeParam <- function (param, level) {
  makeDefaultSpecial(paste(param, " = ", sep=""), level);
}

makeFunction <- function (functionName, level) {
  makeDefaultSpecial(functionName, level);
}

makeComma <- function (level) {
  makeDefaultSpecial(",", level);
}

getMaxDepth <- function(drawable) {
  depth <- drawable$level;
  if (! is.null(drawable$children)) {
    for (i in 1:length(drawable$children)) {
      depth <- max(depth, getMaxDepth(drawable$children[[i]]));
    }
  }
  depth;
}

getMaxHeightForRaw <- function(drawable, level, height) {
  if ((drawable$level < level) & (! is.null(drawable$children))) {
    for (i in 1:length(drawable$children)) {
      height <- max(height, getMaxHeightForRaw(drawable$children[[i]], level, height));
    }
  } else {
    height <- max(height, drawable$height);
  }
  print(paste(drawable$eval, ":", height));
# add margin
  height <- height + 0.005;
}

############## ############## ############## ############## ##############
# grid object (Grob)
############## ############## ############## ############## ##############

objectGrob <- function(obj) {
 if (is.data.frame(obj)) {
   return(dataframeBoxGrob(obj));
 } else if (is.list(obj)) {
   return(listBoxGrob(obj));
 } else if (is.factor(obj)) {
   return(vectorBoxGrob(as.character(obj)));
 } else if (is.vector(obj)) {
   return(vectorBoxGrob(obj));
 } else if (is.matrix(obj)) {
   return(matrixBoxGrob(obj));
 }
 stop(paste("Object not known", mode(obj)));
}

