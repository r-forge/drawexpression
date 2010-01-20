\name{drawExpression}
\alias{drawExpression}

\title{
   Draw a graphical representation of the evaluation of an R expression.
}
\description{
  Draw a graphical representation of the steps of the evaluation of an R
  expression. Each line in the graphic represent a step in the resolution of
  the expression. Following the syntax tree, each function is resolved starting
  with the inner most. Common R object (vector, matrix, list) and mode
  (numeric, character, logical) are distinguished thanks graphical conventions.
  This representation is intented to represent the abstract syntax for
  pedagogical purposes.
}

\usage{
drawExpression(exprs, draw.index = FALSE, draw.names = FALSE, filename = NULL)
}

\arguments{
  \item{exprs}{
   An R expression
}
  \item{draw.index}{
   Should index (for R objects such as vector, matrix or list) be drawn on the margin of graphics.
}
  \item{draw.names}{
   Should names (if any) be drawn on the margin of graphics.
}
  \item{filename}{
  The name of a PDF file where a PDF file must be created.
}
}
\details{
Some 
}
\value{
None.
}
\author{
Sylvain Loiseau
}

\examples{
  drawExpression("1:4");
  drawExpression("matrix(1:4, 2)");
  drawExpression("list(1:4, matrix(1:4, 2), 2, 3, 4)");
  drawExpression("c(1, 2, 3:5) > 2");
}
