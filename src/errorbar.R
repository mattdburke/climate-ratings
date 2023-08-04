# R Project function to plot errorbars and confidence regions
#
# By Alastair Sanderson (2004-08-26)
#
# $Id: errorbar.R,v 1.18 2008/03/27 14:06:04 ajrs Exp $
#
# The cross & box options plot each error bar separately as value->bound, 
# rather than lobnd->upbnd in one go. This handles invalid error bars properly 
# (e.g. negative bounds on a log scale) so that if only one error in a pair 
# cannot be plotted, the opposite error will be displayed.
#
# In order to allow a vector of errorbar types to be used in conjunction with 
# a vector of colours, line widths & line types, it's necessary to specify the
# latter 3 explicitly, albeit using the global default values as per the 
# arrows man page.
#
errorbar <- function(x, xlo, xup, y, ylo, yup, type="b", len=0.02, bnd=T, col=par("fg"), lty=NULL, lwd=par("lwd"),...)
{
	stopifnot(length(x)==length(y))
	# Assume extrema are errors not bounds
	if ( ! bnd ) {
		xlo <- x - xlo
		xup <- x + xup
		ylo <- y - ylo
		yup <- y + yup
		bnd <- T
	}

	nt <- length(type)
	if ( nt > 1 ) {            # Multiple errorbar types
		nv <- length(x)
		# Pad out vectors to at least as long as the data vectors
		if ( nt < nv ) type <- rep(type,ceiling(nv/nt))
		if ( ! exists("col") ) col <- par("fg")
		if ( ! exists("lty") ) lty <- NULL
		if ( ! exists("lwd") ) lty <- par("lwd")
		col <- rep(col,ceiling(nv/length(col)))
		lty <- rep(lty,ceiling(nv/length(lty)))
		lwd <- rep(lwd,ceiling(nv/length(lwd)))


#		mapply(errorbar,x,xlo,xup,y,ylo,yup,type,col,lty,lwd, MoreArgs=list(len=len,bnd=bnd,...))

		for ( i in 1:nv ) {
			errorbar(x[i],xlo[i],xup[i],y[i],ylo[i],yup[i],type=type[i],len=len,bnd=T,col=col[i],lty=lty[i],lwd=lwd[i],...)
		}
	}
	else {

		if ( type == "c" ) {       # Plain cross error bars
			len <- 0
			type <- "b"        # Reset to barred cross
		}
		if ( type == "b" ) {       # Barred cross error bars
		  if ( ! identical(yup,y) ) arrows(x,y,x,yup,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
		  if ( ! identical(ylo,y) ) arrows(x,y,x,ylo,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
		  if ( ! identical(xup,x) ) arrows(x,y,xup,y,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
		  if ( ! identical(xlo,x) ) arrows(x,y,xlo,y,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
		}
		else if ( type == "d" ) {  # Diamond error region
			len <- 0
			arrows(x,yup,xup,y,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
			arrows(xup,y,x,ylo,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
			arrows(x,ylo,xlo,y,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
			arrows(xlo,y,x,yup,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
		}
		else if ( type == "x" ) {  # Box error region
			len <- 0
			arrows(x,yup,xup,yup,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
			arrows(xup,yup,xup,y,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
			arrows(xup,y,xup,ylo,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
			arrows(xup,ylo,x,ylo,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
			arrows(x,ylo,xlo,ylo,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
			arrows(xlo,ylo,xlo,y,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
			arrows(xlo,y,xlo,yup,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
			arrows(xlo,yup,x,yup,code=3,angle=90,length=len,col=col,lty=lty,lwd=lwd,...)
		}
	}
}

#---Invoke errorbar using a list block with predefined entries for (x,y) and their errors:
errbar <- function(list,...)
{
	stopifnot(is.list(list))
	errorbar(list$x,list$x.err,list$x.err,list$y,list$y.err,list$y.err,bnd=F,...)
}

ebar <- function(df,xtype="x",ytype="y",...) {
  if ( ! is.data.frame(df) ) stop("Input must be data frame")
  df <- xy.bounds(df,xtype=xtype,ytype=ytype)
  errorbar(df$x,df$x.lo,df$x.up,df$y,df$y.lo,df$y.up,...)
}

conf.reg <- function(obj,xtype="x",ytype="y",bnd=T,...) {
        A <- xy.bounds(obj,xtype=xtype,ytype=ytype)
	conf_region(A$x,A$x,A$x,A$y.lo,A$y.up,...)
}

#---Plot (shaded) confidence region
conf_region <- function(x, xlo, xup, ylo, yup, fill_col="grey", border=par("bg") , lcol=par("bg"), ... )
{
	N <- length(x)
	stopifnot(length(xlo)==N)
	stopifnot(length(xup)==N)
	stopifnot(length(ylo)==N)
	stopifnot(length(yup)==N)

        #--Get plot axis limits
	lims <- par("usr")
	if ( par("xlog") ) lims[1:2] <- 10^lims[1:2]
	if ( par("ylog") ) lims[3:4] <- 10^lims[3:4]
	xax.lo <- lims[1]; xax.up <- lims[2]
	yax.lo <- lims[3]; yax.up <- lims[4]

	#--Calculate coordinates of polygon vertices:
	X <- c(xlo[1],x,xup[N])
	Y1 <- c(ylo[1],ylo,ylo[N])
	Y2 <- c(yup[1],yup,yup[N])

	# Plot shaded region with supplied border colour:
	polygon(c(X,rev(X)),c(Y1,rev(Y2)),col=fill_col,border=border,...)

	#--Plot top & bottom bounding edges with supplied colour:
	lines(X,Y1,col=lcol)
	lines(X,Y2,col=lcol)
}

##
# From RJ, based on "confidence.band()" available at:
#   www.stat.psu.edu/~dhunter/R/confidence.band.r
##
confidence.band.data <- function(model,levels=0.95,segments=50,
                         col.points=palette()[1],col.line="black",
                         col.bands="black",lty.line=1,lty.bands=2,
                         plot=TRUE,list.data=FALSE,add=FALSE,...) {
  if (attr(model$terms,"intercept")!=1 || length(model$coef) !=2) {
    stop(paste("confidence.bands only works for simple linear regression\n",
               "with one predictor and an intercept"))
  }
  if ( plot ) plot(model$model[,2:1], col=col.points, type="n")
  if ( add ) abline(model, col=col.line[1], lty=lty.line[1], lwd=2)
  #
  angles=(0:segments)*pi/segments
  halfcircle = cbind(cos(angles), sin(angles))
  chol.shape = chol(vcov(model))
  slopes = (halfcircle %*% chol.shape)[,2]
  angles = angles+angles[which.max(slopes)]
  halfcircle = cbind(cos(angles), sin(angles))
  center = model$coef
  radius = sqrt(2*qf(levels, 2, df.residual(model)))

  for (r in radius) {
      halfcircle = -halfcircle
      ellipse = sweep(r*(halfcircle %*% chol.shape), 2, center, "+")
      int = ellipse[,1]
      slope = ellipse[,2]
      x1 = -diff(int)/diff(slope)
      y1 = int[-1]+slope[-1]*x1

      halfcircle = -halfcircle
      ellipse = sweep(r*(halfcircle %*% chol.shape), 2, center, "+")
      int = ellipse[,1]
      slope = ellipse[,2]
      x2 = -diff(int)/diff(slope)
      y2 = int[-1]+slope[-1]*x2

      if ( plot || add ) {
        lines(x1, y1, lwd=2, lty=lty.bands, col=col.bands)
        lines(x2, y2, lwd=2, lty=lty.bands, col=col.bands)
      }
  }
  #--Keep the confidence bounds:
  dframe <- data.frame(x1)
  names(dframe) <- all.vars(formula(model))[2]
  y <- predict(model,dframe)
  res <- data.frame(x=x1,y=y,y.lo=y2,y.up=y1)
#  res$x.lo <- x2; res$x.up <- x1
#  res$y.lo <- y2; res$y.up <- y1
  #
  if( list.data ) return(res)
}