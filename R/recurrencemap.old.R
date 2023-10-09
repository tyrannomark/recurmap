# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ## Load the libraries needed in this file
# ## ### ##### ######## ############## ######################
require(tidyverse)
require(textgRid)
require(zoo)
# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ## Add Some Utility Functions
# ## ### ##### ######## ############## ######################
## Do nothing, but make the pipelines a little prettier
nop <- function(x) x
nope <- function(x) NULL
#
## Print out the current value in the pipeline
Pr <- function(x) { print(x); x }
## Print out the current value in the pipeline with wrapping
Pwrap <- function(x,a,b) { print(a); print(x); print(b); x }
#
#
# Vector functions
#
vToIndex <- function(v) {
  l <- (1:length(v))
  names(l) <- v
  l
}
#
## Add field, value by function
addFieldByFn <- function(df, fname, fn) {
  l <- dim(df)[1]
  dffn <- function(i) fn(df[i,])
  df[,fname] <- unlist( map((1:l),dffn) )
  df
}
#
## Add a count variable to a dataframe
addCount <- function(df,fname) addFieldByFn(df,fname,function(l) 1)
# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ## Add variable setting functions
# ## ### ##### ######## ############## ######################
setVar <- function(i, value, varNames, e) {
  varNames[[i]] %>%
    as_string() %>%
    assign(value[[i]], envir=e)
  TRUE
}
setPass <- function(value, varName, e) {
  varName %>%
    as_string() %>%
    assign(value, envir=e)
  value
}

mkLet <- function() {
  e <- parent.frame()
  assign("let",
         function(x,...) {
           ensyms(...) -> a
           (1:length( a )) %>%
             purrr::map(
               setVar,
               list.skip(x,n=1),
               a,
               e
             )
           x[[1]]
         }
         , envir = e
  )
  assign("letPass",
         function(x,varName) {
           ensym(varName) -> a
           setPass(x,a,e)
         }
         , envir = e
  )
}
testLet <- function() {
  ll <- list(27,33,42,88)
  X <- 0
  mkLet()
  ll %>%
    let(X,Y,Z) %>%
    letPass(T) %>%
    print()
  print(X)
  print(Y)
  print(Z)
  print(T)
}
testLet()
# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ##
# ## ### ##### ######## ############## ######################
`%|%` <- function(f1,f2) {
  if (is.null(f1)) return( f2 )
  if (is.null(f2)) return( f1 )
  function(...) f2(f1(...))
}
testCompose <- function() {
  a <- function(x,y) x + y
  b <- function(x) 5*x
  (a %|% b)(1,3)
}
# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ## Putting together the Recurrence Maps
# ## ### ##### ######## ############## ######################
baseMap <- function(d,postprocessing=NULL) {
  if (is.null(postprocessing)) { postprocessing <- function(x) x }
  (ggplot() +
      geom_tile( data=d, aes(x=t1,y=t2,fill=delta ) ) +
      theme_bw() +
      scale_x_continuous() +
      scale_y_continuous()
  ) %>%
    postprocessing() %>%
    plot()
}
test.baseMap <- function() {
  data.frame(t1=(0:100),v1=sin((0:100)/3*pi)) %>%
    merge( data.frame(t2=(0:100),v2=sin((0:100)/17*pi)) ) %>%
    (function(d) { d$delta <- (d$v2-d$v1) ** 2; d } ) %>%
    baseMap()
}
## test.baseMap()
# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ##
# ## ### ##### ######## ############## ######################
addPng <- function(ff,pngfilepath) {
  function(d,postprocessing=NULL) {
    png( pngfilepath, width=2048,height=2048 )
    postP <- function(g) g + theme(text=element_text(size=32))
    (ff)(d,(postP %|% postprocessing))
    dev.off()
  }
}
test.addPng <- function() {
  fn <- addPng(baseMap,"test-saved-image.png")
  data.frame(t1=(0:100),v1=sin((0:100)/3*pi)) %>%
    merge( data.frame(t2=(0:100),v2=sin((0:100)/19*pi)) ) %>%
    (function(d) { d$delta <- (d$v2-d$v1) ** 2; d } ) %>%
    (fn)()
}
## test.addPng()
# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ## Add a rainbow palette, discretised linearly, or by quantile
# ## ### ##### ######## ############## ######################
addPalette <- function(f,nlevels,quantised=FALSE) {
  function(d,postprocessing=NULL) {
    if (quantised) quantile_range <- quantile(d$delta, probs = seq(0,1, 1.0/(nlevels-1)))
    else {
      mnDelta <- min(d$delta)
      quantile_range <- seq(0,1,1.0/(nlevels-1)) * (max(d$delta) - mnDelta) + mnDelta
    }
    colour_palette <- colorRampPalette(
      c("#FF0000", "#FFFF00", "#00FF00", "#00FFFF", "#0000FF", "#FF00FF")
    )(length(quantile_range) - 1)
    label_text <- rollapply(round(quantile_range, 2), width = 2, by = 1, FUN = function(i) paste(i, collapse = " : "))
    d$delta <- factor(findInterval(d$delta, quantile_range, all.inside = TRUE))
    postP <- function(g) g + scale_fill_manual(values = colour_palette, name = "", labels = label_text)
    f(d,(postP %|% postprocessing))
  }
}

test.addPalette <- function() {
  fn <- addPalette(baseMap,11)
  data.frame(t1=(0:100),v1=sin((0:100)/3*pi)) %>%
    merge( data.frame(t2=(0:100),v2=sin((0:100)/17*pi)) ) %>%
    (function(d) { d$delta <- (d$v2-d$v1) ** 2; d } ) %>%
    fn()
}
## test.addPalette()
# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ## Make a recurrence data given a data frame and comparator
# ## ### ##### ######## ############## ######################
mkRecurrenceMapper <- function(f,timeF,comparator) {
  function(d,postprocessing=NULL) {
    timeV <- d[,timeF]; l <- length(timeV)
    dd <- data.frame(i1=rep((1:l),l),i2=rep((1:l),each=l))
    (1:(l*l)) %>%
      map(function(j) comparator(d[dd$i1[j],],d[dd$i2[j],])) %>%
      unlist() %>%
      nop() -> dd$delta
    dd$t1 <- timeV[dd$i1]; dd$t2 <- timeV[dd$i2]
    f(dd,postprocessing)
  }
}
test.mkRecurrenceMapper <- function() {
  comparator <- function(a,b) (a$v-b$v) ** 2
  baseMap %>%
    addPalette(11) %>%
    mkRecurrenceMapper("t",comparator) %>%
    nop() -> fn
  ##
  data.frame(t=(0:100),v=sin((0:100)/17*pi)+rnorm(101)*0.2) %>%
    fn()
}
# test.mkRecurrenceMapper()
# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ## Load a TextGrid File
# ## ### ##### ######## ############## ######################
require(textgRid)
require(PTXQC)
tgfile <- "c:/Users/tmark/Downloads/keraa-20190804-UR-01-G-kaliwu.TextGrid"
tg <- TextGrid(tgfile)

tg[[1]]@labels %>%
  grep('^[^(*]', ., value=TRUE) %>%
  gsub(' ', '', .) %>%
  map(str_to_lower) %>%
  unlist() %>%
  data.frame(v=.) %>%
  (function(d) { d$t <- (1:length(d$v)); d }) %>%
  nop() -> x
x


# comparator <- function(a,b) 2*adist(a$v,b$v)/(str_length(a$v)+str_length(b$v))
# comparator <- function(a,b) 1-2*str_length(LCS(a$v,b$v))/(str_length(a$v)+str_length(b$v))
# comparator <- function(a,b) 1 - 2*str_length(lcSuffix(c(a$v,b$v)))/(str_length(a$v)+str_length(b$v))
comparator <- function(a,b) { ifelse(a$t == b$t, 0.0, str_length(lcSuffix(c(a$v,b$v)))) }


baseMap %>%
  addPalette(11) %>%
  mkRecurrenceMapper("t",comparator) %>%
  nop() -> fn
##
x %>%
  fn()



# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ##
# ## ### ##### ######## ############## ######################
lcSuffix <- function(x, ignore.case = FALSE) {
  x <- as.character(x)
  if( ignore.case )
    x <- toupper(x)

  nc <- nchar(x, type="char")
  for(i in 1:min(nc)) {
    ## The +1 and +2 are because substr is funny
    ss = substr(x, nc - i + 1, nc)
    if( any(ss != ss[1] )) {
      if (i == 1L)                 # trailing char mismatch
        return("")
      return(substr(x[1], nc - i + 2, nc))
    }
  }
  return(substr(x[1], nc - i + 1, nc))
}

# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ## Get the F0 from a .wav File
# ## ### ##### ######## ############## ######################
require(seewave)
require(tuneR)
s <- function(t) t*44100
filepath <- "g:/My Drive/@@Projects/2022/2022-Igu-ritual-Keraa/01-Data/keraa-20190804-UR-01-G-kaliwu-01.WAV"
filepath %>%
  readWave(from=s(38), to=s(43)) %>%
  nop() -> wave

wave %>%
  spectro(flim=c(0.0,2.0))

wave %>%
  fund(fmax=1000,wl=64,ovlp=50) %>%
  as.data.frame() %>%
  (function(d) { colnames(d) <- c("t","kHz"); d }) %>%
  nop() -> fundo
fundo %>%
  (function(d) {
    ggplot(data=d) +
      geom_line(aes(x=t,y=kHz)) +
      theme_bw()
  }) %>%
  plot()
# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ##
# ## ### ##### ######## ############## ######################

# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ##
# ## ### ##### ######## ############## ######################

# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ##
# ## ### ##### ######## ############## ######################

# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ##
# ## ### ##### ######## ############## ######################

# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ##
# ## ### ##### ######## ############## ######################

# ## ### ##### ######## ############## ######################


# ## ### ##### ######## ############## ######################
# ## ### ##### ######## ############## ######################
# ##
# ## ### ##### ######## ############## ######################

# ## ### ##### ######## ############## ######################



