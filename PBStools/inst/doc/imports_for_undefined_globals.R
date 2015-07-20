txt <- "
abline AIC approx arrows axis
barplot box boxplot
col2rgb colorRamp colorRampPalette contourLines contr.sum
dev.cur dev.off
extendrange
flush.console frame
hat hist
jpeg
legend lines lm lowess
median mtext
na.omit nls
pairs par pdf plot png points polygon postscript predict.lm
qnorm qqnorm quantile
rbinom rect residuals rgamma rgb rnorm rstudent runif
savePlot sd start
text
var
write.csv write.table
"
txt = gsub("\\n"," ",txt)

imports_for_undefined_globals <-
  function(txt, lst, selective = TRUE)
  {
    if(!missing(txt))
      lst <- scan(what = character(), text = txt, quiet = TRUE)
    nms <- lapply(lst, find)
    ind <- sapply(nms, length) > 0L
    imp <- split(lst[ind], substring(unlist(nms[ind]), 9L))
    if(selective) {
      sprintf("importFrom(%s)",
              vapply(Map(c, names(imp), imp),
                     function(e)
                       paste0("\"", e, "\"", collapse = ", "),
                     ""))
    } else {
      sprintf("import(\"%s\")", names(imp))
    }
  }

# writeLines(imports_for_undefined_globals(txt))

