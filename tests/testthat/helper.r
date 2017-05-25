# runs before tests are executed
# here we source the files in the package R directory
# to facilitate use of test_dir() during development

# example from ?source
sourceDir <- function(path, trace = TRUE, ...) {
	for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
		if(trace) cat(nm,":")
		source(file.path(path, nm), ...)
		if(trace) cat("\n")
	}
	if(!trace) print( paste("sourced", path)) 
}

sourceDir("../../R", trace = FALSE)
