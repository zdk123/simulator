do_in_batch <- function(function_to_do, function_params, save_to_file, save_params,
                        conf_file = ".BatchJobs.R", job_resources=list(), out_dir,
                        libraries=character(0L), progressbars=TRUE, clearreg=TRUE) {

  njobs <- length(function_params)
  for (i in seq(njobs)) stopifnot("out_dir" %in% names(save_params[[i]]))
  wrapper <- function(i, fun, params) {
    do.call(fun, params[[i]])
  }
  regdir <- file.path(out_dir, "BatchJobs")
  if (clearreg) unlink(regdir, recursive=TRUE)
  regid <- gsub("-", "_", function_params[[1]]$method@name)
  BatchJobs::loadConfig(conf_file)
  reg <- BatchJobs::makeRegistry(id=regid, file.dir=regdir, packages=libraries)
  id  <- BatchJobs::batchMap(reg, wrapper, seq(njobs), 
                             more.args=list(fun=function_to_do, params=function_params))
  doneSub <- BatchJobs::submitJobs(reg=reg, resources=job_resources, progressbar=progressbars)
  ## wait for results / load results??
  return(list(reg=reg, id=id, params=list(function_params, save_params), file=save_to_file))
}

wrap_up_batch <- function(reg, id, params, save_to_file, progressbars = TRUE) {
  doneRun <- BatchJobs::waitForJobs(reg, id, progressbar=progressbars)
  if (!doneRun) stop('Errors found in jobs')
  out <- NULL #BatchJobs::loadResults(reg, id)
#  njobs <- length(out)
  ## TODO: how to deal with output
#  refs <- list()
#  for (i in seq(njobs))
#    refs[[i]] <- do.call("save_to_file", c(out[[i]], save_params[[i]]))
  refs <- save_to_file(out, params, reg, id)
  catsim("..Created", as.character(refs), "in batch.", fill = TRUE)
  invisible(refs)
}
