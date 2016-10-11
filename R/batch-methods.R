run_method_batch <- function(my_methods, dir, model_name, index,
                                out_dir, out_loc, libraries=character(0L), 
                                conf_file = ".BatchJobs.R", job_resources=list(), #regdir,
                                progressbars=TRUE) {
  options(BatchJobs.clear.function.env = TRUE) 
  function_to_do <- function(dir, model_name, index, method) {
    model      <- load_model(dir, model_name)
    draws_list <- load_draws(dir, model_name, index, more_info = TRUE)
    out_list   <- run_method_single(method, model, draws_list)
    return(out_list)
  }

  num_methods <- length(my_methods)
  num_index <- length(index)
  # index over all method-index pairs:
  ii <- cbind(rep(seq(num_methods), each = num_index),
              rep(seq(num_index), times = num_methods))
  njobs <- nrow(ii)

    # make list where params1[[i]] are the arguments to pass to
  # function_to_do for i-th job.  Here i indexes method-index pairs
  params1 <- lapply(seq(njobs),
                    function(i) list(dir = dir, model_name = model_name,
                                     index = index[ii[i, 2]],
                                     method = my_methods[[ii[i, 1]]]))
  # this is function to use when saving info in out_list to file
  save_to_file <- function(output, params, reg, id) {
    save_batchjob_output_to_file(output, params, reg, id)
  }
  # parameters to be passed to save_to_file other than out_list
  params2 <- lapply(seq(njobs), function(i) list(out_dir = out_dir,
                                                 dir = dir,
                                                 out_loc = out_loc))
  do_in_batch(function_to_do, params1,
                 save_to_file, params2,
                 libraries = libraries,
                 conf_file=conf_file, progressbars=progressbars,
                 job_resources=job_resources, out_dir=out_dir)
}
