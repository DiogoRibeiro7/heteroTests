# Random number streams for work done in parallel --------------------------
#
# A replicate computed in a worker process draws from that worker's random
# number generator, which set.seed() in the session does not decide: a forked
# worker starts from a state of its own unless the session uses L'Ecuyer-CMRG,
# and a socket worker starts from a seed of its own. Before 0.15.0 a seeded
# call with `parallel = TRUE` therefore returned a different p-value each time
# it ran.
#
# Since 0.15.0 every task run in parallel draws from a stream of its own: one
# number drawn from the session's generator seeds a L'Ecuyer-CMRG generator,
# whose streams (parallel::nextRNGStream()) are independent of one another by
# construction, and task i takes stream i. What a task draws then depends on
# the seed and on i alone, not on the number of workers, how the tasks are
# split between them, or whether the workers are forked or started as
# sockets. A serial call draws every replicate from the session's stream, as
# it always did, so it returns what it returned before 0.15.0, and a
# parallel call returns another value from the same seed: both are bootstraps
# of the same data, with different draws.

#' One L'Ecuyer-CMRG stream for each of n tasks
#'
#' Draws one number from the session's generator, which moves it on as any
#' random draw does, seeds a L'Ecuyer-CMRG generator with it and returns the
#' first `n` streams of that generator. The session's generator, its kind
#' included, is left as the single draw left it.
#'
#' @param n Number of tasks.
#' @return A list of `n` integer vectors, each a value of `.Random.seed`.
#' @keywords internal
#' @noRd
rtask_streams <- function(n) {
  seed <- sample.int(.Machine$integer.max, 1L)
  session <- get(".Random.seed", envir = globalenv())
  on.exit(assign(".Random.seed", session, envir = globalenv()), add = TRUE)
  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)
  stream <- get(".Random.seed", envir = globalenv())
  streams <- vector("list", n)
  for (i in seq_len(n)) {
    streams[[i]] <- stream
    stream <- parallel::nextRNGStream(stream)
  }
  streams
}

#' Run a task on a stream of its own
#'
#' Sets `.Random.seed` to `stream`, which also sets the kind of generator,
#' calls `fun(i)` and puts back the state the process had. In a worker that
#' state is the worker's own; run in the session, the session's generator is
#' left as it was.
#'
#' @param stream A value of `.Random.seed` from [rtask_streams()].
#' @param fun A function of one argument.
#' @param i The argument.
#' @return What `fun(i)` returns.
#' @keywords internal
#' @noRd
rrun_on_stream <- function(stream, fun, i) {
  env <- globalenv()
  had <- exists(".Random.seed", envir = env, inherits = FALSE)
  if (had) {
    saved <- get(".Random.seed", envir = env)
  }
  on.exit(
    if (had) {
      assign(".Random.seed", saved, envir = env)
    } else {
      rm(".Random.seed", envir = env)
    },
    add = TRUE
  )
  assign(".Random.seed", stream, envir = env)
  fun(i)
}

#' Run tasks in parallel, each on a stream of its own
#'
#' Forks the workers with [parallel::mclapply()] where the platform allows it
#' and starts a socket cluster with [parallel::makeCluster()] elsewhere. Task
#' `i` draws from stream `i` of `streams` on either path, so the result does
#' not depend on the path or on the number of workers.
#'
#' @param indices Integer vector of task numbers, a subset of
#'   `seq_along(streams)`.
#' @param fun A function of one task number.
#' @param streams Streams from [rtask_streams()].
#' @param workers Number of worker processes.
#' @param cl A cluster from [parallel::makeCluster()] to run the tasks on, on
#'   any platform, or `NULL` to fork where the platform can and to start a
#'   socket cluster for this call elsewhere.
#' @return A list of the values of `fun`, in the order of `indices`.
#' @keywords internal
#' @noRd
rparallel_tasks <- function(indices, fun, streams, workers, cl = NULL) {
  # Forced here, in the session: an argument left as a promise travels with
  # the task to a socket worker and is evaluated there, so rtask_streams()
  # would draw from the worker's generator.
  force(fun)
  force(streams)
  # The function travels with the task to a socket worker, so the task does
  # not depend on the version of the package the worker loads.
  run <- rrun_on_stream
  task <- function(i) run(streams[[i]], fun, i)
  if (!is.null(cl)) {
    return(parallel::parLapply(cl, as.list(indices), task))
  }
  if (.Platform$OS.type == "unix") {
    # Each task sets its own stream, so the seeds mclapply() would set are not
    # needed, and leaving them out leaves the session's generator alone. For
    # the same reason the tasks can be split between the workers in advance,
    # one fork per worker rather than one per task, without changing what any
    # task draws.
    return(parallel::mclapply(indices, task,
      mc.cores = workers, mc.preschedule = TRUE, mc.set.seed = FALSE
    ))
  }
  cl <- parallel::makeCluster(workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, library(heteroTests))
  parallel::parLapply(cl, as.list(indices), task)
}
