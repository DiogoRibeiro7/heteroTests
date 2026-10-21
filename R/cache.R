# The caches of results: cachedTest() and runHeteroTests(use_cache = TRUE).
#
# The contract, decided by the author on 2026-10-21:
#
# - A test that draws random numbers is never cached. The package flags each
#   test it registers as deterministic or random (rregister_package_diagnostic(),
#   R/diagnosticsRegistry.R), and the caches hold only those flagged
#   deterministic: a resampling test, and any diagnostic a user registers or
#   registers again under a name of the package, runs every time, through
#   cachedTest(), runHeteroTests(), the registry and runHeteroTestsParallel()
#   alike. Its result, and the stream of random numbers after it, are then
#   those of a call without the cache. As a second guard, a result whose
#   computation moved the generator (.Random.seed) is not stored.
# - A deterministic test is cached under a key that holds everything that
#   determines its result (rcache_fit_key(), rcache_data_key()): the test and
#   its arguments; the class, family and link of the model; the parts of the
#   model context the tests read (rmodel_context()); the terms, the call and
#   the other parts of the fit; and `data`, whole. The key holds values, never
#   environments, so a model refitted to the same data in another environment
#   has the same key, and the result returned carries the model of the call.
# - Each cache holds at most getOption("heteroTests.cache_size", 100) results
#   and evicts the one used least recently; 0 turns both caches off. The keys
#   are kept in a character vector, never as the names of an environment: R
#   never frees a name once it has been used as one, and before 0.16.0 every
#   key left 0.2 to 0.3 KB behind after the caches were cleared.
#
# The validation cache of R/validation.R uses the same store, with its own
# bound (getOption("heteroTests.validation_cache_size", 1000)).

# The default number of results each cache of results holds.
.ht_cache_size_default <- 100L

#' A bounded store that evicts the entry used least recently
#'
#' An environment holding the keys (a character vector), the values (a list)
#' and, for each entry, when it was last stored or read (`used`, from the
#' counter `tick`). A lookup is a `match()` of the key in the keys.
#'
#' @return The store, an environment.
#' @keywords internal
#' @noRd
rcache_new <- function() {
  store <- new.env(parent = emptyenv())
  rcache_clear(store)
  store
}

rcache_clear <- function(store) {
  store$keys <- character()
  store$values <- list()
  store$used <- numeric()
  store$tick <- 0
  invisible(NULL)
}

rcache_size <- function(store) {
  length(store$keys)
}

rcache_get <- function(store, key) {
  if (is.null(key)) {
    return(NULL)
  }
  slot <- match(key, store$keys)
  if (is.na(slot)) {
    return(NULL)
  }
  store$tick <- store$tick + 1
  store$used[slot] <- store$tick
  store$values[[slot]]
}

#' Store a value, evicting the entries used least recently beyond `limit`
#'
#' @param store The store ([rcache_new()]).
#' @param key Character scalar, or `NULL` to store nothing.
#' @param value The value.
#' @param limit The most entries the store may hold; `0` empties it and
#'   stores nothing.
#' @return `value`, invisibly.
#' @keywords internal
#' @noRd
rcache_put <- function(store, key, value, limit) {
  if (is.null(key)) {
    return(invisible(value))
  }
  if (limit < 1) {
    rcache_clear(store)
    return(invisible(value))
  }
  slot <- match(key, store$keys)
  if (is.na(slot)) {
    excess <- length(store$keys) - limit + 1
    if (excess > 0) {
      drop <- order(store$used)[seq_len(excess)]
      store$keys <- store$keys[-drop]
      store$values <- store$values[-drop]
      store$used <- store$used[-drop]
    }
    slot <- length(store$keys) + 1L
    store$keys[slot] <- key
  }
  store$tick <- store$tick + 1
  store$used[slot] <- store$tick
  # Taken out of the environment first, the list has a single reference and
  # is changed in place rather than copied on every store.
  values <- store$values
  store$values <- NULL
  values[slot] <- list(value)
  store$values <- values
  invisible(value)
}

#' The bound of a cache, from its option
#'
#' @param option Character scalar, the name of the option.
#' @param default The bound when the option is not set.
#' @return A non-negative whole number.
#' @keywords internal
#' @noRd
rcache_limit <- function(option, default) {
  value <- getOption(option, default)
  if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value) ||
        value < 0 || value != round(value)) {
    stop(
      sprintf(
        "`options(%s = )` must be a single non-negative whole number; 0 turns the cache off.",
        option
      ),
      call. = FALSE
    )
  }
  value
}

rcache_result_limit <- function() {
  rcache_limit("heteroTests.cache_size", .ht_cache_size_default)
}

# Are the caches of results on? They need digest for the keys.
rcache_enabled <- function() {
  rcache_result_limit() > 0 && requireNamespace("digest", quietly = TRUE)
}

#' May a registered test be cached?
#'
#' Only the tests the package registers itself and flags as deterministic
#' ([rregister_package_diagnostic()]): never one that draws random numbers, nor
#' a diagnostic registered with [registerDiagnostic()] or with
#' `test_factory$register()`, whose behaviour the package cannot know.
#'
#' @param test_name Character scalar.
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
rcache_cacheable <- function(test_name) {
  is.character(test_name) && length(test_name) == 1L && !is.na(test_name) &&
    identical(get0(test_name, envir = .diagnostic_kind, inherits = FALSE), "deterministic")
}

#' The xxhash64 digest of a value
#'
#' The value is serialized in the native byte order (`xdr = FALSE`), which
#' writes a vector of doubles as it is in memory, three to four times faster
#' than the big-endian serialization `digest::digest()` makes; the keys live
#' in one session only, so the order of the bytes of the machine does not
#' matter. It is serialized in version 2 of the format, which writes a vector
#' as its values whatever its representation in memory: version 3 writes an
#' ALTREP vector by its state, which changes once its values have been read,
#' so that the names of the residuals of a fit, `as.character()` of its row
#' numbers until a test reads them, would give the same fit another key after
#' its first test.
#'
#' @param x Any value.
#' @return Character scalar.
#' @keywords internal
#' @noRd
rcache_hash <- function(x) {
  digest::digest(
    serialize(x, connection = NULL, xdr = FALSE, version = 2L),
    algo = "xxhash64", serialize = FALSE
  )
}

# The state of the session's generator: .Random.seed, or NULL before it is
# first used. A computation that changes it drew random numbers.
rrng_state <- function() {
  get0(".Random.seed", envir = globalenv(), inherits = FALSE)
}

#' A value without the environments it refers to
#'
#' Formulas and terms lose their environment, environments become a marker,
#' and functions their source, so that the key of a model refitted in another
#' environment is the key of the first fit. Used on the small parts of a key
#' (terms, call, family, attributes), never on the vectors of the data.
#'
#' @param x Any value.
#' @return `x` without environments.
#' @keywords internal
#' @noRd
rcache_plain <- function(x) {
  if (is.environment(x)) {
    return("<environment>")
  }
  if (is.function(x)) {
    return(paste(deparse(x), collapse = "\n"))
  }
  if (is.list(x) && !is.data.frame(x)) {
    at <- attributes(x)
    x <- lapply(unclass(x), rcache_plain)
    at$.Environment <- NULL
    attributes(x) <- at
    return(x)
  }
  if (!is.null(attr(x, ".Environment", exact = TRUE))) {
    attr(x, ".Environment") <- NULL
  }
  x
}

#' The key of a fitted model: everything a test reads from it
#'
#' Built from the model context ([rmodel_context()] without `data`), which
#' takes from the stored fit the model frame, the response, the design (every
#' column), the residuals and fitted values, the prior weights, the total
#' offset, the rank and the aliased columns, the names of the observations
#' and `na.action`; to them it adds the class of the model, its family and
#' link, its terms (with their `predvars`) and call, its coefficients, the
#' linear predictor of a `glm`, the weights it stores (the estimated weights
#' of a [fitWLS()] fit, the working weights of a `glm`), its residual
#' degrees of freedom, the pivot of its decomposition, its contrasts and
#' levels, whether it keeps its model frame, and its attributes (the variance
#' function of a [fitWLS()] fit). The vectors are hashed without their names,
#' which are those of the observations, hashed once.
#'
#' A model the context refuses, such as a `glm` that is not a least-squares
#' fit, a multivariate `lm` or a fit with `model = FALSE` whose model frame
#' cannot be rebuilt, or rebuilds differently from its fit, has no key: the
#' test runs without the cache and refuses it, as it does uncached.
#'
#' @param model The fitted model.
#' @return Character scalar, an xxhash64 digest, or `NULL`.
#' @keywords internal
#' @noRd
rcache_fit_key <- function(model) {
  if (!inherits(model, "lm")) {
    return(NULL)
  }
  ctx <- tryCatch(
    suppressWarnings(suppressMessages(rmodel_context(model, NULL, fn = "cachedTest"))),
    error = function(e) NULL
  )
  if (is.null(ctx)) {
    return(NULL)
  }
  family <- if (inherits(model, "glm")) {
    fam <- tryCatch(stats::family(model), error = function(e) NULL)
    if (!is.null(fam)) c(family = fam$family, link = fam$link)
  }
  design <- ctx$design
  dimnames(design) <- list(NULL, colnames(design))
  frame <- ctx$frame
  attr(frame, "terms") <- rcache_plain(attr(frame, "terms"))
  extra <- attributes(model)
  extra$names <- NULL
  parts <- list(
    version = "heteroTests 0.16.0 cache key 1",
    class = class(model),
    kind = ctx$kind,
    family = family,
    call = rcache_plain(model$call),
    terms = rcache_plain(stats::terms(model)),
    stored_frame = !is.null(model[["model"]]),
    frame = frame,
    obs_id = ctx$obs_id,
    na_action = ctx$na_action,
    response = unname(ctx$response),
    design = design,
    coefficients = model$coefficients,
    residuals = unname(ctx$residuals),
    fitted = unname(ctx$fitted),
    linear_predictors = unname(model$linear.predictors),
    prior_weights = ctx$weights,
    weights = unname(model$weights),
    offset = ctx$offset,
    rank = ctx$rank,
    aliased = ctx$aliased,
    qr_tol = ctx$qr_tol,
    pivot = model$qr$pivot,
    df_residual = model$df.residual,
    contrasts = model$contrasts,
    xlevels = model$xlevels,
    attributes = rcache_plain(extra)
  )
  tryCatch(rcache_hash(parts), error = function(e) NULL)
}

#' The key of the `data` of a call: the whole object
#'
#' Its values, its row names, its class and its attributes (such as the
#' spatial weights `attr(data, "listw")` of the registered spatial test).
#'
#' @param data The `data` argument, or `NULL`.
#' @return Character scalar, or `NULL` when it cannot be hashed.
#' @keywords internal
#' @noRd
rcache_data_key <- function(data) {
  tryCatch(rcache_hash(rcache_plain_data(data)), error = function(e) NULL)
}

# A data frame keeps its columns as they are; a formula or environment that a
# caller passes as `data` loses its environment.
rcache_plain_data <- function(data) {
  if (is.data.frame(data) || is.null(data) || is.atomic(data)) data else rcache_plain(data)
}

#' The keys of the inputs of a call, computed once per call
#'
#' @param model,data The model and the data of the call.
#' @return A list with `fit` and `data`, or `NULL` when either has no key,
#'   in which case nothing is cached.
#' @keywords internal
#' @noRd
rcache_inputs <- function(model, data) {
  if (!rcache_enabled()) {
    # Turned off: what the caches held is dropped too.
    rcache_clear(.test_cache)
    rcache_clear(.analysis_cache)
    return(NULL)
  }
  fit <- rcache_fit_key(model)
  if (is.null(fit)) {
    return(NULL)
  }
  data_key <- rcache_data_key(data)
  if (is.null(data_key)) {
    return(NULL)
  }
  list(fit = fit, data = data_key)
}

rcache_test_key <- function(test_name, inputs, args = list()) {
  if (is.null(inputs)) {
    return(NULL)
  }
  tryCatch(
    rcache_hash(list("test", test_name, rcache_plain(args), inputs$fit, inputs$data)),
    error = function(e) NULL
  )
}

rcache_analysis_key <- function(tests, streaming, chunk_size, inputs) {
  if (is.null(inputs)) {
    return(NULL)
  }
  tryCatch(
    rcache_hash(
      list("analysis", as.character(tests), streaming, chunk_size, inputs$fit, inputs$data)
    ),
    error = function(e) NULL
  )
}

#' A result as it is stored: without the model of the call
#'
#' A result carries the model it was computed on as `attr(, "model")`, the
#' whole fit; the entry keeps the result without it, and gives the model of
#' the call back when it is read ([rcache_attach()]), so that a hit is
#' `identical()` to the result the test computes on that model.
#'
#' @param x A result, or a `hetero_test_suite` of results.
#' @param model The model of the call.
#' @return A list with the value and where the model was taken out.
#' @keywords internal
#' @noRd
rcache_detach <- function(x, model) {
  attached <- identical(attr(x, "model", exact = TRUE), model)
  if (attached) {
    attr(x, "model") <- NULL
  }
  inner <- logical()
  if (inherits(x, "hetero_test_suite")) {
    inner <- vapply(
      seq_along(x),
      function(i) identical(attr(x[[i]], "model", exact = TRUE), model),
      logical(1)
    )
    for (i in which(inner)) {
      attr(x[[i]], "model") <- NULL
    }
  }
  list(value = x, attached = attached, inner = inner)
}

rcache_attach <- function(entry, model) {
  x <- entry$value
  for (i in which(entry$inner)) {
    attr(x[[i]], "model") <- model
  }
  if (isTRUE(entry$attached)) {
    attr(x, "model") <- model
  }
  x
}

#' Run a registered test through the cache of `cachedTest()`
#'
#' Looks the result up under the key of the test, its arguments and the
#' inputs; on a miss runs it with [.run_registered_diagnostic()] and stores
#' it, unless its computation drew random numbers.
#'
#' @param test_name Character scalar naming the registered test.
#' @param model,data The model and the data of the call.
#' @param inputs The keys of the inputs ([rcache_inputs()]), or `NULL` to run
#'   the test without the cache.
#' @param ... Further arguments of the test.
#' @return The result of the test.
#' @keywords internal
#' @noRd
rcached_diagnostic <- function(test_name, model, data, inputs, ...) {
  key <- rcache_test_key(test_name, inputs, list(...))
  entry <- rcache_get(.test_cache, key)
  if (!is.null(entry)) {
    return(rcache_attach(entry, model))
  }
  state <- rrng_state()
  result <- .run_registered_diagnostic(test_name, model, data, ...)
  if (!is.null(key) && identical(state, rrng_state())) {
    rcache_put(.test_cache, key, rcache_detach(result, model), rcache_result_limit())
  }
  result
}
