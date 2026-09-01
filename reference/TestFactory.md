# Heteroscedasticity Test Factory

Provides a registry of heteroscedasticity tests with metadata and
convenience methods to run them with input validation. This is an
alternative to the older environment-based registry used by
[`runHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md).

## Usage

The shared instance `test_factory` can be used to register new tests and
execute them. See `register` and `run_test` methods for details.

## Methods

### Public methods

- [`TestFactory$register()`](#method-TestFactory-register)

- [`TestFactory$get_available()`](#method-TestFactory-get_available)

- [`TestFactory$run_test()`](#method-TestFactory-run_test)

- [`TestFactory$clone()`](#method-TestFactory-clone)

------------------------------------------------------------------------

### Method `register()`

#### Usage

    TestFactory$register(name, func, metadata = list())

#### Arguments

- `name`:

  Name of the test

- `func`:

  Function with arguments `model` and `data`

- `metadata`:

  Optional list with metadata fields

#### Returns

Invisibly returns the factory Get available tests

------------------------------------------------------------------------

### Method `get_available()`

#### Usage

    TestFactory$get_available(data_type = NULL, min_n = NULL)

#### Arguments

- `data_type`:

  Filter by supported data type

- `min_n`:

  Filter by minimum observations

#### Returns

Character vector of test names Run a registered test

------------------------------------------------------------------------

### Method `run_test()`

#### Usage

    TestFactory$run_test(test_name, model, data, ...)

#### Arguments

- `test_name`:

  Name of the test

- `model`:

  Fitted model

- `data`:

  Data frame used to fit the model

- `...`:

  Additional arguments passed to the test

#### Returns

Result of the test

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TestFactory$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
