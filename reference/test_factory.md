# Shared instance of the test factory

Users can register new tests via this object. Built-in tests are
pre-registered at package load time.

## Details

Deprecated since 0.14.0, with
[TestFactory](https://diogoribeiro7.github.io/heteroTests/reference/TestFactory.md):
it leaves the exports in 1.0.0, and as an object it cannot warn when
used. Use
[`registerDiagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/registerDiagnostic.md)
and
[`runHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)
instead.
