# ambar-sqlserver

A wrapper around the [`mssql-simple`](https://hackage.haskell.org/package/mssql-simple) library to provide:
- Applicative interfaces for field and row parsing.
- Ability to provide explicit parsers for queries.
- A strongly typed `Query` type with automatic transaction wrapping.
- A `ToRow` class for standardised encoding of types in queries.

-
