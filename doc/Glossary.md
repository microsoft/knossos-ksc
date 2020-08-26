## Knossos Glossary

Concepts that arise throughout the documentation

## KINAUFL

Knossos is not a user-facing language.  

The IR as been designed to be ultra-lightweight, in order to simplify interop with a wide range of source languages and backends.  Wherever that light weight causes runtime slowdown, we expect to recover it using rewrite rules.  At the same time, the IR is very strongly designed to be 1-1 with its text representation, to keep compiler passes honest, and to facilitate debugging.  So we tread a tightrope -- it is a merit that it's human readable and writeable, but we want to eschew festoonage.
