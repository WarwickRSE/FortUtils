# FortUtils

Handy Fortran snippets

## Generic Helper Code

A collection of generic snippets to make common tasks a bit easier. Developed
from teaching/training code.

Currently contains:

- command\_line.f90
  - A command line parser to allow flag or key/value pairs
  - Arguments are parsed as a target type supplied by client
  - Spaces within identifiers or values must be quoted - other spaces are allowed

## Usage Notes

These snippets are intended more to be copied and (potentially) modified, than to be included as a library. They aim to tackle common tasks in a more robust fashion than most of us would have time to write every time. Please keep the attribution in the files intact if you do!
