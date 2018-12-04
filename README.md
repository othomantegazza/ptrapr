# An R package to analyze Rice panicle architecture.

With this package soon you will be able to parse and analyze the output of [PTRAP](https://bioinfo.ird.fr/index.php/resources/49-uncategorised/102-p-trap)

# Development

This package is in the first stages of development. You are welcome to contribute opening an issue or sending a pull request.

# Install

You can install `ptrapr` from Github with `devtools`.

```r
install.packages("devtools")
devtools::install_github("othomantegazza/ptrapr")
```

## Notes

This package imports the R package `rgeos`. To use `rgeos` on Ubuntu I had to install these system packages:

```sh
sudo apt-get install libgeos-dev
sudo apt-get install libgeos++-dev 
```

# Use - documentation

Access here **online** the [full documentation](https://othomantegazza.github.io/ptrapr/) and the [package vignette](https://othomantegazza.github.io/fluidgr/articles/)

# Warranty

This package is under active development and comes with no warranty.
