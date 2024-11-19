# mandelbrot

<!-- badges: start -->
<!-- badges: end -->

Explore the Mandelbrot set in an interactive Shiny application.
There are many ways to download and run the app:

``` r
library(shiny)

# Easiest way is to use runGitHub
runGitHub("mandelbrot", "hrryt")

# Run a tar or zip file directly
runUrl("https://github.com/hrryt/mandelbrot/archive/master.tar.gz")
runUrl("https://github.com/hrryt/mandelbrot/archive/master.zip")
```

Or you can clone the git repository, then use `runApp()`:

```r
# First clone the repository with git. If you have cloned it into
# ~/mandelbrot, first go to that directory, then use runApp().
setwd("~/mandelbrot")
runApp()
```

<img src="README-mandelbrot.png" />
