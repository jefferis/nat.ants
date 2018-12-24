# nat.ants

The goal of nat.ants is to provide a bridge between the [NeuroAnatomy Toolbox](https://jefferis.github.io/nat/)
suite of 3D analysis and visualisation tools for R and the [ANTs](http://stnava.github.io/ANTs/)
image registration toolkit. This is achieved by wrapping the [ANTsRCore](https://github.com/ANTsX/ANTsRCore) 
R package with a small number of functions to enable `nat` functions to use
`ANTs` registrations.

## Installation

ANTsRcore provides everything that you need to use ANTs transformations. This can
make installation a little complicated since it requires compilation of several
large libraries including ITK. The easiest way that we have found to install ANTsRcore
is to use the [neuroconductor project](https://www.neuroconductor.org/package/ANTsRCore).

``` r
source("https://neuroconductor.org/neurocLite.R")
neuro_install('ANTsRCore')
```
You can then install *nat.ants* from GitHub

``` r
# install devtools if required
if (!requireNamespace("devtools")) install.packages("devtools")
# then install nat
devtools::install_github("jefferis/nat.ants")
```

## Example

This example makes use of ANTs registations developed by John Bogovic
and Stephan Saalfeld to map between different Drosophila brains.

See https://www.janelia.org/open-science/jrc-2018-brain-templates for downloads
and further information.

``` r
inv=antsreg("JRC2018F_FAFB/JRC2018F_FAFB1InverseWarp_down.nii.gz",
    "JRC2018F_FAFB/JRC2018F_FAFB0GenericAffine.mat", swap=c(FALSE,TRUE))
fwd=antsreg("JRC2018F_FAFB/JRC2018F_FAFB0GenericAffine.mat",
    "JRC2018F_FAFB/JRC2018F_FAFB1Warp_down.nii.gz", swap=c(TRUE,TRUE))
    
# position of DA1 glomerulus in FAFB
da1glomr.fafb <- cbind(429316, 217924, 42960)
da1glomr.fafbum=da1glomr.fafb/1e3
# map position from FAFB to JRC2018
res <- xform(da1glomr.fafbum, inv)
# and back again
res2 <- xform(res, fwd)

# print out locations
da1glomr.fafbum
res
res2
```

