#' Wrapper object for one or more ANTs transforms
#'
#' @description Wrapping paths to one or more ANTs registration files on disk in
#'   an \code{antsreg} object means that they can be used by
#'   \code{nat::\link{xform}} and inside \code{nat::\link{reglist}} objects.
#' @details At present, we assume that all ANTs transforms are specified by
#'   files on disk.
#'
#'   For the purposes of the swap argument, we assume we are trying to specify a
#'   registration in the direction required for images to be transformed. This
#'   is the opposite of the direction required for transformation of points.
#'
#'   Registrations are given in the order that they are to be applied reading
#'   from left to right. See \code{nat::\link{xform}} and
#'   \code{nat::\link{reglist}} for further details.
#'
#' @param ... One or more ANTs registrations.
#' @param swap Whether or not to invert the registration. Note that while affine
#'   registrations are invertible, deformation fields (usually specifying
#'   non-rigid registrations) are not.
#' @export
#' @return A character vector with additional class \code{antsreg}.
#' @examples
#' \dontrun{
#' # define forward and inverse registrations
#' inv=antsreg("JRC2018F_FAFB/JRC2018F_FAFB1InverseWarp_down.nii.gz",
#'     "JRC2018F_FAFB/JRC2018F_FAFB0GenericAffine.mat", swap=c(FALSE,TRUE))
#' fwd=antsreg("JRC2018F_FAFB/JRC2018F_FAFB0GenericAffine.mat",
#'     "JRC2018F_FAFB/JRC2018F_FAFB1Warp_down.nii.gz", swap=c(TRUE,TRUE))
#'
#' # position of DA1 glomerulus in FAFB
#' da1glomr.fafb <- cbind(429316, 217924, 42960)
#' da1glomr.fafbum=da1glomr.fafb/1e3
#' # map position from FAFB to JRC2018
#' res <- xform(da1glomr.fafbum, inv)
#' # and back again
#' res2 <- xform(res, fwd)
#'
#' # print out locations
#' da1glomr.fafbum
#' res
#' res2
#' }
#' @family antsreg
antsreg <- function(..., swap=NULL) {
  x <- path.expand(as.character(list(...)))
  if(!all(file.exists(x))) stop("... must point to files on disk!")
  swap <- if(is.null(swap)) rep(T, length(x)) else swap
  if(length(swap)!=length(x)) stop("swap must have same length as x!")
  attr(x,'swap') <- swap
  class(x)='antsreg'
  x
}

#' Map points using ANTs registrations
#'
#' @details See \code{\link{antsreg}} for details of the ordering of
#'   registrations.
#'
#' @param reg A \code{\link{antsreg}} object specifying one or more ANTs
#'   registrations.
#' @param points An Nx3 matrix of 3D points to transform
#' @param ... Additional arguments passed to \code{antsApplyTransformsToPoints}
#' @importFrom ANTsRCore antsApplyTransformsToPoints
#' @export
#' @return An Nx3 matrix of transformed points
#' @importFrom nat xformpoints
#' @family antsreg
xformpoints.antsreg <- function(reg, points, ...) {
  if(ncol(points)!=3L)
    stop("xformpoints.antsreg only supports 3 dimensions!")
  
  # ANTs wants the transforms in reverse order
  tlist=rev(reg)
  swapped=rev(attr(reg, 'swap'))
  if(is.null(swapped)) swapped=rep(TRUE, length(reg))
  
  antsApplyTransformsToPoints(3L, points, 
                              transformlist=rev(reg),
                              whichtoinvert = !swapped)
}


#' @description \code{as.antsreg} constructs an \code{antsreg} object from an
#'   ANTs ouput directory. This is more convenient for end users.
#'
#' @param x Path to a directory containing ANTs registration files
#' @param inverse Whether or not to select the inverse direction when
#'   constructing an \code{antsreg} object from a directory.
#'
#' @details ANTs typically writes registrations into folders containing a single
#'   affine matrix and a pair of deformation fields (one defining the forward
#'   transform, the other the inverse transform).
#' @export
#' @rdname antsreg
as.antsreg <- function(x, inverse=FALSE) {
  if(!file.exists(x)) stop("Cannot read directory :", x)
  if(!file.info(x)$isdir) stop(x, "is not a directory!")
  x <- normalizePath(x, mustWork = TRUE)
  ff=dir(x)
  affmat=grep("GenericAffine\\.mat$", ff, value = T)
  if(length(affmat)!=1)
    stop("I was expecting one affine .mat file, not ", length(affmat),"!")
  remaining=setdiff(ff, affmat)
  invdfield=grep("InverseWarp.*\\.nii(\\.gz){0,1}$", remaining, value = T)
  if(!length(invdfield)==1)
    stop("I was expecting one inverse deformation field, not ", length(invdfield),"!")
  remaining=setdiff(ff, invdfield)
  fwddfield <- grep("Warp.*\\.nii(\\.gz){0,1}$", remaining, value = T)
  if(!length(fwddfield)==1)
    stop("I was expecting one forward deformation field, not ", length(fwddfield),"!")
  if(inverse) {
    antsreg(file.path(x, affmat), file.path(x, fwddfield), swap=c(TRUE,TRUE))
  } else {
    antsreg(file.path(x, invdfield), file.path(x, affmat), swap=c(TRUE,FALSE))
  }
}

