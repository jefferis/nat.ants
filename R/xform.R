#' Wrapper object for one or more ANTs transforms
#'
#' @description Wrapping paths to one or more ANTs registration files on disk in
#'   an \code{antsreg} object means that they can be used by
#'   \code{nat::\link{xform}} and inside \code{nat::reglist} objects.
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
