# TODO: Add comment
# 
# Author: mike
###############################################################################


## bitlen gives number of bits intended to be saved
## nbitset is the number of 1
## length(gives number of bytes used)
### n is number of bits inteded to be saved
.makeBitVecZeros <- function(n) {
	n <- as.integer(n)
	structure(raw(ceiling(n / 8)), bitlen = n, nbitset = 0L)
}

.makeBitVecOnes <- function(n) {
	bt <- .makeBitVecZeros(n)
	indx <- rep(TRUE, n)
	sbt <- .Call("ncdf_bitarray_set", bt, indx)
	sbt
}

.makeBitVec <- function(n, indx) {
	bt <- .makeBitVecZeros(n)
	if(length(indx) != attr(bt, "bitlen"))
		stop("indx must be  a logical vector of the same length as number of
						bits represented in the bit vector")
	sbt <- .Call("ncdf_bitarray_set", bt, indx)
	sbt
}

.getSetBitIndx <- function(bt){
	.Call("ncdf_bitarray_getSetBitPos", bt)
}

.getBitStatus <- function(bt){
	.Call("ncdf_bitarray_getBitStatus", bt)
}
