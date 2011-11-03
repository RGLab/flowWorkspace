
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#define IS_SET(b, i, bit) ((b)[i] != 0 && ((b)[i] & (1 << (bit))))

SEXP ncdf_bitarray_set(SEXP bits, SEXP _indx) {
    SEXP ans = PROTECT(duplicate(bits)), btcnt;
    unsigned char *bytes = (unsigned char*) RAW(ans);
    int *indx =  LOGICAL(_indx);
    int len = asInteger(getAttrib(bits, install("bitlen"))) ;
    int i, byteIndex, bitIndex , nSet =0;
    unsigned char mask;
    int st =0 ;
    for( i =0 ; i < len; i++) {
        byteIndex = i / 8;
        bitIndex = i % 8;
        st = IS_SET(bytes, byteIndex, bitIndex);
        if(indx[i] == 1) {
            // set bit  
            if(!st){
                mask  = 1 << bitIndex;
                bytes[byteIndex] = bytes[byteIndex] | mask;
            }
            nSet++;
        } else {
            //reset bit
            if(st){
                mask  = ~(1 << bitIndex);
                bytes[byteIndex] = bytes[byteIndex] & mask;
            }
        }
    }
    PROTECT(btcnt = ScalarInteger(nSet));
    setAttrib(ans, install("nbitset"), btcnt);
    UNPROTECT(2);
    return(ans);
}

SEXP ncdf_bitarray_getSetBitPos(SEXP bits) {
    SEXP ans;
    int n = asInteger(getAttrib(bits, install("nbitset"))) ;
    PROTECT(ans = allocVector(INTSXP, n));
    int *indx = INTEGER(ans);
    unsigned char *bytes = (unsigned char*) RAW(bits);
    int len = asInteger(getAttrib(bits, install("bitlen"))) ;
    int i, byteIndex, bitIndex, subIndx =0;

    for( i =0 ; i < len; i++) {
        byteIndex = i / 8;
        bitIndex = i % 8;
        if(IS_SET(bytes, byteIndex, bitIndex)) {
            indx[subIndx] = i+1;
            subIndx++;
        }

    }
    UNPROTECT(1);
    return(ans);
}

SEXP ncdf_bitarray_getBitStatus(SEXP bits) {
    SEXP ans;
    unsigned char *bytes = (unsigned char*) RAW(bits);
    int len = asInteger(getAttrib(bits, install("bitlen"))) ;
    PROTECT(ans = allocVector(LGLSXP, len));
    int *indx = LOGICAL(ans);
    int i, byteIndex, bitIndex;

    for( i =0 ; i < len; i++) {
        byteIndex = i / 8;
        bitIndex = i % 8;
        if(IS_SET(bytes, byteIndex, bitIndex)) {
            indx[i] = 1;
        } else {
            indx[i] = 0;
        }
    }
    UNPROTECT(1);
    return(ans);
}

SEXP ncdf_bitarray_Flip(SEXP bits) {
    SEXP ans = PROTECT(duplicate(bits)), btcnt;
    unsigned char *bytes = (unsigned char*) RAW(ans);
    int len = asInteger(getAttrib(bits, install("bitlen"))) ;
    int i, byteIndex, bitIndex , nSet =0;
    unsigned char mask;
    for( i =0 ; i < len; i++) {
        byteIndex = i / 8;
        bitIndex = i % 8;
        if(!IS_SET(bytes, byteIndex, bitIndex)){
            //set bit
            mask  = 1 << bitIndex;
            bytes[byteIndex] = bytes[byteIndex] | mask;
            nSet++;
        } else {
            // unset bit
            mask  = ~(1 << bitIndex);
            bytes[byteIndex] = bytes[byteIndex] & mask;
            
        }
    }
    PROTECT(btcnt = ScalarInteger(nSet));
    setAttrib(ans, install("nbitset"), btcnt);
    UNPROTECT(2);
    return(ans);
}

