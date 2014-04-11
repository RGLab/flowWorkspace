/* We copy "natural method" part of spline.c from stats package,
 *
 *	Natural Splines
 *	---------------
 *	Here the end-conditions are determined by setting the second
 *	derivative of the spline at the end-points to equal to zero.
 *
 *	There are n-2 unknowns (y[i]'' at x[2], ..., x[n-1]) and n-2
 *	equations to determine them.  Either Choleski or Gaussian
 *	elimination could be used.
 */
#include "include/spline.hpp"
/*TODO:change it to c++ version
 * n :number of input points
 * x,y:coordinates of input points
 * b,c,d:output with the same length of x
 */
void natural_spline_C(int n, double *x, double *y, double *b, double *c, double *d)
{
//	COUT<<"entering natural_spline_C"<<endl;
    int nm1, i;
    double t;

    x--; y--; b--; c--; d--;

    if(n < 2) {
    	throw(domain_error("not enough number of points"));
	return;
    }

    if(n < 3) {
	t = (y[2] - y[1]);
	b[1] = t / (x[2]-x[1]);
	b[2] = b[1];
	c[1] = c[2] = d[1] = d[2] = 0.0;
	return;
    }

    nm1 = n - 1;

//	COUT<<"Set up the tridiagonal system"<<endl;

    /* Set up the tridiagonal system */
    /* b = diagonal, d = offdiagonal, c = right hand side */

    d[1] = x[2] - x[1];
    c[2] = (y[2] - y[1])/d[1];
    for( i=2 ; i<n ; i++) {
	d[i] = x[i+1] - x[i];
	b[i] = 2.0 * (d[i-1] + d[i]);
	c[i+1] = (y[i+1] - y[i])/d[i];
	c[i] = c[i+1] - c[i];
    }

//	COUT<<"Gaussian elimination"<<endl;

    /* Gaussian elimination */

    for(i=3 ; i<n ; i++) {
	t = d[i-1]/b[i-1];
	b[i] = b[i] - t*d[i-1];
	c[i] = c[i] - t*c[i-1];
    }

    /* Backward substitution */

    c[nm1] = c[nm1]/b[nm1];
    for(i=n-2 ; i>1 ; i--)
	c[i] = (c[i]-d[i]*c[i+1])/b[i];

    /* End conditions */

    c[1] = c[n] = 0.0;

    /* Get cubic coefficients */
//	COUT<<"Get cubic coefficients"<<endl;

    b[1] = (y[2] - y[1])/d[1] - d[i] * c[2];
    c[1] = 0.0;
    d[1] = c[2]/d[1];
    b[n] = (y[n] - y[nm1])/d[nm1] + d[nm1] * c[nm1];
    for(i=2 ; i<n ; i++) {
	b[i] = (y[i+1]-y[i])/d[i] - d[i]*(c[i+1]+2.0*c[i]);
	d[i] = (c[i+1]-c[i])/d[i];
	c[i] = 3.0*c[i];
    }
    c[n] = 0.0;
    d[n] = 0.0;

    return;
}


/*
 * *method=2 is natural method
 * nu is the length of input vector
 * u: is the input vector x
 * v:is the output vector y
 * n,x,y, b,c,d: from spline_coef
 */
void spline_eval_C(int *method, int *nu, double *u, double *v,
		 int *n, double *x, double *y, double *b, double *c, double *d)
{
/* Evaluate  v[l] := spline(u[l], ...),	    l = 1,..,nu, i.e. 0:(nu-1)
 * Nodes x[i], coef (y[i]; b[i],c[i],d[i]); i = 1,..,n , i.e. 0:(*n-1)
 */
    const int n_1 = *n - 1;
    int i, j, k, l;
    double ul, dx, tmp;

    if(*method == 1 && *n > 1) { /* periodic */
	dx = x[n_1] - x[0];
	for(l = 0; l < *nu; l++) {
	    v[l] = fmod(u[l]-x[0], dx);
	    if(v[l] < 0.0) v[l] += dx;
	    v[l] += x[0];
	}
    }
    else {
	for(l = 0; l < *nu; l++)
	    v[l] = u[l];
    }

    i = 0;
    for(l = 0; l < *nu; l++) {
	ul = v[l];
	if(ul < x[i] || (i < n_1 && x[i+1] < ul)) {
	    /* reset i  such that  x[i] <= ul <= x[i+1] : */
	    i = 0;
	    j = *n;
	    do {
		k = (i+j)/2;
		if(ul < x[k]) j = k;
		else i = k;
	    }
	    while(j > i+1);
	}
	dx = ul - x[i];
	/* for natural splines extrapolate linearly left */
	tmp = (*method == 2 && ul < x[0]) ? 0.0 : d[i];

	v[l] = y[i] + dx*(b[i] + dx*(c[i] + dx*tmp));
    }
}

/*
 * valarray version
 */

void natural_spline(valarray<double>x, valarray<double> y, valarray<double>& b,valarray<double>& c,valarray<double>& d)
{
    int nm1, i;
    double t;
    int n=x.size();
//    x--; y--; b--; c--; d--;

    if(n < 2) {
	throw(domain_error("not enough number of points"));
	return;
    }

    if(n < 3) {
	t = (y[1] - y[0]);
	b[0] = t / (x[1]-x[0]);
	b[1] = b[0];
	c[0] = c[1] = d[0] = d[1] = 0.0;
	return;
    }

    nm1 = n - 2;

    /* Set up the tridiagonal system */
    /* b = diagonal, d = offdiagonal, c = right hand side */

    d[0] = x[1] - x[0];
    c[1] = (y[1] - y[0])/d[0];
    for( i=1 ; i<n-1 ; i++) {
	d[i] = x[i+1] - x[i];
	b[i] = 2.0 * (d[i-1] + d[i]);
	c[i+1] = (y[i+1] - y[i])/d[i];
	c[i] = c[i+1] - c[i];
    }

    /* Gaussian elimination */

    for(i=2 ; i<n-1 ; i++) {
	t = d[i-1]/b[i-1];
	b[i] = b[i] - t*d[i-1];
	c[i] = c[i] - t*c[i-1];
    }

    /* Backward substitution */

    c[nm1] = c[nm1]/b[nm1];
    for(i=n-2-1 ; i>0 ; i--)
	c[i] = (c[i]-d[i]*c[i+1])/b[i];

    /* End conditions */

    c[0] = c[n-1] = 0.0;

    /* Get cubic coefficients */

    b[0] = (y[1] - y[0])/d[0] - d[i-1] * c[1];
    c[0] = 0.0;
    d[0] = c[1]/d[0];
    b[n-1] = (y[n-1] - y[nm1])/d[nm1] + d[nm1] * c[nm1];
//    COUT<<"loop to Get cubic coefficients"<<endl;
    for(i=1 ; i<n-1 ; i++) {
	b[i] = (y[i+1]-y[i])/d[i] - d[i]*(c[i+1]+2.0*c[i]);
	d[i] = (c[i+1]-c[i])/d[i];
	c[i] = 3.0*c[i];
    }
//    COUT<<"end loop"<<endl;
    c[n-1] = 0.0;
    d[n-1] = 0.0;

}

void spline_eval(int method, valarray<double> u, valarray<double> & v,
		  const valarray<double> & x, const valarray<double> & y, const valarray<double> & b, const valarray<double> & c, const valarray<double> & d)
{
/* Evaluate  v[l] := spline(u[l], ...),	    l = 1,..,nu, i.e. 0:(nu-1)
 * Nodes x[i], coef (y[i]; b[i],c[i],d[i]); i = 1,..,n , i.e. 0:(*n-1)
 */
//	COUT<<"entering spline_eval"<<endl;

	int n=x.size();
	int nu=u.size();
    const int n_1 = n - 1;
    int i, j, k, l;
    double ul, dx, tmp;

    if(method == 1 && n > 1) { /* periodic */
	dx = x[n_1] - x[0];
	for(l = 0; l < nu; l++) {
	    v[l] = fmod(u[l]-x[0], dx);
	    if(v[l] < 0.0) v[l] += dx;
	    v[l] += x[0];
	}
    }
    else {
	for(l = 0; l < nu; l++)
	    v[l] = u[l];
    }

    i = 0;
    for(l = 0; l < nu; l++) {
	ul = v[l];
	if(ul < x[i] || (i < n_1 && x[i+1] < ul)) {
	    /* reset i  such that  x[i] <= ul <= x[i+1] : */
	    i = 0;
	    j = n;
	    do {
		k = (i+j)/2;
		if(ul < x[k]) j = k;
		else i = k;
	    }
	    while(j > i+1);
	}
	dx = ul - x[i];
	/* for natural splines extrapolate linearly left */
	tmp = (method == 2 && ul < x[0]) ? 0.0 : d[i];

	v[l] = y[i] + dx*(b[i] + dx*(c[i] + dx*tmp));
    }
}
