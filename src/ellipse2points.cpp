#include "include/ellipse2points.hpp"
ellipse_parsed parseEllipse(vector<float> x, vector<float> y){


// get center
  if(x.size()!=y.size())
    throw(logic_error("invalid antipodal coordinates!"));

  int n = x.size();
  float mu_x = 0, mu_y = 0;
  for(auto & i : x)
    mu_x+=i;
  mu_x/=n;
  for(auto & i : y)
    mu_y+=i;
  mu_y/=n;


 //center the antipods
  for(auto & i :x)
    i-=mu_x;
  for(auto & i :y)
    i-=mu_y;

  //compute  a, b
  //far left point
  int l = min_element(x.begin(), x.end()) - x.begin();

  //far right point
  int r = max_element(x.begin(), x.end()) - x.begin();
  //get length of a
  int  a = sqrt(pow(x[l]-x[r],2) + pow(y[l]-y[r],2))/2;

  // use rest of two points for b
  vector<int> ind;
  for(int i = 0; i < n; ++i)
    if(i !=l &&i!=r)
      ind.push_back(i);
  int  b = sqrt(pow(x[ind[0]]-x[ind[1]],2) + pow(y[ind[0]]-y[ind[1]],2))/2;

  //compute angle
  float alpha = atan2(y[r]-y[l],x[r]-x[l]);
  // Q <- rbind(L,antipods.rest[1, ])
  //     rownames(Q) <- NULL
  ellipse_parsed res;
  //record two antipods
  res.x.push_back(x[l]);
  res.y.push_back(y[l]);
  res.x.push_back(x[ind[0]]);
  res.y.push_back(y[ind[0]]);
  //record mu and a, b, alpha
  res.mu_x = mu_x;
  res.mu_y = mu_y;
  res.a = a;
  res.b = b;
  res.alpha = alpha;
  return res;
}
/**
 * translated from flowClust:::.ellipsePoints R code
 * @param res
 * @param n
 * @return
 */
matrix toPoly(ellipse_parsed res, int n){


	float a = res.a;
	float b = res.b;
	float alpha = res.alpha;


	float B = min(a,b);
	float A = max(a,b);

	float d2 = (A-B)*(A+B);

	vector<float>x1(n),y1(n);
	for(int i = 0; i <n; i++)
	{
	  float phi = 2*M_PI*i/n;

	  float sp = sin(phi);
	  float cp = cos(phi);
	  float r = a*b / sqrt(B * B + d2 * sp * sp);
	  float x = r * cp;
	  float y = r * sp;
	  // xy are the ellipse points for alpha = 0 and loc = (0,0)

	  //rotate and shift
	  float ca = cos(alpha);
	  float sa = sin(alpha);
	  x1[i] = x * ca - y *sa + res.mu_x;
	  y1[i] = x * sa + y *ca + res.mu_y;


	}

	matrix m;
	m.x = x1;
	m.y = y1;

	return m;

}
