#include "phool.h"

#include <cmath>
#include <cstdlib>
#include <iostream>

using namespace std;

static const int maxSize = 20;
// This code gets called a zillion times, so use statically
// allocated arrays to speed things up.
static  double c[maxSize], d[maxSize];

void
utiPolyint (double xa[], double ya[], int n, double x, double *y, double *dy)
{
  //  Given arrays xa[0,..,n-1] and ya[0,..,n-1], and given a value x,
  //  this routine returns a value y, and an error estimate dy.  If
  //  P(x) is the polynomial of degree N-1 such that P(xa[i])=ya[i], i
  //  = 0, .., n-1, then the returned value y = P(x).

  int i, m, ns = 0;
  double den, dif, dift, ho, hp, w;



  if (n >= maxSize) 
    {
      cout << PHWHERE << "utiPolyint increase maxSize from " << maxSize
	   << " to at least " << (n+1) << endl;
      exit(1);
      return;
    }
  dif = fabs (x - xa[0]);
  for (i = 0; i < n; i++)
    //  Find the index ns of the closest table entry 
    {
      if ((dift = fabs (x - xa[i])) < dif)
	{
	  ns = i;
	  dif = dift;
	}
      // Initialize the tableau of c's and d's 
      c[i] = ya[i];
      d[i] = ya[i];
    }

  // Initial approximation to y 
  *y = ya[ns--];

  // Loop over the current c's and d's for each column of the tableau, updating them 
  for (m = 0; m < (n - 1); m++)
    {
      for (i = 0; i < (n - m - 1); i++)
	{
	  ho = xa[i] - x;
	  hp = xa[i + m + 1] - x;
	  w = c[i + 1] - d[i];
	  // Error if two input xa's are equal 
	  if ((den = ho - hp) == 0.0)
	    {
	    }
	  else
	    {
	      den = w / den;
	    }
	  // Update the c's and d's 
	  d[i] = hp * den;
	  c[i] = ho * den;
	}

//        Decide which correction, c or d to add to the accumulating
//        value of y, taking the most 'straight line' route through
//        the tableau to its apex, updating ns accordingly to keep
//        track of where we are.  This route keeps the partial
//        approximation centered on the target x.  The last dy added
//        is thus the error indication.

      *y += (*dy = (2 * ns < (n - m - 1) ? c[ns + 1] : d[ns--]));
    }
}
