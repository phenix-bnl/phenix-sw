#include "Tools.h"
#include <cmath>
#include <gsl/gsl_linalg.h>

//_________________________________________________
void Tools::distcls(float x1[3],    float x2[3],
	     float tanx1[2], float tanx2[2],
	     float x1c[3],   float x2c[3],
	     int ifail)
// This routine calculates the postions on two lines between
// which the distance is the shortest
// inputs:
//   x1[3] and x2[3] are the coordinates on any point on line 1
//   and 2, respectively. tanx1[2] and tanx2[2] are the tanx and
//   tany for the two lines
// outputs:
//   x1c[3] and x2c[3] are the points on the two lines for which
//   the distance is the shortest
//   ifail is nonzero if the matrix inversion failed
{
  double b[6], c[6];
  int i, j, s;

  gsl_permutation *p = gsl_permutation_alloc(6);
  gsl_matrix *e = gsl_matrix_calloc(6, 6);
  gsl_matrix *n = gsl_matrix_alloc(6, 6);

  gsl_matrix_set(e, 0, 0, tanx1[0]);
  gsl_matrix_set(e, 1, 0, tanx2[0]);
  gsl_matrix_set(e, 3, 0, 1);
  gsl_matrix_set(e, 0, 1, -gsl_matrix_get(e, 0, 0));
  gsl_matrix_set(e, 1, 1, -gsl_matrix_get(e, 1, 0));
  gsl_matrix_set(e, 5, 1, 1);
  gsl_matrix_set(e, 0, 2, tanx1[1]);
  gsl_matrix_set(e, 1, 2, tanx2[1]);
  gsl_matrix_set(e, 2, 2, 1);
  gsl_matrix_set(e, 0, 3, -gsl_matrix_get(e, 0, 2));
  gsl_matrix_set(e, 1, 3, -gsl_matrix_get(e, 1, 2));
  gsl_matrix_set(e, 4, 3, 1);
  gsl_matrix_set(e, 0, 4, 1);
  gsl_matrix_set(e, 1, 4, 1);
  gsl_matrix_set(e, 2, 4, -gsl_matrix_get(e, 0, 2));
  gsl_matrix_set(e, 3, 4, -gsl_matrix_get(e, 0, 0));
  gsl_matrix_set(e, 0, 5, -1);
  gsl_matrix_set(e, 1, 5, -1);
  gsl_matrix_set(e, 4, 5, -gsl_matrix_get(e, 1, 2));
  gsl_matrix_set(e, 5, 5, -gsl_matrix_get(e, 1, 0));

  if (gsl_linalg_LU_decomp(e, p, &s) == GSL_SUCCESS &&
      gsl_linalg_LU_invert(e, p, n) == GSL_SUCCESS)
    {
      b[0] = 0;
      b[1] = 0;
      b[2] = x1[1] - tanx1[1] * x1[2];
      b[3] = x1[0] - tanx1[0] * x1[2];
      b[4] = x2[1] - tanx2[1] * x2[2];
      b[5] = x2[0] - tanx2[0] * x2[2];
      for (i = 0; i < 6; i++)
	{
	  c[i] = 0;
	  for (j = 0; j < 6; j++)
	    {
	      c[i] = c[i] + gsl_matrix_get(n, i, j) * b[j];
	    }
	}
      x1c[0] = c[0];
      x2c[0] = c[1];
      x1c[1] = c[2];
      x2c[1] = c[3];
      x1c[2] = c[4];
      x2c[2] = c[5];
    }

  gsl_matrix_free(e);
  gsl_matrix_free(n);
  gsl_permutation_free(p);

  return;
}
