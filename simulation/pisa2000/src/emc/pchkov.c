// Returns a parameterization of Cerenkov light generation

#include "pchkov.h"

float
pchkov_(int *pi, int *pj, int *pk)
{
  int i = *pi - 1;
  int j = *pj - 1;
  int k = *pk - 1;

  return pchkov[i][j][k];
}
