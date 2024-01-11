//  Purpose: Implementation file for PHDchTrackFitter class
//  Created by: Jane M. Burward-Hoy and Stephen Johnson

#include <cstdio>
#include <cstdlib>
#include <algorithm>
#include "PHDchTrackFitter.hh"
#include "utiPrototype.hh"

using namespace std;

PHDchTrackFitter *PHDchTrackFitter::_instance = 0;

const char *fieldIntegralFile = "fieldIntegral.dat";

PHDchTrackFitter *
PHDchTrackFitter::instance ()
{
  if (!_instance)
    {
      _instance = new PHDchTrackFitter;
    }
  return _instance;
}

PHDchTrackFitter::PHDchTrackFitter ()
{
  int i;
  double v;

  // Just make double-sure all these arrays are initialized.
  fill_n(initialTheta, numInitialTheta, 0.0);
  fill_n(r, numR, 0.0);
  fill_n(p, numP, 0.0);
  fill_n(z, numZ, 0.0);

  initialTheta[0] = firstInitialTheta;
  for (i = 1; i < numInitialTheta; i++)
    {
      initialTheta[i] = initialTheta[0] + i * initialThetaBin;
    }

  r[0] = initialR;
  for (i = 1; i < numR; i++)
    {
      v = r[i - 1];
      if (v < dcInsideR)
        {
          r[i] = v + rBin1;
        }
      else if (v >= dcInsideR && v < dcOutsideR)
        {
          r[i] = v + rBin2;
        }
      else
        {
          r[i] = v + rBin3;
        }
    }

  p[0] = initialP;
  for (i = 1; i < numP; i++)
    {
      p[i] = p[0] + i * pBin;
    }

  z[0] = initialZ;
  for (i = 1; i < numZ; i++)
    {
      z[i] = z[0] + i * zBin;
    }

  if (not readFieldIntegralFromFile())
    {
      std::cerr
        << "ERROR: Could not read "
        << fieldIntegralFile
        << " ... cannot continue" << std::endl;
      exit(1);
    }
}

PHBoolean
PHDchTrackFitter::readFieldIntegralFromFile ()
{
  const int COLUMNS = 3;
  float input[COLUMNS];
  int ncols;
  int numlines;
  PHBoolean error = False;

  FILE *infile;
  infile = fopen (fieldIntegralFile, "r");

  if (infile == NULL)
    {
      return False;
    }
  else
    {
      numlines = 1;
      for (int pi = 0; pi < numP; pi++)
        {
          for (int ti = 0; ti < numInitialTheta; ti++)
            {
              for (int zi = 0; zi < numZ; zi++)
                {
                  for (int ri = 0; ri < numR; ri++)
                    {
                      ncols = fscanf (infile, "%f %f %f",
                                      &input[0], &input[1], &input[2]);
                      if (ncols != COLUMNS)
                        {
                          error = True;
                        }
                      if (!error)
                        {
                          f[pi][ri][ti][zi] = input[0];
                          g[pi][ri][ti][zi] = input[1];
                          delta[pi][ri][ti][zi] = input[2];
                        }
                      numlines++;
                    }
                }
            }
        }
    }

  fclose (infile);

  if (error)
    {
      return False;
    }
  else
    {
      return True;
    }
}

void
PHDchTrackFitter::massageFieldIntegralArray()
{
  // Replace domains where -999. in the f field-integral with average
  // value from neighboring gridpoints to smooth array

  int i, j, k, l, m, q;
  double v[16];
  int n, N;
  int changed;
  double sum;

  N = 1;
  for (q = 1; q < 10; q++)
    {
      changed = 0;
      for (i = 1; i < numP - 1; i++)
        {
          for (j = 1; j < numR - 1; j++)
            {
              for (k = 1; k < numInitialTheta - 1; k++)
                {
                  for (l = 1; l < numZ - 1; l++)
                    {
                      if (f[i][j][k][l] < -100.0)
                        {
                          v[0] = f[i - 1][j - 1][k - 1][l - 1];
                          v[1] = f[i - 1][j - 1][k - 1][l + 1];
                          v[2] = f[i - 1][j - 1][k + 1][l - 1];
                          v[3] = f[i - 1][j - 1][k + 1][l + 1];
                          v[4] = f[i - 1][j + 1][k - 1][l - 1];
                          v[5] = f[i - 1][j + 1][k - 1][l + 1];
                          v[6] = f[i - 1][j + 1][k + 1][l - 1];
                          v[7] = f[i - 1][j + 1][k + 1][l + 1];
                          v[8] = f[i + 1][j - 1][k - 1][l - 1];
                          v[9] = f[i + 1][j - 1][k - 1][l + 1];
                          v[10] = f[i + 1][j - 1][k + 1][l - 1];
                          v[11] = f[i + 1][j - 1][k + 1][l + 1];
                          v[12] = f[i + 1][j + 1][k - 1][l - 1];
                          v[13] = f[i + 1][j + 1][k - 1][l + 1];
                          v[14] = f[i + 1][j + 1][k + 1][l - 1];
                          v[15] = f[i + 1][j + 1][k + 1][l + 1];

                          sum = 0.0;
                          n = 0;
                          for (m = 0; m < 16; m++)
                            {
                              if (v[m] > -100.0)
                                {
                                  sum += v[m];
                                  n++;
                                }
                            }
                          if (n >= N)
                            {
                              f[i][j][k][l] = sum / n;
                              changed++;
                            }
                        }
                    }
                }
            }
        }
    }
}

void
PHDchTrackFitter::massageDeltaFieldIntegralArray()
{
  // Replace domains where -999. in the delta field-integral with
  // average value from neighboring gridpoints to smooth array

  int i, j, k, l, m, q;
  double v[16];
  int n, N;
  int changed;
  double sum;

  N = 1;
  for (q = 1; q < 10; q++)
    {
      changed = 0;
      for (i = 1; i < numP - 1; i++)
        {
          for (j = 1; j < numR - 1; j++)
            {
              for (k = 1; k < numInitialTheta - 1; k++)
                {
                  for (l = 1; l < numZ - 1; l++)
                    {
                      if (delta[i][j][k][l] < -100.0)
                        {
                          v[0] = delta[i - 1][j - 1][k - 1][l - 1];
                          v[1] = delta[i - 1][j - 1][k - 1][l + 1];
                          v[2] = delta[i - 1][j - 1][k + 1][l - 1];
                          v[3] = delta[i - 1][j - 1][k + 1][l + 1];
                          v[4] = delta[i - 1][j + 1][k - 1][l - 1];
                          v[5] = delta[i - 1][j + 1][k - 1][l + 1];
                          v[6] = delta[i - 1][j + 1][k + 1][l - 1];
                          v[7] = delta[i - 1][j + 1][k + 1][l + 1];
                          v[8] = delta[i + 1][j - 1][k - 1][l - 1];
                          v[9] = delta[i + 1][j - 1][k - 1][l + 1];
                          v[10] = delta[i + 1][j - 1][k + 1][l - 1];
                          v[11] = delta[i + 1][j - 1][k + 1][l + 1];
                          v[12] = delta[i + 1][j + 1][k - 1][l - 1];
                          v[13] = delta[i + 1][j + 1][k - 1][l + 1];
                          v[14] = delta[i + 1][j + 1][k + 1][l - 1];
                          v[15] = delta[i + 1][j + 1][k + 1][l + 1];

                          sum = 0.0;
                          n = 0;
                          for (m = 0; m < 16; m++)
                            {
                              if (v[m] > -100.0)
                                {
                                  sum += v[m];
                                  n++;
                                }
                            }
                          if (n >= N)
                            {
                              delta[i][j][k][l] = sum / n;
                              changed++;
                            }
                        }
                    }
                }
            }
        }
    }
}

void
PHDchTrackFitter::massageGFieldIntegralArray()
{
  // Replace domains where -999. in the g field-integral with average
  // value from neighboring gridpoints to smooth array

  int i, j, k, l, m, q;
  double v[16];
  int n, N;
  int changed;
  double sum;

  N = 1;
  for (q = 1; q < 10; q++)
    {
      changed = 0;
      for (i = 1; i < numP - 1; i++)
        {
          for (j = 1; j < numR - 1; j++)
            {
              for (k = 1; k < numInitialTheta - 1; k++)
                {
                  for (l = 1; l < numZ - 1; l++)
                    {
                      if (g[i][j][k][l] < -100.0)
                        {
                          v[0] = g[i - 1][j - 1][k - 1][l - 1];
                          v[1] = g[i - 1][j - 1][k - 1][l + 1];
                          v[2] = g[i - 1][j - 1][k + 1][l - 1];
                          v[3] = g[i - 1][j - 1][k + 1][l + 1];
                          v[4] = g[i - 1][j + 1][k - 1][l - 1];
                          v[5] = g[i - 1][j + 1][k - 1][l + 1];
                          v[6] = g[i - 1][j + 1][k + 1][l - 1];
                          v[7] = g[i - 1][j + 1][k + 1][l + 1];
                          v[8] = g[i + 1][j - 1][k - 1][l - 1];
                          v[9] = g[i + 1][j - 1][k - 1][l + 1];
                          v[10] = g[i + 1][j - 1][k + 1][l - 1];
                          v[11] = g[i + 1][j - 1][k + 1][l + 1];
                          v[12] = g[i + 1][j + 1][k - 1][l - 1];
                          v[13] = g[i + 1][j + 1][k - 1][l + 1];
                          v[14] = g[i + 1][j + 1][k + 1][l - 1];
                          v[15] = g[i + 1][j + 1][k + 1][l + 1];

                          sum = 0.0;
                          n = 0;
                          for (m = 0; m < 16; m++)
                            {
                              if (v[m] > -100.0)
                                {
                                  sum += v[m];
                                  n++;
                                }
                            }
                          if (n >= N)
                            {
                              g[i][j][k][l] = sum / n;
                              changed++;
                            }
                        }
                    }
                }
            }
        }
    }
}

PHBoolean
PHDchTrackFitter::writeFieldIntegralMassagedFile(const char *filename)
{
  // Function writes-out massaged field-integral file.  This usually
  // is not called at run-time, only ahead of time to create the file
  // for the database or for soft-linking against.  This is to
  // eliminate domains where no field-integral values exist.  The
  // average of neighboring field-integral values are used.

  FILE *outfile;
  outfile = fopen(filename, "w");

  int pi, ti, ri, zi;

  if (outfile)
    {

      std::cout << "Massaging entries in field arrays. . ." << std::endl;

      massageFieldIntegralArray();
      massageDeltaFieldIntegralArray();
      massageGFieldIntegralArray();

      std::cout << "Writing new file:  fieldIntegralMassaged.dat" << std::endl;

      for (pi = 0; pi < numP; pi++)
        {
          for (ti = 0; ti < numInitialTheta; ti++)
            {
              for (zi = 0; zi < numZ; zi++)
                {
                  for (ri = 0; ri < numR; ri++)
                    {
                      fprintf(outfile, "%g %g %g\n",
                              f[pi][ri][ti][zi],
                              g[pi][ri][ti][zi],
                              delta[pi][ri][ti][zi]);
                    }
                }
            }

        }

      fclose(outfile);

    }
  else
    {
      return False;
    }

  return True;
}

// Functions below used for weighted averaging calcInterp* functions
static void
findBoundingIndexes(const double xx[], const unsigned long n,
                    const double x,
                    long &il, long &ih,
                    double &w)
{
  if (x < xx[0])
    {
      il = 0;
      ih = il;
      w = 0.0;
    }
  else if (x > xx[n-1])
    {
      il = n - 1;
      ih = il;
      w = 1.0;
    }
  else
    {
      // this construct will always return the index of the
      // "next" element (even if x is equal to a value in the array)
      // and one has to subtract one
      // using lower_bound would require a special case if
      // x is a value in the array
      unsigned long index = std::upper_bound(xx, xx + n, x) - xx;
      index--;
      il = index;
      ih = index + 1;
      if (ih < (long) n)
        {
          w = (x - xx[il]) / (xx[ih] - xx[il]);
        }
      else
        {
          il = n - 1;
          ih = il;
          w = 1.0;
        }
    }
  
  return;
}

PHBoolean
PHDchTrackFitter::calcInterpF (const double &zvtxin, const int &zOrder,
                               const double &theta0in, const int &theta0Order,
                               const double &rin, const int &rOrder,
                               const double &thetain,
                               const double &pin, const int &pOrder,
                               double &interpF)
{
  // This routine doesn't need all the "order" business; it's for
  // backward compatibility.
  return calcInterpF (zvtxin, theta0in, rin, thetain, pin, interpF);
}




PHBoolean
PHDchTrackFitter::calcInterpF (const double &zin,
                               const double &t0in,
                               const double &rin,
                               const double &tin,
                               const double &pin, double &interpF)
{
  // Perform a weighted average of the field integral map on the
  // nearest neighbor grid points surrounding the (z, t, r, p)
  // coordinates of the hit in question.  If the coordinates lie
  // outside the map in a given dimension, the value taken for that
  // dimension is the value of the edge.
  interpF = 0.0;
  errF = 0;
  int i, nValues;
  long l[4], h[4];
  double w[4];
  double weight;
  double v[16], x[16];

  // Find the bounding indexes in z, p, r, and theta
  findBoundingIndexes(p, numP, pin, l[0], h[0], w[0]);
  findBoundingIndexes(r, numR, rin, l[1], h[1], w[1]);
  findBoundingIndexes(initialTheta, numInitialTheta, t0in, l[2], h[2], w[2]);
  findBoundingIndexes(z, numZ, zin, l[3], h[3], w[3]);

  // Find the 2^4 = 16 values on the grid points around the hit and
  // average them. Use equal weights for each dimension for now; this
  // is easily generalized.  Weight a given dimension by the distance
  // to each endpoint;

  v[0] = f[l[0]][l[1]][l[2]][l[3]];
  v[1] = f[l[0]][l[1]][l[2]][h[3]];
  v[2] = f[l[0]][l[1]][h[2]][l[3]];
  v[3] = f[l[0]][l[1]][h[2]][h[3]];
  v[4] = f[l[0]][h[1]][l[2]][l[3]];
  v[5] = f[l[0]][h[1]][l[2]][h[3]];
  v[6] = f[l[0]][h[1]][h[2]][l[3]];
  v[7] = f[l[0]][h[1]][h[2]][h[3]];
  v[8] = f[h[0]][l[1]][l[2]][l[3]];
  v[9] = f[h[0]][l[1]][l[2]][h[3]];
  v[10] = f[h[0]][l[1]][h[2]][l[3]];
  v[11] = f[h[0]][l[1]][h[2]][h[3]];
  v[12] = f[h[0]][h[1]][l[2]][l[3]];
  v[13] = f[h[0]][h[1]][l[2]][h[3]];
  v[14] = f[h[0]][h[1]][h[2]][l[3]];
  v[15] = f[h[0]][h[1]][h[2]][h[3]];

  x[0] = ((1.0 - w[0]) * (1.0 - w[1]) * (1.0 - w[2]) * (1.0 - w[3]));
  x[1] = ((1.0 - w[0]) * (1.0 - w[1]) * (1.0 - w[2]) * (0.0 + w[3]));
  x[2] = ((1.0 - w[0]) * (1.0 - w[1]) * (0.0 + w[2]) * (1.0 - w[3]));
  x[3] = ((1.0 - w[0]) * (1.0 - w[1]) * (0.0 + w[2]) * (0.0 + w[3]));
  x[4] = ((1.0 - w[0]) * (0.0 + w[1]) * (1.0 - w[2]) * (1.0 - w[3]));
  x[5] = ((1.0 - w[0]) * (0.0 + w[1]) * (1.0 - w[2]) * (0.0 + w[3]));
  x[6] = ((1.0 - w[0]) * (0.0 + w[1]) * (0.0 + w[2]) * (1.0 - w[3]));
  x[7] = ((1.0 - w[0]) * (0.0 + w[1]) * (0.0 + w[2]) * (0.0 + w[3]));
  x[8] = ((0.0 + w[0]) * (1.0 - w[1]) * (1.0 - w[2]) * (1.0 - w[3]));
  x[9] = ((0.0 + w[0]) * (1.0 - w[1]) * (1.0 - w[2]) * (0.0 + w[3]));
  x[10] = ((0.0 + w[0]) * (1.0 - w[1]) * (0.0 + w[2]) * (1.0 - w[3]));
  x[11] = ((0.0 + w[0]) * (1.0 - w[1]) * (0.0 + w[2]) * (0.0 + w[3]));
  x[12] = ((0.0 + w[0]) * (0.0 + w[1]) * (1.0 - w[2]) * (1.0 - w[3]));
  x[13] = ((0.0 + w[0]) * (0.0 + w[1]) * (1.0 - w[2]) * (0.0 + w[3]));
  x[14] = ((0.0 + w[0]) * (0.0 + w[1]) * (0.0 + w[2]) * (1.0 - w[3]));
  x[15] = ((0.0 + w[0]) * (0.0 + w[1]) * (0.0 + w[2]) * (0.0 + w[3]));

  nValues = 0;
  weight = 0.0;
  interpF = 0.0;
  for (i = 0; i < 16; i++)
    {
      if (v[i] > -100.0)
        {
          nValues++;
          weight += x[i];
          interpF += v[i] * x[i];
        }
    }
  if (nValues > 4 && weight != 0.0)
    {
      interpF = interpF / weight;
      return true;
    }

  errF = errFieldBounds;
  return false;
}

PHBoolean
PHDchTrackFitter::calcInterpDelta (const double &zvtxin, const int &zOrder,
                                   const double &theta0in,
                                   const int &theta0Order, const double &rin,
                                   const int &rOrder, const double &thetain,
                                   const double &pin, const int &pOrder,
                                   double &interpDelta)
{
  // For backware compatibility only -- this routine does not need
  // polynomial orders set.
  return calcInterpDelta (zvtxin, theta0in, rin, thetain, pin, interpDelta);
}

PHBoolean
PHDchTrackFitter::calcInterpDelta(const double &zin,
                                  const double &t0in,
                                  const double &rin,
                                  const double &tin,
                                  const double &pin, double &interpDelta)
{
  // Perform a weighted average of the field integral map on the
  // nearest neighbor grid points surrounding the (z, t, r, p)
  // coordinates of the hit in question.  If the coordinates lie
  // outside the map in a given dimension, the value taken for that
  // dimension is the value of the edge.

  errDelta = 0;
  int i, nValues;
  long l[4], h[4];
  double w[4];
  double weight;
  double v[16], x[16];

  // Find the bounding indexes in z, p, r, and theta
  findBoundingIndexes(p, numP, pin, l[0], h[0], w[0]);
  findBoundingIndexes(r, numR, rin, l[1], h[1], w[1]);
  findBoundingIndexes(initialTheta, numInitialTheta, t0in, l[2], h[2], w[2]);
  findBoundingIndexes(z, numZ, zin, l[3], h[3], w[3]);

  // Find the 2^4 = 16 values on the grid points around the hit and
  // average them. Use equal weights for each dimension for now; this
  // is easily generalized.  Weight a given dimension by the distance
  // to each endpoint;

  v[0] = delta[l[0]][l[1]][l[2]][l[3]];
  v[1] = delta[l[0]][l[1]][l[2]][h[3]];
  v[2] = delta[l[0]][l[1]][h[2]][l[3]];
  v[3] = delta[l[0]][l[1]][h[2]][h[3]];
  v[4] = delta[l[0]][h[1]][l[2]][l[3]];
  v[5] = delta[l[0]][h[1]][l[2]][h[3]];
  v[6] = delta[l[0]][h[1]][h[2]][l[3]];
  v[7] = delta[l[0]][h[1]][h[2]][h[3]];
  v[8] = delta[h[0]][l[1]][l[2]][l[3]];
  v[9] = delta[h[0]][l[1]][l[2]][h[3]];
  v[10] = delta[h[0]][l[1]][h[2]][l[3]];
  v[11] = delta[h[0]][l[1]][h[2]][h[3]];
  v[12] = delta[h[0]][h[1]][l[2]][l[3]];
  v[13] = delta[h[0]][h[1]][l[2]][h[3]];
  v[14] = delta[h[0]][h[1]][h[2]][l[3]];
  v[15] = delta[h[0]][h[1]][h[2]][h[3]];


  x[0] = ((1.0 - w[0]) * (1.0 - w[1]) * (1.0 - w[2]) * (1.0 - w[3]));
  x[1] = ((1.0 - w[0]) * (1.0 - w[1]) * (1.0 - w[2]) * (0.0 + w[3]));
  x[2] = ((1.0 - w[0]) * (1.0 - w[1]) * (0.0 + w[2]) * (1.0 - w[3]));
  x[3] = ((1.0 - w[0]) * (1.0 - w[1]) * (0.0 + w[2]) * (0.0 + w[3]));
  x[4] = ((1.0 - w[0]) * (0.0 + w[1]) * (1.0 - w[2]) * (1.0 - w[3]));
  x[5] = ((1.0 - w[0]) * (0.0 + w[1]) * (1.0 - w[2]) * (0.0 + w[3]));
  x[6] = ((1.0 - w[0]) * (0.0 + w[1]) * (0.0 + w[2]) * (1.0 - w[3]));
  x[7] = ((1.0 - w[0]) * (0.0 + w[1]) * (0.0 + w[2]) * (0.0 + w[3]));
  x[8] = ((0.0 + w[0]) * (1.0 - w[1]) * (1.0 - w[2]) * (1.0 - w[3]));
  x[9] = ((0.0 + w[0]) * (1.0 - w[1]) * (1.0 - w[2]) * (0.0 + w[3]));
  x[10] = ((0.0 + w[0]) * (1.0 - w[1]) * (0.0 + w[2]) * (1.0 - w[3]));
  x[11] = ((0.0 + w[0]) * (1.0 - w[1]) * (0.0 + w[2]) * (0.0 + w[3]));
  x[12] = ((0.0 + w[0]) * (0.0 + w[1]) * (1.0 - w[2]) * (1.0 - w[3]));
  x[13] = ((0.0 + w[0]) * (0.0 + w[1]) * (1.0 - w[2]) * (0.0 + w[3]));
  x[14] = ((0.0 + w[0]) * (0.0 + w[1]) * (0.0 + w[2]) * (1.0 - w[3]));
  x[15] = ((0.0 + w[0]) * (0.0 + w[1]) * (0.0 + w[2]) * (0.0 + w[3]));

  nValues = 0;
  weight = 0.0;
  interpDelta = 0.0;
  for (i = 0; i < 16; i++)
    {
      if (v[i] > -100.0)
        {
          nValues++;
          weight += x[i];
          interpDelta += v[i] * x[i];
        }
    }
  if (nValues > 4 && weight != 0.0)
    {
      interpDelta = interpDelta / weight;
      return true;
    }

  errDelta = errFieldBounds;

  return false;
}

PHBoolean
PHDchTrackFitter::calcInterpG (const double &zvtxin, const int &zOrder,
                               const double &theta0in, const int &theta0Order,
                               const double &rin, const int &rOrder,
                               const double &thetain,
                               const double &pin, const int &pOrder,
                               double &interpG)
{
  // For backward-compatibility only --> function does not require
  // polynomial orders set.
  return calcInterpG (zvtxin, theta0in, rin, thetain, pin, interpG);
}

PHBoolean
PHDchTrackFitter::calcInterpG (const double &zin,
                               const double &t0in,
                               const double &rin,
                               const double &tin,
                               const double &pin,
                               double &interpG)
{
  // Perform a weighted average of the field integral map on the
  // nearest neighbor grid points surrounding the (z, t, r, p)
  // coordinates of the hit in question.  If the coordinates lie
  // outside the map in a given dimension, the value taken for that
  // dimension is the value of the edge.
  errG = 0;
  int i, nValues;
  long l[4], h[4];
  double w[4];
  double weight;
  double v[16], x[16];

  // Find the bounding indexes in z, p, r, and theta
  findBoundingIndexes(p, numP, pin, l[0], h[0], w[0]);
  findBoundingIndexes(r, numR, rin, l[1], h[1], w[1]);
  findBoundingIndexes(initialTheta, numInitialTheta, t0in, l[2], h[2], w[2]);
  findBoundingIndexes(z, numZ, zin, l[3], h[3], w[3]);

  // Find the 2^4 = 16 values on the grid points around the hit and
  // average them. Use equal weights for each dimension for now; this
  // is easily generalized.  Weight a given dimension by the distance
  // to each endpoint;
  v[0] = g[l[0]][l[1]][l[2]][l[3]];
  v[1] = g[l[0]][l[1]][l[2]][h[3]];
  v[2] = g[l[0]][l[1]][h[2]][l[3]];
  v[3] = g[l[0]][l[1]][h[2]][h[3]];
  v[4] = g[l[0]][h[1]][l[2]][l[3]];
  v[5] = g[l[0]][h[1]][l[2]][h[3]];
  v[6] = g[l[0]][h[1]][h[2]][l[3]];
  v[7] = g[l[0]][h[1]][h[2]][h[3]];
  v[8] = g[h[0]][l[1]][l[2]][l[3]];
  v[9] = g[h[0]][l[1]][l[2]][h[3]];
  v[10] = g[h[0]][l[1]][h[2]][l[3]];
  v[11] = g[h[0]][l[1]][h[2]][h[3]];
  v[12] = g[h[0]][h[1]][l[2]][l[3]];
  v[13] = g[h[0]][h[1]][l[2]][h[3]];
  v[14] = g[h[0]][h[1]][h[2]][l[3]];
  v[15] = g[h[0]][h[1]][h[2]][h[3]];

  x[0] = ((1.0 - w[0]) * (1.0 - w[1]) * (1.0 - w[2]) * (1.0 - w[3]));
  x[1] = ((1.0 - w[0]) * (1.0 - w[1]) * (1.0 - w[2]) * (0.0 + w[3]));
  x[2] = ((1.0 - w[0]) * (1.0 - w[1]) * (0.0 + w[2]) * (1.0 - w[3]));
  x[3] = ((1.0 - w[0]) * (1.0 - w[1]) * (0.0 + w[2]) * (0.0 + w[3]));
  x[4] = ((1.0 - w[0]) * (0.0 + w[1]) * (1.0 - w[2]) * (1.0 - w[3]));
  x[5] = ((1.0 - w[0]) * (0.0 + w[1]) * (1.0 - w[2]) * (0.0 + w[3]));
  x[6] = ((1.0 - w[0]) * (0.0 + w[1]) * (0.0 + w[2]) * (1.0 - w[3]));
  x[7] = ((1.0 - w[0]) * (0.0 + w[1]) * (0.0 + w[2]) * (0.0 + w[3]));
  x[8] = ((0.0 + w[0]) * (1.0 - w[1]) * (1.0 - w[2]) * (1.0 - w[3]));
  x[9] = ((0.0 + w[0]) * (1.0 - w[1]) * (1.0 - w[2]) * (0.0 + w[3]));
  x[10] = ((0.0 + w[0]) * (1.0 - w[1]) * (0.0 + w[2]) * (1.0 - w[3]));
  x[11] = ((0.0 + w[0]) * (1.0 - w[1]) * (0.0 + w[2]) * (0.0 + w[3]));
  x[12] = ((0.0 + w[0]) * (0.0 + w[1]) * (1.0 - w[2]) * (1.0 - w[3]));
  x[13] = ((0.0 + w[0]) * (0.0 + w[1]) * (1.0 - w[2]) * (0.0 + w[3]));
  x[14] = ((0.0 + w[0]) * (0.0 + w[1]) * (0.0 + w[2]) * (1.0 - w[3]));
  x[15] = ((0.0 + w[0]) * (0.0 + w[1]) * (0.0 + w[2]) * (0.0 + w[3]));

  nValues = 0;
  weight = 0.0;
  interpG = 0.0;
  for (i = 0; i < 16; i++)
    {
      if (v[i] > -100.0)
        {
          nValues++;
          weight += x[i];
          interpG += v[i] * x[i];
        }
    }
  if (nValues > 4 && weight != 0.0)
    {
      interpG = interpG / weight;
      return true;
    }

  errG = errFieldBounds;
  return false;

}

// Polynomial Interpolation Functions (to be phased out).
PHBoolean
PHDchTrackFitter::setIndices (const double &trkMomentum, const
                              double &trkZvertex, const double
                              &trkTheta, const double &trkInitTheta)
{
  // Used for polynomial interpoloation.  Given a momentum, zvertex,
  // theta, and initial theta, determines and sets the member variable
  // indices for each dimension.
  int stop = 0;
  int jumpsUp = 0;
  int jumpsDown = 0;

  if (trkMomentum >= maximumP)
    {
      // p is outside our field integral lookup table, use the
      // greatest p element in table DO NOT INTERPOLATE because of
      // instability (!= interpolation but extrapolation)
      startP = numP - 1;
      stopP = numP - 1;
    }
  else if (trkMomentum <= initialP)
    {
      startP = 0;
      stopP = 0;
    }
  else
    {
      startP = getStartIndex (p, numP, trkMomentum, polyOrderP);
      stopP = startP + polyOrderP - 1;
    }

  if (trkZvertex >= maximumZ)
    {
      startZ = numZ - 1;
      stopZ = numZ - 1;
    }
  else if (trkZvertex <= initialZ)
    {
      startZ = 0;
      stopZ = 0;
    }
  else
    {
      startZ = getStartIndex (z, numZ, trkZvertex, (int) polyOrderZ);
      stopZ = startZ + polyOrderZ - 1;
    }

  if (trkInitTheta >= lastInitialTheta)
    {
      startInitialTheta = numInitialTheta - 1;
      stopInitialTheta = numInitialTheta - 1;
    }
  else if (trkInitTheta <= firstInitialTheta)
    {
      startInitialTheta = 0;
      stopInitialTheta = 0;
    }
  else
    {
      startInitialTheta =
        getStartIndex (initialTheta, numInitialTheta, trkInitTheta,
                       polyOrderInitialTheta);
      stopInitialTheta = startInitialTheta + polyOrderInitialTheta - 1;
    }

  while (!stop)
    {

      if (startP < 0 || stopInitialTheta < 0 || startZ < 0)
        {
          return false;
        }
      if (startP >= numP || stopInitialTheta >= numInitialTheta
          || startZ >= numZ)
        {
          return false;
        }

      double temp1 = f[startP][0][stopInitialTheta][startZ];

      if (startP < 0 || startInitialTheta < 0 || stopZ < 0)
        {
          return false;
        }
      if (startP >= numP
          || startInitialTheta >= numInitialTheta
          || stopZ >= numZ)
        {
          return false;
        }

      double temp2 = f[startP][0][startInitialTheta][stopZ];

      if (temp1 == -999.0)
        {
          stopInitialTheta = stopInitialTheta - 1;
          startZ = startZ + 1;
          jumpsDown++;
        }
      else if (temp2 == -999.0)
        {
          startInitialTheta = startInitialTheta + 1;
          stopZ = stopZ - 1;
          jumpsUp++;
        }
      else
        {
          stop = 1;
        }
    }

  if (jumpsUp > 0)
    {
      stopInitialTheta = stopInitialTheta + jumpsUp;
      startZ = startZ - jumpsUp;
    }
  if (jumpsDown > 0)
    {
      startInitialTheta = startInitialTheta - jumpsDown;
      stopZ = stopZ + jumpsDown;
    }

  if (f[startP][0][stopInitialTheta][startZ] == -999.0 ||
      f[startP][0][startInitialTheta][stopZ] == -999.0)
    {
      return False;
    }

  return True;
}

int
PHDchTrackFitter::getStartIndex (const double array[], const int &n,
                                 const double &val,
                                 const int &polyOrder) const
{
  if (val < array[0])
    {
      return 0;
    }
  // this construct will always return the index of the
  // "next" element (even if x is equal to a value in the array)
  // and one has to subtract one
  // using lower_bound would require a special case if
  // x is a value in the array
  int index = std::upper_bound(array, array + n, val) - array;
  index --;
  int start = index - polyOrder / 2 + 1;

  if (start < 0)
    {
      // val is to the left of the array
      start = 0;
    }
  else if (start >= (n - polyOrder - 1))
    {
      // val is to the right of the array
      start = n - polyOrder - 1;
    }

   return start;
 }

void
PHDchTrackFitter::setPolyOrders (const int &rOrder, const int &pOrder,
                                 const int &zOrder, const int &theta0Order)
{
  polyOrderP = pOrder;
  polyOrderZ = zOrder;
  polyOrderInitialTheta = theta0Order;
  polyOrderR = rOrder;
}


PHBoolean
PHDchTrackFitter::calcPolyInterpG (const double &zvtxin, const int &zOrder,
                                   const double &theta0in, const int &theta0Order,
                                   const double &rin, const int &rOrder,
                                   const double &thetain,
                                   const double &pin, const int &pOrder,
                                   double &interpG)
{
  setPolyOrders (rOrder, pOrder, zOrder, theta0Order);
  return calcPolyInterpG (zvtxin, theta0in, rin, thetain, pin, interpG);
}

PHBoolean
PHDchTrackFitter::calcPolyInterpG (const double &zvtxin,
                                   const double &theta0in,
                                   const double &rin,
                                   const double &thetain,
                                   const double &pin, double &interpG)
{
  PHBoolean interpOkay = True;

  // Interpolation in three dimensions:  momentum, zed (theta), zvertex
  double zvertex = zvtxin;
  double trkTheta = thetain;
  double theta0Guess = theta0in;
  double pGuess = pin;
  double thisR = rin;

  double gTheta[numInitialTheta];
  double gInterpTheta[numZ], gInterpThetaErr[numZ];
  double gInterpP[numP], gInterpPErr[numP];
  double gInterp[numR], gInterpErr[numR];

  double err;

  PHBoolean inBounds = setIndices (pGuess, zvertex, trkTheta, theta0Guess);

  if (inBounds)
    {
      for (int ri = 0; ri < numR; ri++)
        {
          for (int pi = startP; pi <= stopP; pi++)
            {
              for (int zi = startZ; zi <= stopZ; zi++)
                {
                  for (int ti = startInitialTheta; ti <= stopInitialTheta;
                       ti++)
                    {
                      gTheta[ti] = g[pi][ri][ti][zi];
                    }
                  utiPolyint (&initialTheta[startInitialTheta],
                              &gTheta[startInitialTheta],
                              polyOrderInitialTheta, theta0Guess,
                              &gInterpTheta[zi], &gInterpThetaErr[zi]);
                }

              gInterpP[pi] = gInterpTheta[startZ];
              if (startZ < stopZ)
                {
                  utiPolyint (&z[startZ], &gInterpTheta[startZ],
                              polyOrderZ, zvertex,
                              &gInterpP[pi], &gInterpPErr[pi]);
                }
            }

          gInterp[ri] = gInterpP[startP];
          if (startP < stopP)
            {
              utiPolyint (&p[startP], &gInterpP[startP],
                          polyOrderP, pGuess, &gInterp[ri], &gInterpErr[ri]);
            }
        }

      int startR = getStartIndex (r, numR, thisR, (int) polyOrderR);
      utiPolyint (&r[startR], &gInterp[startR],
                  polyOrderR, thisR, &interpG, &err);

    }
  else
    {
      //theta out of bounds
      errG = errThetaBounds;
      interpOkay = False;
    }

  return interpOkay;

}

PHBoolean
PHDchTrackFitter::calcPolyInterpF (const double &zvtxin, const int &zOrder,
                                   const double &theta0in, const int &theta0Order,
                                   const double &rin, const int &rOrder,
                                   const double &thetain,
                                   const double &pin, const int &pOrder,
                                   double &interpF)
{
  // This routine _should_ be superfluous - but you never know.
  setPolyOrders (rOrder, pOrder, zOrder, theta0Order);
  return calcPolyInterpF (zvtxin, theta0in, rin, thetain, pin, interpF);
}

PHBoolean
PHDchTrackFitter::calcPolyInterpF (const double &zvtxin,
                                   const double &theta0in,
                                   const double &rin,
                                   const double &thetain,
                                   const double &pin, double &interpF)
{
  PHBoolean interpOkay = True;

  double zvertex = zvtxin;
  double trkTheta = thetain;
  double theta0Guess = theta0in;
  double pGuess = pin;
  double thisR = rin;

  double fofZ[numZ];
  double fofPInitialTheta[numP][numInitialTheta];
  double fofPInitialThetaErr[numP][numInitialTheta];
  double fofP[numInitialTheta];
  double fofPinterp[numP], fofPerr[numP];
  double fofRinterp[numR], fofRerr[numR];
  double err;

  int ri, pi, ti, zi;

  // Get starting and stopping indices in z, p, and theta0
  PHBoolean inBounds = setIndices (pGuess, zvertex, trkTheta, theta0Guess);

  if (inBounds)
    {
      // Interpolate in z, p, and theta
      for (ri = 0; ri < numR; ri++)
        {
          for (pi = startP; pi <= stopP; pi++)
            {
              for (ti = startInitialTheta; ti <= stopInitialTheta; ti++)
                {
                  for (zi = startZ; zi <= stopZ; zi++)
                    {
                      fofZ[zi] = f[pi][ri][ti][zi];
                    }

                  fofPInitialTheta[pi][ti] = fofZ[startZ];
                  if (startZ < stopZ)
                    {
                      utiPolyint (&z[startZ], &fofZ[startZ],
                                  polyOrderZ, zvertex,
                                  &fofPInitialTheta[pi][ti],
                                  &fofPInitialThetaErr[pi][ti]);
                    }
                }
            }

          for (pi = startP; pi <= stopP; pi++)
            {
              for (ti = startInitialTheta; ti <= stopInitialTheta; ti++)
                {
                  fofP[ti] = fofPInitialTheta[pi][ti];
                }		// build fofP array for given theta0
              utiPolyint (&initialTheta[startInitialTheta],
                          &fofP[startInitialTheta], polyOrderInitialTheta,
                          theta0Guess, &fofPinterp[pi], &fofPerr[pi]);


            }			// build fofPinterp array

          fofRinterp[ri] = fofPinterp[startP];
          if (startP < stopP)
            {
              utiPolyint (&p[startP], &fofPinterp[startP],
                          polyOrderP, pGuess, &fofRinterp[ri], &fofRerr[ri]);
            }

        }			// build fofRinterp array

      int startR = getStartIndex (r, numR, thisR, (int) polyOrderR);
      utiPolyint (&r[startR], &fofRinterp[startR],
                  polyOrderR, thisR, &interpF, &err);
      if (fabs (interpF) > 1.0)
        {
          interpOkay = False;
          errF = errFieldBounds;
        }
    }
  else
    {
      errF = errThetaBounds;
      interpOkay = False;
    }

  return interpOkay;
}

PHBoolean
PHDchTrackFitter::calcPolyInterpDelta (const double &zvtxin, const int &zOrder,
                                       const double &theta0in,
                                       const int &theta0Order, const double &rin,
                                       const int &rOrder, const double &thetain,
                                       const double &pin, const int &pOrder,
                                       double &interpDelta)
{
  setPolyOrders (rOrder, pOrder, zOrder, theta0Order);
  return calcPolyInterpDelta (zvtxin, theta0in, rin,
                              thetain, pin, interpDelta);
}


PHBoolean
PHDchTrackFitter::calcPolyInterpDelta (const double &zvtxin,
                                       const double &theta0in,
                                       const double &rin,
                                       const double &thetain,
                                       const double &pin, double &interpDelta)
{
  PHBoolean interpOkay = True;

  double zvertex = zvtxin;
  double trkTheta = thetain;
  double theta0Guess = theta0in;
  double pGuess = pin;
  double thisR = rin;

  // Interpolation in three dimensions:  momentum, zed (theta), zvertex
  double deltaTheta[numInitialTheta];
  double deltaInterpTheta[numZ], deltaInterpThetaErr[numZ];
  double deltaInterpP[numP], deltaInterpPErr[numP];
  double deltaInterpR[numR], deltaInterpRErr[numR];
  double err;

  PHBoolean inBounds = setIndices (pGuess, zvertex, trkTheta, theta0Guess);

  if (inBounds)
    {
      for (int ri = 0; ri < numR; ri++)
        {
          for (int pi = startP; pi <= stopP; pi++)
            {
              for (int zi = startZ; zi <= stopZ; zi++)
                {
                  for (int ti = startInitialTheta; ti <= stopInitialTheta;
                       ti++)
                    {
                      deltaTheta[ti] = delta[pi][ri][ti][zi];
                    }
                  utiPolyint (&initialTheta[startInitialTheta],
                              &deltaTheta[startInitialTheta],
                              polyOrderInitialTheta, theta0Guess,
                              &deltaInterpTheta[zi],
                              &deltaInterpThetaErr[zi]);
                }
              deltaInterpP[pi] = deltaInterpTheta[startZ];
              if (startZ < stopZ)
                {
                  utiPolyint (&z[startZ], &deltaInterpTheta[startZ],
                              polyOrderZ, zvertex,
                              &deltaInterpP[pi], &deltaInterpPErr[pi]);
                }

            }

          deltaInterpR[ri] = deltaInterpP[startP];
          if (startP < stopP)
            {
              utiPolyint (&p[startP], &deltaInterpP[startP],
                          polyOrderP, pGuess,
                          &deltaInterpR[ri], &deltaInterpRErr[ri]);
            }

        }
      int startR = getStartIndex (r, numR, thisR, (int) polyOrderR);
      utiPolyint (&r[startR], &deltaInterpR[startR],
                  polyOrderR, thisR, &interpDelta, &err);

    }
  else
    {
      //theta out of bounds
      errDelta = errThetaBounds;
      interpOkay = False;
    }

  return interpOkay;
}
