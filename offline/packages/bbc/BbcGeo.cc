#include "Bbc.hh"
#include "BbcGeo.hh"

BbcGeo::BbcGeo() {
  static const float X[32] = { 
    -123.0, -123.0, -98.4, -98.4, -98.4, -73.8, -73.8, -73.8, 
     -73.8,  -49.2, -49.2, -49.2, -24.6, -24.6, -24.6,   0.0,
       0.0,   24.6,  24.6,  24.6,  49.2,  49.2,  49.2,  73.8,
      73.8,   73.8,  73.8,  98.4,  98.4,  98.4, 123.0, 123.0
  };
  static const float Y[32] = { 
      42.6,  14.2,  85.2,  56.8,  28.4,  99.4,  71.0,  42.6,
      14.2, 113.6,  85.2,  56.8, 127.8,  99.4,  71.0, 113.6,
      85.2, 127.8,  99.4,  71.0, 113.6,  85.2,  56.8,  99.4,
      71.0,  42.6,  14.2,  85.2,  56.8,  28.4,  42.6,  14.2
  };
  static const float Z[32] = {
      1443.5, 1443.5, 1443.5, 1443.5, 1443.5, 1443.5, 1443.5, 1443.5,
      1443.5, 1443.5, 1443.5, 1443.5, 1443.5, 1443.5, 1443.5, 1443.5,
      1443.5, 1443.5, 1443.5, 1443.5, 1443.5, 1443.5, 1443.5, 1443.5,
      1443.5, 1443.5, 1443.5, 1443.5, 1443.5, 1443.5, 1443.5, 1443.5 
  }; 

  for (int ipmt = 0; ipmt < BBC_N_PMT; ipmt++)
    {
      if (ipmt >= 0 && ipmt < 32)
	{
	  int index = ipmt;
	  float xcord = X[index];
	  float ycord = Y[index];
	  float zcord = -Z[index];
	  PmtPosition[ipmt].setX(xcord);
	  PmtPosition[ipmt].setY(ycord);
	  PmtPosition[ipmt].setZ(zcord);
	}
      else if (ipmt >= 32 && ipmt < 64)
	{
	  int index = ipmt - 32;
	  float xcord = -X[index];
	  float ycord = -Y[index];
	  float zcord = -Z[index];
	  PmtPosition[ipmt].setX(xcord);
	  PmtPosition[ipmt].setY(ycord);
	  PmtPosition[ipmt].setZ(zcord);
	}
      else if (ipmt >= 64 && ipmt < 96)
	{
	  int index = ipmt - 64;
	  float xcord = -X[index];
	  float ycord = Y[index];
	  float zcord = Z[index];
	  PmtPosition[ipmt].setX(xcord);
	  PmtPosition[ipmt].setY(ycord);
	  PmtPosition[ipmt].setZ(zcord);
	}
      else if (ipmt >= 96 && ipmt < 128)
	{
	  int index = ipmt - 96;
	  double xcord = X[index];
	  double ycord = -Y[index];
	  double zcord = Z[index];
	  PmtPosition[ipmt].setX(xcord);
	  PmtPosition[ipmt].setY(ycord);
	  PmtPosition[ipmt].setZ(zcord);
      }
  }
}
