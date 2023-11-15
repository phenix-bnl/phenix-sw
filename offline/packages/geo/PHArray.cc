//-----------------------------------------------------------------------------
//  $Header: /afs/rhic.bnl.gov/phenix/PHENIX_CVS/offline/packages/geo/PHArray.cc,v 1.1 2001/08/22 18:02:51 federica Exp $
//
//  COOL Program Library  
//  Copyright (C) CERES collaboration, 1996
//
//  Implementation of classes PH1DimArray, PH2DimArray, PH3DimArray
//
//-----------------------------------------------------------------------------
#include "PHArray.hh"     

template<class T> PH1DimArray<T>::PH1DimArray(int xd)
{
   init_(0, xd);
}

template<class T> PH1DimArray<T>::PH1DimArray(int xmn, int xmx)
{
   init_(xmn, xmx);
}

template<class T> void PH1DimArray<T>::init_(int xmn, int xmx)
{
   xMin = xmn;
   xMax = xmx;
   xDim = xMax - xMin;
   collumn = new T[xDim] - xMin;
}

template<class T> PH1DimArray<T>::~PH1DimArray()
{
   collumn += xMin;
   delete [] collumn;
}

template<class T> PH1DimArray<T> & PH1DimArray<T>::operator = (const PH1DimArray<T> & a)
{
   collumn = a.collumn;
   xDim = a.xDim;
   return *this;
}

template<class T> void PH1DimArray<T>::resize(int xmn, int xmx)
{
   collumn += xMin;
   delete [] collumn;
   init_(xmn, xmx);
}

template<class T> PH1DimArray<T> & PH1DimArray<T>::operator = (const T value)
{
   for (int i=xMin; i<xMax; i++)
      collumn[i] = value;
   return *this;
}
   
//
// Implentation of PH2DimArray
//

template<class T> PH2DimArray<T>::PH2DimArray(int y, int x)
{
   row = new PH1DimArray<T>[y];
   yDim = y;
   for (int i=0; i<y; i++)
      row[i].resize(0, x);
}

template<class T> PH2DimArray<T>::PH2DimArray(int ymn, int ymx, int xmn, int xmx)
{
   init_(ymn, ymx, xmn, xmx);
}

template<class T> PH2DimArray<T>::~PH2DimArray()
{
   row += yMin;
   delete [] row;
}

template<class T> void PH2DimArray<T>::init_(int ymn, int ymx, int xmn, int xmx)
{
   yMin = ymn;
   yMax = ymx;
   yDim = yMax - yMin;
   row  = new PH1DimArray<T>[yDim] - yMin;
   for (int i=yMin; i<yMax; i++)
      row[i].resize(xmn, xmx);
}

template<class T> void PH2DimArray<T>::resize(int ymn, int ymx, int xmn, int xmx)
{
   row += yMin;
   delete [] row;
   init_(ymn, ymx, xmn, xmx);
}

template<class T> PH2DimArray<T> & PH2DimArray<T>::operator = (const PH2DimArray<T> & a)
{
   row = a.row;
   yDim = a.yDim;
   return *this;
}

template<class T> PH2DimArray<T> & PH2DimArray<T>::operator = (const T value)
{
   for (int i=yMin; i<yMax; i++)
      row[i] = value;
   return *this;
}

//
// Implentation of PH3DimArray
//

template<class T> PH3DimArray<T>::PH3DimArray(int z, int y, int x)
{
   vector = new PH2DimArray<T>[z];
   zDim = z;
   for (int i=0; i<z; i++)
      vector[i].resize(0, y, 0, x);
}

template<class T> PH3DimArray<T>::PH3DimArray(int zmn, int zmx, int ymn, int ymx, int xmn, int xmx)
{
   init_(zmn, zmx, ymn, ymx, xmn, xmx);
}

template<class T> PH3DimArray<T>::~PH3DimArray()
{
   vector += zMin;
   delete [] vector;
}

template<class T> void PH3DimArray<T>::init_(int zmn, int zmx, int ymn, int ymx, int xmn, int xmx)
{
   zMin   = zmn;
   zMax   = zmx;
   zDim   = zMax - zMin;
   vector = new PH2DimArray<T>[zDim] - zMin;
   for (int i=zMin; i<zMax; i++)
      vector[i].resize(ymn, ymx, xmn, xmx);
}

template<class T> void PH3DimArray<T>::resize(int zmn, int zmx, int ymn, int ymx, int xmn, int xmx)
{
   vector += zMin;
   delete [] vector;
   init_(zmn, zmx, ymn, ymx, xmn, xmx);
}

template<class T> PH3DimArray<T> & PH3DimArray<T>::operator = (const PH3DimArray<T> & a)
{
   vector = a.vector;
   zDim = a.zDim;  
   return *this;
}

template<class T> PH3DimArray<T> & PH3DimArray<T>::operator = (const T value)
{
   for (int i=zMin; i<zMax; i++)
      vector[i] = value;
   return *this;
}


