//-----------------------------------------------------------------------------
//  $Header: /afs/rhic.bnl.gov/phenix/PHENIX_CVS/offline/packages/dch/DchArray.cc,v 1.6 2000/03/29 21:46:10 johnson Exp $
//
//  COOL Program Library  
//  Copyright (C) CERES collaboration, 1996
//
//  Implementation of classes Dch1DimArray, Dch2DimArray, Dch3DimArray
//
//-----------------------------------------------------------------------------
#include "DchArray.h"     

template<class T> Dch1DimArray<T>::Dch1DimArray(int xd)
{
   init_(0, xd);
}

template<class T> Dch1DimArray<T>::Dch1DimArray(int xmn, int xmx)
{
   init_(xmn, xmx);
}

template<class T> void Dch1DimArray<T>::init_(int xmn, int xmx)
{
   xMin = xmn;
   xMax = xmx;
   xDim = xMax - xMin;
   collumn = new T[xDim] - xMin;
}

template<class T> Dch1DimArray<T>::~Dch1DimArray()
{
   collumn += xMin;
   delete [] collumn;
}

template<class T> Dch1DimArray<T> & Dch1DimArray<T>::operator = (const Dch1DimArray<T> & a)
{
   collumn = a.collumn;
   xDim = a.xDim;
   return *this;
}

template<class T> void Dch1DimArray<T>::resize(int xmn, int xmx)
{
   collumn += xMin;
   delete [] collumn;
   init_(xmn, xmx);
}

template<class T> Dch1DimArray<T> & Dch1DimArray<T>::operator = (const T value)
{
   for (int i=xMin; i<xMax; i++)
      collumn[i] = value;
   return *this;
}
   
//
// Implentation of Dch2DimArray
//

template<class T> Dch2DimArray<T>::Dch2DimArray(int y, int x)
{
   row = new Dch1DimArray<T>[y];
   yDim = y;
   for (int i=0; i<y; i++)
      row[i].resize(0, x);
}

template<class T> Dch2DimArray<T>::Dch2DimArray(int ymn, int ymx, int xmn, int xmx)
{
   init_(ymn, ymx, xmn, xmx);
}

template<class T> Dch2DimArray<T>::~Dch2DimArray()
{
   row += yMin;
   delete [] row;
}

template<class T> void Dch2DimArray<T>::init_(int ymn, int ymx, int xmn, int xmx)
{
   yMin = ymn;
   yMax = ymx;
   yDim = yMax - yMin;
   row  = new Dch1DimArray<T>[yDim] - yMin;
   for (int i=yMin; i<yMax; i++)
      row[i].resize(xmn, xmx);
}

template<class T> void Dch2DimArray<T>::resize(int ymn, int ymx, int xmn, int xmx)
{
   row += yMin;
   delete [] row;
   init_(ymn, ymx, xmn, xmx);
}

template<class T> Dch2DimArray<T> & Dch2DimArray<T>::operator = (const Dch2DimArray<T> & a)
{
   row = a.row;
   yDim = a.yDim;
   return *this;
}

template<class T> Dch2DimArray<T> & Dch2DimArray<T>::operator = (const T value)
{
   for (int i=yMin; i<yMax; i++)
      row[i] = value;
   return *this;
}

//
// Implentation of Dch3DimArray
//

template<class T> Dch3DimArray<T>::Dch3DimArray(int z, int y, int x)
{
   vector = new Dch2DimArray<T>[z];
   zDim = z;
   for (int i=0; i<z; i++)
      vector[i].resize(0, y, 0, x);
}

template<class T> Dch3DimArray<T>::Dch3DimArray(int zmn, int zmx, int ymn, int ymx, int xmn, int xmx)
{
   init_(zmn, zmx, ymn, ymx, xmn, xmx);
}

template<class T> Dch3DimArray<T>::~Dch3DimArray()
{
   vector += zMin;
   delete [] vector;
}

template<class T> void Dch3DimArray<T>::init_(int zmn, int zmx, int ymn, int ymx, int xmn, int xmx)
{
   zMin   = zmn;
   zMax   = zmx;
   zDim   = zMax - zMin;
   vector = new Dch2DimArray<T>[zDim] - zMin;
   for (int i=zMin; i<zMax; i++)
      vector[i].resize(ymn, ymx, xmn, xmx);
}

template<class T> void Dch3DimArray<T>::resize(int zmn, int zmx, int ymn, int ymx, int xmn, int xmx)
{
   vector += zMin;
   delete [] vector;
   init_(zmn, zmx, ymn, ymx, xmn, xmx);
}

template<class T> Dch3DimArray<T> & Dch3DimArray<T>::operator = (const Dch3DimArray<T> & a)
{
   vector = a.vector;
   zDim = a.zDim;
   return *this;
}

template<class T> Dch3DimArray<T> & Dch3DimArray<T>::operator = (const T value)
{
   for (int i=zMin; i<zMax; i++)
      vector[i] = value;
   return *this;
}


