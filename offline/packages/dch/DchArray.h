// Purpose:  An array class.
//-----------------------------------------------------------------------------
//  Copyright (C) Matthias Messer, 1996
//
//  Declaration of array classes Dch1DimArray, Dch2DimArray, Dch3DimArray
//
//  Arrays of a dimension higher than one can, as they are implemented
//  in the C(C++) - language, not be allocated dynamically AND then
//  be accessed using the [] operator.
//  The following classes have been created to solve this problem, taking advantage
//  of the fact that an array implemented as a class can always know it's
//  own dimensions.
//  Thus a Dch?DimArray can also be passed to a function by pointer (or reference)
//  and than be accessed from within the function again using the [] operator.
//
//  e.g.:
//     Dch3DimArray x(xDim, yDim, zDim);   
//     x = 0.0;                         // initialize all elements
//     x[ix][iy][iz] = blablabla;
//
//  An index of a Dch?DimArray can be any integer, positive or negative.
//
//  e.g:
//     int xMin = -10, xMax = 5, yMin = -20, yMax = -15;
//     Dch2DimArray x(xMin, xMax, yMin, yMax);
//     x[-7][-18] = blablabla;
//
//  or:
//     Dch2DimArray x;
//     int xMin = -10, xMax = 5, yMin = -20, yMax = -15;
//     x.resize(xMin, xMax, yMin, yMax);    // dynamic memory allocation
//     x[-7][-18] = blablabla;
//
//  Note that the resize() function mentioned above deletes the data
//  in the present implementation.
//  The [] operator is designed in a (somewhat) speed optimized fashion.
//  Therefor it does not perform bound-checks.
//
//-----------------------------------------------------------------------------
#ifndef __DCHARRAY_H
#define __DCHARRAY_H

template<class T> class Dch1DimArray { 
public: 
   Dch1DimArray() { collumn = 0; xMin = xMax = xDim = 0; }
   Dch1DimArray(int);
   Dch1DimArray(int, int);
   virtual ~Dch1DimArray();
   Dch1DimArray(const Dch1DimArray & a) { *this = a; }
   Dch1DimArray & operator = (const Dch1DimArray &);

public: 
   inline T & operator () (int x) { return collumn[x]; }
   inline T & operator [] (int x) { return collumn[x]; }
   Dch1DimArray & operator = (const T);
   void resize(int, int);
   
private: 
   void init_(int, int);
   
private: 
   T *collumn;
   int xDim, xMin, xMax;
}; 

template<class T> class Dch2DimArray {
public:
   Dch2DimArray() { row = 0; yMin = yMax = yDim = 0; }
   Dch2DimArray(int, int);
   Dch2DimArray(int, int, int, int);
   virtual ~Dch2DimArray();
   Dch2DimArray(const Dch2DimArray &  a) { *this = a; }
   Dch2DimArray & operator = (const Dch2DimArray &);
   
public:
   inline Dch1DimArray<T> & operator [] (int y) { return row[y]; }
   inline T & operator () (int x, int y) { return row[y][x]; }
   Dch2DimArray & operator = (const T);
   void resize(int, int, int, int);
   
private:
   void init_(int, int, int, int);
   
private:
   Dch1DimArray<T> *row;
   int yMin, yMax, yDim;
};

template<class T> class Dch3DimArray {
public:
   Dch3DimArray() { vector = 0; zMin = zMax = zDim = 0; }
   Dch3DimArray(int, int, int);
   Dch3DimArray(int, int, int, int, int, int);
   virtual ~Dch3DimArray();
   Dch3DimArray(const Dch3DimArray &  a) { *this = a; }
   Dch3DimArray & operator = (const Dch3DimArray &);
   
public:
   inline Dch2DimArray<T> & operator[] (int z) { return vector[z]; }
   inline T & operator() (int x, int y, int z) { return vector[z][y][x]; }
   Dch3DimArray & operator = (const T);
   void resize(int, int, int, int, int, int);
   
private:
   void init_(int, int, int, int, int, int);
   
private:
   Dch2DimArray<T> *vector;
   int zMin, zMax, zDim;
};

#endif /* __DCHARRAY_H */
