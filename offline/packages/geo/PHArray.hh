// Purpose:  An array class.
//-----------------------------------------------------------------------------
//  Copyright (C) Matthias Messer, 1996
//
//  Declaration of array classes PH1DimArray, PH2DimArray, PH3DimArray
//
//  Arrays of a dimension higher than one can, as they are implemented
//  in the C(C++) - language, not be allocated dynamically AND then
//  be accessed using the [] operator.
//  The following classes have been created to solve this problem, taking advantage
//  of the fact that an array implemented as a class can always know it's
//  own dimensions.
//  Thus a PH?DimArray can also be passed to a function by pointer (or reference)
//  and than be accessed from within the function again using the [] operator.
//
//  e.g.:
//     PH3DimArray x(xDim, yDim, zDim);   
//     x = 0.0;                         // initialize all elements
//     x[ix][iy][iz] = blablabla;
//
//  An index of a PH?DimArray can be any integer, positive or negative.
//
//  e.g:
//     int xMin = -10, xMax = 5, yMin = -20, yMax = -15;
//     PH2DimArray x(xMin, xMax, yMin, yMax);
//     x[-7][-18] = blablabla;
//
//  or:
//     PH2DimArray x;
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
#ifndef PHARRAY_H
#define PHARRAY_H

template<class T> class PH1DimArray { 
public: 
   PH1DimArray() { collumn = 0; xMin = xMax = xDim = 0; }
   PH1DimArray(int);
   PH1DimArray(int, int);
   ~PH1DimArray();
   PH1DimArray(const PH1DimArray & a) { *this = a; }
   PH1DimArray & operator = (const PH1DimArray &);

public: 
   inline T & operator () (int x) { return collumn[x]; }
   inline T & operator [] (int x) { return collumn[x]; }
   PH1DimArray & operator = (const T);
   void resize(int, int);
   
private: 
   void init_(int, int);
   
private: 
   T *collumn;
   int xDim, xMin, xMax;
}; 

template<class T> class PH2DimArray {
public:
   PH2DimArray() { row = 0; yMin = yMax = yDim = 0; }
   PH2DimArray(int, int);
   PH2DimArray(int, int, int, int);
   ~PH2DimArray();
   PH2DimArray(const PH2DimArray &  a) { *this = a; }
   PH2DimArray & operator = (const PH2DimArray &);
   
public:
   inline PH1DimArray<T> & operator [] (int y) { return row[y]; }
   inline T & operator () (int x, int y) { return row[y][x]; }
   PH2DimArray & operator = (const T);
   void resize(int, int, int, int);
   
private:
   void init_(int, int, int, int);
   
private:
   PH1DimArray<T> *row;
   int yMin, yMax, yDim;
};

template<class T> class PH3DimArray {
public:
   PH3DimArray() { vector = 0; zMin = zMax = zDim = 0; }
   PH3DimArray(int, int, int);
   PH3DimArray(int, int, int, int, int, int);
   ~PH3DimArray();
   PH3DimArray(const PH3DimArray &  a) { *this = a; }
   PH3DimArray & operator = (const PH3DimArray &);
   
public:
   inline PH2DimArray<T> & operator[] (int z) { return vector[z]; }
   inline T & operator() (int x, int y, int z) { return vector[z][y][x]; }
   PH3DimArray & operator = (const T);
   void resize(int, int, int, int, int, int);
   
private:
   void init_(int, int, int, int, int, int);
   
private:
   PH2DimArray<T> *vector;
   int zMin, zMax, zDim;
};

#endif /* PHARRAY_H */
