#ifndef PHMATRIX_H
#define PHMATRIX_H

//---------------------------------------------------------------------------------------
// Class:  PHMatrix header
//
// Created by:  Julia Velkovska
//
// Purpose:  A 3x3 matrix
// 
//
//jfrantz 7/27/01: inverse() added
//jfrantz 9/30/01:  changed inverse() to use gsl at dave's request
//
//--------------------------------------------------------------------------

#include "PHVector.h"
#include <iosfwd>

//#include "gsl/gsl_matrix.h"
//doesn't work because rootcint can't handle gsl --jfrantz

class PHMatrix
{
public:
  PHMatrix();
 //construct from 3 row vectors  
  PHMatrix(const PHVector &,const PHVector &,const PHVector &);
  //  PHMatrix(const gsl_matrix &);
  // until gsl/rootcint conflict is resolved

  // the following constructs a PHMatrix using 3 Euler angles
  // the angles alpha1, alpha2, alpha3 are the angles of the 
  //            first , second, third  rotation
  // the permutation vector defines the axes of the three rotations
  //
  // permutation ( 3,2,3) means: 
  // the first rotation is about Z axis (axis 3) to an angle alpha1
  // the second rotation is about Y axis (axis 2) to an angle alpha2
  // the third rotation is about Z axis (axis 3) to an angla alpha3
  //
  // permutation ( 3,1,2) means: 
  // the first rotation is about Z axis (axis 3) to an angle alpha1
  // the second rotation is about X axis (axis 1) to an angle alpha2
  // the third rotation is about Y axis (axis 2) to an angla alpha3
  //  
  PHMatrix(const PHVector& permutation,const double &alpha1,
                  const double &alpha2,const double &alpha3);
  virtual ~PHMatrix() {}
  
  // matrix operations : subtraction, addition
  // matrix operations : multiplication by a scalar
  // matrix operations : multiplication by column vector: newVector=AxVector
  // matrix operations : multiplication by a matrix: newMatrix=AxB
  PHMatrix operator- (const PHMatrix &) const;
  PHMatrix operator+ (const PHMatrix &) const; 
  PHMatrix operator* (const double &) const;
  PHVector operator* (const PHVector &) const;
  PHMatrix operator* (const PHMatrix &) const;
   
  PHMatrix transpose() const;
  
  double det() const; //calculate determinant
  double trace() const;//calculate trace
  PHMatrix inverse();  //return the inverse 
  //warning assumes matrix is invertible.
  //check if .det() = 0 first

  //  gsl_matrix getGslMatrix(); // returns a gsl version of self
  // see above for  comment about no gsl

  void print() const; 

  friend std::ostream& operator<<(std::ostream& ,const PHMatrix&);
 
  void setA00(double t00);
  void setA01(double t01);
  void setA02(double t02);

  void setA10(double t10);
  void setA11(double t11);
  void setA12(double t12);

  void setA20(double t20);
  void setA21(double t21);
  void setA22(double t22);

  double getA00() const {return a00;}
  double getA01() const {return a01;}
  double getA02() const {return a02;}

  double getA10() const {return a10;}
  double getA11() const {return a11;}
  double getA12() const {return a12;}

  double getA20() const {return a20;}
  double getA21() const {return a21;}
  double getA22() const {return a22;}

  void setColumn0(PHVector &c0);
  void setColumn1(PHVector &c1);
  void setColumn2(PHVector &c2);

  void setRow0(PHVector &r0);
  void setRow1(PHVector &r1);
  void setRow2(PHVector &r2);

  PHVector getColumn0() const { return column0;}
  PHVector getColumn1() const { return column1;}
  PHVector getColumn2() const { return column2;}

  PHVector getRow0() const { return row0;}
  PHVector getRow1() const { return row1;}
  PHVector getRow2() const { return row2;}

protected:

  double a00,a01,a02,a10,a11,a12,a20,a21,a22;
  PHVector column0;
  PHVector column1;
  PHVector column2;

  PHVector row0;
  PHVector row1;
  PHVector row2;

};

#endif /* PHMATRIX_H */




