// Class:  PHMatrix source
// Created by: Julia Velkovska

//jfrantz 7/27/01: invert() added

//INCLUDECHECKER: Removed this line: #include "PHMatrix.h"
#include "PHGeometry.h"
#include "gsl/gsl_matrix_double.h"
#include "gsl/gsl_linalg.h"
#include "gsl/gsl_permutation.h"
#include "gsl/gsl_machine.h"

#include <iostream>
using namespace std;

PHMatrix::PHMatrix()
{
  a00 = 1.;
  a01 = 0.;
  a02 = 0.;

  a10 = 0.;
  a11 = 1.;
  a12 = 0.;

  a20 = 0.;
  a21 = 0.;
  a22 = 1.;

  PHVector rc0(1,0,0);
  PHVector rc1(0,1,0);
  PHVector rc2(0,0,1);
  column0=rc0;
  column1=rc1;
  column2=rc2;
  row0=rc0;
  row1=rc1;
  row2=rc2;

}
 
PHMatrix::PHMatrix(const PHVector & a0, const PHVector &a1,const PHVector &a2 )
{
  a00=a0.getX();
  a01=a0.getY();
  a02=a0.getZ();

  a10=a1.getX();
  a11=a1.getY();
  a12=a1.getZ();

  a20=a2.getX();
  a21=a2.getY();
  a22=a2.getZ();

  row0=a0;
  row1=a1;
  row2=a2;

  PHVector c0(a0.getX(),a1.getX(),a2.getX());
  PHVector c1(a0.getY(),a1.getY(),a2.getY());
  PHVector c2(a0.getZ(),a1.getZ(),a2.getZ());

  column0=c0;
  column1=c1;
  column2=c2;

}
 
PHMatrix::PHMatrix(const PHVector & permutation,const double &alpha1,const double &alpha2,const double &alpha3)
{
  PHMatrix A1;
  PHMatrix A2;
  PHMatrix A3;
  PHVector Xaxis(1,0,0);
  PHVector Yaxis(0,1,0);
  PHVector Zaxis(0,0,1);
  int Rotation1  = int(permutation.getX());
  int Rotation2  = int(permutation.getY());
  int Rotation3  = int(permutation.getZ());
  PHVector axis1=Xaxis;
  PHVector axis2=Yaxis;
  PHVector axis3=Zaxis;

  switch (Rotation1)
    {
    case 0: // no rotation 
      break;  
    case 1: // the first rotation is about X 
      axis1=Xaxis;
      break;
    case 2: // the first rotation is about Y
      axis1=Yaxis;
      break;
    case 3: // the first rotation is about Z
      axis1=Zaxis;
      break;
    }

  switch (Rotation2)
    {
    case 0: // no rotation 
      break;  
    case 1: // the second rotation is about X 
      axis2=Xaxis;
      break;
    case 2: // the second rotation is about Y
      axis2=Yaxis;
      break;
    case 3: // the second rotation is about Z
      axis2=Zaxis;
      break;
    }

  switch (Rotation3)
    {
    case 0: // no rotation 
      break;  
    case 1: // the third rotation is about X 
      axis3=Xaxis;
      break;
    case 2: // the third rotation is about Y
      axis3=Yaxis;
      break;
    case 3: // the third rotation is about Z
      axis3=Zaxis;
      break;
    }
  A1=PHGeometry::rotationMatrix(alpha1,axis1);
  A2=PHGeometry::rotationMatrix(alpha2,axis2);
  A3=PHGeometry::rotationMatrix(alpha3,axis3);
  PHMatrix temp=A2*A1;
  PHMatrix out=A3*temp;
  a00=(out.a00);
  a01=(out.a01);
  a02=(out.a02);

  a10=(out.a10);
  a11=(out.a11);
  a12=(out.a12);

  a20=(out.a20);
  a21=(out.a21);
  a22=(out.a22);

  column0=(out.column0);
  column1=(out.column1);
  column2=(out.column2);

  row0=(out.row0);
  row1=(out.row1);
  row2=(out.row2);
}

/*
//create from a gsl matrix
PHMatrix::PHMatrix(const gsl_matrix& inMatrix)
{
  a00 = gsl_matrix_get(&inMatrix,0,0);
  a01 = gsl_matrix_get(&inMatrix,0,1);
  a02 = gsl_matrix_get(&inMatrix,0,2);
  a10 = gsl_matrix_get(&inMatrix,1,0);
  a11 = gsl_matrix_get(&inMatrix,1,1);
  a12 = gsl_matrix_get(&inMatrix,1,2);
  a20 = gsl_matrix_get(&inMatrix,2,0);
  a21 = gsl_matrix_get(&inMatrix,2,1);
  a22 = gsl_matrix_get(&inMatrix,2,2);
}
*/

void PHMatrix::setA00(double t00) 
{
  a00 = t00;
  column0.setX(a00);
  row0.setX(a00);
}

void PHMatrix::setA01(double t01) 
{
  a01 = t01;
  column1.setX(a01);
  row0.setY(a01);
}

void PHMatrix::setA02(double t02) 
{
  a02 = t02;
  column2.setX(a02);
  row0.setZ(a02);
}

void PHMatrix::setA10(double t10) 
{
  a10 = t10;
  column0.setY(a10);
  row1.setX(a10);
}

void PHMatrix::setA11(double t11) 
{
  a11 = t11;
  column1.setY(a11);
  row1.setY(a11);
}
void PHMatrix::setA12(double t12) 
{
  a12 = t12;
  column2.setY(a12);
  row1.setZ(a12);
}

void PHMatrix::setA20(double t20) 
{
  a20 = t20;
  column0.setZ(a20);
  row2.setX(a20);
}
void PHMatrix::setA21(double t21) 
{
  a21 = t21;
  column1.setZ(a21);
  row2.setY(a21);
}
void PHMatrix::setA22(double t22) 
{
  a22 = t22;
  column2.setZ(a22);
  row2.setZ(a22);
}

void PHMatrix::setColumn0(PHVector &c0)
{
  setA00(c0.getX());
  setA10(c0.getY());
  setA20(c0.getZ());
}
void PHMatrix::setColumn1(PHVector &c1)
{
  setA01(c1.getX());
  setA11(c1.getY());
  setA21(c1.getZ());
}
void PHMatrix::setColumn2(PHVector &c2)
{
  setA02(c2.getX());
  setA12(c2.getY());
  setA22(c2.getZ());
}

void PHMatrix::setRow0(PHVector &r0)
{
  setA00(r0.getX());
  setA01(r0.getY());
  setA02(r0.getZ());
}
void PHMatrix::setRow1(PHVector &r1)
{
  setA10(r1.getX());
  setA11(r1.getY());
  setA12(r1.getZ());
}
void PHMatrix::setRow2(PHVector &r2)
{
  setA20(r2.getX());
  setA21(r2.getY());
  setA22(r2.getZ());
}

PHMatrix PHMatrix::operator- (const PHMatrix &b) const
{

  PHVector r0=row0-b.getRow0();
  PHVector r1=row1-b.getRow1();
  PHVector r2=row2-b.getRow2();

  PHMatrix newMatrix(r0,r1,r2);
  return newMatrix;
}

PHMatrix PHMatrix::operator+ (const PHMatrix &b) const
{

  PHVector r0=row0+b.getRow0();
  PHVector r1=row1+b.getRow1();
  PHVector r2=row2+b.getRow2();

  PHMatrix newMatrix(r0,r1,r2);
  return newMatrix;
}

PHMatrix PHMatrix::operator* (const double &c) const
{
  PHVector r0=row0*c;
  PHVector r1=row1*c;
  PHVector r2=row2*c;

  PHMatrix newMatrix(r0,r1,r2);
  return newMatrix;
}
PHVector PHMatrix::operator* (const PHVector &v) const
{

  PHVector vector;
  vector.setX(row0.dot(v));
  vector.setY(row1.dot(v));
  vector.setZ(row2.dot(v));

  return vector;
}

PHMatrix PHMatrix::operator* (const PHMatrix &b) const
{
  PHVector r0(row0.dot(b.column0),row0.dot(b.column1),row0.dot(b.column2));
  PHVector r1(row1.dot(b.column0),row1.dot(b.column1),row1.dot(b.column2));
  PHVector r2(row2.dot(b.column0),row2.dot(b.column1),row2.dot(b.column2));
  PHMatrix newMatrix(r0,r1,r2);
  return newMatrix;
}

PHMatrix PHMatrix::transpose() const

{
  PHMatrix newMatrix(column0,column1,column2);
  return newMatrix;
}

double PHMatrix::det()const
{

  double determinant=a00*(a11*a22-a21*a12)
    -a01*(a10*a22-a20*a12)
    +a02*(a10*a21-a11*a20);
    
  return determinant;
}

double PHMatrix::trace()const
{
  return (double)(a00+a11+a22);
}

ostream& operator<<(ostream& os, const PHMatrix &a)
{
  return (os << "( " << a.getA00() << ", " << a.getA01() << ", " << a.getA02() << " ) "<<endl<<"( " << a.getA10() << ", " << a.getA11() << ", " << a.getA12() << " ) "<<endl<<"( " << a.getA20() << ", " << a.getA21() << ", " << a.getA22() << " ) "<<endl);
}

void PHMatrix::print() const
{
  cout << "( " << a00 << ",  " << a01 << ",  " << a02 << "  ) " << endl<<
"( " << a10 << ",  " << a11 << ",  " << a12 << "  ) " << endl<<
    "( " << a20 << ",  " << a21 << ",  " << a22 << "  ) " << endl;
}

PHMatrix PHMatrix::inverse() //!returns a new PHMatrix that is the inverse
{
  double myDet = det();
  PHMatrix invM; 
  if (fabs(myDet) < GSL_FLT_EPSILON) {
    cout << "PROBLEM!! PHMatrix::invert(): tried to invert a matrix whose determinant is 0!!!!!! returning 0 matrix"<<endl;
    cerr << "PROBLEM!! PHMatrix::invert(): tried to invert a matrix whose determinant is 0!!!!!! returning 0 matrix"<<endl;
    return invM * 0.0;
  }
 
  //do the inversion with gsl_matrices
  int irrelavent=0;
  gsl_matrix* origMatrixG = gsl_matrix_alloc(3,3);
  gsl_matrix* invMatrixG = gsl_matrix_alloc(3,3);
  gsl_permutation* p = gsl_permutation_alloc(3);
  gsl_permutation_init(p);

  gsl_matrix_set(origMatrixG,0,0,a00);
  gsl_matrix_set(origMatrixG,0,1,a01);
  gsl_matrix_set(origMatrixG,0,2,a02);
  gsl_matrix_set(origMatrixG,1,0,a10);
  gsl_matrix_set(origMatrixG,1,1,a11);
  gsl_matrix_set(origMatrixG,1,2,a12);
  gsl_matrix_set(origMatrixG,2,0,a20);
  gsl_matrix_set(origMatrixG,2,1,a21);
  gsl_matrix_set(origMatrixG,2,2,a22);

  // to use the LU inversion, you must first perform the 
  // LU decomp.  Apparently this is common for numerical 
  // packages.  I think it sucks.  Why not have a function
  // that does both so I don't have to spend 1/2 hour 
  // looking through the gsl doc to find out what the hell
  // the permuation's for.  Just my two cents.

  gsl_linalg_LU_decomp(origMatrixG,p,&irrelavent);
  gsl_linalg_LU_invert (origMatrixG,p,invMatrixG);

  invM.setA00(gsl_matrix_get(invMatrixG,0,0));
  invM.setA01(gsl_matrix_get(invMatrixG,0,1));
  invM.setA02(gsl_matrix_get(invMatrixG,0,2));
  invM.setA10(gsl_matrix_get(invMatrixG,1,0));
  invM.setA11(gsl_matrix_get(invMatrixG,1,1));
  invM.setA12(gsl_matrix_get(invMatrixG,1,2));
  invM.setA20(gsl_matrix_get(invMatrixG,2,0));
  invM.setA21(gsl_matrix_get(invMatrixG,2,1));
  invM.setA22(gsl_matrix_get(invMatrixG,2,2));

  /* this was abandoned because of potential 
 round off errors, resulting in the current hodgepodge gsl
 method.  We could turn this on for speed though
  invM.setA00((a11*a22 - a12*a21)/ myDet);
  invM.setA10((a12*a20 - a10*a22)/ myDet);
  invM.setA20((a10*a21 - a20*a11)/ myDet);
  invM.setA01((a21*a02 - a01*a22)/ myDet);
  invM.setA11((a00*a22 - a02*a20)/ myDet);
  invM.setA21((a01*a20 - a00*a21)/ myDet);
  invM.setA02((a01*a12 - a02*a11)/ myDet);
  invM.setA12((a02*a10 - a00*a12)/ myDet);
  invM.setA22((a00*a11 - a10*a01)/ myDet);
  */

  return invM;

}

/*
gsl_matrix PHMatrix::getGslMatrix() 
{
  gsl_matrix* outMatrix = gsl_matrix_alloc (3,3);
  
  gsl_matrix_set(outMatrix,0,0,a00);
  gsl_matrix_set(outMatrix,0,1,a01);
  gsl_matrix_set(outMatrix,0,2,a02);
  gsl_matrix_set(outMatrix,1,0,a10);
  gsl_matrix_set(outMatrix,1,1,a11);
  gsl_matrix_set(outMatrix,1,2,a12);
  gsl_matrix_set(outMatrix,2,0,a20);
  gsl_matrix_set(outMatrix,2,1,a21);
  gsl_matrix_set(outMatrix,2,2,a22);

  return *outMatrix;
   
}

*/
