//
// This is a program example which will show how make use of the matrix
// object. 
// 
//

#include <iostream.h>
#include <math.h>
#include "PHMatrix.h"
#include "phool.h"
main()
{
  //--------------------------
  // constructors
  //--------------------------

  // 1. blanc costructor - initializes unit matrix

  
  PHMatrix matrix;
  matrix.print();

  // 2. costruct using 3 row vectors

  PHVector r0(0,1,0);
  PHVector r1(-1,0,0);
  PHVector r2(0,0,1);
  PHMatrix matrix1(r0,r1,r2);
  PHVector cc0;
  cc0=matrix1.getColumn0();
  cout<<" -------get  column0 vector  --------------------"<<endl;
  cc0.print();
  double t00=matrix1.getA00();
  cout<<" -------get the a00 matrix element -----  "<<t00<<endl;
  matrix1.print();
  cout<<" --------------------"<<endl;

  // 3. counstruct with Euler angles

  cout<<" construct with Euler angles "<<endl;
  double theta=45*Pi/180;
  double phi=30*Pi/180;

  PHVector permut(2,3,1);
  double zero=0;
  PHMatrix rotation(permut,theta,phi,zero);
  cout<<" "<<endl;
  rotation.print();
  cout <<" -------------------------------"<<endl;
  //------------------------------------------
  // operators
  //------------------------------------------
  //   =
  cout<<" operator = "<<endl;
  PHMatrix matrix3=matrix1;
  matrix3.print();
  cout<<"-----------------------"<<endl;
  // +
  cout<<" operator + "<<endl;
  PHMatrix matrix4=matrix3+matrix1;
  matrix4.print();
  cout<<"-----------------------"<<endl;
  // -
  cout<<" operator - "<<endl;
  PHMatrix matrix5=matrix4-matrix1;
  matrix5.print();
  cout<<"-----------------------"<<endl;
  // * multiply by a scalar, vector, matrix
  cout<<" operator *  "<<endl;
  cout<<" scalar multiplication "<<endl;
  double c=2;
  PHMatrix matrix6=matrix4*c;
  cout<<matrix4<<endl<<" * "<<c<<endl<<"  =  "<<endl<<matrix6<<endl;
  cout<<" -------------- "<<endl;
  PHVector v(1,2,1);
  PHVector vMultiplied=matrix4*v;
  cout<<" vector multiplication "<<endl;
  cout<<matrix4<<endl<<" * "<<v<<endl<<"  =  "<<endl<<vMultiplied<<endl;
  cout<<" -------------- "<<endl;
  cout<<" matrix multiplication "<<endl;
  PHMatrix matrix7=matrix4*matrix1;
  cout<<matrix4<<endl<<" * "<<endl<<matrix1<<endl<<"  =  "<<endl<<matrix7<<endl;

  //-------------------------------------------
  // get determinant, trace, transpose
  //-------------------------------------------
  double d=matrix1.det();
  double t=matrix1.trace();
  PHMatrix trm4=matrix1.transpose();

  cout<<"-------------------"<<endl;
  cout<<"get determinant, trace, transpose"<<endl;
  cout<<" the matrix "<<endl<< matrix1<<endl;
  cout<<" determinant "<<d<<endl;
  cout<<" trace  "<<t<<endl;
  cout<<" transpose "<<endl<<trm4<<endl;

}





