//
// This is a program example which will show how make use of 
// transformations of different objects using either 2 frames,
// or a matrix and a vector
// author: Julia Velkovska julia.velkovska@sunysb.edu

#include <iostream.h>
#include <math.h>
#include "phool.h"
#include "PHGeometron.h"

main()
{
  //
  // instantiate a PHGeometron
  //
  PHGeometron *geometry = PHGeometron::instance(); 

  //
  //  Point transformations
  //
  PHPoint point1(2,1,-1);

  //-------------------------------------------------------------------
  //  Passive transformation - transform the coordinate system
  //--------------------------------------------------------------------
  PHPoint origin1(0,0,0);
  PHVector u1(1,0,0);
  PHVector v1(0,1,0);
  PHFrame F1(origin1,u1,v1);;
  //
  // Create a new Frame rotated and translated with  respect to the first one
  //
   PHPoint origin2(1,1,3);
   PHVector u2(1,0,0);
   PHVector v2(0,.866025, -0.5 );
   PHFrame F2(origin2,u2,v2);
   cout<<"Coordinate transformations can be done in two ways:"<<endl;

   cout<<"---------------------------------------------------"<<endl;
   cout<<"1. Passive - we transform the coordinate system. "<<endl;
   cout<<"             the point does not move "<<endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<"Example of passive coordinate transformation"<<endl;
   cout<<"Define two reference frames "<<endl;
   F1.print();
   F2.print();
   cout<<"Define a point in F1     "<<point1<<endl;
   cout<<"transform to F2 using transformPoint(F1,pointIn,F2)"<<endl; 
   PHPoint trans = geometry->transformPoint(F1,point1,F2);
   cout <<"Transformed in F2    " << trans << endl;
   PHPoint transBack = geometry->transformPoint(F2,trans,F1);
   cout<<"concistency check"<<endl; 
   cout<<"Transformed Back in F1   "<< transBack << endl;
   cout<<"---------------------------------------------------"<<endl;

   cout<<"2. Active - the point is moved in the original coordinate"<<endl;
   cout<<"            system by applying a rotation and translation"<<endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<"To do this, you need to obtain the rotation matrix and the"<<endl; 
   cout<<"translation vector  and then apply:"<<endl;
   cout<<"transformPoint(const PHMatrix &,const PHVector &,const PHPoint&)"<<endl;
   cout<<"---------------------------------------------------"<<endl;

   cout<<"There are two ways to obtain the matrix and vector"<<endl;
   cout<<" Active transformation - EXAMPLE A"<<endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<"   A.1 Specify and axis about which to rotate and an angle of rotation"<<endl;
   cout<<"   use rotationMatrix(delta,axis) to obtain the matrix "<<endl;
   PHVector axis(1,0,0);
   double delta=30*Pi/180;   // 30 degrees about X
   PHMatrix X30degrees=geometry->rotationMatrix(delta,axis);
   PHVector do_translate(-1, 0.633975, -3.09808 );
   cout<<"this is the rotation matrix that will rotate an object by "<<delta*180/Pi<< " degrees  about the axis "<<axis<<endl;
   cout<<X30degrees;

   cout<<"   A.2 Define a translation vector "<<endl;
   cout<<"translation"<<do_translate<<endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<"   A.3 perform an active transformation - rotate and translate the object"<<endl;
   cout << "point in F1    "<< point1<< endl;
   trans=geometry->transformPoint(X30degrees,do_translate,point1);
   cout << "the new point in F1 transformed with matrix and vector "<< endl;
   cout<< trans << endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<" Active transformation - EXAMPLE B"<<endl;
   cout<<" "<<endl;
   cout<<"    B.1 If we know the two frames, we can obtain the rotation"<<endl;
   cout<<"       and translation needed in F1, that will be equivalent"<<endl;
   cout<<"       to transforming the point from F1 to F2"<<endl;
   cout<<"       use frames2MatrixAndVector(F1,F2,matrix,vector)"<<endl;
   cout<<"    B.2 transformPoint(matrix,vector,point)"<<endl;
   PHMatrix rotation12;
   PHVector translation12;
   geometry->frames2MatrixAndVector(F1,F2,rotation12,translation12);
   trans=geometry->transformPoint(rotation12,translation12,point1);
   cout<<" rotation matrix and translation vector in F1"<<endl;
   cout<<" equivalent to coordinate transformation from F1 to F2  "<<endl;
   cout<<rotation12;
   cout<<" translation vector "<<translation12<<endl;
   cout << "point in F1 "<< point1<< endl;
   cout << "rotated and translated in F1 " <<endl;
   cout << trans << endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<"  If you need to go back to the original coordinates"<<endl;
   cout<<"  you can do it either using frames "<<endl;
   cout<<" transformPoint(F2,point,F1)"<<endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<" going back with frames"<<endl;
   PHPoint backwithframes=geometry->transformPoint(F2,trans,F1);
   cout<< backwithframes<<endl;
   cout<<" or by obtaining the inverse"<<endl;
   cout<<"  transformation as:  "<<endl; 
   cout<<"  geometry->frames2MatrixAndVector(F2,F1,rotation21,translation21);"<<endl;
   cout<<" and then:  transformPoint(rotation21,translation21,point)"<<endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<" going back by obtaining the inverse transformation"<<endl;
   cout<<" "<<endl;
   PHMatrix rotation21;
   PHVector translation21;
   geometry->frames2MatrixAndVector(F2,F1,rotation21,translation21);
   cout<<"rotation matrix and translation vector equivalent to "<<endl;
   cout<<"coordinate transformation from F2 to F1 "<<endl;
   cout<<rotation21;
   cout<<" translation vector  "<<translation21<<endl;
   PHPoint backwithmatrix=geometry->transformPoint(rotation21,translation21,trans);
   cout<<" "<<endl;
   cout<<" back with matrix and vector "<<endl;
   cout << backwithmatrix << endl;
   cout<<" --------------------------------------"<<endl;
   cout<<" --------------------------------------"<<endl;
   cout<<" "<<endl;
   cout<<"3. Additional functionality"<<endl;
   cout<<"---------------------------------------"<<endl;
   cout<<"   If the matrix and vector of the transformation in F1 are known"<<endl;
   cout<<"   we can obtain F2, such that a passive transformation from F1 to F2"<<endl;
   cout<<"   is equivalent to the active transformation in F1. USE :"<<endl;
   cout<<"   MatrixAndVector2frames(F1,matrix,vector) , which will return F2"<<endl;   
PHFrame newFrame=geometry->MatrixAndVector2frames(F1,X30degrees,do_translate);
 cout<<" here is the returned frame "<<endl;
 newFrame.print();
   cout<<"---------------------------------------"<<endl;
   cout<<"---------------------------------------"<<endl;
   cout<<"vector transformations - active and passive  "<<endl;
   PHVector vector1(2,1,0);
   cout<<"vector in F1    "<<vector1<<endl;
   PHVector vector2matrix=geometry->transformVector(rotation12,vector1);
   cout<<"transformed with matrix  "<<vector2matrix<<endl;
   PHVector vback=geometry->transformVector(rotation21,vector2matrix);
   cout<<"going back with the inverse matrix  "<<vback<<endl;
   PHVector vector2=geometry->transformVector(F1,vector1,F2);
   cout<<"transformed with frames  "<<vector2<<endl;
   vback=geometry->transformVector(F2,vector2,F1);
   cout<<"going back with frames  "<<vback<<endl;
   cout<<"---------------------------------------"<<endl;
   cout<<"---------------------------------------"<<endl;
   cout<<"line transformations - active and passive"<<endl;
   PHPoint point2(4,3,-1);
   PHLine line1(point1,point2);
   cout<<"line in F1  "<<line1<<endl;
   PHLine line2=geometry->transformLine(rotation12,translation12,line1);
   cout<<"transformed with matrix and vector  "<<line2<<endl;
   PHLine backline=geometry->transformLine(rotation21,translation21,line2);
   cout<<"going back with the inverse transformation "<<backline<<endl;
   line2=geometry->transformLine(F1,line1,F2);
   cout<<"transformed with frames "<<line2<<endl;
   backline=geometry->transformLine(F2,line2,F1);
   cout<<"going back with frames "<<backline<<endl;
   cout<<"---------------------------------------"<<endl;
   cout<<"---------------------------------------"<<endl;
   cout<<"panel transformations - active "<<endl;
   PHPoint point3(-3,-1,-3);
   PHPanel panel1(point1,point2,point3);
   cout<<"original panel "<<endl;
   panel1.print();
   PHPanel panel2=geometry->transformPanel(rotation12,translation12,panel1);
   cout<<"transformed panel "<<endl;
   panel2.print();
   cout<<"transformed back panel "<<endl;
   PHPanel backpanel=geometry->transformPanel(rotation21,translation21,panel2);
   backpanel.print();
   
 
}









