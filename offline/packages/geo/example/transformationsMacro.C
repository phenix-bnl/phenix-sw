//
// This is a root macro example which will show how make use of 
// transformations of different objects using either 2 frames,
// or a matrix and a vector
// author: Julia Velkovska julia@bnl.gov

{
  gSystem->Load("libphgeo.so");
  //
  // instantiate a PHGeometron
  //

  using namespace PHGeometry;

  //  PHGeometron *geometry = PHGeometron::instance(); 

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
   cout<<"Define a point in F1:     "<<endl;
   point1.print();
   cout<<"Transform to F2 using transformPoint(F1,pointIn,F2)"<<endl; 
   PHPoint trans =  transformPoint(F1,point1,F2);
   cout <<"Transformed in F2:    " << endl;
   trans.print();
   PHPoint transBack =  transformPoint(F2,trans,F1);
   cout<<"Concistency check:"<<endl; 
   cout<<"Transformed Back in F1:   "<< endl;
   transBack.print();
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
   cout<<"   A.1 Specify and axis about which to rotate and an angle of rotation."<<endl;
   cout<<"   Use: rotationMatrix(delta,axis) to obtain the matrix. "<<endl;
   PHVector axis(1,0,0);
   Double_t Pi=3.141592654;
   double delta=30*Pi/180;   // 30 degrees about X
   PHMatrix X30degrees= rotationMatrix(delta,axis);
   PHVector do_translate(-1, 0.633975, -3.09808 );
   cout<<"This is the rotation matrix that will rotate an object by "<<delta*180/Pi<< " degrees  about the axis ";
   axis.print();
   X30degrees.print();

   cout<<"   A.2 Define a translation vector: "<<endl;
   cout<<"translation";
   do_translate.print();
   cout<<"---------------------------------------------------"<<endl;
   cout<<"   A.3 Perform an active transformation - rotate and translate the object."<<endl;
   cout << "point in F1:    ";
   point1.print();
   trans= transformPoint(X30degrees,do_translate,point1);
   cout << "The new point in F1 transformed with matrix and vector: "<< endl;
   trans.print(); 
   cout<<"---------------------------------------------------"<<endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<" Active transformation - EXAMPLE B"<<endl;
   cout<<" "<<endl;
   cout<<"    B.1 If we know the two frames, we can obtain the rotation"<<endl;
   cout<<"       and translation needed in F1, that will be equivalent"<<endl;
   cout<<"       to transforming the point from F1 to F2."<<endl;
   cout<<"       Use: frames2MatrixAndVector(F1,F2,matrix,vector)"<<endl;
   cout<<"    B.2 transformPoint(matrix,vector,point)"<<endl;
   PHMatrix rotation12;
   PHVector translation12;
    frames2MatrixAndVector(F1,F2,rotation12,translation12);
   trans= transformPoint(rotation12,translation12,point1);
   cout<<" Rotation matrix and translation vector in F1"<<endl;
   cout<<" equivalent to coordinate transformation from F1 to F2:  "<<endl;
   rotation12.print();
   cout<<" translation vector: ";
   translation12.print();
   cout << "point in F1: ";
   point1.print();
   cout << "rotated and translated in F1: " <<endl;
   trans.print();
   cout<<"---------------------------------------------------"<<endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<"  If you need to go back to the original coordinates";
   cout<<"  you can do it either using frames "<<endl;
   cout<<" transformPoint(F2,point,F1)"<<endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<" Going back with frames:"<<endl;
   PHPoint backwithframes= transformPoint(F2,trans,F1);
   
   backwithframes.print();
   
   cout<<" or by obtaining the inverse";
   cout<<"  transformation as:  "<<endl; 
   cout<<"   frames2MatrixAndVector(F2,F1,rotation21,translation21);"<<endl;
   cout<<" and then:  transformPoint(rotation21,translation21,point)"<<endl;
   cout<<"---------------------------------------------------"<<endl;
   cout<<" Going back by obtaining the inverse transformation:"<<endl;
   cout<<" "<<endl;
   PHMatrix rotation21;
   PHVector translation21;
    frames2MatrixAndVector(F2,F1,rotation21,translation21);
   cout<<"Rotation matrix and translation vector equivalent to "<<endl;
   cout<<"coordinate transformation from F2 to F1: "<<endl;
   cout<<"matrix:";
   rotation21.print();
   cout<<" translation vector:  ";
   translation21.print();
   PHPoint backwithmatrix= transformPoint(rotation21,translation21,trans);
   cout<<" "<<endl;
   cout<<" Back with matrix and vector: "<<endl;
   backwithmatrix.print(); 
   cout<<" --------------------------------------"<<endl;
   cout<<" --------------------------------------"<<endl;
   cout<<" "<<endl;
   cout<<"3. Additional functionality"<<endl;
   cout<<"---------------------------------------"<<endl;
   cout<<"   If the matrix and vector of the transformation in F1 are known"<<endl;
   cout<<"   we can obtain F2, such that a passive transformation from F1 to F2"<<endl;
   cout<<"   is equivalent to the active transformation in F1. USE :"<<endl;
   cout<<"   MatrixAndVector2frames(F1,matrix,vector) , which will return F2"<<endl;   
PHFrame newFrame= MatrixAndVector2frames(F1,X30degrees,do_translate);
 cout<<" Here is the returned frame: "<<endl;
 newFrame.print();
   cout<<"---------------------------------------"<<endl;
   cout<<"---------------------------------------"<<endl;
   cout<<"Vector transformations - active and passive  "<<endl;
   PHVector vector1(2,1,0);
   cout<<"vector in F1:    ";
   vector1.print();
   PHVector vector2matrix= transformVector(rotation12,vector1);
   cout<<"Transformed with matrix:  ";
   vector2matrix.print();
   PHVector vback= transformVector(rotation21,vector2matrix);
   cout<<"Going back with the inverse matrix:  ";
   vback.print();
   PHVector vector2= transformVector(F1,vector1,F2);
   cout<<"Transformed with frames:  ";
   vector2.print();
   vback= transformVector(F2,vector2,F1);
   cout<<"Going back with frames:  ";
   vback.print();
   cout<<"---------------------------------------"<<endl;
   cout<<"---------------------------------------"<<endl;
   cout<<"Line transformations - active and passive"<<endl;
   PHPoint point2(4,3,-1);
   PHLine line1(point1,point2);
   cout<<"Line in F1:  ";
   line1.print();
   PHLine line2= transformLine(rotation12,translation12,line1);
   cout<<"Transformed with matrix and vector:  ";
   line2.print();
   PHLine backline= transformLine(rotation21,translation21,line2);
   cout<<"going back with the inverse transformation ";
   backline.print();
   line2= transformLine(F1,line1,F2);
   cout<<"Transformed with frames: ";
   line2.print();
   backline= transformLine(F2,line2,F1);
   cout<<"Going back with frames: ";
   backline.print();
   cout<<"---------------------------------------"<<endl;
   cout<<"---------------------------------------"<<endl;
   cout<<"Panel transformations - active "<<endl;
   PHPoint point3(-3,-1,-3);
   PHPanel panel1(point1,point2,point3);
   cout<<"original panel: "<<endl;
   panel1.print();
   PHPanel panel2= transformPanel(rotation12,translation12,panel1);
   cout<<"transformed panel: "<<endl;
   panel2.print();
   cout<<"transformed back panel: "<<endl;
   PHPanel backpanel= transformPanel(rotation21,translation21,panel2);
   backpanel.print();
   
 
}









