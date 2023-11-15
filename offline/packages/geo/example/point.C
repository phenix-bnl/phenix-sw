//
// This is a program example which will show how make use of the different point
// objects. 
//

#include <iostream.h>
#include <math.h>
#include "phool.h"
#include "PHGeometron.h"

main()
{
  //----------------------------------------------------------------------
  // Some functionality of the several point classes
  //----------------------------------------------------------------------

  //
  // instantiate a PHGeometron
  //
  PHGeometron *geometry = PHGeometron::instance(); 

  //
  //  Create a cartesian point (x,y,z)
  //
  PHPoint cartesianPoint(0,5,3);
  cout << "Cartesian Point :"<< cartesianPoint << endl;
  //
  // the same point in cylindrical coordinates
  //
  PHCylPoint cylindricalPoint = cartesianPoint;  // use the equal operator
  cout << "Cylindrical Point :"<< cylindricalPoint << endl;
  //or
  geometry->cartesianToCylindrical(cartesianPoint,cylindricalPoint);
  cout << "Cylindrical Point :"<< cylindricalPoint << endl;

  //
  // the same point in spherical coordinates
  //

  PHSphPoint sphericalPoint  = cartesianPoint;
  cout << "Spherical Point :"<< sphericalPoint << endl;
  // or
  geometry->cartesianToSpherical(cartesianPoint,sphericalPoint);
  cout << "Spherical Point :"<< sphericalPoint << endl;
  
  
  //-------------------------------------------------------------
  //  Distance Point to Point  (2 methods)
  //------------------------------------------------------------

  PHPoint point1(0,2,3);
  PHPoint point2(4,2,1);
  double distance1 = point1.distanceToPoint(point2);
  double distance2 = geometry->distancePointToPoint(point1,point2);


  cout << "distance between 2 Cartesian points (first  method): " << distance1 << endl; 
  cout << "distance between 2 Cartesian points (second method): " << distance2 << endl; 

  PHSphPoint spoint1 = point1;
  PHSphPoint spoint2 = point2;
  distance2 = geometry->distancePointToPoint(spoint1,spoint2);
  cout << "distance between 2 Spherical points (second method): " << distance2 << endl; 

  PHCylPoint cpoint1 = point1;
  PHCylPoint cpoint2 = point2;
  distance2 = geometry->distancePointToPoint(cpoint1,cpoint2);
  cout << "distance between 2 Cylindrical points (second method): " << distance2 << endl; 


  //-------------------------------------------------------------------
  //  Transform a point from a reference frame to an other one
  //--------------------------------------------------------------------
  PHFrame XYZ;
  //
  // Create a new Frame translated and rotate in respect to the first one
  //
   PHPoint newOrigin(5,1,0);
   PHVector u(0,1,0);
   PHVector v(1,0,0); //= u.orthogonal();
   PHFrame newFrame(newOrigin,u,v);

   XYZ.print();
   newFrame.print();
   
   PHPoint trans = geometry->transformPoint(XYZ,point1,newFrame);
   PHPoint transBack = geometry->transformPoint(newFrame,trans,XYZ);

   cout << "Original point "<< point1<< endl;
   cout << "Transformed  " << trans << endl;
   cout << "Transformed Back  "<< transBack << endl;
 
}
