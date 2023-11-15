//
// This is a program example which will show how make use of the cylinder
// object. It shows how to calculate intersections with lines and polylines
//


#include <iostream.h>
#include <math.h>
#include "phool.h"
#include "PHGeometron.h"

main()
{
  //----------------------------------------------------------------------
  // intersection Cylinder - line  
  //----------------------------------------------------------------------
 

  //
  // instantiate a PHGeometron
  //
  PHGeometron *geometry = PHGeometron::instance(); 
  //
  // Create a cylinder centered in the origin (0,0,0) and axis along the Z direction and radius r
  // The length of the axis vector is the half-length of the cylinder
  // 
  
   PHPoint  origin(0,0,0);
   PHVector axis(0,0,10); 
   double radius = 4;  
   
   PHCylinder cylinder(origin,radius,axis);

   cylinder.print();
   
   //
   // Create a line which has a basepoint  (point1) and a direction (vector1)
   //
   PHPoint  point1(0,0,1);
   PHVector vector1(0,7,0);

   PHLine line(point1,vector1);

   //
   //  Intersection points
   // 
   PHPoint out1,out2;

   // use the function intersectionLineCylinder in the PHGeometron
   // it returns the number of intersection points
   short numberOfIntersections;
   numberOfIntersections = geometry->intersectionLineCylinder(line,cylinder,out1,out2);

   //
   // Print out the intersection points if any
   //
   cout << "*************************************************"<<endl;
   cout << "Result from line - Cylinder intersection "<< endl;
   cout << "*************************************************"<<endl;
   
   if (numberOfIntersections==1) {  // if one intersection point 
     cout <<"There is only one intersection point: "<<  out1 << endl;
   }else if (numberOfIntersections ==2) {  // if two intersection points
     cout <<"There are two intersection points: "<<  out1 << " " << out2 << endl;
   }else { // no intersection or infinite intersections
     cout << "No good intersection " << endl;
   }

   
   //----------------------------------------------------------------------
   // intersection PolyLine - Cylinder
   //----------------------------------------------------------------------

   // create a polyline with just two points
   
   PHPoint *p1 = new PHPoint(0,0,1);  
   PHPoint *p2 = new PHPoint(4,0,1);

   PHPolyLine polyline;
   polyline.appendPoint(p1);
   polyline.appendPoint(p2);

   // use the previous cylinder
   
   numberOfIntersections = geometry->intersectionPolyLineCylinder(polyline,cylinder,out1,out2);
   cout << "*************************************************"<<endl;
   cout << "Result from polyline- Cylinder intersection "<< endl;
   cout << "*************************************************"<<endl;

    if (numberOfIntersections==1) {  // if one intersection point 
     cout <<"There is only one intersection point: "<<  out1 << endl;
   }else if (numberOfIntersections ==2) {  // if two intersection points
     cout <<"There are two intersection points: "<<  out1 << " " << out2 << endl;
   }else { // no intersection or infinite intersections
     cout << "No good intersection " << endl;
   }

 
 
}

