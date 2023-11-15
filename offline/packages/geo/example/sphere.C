//
// This is a program example which will show how make use of the sphere
// object. It shows how to calculate intersections with lines and polylines
//

#include <iostream.h>
#include <math.h>
#include "PHGeometron.h"

main()
{
  //----------------------------------------------------------------------
  // intersection Sphere - line  
  //----------------------------------------------------------------------
 

  //
  // instantiate a PHGeometron
  //

  PHGeometron *geometry = PHGeometron::instance();
  //
  // Create a sphere centered in the origin (0,0,0) and radius r
  // 
  double radius = 4;
  PHPoint  origin(0,0,0);
  PHSphere sphere(origin,radius);

  sphere.print();
   //
   // Create a line which has a basepoint  (point1) and a direction (vector1)
   //
  
  PHPoint  point1(4,0,0);
  PHVector vector1(0,0,1);
  PHLine line(point1,vector1);

  //
  //  Intersection points
  //   
  PHPoint out1,out2;

   // use the function intersectionLineSphere   in the PHGeometron
   // it returns the number of intersection points
  
  short numberOfIntersections;
  numberOfIntersections = geometry->intersectionLineSphere(line,sphere,out1,out2);
   //
   // Print out the intersection points if any
   //
   cout << "*************************************************"<<endl;
   cout << "Result from line - Sphere intersection "<< endl;
   cout << "*************************************************"<<endl;
   if (numberOfIntersections==1) {  // if one intersection point 
     cout <<"There is only one intersection point: "<<  out1 << endl;
   }else if (numberOfIntersections ==2) {  // if two intersection points
     cout <<"There are two intersection points: "<<  out1 << " " << out2 << endl;
   }else { // no intersection or infinite intersections
     cout << "No good intersection " << endl;
   }

  //----------------------------------------------------------------------
   // intersection PolyLine - Sphere
   //----------------------------------------------------------------------

   // create a polyline with just two points

   PHPoint *p1 = new PHPoint(9,0,0);
   PHPoint *p2 = new PHPoint(7,0,0);

   PHPolyLine polyline;
   polyline.appendPoint(p1);
   polyline.appendPoint(p2);

   // use the previous sphere

   numberOfIntersections= geometry->intersectionPolyLineSphere(polyline,sphere,out1,out2);
 

   cout << "*************************************************"<<endl;
   cout << "Result from polyline- Sphere intersection "<< endl;
   cout << "*************************************************"<<endl;

    if (numberOfIntersections==1) {  // if one intersection point 
     cout <<"There is only one intersection point: "<<  out1 << endl;
   }else if (numberOfIntersections ==2) {  // if two intersection points
     cout <<"There are two intersection points: "<<  out1 << " " << out2 << endl;
   }else { // no intersection or infinite intersections
     cout << "No good intersection " << endl;
   }

 
 
 
}

