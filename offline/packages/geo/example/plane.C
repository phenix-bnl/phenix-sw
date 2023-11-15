//
// This is a program example which will show how make use of the plane
// object. It shows how to calculate intersections with lines and polylines,
// how to project a line into a plane and to intersect two line (projected) into a plane
//

// PHBoolean intersectionLinePlane(const PHLine&, const PHPlane&, PHPoint&);
// PHBoolean intersectionPolyLinePlane(const PHPolyLine & , const PHPlane&, PHPoint&); 
// PHBoolean intersectionLineLineOnPlane(const PHLine&, const PHLine&, const PHPlane&, PHPoint&);
// PHLine   projectLineIntoPlane(const PHLine&, const PHPlane&);


#include <iostream.h>
#include <math.h>
#include "phool.h"
#include "PHGeometron.h"

main()
{
  //----------------------------------------------------------------------
  // intersection line - Plane
  //----------------------------------------------------------------------
 

  //
  // instantiate a PHGeometron
  //
  PHGeometron *geometry = PHGeometron::instance();
  
  //
  // Create a plane defined by a reference point and a normal vector 
  // 
  PHPoint  reference(0,0,0);
  PHVector normal(0,0,1);
   
  PHPlane plane(reference,normal);

  // print out the plane
  plane.print();

  //
  // Create a line which has a basepoint  (point1) and a direction (vector1)
  //
   PHPoint  point1(0,0,1);
   PHVector vector1(1,1,1);
   PHLine line(point1,vector1);

   //
   //  Intersection point
   // 
   PHPoint out1;

   // use the function   PHBoolean intersectionLinePlane(const PHLine&, const PHPlane&, PHPoint&);
   // it returns a boolean if there is an intersection point
   PHBoolean intersection;
   intersection = geometry->intersectionLinePlane(line,plane,out1);
   //
   // Print out the intersection points if any
   //
   cout << "*************************************************"<<endl;
   cout << "Result from line - Plane intersection "<< endl;
   cout << "*************************************************"<<endl;
   
   if (intersection) {  // if an intersection 
     cout <<"There is one intersection point: "<<  out1 << endl;
   }else { // no intersection or infinite intersections
     cout << "No good intersection " << endl;
   }

   
   //----------------------------------------------------------------------
   // intersection PolyLine - Plane
   //----------------------------------------------------------------------

   // create a polyline with just two points
   
   PHPoint *p1 = new PHPoint(0,0,1);  
   PHPoint *p2 = new PHPoint(4,0,1);

   PHPolyLine polyline;
   polyline.appendPoint(p1);
   polyline.appendPoint(p2);

   // use the previous plane
   
   intersection = geometry->intersectionPolyLinePlane(polyline,plane,out1);
   cout << "*************************************************"<<endl;
   cout << "Result from polyline- Plane intersection "<< endl;
   cout << "*************************************************"<<endl;

   if (intersection) {  // if an intersection 
     cout <<"There is one intersection point: "<<  out1 << endl;
   }else { // no intersection or infinite intersections
     cout << "No good intersection " << endl;
   }

   //---------------------------------------------------------------------
   // Intersection Line Line on Plane
   //--------------------------------------------------------------------

   //
   // Create a second line (use the previous plane and line also)
   //

   PHPoint point2(0,2,4);
   PHVector vector2(1,3,2);
   PHLine line2(point2,vector2);
   
   intersection = geometry->intersectionLineLineOnPlane(line,line2,plane,out1);
   cout << "*************************************************"<<endl;
   cout << "Result from intersection Line-Line on Plane"<< endl;
   cout << "*************************************************"<<endl;

   if (intersection) {  // if an intersection 
     cout <<"There is one intersection point: "<<  out1 << endl;
   }else { // no intersection or infinite intersections
     cout << "No good intersection " << endl;
   }

   //---------------------------------------------------------------------
   // Project line into plane
   //--------------------------------------------------------------------

   // use previous line and plane

   PHLine projectedLine;
   projectedLine = geometry->projectLineIntoPlane(line,plane);
   cout << "*************************************************"<<endl;
   cout << "Projected Line on plane:"<< endl;
   cout << "*************************************************"<<endl;

   cout << projectedLine << endl;
   
   
 
}

