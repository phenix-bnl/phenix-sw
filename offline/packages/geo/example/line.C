//
//  An example program using the PHLine class and related routines located in
//  the class PHGeometron.
//
//  Some relevant definitions:
//
//  Line
//    A line is defined by a basepoint and a direction vector.
//
//  Plane
//    A plane is defined by an origin point and a unit vector that is 
//    normal to the plane.
//
//  Transform
//    To both translate and rotate a point, vector, or line from one
//    reference frame into another.

// First, include the PHGeometron header file.

#include <iostream.h>
#include <math.h>
#include "PHGeometron.h"

main()
{

  //
  // Create a geometry object pointer to access the geometry routines.
  //
  
  PHGeometron *geometry = PHGeometron::instance();
  
  //
  //   1.  Create a line using a point and vector (PHPoint and PHVector).
  //
  
  PHPoint point(1,1,0);
  PHVector vector(0,0,10);
  
  PHLine line1(point,vector);
 
  cout << endl;
  cout << "1.  line1(point,vector) = " << line1 << endl;
  cout << "    length of line = " << line1.length() << endl;
  cout << endl;

  //
  //  NOTE:  the following is equivalent.
  // 
  //  PHLine line1;
  //
  //  line1.setBasepoint(point);
  //  line1.setDirection(vector);
  //
  //  line1.print();
  //

  //
  //   2.  Create another line using two points (PHPoint).
  //

  PHPoint point1(0,0,1);
  PHPoint point2(10,0,9);

  PHLine line2(point1,point2);

  cout << "2.  line2(point, point) = " << line2 << endl;
  cout << "    length of line = " << line2.length() << endl;
  cout << endl;
 
  //
  //  NOTE:  the following is equivalent.
  //
  //  PHLine line2;
  //
  //  line2.setBasepoint(point1);
  //  line2.setDirection((PHVector)(point2 - point1));
  //
  //  line2.print();
  //

  //
  //   3.  Find the minimum distance between the two lines
  //       (PHGeometron::distanceLineLine).
  //

  double lineDistance = geometry->distanceLineLine(line1,line2);

  cout << "3.  distance between lines = " << lineDistance << endl;
  cout << endl;

  //
  //   4. Find the point of closest approach of the line2 to line1
  //      and print out the coordinates
  //
  PHPoint closest = geometry->closestApproachLineLine(line1,line2);
  cout<< "4. the point on line2 closest to line1 " << closest <<endl;
  cout<< endl;
  //
  //   5.  Find the minimum distance between a point (for example, the
  //       origin) and one of the lines (PHGeometron::distanceLinePoint).
  //

  PHPoint origin(0,0,0);
  
  double originLine1Distance = geometry->distanceLinePoint(line1,origin);
  double originLine2Distance = geometry->distanceLinePoint(line2,origin);

  cout << "5.  distance of line1 from origin = " << originLine1Distance << endl;
  cout << "    distance of line2 from origin = " << originLine2Distance << endl;
  cout << endl;

  // 6. Get the coordinates of the points on line1 and line2 
  //that are closest to a given point (in this case - the origin) 
  //
  PHPoint closest1 = geometry->closestApproachLinePoint(line1,origin);
  PHPoint closest2 = geometry->closestApproachLinePoint(line2,origin);

  cout << "6.  closest point on line1 to the origin = " << closest1 << endl;
  cout << "    closest point on line2 to the origin = " << closest2 << endl;
  cout << endl;

  //
  //   7.  Find the dot/cross products and angle between the 
  //       direction vectors of the lines (PHGeometron::dot, product, angle).  
  //

  double dotProduct = geometry->dot(line1.getDirection(),line2.getDirection());
  PHVector crossProduct = geometry->cross(line1.getDirection(),line2.getDirection());
  PHAngle angleBetween = geometry->angle(line1.getDirection(),line2.getDirection());

  cout << "7.  dot product = " << dotProduct << endl;
  cout << "    cross product = " << crossProduct << endl;
  cout << "    angle between = " << angleBetween.degree() << endl;
  cout << endl;

  //
  //  NOTE:  the following is equivalent.
  //
  //  double dotProduct = line1.getDirection().dot(line2.getDirection());
  //  PHVector crossProduct = line1.getDirection().cross(line2.getDirection());
  //  PHAngle angleBetween = line1.getDirection().angle(line2.getDirection());
  //

  //
  //   8.  Create a plane (PHPlane).
  //

  PHVector zAxis(0,0,1);
  PHPlane xyPlane(origin,zAxis);

  cout << "8.  the xy-plane = " << xyPlane << endl;
  cout << endl;

  //
  //  NOTE:  the following is equivalent.
  //
  //  PHPlane xyPlane;
  //
  //  xyPlane.setOrigin(origin);
  //  xyPlane.setNormal(zAxis);
  //
  //  xyPlane.print();
  //

  //
  //   9.  Find the intersection between one of the lines and this plane
  //       (PHGeometron::intersectionLinePlane).
  //
  
  PHPoint intersection;
  PHBoolean intersect = geometry->intersectionLinePlane(line2,xyPlane,intersection);

  if (intersect) {
    cout << "9.  intersection of line2 with xy-plane = " << intersection << endl;
  }else{
    cout << "9.  the xy-plane and line2 do not intersect" << endl;
  }
  cout << endl;

  //
  //   10.  Project lines into this plane 
  //       (PHGeometron::projectLineIntoPlane).
  //

  PHLine projectedLine1 = geometry->projectLineIntoPlane(line1,xyPlane);
  PHLine projectedLine2 = geometry->projectLineIntoPlane(line2,xyPlane);
  
  cout << "10.  line1 projected into xy-plane = " << projectedLine1 << endl;
  cout << "    line2 projected into xy-plane = " << projectedLine2 << endl;
  cout << endl;

  //
  //   11. Create two reference frames (PHFrame).
  //

  PHFrame XYZ;   // the default is the standard xyz cartesian frame

  PHPoint originPrime(1,1,1);
  PHVector xAxis, yAxis;

  xAxis.setX(1);
  yAxis.setY(1);

  PHFrame UVW(originPrime,zAxis,xAxis,yAxis);
  
  cout << "11. xyz-frame: " << endl;
  XYZ.print();
  cout << endl;

  cout << "    uvw-frame: " <<endl;
  UVW.print();
  cout << endl;

  //
  //   12. Transform a line from one frame to another 
  //       (PHGeometron::transformLine).
  //

  PHLine transformedLine = geometry->transformLine(XYZ,line2,UVW);
  
  cout << "12. transformed line = " << transformedLine << endl;
  cout << endl;

  //
  //   13. Project the two lines onto a plane and find the intersection
  //       (PHGeometron::intersectionLineLineOnPlane).
  //

  PHPoint intersectionOnPlane;
  PHBoolean intersectOnPlane = geometry->intersectionLineLineOnPlane(line1,line2,xyPlane,intersectionOnPlane);

  if (intersectOnPlane)
    cout << "13. intersection of lines on plane = " << intersectionOnPlane << endl;
  else
    cout << "13. the lines do not intersect on plane" << endl;
  
  cout << endl;

  //
  //   14. Create a Polyline using the two lines (PHPolyline).
  //

  PHPolyLine polyline;

  PHPoint *first = new PHPoint(line1.getBasepoint());
  PHPoint *second = new PHPoint( (PHPoint)line1.getDirection() + *(first) );
  PHPoint *third = new PHPoint( (PHPoint)line2.getDirection() + *(second) );

  polyline.appendPoint(first);
  polyline.appendPoint(second);
  polyline.appendPoint(third);
 
  cout << "14. the polyline points created by the two lines:" << endl;

  for ( int i = 0; i < (int)polyline.numberOfPoints() ; i++) {
    cout << "  point " << (i+1) << " = " << *(polyline.getPoint(i)) << endl;
  }
  cout << endl;
  
  //
  //   15. Find the intersection of the Polyline and the plane in 9.
  //       (PHGeometron::intersectionPolylinePlane).
  
  PHPoint intersectionPolylinePlane;
  PHBoolean intersectPolylinePlane = geometry->intersectionPolyLinePlane(polyline,xyPlane,intersectionPolylinePlane);
  
  if (intersectPolylinePlane){
    cout << "15. the intersection of polyline with xy-plane = " << intersectionPolylinePlane << endl;
  }else{
    cout << "15. the polyline and xy-plane do not intersect " << endl;
  }
  cout << endl;
  cout << "End of program line.C " << endl;
  cout << endl;

}
