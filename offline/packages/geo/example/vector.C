//
// This is a program example which will show how make use of the vector
// object. It shows how to calculate dot,cross product and angle between 2 vectors.
// How to transform a vector from one reference frame to an other
//

//  double   dot(const PHVector&,const PHVector&);
//  PHVector cross(const PHVector&,const PHVector&);
//  PHAngle  angle(const PHVector&, const PHVector&);
//  PHVector transformVector(const PHFrame&, const PHVector&, const PHFrame&);


#include <iostream.h>
#include <math.h>
#include "phool.h"
#include "PHGeometron.h"

main()
{
  //----------------------------------------------------------------------
  // Some easy operations with vectors
  //----------------------------------------------------------------------
 
  //
  // instantiate a PHGeometron
  //
  PHGeometron *geometry = PHGeometron::instance(); 
  //
  // Create two vectors. A vector  does not need to be normalized 
  // 
  
   PHVector vector1(0,2,5);
   PHVector vector2(1,3,0);

   // print out the vectors
   vector1.print();
   vector2.print();

   //-------------------------------------------------
   // calculate dot product in two different ways
   //-------------------------------------------------
   double dotProduct1 = geometry->dot(vector1,vector2); // first method
   double dotProduct2 = vector1.dot(vector2);           // second method

   cout << "Dot product (first  method):" << dotProduct1 << endl;
   cout << "Dot product (second method):" << dotProduct2 << endl;

   //-------------------------------------------------
   // calculate cross product in two different ways
   //-------------------------------------------------
   PHVector crossProduct1 = geometry->cross(vector1,vector2); // first method
   PHVector crossProduct2 = vector1.cross(vector2);           // second method

   cout << "Cross product (first  method):" << crossProduct1 << endl;
   cout << "Cross product (second method):" << crossProduct2 << endl;


   //-------------------------------------------------
   // calculate angle between to vector  in two different ways
   //-------------------------------------------------
   
   PHAngle angle1 = geometry->angle(vector1,vector2); // first method
   PHAngle angle2 = vector1.angle(vector2);           // second method

   cout << "Angle (first  method):" << angle1 << endl;
   cout << "Angle (second method):" << angle2 << endl;



   // -------------------------------------------------
   // Get a vector orthogonal to the given one
   //--------------------------------------------------

   PHVector orthogonal = vector1.orthogonal();

   cout << "Orthogonal Vector:" << orthogonal << endl;


   //-------------------------------------------------------------
   // Add , subtract two vector or multiply one with a constant
   //-------------------------------------------------------------

   double constant = 4;
   PHVector vectorAddition = vector1 + vector2;
   PHVector vectorSubtraction = vector1-vector2;
   PHVector vectorMultiplication = vector1*constant;
   
   cout << "addition       "<< vectorAddition << endl;
   cout << "subtraction    "<< vectorSubtraction<< endl;
   cout << "multiplication "<< vectorMultiplication  << endl;

 
   //-------------------------------------------------
   // vector length
   //------------------------------------------------

   cout << "Length of vector1 " << vector1.length() << endl;

   vector1.normalize();

   cout << "Normalized Vector "<< vector1 << endl;
   cout << "Length of vector1 after normalization "<< vector1.length() << endl;
  
   //------------------------------------------------
   // Transform vector from one frame to an other
   //----------------------------------------------

   
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
   
   PHVector transVector = geometry->transformVector(XYZ,vector2,newFrame);
   PHVector transBackVector = geometry->transformVector(newFrame,transVector,XYZ);

   cout << "Original vector "<< vector2<< endl;
   cout << "Transformed Vector " << transVector << endl;
   cout << "Transformed Back Vector "<< transBackVector << endl;

 

}

