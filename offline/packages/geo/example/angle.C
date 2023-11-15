//
// This is a program example which will show how make use of the angle class
//

#include <iostream.h>
#include <math.h>
#include "phool.h"
#include "PHGeometron.h"

main()
{
  //----------------------------------------------------------------------
  // Some functionality of the Angle class
  //----------------------------------------------------------------------

  PHAngle angle(2.7);

  cout << "Angle in radiant:" <<angle << endl;
  cout << "Angle in degree :" << angle.degree() << endl;

  PHAngle angle2(1.57);

  PHAngle sumAngle = angle + angle2;
  PHAngle subAngle = angle - angle2;
  cout << "Sum to angles" << endl;
  cout << "Angle:" << angle << "+" << angle2 << " = "<< sumAngle << endl;
  
  cout << "Subtract to angles" << endl;
  cout << "Angle:" << angle << "-" << angle2 << " = "<< subAngle << endl;


  cout << "Average Angle" << endl;

  PHAngle angleA(2.5);
  PHAngle angleB(-2.5);

  PHAngle aver = average(angleA,angleB);
  cout << "average : " << angleA << "and " << angleB << " = " << aver << endl;  
  
}
