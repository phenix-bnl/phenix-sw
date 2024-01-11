#include <RpSnglSumXY.h>

#include <cmath>
#include <iostream>

ClassImp(RpSnglSumXY)

using namespace std;

/**
 * @brief  The abstract class for a storage of reaction plane element 
 *
 * Created on 11/16/2011 by Takashi Hachiya.
 */

RpSnglSumXY* RpSnglSumXY::clone() const 
{
  cout << "RpSnglSumXY::clone not implemented by daughter class" << endl;
  return NULL;
}


RpSnglSumXY& RpSnglSumXY::operator=(const RpSnglSumXY& source)
{
  if (this != &source)
    {
      for(int i=0; i<2; i++)
        {
          QVector(i, source.QVector(i));
        }
      Weight(source.Weight());
      Name(source.Name());
      IdCode(source.IdCode());
    }
  return *this;
}
