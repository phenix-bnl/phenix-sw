#include <ReactionPlaneSngl.h>

#include <cmath>
#include <iostream>

ClassImp(ReactionPlaneSngl)

using namespace std;

/**
 * @brief  The abstract class for a storage of reaction plane.
 *
 * Created on /  /2012 by Hiroshi Nakagomi 
 */

ReactionPlaneSngl* ReactionPlaneSngl::clone() const 
{
  cout << "ReactionPlaneSngl::clone not implemented by daughter class" << endl;
  return NULL;
}


ReactionPlaneSngl& ReactionPlaneSngl::operator=(const ReactionPlaneSngl& source)
{
  if (this != &source)
    {
      cout <<GetPsi() << endl;
      SetPsi(source.GetPsi());
      SetName(source.GetName());
      SetIdCode(source.GetIdCode());
    }
  return *this;
}
