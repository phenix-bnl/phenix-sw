#include <ReactionPlaneSnglv1.h>

#include <cmath>
#include <iostream>

ClassImp(ReactionPlaneSnglv1)

using namespace std;

/**
 * @brief  The implement v1 class for a storage of reaction plane 
 *
 * Created on / /2012 by Hiroshi Nakagomi.
 */


ReactionPlaneSnglv1::ReactionPlaneSnglv1() :
  m_idcode(0), m_name("NONE")
{
  m_psi = -9999.0;
}

ReactionPlaneSnglv1::ReactionPlaneSnglv1(const char *name, const unsigned int id, const float rp):
  m_idcode(id), m_name(name)
{
  m_psi = rp;
}

