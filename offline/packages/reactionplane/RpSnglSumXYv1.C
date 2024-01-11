#include <RpSnglSumXYv1.h>

#include <cmath>
#include <iostream>

ClassImp(RpSnglSumXYv1)

using namespace std;

/**
 * @brief  The implement v1 class for a storage of reaction plane element 
 *
 * Created on 11/16/2011 by Takashi Hachiya.
 */


RpSnglSumXYv1::RpSnglSumXYv1() :
  m_weight(0), m_idcode(0), m_name("NONE")
{
  m_qvector[0] = -9999.0;
  m_qvector[1] = -9999.0;
}

RpSnglSumXYv1::RpSnglSumXYv1(const char *name, const unsigned int id, const float qx, const float qy, const float w):
  m_weight(w), m_idcode(id), m_name(name)
{
  m_qvector[0] = qx;
  m_qvector[1] = qy;
}

