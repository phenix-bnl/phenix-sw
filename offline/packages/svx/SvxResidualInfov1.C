// ==================
// FILE: SvxResidualInfov1.C
// ==================

#include <SvxResidualInfov1.h>
#include <iostream>

using namespace std;

ClassImp(SvxResidualInfov1)



/**
 * @brief  The implementation v1 of SvxResidualInfo.
 *
 * Created on 4/14/2012 by Takashi Hachiya.
 */

void SvxResidualInfov1::copy(const SvxResidualInfo& info)
{
//  cout<<"SvxResidualInfov1::copy"<<endl;

  setClusterId(info.getClusterId());

  setdphi(info.getdphi());
  setdz  (info.getdz());
}

void SvxResidualInfov1::Reset(){
  m_id    = -9999;

  m_dphi  = -9999;
  m_dphi  = -9999;
}

void SvxResidualInfov1::print(){
  cout<<" "<<m_id;
  cout<<" ("<<m_dphi<<" "<<m_dz<<") ";
  cout<<endl;
}
