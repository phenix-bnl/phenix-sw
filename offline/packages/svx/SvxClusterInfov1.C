// ==================
// FILE: SvxClusterInfov1.C
// ==================

#include <SvxClusterInfov1.h>
#include <iostream>

using namespace std;

ClassImp(SvxClusterInfov1)



/**
 * @brief  The implementation v1 of SvxClusterInfo.
 *
 * Created on 11/27/2011 by Takashi Hachiya.
 */

void SvxClusterInfov1::copy(const SvxClusterInfo& info)
{
//  cout<<"SvxClusterInfov1::copy"<<endl;

  setClusterId(    info.getClusterId());
  setLayer(        info.getLayer());
  setLadder(       info.getLadder());
  setSensor(       info.getSensor());
  setSize(         info.getSize());
  setEdgeFlag(     info.getEdgeFlag());
  setCircumference(info.getCircumference());
  setAmbiguous(    info.getAmbiguous());

  setPosition(info.getPosition(0),info.getPosition(1), info.getPosition(2));
  setXZSize(  info.getXZSize(0),  info.getXZSize(1));
  setAdc(     info.getAdc(0),     info.getAdc(1));
}

void SvxClusterInfov1::Reset(){
  m_id            = -9999;
  m_layer         = -127;
  m_ladder        = -127;
  m_sensor        = -127;
  m_size          = -9999;
  m_edgeflag      = -9999;
  m_circumference = -9999;
  m_ambiguous     = -9999;

  for(int i=0; i<3; i++){ m_global_pos[i] = -9999.; }
  for(int i=0; i<2; i++){ 
    m_xz_size[i] = -9999;
    m_adc[i]     = -9999;  
  }
}

void SvxClusterInfov1::print(){
  cout<<" "<<m_id;
  cout<<" " <<(int)m_layer;
  cout<<" ("<<m_global_pos[0]<<" "<<m_global_pos[1]<<" "<<m_global_pos[2]<<")";
  cout<<" " <<m_size;
  cout<<" ("<<m_adc[0]<<" "<<m_adc[1]<<") ";
  cout<<endl;
}
