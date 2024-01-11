// ====================
// FILE: SvxCentralTrackv3.C
// ====================

#include "SvxCentralTrackv3.h"

ClassImp(SvxCentralTrackv3)

using namespace std;

SvxCentralTrackv3::SvxCentralTrackv3()
{  
  Reset();
}

void SvxCentralTrackv3::Reset()
{
  m_dchindex = -9999;

  m_ninfo = 0;
  for(int i=0; i<MAXCLUSTERS; i++){
    m_vinfo[i] = SvxClusterInfov2();
  }

  for(int icoor=0; icoor<3; icoor++){
    m_mom3AtDCA[icoor]       = -9999.;
    m_mom3ErrAtDCA[icoor]    = -9999.;
    m_closestApproach[icoor] = -9999.;
  }

  m_quality   = -9999.;
  m_DCA2D     = -9999.;
  m_DCA2DErr  = -9999.;
  m_primary   = true;
  m_dEdX[0]   = -9999.;
  m_dEdX[1]   = -9999.;
  m_vtxId     = -9999;
  m_chisquare = -9999.;
  m_ndf       = -9999.;

  m_unique = -9999;

  m_rotationAngle[0] = 0.0;
  m_rotationAngle[1] = 0.0;
}

void  SvxCentralTrackv3::addClusterInfo(SvxClusterInfo* cinfo){
  SvxClusterInfov2* cinfov2 = dynamic_cast<SvxClusterInfov2 *>(cinfo);
  if(cinfov2!=NULL) {
    if(m_ninfo<MAXCLUSTERS){
      m_vinfo[m_ninfo] = *cinfov2;
      m_ninfo++;
    }
    else {
      cout<<" SvxCentralTrackv3::addClusterInfo : N associated tracks exceeds "<<MAXCLUSTERS<<endl;
    }
  } 
  else {
    cout<<"SvxCentralTrackv3::addClusterInfo failed to add"<<endl;
  }
}

SvxClusterInfo* SvxCentralTrackv3::getClusterInfo(int hit){
  if(isValidHit(hit)) {
    return &(m_vinfo[hit]);
  }
  return NULL;
} 

short SvxCentralTrackv3::getNhits(){
  return (short)m_ninfo;
}


float SvxCentralTrackv3::getClosestApproach(int coor){
  if(isValidCoordinate(coor)) {
    return m_closestApproach[coor];
  }
  else { 
    cout<<"getClosestApproach:: Invalid argumet : ";
    cout<<"coor="     <<coor<<", ";
    cout<<endl;
    return -9999;
  }
}

float SvxCentralTrackv3::get3MomentumAtPrimaryVertex(int coor){
  if(isValidCoordinate(coor)) {
    return m_mom3AtDCA[coor];
  }
  else { 
    cout<<"get3MomentumAtPrimaryVertex:: Invalid argumet : ";
    cout<<"coor="     <<coor<<", ";
    cout<<endl;
    return -9999;
  }
}

float SvxCentralTrackv3::get3MomErrorAtPrimaryVertex(int coor){
  if(isValidCoordinate(coor)) {
    return m_mom3ErrAtDCA[coor];
  }
  else { 
    cout<<"get3MomErrorAtPrimaryVertex:: Invalid argumet : ";
    cout<<"coor="     <<coor<<", ";
    cout<<endl;
    return -9999;
  }
}

void SvxCentralTrackv3::print()
{
  cout<<" DchIndex : "<<m_dchindex<<endl;

  cout<<" P(x,y,z) : ";
  for(int i=0; i<3; i++){
    cout<<m_mom3AtDCA[i]<<" ("<<m_mom3ErrAtDCA[i]<<") ";
  }
  cout<<endl;

  cout<<" DCA_pos  : ";
  for(int i=0; i<3; i++){
    cout<<m_closestApproach[i]<<" ";
  }
  cout<<endl;

  cout<<" Quality  : "<<m_quality<<endl;
  cout<<" DCA2D    : "<<m_DCA2D<<" +- "<<m_DCA2DErr<<endl; 
  cout<<" Primary  : "<<m_primary<<endl;
  cout<<" dEdx     : "<<m_dEdX[0]<<" "<<m_dEdX[1]<<endl;
  cout<<" VtxId    : "<<m_vtxId<<endl;
  cout<<" Chi2/NDF : "<<m_chisquare<<" / "<<m_ndf<<endl;

  cout<<" Ncls    "<<m_ninfo<<endl;
  for(int i=0; i<m_ninfo; ++i){ m_vinfo[i].print(); }

}
