// ====================
// FILE: SvxCentralTrackv5.C
// ====================

#include "SvxCentralTrackv5.h"

ClassImp(SvxCentralTrackv5)

using namespace std;

SvxCentralTrackv5::SvxCentralTrackv5()
{  
  Reset();
}

void SvxCentralTrackv5::Reset()
{
  m_dchindex = -9999;

  m_ninfo = 0;
  for(int i=0; i<MAXCLUSTERS; i++){
    m_vinfo[i] = SvxClusterInfov3();
  }
  m_hitpattern = 0;

  for(int icoor=0; icoor<3; icoor++){
    m_mom3AtDCA[icoor]       = -9999.;
    m_mom3ErrAtDCA[icoor]    = -9999.;
    m_closestApproach[icoor] = -9999.;
  }

  m_quality   = -9999.;
  m_DCA2D     = -9999.;
  m_DCA2DErr  = -9999.;
  m_DCAZ      = -9999.;
  m_DCA2D0    = -9999.;
  m_primary   = true;
  m_dEdX[0]   = -9999.;
  m_dEdX[1]   = -9999.;
  m_vtxId     = -9999;
  m_chisquare = -9999.;
  m_ndf       = -9999.;
  m_chisquare2 = -9999.;

  m_chisquare_dphi = -9999.;
  m_ndf_dphi       = -9999.;
  m_chisquare_dz = -9999.;
  m_ndf_dz       = -9999.;

  m_unique = -9999;

  m_rotatedAngle[0] = 0.0;
  m_rotatedAngle[1] = 0.0;

  for(int ilay=0; ilay<MAXRESIDUALLAYER; ilay++){
    m_nresidual[ilay] = 0;
    for(int i=0; i<MAXRESIDUALS; i++){
      m_vresidual[ilay][i] = SvxResidualInfov1();
    }
  }
}

void  SvxCentralTrackv5::addClusterInfo(SvxClusterInfo* cinfo){
  SvxClusterInfov3* cinfov3 = dynamic_cast<SvxClusterInfov3 *>(cinfo);
  if(cinfov3!=NULL) {
    if(m_ninfo<MAXCLUSTERS){
      int sublayer = cinfov3->get_sublayer();
      if(0<=sublayer&&sublayer<8) m_hitpattern |= (0x1<<sublayer);
      else                     {cout<<"unknown sublayer : "<<sublayer<<endl; return;}
//      cout<<"DEBUG : "<<sublayer<<" "<<(int)cinfov3->getLayer()<<" "<<(int)cinfov3->getLadder()<<endl;

      m_vinfo[m_ninfo] = *cinfov3;
      m_ninfo++;
    }
    else {
      cout<<" SvxCentralTrackv5::addClusterInfo : N associated tracks exceeds "<<MAXCLUSTERS<<endl;
    }
  } 
  else {
    cout<<"SvxCentralTrackv5::addClusterInfo failed to add"<<endl;
  }
}

SvxClusterInfo* SvxCentralTrackv5::getClusterInfo(int hit){
  if(isValidHit(hit)) {
    return &(m_vinfo[hit]);
  }
  return NULL;
} 

short SvxCentralTrackv5::getNhits(){
  return (short)m_ninfo;
}

short SvxCentralTrackv5::getNhits(int layer){
  short int nhit = 0;
  if(0<=layer&&layer<2) 
  {
    if( ((m_hitpattern>>layer)&0x1)==1) nhit=1; 
  }
  else if(2<=layer&&layer<8) 
  {
    int offset = (2<=layer&&layer<5) ? 2 : 5;
    for(int i=0; i<3; i++)
    {
      if( ( (m_hitpattern>>(i+offset))&0x1)==1 ) nhit++; 
    }
  }

  return (short)nhit;
}


float SvxCentralTrackv5::getClosestApproach(int coor){
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

float SvxCentralTrackv5::get3MomentumAtPrimaryVertex(int coor){
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

float SvxCentralTrackv5::get3MomErrorAtPrimaryVertex(int coor){
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

void SvxCentralTrackv5::addSecondHit(int layer, SvxResidualInfo* cinfo){
  if(!isValidResidualLayer(layer)){
    cerr<<"SvxCentralTrackRecov5::addSecondHit InvaridLayer : "<<layer<<endl;
    return;
  }


  SvxResidualInfov1* cinfov1 = dynamic_cast<SvxResidualInfov1 *>(cinfo);
  if(cinfov1!=NULL) {
    if(m_nresidual[layer]<MAXRESIDUALS){
      m_vresidual[layer][m_nresidual[layer]] = *cinfov1;
      m_nresidual[layer]++;
    }
  } 
  else {
    cout<<"SvxCentralTrackv5::addResidualInfo failed to add"<<endl;
  }
}

SvxResidualInfo* SvxCentralTrackv5::getSecondHit(int layer, int hit){
 return (isValidResidualLayer(layer)) ? 
          (&m_vresidual[layer][hit]) : NULL;
}



void SvxCentralTrackv5::print()
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

  cout<<" hitpatter 0x"<<hex<<m_hitpattern<<dec<<endl;

  cout<<" Nresidual    "<<m_nresidual<<endl;
  for(int ilay=0; ilay<MAXRESIDUALLAYER; ilay++){
    for(int i=0; i<m_nresidual[ilay]; ++i){ m_vresidual[ilay][i].print(); }
  }

}
