// ====================
// FILE: SvxCentralTrackv8.C
// ====================

#include "SvxCentralTrackv8.h"

ClassImp(SvxCentralTrackv8)

using namespace std;

SvxCentralTrackv8::SvxCentralTrackv8()
{  
  Reset();
}

void SvxCentralTrackv8::Reset()
{
  m_dchindex = -9999;

  m_ninfo = 0;
  for(int i=0; i<MAXCLUSTERS; i++) {
    m_vinfo[i] = SvxClusterInfov5();
  }
  m_hitpattern = 0;

  for (int ily=0; ily<4; ily++) {
    m_live_percentage[ily] = -1;
  }

  for(int icoor=0; icoor<3; icoor++) {
    m_mom3AtDCA[icoor]       = -9999.;
//    m_mom3ErrAtDCA[icoor]    = -9999.;
    m_closestApproach[icoor] = -9999.;
  }

  m_quality   = -9999.;
  m_linkquality = -9999.;
  m_linkscore = -9999.;
  m_DCA2D     = -9999.;
  m_DCAZ      = -9999.;
  m_primary   = true;
  m_dEdX[0]   = -9999.;
  m_dEdX[1]   = -9999.;
  m_vtxId     = -9999;
  m_ndf       = -9999.;
  m_chisquare = -9999.;
  m_chisquare_dphi = -9999.;
  m_chisquare_dz = -9999.;
  m_chisquare2= -9999.;

  m_unique = -9999;

  m_chisquareNoCNT = -9999.;
  m_ndfNoCNT       = -9999;
  m_DCA2DNoCNT     = -9999.;
  m_DCAZNoCNT      = -9999.;


  m_rotatedAngle[0] = 0.0;
  m_rotatedAngle[1] = 0.0;
}

void  SvxCentralTrackv8::addClusterInfo(SvxClusterInfo* cinfo) {
  SvxClusterInfov5* cinfov5 = dynamic_cast<SvxClusterInfov5 *>(cinfo);
  if(cinfov5!=NULL) {
    if(m_ninfo<MAXCLUSTERS) {
      int sublayer = cinfov5->get_sublayer();
      if(0<=sublayer&&sublayer<8) m_hitpattern |= (0x1<<sublayer);
      else                     {cout<<"unknown sublayer : "<<sublayer<<endl; return;}
//      cout<<"DEBUG : "<<sublayer<<" "<<(int)cinfov3->getLayer()<<" "<<(int)cinfov3->getLadder()<<endl;

      m_vinfo[m_ninfo] = *cinfov5;
      m_ninfo++;
    }
    else {
      cout<<" SvxCentralTrackv8::addClusterInfo : N associated tracks exceeds "<<MAXCLUSTERS<<endl;
    }
  } 
  else {
    cout<<"SvxCentralTrackv8::addClusterInfo failed to add"<<endl;
  }
}

SvxClusterInfo* SvxCentralTrackv8::getClusterInfo(int hit) {
  if(isValidHit(hit)) {
    return &(m_vinfo[hit]);
  }
  return NULL;
}

void SvxCentralTrackv8::setLivePercentage(int layer, float livepercent)
{
  if (layer < 0 || layer > 3) {
    cerr << PHWHERE << "Invalid Layer: " << layer << endl;
    return;
  }

  if (livepercent > 100) { // But allow values < 0 (no hit in this layer)
    cerr << PHWHERE << "Invalid percentage value: " << livepercent << endl;
    return;
  }

  m_live_percentage[layer] = livepercent;
}

float SvxCentralTrackv8::getLivePercentage(int layer)
{
  if (layer < 0 || layer > 3) {
    cerr << PHWHERE << "Invalid Layer: " << layer << endl;
    return -1;
  }
  
  return m_live_percentage[layer];
}


short SvxCentralTrackv8::getNhits() {
  return (short)m_ninfo;
}

short SvxCentralTrackv8::getNhits(int sublayer) {
  short int nhit = 0;
  if(0<=sublayer&&sublayer<8) 
  {
    if( ((m_hitpattern>>sublayer)&0x1)==1) nhit=1; 
  }

  return (short)nhit;
}


float SvxCentralTrackv8::getClosestApproach(int coor) {
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

float SvxCentralTrackv8::get3MomentumAtPrimaryVertex(int coor) {
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

//--float SvxCentralTrackv8::get3MomErrorAtPrimaryVertex(int coor) {
//--  if(isValidCoordinate(coor)) {
//--    return m_mom3ErrAtDCA[coor];
//--  }
//--  else { 
//--    cout<<"get3MomErrorAtPrimaryVertex:: Invalid argumet : ";
//--    cout<<"coor="     <<coor<<", ";
//--    cout<<endl;
//--    return -9999;
//--  }
//--}

void SvxCentralTrackv8::print()
{
  cout<<" DchIndex : "<<m_dchindex<<endl;

  cout<<" P(x,y,z) : ";
  for(int i=0; i<3; i++) {
    cout<<m_mom3AtDCA[i]<<" ";
  }
  cout<<endl;

  cout<<" DCA_pos  : ";
  for(int i=0; i<3; i++) {
    cout<<m_closestApproach[i]<<" ";
  }
  cout<<endl;

  cout<<" Quality   : "<<m_quality<<endl;
  cout<<" DCA2D,DCAZ: "<<m_DCA2D<<" "<<m_DCAZ<<endl; 
  cout<<" Primary   : "<<m_primary<<endl;
  cout<<" dEdx      : "<<m_dEdX[0]<<" "<<m_dEdX[1]<<endl;
  cout<<" VtxId     : "<<m_vtxId<<endl;
  cout<<" Chi2/NDF  : "<<m_chisquare<<" / "<<m_ndf<<endl;

  cout<<" Ncls    "<<m_ninfo<<endl;
  for(int i=0; i<m_ninfo; ++i) { m_vinfo[i].print(); }

  cout<<" hitpatter 0x"<<hex<<m_hitpattern<<dec<<endl;

  cout<<" Link Quality : "<<m_linkquality<<endl;
  cout<<" Link Score   : "<<m_linkscore<<endl;
  for(int ilay=0; ilay<4; ilay++) {
    cout<<" Live Frac B"<<ilay<<" : "<<m_live_percentage[ilay]<<endl;
  }

  cout<<" DCA2DNoCNT, DCAZNoCNT : "<<m_DCA2DNoCNT<<", "<<m_DCAZNoCNT<<endl; 
  cout<<" Chi2NoCNT/NDFNoCNT   : " <<m_chisquareNoCNT<<" / "<<m_ndfNoCNT<<endl;
}
