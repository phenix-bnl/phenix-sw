#include "RpcTriggerMap.h"
#include "RPCFINALGEOM.h"
#include "RpcStrip_v2.h"
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <TMath.h>
#include <TLine.h>
#include <TVector3.h>

using namespace std;

void RpcTriggerMap::InitVal()
{
  setReferencePosition(0,0.,0.,0.,0.,0.,0.);
  setReferencePosition(1,0.,0.,0.,0.,0.,0.);

  //Set the strips to null
  fRad0 = -1;
  fRad1 = -1;
  fRad2 = -1;
  fCurrentStation = -1;
  fCurrentArm = -1;
  fCurrentOctant = -1;
  fCurrentHalfOctant = -1;

  //set all tdc channels to null
  fTDCCh0 = -1;
  fTDCCh1 = -1;
  fTDCCh2 = -1;
  fCurrentModule0 = -1;
  fCurrentModule1 = -1;
  fCurrentModule2 = -1;
}

RpcTriggerMap::~RpcTriggerMap()
{
  //if(strip) { delete strip; }//Doesn't like this
}

void RpcTriggerMap::setReferencePosition(int pos,
					 float fXbeg, float fYbeg, float fZbeg,
					 float fXend, float fYend, float fZend)
{
  PHPoint pbeg(fXbeg,fYbeg,fZbeg);
  PHPoint pend(fXend,fYend,fZend);
    
  setReferencePosition(pos,pbeg,pend);
}

void RpcTriggerMap::setReferencePosition(int pos, PHPoint pbeg, PHPoint pend)
{
  //Just in case, reset the strips to null
  fRad0 = -1;
  fRad1 = -1;
  fRad2 = -1;
  
  if(pos==0) {
    fRef0beg = pbeg;
    fRef0end = pend; }
  else {
    fRef1beg = pbeg;
    fRef1end = pend; }
}

void RpcTriggerMap::findNearestStrips(int fArm, int fStation, int fOctant)
{
  fCurrentArm = fArm;
  fCurrentStation = fStation;
  fCurrentOctant = fOctant;
  fCurrentHalfOctant = fOctant;  

  //Just in case, reset the strips to null
  fRad0 = -1;
  fRad1 = -1;
  fRad2 = -1;
  
  //Make a dummy geometry variable ... this is only to get the z position
  RPCFINALGEOM final(RPCFINALGEOM::ArmNumber(fArm),RPCFINALGEOM::StationNumber(fStation),0,0,RPCFINALGEOM::RadialSegment(0),17);
  
  //We scan over the length of the MuTrg strip (beg to end) in ntrials steps
  int ntrials=100;
  
  //For now, let's use the vertex, it's reasonable and is not too different
  //from the case when MuTrg1 and MuTrg3 are used alone...
  PHPoint pbeg = fRef1beg;
  PHPoint pend = fRef1end;
  PHPoint pmid = (pbeg+pend)*0.5;//not used as such, is reset see below
  PHPoint pvtx(0.,0.,0.);
  //if you want to use fRef1beg and fRef0beg, then you should
  // set pvtx = (fRef0beg+fRef0beg*0.5 or set up a second scan-like procedure

  bool kHalfOct=false;//is this position near the half-oct boundary??
  strip = new RpcStrip_v2();

  double myrpc_x(0),myrpc_y(0);
  
  for(int i=ntrials ; i>=0 ; i--) {//Inner to outer
    if(fRad0>0 && fRad1>0 && fRad2>0) {//if we have one, don't waste time
      break; }
    
    //set the reference along the path fRef1beg to fRef1end
    pmid=(pbeg*(i*1.0/ntrials)+pend*((ntrials-i)*1.0/ntrials));
    //pull out the x and y positions extrapolated onto the RPC plane
    double rpc_x = ((pmid.getX()-pvtx.getX())/(pmid.getZ()-pvtx.getZ()))*(final.GetMid().getZ()-pvtx.getZ())+pvtx.getX();
    double rpc_y = ((pmid.getY()-pvtx.getY())/(pmid.getZ()-pvtx.getZ()))*(final.GetMid().getZ()-pvtx.getZ())+pvtx.getY();
    
    TVector3 *v3 = new TVector3(rpc_x,rpc_y,final.GetMid().getZ());
    if(fabs(TMath::Sin(4.*v3->Phi()))<0.5) { //is nearer the half-oct boundary
      kHalfOct = true; }
    delete v3;

    if(fRad0<0) {
      strip->SetStrip(rpc_x,rpc_y,fArm,fStation,0);
      fCurrentHalfOctant = strip->GetHalfOctant();
      if(strip->strip>=32) {
	myrpc_x = rpc_x;
	myrpc_y = rpc_y;
	fRad0 = strip->strip; }
      if(strip->strip>0) {
	strip->SetStrip(rpc_x,rpc_y,fArm,fStation,0,fRad0);
        fRad0n = strip->strip;
	fRad0n = fRad0n + (fRad0n<32 ? 32:0); } }
    
    if(fRad1<0) {
      strip->SetStrip(rpc_x,rpc_y,fArm,fStation,1);
      fCurrentHalfOctant = strip->GetHalfOctant();
      if(strip->strip>=32) {
	fRad1 = strip->strip; }
      if(fRad1>0) {
	strip->SetStrip(rpc_x,rpc_y,fArm,fStation,1,fRad1);
	fRad1n = strip->strip;
	fRad1n = fRad1n + (fRad1n<32 ? 32:0); } }
    
    if(fRad2<0) {
      strip->SetStrip(rpc_x,rpc_y,fArm,fStation,2);
      fCurrentHalfOctant = strip->GetHalfOctant();
      if(strip->strip>=32) {
	fRad2 = strip->strip; }
      if(fRad2>0) {
	strip->SetStrip(rpc_x,rpc_y,fArm,fStation,2,fRad2);
	fRad2n = strip->strip;
	fRad2n = fRad2n + (fRad2n<32 ? 32:0); } }
    
  }//ntrials

  //  fCurrentHalfOctant = strip->GetHalfOctant();
  if(fCurrentHalfOctant>1) {
    cout << fCurrentHalfOctant << " " << myrpc_x << " " << myrpc_y << endl; }
  //  if(fStation==0) { cout << fRad0 << " " << fRad1 << endl; }


  //Now make sure that the half-octant boundary is always covered
  if(kHalfOct) {
    if(0 && kHalfOct) {
      if(fRad0<0) {
	fRad0 = RPCFINALGEOM::FirstStripOuterNumber[fStation][0]; }
      if(fRad1<0) {
	fRad1 = RPCFINALGEOM::FirstStripOuterNumber[fStation][1]; }
      if(fRad2<0) {
	fRad2 = RPCFINALGEOM::FirstStripOuterNumber[fStation][2]; } }
  } 
  if(!kHalfOct) {
    //Now make sure that the octant boundary is always covered
    if(0 && !kHalfOct) {
      if(fRad0<0) {
	fRad0 = RPCFINALGEOM::LastStripOuterNumber[fStation][0]; }
      if(fRad1<0) {
	fRad1 = RPCFINALGEOM::LastStripOuterNumber[fStation][1]; }
      if(fRad2<0) {
	fRad2 = RPCFINALGEOM::LastStripOuterNumber[fStation][2]; } }
  }
  RPCFINALGEOM finaler(RPCFINALGEOM::ArmNumber(fArm),RPCFINALGEOM::StationNumber(fStation),strip->GetOctant(),strip->GetHalfOctant(),RPCFINALGEOM::RadialSegment(2),fRad2);
  
  setTDCChannels();
  
  if(strip) delete strip;
}

bool RpcTriggerMap::checkBoundary()
{
  bool kCrossBoundary = false;
  int strips[3] = {fRad0,fRad1,fRad2};

  //If we are on the edge, shift inward by one to make sure that the
  //edges are well covered ... if not, then we might miss something...
  //several strips have an edge along each boundary
  for(int i=0 ; i<3 ; i++) {
    if(strips[i]==RPCFINALGEOM::FirstStripOuterNumber[fCurrentStation][i]) {
      kCrossBoundary=true;
      strips[i] += 1; }
    if(strips[i]==RPCFINALGEOM::LastStripOuterNumber[fCurrentStation][i])  {
      strips[i] -= 1; } }  
  
  fRad0 = strips[0];
  fRad1 = strips[1];
  fRad2 = strips[2];
  
  setTDCChannels();
  
  return kCrossBoundary;
}

bool RpcTriggerMap::checkBoundary2Strip()
{
  bool kCrossBoundary = false;
  int strips[3]  = {fRad0,fRad1,fRad2};
  int stripsn[3] = {fRad0n,fRad1n,fRad2n};

  //If we are on the edge, define the "next nearest" to always
  //be the next strip, not outside the bounds of the detector.
  for(int i=0 ; i<3 ; i++) {
    if(strips[i]==RPCFINALGEOM::FirstStripOuterNumber[fCurrentStation][i]) {
      kCrossBoundary=true;
      stripsn[i] = strips[i]+1; }
    if(strips[i]==RPCFINALGEOM::LastStripOuterNumber[fCurrentStation][i])  {
      stripsn[i] = strips[i]-1; } }  
  
  fRad0n = stripsn[0];
  fRad1n = stripsn[1];
  fRad2n = stripsn[2];
   
  setTDCChannels();
  
  return kCrossBoundary;
}

RpcStrip_v2 *RpcTriggerMap::getStrip(int fRadSeg)
{
  int strips[3] = {fRad0,fRad1,fRad2};

  //if(strip) { delete strip; }

  strip = new RpcStrip_v2();
  strip->SetStrip(fCurrentArm,fCurrentStation,fCurrentOctant,fCurrentHalfOctant,fRadSeg,strips[fRadSeg]);

  return strip;
}

RpcStrip_v2 *RpcTriggerMap::getStripn(int fRadSeg)
{
  int strips[3] = {fRad0n,fRad1n,fRad2n};

  //if(strip) { delete strip; }

  stripn = new RpcStrip_v2();
  stripn->SetStrip(fCurrentArm,fCurrentStation,fCurrentOctant,fCurrentHalfOctant,fRadSeg,strips[fRadSeg]);

  return strip;
}

void RpcTriggerMap::setTDCChannels()
{
  if(fCurrentStation==2) {//RPC3
    //Convert the strip number into TDC Channel number
    fTDCCh0 = (47-fRad0)+32*(fRad0/48);
    fTDCCh1 = (47-fRad1)+32*(fRad1/48);
    fTDCCh2 = (47-fRad2)+32*(fRad2/48);
    
    //Convert the strip number into TDC Channel number
    fTDCCh0n = (47-fRad0n)+32*(fRad0n/48);
    fTDCCh1n = (47-fRad1n)+32*(fRad1n/48);
    fTDCCh2n = (47-fRad2n)+32*(fRad2n/48);
    
    //Set the current module number for each radial segment
    fCurrentModule0 = 0+3*fCurrentHalfOctant;
    fCurrentModule1 = 1+3*fCurrentHalfOctant;
    fCurrentModule2 = 2+3*fCurrentHalfOctant; }//end RPC3
  else {//is fCurrentStation==0 (RPC1)
    //Convert the strip number into TDC Channel number
    if(fCurrentHalfOctant==0) {//is RIGHT
      if(fRad0>=32 && fRad0<=47) { fTDCCh0 = fRad0+16; }
      if(fRad0>=48 && fRad0<=63) { fTDCCh0 = fRad0-16; }
      
      if(fRad1>=32 && fRad1<=47) { fTDCCh1 = 51-fRad1; }
      if(fRad1>=48 && fRad1<=63) { fTDCCh1 = 75-fRad1; }
      
      fTDCCh2 = -1; }
    
    else if(fCurrentHalfOctant==1) {//is LEFT
      if(fRad0>=32 && fRad0<=47) { fTDCCh0 = 79 -fRad0; }
      if(fRad0>=48 && fRad0<=63) { fTDCCh0 = 111-fRad0; }
      
      if(fRad1>=32 && fRad1<=47) { fTDCCh1 = fRad1-20; }
      if(fRad1>=48 && fRad1<=63) { fTDCCh1 = fRad1-44; }
      
      fTDCCh2 = -1; }
    
    if(fCurrentHalfOctant==0) {//is RIGHT
      if(fRad0n>=32 && fRad0n<=47) { fTDCCh0n = fRad0n+16; }
      if(fRad0n>=48 && fRad0n<=63) { fTDCCh0n = fRad0n-16; }
      
      if(fRad1n>=32 && fRad1n<=47) { fTDCCh1n = 51-fRad1n; }
      if(fRad1n>=48 && fRad1n<=63) { fTDCCh1n = 75-fRad1n; }
      
      fTDCCh2n = -1; }
    
    else if(fCurrentHalfOctant==1) {//is LEFT
      if(fRad0n>=32 && fRad0n<=47) { fTDCCh0n = 79 -fRad0n; }
      if(fRad0n>=48 && fRad0n<=63) { fTDCCh0n = 111-fRad0n; }
      
      if(fRad1n>=32 && fRad1n<=47) { fTDCCh1n = fRad1n-20; }
      if(fRad1n>=48 && fRad1n<=63) { fTDCCh1n = fRad1n+44; }
      
      fTDCCh2n = -1; }
    
    //Set the current module number for each radial segment
    fCurrentModule0 = fCurrentHalfOctant+1;
    fCurrentModule1 = fCurrentHalfOctant+1;
    fCurrentModule2 = -1; }//end RPC1
}

void RpcTriggerMap::setGeom()
{
  //This function is used for the conversion of TDC channels into 
  //geometry channels ONLY.  It is used to test the mapping.
  if(fCurrentStation==2) { //RPC3
    fRad0 = (47-fTDCCh0)+32*(fTDCCh0/16);
    fRad1 = (47-fTDCCh1)+32*(fTDCCh1/16);
    fRad2 = (47-fTDCCh2)+32*(fTDCCh2/16);
    
    fCurrentHalfOctant = fCurrentModule0/3; }//end RPC3
  else if(fCurrentStation==0) { //RPC1
    if(fCurrentModule0==1) {
      if(fTDCCh0>=0  && fTDCCh0<=15) { fRad0 = 51 -fTDCCh0; }
      if(fTDCCh0>=16 && fTDCCh0<=31) { fRad0 = 75 -fTDCCh0; }
      if(fTDCCh0>=32 && fTDCCh0<=47) { fRad0 = 111-fTDCCh0; }
      if(fTDCCh0>=48 && fTDCCh0<=63) { fRad0 = 44 +fTDCCh0; }
      fCurrentHalfOctant = fCurrentModule0-1; }
    if(fCurrentModule0==2) {
      if(fTDCCh0>=0  && fTDCCh0<=15) { fRad0 = fTDCCh0 - 44; }
      if(fTDCCh0>=16 && fTDCCh0<=31) { fRad0 = fTDCCh0 + 20; }
      if(fTDCCh0>=32 && fTDCCh0<=47) { fRad0 = 79 - fTDCCh0; }
      if(fTDCCh0>=48 && fTDCCh0<=63) { fRad0 = 111 +fTDCCh0; }
      fCurrentHalfOctant = fCurrentModule0-1; }

    
  }//end RPC1
}

void RpcTriggerMap::set_strip(int arm, int sta, int oct, int hoct, int rad0, int rad1, int rad2)
{
  //This function is used to test the mapping.
  
  fRad0 = rad0;
  fRad1 = rad1;
  fRad2 = rad2;
  
  fCurrentStation = sta;
  fCurrentArm = arm;
  fCurrentOctant = oct;
  fCurrentHalfOctant = hoct;  
  
  setTDCChannels();  
}

void RpcTriggerMap::set_tdc(int arm, int sta, int oct, int mod0, int mod1, int mod2, int tdc0, int tdc1, int tdc2)
{
  //This function is used to test the mapping.

  fTDCCh0 = tdc0;
  fTDCCh1 = tdc1;
  fTDCCh2 = tdc2;
  fCurrentModule0 = mod0;
  fCurrentModule1 = mod1;
  fCurrentModule2 = mod2;

  setGeom();
}

void RpcTriggerMap::testGeomTDCMapping()
{
  //This function is used to test the mapping.

  cout << "Checking TDC Module Map: RPC3" << endl;
  for(int ihoct=0 ; ihoct<2 ; ihoct++) {
    set_strip(0,2,0,ihoct,40,40,40);
    cout << "     half octant " << ihoct << " has TDC modules "
	 << fCurrentModule0 << " "
	 << fCurrentModule1 << " "
	 << fCurrentModule2 << endl; }
  cout << endl;
  
  cout << "Checking TDC Channel Map: " << endl;
  for(int i=32 ; i<64 ; i++) {
    set_strip(0,2,0,0,i,i,i);
    if(i==32) { cout << "     "; }
    cout << i << "(" << fTDCCh0 << ") ";
    if(fTDCCh0<10) { cout << " "; }
    if(fTDCCh0==0) { cout << endl << "     "; } }
  cout << endl;
  cout << endl;

  cout << "Checking RPC3 Geom Map: " << endl;
  for(int i=0 ; i<6 ; i++) {
    set_tdc(0,2,0,i,i+1,i+2,10,10,10);
    cout << "     module " << i << " is half octant "
	 << fCurrentHalfOctant << endl; }
  
  cout << " ************************************* " << endl;
  
  cout << "Checking TDC Module Map: RPC1" << endl;
  for(int ihoct=0 ; ihoct<2 ; ihoct++) {
    set_strip(0,0,0,ihoct,40,40,40);
    cout << "     half octant " << ihoct << " has TDC modules "
	 << fCurrentModule0 << " "
	 << fCurrentModule1 << " "
	 << fCurrentModule2 << endl; }
  cout << endl;
  
  cout << "Checking TDC Channel Map: RadSeg 1(right)" << endl;
  for(int i=32 ; i<64 ; i++) {
    set_strip(0,0,0,0,i,i,i);
    if(i==32) { cout << "     "; }
    cout << i << "(" << fTDCCh1 << ") ";
    if(fTDCCh0<10) { cout << " "; }
    if(fTDCCh0==0) { cout << endl << "     "; } }
  cout << endl;
  cout << "Checking TDC Channel Map: RadSeg 0(right)" << endl;
  for(int i=32 ; i<64 ; i++) {
    set_strip(0,0,0,0,i,i,i);
    if(i==32) { cout << "     "; }
    cout << i << "(" << fTDCCh0 << ") ";
    if(fTDCCh0<10) { cout << " "; }
    if(fTDCCh0==0) { cout << endl << "     "; } }
  cout << endl;
  cout << endl;
  cout << "Checking TDC Channel Map: RadSeg 1(left)" << endl;
  for(int i=32 ; i<64 ; i++) {
    set_strip(0,0,0,1,i,i,i);
    if(i==32) { cout << "     "; }
    cout << i << "(" << fTDCCh1 << ") ";
    if(fTDCCh0<10) { cout << " "; }
    if(fTDCCh0==0) { cout << endl << "     "; } }
  cout << endl;
  cout << "Checking TDC Channel Map: RadSeg 0(left)" << endl;
  for(int i=32 ; i<64 ; i++) {
    set_strip(0,0,0,1,i,i,i);
    if(i==32) { cout << "     "; }
    cout << i << "(" << fTDCCh0 << ") ";
    if(fTDCCh0<10) { cout << " "; }
    if(fTDCCh0==0) { cout << endl << "     "; } }
  cout << endl;
  cout << endl;

  cout << "Checking RPC1 Geom Map: " << endl;
  for(int i=0 ; i<2 ; i++) {
    set_tdc(0,0,0,i,i+1,i+2,10,10,10);
    cout << "     module " << i << " is half octant "
	 << fCurrentHalfOctant << endl; }
 
}
