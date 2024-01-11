#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include "SvxVertexReco.h"
#include "SvxClusterListv3.h"
#include "SvxSegmentListv1.h"
#include "VtxOutv7.h"
#include "CglTrack.h"
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHIODataNode.h>
#include <getClass.h>
#include <iostream>
#include <math.h>

using namespace std;


SvxVertexReco::SvxVertexReco()
{
  maxtracks=300;
  verbosity=0;
}


void SvxVertexReco::sortHighMom()
{
  
//   cout<<"nSeg = "<<d_segment->get_nSegments()<<endl;
  
  highmomlist.clear();
  if(d_segment->get_nSegments()<2)
  {
    return;
  }
//   cout<<"passed"<<endl;
  
  
  bool inserted=false;
  
  vector<int>::iterator iter;
  
  highmomlist.push_back(0);
  
  int n=d_segment->get_nSegments();
  
//   if(n>50)
//   {
//     maxtracks = 50 + (int)(sqrt((float)n - 49.99));
//   }
  
  for(int trk=1;trk<n;trk++)
  {
    inserted=false;
    if(highmomlist.size()<maxtracks)
    {
      iter=highmomlist.begin();
      for(unsigned int i=0;i<highmomlist.size();i++)
      {
        if(TMath::Abs(d_segment->get_segment(trk)->getMomentum())>TMath::Abs(d_segment->get_segment(highmomlist.at(i))->getMomentum()))
        {
          highmomlist.insert(iter, trk);
          inserted=true;
          break;
        }
        iter++;
      }
      if(inserted==false)
      {
        highmomlist.push_back(trk);
      }
    }
    else
    {
      iter=highmomlist.begin();
      for(unsigned int i=0;i<highmomlist.size();i++)
      {
        if(TMath::Abs(d_segment->get_segment(trk)->getMomentum())>TMath::Abs(d_segment->get_segment(highmomlist.at(i))->getMomentum()))
        {
          highmomlist.insert(iter, trk);
          highmomlist.pop_back();
          break;
        }
        iter++;
      }
    }
  }
}


int SvxVertexReco::process_event(PHCompositeNode *topNode)
{
  
/*
  SvxClusterList       *d_cluster=0;           
  PHTypedNodeIterator<SvxClusterList> clusteriter(topNode);
  PHIODataNode<SvxClusterList> *SvxClusterListNode = clusteriter.find("SvxClusterList");
  if (!SvxClusterListNode) { cerr << PHWHERE << " ERROR: Can't find SvxClusterList." << endl;  return EVENT_OK; }
  else { d_cluster = (SvxClusterList*)SvxClusterListNode->getData(); }
  
  d_segment=0;           
  PHTypedNodeIterator<SvxSegmentList> segmentiter(topNode);
  PHIODataNode<SvxSegmentList> *SvxSegmentListNode = segmentiter.find("SvxSegmentList");
  if (!SvxSegmentListNode) { cerr << PHWHERE << " ERROR: Can't find SvxSegmentList." << endl;  return EVENT_OK; }
  else { d_segment = (SvxSegmentList*)SvxSegmentListNode->getData(); }
  
  VtxOut *vtxout=0;           
  PHTypedNodeIterator<VtxOut> vtxoutiter(topNode);
  PHIODataNode<VtxOut> *VtxOutNode = vtxoutiter.find("VtxOut");
  if (!VtxOutNode) { cerr << PHWHERE << " ERROR: Can't find VtxOut." << endl;  return EVENT_OK; }
  else { vtxout = (VtxOut*)VtxOutNode->getData(); }
*/
  
  SvxClusterList *d_cluster = findNode::getClass<SvxClusterList>(topNode, "SvxClusterList");
  if(d_cluster==NULL){
    cerr<<"SvxVertexReco::process_event no SvxClusterList object"<<endl;
    return EVENT_OK;
  }

  d_segment = findNode::getClass<SvxSegmentList>(topNode, "SvxSegmentList");
  if(d_segment==NULL){
    cerr<<"SvxVertexReco::process_event no SvxSegmentList object"<<endl;
    return EVENT_OK;
  }

  VtxOut* vtxout = findNode::getClass<VtxOut>(topNode, "VtxOut");
  if(vtxout==NULL){
    cerr<<"SvxVertexReco::process_event no VtxOut object"<<endl;
    return EVENT_OK;
  }
  
  
  
  
  
  vector<vector<float> > tracks;//inner vector contains pixel positions: x1,y1,z1,x2,y2,z2;
  vector<float> tempvec;
  
  int partitionsr;
  int partitionsz;
  float halfwidthx=0.;
  float halfwidthy=0.;
  float halfwidthz=0.;
  
  float halfwidthxbig;
  float halfwidthybig;
  float halfwidthzbig;
  
  
  
  float vx,vy,vz;
  float v;
  
  vector<float> bincenter;
  
  sortHighMom();
  
  if(highmomlist.size()==0)
  {
    return 0;
  }
  
  int tt;
  for(unsigned int tr=0;tr<highmomlist.size();tr++)
  {
    tt=highmomlist.at(tr);
    if(d_segment->get_segment(tt)->getMomentum()>0.1)
    {
      tempvec.clear();
      tempvec.push_back(d_segment->get_segment(tt)->getProjectedPosition(0,0));
      tempvec.push_back(d_segment->get_segment(tt)->getProjectedPosition(0,1));
      tempvec.push_back(d_segment->get_segment(tt)->getProjectedPosition(0,2));
      tempvec.push_back(d_segment->get_segment(tt)->getProjectedPosition(1,0));
      tempvec.push_back(d_segment->get_segment(tt)->getProjectedPosition(1,1));
      tempvec.push_back(d_segment->get_segment(tt)->getProjectedPosition(1,2));
      tracks.push_back(tempvec);
    }
  }
  
  vertex[0]=d_segment->getVertex(0);
  vertex[1]=d_segment->getVertex(1);
  vertex[2]=d_segment->getVertex(2);
  
  if(verbosity>0){cout<<"SvxVertexReco: seed vertex = "<<vertex[0]<<", "<<vertex[1]<<", "<<vertex[2]<<endl;}
  
  partitionsr=10;
  partitionsz=4;
  halfwidthxbig = 0.3;
  halfwidthybig = 0.3;
  halfwidthzbig = 0.3;
  
  
  for(int iterations=0;iterations<=4;iterations++)
  {
    cube = TH3I("cube", "cube", partitionsr, vertex[0]-halfwidthxbig, vertex[0]+halfwidthxbig, partitionsr, vertex[1]-halfwidthybig, vertex[1]+halfwidthybig, partitionsz, vertex[2]-halfwidthzbig, vertex[2]+halfwidthzbig);
    cubez = TH1I("cubez", "cubez", partitionsz, vertex[2]-halfwidthzbig, vertex[2]+halfwidthzbig);
    cubex = TH1I("cubex", "cubex", partitionsr, vertex[0]-halfwidthxbig, vertex[0]+halfwidthxbig);
    cubey = TH1I("cubey", "cubey", partitionsr, vertex[1]-halfwidthybig, vertex[1]+halfwidthybig);
    
    for(unsigned int trk=0;trk<tracks.size();trk++)
    {
      halfwidthx = halfwidthxbig;
      halfwidthy = halfwidthybig;
      halfwidthz = halfwidthzbig;
      
      vx=tracks[trk][0]-tracks[trk][3];
      vy=tracks[trk][1]-tracks[trk][4];
      vz=tracks[trk][2]-tracks[trk][5];
      v=sqrt(vx*vx+vy*vy+vz*vz);
      ex=vx/v;
      ey=vy/v;
      ez=vz/v;
      x0=tracks[trk][0];
      y0=tracks[trk][1];
      z0=tracks[trk][2];
      
      
      intersectCube(vertex[0], vertex[1], vertex[2], halfwidthx, halfwidthy, halfwidthz);//gives iplane and isect(x,y,z)
      findCell(cubex, cubey, cubez);//takes in iplane and isect(x,y,z) and gives bin(x,y,z)
      halfwidthx=halfwidthx/((float)partitionsr);
      halfwidthy=halfwidthy/((float)partitionsr);
      halfwidthz=halfwidthz/((float)partitionsz);
      while(intersected==true)
      {
        cube.SetBinContent(binx,biny,binz, cube.GetBinContent(binx,biny,binz)+1);
        bincenter=getBinCenter3D(cubex, cubey, cubez, binx, biny, binz);
        intersectCubeFromSide(bincenter[0], bincenter[1], bincenter[2], halfwidthx, halfwidthy, halfwidthz);//gives iplane and isect(x,y,z)
        if(intersected==false){break;}
        if(!(stepCell(cube))){break;}//takes iplane and changes it to the iplane for the neighboring cell, and increments bin.false if on boundary
      }
    }
    
    
    
    
    findMaxBin(cube);
    bincenter=getBinCenter3D(cubex, cubey, cubez, binx, biny, binz);
    vertex[0]=bincenter[0];
    vertex[1]=bincenter[1];
    vertex[2]=bincenter[2];
    
    if(verbosity>0){cout<<"SvxVertexReco: current vertex: "<<vertex[0]<<" "<<vertex[1]<<" "<<vertex[2]<<endl;}
    
    halfwidthxbig/=2.;
    halfwidthybig/=2.;
    
    cube.Clear();
    cubex.Clear();
    cubey.Clear();
    cubez.Clear();
  }
  
  float prob = totalProbReco2(vertex);
  float vvv[3];
  vvv[0] = vertex[0];
  vvv[1] = vertex[1];
  vvv[2] = vertex[2] - (10.*0.01);
  for(int i=-10;i<=10;i++)
  {
    float tprob = totalProbReco2(vvv);
    if(tprob<prob)
    {
      vertex[2] = vvv[2];
      prob = tprob;
    }
    
    vvv[2] += 0.01;
  }
  
  if(verbosity>0){cout<<"SvxVertexReco: current vertex: "<<vertex[0]<<" "<<vertex[1]<<" "<<vertex[2]<<endl;}
  
  
  int nstep = 10;
  for(int j=0;j<40;j++)
  {
  
    float dx, dy, dz;
    float step = 0.0005;
    
    float pr1, pr2;
    
    float vv[3];
    
    pr1 = totalProbReco2(vertex);
    vv[0] = vertex[0] + step;
    vv[1] = vertex[1];
    vv[2] = vertex[2];
    pr2 = totalProbReco2(vv);
    dx = -(pr2 - pr1)/step;
    
    vv[0] = vertex[0];
    vv[1] = vertex[1] + step;
    vv[2] = vertex[2];
    pr2 = totalProbReco2(vv);
    dy = -(pr2 - pr1)/step;
    
    vv[0] = vertex[0];
    vv[1] = vertex[1];
    vv[2] = vertex[2] + step;
    pr2 = totalProbReco2(vv);
    dz = -(pr2 - pr1)/step;
    
    float dd = 1./sqrt(dx*dx + dy*dy + dz*dz);
    
    float dir[3];
    dir[0] = dx*dd;
    dir[1] = dy*dd;
    dir[2] = dz*dd;
    
    
    step = 0.0005;
    dx = dir[0]*step;
    dy = dir[1]*step;
    dz = dir[2]*step;
    
    vv[0] = vertex[0];
    vv[1] = vertex[1];
    vv[2] = vertex[2];
    bool changed = false;
    for(int i=1;i<=nstep;i++)
    {
      vv[0]+=dx;
      vv[1]+=dy;
      vv[2]+=dz;
      pr2 = totalProbReco2(vv);
      if(pr2<pr1)
      {
        changed = true;
        pr1 = pr2;
        vertex[0]=vv[0];
        vertex[1]=vv[1];
        vertex[2]=vv[2];
      }
    }
    if(changed==false){break;}
    if(verbosity>0){cout<<"SvxVertexReco: current vertex: "<<vertex[0]<<" "<<vertex[1]<<" "<<vertex[2]<<endl;}
  
  }
  
  
  
  
  prob = totalProbReco2(vertex);
  vvv[0] = vertex[0];
  vvv[1] = vertex[1];
  vvv[2] = vertex[2] - (10.*0.005);
  for(int i=-10;i<=10;i++)
  {
    float tprob = totalProbReco2(vvv);
    if(tprob<prob)
    {
      vertex[2] = vvv[2];
      prob = tprob;
    }
    
    vvv[2] += 0.005;
  }
  
  if(verbosity>0){cout<<"SvxVertexReco: current vertex: "<<vertex[0]<<" "<<vertex[1]<<" "<<vertex[2]<<endl;}
  
  for(int j=0;j<40;j++)
  {
  
    float dx, dy, dz;
    float step = 0.0005;
    
    float pr1, pr2;
    
    float vv[3];
    
    pr1 = totalProbReco2(vertex);
    vv[0] = vertex[0] + step;
    vv[1] = vertex[1];
    vv[2] = vertex[2];
    pr2 = totalProbReco2(vv);
    dx = -(pr2 - pr1)/step;
    
    vv[0] = vertex[0];
    vv[1] = vertex[1] + step;
    vv[2] = vertex[2];
    pr2 = totalProbReco2(vv);
    dy = -(pr2 - pr1)/step;
    
    vv[0] = vertex[0];
    vv[1] = vertex[1];
    vv[2] = vertex[2] + step;
    pr2 = totalProbReco2(vv);
    dz = -(pr2 - pr1)/step;
    
    float dd = 1./sqrt(dx*dx + dy*dy + dz*dz);
    
    float dir[3];
    dir[0] = dx*dd;
    dir[1] = dy*dd;
    dir[2] = dz*dd;
    
    
    step = 0.0005;
    dx = dir[0]*step;
    dy = dir[1]*step;
    dz = dir[2]*step;
    
    vv[0] = vertex[0];
    vv[1] = vertex[1];
    vv[2] = vertex[2];
    bool changed = false;
    for(int i=1;i<=nstep;i++)
    {
      vv[0]+=dx;
      vv[1]+=dy;
      vv[2]+=dz;
      
      pr2 = totalProbReco2(vv);
      if(pr2<pr1)
      {
        changed = true;
        pr1 = pr2;
        vertex[0]=vv[0];
        vertex[1]=vv[1];
        vertex[2]=vv[2];
      }
    }
    if(changed==false){break;}
    if(verbosity>0){cout<<"SvxVertexReco: current vertex: "<<vertex[0]<<" "<<vertex[1]<<" "<<vertex[2]<<endl;}
  
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  if(verbosity>0){cout<<"SvxVertexReco: final vertex = "<<vertex[0]<<", "<<vertex[1]<<", "<<vertex[2]<<endl;}
  float vertexerror[3]={0.003, 0.003, 0.007};
  float tver[3];
  tver[0]=vertex[0];
  tver[1]=vertex[1];
  tver[2]=vertex[2];
  
  d_segment->setVertex(vertex[0], vertex[1], vertex[2]);
  vtxout->AddVtx("SVX", tver, vertexerror, VTX::SVXORDER);
  
  
  reco.reset();
  return 0;
  
}


float SvxVertexReco::totalProbReco2(float *ver)
{
  
  float totalprob=0.;
  float prob=0.;
  float dprob=0.;
  
  probvec.clear();
  probvec.assign(highmomlist.size(),0.);
  
  vector<float> hitvec;
  hitvec.assign(9, 0.);
  vector<int> is_strip;
  is_strip.push_back(0);
  
  hitvec[0] = ver[0];
  hitvec[1] = ver[1];
  hitvec[2] = ver[2];
  
  float mom=0.;
  
  int trk;
  for(unsigned int tr=0;tr<highmomlist.size();tr++)
  {
    trk=highmomlist.at(tr);
    
    mom = d_segment->get_segment(trk)->getMomentum();
    bool helicity = d_segment->get_segment(trk)->IsPositive();
    
    
    
    for(int ll=0;ll<2;ll++)
    {
      for(int ii=0;ii<3;ii++)
      {
        hitvec[(3*(ll+1))+ii]=d_segment->get_segment(trk)->getProjectedPosition(ll,ii);
      }
    }
    trkr.probDers(helicity, hitvec, is_strip, mom*mom, prob, dprob);
    
    probvec[tr] = (-prob);
    
//     totalprob-=prob;
  }
  
  float mean=0.;
  for(unsigned int i=0;i<probvec.size();i++)
  {
    mean += probvec[i];
  }
  mean/=((float)(probvec.size()));
  
  float sigma=0.;
  for(unsigned int i=0;i<probvec.size();i++)
  {
    sigma += (probvec[i] - mean)*(probvec[i] - mean);
  }
  sigma/=((float)(probvec.size()));
  sigma = sqrt(sigma);
  sigma*=4.;
  
  for(unsigned int i=0;i<probvec.size();i++)
  {
    if((probvec[i] - mean)<sigma){totalprob += probvec[i];}
  }
  
  return totalprob;
}




void SvxVertexReco::findMaxBin(TH3I &hist)
{
  int maxval=0;
  int tempval;
  int maxbinx=1;
  int maxbiny=1;
  int maxbinz=1;
  
  
  for(int i=1;i<=hist.GetNbinsX();i++)
  {
    for(int j=1;j<=hist.GetNbinsY();j++)
    {
      for(int k=1;k<=hist.GetNbinsZ();k++)
      {
        tempval=(int)(hist.GetBinContent(i,j,k));
        if(tempval>maxval)
        {
          maxval=tempval;
          maxbinx=i;
          maxbiny=j;
          maxbinz=k;
        }
      }
    }
  }
  
  binx=maxbinx;
  biny=maxbiny;
  binz=maxbinz;
}


bool SvxVertexReco::inBoundary(float center, float halfwidth, float pos)
{
  if(pos>((center-halfwidth)+0.0001) && pos<((center+halfwidth)-0.0001))
  {
    return true;
  }
  else
  {
    return false;
  }
}


void SvxVertexReco::findCell(TH1I &hx, TH1I &hy, TH1I &hz)
{
  float wx = hx.GetBinWidth(1);
  float wy = hy.GetBinWidth(1);
  float wz = hz.GetBinWidth(1);
  
  float ix=isectx;
  float iy=isecty;
  float iz=isectz;
  
  switch(iplane)
  {
    case 0:
      ix+=wx/4.;
      break;
    case 1:
      ix-=wx/4.;
      break;
    case 2:
      iy+=wy/4.;
      break;
    case 3:
      iy-=wy/4.;
      break;
    case 4:
      iz+=wz/4.;
      break;
    case 5:
      iz-=wz/4.;
      break;
  }
  
  binx = hx.FindBin(ix);
  biny = hy.FindBin(iy);
  binz = hz.FindBin(iz);
}


void SvxVertexReco::intersectCube(float centerx, float centery, float centerz, float halfwidthx, float halfwidthy, float halfwidthz)
{
  intersected=false;
  
  
  float xp, yp, zp;
  float xi, yi, zi;
  float t;
  
  xp = centerx - halfwidthx;
  t = (xp-x0)/ex;
  yi = y0+t*ey;
  zi = z0+t*ez;
  if(inBoundary(centery, halfwidthy, yi) && inBoundary(centerz, halfwidthz, zi))
  {
    intersected=true;
    iplane=0;
    isectx=xp;
    isecty=yi;
    isectz=zi;
    return;
  }
  
  xp = centerx + halfwidthx;
  t = (xp-x0)/ex;
  yi = y0+t*ey;
  zi = z0+t*ez;
  if(inBoundary(centery, halfwidthy, yi) && inBoundary(centerz, halfwidthz, zi))
  {
    intersected=true;
    iplane=1;
    isectx=xp;
    isecty=yi;
    isectz=zi;
    return;
  }
  
  yp = centery - halfwidthy;
  t = (yp-y0)/ey;
  xi = x0+t*ex;
  zi = z0+t*ez;
  if(inBoundary(centerx, halfwidthx, xi) && inBoundary(centerz, halfwidthz, zi))
  {
    intersected=true;
    iplane=2;
    isectx=xi;
    isecty=yp;
    isectz=zi;
    return;
  }
  
  yp = centery + halfwidthy;
  t = (yp-y0)/ey;
  xi = x0+t*ex;
  zi = z0+t*ez;
  if(inBoundary(centerx, halfwidthx, xi) && inBoundary(centerz, halfwidthz, zi))
  {
    intersected=true;
    iplane=3;
    isectx=xi;
    isecty=yp;
    isectz=zi;
    return;
  }
  
  zp = centerz - halfwidthz;
  t = (zp-z0)/ez;
  xi = x0+t*ex;
  yi = y0+t*ey;
  if(inBoundary(centerx, halfwidthx, xi) && inBoundary(centery, halfwidthy, yi))
  {
    intersected=true;
    iplane=4;
    isectx=xi;
    isecty=yi;
    isectz=zp;
    return;
  }
  
  zp = centerz + halfwidthz;
  t = (zp-z0)/ez;
  xi = x0+t*ex;
  yi = y0+t*ey;
  if(inBoundary(centerx, halfwidthx, xi) && inBoundary(centery, halfwidthy, yi))
  {
    intersected=true;
    iplane=5;
    isectx=xi;
    isecty=yi;
    isectz=zp;
    return;
  }
  
  
  
  
}


void SvxVertexReco::intersectCubeFromSide(float centerx, float centery, float centerz, float halfwidthx, float halfwidthy, float halfwidthz)
{
  intersected=false;
  
  
  float xp, yp, zp;
  float xi, yi, zi;
  float t;
  
  if(iplane!=0)
  {
    xp = centerx - halfwidthx;
    t = (xp-x0)/ex;
    yi = y0+t*ey;
    zi = z0+t*ez;
    if(inBoundary(centery, halfwidthy, yi) && inBoundary(centerz, halfwidthz, zi))
    {
      intersected=true;
      iplane=0;
      isectx=xp;
      isecty=yi;
      isectz=zi;
      return;
    }
  }
  
  if(iplane!=1)
  {
    xp = centerx + halfwidthx;
    t = (xp-x0)/ex;
    yi = y0+t*ey;
    zi = z0+t*ez;
    if(inBoundary(centery, halfwidthy, yi) && inBoundary(centerz, halfwidthz, zi))
    {
      intersected=true;
      iplane=1;
      isectx=xp;
      isecty=yi;
      isectz=zi;
      return;
    }
  }
  
  if(iplane!=2)
  {
    yp = centery - halfwidthy;
    t = (yp-y0)/ey;
    xi = x0+t*ex;
    zi = z0+t*ez;
    if(inBoundary(centerx, halfwidthx, xi) && inBoundary(centerz, halfwidthz, zi))
    {
      intersected=true;
      iplane=2;
      isectx=xi;
      isecty=yp;
      isectz=zi;
      return;
    }
  }
  
  if(iplane!=3)
  {
    yp = centery + halfwidthy;
    t = (yp-y0)/ey;
    xi = x0+t*ex;
    zi = z0+t*ez;
    if(inBoundary(centerx, halfwidthx, xi) && inBoundary(centerz, halfwidthz, zi))
    {
      intersected=true;
      iplane=3;
      isectx=xi;
      isecty=yp;
      isectz=zi;
      return;
    }
  }
  
  if(iplane!=4)
  {
    zp = centerz - halfwidthz;
    t = (zp-z0)/ez;
    xi = x0+t*ex;
    yi = y0+t*ey;
    if(inBoundary(centerx, halfwidthx, xi) && inBoundary(centery, halfwidthy, yi))
    {
      intersected=true;
      iplane=4;
      isectx=xi;
      isecty=yi;
      isectz=zp;
      return;
    }
  }
  
  if(iplane!=5)
  {
    zp = centerz + halfwidthz;
    t = (zp-z0)/ez;
    xi = x0+t*ex;
    yi = y0+t*ey;
    if(inBoundary(centerx, halfwidthx, xi) && inBoundary(centery, halfwidthy, yi))
    {
      intersected=true;
      iplane=5;
      isectx=xi;
      isecty=yi;
      isectz=zp;
      return;
    }
  }
}


bool SvxVertexReco::stepCell(TH3I &hist)
{
  switch(iplane)
  {
    case 0:
      iplane=1;
      binx--;
      if(binx<=1 || binx>=hist.GetNbinsX()){return false;}
      else{return true;}
      break;
      
    case 1:
      iplane=0;
      binx++;
      if(binx<=1 || binx>=hist.GetNbinsX()){return false;}
      else{return true;}
      break;
      
    case 2:
      iplane=3;
      biny--;
      if(biny<=1 || biny>=hist.GetNbinsY()){return false;}
      else{return true;}
      break;
      
    case 3:
      iplane=2;
      biny++;
      if(biny<=1 || biny>=hist.GetNbinsY()){return false;}
      else{return true;}
      break;
      
    case 4:
      iplane=5;
      binz--;
      if(binz<=1 || binz>=hist.GetNbinsZ()){return false;}
      else{return true;}
      break;
      
    case 5:
      iplane=4;
      binz++;
      if(binz<=1 || binz>=hist.GetNbinsZ()){return false;}
      else{return true;}
      break;
  }
  return false;
}


vector<float> SvxVertexReco::getBinCenter3D(TH1I &hx, TH1I &hy, TH1I &hz, int binx, int biny, int binz)
{
  vector<float> vec;
  
  vec.push_back(hx.GetBinCenter(binx));
  vec.push_back(hy.GetBinCenter(biny));
  vec.push_back(hz.GetBinCenter(binz));
  
  return vec;
  
}





