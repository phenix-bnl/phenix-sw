#ifndef __PHSNGLTRACKV4_HH__
#define __PHSNGLTRACKV4_HH__

#include "PHSnglTrack.h"

class PHSnglTrackv4:public PHSnglTrack
{
 public:
  PHSnglTrackv4();
  virtual ~PHSnglTrackv4(){}

  unsigned short get_trackIndex()     const {return trackIndex;}
  void  set_trackIndex(const unsigned short val) {trackIndex =val;}
  void  set_projectionVtx(const short d0, const float v){projectionVtx[d0] = v;}
  float get_projectionVtx(const short d0) const  {return projectionVtx[d0];}
  void set_projectionDch(const short d0, const float v){projectionDch[d0] = v;}
  float get_projectionDch(const short d0) const  {return projectionDch[d0];}
  void set_projectionPc1(const short d0, const float v){projectionPc1[d0] = v;}
  float get_projectionPc1(const short d0) const  {return projectionPc1[d0];}
  void set_projectionPc2(const short d0,const float v) {projectionPc2[d0] = v;}
  float get_projectionPc2(const short d0) const  {return projectionPc2[d0];}
  void set_projectionPc3(const short d0, const float v){projectionPc3[d0] = v;}
  float get_projectionPc3(const short d0) const  {return projectionPc3[d0];}
  void set_projectionCrk(const short d0,const float v) {projectionCrk[d0] = v;}
  float get_projectionCrk(const short d0) const  {return projectionCrk[d0];}
  void set_projectionTec(const short d0, const float v){projectionTec[d0] = v;}
  float get_projectionTec(const short d0) const  {return projectionTec[d0];}
  void set_projectionTof(const short d0, const float v){projectionTof[d0] = v;}
  float get_projectionTof(const short d0) const  {return projectionTof[d0];}
  void set_projectionPbSc(const short d0,const float v){projectionPbSc[d0]= v;}
  float get_projectionPbSc(const short d0) const {return projectionPbSc[d0];}
  void set_projectionPbGl(const short d0,const float v){projectionPbGl[d0]= v;}
  float get_projectionPbGl(const short d0) const {return projectionPbGl[d0];}
  void set_projectionAcc(const short d0,const float v){projectionAcc[d0]= v;}
  float get_projectionAcc(const short d0) const {return projectionAcc[d0];}
  void set_directionVtx(const short d0, const float v) {directionVtx[d0]  = v;}
  float get_directionVtx(const short d0) const   {return directionVtx[d0];}
  void set_directionDch(const short d0,const float v)  {directionDch[d0] = v;}
  float get_directionDch(const short d0) const   {return directionDch[d0];}
  void set_directionPc1(const short d0,const float v)  {directionPc1[d0] = v;}
  float get_directionPc1(const short d0) const   {return directionPc1[d0];}
  void set_directionPc2(const short d0, const float v) {directionPc2[d0] = v;}
  float get_directionPc2(const short d0) const   {return directionPc2[d0];}
  void set_directionPc3(const short d0,const float v)  {directionPc3[d0] = v;}
  float get_directionPc3(const short d0) const   {return directionPc3[d0];}
  void set_directionCrk(const short d0, const float v) {directionCrk[d0] = v;}
  float get_directionCrk(const short d0) const   {return directionCrk[d0];}
  void set_directionTec(const short d0, const float v) {directionTec[d0] = v;}
  float get_directionTec(const short d0) const   {return directionTec[d0];}
  void set_directionTof(const short d0, const float v) {directionTof[d0] = v;}
  float get_directionTof(const short d0) const   {return directionTof[d0];}
  void set_directionPbSc(const short d0,const float v) {directionPbSc[d0] = v;}
  float get_directionPbSc(const short d0) const  {return directionPbSc[d0];}
  void set_directionPbGl(const short d0,const float v) {directionPbGl[d0] = v;}
  float get_directionPbGl(const short d0) const  {return directionPbGl[d0];}
  void set_directionAcc(const short d0,const float v) {directionAcc[d0] = v;}
  float get_directionAcc(const short d0) const  {return directionAcc[d0];}
  void set_crkPathLength(const float v)          {crkPathLength = v;}
  float get_crkPathLength() const          {return crkPathLength;}
  void set_tofPathLength(const float v)          {tofPathLength = v;}
  float get_tofPathLength() const          {return tofPathLength;}
  void set_emcPathLength(const float v)          {emcPathLength = v;}
  float  get_emcPathLength() const         {return emcPathLength;}

 protected:
  unsigned short trackIndex;

  float projectionAcc[3];
  float projectionCrk[3];
  float projectionDch[3];
  float projectionPbGl[3];
  float projectionPbSc[3];
  float projectionPc1[3];
  float projectionPc2[3];
  float projectionPc3[3];
  float projectionTec[3];
  float projectionTof[3];
  float projectionVtx[3];

  float directionAcc[3];
  float directionCrk[3];
  float directionDch[3];
  float directionPbGl[3];
  float directionPbSc[3];
  float directionPc1[3];
  float directionPc2[3];
  float directionPc3[3];
  float directionTec[3];
  float directionTof[3];
  float directionVtx[3];

  float crkPathLength;
  float emcPathLength;
  float tofPathLength;

  ClassDef(PHSnglTrackv4,1) 

};
#endif
