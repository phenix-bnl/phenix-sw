#ifndef __DPHTRACKWRAPPER_H__
#define __DPHTRACKWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPHTrack.h"
class dPHTrackWrapper: public PHTable
{
public:
  dPHTrackWrapper(const char* name = "dPHTrack", const size_t& max_rows = 1);
  dPHTrackWrapper(const dPHTrackWrapper& source);
  dPHTrackWrapper& operator=(const dPHTrackWrapper& source);

  ~dPHTrackWrapper();

  void* RawTableData();
  DPHTRACK_ST* TableData();

  DPHTRACK_ST& operator[](const size_t& row);
  const DPHTRACK_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_trackIndex(size_t n, int v) {
    fTableData[n].trackIndex = v;
  }
  int get_trackIndex(size_t n) const {
    return fTableData[n].trackIndex;
  }
  void set_arm(size_t n, short v) {
    fTableData[n].arm = v;
  }
  short get_arm(size_t n) const {
    return fTableData[n].arm;
  }
  void set_ifIntersectVtx(size_t n, short v) {
    fTableData[n].ifIntersectVtx = v;
  }
  short get_ifIntersectVtx(size_t n) const {
    return fTableData[n].ifIntersectVtx;
  }
  void set_ifIntersectDch(size_t n, short v) {
    fTableData[n].ifIntersectDch = v;
  }
  short get_ifIntersectDch(size_t n) const {
    return fTableData[n].ifIntersectDch;
  }
  void set_ifIntersectPc1(size_t n, short v) {
    fTableData[n].ifIntersectPc1 = v;
  }
  short get_ifIntersectPc1(size_t n) const {
    return fTableData[n].ifIntersectPc1;
  }
  void set_ifIntersectPc2(size_t n, short v) {
    fTableData[n].ifIntersectPc2 = v;
  }
  short get_ifIntersectPc2(size_t n) const {
    return fTableData[n].ifIntersectPc2;
  }
  void set_ifIntersectPc3(size_t n, short v) {
    fTableData[n].ifIntersectPc3 = v;
  }
  short get_ifIntersectPc3(size_t n) const {
    return fTableData[n].ifIntersectPc3;
  }
  void set_ifIntersectCrk(size_t n, short v) {
    fTableData[n].ifIntersectCrk = v;
  }
  short get_ifIntersectCrk(size_t n) const {
    return fTableData[n].ifIntersectCrk;
  }
  void set_ifIntersectTec(size_t n, short v) {
    fTableData[n].ifIntersectTec = v;
  }
  short get_ifIntersectTec(size_t n) const {
    return fTableData[n].ifIntersectTec;
  }
  void set_ifIntersectTof(size_t n, short v) {
    fTableData[n].ifIntersectTof = v;
  }
  short get_ifIntersectTof(size_t n) const {
    return fTableData[n].ifIntersectTof;
  }
  void set_ifIntersectPbsc(size_t n, short v) {
    fTableData[n].ifIntersectPbsc = v;
  }
  short get_ifIntersectPbsc(size_t n) const {
    return fTableData[n].ifIntersectPbsc;
  }
  void set_ifIntersectPbgl(size_t n, short v) {
    fTableData[n].ifIntersectPbgl = v;
  }
  short get_ifIntersectPbgl(size_t n) const {
    return fTableData[n].ifIntersectPbgl;
  }
  void set_projectionVtx(size_t d0, size_t n, double v) {
    fTableData[n].projectionVtx[d0] = v;
  }
  double get_projectionVtx(size_t d0, size_t n) const {
    return fTableData[n].projectionVtx[d0];
  }
  void set_projectionDch(size_t d0, size_t n, double v) {
    fTableData[n].projectionDch[d0] = v;
  }
  double get_projectionDch(size_t d0, size_t n) const {
    return fTableData[n].projectionDch[d0];
  }
  void set_projectionPc1(size_t d0, size_t n, double v) {
    fTableData[n].projectionPc1[d0] = v;
  }
  double get_projectionPc1(size_t d0, size_t n) const {
    return fTableData[n].projectionPc1[d0];
  }
  void set_projectionPc2(size_t d0, size_t n, double v) {
    fTableData[n].projectionPc2[d0] = v;
  }
  double get_projectionPc2(size_t d0, size_t n) const {
    return fTableData[n].projectionPc2[d0];
  }
  void set_projectionPc3(size_t d0, size_t n, double v) {
    fTableData[n].projectionPc3[d0] = v;
  }
  double get_projectionPc3(size_t d0, size_t n) const {
    return fTableData[n].projectionPc3[d0];
  }
  void set_projectionCrk(size_t d0, size_t n, double v) {
    fTableData[n].projectionCrk[d0] = v;
  }
  double get_projectionCrk(size_t d0, size_t n) const {
    return fTableData[n].projectionCrk[d0];
  }
  void set_projectionTec(size_t d0, size_t n, double v) {
    fTableData[n].projectionTec[d0] = v;
  }
  double get_projectionTec(size_t d0, size_t n) const {
    return fTableData[n].projectionTec[d0];
  }
  void set_projectionTof(size_t d0, size_t n, double v) {
    fTableData[n].projectionTof[d0] = v;
  }
  double get_projectionTof(size_t d0, size_t n) const {
    return fTableData[n].projectionTof[d0];
  }
  void set_projectionPbSc(size_t d0, size_t n, double v) {
    fTableData[n].projectionPbSc[d0] = v;
  }
  double get_projectionPbSc(size_t d0, size_t n) const {
    return fTableData[n].projectionPbSc[d0];
  }
  void set_projectionPbGl(size_t d0, size_t n, double v) {
    fTableData[n].projectionPbGl[d0] = v;
  }
  double get_projectionPbGl(size_t d0, size_t n) const {
    return fTableData[n].projectionPbGl[d0];
  }
  void set_errorVtx(size_t d0, size_t n, double v) {
    fTableData[n].errorVtx[d0] = v;
  }
  double get_errorVtx(size_t d0, size_t n) const {
    return fTableData[n].errorVtx[d0];
  }
  void set_errorDch(size_t d0, size_t n, double v) {
    fTableData[n].errorDch[d0] = v;
  }
  double get_errorDch(size_t d0, size_t n) const {
    return fTableData[n].errorDch[d0];
  }
  void set_errorPc1(size_t d0, size_t n, double v) {
    fTableData[n].errorPc1[d0] = v;
  }
  double get_errorPc1(size_t d0, size_t n) const {
    return fTableData[n].errorPc1[d0];
  }
  void set_errorPc2(size_t d0, size_t n, double v) {
    fTableData[n].errorPc2[d0] = v;
  }
  double get_errorPc2(size_t d0, size_t n) const {
    return fTableData[n].errorPc2[d0];
  }
  void set_errorPc3(size_t d0, size_t n, double v) {
    fTableData[n].errorPc3[d0] = v;
  }
  double get_errorPc3(size_t d0, size_t n) const {
    return fTableData[n].errorPc3[d0];
  }
  void set_errorCrk(size_t d0, size_t n, double v) {
    fTableData[n].errorCrk[d0] = v;
  }
  double get_errorCrk(size_t d0, size_t n) const {
    return fTableData[n].errorCrk[d0];
  }
  void set_errorTec(size_t d0, size_t n, double v) {
    fTableData[n].errorTec[d0] = v;
  }
  double get_errorTec(size_t d0, size_t n) const {
    return fTableData[n].errorTec[d0];
  }
  void set_errorTof(size_t d0, size_t n, double v) {
    fTableData[n].errorTof[d0] = v;
  }
  double get_errorTof(size_t d0, size_t n) const {
    return fTableData[n].errorTof[d0];
  }
  void set_errorPbSc(size_t d0, size_t n, double v) {
    fTableData[n].errorPbSc[d0] = v;
  }
  double get_errorPbSc(size_t d0, size_t n) const {
    return fTableData[n].errorPbSc[d0];
  }
  void set_errorPbGl(size_t d0, size_t n, double v) {
    fTableData[n].errorPbGl[d0] = v;
  }
  double get_errorPbGl(size_t d0, size_t n) const {
    return fTableData[n].errorPbGl[d0];
  }
  void set_directionVtx(size_t d0, size_t n, double v) {
    fTableData[n].directionVtx[d0] = v;
  }
  double get_directionVtx(size_t d0, size_t n) const {
    return fTableData[n].directionVtx[d0];
  }
  void set_directionDch(size_t d0, size_t n, double v) {
    fTableData[n].directionDch[d0] = v;
  }
  double get_directionDch(size_t d0, size_t n) const {
    return fTableData[n].directionDch[d0];
  }
  void set_directionPc1(size_t d0, size_t n, double v) {
    fTableData[n].directionPc1[d0] = v;
  }
  double get_directionPc1(size_t d0, size_t n) const {
    return fTableData[n].directionPc1[d0];
  }
  void set_directionPc2(size_t d0, size_t n, double v) {
    fTableData[n].directionPc2[d0] = v;
  }
  double get_directionPc2(size_t d0, size_t n) const {
    return fTableData[n].directionPc2[d0];
  }
  void set_directionPc3(size_t d0, size_t n, double v) {
    fTableData[n].directionPc3[d0] = v;
  }
  double get_directionPc3(size_t d0, size_t n) const {
    return fTableData[n].directionPc3[d0];
  }
  void set_directionCrk(size_t d0, size_t n, double v) {
    fTableData[n].directionCrk[d0] = v;
  }
  double get_directionCrk(size_t d0, size_t n) const {
    return fTableData[n].directionCrk[d0];
  }
  void set_directionTec(size_t d0, size_t n, double v) {
    fTableData[n].directionTec[d0] = v;
  }
  double get_directionTec(size_t d0, size_t n) const {
    return fTableData[n].directionTec[d0];
  }
  void set_directionTof(size_t d0, size_t n, double v) {
    fTableData[n].directionTof[d0] = v;
  }
  double get_directionTof(size_t d0, size_t n) const {
    return fTableData[n].directionTof[d0];
  }
  void set_directionPbSc(size_t d0, size_t n, double v) {
    fTableData[n].directionPbSc[d0] = v;
  }
  double get_directionPbSc(size_t d0, size_t n) const {
    return fTableData[n].directionPbSc[d0];
  }
  void set_directionPbGl(size_t d0, size_t n, double v) {
    fTableData[n].directionPbGl[d0] = v;
  }
  double get_directionPbGl(size_t d0, size_t n) const {
    return fTableData[n].directionPbGl[d0];
  }
  void set_crkPathLength(size_t n, double v) {
    fTableData[n].crkPathLength = v;
  }
  double get_crkPathLength(size_t n) const {
    return fTableData[n].crkPathLength;
  }
  void set_tofPathLength(size_t n, double v) {
    fTableData[n].tofPathLength = v;
  }
  double get_tofPathLength(size_t n) const {
    return fTableData[n].tofPathLength;
  }
  void set_emcPathLength(size_t n, double v) {
    fTableData[n].emcPathLength = v;
  }
  double get_emcPathLength(size_t n) const {
    return fTableData[n].emcPathLength;
  }

private:
  DPHTRACK_ST* fTableData;

  ClassDef(dPHTrackWrapper,1)
};
#endif /*__DPHTRACKWRAPPER_H__*/
