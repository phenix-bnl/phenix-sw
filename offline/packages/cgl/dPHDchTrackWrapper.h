#ifndef __DPHDCHTRACKWRAPPER_H__
#define __DPHDCHTRACKWRAPPER_H__

#include <stddef.h>
#include <PHTable.hh>
#include <dPHDchTrack.h>
class dPHDchTrackWrapper: public PHTable
{
public:
  dPHDchTrackWrapper(const char* name = "dPHDchTrack", const size_t& max_rows = 1);
  dPHDchTrackWrapper(const dPHDchTrackWrapper& source);
  dPHDchTrackWrapper& operator=(const dPHDchTrackWrapper& source);

  ~dPHDchTrackWrapper();

  void* RawTableData();
  DPHDCHTRACK_ST* TableData();

  DPHDCHTRACK_ST& operator[](const size_t& row);
  const DPHDCHTRACK_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_arm(size_t n, short v) {
    fTableData[n].arm = v;
  }
  short get_arm(size_t n) const {
    return fTableData[n].arm;
  }
  void set_side(size_t n, short v) {
    fTableData[n].side = v;
  }
  short get_side(size_t n) const {
    return fTableData[n].side;
  }
  void set_charge(size_t n, short v) {
    fTableData[n].charge = v;
  }
  short get_charge(size_t n) const {
    return fTableData[n].charge;
  }

  void set_alpha(size_t n, double v) {
    fTableData[n].alpha = v;
  }
  double get_alpha(size_t n) const {
    return fTableData[n].alpha;
  }

  void set_zed(size_t n, double v) {
    fTableData[n].zed = v;
  }
  double get_zed(size_t n) const {
    return fTableData[n].zed;
  }

  void set_numberOfX1X2hitsFitted(size_t n, short v) {
    fTableData[n].numberOfX1X2hitsFitted = v;
  }
  short get_numberOfX1X2hitsFitted(size_t n) const {
    return fTableData[n].numberOfX1X2hitsFitted;
  }

  void set_chi2(size_t n, double v) {
    fTableData[n].chi2 = v;
  }
  double get_chi2(size_t n) const {
    return fTableData[n].chi2;
  }

  void set_numberOfSuccessfulIterations(size_t n, short v) {
    fTableData[n].numberOfSuccessfulIterations = v;
  }
  short get_numberOfSuccessfulIterations(size_t n) const {
    return fTableData[n].numberOfSuccessfulIterations;
  }

  void set_ErrorCode(size_t n, short v) {
    fTableData[n].ErrorCode = v;
  }
  short get_ErrorCode(size_t n) const {
    return fTableData[n].ErrorCode;
  }

  void set_momentum(size_t n, double v) {
    fTableData[n].momentum = v;
  }
  double get_momentum(size_t n) const {
    return fTableData[n].momentum;
  }
  void set_fittedPhi0(size_t n, double v) {
    fTableData[n].fittedPhi0 = v;
  }
  double get_fittedPhi0(size_t n) const {
    return fTableData[n].fittedPhi0;
  }
  void set_fittedTheta0(size_t n, double v) {
    fTableData[n].fittedTheta0 = v;
  }
  double get_fittedTheta0(size_t n) const {
    return fTableData[n].fittedTheta0;
  }

  void set_fittedAlpha(size_t n, double v) {
    fTableData[n].fittedAlpha = v;
  }
  double get_fittedAlpha(size_t n) const {
    return fTableData[n].fittedAlpha;
  }
  void set_fittedPhi(size_t n, double v) {
    fTableData[n].fittedPhi = v;
  }
  double get_fittedPhi(size_t n) const {
    return fTableData[n].fittedPhi;
  }

  void set_fittedBeta(size_t n, double v) {
    fTableData[n].fittedBeta = v;
  }
  double get_fittedBeta(size_t n) const {
    return fTableData[n].fittedBeta;
  }

  void set_vertex(size_t d0, size_t n, float v) {
    fTableData[n].vertex[d0] = v;
  }
  float get_vertex(size_t d0, size_t n) const {
    return fTableData[n].vertex[d0];
  }
  void set_predictMomentum(size_t d0, size_t n, float v) {
    fTableData[n].predictMomentum[d0] = v;
  }
  float get_predictMomentum(size_t d0, size_t n) const {
    return fTableData[n].predictMomentum[d0];
  }

  void set_projectToVertex(size_t d0, size_t n, float v) {
    fTableData[n].projectToVertex[d0] = v;
  }
  float get_projectToVertex(size_t d0, size_t n) const {
    return fTableData[n].projectToVertex[d0];
  }

  void set_projectToPc1(size_t d0, size_t n, float v) {
    fTableData[n].projectToPc1[d0] = v;
  }
  float get_projectToPc1(size_t d0, size_t n) const {
    return fTableData[n].projectToPc1[d0];
  }
  void set_projectToPc2(size_t d0, size_t n, float v) {
    fTableData[n].projectToPc2[d0] = v;
  }
  float get_projectToPc2(size_t d0, size_t n) const {
    return fTableData[n].projectToPc2[d0];
  }
  void set_projectToPc3(size_t d0, size_t n, float v) {
    fTableData[n].projectToPc3[d0] = v;
  }
  float get_projectToPc3(size_t d0, size_t n) const {
    return fTableData[n].projectToPc3[d0];
  }
  void set_projectToTec(size_t d0, size_t n, float v) {
    fTableData[n].projectToTec[d0] = v;
  }
  float get_projectToTec(size_t d0, size_t n) const {
    return fTableData[n].projectToTec[d0];
  }
  void set_projectToPbSc(size_t d0, size_t n, float v) {
    fTableData[n].projectToPbSc[d0] = v;
  }
  float get_projectToPbSc(size_t d0, size_t n) const {
    return fTableData[n].projectToPbSc[d0];
  }
  void set_projectToPbGl(size_t d0, size_t n, float v) {
    fTableData[n].projectToPbGl[d0] = v;
  }
  float get_projectToPbGl(size_t d0, size_t n) const {
    return fTableData[n].projectToPbGl[d0];
  }
  void set_projectToCrk(size_t d0, size_t n, float v) {
    fTableData[n].projectToCrk[d0] = v;
  }
  float get_projectToCrk(size_t d0, size_t n) const {
    return fTableData[n].projectToCrk[d0];
  }
  void set_projectToTof(size_t d0, size_t n, float v) {
    fTableData[n].projectToTof[d0] = v;
  }
  float get_projectToTof(size_t d0, size_t n) const {
    return fTableData[n].projectToTof[d0];
  }

private:
  DPHDCHTRACK_ST* fTableData;

  ClassDef(dPHDchTrackWrapper,1)
};
#endif /*__DPHDCHTRACKWRAPPER_H__*/
