#ifndef __DDCHTRACKSWRAPPER_H__
#define __DDCHTRACKSWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dDchTracks.h"
class dDchTracksWrapper: public PHTable
{
public:
  dDchTracksWrapper(const char* name = "dDchTracks", const size_t& max_rows = 1);
  ~dDchTracksWrapper();

  void* RawTableData();
  DDCHTRACKS_ST* TableData();

  DDCHTRACKS_ST& operator[](const size_t& row);
  const DDCHTRACKS_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_trackid(size_t n, short v) {
    fTableData[n].trackid = v;
  }
  short get_trackid(size_t n) const {
    return fTableData[n].trackid;
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
  void set_point(size_t d0, size_t n, float v) {
    fTableData[n].point[d0] = v;
  }
  float get_point(size_t d0, size_t n) const {
    return fTableData[n].point[d0];
  }
  void set_err_point(size_t d0, size_t n, float v) {
    fTableData[n].err_point[d0] = v;
  }
  float get_err_point(size_t d0, size_t n) const {
    return fTableData[n].err_point[d0];
  }
  void set_direction(size_t d0, size_t n, float v) {
    fTableData[n].direction[d0] = v;
  }
  float get_direction(size_t d0, size_t n) const {
    return fTableData[n].direction[d0];
  }
  void set_err_direction(size_t d0, size_t n, float v) {
    fTableData[n].err_direction[d0] = v;
  }
  float get_err_direction(size_t d0, size_t n) const {
    return fTableData[n].err_direction[d0];
  }
  void set_hits(size_t d0, size_t n, short v) {
    fTableData[n].hits[d0] = v;
  }
  short get_hits(size_t d0, size_t n) const {
    return fTableData[n].hits[d0];
  }
  void set_quality(size_t n, short v) {
    fTableData[n].quality = v;
  }
  short get_quality(size_t n) const {
    return fTableData[n].quality;
  }
  void set_phi(size_t n, float v) {
    fTableData[n].phi = v;
  }
  float get_phi(size_t n) const {
    return fTableData[n].phi;
  }
  void set_alpha(size_t n, float v) {
    fTableData[n].alpha = v;
  }
  float get_alpha(size_t n) const {
    return fTableData[n].alpha;
  }
  void set_beta(size_t n, float v) {
    fTableData[n].beta = v;
  }
  float get_beta(size_t n) const {
    return fTableData[n].beta;
  }
  void set_betaNoVertex(size_t n, float v) {
    fTableData[n].betaNoVertex = v;
  }
  float get_betaNoVertex(size_t n) const {
    return fTableData[n].betaNoVertex;
  }
  void set_zed(size_t n, float v) {
    fTableData[n].zed = v;
  }
  float get_zed(size_t n) const {
    return fTableData[n].zed;
  }
  void set_phi0(size_t n, float v) {
    fTableData[n].phi0 = v;
  }
  float get_phi0(size_t n) const {
    return fTableData[n].phi0;
  }
  void set_theta0(size_t n, float v) {
    fTableData[n].theta0 = v;
  }
  float get_theta0(size_t n) const {
    return fTableData[n].theta0;
  }
  void set_momentum(size_t n, float v) {
    fTableData[n].momentum = v;
  }
  float get_momentum(size_t n) const {
    return fTableData[n].momentum;
  }

private:
  DDCHTRACKS_ST* fTableData;

  ClassDef(dDchTracksWrapper,1)
};
#endif /*__DDCHTRACKSWRAPPER_H__*/
