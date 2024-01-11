#ifndef __DCGLTRACKWRAPPER_H__
#define __DCGLTRACKWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCglTrack.h"
class dCglTrackWrapper: public PHTable
{
public:
  dCglTrackWrapper(const char* name = "dCglTrack", const size_t& max_rows = 1);
  ~dCglTrackWrapper();

  void* RawTableData();
  DCGLTRACK_ST* TableData();

  DCGLTRACK_ST& operator[](const size_t& row);
  const DCGLTRACK_ST& operator[](const size_t& row) const;

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
  void set_dctracksid(size_t n, short v) {
    fTableData[n].dctracksid = v;
  }
  short get_dctracksid(size_t n) const {
    return fTableData[n].dctracksid;
  }
  void set_tectrackid(size_t n, short v) {
    fTableData[n].tectrackid = v;
  }
  short get_tectrackid(size_t n) const {
    return fTableData[n].tectrackid;
  }
  void set_pc1clusid(size_t n, short v) {
    fTableData[n].pc1clusid = v;
  }
  short get_pc1clusid(size_t n) const {
    return fTableData[n].pc1clusid;
  }
  void set_pc2clusid(size_t n, short v) {
    fTableData[n].pc2clusid = v;
  }
  short get_pc2clusid(size_t n) const {
    return fTableData[n].pc2clusid;
  }
  void set_pc3clusid(size_t n, short v) {
    fTableData[n].pc3clusid = v;
  }
  short get_pc3clusid(size_t n) const {
    return fTableData[n].pc3clusid;
  }
  void set_tofrecid(size_t n, short v) {
    fTableData[n].tofrecid = v;
  }
  short get_tofrecid(size_t n) const {
    return fTableData[n].tofrecid;
  }
  void set_emcclusid(size_t n, short v) {
    fTableData[n].emcclusid = v;
  }
  short get_emcclusid(size_t n) const {
    return fTableData[n].emcclusid;
  }
  void set_richringid(size_t n, short v) {
    fTableData[n].richringid = v;
  }
  short get_richringid(size_t n) const {
    return fTableData[n].richringid;
  }
  void set_quality(size_t n, float v) {
    fTableData[n].quality = v;
  }
  float get_quality(size_t n) const {
    return fTableData[n].quality;
  }
  void set_trackModel(size_t n, short v) {
    fTableData[n].trackModel = v;
  }
  short get_trackModel(size_t n) const {
    return fTableData[n].trackModel;
  }

private:
  DCGLTRACK_ST* fTableData;

  ClassDef(dCglTrackWrapper,1)
};
#endif /*__DCGLTRACKWRAPPER_H__*/
