#ifndef __MUMGEOWRAPPER_H__
#define __MUMGEOWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "mumgeo.h"
class mumgeoWrapper: public PHTable
{
public:
  mumgeoWrapper(const char* name = "mumgeo", const size_t& max_rows = 1);
//  mumgeoWrapper(const mumgeoWrapper& source);
//  mumgeoWrapper& operator=(const mumgeoWrapper& source);

  ~mumgeoWrapper();

  void* RawTableData();
  MUMGEO_ST* TableData();

  MUMGEO_ST& operator[](const size_t& row);
  const MUMGEO_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_mt_frame_side_thick1(size_t d0, size_t n, float v) {
    fTableData[n].mt_frame_side_thick1[d0] = v;
  }
  float get_mt_frame_side_thick1(size_t d0, size_t n) const {
    return fTableData[n].mt_frame_side_thick1[d0];
  }
  void set_mt_frame_side_thick2(size_t d0, size_t n, float v) {
    fTableData[n].mt_frame_side_thick2[d0] = v;
  }
  float get_mt_frame_side_thick2(size_t d0, size_t n) const {
    return fTableData[n].mt_frame_side_thick2[d0];
  }
  void set_mt_frame_end_thick1(size_t d0, size_t n, float v) {
    fTableData[n].mt_frame_end_thick1[d0] = v;
  }
  float get_mt_frame_end_thick1(size_t d0, size_t n) const {
    return fTableData[n].mt_frame_end_thick1[d0];
  }
  void set_mt_frame_end_thick2(size_t d0, size_t n, float v) {
    fTableData[n].mt_frame_end_thick2[d0] = v;
  }
  float get_mt_frame_end_thick2(size_t d0, size_t n) const {
    return fTableData[n].mt_frame_end_thick2[d0];
  }
  void set_mt_plane_thickness1(size_t d0, size_t n, float v) {
    fTableData[n].mt_plane_thickness1[d0] = v;
  }
  float get_mt_plane_thickness1(size_t d0, size_t n) const {
    return fTableData[n].mt_plane_thickness1[d0];
  }
  void set_mt_plane_thickness2(size_t d0, size_t n, float v) {
    fTableData[n].mt_plane_thickness2[d0] = v;
  }
  float get_mt_plane_thickness2(size_t d0, size_t n) const {
    return fTableData[n].mt_plane_thickness2[d0];
  }
  void set_mt_plane_spacing11(size_t d0, size_t n, float v) {
    fTableData[n].mt_plane_spacing11[d0] = v;
  }
  float get_mt_plane_spacing11(size_t d0, size_t n) const {
    return fTableData[n].mt_plane_spacing11[d0];
  }
  void set_mt_plane_spacing12(size_t d0, size_t n, float v) {
    fTableData[n].mt_plane_spacing12[d0] = v;
  }
  float get_mt_plane_spacing12(size_t d0, size_t n) const {
    return fTableData[n].mt_plane_spacing12[d0];
  }
  void set_mt_plane_spacing13(size_t d0, size_t n, float v) {
    fTableData[n].mt_plane_spacing13[d0] = v;
  }
  float get_mt_plane_spacing13(size_t d0, size_t n) const {
    return fTableData[n].mt_plane_spacing13[d0];
  }
  void set_mt_plane_spacing14(size_t d0, size_t n, float v) {
    fTableData[n].mt_plane_spacing14[d0] = v;
  }
  float get_mt_plane_spacing14(size_t d0, size_t n) const {
    return fTableData[n].mt_plane_spacing14[d0];
  }
  void set_mt_plane_spacing15(size_t d0, size_t n, float v) {
    fTableData[n].mt_plane_spacing15[d0] = v;
  }
  float get_mt_plane_spacing15(size_t d0, size_t n) const {
    return fTableData[n].mt_plane_spacing15[d0];
  }
  void set_mt_plane_spacing21(size_t d0, size_t n, float v) {
    fTableData[n].mt_plane_spacing21[d0] = v;
  }
  float get_mt_plane_spacing21(size_t d0, size_t n) const {
    return fTableData[n].mt_plane_spacing21[d0];
  }
  void set_mt_plane_spacing22(size_t d0, size_t n, float v) {
    fTableData[n].mt_plane_spacing22[d0] = v;
  }
  float get_mt_plane_spacing22(size_t d0, size_t n) const {
    return fTableData[n].mt_plane_spacing22[d0];
  }
  void set_mt_plane_spacing23(size_t d0, size_t n, float v) {
    fTableData[n].mt_plane_spacing23[d0] = v;
  }
  float get_mt_plane_spacing23(size_t d0, size_t n) const {
    return fTableData[n].mt_plane_spacing23[d0];
  }
  void set_mt_plane_spacing24(size_t d0, size_t n, float v) {
    fTableData[n].mt_plane_spacing24[d0] = v;
  }
  float get_mt_plane_spacing24(size_t d0, size_t n) const {
    return fTableData[n].mt_plane_spacing24[d0];
  }
  void set_mt_plane_spacing25(size_t d0, size_t n, float v) {
    fTableData[n].mt_plane_spacing25[d0] = v;
  }
  float get_mt_plane_spacing25(size_t d0, size_t n) const {
    return fTableData[n].mt_plane_spacing25[d0];
  }
  void set_mt_station_z1(size_t d0, size_t n, float v) {
    fTableData[n].mt_station_z1[d0] = v;
  }
  float get_mt_station_z1(size_t d0, size_t n) const {
    return fTableData[n].mt_station_z1[d0];
  }
  void set_mt_station_z2(size_t d0, size_t n, float v) {
    fTableData[n].mt_station_z2[d0] = v;
  }
  float get_mt_station_z2(size_t d0, size_t n) const {
    return fTableData[n].mt_station_z2[d0];
  }
  void set_mt_station_inner_radius1(size_t d0, size_t n, float v) {
    fTableData[n].mt_station_inner_radius1[d0] = v;
  }
  float get_mt_station_inner_radius1(size_t d0, size_t n) const {
    return fTableData[n].mt_station_inner_radius1[d0];
  }
  void set_mt_station_inner_radius2(size_t d0, size_t n, float v) {
    fTableData[n].mt_station_inner_radius2[d0] = v;
  }
  float get_mt_station_inner_radius2(size_t d0, size_t n) const {
    return fTableData[n].mt_station_inner_radius2[d0];
  }
  void set_mt_station_outer_radius1(size_t d0, size_t n, float v) {
    fTableData[n].mt_station_outer_radius1[d0] = v;
  }
  float get_mt_station_outer_radius1(size_t d0, size_t n) const {
    return fTableData[n].mt_station_outer_radius1[d0];
  }
  void set_mt_station_outer_radius2(size_t d0, size_t n, float v) {
    fTableData[n].mt_station_outer_radius2[d0] = v;
  }
  float get_mt_station_outer_radius2(size_t d0, size_t n) const {
    return fTableData[n].mt_station_outer_radius2[d0];
  }
  void set_mt_chm_thick1(size_t d0, size_t n, float v) {
    fTableData[n].mt_chm_thick1[d0] = v;
  }
  float get_mt_chm_thick1(size_t d0, size_t n) const {
    return fTableData[n].mt_chm_thick1[d0];
  }
  void set_mt_chm_thick2(size_t d0, size_t n, float v) {
    fTableData[n].mt_chm_thick2[d0] = v;
  }
  float get_mt_chm_thick2(size_t d0, size_t n) const {
    return fTableData[n].mt_chm_thick2[d0];
  }
  void set_mum_arms(size_t n, short v) {
    fTableData[n].mum_arms = v;
  }
  short get_mum_arms(size_t n) const {
    return fTableData[n].mum_arms;
  }
  void set_mum_stations(size_t n, short v) {
    fTableData[n].mum_stations = v;
  }
  short get_mum_stations(size_t n) const {
    return fTableData[n].mum_stations;
  }
  void set_mum_channels(size_t n, short v) {
    fTableData[n].mum_channels = v;
  }
  short get_mum_channels(size_t n) const {
    return fTableData[n].mum_channels;
  }
  void set_mum_color(size_t n, short v) {
    fTableData[n].mum_color = v;
  }
  short get_mum_color(size_t n) const {
    return fTableData[n].mum_color;
  }
  void set_mu_arm_medium(size_t n, short v) {
    fTableData[n].mu_arm_medium = v;
  }
  short get_mu_arm_medium(size_t n) const {
    return fTableData[n].mu_arm_medium;
  }
  void set_mt_frame_medium1(size_t d0, size_t n, short v) {
    fTableData[n].mt_frame_medium1[d0] = v;
  }
  short get_mt_frame_medium1(size_t d0, size_t n) const {
    return fTableData[n].mt_frame_medium1[d0];
  }
  void set_mt_frame_medium2(size_t d0, size_t n, short v) {
    fTableData[n].mt_frame_medium2[d0] = v;
  }
  short get_mt_frame_medium2(size_t d0, size_t n) const {
    return fTableData[n].mt_frame_medium2[d0];
  }
  void set_mt_planes_per_station1(size_t d0, size_t n, short v) {
    fTableData[n].mt_planes_per_station1[d0] = v;
  }
  short get_mt_planes_per_station1(size_t d0, size_t n) const {
    return fTableData[n].mt_planes_per_station1[d0];
  }
  void set_mt_planes_per_station2(size_t d0, size_t n, short v) {
    fTableData[n].mt_planes_per_station2[d0] = v;
  }
  short get_mt_planes_per_station2(size_t d0, size_t n) const {
    return fTableData[n].mt_planes_per_station2[d0];
  }
  void set_mt_station_medium1(size_t d0, size_t n, short v) {
    fTableData[n].mt_station_medium1[d0] = v;
  }
  short get_mt_station_medium1(size_t d0, size_t n) const {
    return fTableData[n].mt_station_medium1[d0];
  }
  void set_mt_station_medium2(size_t d0, size_t n, short v) {
    fTableData[n].mt_station_medium2[d0] = v;
  }
  short get_mt_station_medium2(size_t d0, size_t n) const {
    return fTableData[n].mt_station_medium2[d0];
  }

private:
  MUMGEO_ST* fTableData;

  ClassDef(mumgeoWrapper,1)
};
#endif /*__MUMGEOWRAPPER_H__*/
