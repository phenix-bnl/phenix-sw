#ifndef __MMUTMUIROADPAR_HH__
#define __MMUTMUIROADPAR_HH__

#include<PHObject.h>
#include<MUTOO.h>
#include<TMutParBase.h>

/*! 
Runtime parameter object for mMutMuiRoad analysis module
*/
class mMutMuiRoadPar : public TMutParBase
{

 public:

  /*! default constructor */
  mMutMuiRoadPar() : 
    _muid_road_proximity_cut(80), // cm
    _sigma_match_dist(18),        // cm
    _sigma_match_ang(0.18)       //radians
    {;}

  /*! destructor */
  ~mMutMuiRoadPar() {;}

  /*! Muid Road proximity cut used during select stub */
  double get_muid_road_proximity_cut() const { return _muid_road_proximity_cut;}

  /*! Muid Road proximity cut used during select stub */
  void set_muid_road_proximity_cut(double muid_road_proximity_cut) {
    _muid_road_proximity_cut=muid_road_proximity_cut;
  }  

  /*! Sigma of the distribusion of distance between the muid road projection point at last cathode plane of station3 and
      the station3 stub projection point at last cathode plane of station3 */
  void set_sigma_match_dist(double sigma_match_dist) { _sigma_match_dist = sigma_match_dist;}
  /*! Sigma of the distribusion of distance between the muid road projection point at last cathode plane of station3 and
      the station3 stub projection point at last cathode plane of station3 */
  double  get_sigma_match_dist() const { return _sigma_match_dist;}

  /*! Sigma of the distribusion of angle between the muid road direction vector and
      the station3 stub direction vector. */
  void set_sigma_match_ang(double sigma_match_ang) { _sigma_match_ang = sigma_match_ang;}

  /*! Sigma of the distribusion of angle between the muid road direction vector and
      the station3 stub direction vector. */
  double  get_sigma_match_ang() const { return _sigma_match_ang;}

 private:

  double _muid_road_proximity_cut;
  double _sigma_match_dist;
  double _sigma_match_ang;
};

#endif /* __MMUTMUIROADPAR_HH__ */
