// $Id: TForwardMPTrack.h,v 1.2 2015/09/09 01:50:17 jinhuang Exp $                                                                                             

/*!
 * \file TForwardMPTrack.h
 * \brief storage for forward tracks, including FVTX, Absorber, MuTR, MuID
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.2 $
 * \date $Date: 2015/09/09 01:50:17 $
 */

#ifndef TFORWARDMPTRACK_H_
#define TFORWARDMPTRACK_H_

#include "TFvtxMPTrack.h"

/*!
 * \brief storage for forward tracks, including FVTX, Absorber, MuTR, MuID
 */
class TForwardMPTrack : public TFvtxMPTrack
{
public:
  TForwardMPTrack();
  virtual
  ~TForwardMPTrack();

  virtual void
  Reset();

  virtual void
  Print(Option_t *option = "") const;

  ////////////////////////////////////////////////////////
  //!@name absorber features
  //@{

public:

  //  static bool
  //  is_after_absorber(TFvtxMPNode::node_type t)
  //  {
  //    return (t == TFvtxMPNode::MuTr or t == TFvtxMPNode::MuID);
  //  }

  bool
  is_after_absorber(const double z) const
  {
    return (abs(z) > abs(z_ref));
  }

  virtual TVector3
  internal_fit_proj(const double z) const
  {
    TVector3 p(TFvtxMPTrack::internal_fit_proj(z));

    if (is_after_absorber(z))
      {
        TVector3 p2(trk_par_absorb_scat.get_proj(z));
        p2.SetZ(0);

        p += p2;
      }

    return p;
  }

  virtual TVector3
  internal_fit_vec(const double z) const
  {
    TVector3 p(trk_par.get_tx(), trk_par.get_ty(), 1);

    if (is_after_absorber(z))
      {
        p += TVector3(trk_par_absorb_scat.get_tx(),
            trk_par_absorb_scat.get_ty(), 0);
      }

    return p;
  }

  double
  get_mutr_disp_lateral_z_ref() const;

  double
  get_mutr_disp_pt_z_ref() const;

  double
  get_mutr_disp_tangent_phi() const;

  double
  get_mutr_disp_tangent_theta() const;

  double
  get_mutr_disp_lateral_z_ref_kalman() const;

  double
  get_mutr_disp_pt_z_ref_kalman() const;

  double
  get_mutr_disp_tangent_phi_kalman() const;

  double
  get_mutr_disp_tangent_theta_kalman() const;

  //! additional to trk_par due to multiple scattering after the absorber
  TFvtxTrkPar trk_par_absorb_scat;

  //! MuTr track difference for tan(polar angle)
  double mutr_disp_tangent_theta;

  //! MuTr track difference for tan(azimuthal angle)
  double mutr_disp_tangent_phi;

  //! MuTr track lateral (pT x z) displacement from beam at VTX vertex z
  double mutr_disp_lateral_z_ref;

  //! MuTr track displacement from beam in pT direction at VTX vertex z
  double mutr_disp_pt_z_ref;
  //@}

  ////////////////////////////////////////////////////////
  //!@name update for internal fits
  //@{

public:
  //! internal generalized least squares fit of full track in 3D
  virtual void
  interanl_fit();

  //! internal generalized least squares fit of Mutr track
  //! result saved to mutr_disp_lateral_vtx, mutr_disp_pt_vtx and mutr_disp_theta
  virtual void
  interanl_fit_MuTr_Alone()
  {
    std::cout << "TForwardMPTrack::interanl_fit_MuTr_Alone is obsolete"
        << std::endl;
    assert(0);
  }

protected:

  //! internal generalized least squares fit for vec_nodes and save to trk_par_fit
  virtual void
  interanl_fit(const t_vec_nodes_const& vec_nodes, TFvtxTrkPar &trk_par_fit,
      TFvtxTrkPar &trk_par_scat_fit);

  //@}

ClassDef(TForwardMPTrack,3)

};

#endif /* TFORWARDMPTRACK_H_ */
