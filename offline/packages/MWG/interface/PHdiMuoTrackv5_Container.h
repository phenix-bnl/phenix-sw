// $Id: PHdiMuoTrackv5_Container.h,v 1.1 2013/02/08 17:21:36 jinhuang Exp $                                                                                             

/*!
 * \file PHdiMuoTrackv5_Container.h
 * \brief template on read/save dimuon part into PHdiMuoTrackv5.
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.1 $
 * \date $Date: 2013/02/08 17:21:36 $
 */

#ifndef PHDIMUOTRACKV5_CONTAINER_H_
#define PHDIMUOTRACKV5_CONTAINER_H_

#include <iostream>
#include "PHObject.h"
#include "TClonesArray.h"
#include "PHdiMuoTrackv5.h"
#include "MWG.h"

/*! template on read/save dimuon part into PHMuoTrackout.
 *
 This class contains all the dimuon candidates stored for each event.
 Dimuon candidates are obtained by doing a combinatorial pairing of Muon Tracks
 stored in $MuoTracks$ branch. Dimuon candidates are then either opposite
 sign or like sign dimuons.
 *
 * It should be coupled to a single muon out object (e.g. PHMuoTracksv14)
 */

template<class PHMuoTracks>
  class PHdiMuoTrackv5_Container : public PHMuoTracks
  {

  public:

    //! selection of dimuont track version
    typedef PHdiMuoTrackv5 PHdiMuoTrack;

    //! destructor
    PHdiMuoTrackv5_Container();

    //! constructor
    virtual
    ~PHdiMuoTrackv5_Container();

    //! reset containers
    void
    Reset()
    {
      PHMuoTracks::Reset();
      ndiMuoTracks = 0;
      Clear();
    }

    //! identify object
    void
    identify(std::ostream &os = std::cout) const
    {
      os << "identify yourself:  PHdiMuoTrackv5_Container template based on "
          <<PHdiMuoTrack::Class_Name()
          << std::endl;
      return;
    }

    //! clear containers
    void
    Clear(Option_t *option = "")
    {
      PHMuoTracks::Clear();
      if (diMuoTracks)
        diMuoTracks->Delete();
    }

    //! object validity
    int
    IsValid() const
    {
      return 1;
    }

    //================================= PHParticle cloning/copying etc...

    TObject*
    GetSingleDimuon(unsigned int ipart);

    void
    AddPHDimuon(unsigned int ipart, TObject *);

    PHdiMuoTrackv5_Container*
    clone() const;

    //======================================= Dimuon fillers
    int
    Set_DimuArraySize(const unsigned int ndimu)
    {
      diMuoTracks->Expand(ndimu);
      return ndimu;
    }
    void
    AddPHDimuon(const unsigned int idimu)
    {
      new ((*diMuoTracks)[idimu]) PHdiMuoTrack();
    }
    void
    RemovePHDimuon(const unsigned int idimu)
    {
      diMuoTracks->RemoveAt(idimu);
      return;
    }

    //==================================== Dimuon Accessors
    //!@name Access methods
    //@{
    //! Return {\bf ndiMuoTracks}.
    int
    get_ndimu() const
    {
      return ndiMuoTracks;
    }

    //! Return {\bf idimu}$^{th}$ ditrack {\bf ditrkIndex[arrayid]}.
    int
    get_ditrkIndex(short arrayid, const unsigned int idimu) const;

    //! dimuon unique id
    ULong_t
    get_dimuon_uid(const unsigned int idimu) const
    {
      PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
      return ((dimu) ? dimu->get_uid() : 0);
    }

    //! Return {\bf idimu}$^{th}$ {\bf dimass}.
    float
    get_dimass(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf dicharge}.
    int
    get_dicharge(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf dipx}.
    float
    get_dipx(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf dipy}.
    float
    get_dipy(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf dipz}.
    float
    get_dipz(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_bp_xpos}.
    float
    get_vtx_bp_xpos(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_bp_ypos}.
    float
    get_vtx_bp_ypos(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_bp_zpos}.
    float
    get_vtx_bp_zpos(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_bp_dca}.
    float
    get_vtx_bp_dca(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_xpos}.
    float
    get_vtx_xpos(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_ypos}.
    float
    get_vtx_ypos(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_zpos}.
    float
    get_vtx_zpos(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_chrg_1}.
    float
    get_vtx_chrg_1(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_px_1}.
    float
    get_vtx_px_1(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_py_1}.
    float
    get_vtx_py_1(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_pz_1}.
    float
    get_vtx_pz_1(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_chrg_2}.
    float
    get_vtx_chrg_2(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_px_2}.
    float
    get_vtx_px_2(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_py_2}.
    float
    get_vtx_py_2(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ ditrack {\bf vtx_pz_2}.
    float
    get_vtx_pz_2(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ reduced number of degrees of freedom
    int
    get_vtx_ndf(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ reduced chisquare from vertex fit.
    float
    get_vtx_chisquare(const unsigned int idimu) const;

    //! Return {\bf idimu}$^{th}$ covariance matrix from vertex fit
    float
    get_vtx_cov(short arrayid1, short arrayid2, const unsigned int idimu) const;
    //@}

    //!@name Dimuon Mutators
    //@{

    void
    set_ndimu(const unsigned int ndimu)
    {
      ndiMuoTracks = ndimu;
    }

    //! dimuon uid
    void
    set_dimuon_uid(const unsigned int idimu, ULong_t value)
    {
      PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
      if (dimu)
        dimu->set_uid(value);
    }

    void
    set_dimass(const unsigned int idimu, float newVal);

    void
    set_dicharge(const unsigned int idimu, int newVal);

    void
    set_ditrkIndex(short arrayid, const unsigned int idimu, int newVal);

    void
    set_dipx(const unsigned int idimu, float newVal);

    void
    set_dipy(const unsigned int idimu, float newVal);

    void
    set_dipz(const unsigned int idimu, float newVal);

    void
    set_vtx_bp_xpos(const unsigned int idimu, float newVal);

    void
    set_vtx_bp_ypos(const unsigned int idimu, float newVal);

    void
    set_vtx_bp_zpos(const unsigned int idimu, float newVal);

    void
    set_vtx_bp_dca(const unsigned int idimu, float newVal);

    void
    set_vtx_xpos(const unsigned int idimu, float newVal);

    void
    set_vtx_ypos(const unsigned int idimu, float newVal);

    void
    set_vtx_zpos(const unsigned int idimu, float newVal);

    void
    set_vtx_chrg_1(const unsigned int idimu, float newVal);

    void
    set_vtx_px_1(const unsigned int idimu, float newVal);

    void
    set_vtx_py_1(const unsigned int idimu, float newVal);

    void
    set_vtx_pz_1(const unsigned int idimu, float newVal);

    void
    set_vtx_chrg_2(const unsigned int idimu, float newVal);

    void
    set_vtx_px_2(const unsigned int idimu, float newVal);

    void
    set_vtx_py_2(const unsigned int idimu, float newVal);

    void
    set_vtx_pz_2(const unsigned int idimu, float newVal);

    //! sets vertex number of degrees of freedom
    void
    set_vtx_ndf(const unsigned int idimu, int newVal);

    //! sets vertex reduced chisquare
    void
    set_vtx_chisquare(const unsigned int idimu, float newVal);

    void
    set_vtx_cov(short arrayid1, short arrayid2, const unsigned int idimu,
        float newVal);

    //@}

  private:

    //!@name Variables
    //@{
    //! Number of Dimuon Candidates in the event.
    int ndiMuoTracks;
    //! Dimuon Candidates Branch.
    TClonesArray* diMuoTracks;
    //@}

  ClassDef(PHdiMuoTrackv5_Container,1)
  };

templateClassImp(PHdiMuoTrackv5_Container)

//_________________________________________________________
//===== constructor/destructor
template<class PHMuoTracks>
  PHdiMuoTrackv5_Container<PHMuoTracks>::PHdiMuoTrackv5_Container() :
      ndiMuoTracks(0)
  {

    diMuoTracks = new TClonesArray(PHdiMuoTrack::Class_Name(), MWG::DIMU_ARRAY_SIZE);

  }

//_________________________________________________________
template<class PHMuoTracks>
  PHdiMuoTrackv5_Container<PHMuoTracks>::~PHdiMuoTrackv5_Container()
  {
    delete diMuoTracks;
  }

//_________________________________________________________
//=============================================== Dimuon Accessors
template<class PHMuoTracks>
  int
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_ditrkIndex(short arrayid,
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_trkIndex(arrayid) : -1);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_dimass(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_mass() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  int
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_dicharge(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_charge() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_dipx(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_px() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_dipy(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_py() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_dipz(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_pz() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_bp_xpos(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_bp_xpos() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_bp_ypos(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_bp_ypos() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_bp_zpos(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_bp_zpos() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_bp_dca(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_bp_dca() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_xpos(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_xpos() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_ypos(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_ypos() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_zpos(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_zpos() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_chrg_1(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_chrg_1() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_px_1(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_px_1() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_py_1(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_py_1() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_pz_1(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_pz_1() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_chrg_2(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_chrg_2() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_px_2(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_px_2() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_py_2(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_py_2() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_pz_2(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_pz_2() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  int
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_ndf(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_ndf() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_chisquare(
      const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_chisquare() : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  float
  PHdiMuoTrackv5_Container<PHMuoTracks>::get_vtx_cov(short arrayid1,
      short arrayid2, const unsigned int idimu) const
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    return ((dimu) ? dimu->get_vtx_cov(arrayid1, arrayid2) : 0);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_dimass(const unsigned int idimu,
      float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_mass(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_dicharge(const unsigned int idimu,
      int newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_charge(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_ditrkIndex(short arrayid,
      const unsigned int idimu, int newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_trkIndex(arrayid, newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_dipx(const unsigned int idimu,
      float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_px(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_dipy(const unsigned int idimu,
      float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_py(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_dipz(const unsigned int idimu,
      float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_pz(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_bp_xpos(
      const unsigned int idimu, float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_bp_xpos(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_bp_ypos(
      const unsigned int idimu, float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_bp_ypos(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_bp_zpos(
      const unsigned int idimu, float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_bp_zpos(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_bp_dca(
      const unsigned int idimu, float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_bp_dca(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_xpos(const unsigned int idimu,
      float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_xpos(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_ypos(const unsigned int idimu,
      float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_ypos(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_zpos(const unsigned int idimu,
      float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_zpos(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_chrg_1(
      const unsigned int idimu, float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_chrg_1(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_px_1(const unsigned int idimu,
      float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_px_1(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_py_1(const unsigned int idimu,
      float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_py_1(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_pz_1(const unsigned int idimu,
      float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_pz_1(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_chrg_2(
      const unsigned int idimu, float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_chrg_2(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_px_2(const unsigned int idimu,
      float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_px_2(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_py_2(const unsigned int idimu,
      float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_py_2(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_pz_2(const unsigned int idimu,
      float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_pz_2(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_ndf(const unsigned int idimu,
      int newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_ndf(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_chisquare(
      const unsigned int idimu, float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_chisquare(newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::set_vtx_cov(short arrayid1,
      short arrayid2, const unsigned int idimu, float newVal)
  {
    PHdiMuoTrack* dimu = (PHdiMuoTrack*) diMuoTracks->UncheckedAt(idimu);
    if (dimu)
      dimu->set_vtx_cov(arrayid1, arrayid2, newVal);
  }

//________________________________________________________________
template<class PHMuoTracks>
  TObject*
  PHdiMuoTrackv5_Container<PHMuoTracks>::GetSingleDimuon(unsigned int ipart)
  {
    // di Muons only...
    return (TObject*) diMuoTracks->UncheckedAt(ipart);
  }

//________________________________________________________________
template<class PHMuoTracks>
  void
  PHdiMuoTrackv5_Container<PHMuoTracks>::AddPHDimuon(unsigned int ipart,
      TObject *o)
  {
    // di Muons only...
    AddPHDimuon(ipart);
    PHdiMuoTrack *destination = static_cast<PHdiMuoTrack*>(GetSingleDimuon(
        ipart));
    PHdiMuoTrack *source = static_cast<PHdiMuoTrack*>(o);
    *destination = *source;
  }

//________________________________________________________________
template<class PHMuoTracks>
  PHdiMuoTrackv5_Container<PHMuoTracks>*
  PHdiMuoTrackv5_Container<PHMuoTracks>::clone() const
  {
    //  Cloning is a complete copy...
    PHdiMuoTrackv5_Container<PHMuoTracks> *MyClone =
        new PHdiMuoTrackv5_Container;

    for (unsigned int i = 0; i < (unsigned int) PHMuoTracks::nMuoTracks; i++)
      {
        MyClone->AddPHParticle(i, PHMuoTracks::MuoTracks->UncheckedAt(i));
      }
    MyClone->set_npart(PHMuoTracks::nMuoTracks);

    for (unsigned int i = 0; i < (unsigned int) ndiMuoTracks; i++)
      {
        MyClone->AddPHDimuon(i, diMuoTracks->UncheckedAt(i));
      }
    MyClone->set_ndimu(ndiMuoTracks);

    return MyClone;
  }

#endif /* PHDIMUOTRACKV5_CONTAINER_H_ */
