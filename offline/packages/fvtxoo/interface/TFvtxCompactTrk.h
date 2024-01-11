/*!
  \file TFvtxCompactTrk.h
  \brief The Forward Silicon (FVTX) Compact Track object 
  \author Cesar L. da Silva
  \version $Revision: 1.9 $
  \date    $Date: 2018/04/11 18:05:18 $
*/

#ifndef __TFVTXCOMPACTTRK_H__
#define __TFVTXCOMPACTTRK_H__

#include<TDataType.h>
#include<PHPoint.h>
#include<PHKey.hh>

class TFvtxCompactTrk : public PHKey
{
  
 public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default constructor */
  TFvtxCompactTrk()
  {}

  /*! Destructor */
  virtual ~TFvtxCompactTrk()
  {}

  /*! Construct with key and location */
  TFvtxCompactTrk(const Key& key) : PHKey(key) 
  {} 

  //@}
  //! return arm
  virtual unsigned short get_arm() const
  {return 0;}

  //! return index
  virtual unsigned short get_index() const
  {return 0;}

  //! return the raw FVTX track vertex X
  virtual unsigned short get_fvtx_vtx_x() const
  {return 999;}

  //! return the raw FVTX track vertex Y
  virtual unsigned short get_fvtx_vtx_y() const
  {return 999;}

  //! return the raw FVTX track vertex Z
  virtual unsigned short get_fvtx_vtx_z() const
  {return 999;}

  //! return raw FVTX track phi
  virtual unsigned short get_fvtx_short_phi() const
  {return 999;}

  //! return raw FVTX track theta
  virtual unsigned short get_fvtx_short_theta() const
  {return 999;}

  //! return raw chi2/NDF from residuals for the track
  virtual unsigned short get_short_chi2_ndf() const
  {return 999;}

  //! return the FVTX track vertex
  virtual PHPoint get_fvtx_vtx() const
  {PHPoint pnt(-999.,-999.,-999.); return pnt;}

  //! return FVTX track phi
  virtual float get_fvtx_phi() const
  {return -999.;}

  //! return FVTX track theta
  virtual float get_fvtx_theta() const
  {return -999.;}

  //! return FVTX track eta
  virtual float get_fvtx_eta() const
  {return -999.;}

  //! return chi2/NDF from residuals for the track
  virtual float get_chi2_ndf() const
  {return 999.;}

  /*! get the hit pattern
    FVTX4_0 FVTX4_1 FVTX3_0 FVTX3_1 FVTX2_0 FVTX2_1 FVTX1_0 FVTX1_1
  */
  virtual unsigned char get_hit_pattern() const
  {return 0;}

  /*! get the SVX hit pattern
    NA NA NA NA VTX4 VTX3 VTX2 VTX1
  */
  virtual unsigned char get_svxhit_pattern() const
  {return 0;}

  //!check if plane has a hit
  virtual bool has_hit(short station) const
  { return 0;}

  //!check if SVX layer has a hit
  virtual bool has_svxhit(short layer) const
  { return false;}

  //!return number of hits
  virtual short get_nhits()
  { return 0;}

  //!return number of degrees of freedom
  virtual short get_NDF()
  { return 0; }

  //! get the covariant matrix element
  virtual float get_cov(short i, short j) const
  {return -999.;}

  //! get the celement of the covariant array as it is stored
  virtual float get_cov(short i) const
  {return -999.;}

  //! get covariant array content
  virtual unsigned short get_short_cov(short i) const
  {return 999;}

  //! set the track (vertex and momentum vector)
  virtual void set_track_vtx( PHPoint fvtx_trk_vtx )
  {}

  //! set FVTX track phi
  virtual void set_fvtx_phi( float phi )
  {}

  //! set FVTX track theta
  virtual void set_fvtx_theta( float theta )
  {}

  //! set FVTX track eta
  virtual void set_fvtx_eta( float eta )
  {}

  //! set the chi2?NDF from residuals for the track
  virtual void set_chi2_ndf( float _chi2 )
  {}
    
  /*! set the hit pattern
    FVTX4_0 FVTX4_1 FVTX3_0 FVTX3_1 FVTX2_0 FVTX2_1 FVTX1_0 FVTX1_1
  */
  virtual void set_hit_pattern( unsigned char a )
  {}

  /*! set the hit pattern
    NA NA NA NA VTX4 VTX3 VTX2 VTX1
  */
  virtual void set_svxhit_pattern( unsigned char a )
  {}
  
  //! set covariant matrix for this track
  virtual void set_cov(short i, short j, float a)
  {}

  virtual void print(std::ostream& os = std::cout) const
  {}
  
  protected:

  ClassDef(TFvtxCompactTrk,1)
};
  
#endif /* __TFVTXTRK_H__*/



