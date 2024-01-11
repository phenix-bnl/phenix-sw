#ifndef __TFvtxCompactTRK_v1H__
#define __TFvtxCompactTRK_v1H__

/*!
\file TFvtxCompactTrk_v1.hh
\brief Compact version of the Forward Silicon (FVTX) Track object 
\author Cesar L. da Silva
\version $Revision: 1.4 $
\date $Date: 2012/11/29 04:22:22 $

*/

#include<TFvtxTrk.h>
#include<TFvtxCompactTrk.h>
#include<PHKey.hh>
#include<FVTXOO.h>
#include<PHPoint.h>

class TFvtxCompactTrk_v1 : public TFvtxCompactTrk
{
  
  public:
  
  //! constructor
  TFvtxCompactTrk_v1();
  
  //! destructor
  virtual ~TFvtxCompactTrk_v1(){;}
  
  //! constructor
  TFvtxCompactTrk_v1(const Key&,
		     const unsigned short& _arm,
		     const unsigned short& _index);
  //! constructor
  TFvtxCompactTrk_v1(const Key&, 
		    const unsigned short& _fvtx_x,
		    const unsigned short& _fvtx_y,
		    const unsigned short& _fvtx_z,
		    const unsigned short& _fvtx_phi,
		    const unsigned short& _fvtx_theta,
		    const unsigned short& _fvtx_chi2);

  //! constructor
  TFvtxCompactTrk_v1(const Key&,
		     const float& _fvtx_x,
		     const float& _fvtx_y,
		     const float& _fvtx_z,
		     const float& _fvtx_phi,
		     const float& _fvtx_theta,
		     const float& _fvtx_chi2);

  //! constructor
  TFvtxCompactTrk_v1(const Key&, 
		    const TFvtxTrk* base_ref);
  
  //! constructor 
  TFvtxCompactTrk_v1(const TFvtxCompactTrk* base_ptr);

  //! constructor 
  TFvtxCompactTrk_v1(const TFvtxCompactTrk& base_ref);

  //! constructor from TFvtxTrk
  TFvtxCompactTrk_v1(const TFvtxTrk* base_ptr);

  //! get arm
  unsigned short get_arm() const
  {return arm;}

  //!get index
  unsigned short get_index() const
  {return index;}

  //! return the raw FVTX track vertex X
  unsigned short get_fvtx_vtx_x() const
  {return fvtx_vtx_x;}

  //! return the raw FVTX track vertex Y
  unsigned short get_fvtx_vtx_y() const
  {return fvtx_vtx_y;}

  //! return the raw FVTX track vertex Z
  unsigned short get_fvtx_vtx_z() const
  {return fvtx_vtx_z;}

  //! return raw FVTX track phi
  unsigned short get_fvtx_short_phi() const
  {return fvtx_phi;}

  //! return raw FVTX track theta
  unsigned short get_fvtx_short_theta() const
  {return fvtx_theta;}

  //! return raw chi2/NDF from residuals for the track
  unsigned short get_short_chi2_ndf() const
  {return chi2_ndf;}

  //! return the FVTX track vertex
  PHPoint get_fvtx_vtx() const;

  //! return FVTX track phi
  float get_fvtx_phi() const;

  //! return FVTX track theta
  float get_fvtx_theta() const;

  //! return FVTX track eta
  float get_fvtx_eta() const;

  //! return chi2/NDF from residuals for the track
  float get_chi2_ndf() const;

  //! set the track (vertex and momentum vector)
  void set_track_vtx( PHPoint fvtx_trk_vtx );

  //! set FVTX track phi
  void set_fvtx_phi( float phi );

  //! set FVTX track theta
  void set_fvtx_theta( float theta );

  //! set FVTX track eta
  void set_fvtx_eta( float eta );

  //! set the chi2/NDF from residuals for the track
  void set_chi2_ndf( float _chi2 );
    
  void print(std::ostream& os = std::cout) const;
  
  //! convert float to short with half precision
  unsigned short FloatToShort( const float a ) const;

  //! convert short to float with half precision
  float ShortToFloat( const unsigned short a ) const;

  private:	
  
  bool arm;
  unsigned short index;
  unsigned short fvtx_vtx_x;
  unsigned short fvtx_vtx_y;
  unsigned short fvtx_vtx_z;
  unsigned short fvtx_phi;
  unsigned short fvtx_theta;
  unsigned short chi2_ndf;

  ClassDef(TFvtxCompactTrk_v1,1)
};

#endif /* __TFVTXCompactTrk_v1H__*/



