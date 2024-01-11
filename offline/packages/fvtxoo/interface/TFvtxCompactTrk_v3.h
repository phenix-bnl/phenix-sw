#ifndef __TFvtxCompactTRK_v2H__
#define __TFvtxCompactTRK_v2H__

/*!
\file TFvtxCompactTrk_v3.hh
\brief Compact version of the Forward Silicon (FVTX) Track object 
\author Cesar L. da Silva
\version $Revision: 1.3 $
\date $Date: 2013/12/17 22:03:39 $

*/

#include<TFvtxTrk.h>
#include<TFvtxCompactTrk.h>
#include<PHKey.hh>
#include<FVTXOO.h>
#include<PHPoint.h>

class TFvtxCompactTrk_v3 : public TFvtxCompactTrk
{
  
  public:
  
  //! constructor
  TFvtxCompactTrk_v3();
  
  //! destructor
  virtual ~TFvtxCompactTrk_v3(){;}
  
  //! constructor
  TFvtxCompactTrk_v3(const Key&,
		     const unsigned short& _arm,
		     const unsigned short& _index);
  //! constructor
  TFvtxCompactTrk_v3(const Key&, 
		    const unsigned short& _fvtx_x,
		    const unsigned short& _fvtx_y,
		    const unsigned short& _fvtx_z,
		    const unsigned short& _fvtx_phi,
		    const unsigned short& _fvtx_theta,
		    const unsigned short& _fvtx_chi2);

  //! constructor
  TFvtxCompactTrk_v3(const Key&,
		     const float& _fvtx_x,
		     const float& _fvtx_y,
		     const float& _fvtx_z,
		     const float& _fvtx_phi,
		     const float& _fvtx_theta,
		     const float& _fvtx_chi2);

  //! constructor
  TFvtxCompactTrk_v3(const Key&, 
		    const TFvtxTrk* base_ref);
  
  //! constructor 
  TFvtxCompactTrk_v3(const TFvtxCompactTrk* base_ptr);

  //! constructor 
  TFvtxCompactTrk_v3(const TFvtxCompactTrk& base_ref);

  //! constructor from TFvtxTrk
  TFvtxCompactTrk_v3(const TFvtxTrk* base_ptr);

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

  //! return hit pattern
  unsigned char get_hit_pattern() const
  {return hit_pattern;}

  //!check if plane has a hit
  bool has_hit(short station) const
  { return hit_pattern & (1 << station);}

  //!return number of hits
  short get_nhits()
  {
    short nhits = 0;
    for (int i=0; i<8; i++)
      nhits += has_hit(i);
    return nhits;
  }

  //!return number of degrees of freedom
  short get_NDF()
  {
    return get_nhits()-2;
  }

  //! get the covariant matrix element
  float get_cov(short i, short j) const;

  //! get the celement of the covariant array as it is stored
  float get_cov(short i) const;

  //! get covariant array content
  unsigned short get_short_cov(short i) const
  { return fvtx_cov[i]; }

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

  //! set the chi2/NDF from residuals for the track
  void set_hit_pattern( unsigned char a ) { hit_pattern = a;}

  //! set covariant matrix for this track
  void set_cov(short i, short j, float a);
    
  void print(std::ostream& os = std::cout) const;
  
  //! convert float to short with half precision
  unsigned short FloatToShort( const float a ) const;

  //! convert short to float with half precision
  float ShortToFloat( const unsigned short a ) const;

  protected:

  short get_cov_index(short i, short j) const; // return covariant array index

  bool arm;
  unsigned short index;
  unsigned short fvtx_vtx_x;
  unsigned short fvtx_vtx_y;
  unsigned short fvtx_vtx_z;
  unsigned short fvtx_phi;
  unsigned short fvtx_theta;
  unsigned short chi2_ndf;
  unsigned char hit_pattern;
  static const size_t ncovel = 15; // number of covariant elements 
  std::vector<unsigned short> fvtx_cov; 

  ClassDef(TFvtxCompactTrk_v3,1)
};

#endif /* __TFVTXCompactTrk_v3H__*/



