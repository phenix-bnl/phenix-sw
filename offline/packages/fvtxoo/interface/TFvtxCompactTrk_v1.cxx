/*!
\file TFvtxCompactTrk_v1.cxx
\brief Compact version of the Forward Silicon (FVTX) Track object 
\author Cesar L. da Silva
\version $Revision: 1.8 $
\date $Date: 2015/04/08 17:26:18 $
*/

#include<TFvtxCompactTrk_v1.h>
#include<PHGeometry.h>
#include<algorithm>
#include<iomanip>

#include <half/half.h>

ClassImp(TFvtxCompactTrk_v1)

using namespace std;

const unsigned short wild_short = 0xffff;
const float wild_float = -9999.9;

//_______________________________________
TFvtxCompactTrk_v1::TFvtxCompactTrk_v1() :
  arm(false),
  index(0),
  fvtx_vtx_x(wild_short),
  fvtx_vtx_y(wild_short),
  fvtx_vtx_z(wild_short),
  fvtx_phi(wild_short),
  fvtx_theta(wild_short),
  chi2_ndf(wild_short)
{
}

//_______________________________________
TFvtxCompactTrk_v1::TFvtxCompactTrk_v1(const Key& key,
				       const unsigned short& _arm,
				       const unsigned short& _index) :
  TFvtxCompactTrk(key),
  arm(_arm>0),
  index(_index),
  fvtx_vtx_x(wild_short),
  fvtx_vtx_y(wild_short),
  fvtx_vtx_z(wild_short),
  fvtx_phi(wild_short),
  fvtx_theta(wild_short),
  chi2_ndf(wild_short)
{
}

//_______________________________________
TFvtxCompactTrk_v1::TFvtxCompactTrk_v1(const Key& key,
				     const unsigned short& _fvtx_x,
				     const unsigned short& _fvtx_y,
				     const unsigned short& _fvtx_z,
				     const unsigned short& _fvtx_phi,
				     const unsigned short& _fvtx_theta,
				     const unsigned short& _fvtx_chi2) :  
  TFvtxCompactTrk(key),
  arm(0),
  index(0),
  fvtx_vtx_x(_fvtx_x),
  fvtx_vtx_y(_fvtx_y),
  fvtx_vtx_z(_fvtx_z),
  fvtx_phi(_fvtx_phi),
  fvtx_theta(_fvtx_theta),
  chi2_ndf(_fvtx_chi2)
{
}

//_______________________________________
TFvtxCompactTrk_v1::TFvtxCompactTrk_v1(const Key& key,
				     const float& _fvtx_x,
				     const float& _fvtx_y,
				     const float& _fvtx_z,
				     const float& _fvtx_phi,
				     const float& _fvtx_theta,
				     const float& _fvtx_chi2) :
  TFvtxCompactTrk(key),
  arm(0),
  index(0),
  fvtx_vtx_x(FloatToShort(_fvtx_x)),
  fvtx_vtx_y(FloatToShort(_fvtx_y)),
  fvtx_vtx_z(FloatToShort(_fvtx_z)),
  fvtx_phi(FloatToShort(_fvtx_phi)),
  fvtx_theta(FloatToShort(_fvtx_theta)),
  chi2_ndf(FloatToShort(_fvtx_chi2))
{
}


//_______________________________________
TFvtxCompactTrk_v1::TFvtxCompactTrk_v1(const TFvtxCompactTrk* base_ptr) :
  TFvtxCompactTrk(*base_ptr),
  arm(base_ptr->get_arm()),
  index(base_ptr->get_index()),
  fvtx_vtx_x(base_ptr->get_fvtx_vtx_x()),
  fvtx_vtx_y(base_ptr->get_fvtx_vtx_y()),
  fvtx_vtx_z(base_ptr->get_fvtx_vtx_z()),
  fvtx_phi(base_ptr->get_fvtx_short_phi()),
  fvtx_theta(base_ptr->get_fvtx_short_theta()),
  chi2_ndf(base_ptr->get_short_chi2_ndf())
{
}

//_______________________________________
TFvtxCompactTrk_v1::TFvtxCompactTrk_v1(const TFvtxCompactTrk& base_ptr) :
  TFvtxCompactTrk(base_ptr),
  arm(base_ptr.get_arm()),
  index(base_ptr.get_index()),
  fvtx_vtx_x(base_ptr.get_fvtx_vtx_x()),
  fvtx_vtx_y(base_ptr.get_fvtx_vtx_y()),
  fvtx_vtx_z(base_ptr.get_fvtx_vtx_z()),
  fvtx_phi(base_ptr.get_fvtx_short_phi()),
  fvtx_theta(base_ptr.get_fvtx_short_theta()),
  chi2_ndf(base_ptr.get_short_chi2_ndf())
{
}

//_______________________________________
TFvtxCompactTrk_v1::TFvtxCompactTrk_v1(const TFvtxTrk* base_ptr) :
  TFvtxCompactTrk()
{
  arm = base_ptr->get_arm();
  index = base_ptr->get_index();
  TMutTrkPar _trk_par_vtx = *(base_ptr->get_trk_par_vtx());
  fvtx_vtx_x = FloatToShort(_trk_par_vtx.get_x());
  fvtx_vtx_y = FloatToShort(_trk_par_vtx.get_y());
  fvtx_vtx_z = FloatToShort(_trk_par_vtx.get_z());

  float fvtx_px = _trk_par_vtx.get_px();
  float fvtx_py = _trk_par_vtx.get_py();
  float fvtx_pz = _trk_par_vtx.get_pz();
  float phi = atan2(fvtx_py, fvtx_px);
  float theta = atan(sqrt(fvtx_px*fvtx_px + fvtx_py*fvtx_py)/fvtx_pz);
  fvtx_phi = FloatToShort(phi);
  fvtx_theta = FloatToShort(theta);

  chi2_ndf = FloatToShort(base_ptr->get_w_chi_square()/base_ptr->get_ndf());
  
}

//_______________________________________
TFvtxCompactTrk_v1::TFvtxCompactTrk_v1(const Key& key,
				       const TFvtxTrk* base_ptr) :
  TFvtxCompactTrk(key)
{
  arm = base_ptr->get_arm();
  index = base_ptr->get_index();
  TMutTrkPar _trk_par_vtx = *(base_ptr->get_trk_par_vtx());
  fvtx_vtx_x = FloatToShort(_trk_par_vtx.get_x());
  fvtx_vtx_y = FloatToShort(_trk_par_vtx.get_y());
  fvtx_vtx_z = FloatToShort(_trk_par_vtx.get_z());

  float fvtx_px = _trk_par_vtx.get_px();
  float fvtx_py = _trk_par_vtx.get_py();
  float fvtx_pz = _trk_par_vtx.get_pz();
  float phi = atan2(fvtx_py, fvtx_px);
  float theta = atan(sqrt(fvtx_px*fvtx_px + fvtx_py*fvtx_py)/fvtx_pz);
  fvtx_phi = FloatToShort(phi);
  fvtx_theta = FloatToShort(theta);

  chi2_ndf = FloatToShort(base_ptr->get_w_chi_square()/base_ptr->get_ndf());
}

//_______________________________________
void TFvtxCompactTrk_v1::print(std::ostream& os) const
{
  FVTXOO::PRINT(os,GetName());

  os
    << " key: " << get_key().get_obj_key() << "    ";

  // dump track parameters
  //
  os << " FVTX compact track = {";
  os << setw(5) << setprecision(3) << setiosflags(ios::showpoint) << setiosflags(ios::fixed);

  os << get_fvtx_vtx().getX() << ", ";
  os << get_fvtx_vtx().getY() << ", ";
  os << get_fvtx_vtx().getZ() << ", ";
  os << get_fvtx_phi() << ", ";
  os << get_fvtx_theta() << "}" << std::endl;
  os << " chi2/NDF:" << get_chi2_ndf() << std::endl;
  os << std::fixed;
  FVTXOO::PRINT(os,"**");
}

unsigned short TFvtxCompactTrk_v1::FloatToShort( const float rval ) const
{
  if (rval == wild_float) return wild_short;
  half ftoi;
  ftoi.setBits(0); //force to initialize internal short variable
  ftoi = rval;
 return ftoi.bits();
}

float TFvtxCompactTrk_v1::ShortToFloat( const unsigned short ival ) const
{
  if (ival == wild_short) return wild_float;
  half halfvar;
  halfvar.setBits(ival);
  return halfvar;
}

 PHPoint TFvtxCompactTrk_v1::get_fvtx_vtx() const
 {
   PHPoint pnt(ShortToFloat(fvtx_vtx_x),
	       ShortToFloat(fvtx_vtx_y),
	       ShortToFloat(fvtx_vtx_z));
   return pnt;
 }

float TFvtxCompactTrk_v1::get_fvtx_phi() const
{
  return ShortToFloat(fvtx_phi);
}

float TFvtxCompactTrk_v1::get_fvtx_theta() const
{
  return ShortToFloat(fvtx_theta);
}

float TFvtxCompactTrk_v1::get_fvtx_eta() const
{
  return -log(tan(get_fvtx_theta()/2));
}

float TFvtxCompactTrk_v1::get_chi2_ndf() const
{
  return ShortToFloat(chi2_ndf);
}

void TFvtxCompactTrk_v1::set_track_vtx( PHPoint pnt )
{
  fvtx_vtx_x = FloatToShort(pnt.getX());
  fvtx_vtx_y = FloatToShort(pnt.getY());
  fvtx_vtx_z = FloatToShort(pnt.getZ());
}

void TFvtxCompactTrk_v1::set_fvtx_phi( float phi )
{
  fvtx_phi = FloatToShort(phi);
}

void TFvtxCompactTrk_v1::set_fvtx_theta( float theta )
{
  fvtx_theta = FloatToShort(theta);
}

void TFvtxCompactTrk_v1::set_fvtx_eta( float eta )
{
  fvtx_theta = FloatToShort(2*atan(exp(-eta)));
}

void TFvtxCompactTrk_v1::set_chi2_ndf( float _chi2 )
{
  chi2_ndf = FloatToShort(_chi2);
}
