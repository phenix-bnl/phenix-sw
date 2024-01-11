// $Id: TMutVtx.cxx,v 1.5 2011/12/29 20:19:31 slash Exp $
#include<TMutVtx.hh>
ClassImp(TMutVtx)


//
PHClassId::id_type TMutVtx::_class_id( 0 );

//_____________________________________________
double TMutVtx::get_ptot( void ) const
{ 
  return sqrt( 
    MUTOO::SQUARE( get_px() ) + 
    MUTOO::SQUARE( get_py() ) + 
    MUTOO::SQUARE( get_pz() ) ); 
}

//_____________________________________________
double TMutVtx::get_pt( void ) const
{ 
  return sqrt( 
    MUTOO::SQUARE( get_px() ) + 
    MUTOO::SQUARE( get_py() ) ); 
}

//_____________________________________________
double TMutVtx::get_rapidity( void ) const
{
  
  double px1 = get_px1();
  double py1 = get_py1();
  double pz1 = get_pz1();
  double px2 = get_px2();
  double py2 = get_py2();
  double pz2 = get_pz2();
  double pz = pz1 + pz2;

  double E1 = sqrt(px1*px1+py1*py1+pz1*pz1 + MUTOO::MASS_MUON_SQUARE);
  double E2 = sqrt(px2*px2+py2*py2+pz2*pz2 + MUTOO::MASS_MUON_SQUARE);
  double E = E1 + E2;
  
  double rapidity = 0.5*log((E+pz)/(E-pz));

  return rapidity;
}

//_________________________________________________________________________
double TMutVtx::get_mass() const 
{
  
  double px1 = get_px1();
  double py1 = get_py1();
  double pz1 = get_pz1();
  double px2 = get_px2();
  double py2 = get_py2();
  double pz2 = get_pz2();
  
  float netmom2 = 
    MUTOO::SQUARE(px1+px2) + 
    MUTOO::SQUARE(py1+py2) + 
    MUTOO::SQUARE(pz1+pz2); // square of the net mom. vector
  
  // energy of particles 1 and 2
  float E1 = sqrt(px1*px1+py1*py1+pz1*pz1 + MUTOO::MASS_MUON_SQUARE);
  float E2 = sqrt(px2*px2+py2*py2+pz2*pz2 + MUTOO::MASS_MUON_SQUARE);
  
  // calc. invariant mass squared of particles 1 and 2 and return mass if
  // it's valid
  float mass2 = (E1+E2)*(E1+E2) - netmom2;
  return (mass2 > 0) ? sqrt(mass2):0;
}

//_________________________________________________________________________
UShort_t TMutVtx::get_sign() const 
{
  if( get_charge1() == 1 && get_charge2() == 1)        return POSPOS;
  else if( get_charge1()*get_charge2() == -1)          return POSNEG;
  else if( get_charge1() == -1 && get_charge2() == -1) return NEGNEG;
  else {
    MUTOO::TRACE("Bad charge specification in TMutVtx::get_sign()");
    return 0;
  }
}
