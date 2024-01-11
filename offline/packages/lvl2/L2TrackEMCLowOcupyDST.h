///////////////////////////////////////////////////////////
//							 //
//     Lvl2 PC1/EMC tracks in RUN3                       //
//							 //
//      Tony Frawley, from a template by Wei             //
//							 //
///////////////////////////////////////////////////////////

#ifndef __L2TrackEMCLowOcupyDST_H__
#define __L2TrackEMCLowOcupyDST_H__

#include <PHObject.h>

class L2TrackEMCLowOcupyDST : public PHObject 
{
 public:
  
  L2TrackEMCLowOcupyDST(){}
  virtual ~L2TrackEMCLowOcupyDST() {}
  virtual void    identify(std::ostream& os = std::cout) const
    {
      os << "virtual L2TrackEMCLowOcupyDST object";
    }
  virtual void    Reset() = 0;
  virtual int     isValid() const = 0;
  
  virtual unsigned int  get_NumTracks() const = 0;
  virtual void    set_NumTracks(unsigned int input)  = 0;
  virtual int     set_TClonesArraySize(unsigned int input) = 0;
  virtual void    AddL2TrackEMC(unsigned int icandidate) = 0;
  
  virtual void    set_arm(unsigned int i, int input) = 0; 
  virtual void    set_alpha(unsigned int i, float input) = 0; 
  virtual void    set_beta(unsigned int i, float input) = 0; 
  virtual void    set_p(unsigned int i, float input) = 0;
  virtual void    set_pxdet(unsigned int i, float input) = 0;
  virtual void    set_pydet(unsigned int i, float input) = 0;
  virtual void    set_pzdet(unsigned int i, float input) = 0;
  virtual void    set_pxphys(unsigned int i, float input) = 0;
  virtual void    set_pyphys(unsigned int i, float input) = 0;
  virtual void    set_pzphys(unsigned int i, float input) = 0;
  virtual void    set_zvertex(unsigned int i, float input) = 0;
  virtual void    set_xdet(unsigned int i, float input) = 0;
  virtual void    set_ydet(unsigned int i, float input) = 0;
  virtual void    set_zdet(unsigned int i, float input) = 0;
  virtual void    set_xdetout(unsigned int i, float input) = 0;
  virtual void    set_ydetout(unsigned int i, float input) = 0;
  virtual void    set_zdetout(unsigned int i, float input) = 0;
  virtual void    set_theta0(unsigned int i, float input) = 0;
  virtual void    set_phi0(unsigned int i, float input) = 0;
  virtual void    set_charge(unsigned int i, float input) = 0;
  virtual void    set_tileID(unsigned int i, int input) = 0;
  virtual void    set_energy(unsigned int i, float input) = 0;
  virtual void    set_CNTmatch(unsigned int i, int input) = 0;
  
  virtual int      get_arm(unsigned int i) const = 0; 
  virtual float    get_alpha(unsigned int i) const = 0; 
  virtual float    get_beta(unsigned int i) const = 0; 
  virtual float    get_p(unsigned int i) const = 0;
  virtual float    get_pxdet(unsigned int i) const = 0;
  virtual float    get_pydet(unsigned int i) const = 0;
  virtual float    get_pzdet(unsigned int i) const = 0;
  virtual float    get_pxphys(unsigned int i) const = 0;
  virtual float    get_pyphys(unsigned int i) const = 0;
  virtual float    get_pzphys(unsigned int i) const = 0;
  virtual float    get_zvertex(unsigned int i) const = 0;
  virtual float    get_xdet(unsigned int i) const = 0;
  virtual float    get_ydet(unsigned int i) const = 0;
  virtual float    get_zdet(unsigned int i) const = 0;
  virtual float    get_xdetout(unsigned int i) const = 0;
  virtual float    get_ydetout(unsigned int i) const = 0;
  virtual float    get_zdetout(unsigned int i) const = 0;
  virtual float    get_theta0(unsigned int i) const = 0;
  virtual float    get_phi0(unsigned int i) const = 0;
  virtual float    get_charge(unsigned int i) const = 0;
  virtual int      get_tileID(unsigned int i) const = 0;
  virtual float    get_energy(unsigned int i) const = 0;
  virtual int      get_CNTmatch(unsigned int i) const = 0;
  
  ClassDef (L2TrackEMCLowOcupyDST, 1)
};

#endif	// __ L2TrackEMCLowOcupyDST_H__
    
    
