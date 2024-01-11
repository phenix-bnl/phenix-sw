///////////////////////////////////////////////////////////
//							 //
//     Lvl2 two point tracker tracks in RUN3 
//							 //
//      Author:  Tony Frawley (from template by Wei) //
//							 //
///////////////////////////////////////////////////////////

#ifndef __L2TrackEMCLowOcupyMicroV1_H__
#define __L2TrackEMCLowOcupyMicroV1_H__

#include <L2TrackEMCLowOcupyDST.h>
#include <iostream>
#include <phool.h>
#include <TClonesArray.h>

class L2TrackEMCLowOcupyMicroV1 : public L2TrackEMCLowOcupyDST
{
  public:

    L2TrackEMCLowOcupyMicroV1();
     ~L2TrackEMCLowOcupyMicroV1();
     void    identify(std::ostream& os = std::cout) const;
     void    Reset();
     int     isValid() const;

     unsigned int  get_NumTracks() const {return NumTracks;}
     void    set_NumTracks(unsigned int input) {NumTracks=input;}
     int     set_TClonesArraySize(unsigned int input);
     void    AddL2TrackEMC(unsigned int itrack);

     void    set_arm(unsigned int i, int input); 
     void    set_alpha(unsigned int i, float input); 
     void    set_beta(unsigned int i, float input); 
     void    set_p(unsigned int i, float input);
     void    set_pxdet(unsigned int i, float input);
     void    set_pydet(unsigned int i, float input);
     void    set_pzdet(unsigned int i, float input);
     void    set_pxphys(unsigned int i, float input);
     void    set_pyphys(unsigned int i, float input);
     void    set_pzphys(unsigned int i, float input);
     void    set_zvertex(unsigned int i, float input);
     void    set_xdet(unsigned int i, float input);
     void    set_ydet(unsigned int i, float input);
     void    set_zdet(unsigned int i, float input);
     void    set_xdetout(unsigned int i, float input);
     void    set_ydetout(unsigned int i, float input);
     void    set_zdetout(unsigned int i, float input);
     void    set_theta0(unsigned int i, float input);
     void    set_phi0(unsigned int i, float input);
     void    set_charge(unsigned int i, float input);
     void    set_tileID(unsigned int i, int input);
     void    set_energy(unsigned int i, float input);
     void    set_CNTmatch(unsigned int i, int input);

     int      get_arm(unsigned int i) const; 
     float    get_alpha(unsigned int i) const; 
     float    get_beta(unsigned int i) const; 
     float    get_p(unsigned int i) const;
     float    get_pxdet(unsigned int i) const;
     float    get_pydet(unsigned int i) const;
     float    get_pzdet(unsigned int i) const;
     float    get_pxphys(unsigned int i) const;
     float    get_pyphys(unsigned int i) const;
     float    get_pzphys(unsigned int i) const;
     float    get_zvertex(unsigned int i) const;
     float    get_xdet(unsigned int i) const;
     float    get_ydet(unsigned int i) const;
     float    get_zdet(unsigned int i) const;
     float    get_xdetout(unsigned int i) const;
     float    get_ydetout(unsigned int i) const;
     float    get_zdetout(unsigned int i) const;
     float    get_theta0(unsigned int i) const;
     float    get_phi0(unsigned int i) const;
     float    get_charge(unsigned int i) const;
     int      get_tileID(unsigned int i) const;
     float    get_energy(unsigned int i) const;
     int      get_CNTmatch(unsigned int i) const;

 protected:

     TClonesArray *GetL2Track() const {return L2Track;}
     unsigned int NumTracks;
     TClonesArray *L2Track;

    ClassDef (L2TrackEMCLowOcupyMicroV1, 1)
};
#endif	// __ L2TrackEMCLowOcupyMicroV1_H__


