///////////////////////////////////////////////////////////
//							 //
//     Lvl2 two point tracker tracks in RUN3 
//							 //
//      Author:  Tony Frawley (from template by Wei) //
//							 //
///////////////////////////////////////////////////////////

#ifndef __L2TrackPC3LowOcupyMicroV1_H__
#define __L2TrackPC3LowOcupyMicroV1_H__

#include <L2TrackPC3LowOcupyDST.h>
#include <TClonesArray.h>
#include <phool.h>
#include <iostream>

class L2TrackPC3LowOcupyMicroV1 : public L2TrackPC3LowOcupyDST
{
  public:

    L2TrackPC3LowOcupyMicroV1();
     ~L2TrackPC3LowOcupyMicroV1();
     void    identify(std::ostream& os = std::cout) const;
     void    Reset();
     int     isValid() const;

     unsigned int  get_NumTracks() const {return NumTracks;}
     void    set_NumTracks(unsigned int input) {NumTracks=input;}
     int     set_TClonesArraySize(unsigned int input);
     void    AddL2TrackPC3(unsigned int icandidate);

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
     void    set_regionPC3(unsigned int i, unsigned int input);
     void    set_clustIDPC3(unsigned int i, unsigned int input);
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
     unsigned int get_regionPC3(unsigned int i) const;
     unsigned int get_clustIDPC3(unsigned int i) const;
     int      get_CNTmatch(unsigned int i) const;

 protected:

     TClonesArray *GetL2Track() const {return L2Track;}
     unsigned int NumTracks;
     TClonesArray *L2Track;

    ClassDef (L2TrackPC3LowOcupyMicroV1, 1)
};
#endif	// __ L2TrackPC3LowOcupyMicroV1_H__


