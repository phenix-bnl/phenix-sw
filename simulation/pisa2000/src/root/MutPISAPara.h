#ifndef _MutPISAPARA_
#define _MutPISAPARA_

/*!
  \file  MutPISAPara.h
  \brief event independent MUT parameters
  \author  T. K. Ghosh
  \version $Revision: 1.5 $
  \date    $Date: 2007/11/13 22:27:51 $
*/

#include <TObject.h>
#include <vector>

//! event independent MUT parameters
class MutPISAPara : public TObject 
{

  private:
  
  Int_t mumtrflg;
  Int_t mum_arms;
  Int_t mum_stations;
  Int_t mum_channels;
  Int_t mum_color;
  Int_t ArmMedium;
  Int_t HoneyMedium;
  Int_t FEEFlag;     
  Int_t StationMedium;
  Int_t PlanesPerStation[6];
  Float_t StationOneFrame[8]; 
  Float_t StationOnePanel[8]; 
  Float_t StationOneAnode[4]; 
  Float_t StationOneRead[8]; 
  Float_t StationOneRib[7]; 
  Float_t StationOneGas[8]; 
  Float_t StationOneMount[7]; 
  Float_t StationOneFee[10]; 
  Float_t StationOneAngles[48]; 
  Float_t StationOneOffsets[24]; 
  Float_t StationTwoFFrame[14]; 
  Float_t StationTwoBFrame[4]; 
  Float_t StationTwoFBar[4]; 
  Float_t StationTwoBBar[4]; 
  Float_t StationTwoAnode[2]; 
  Float_t StationTwoFoilThickness; 
  Float_t StationTwoAlFoilThickness; 
  Float_t StationTwoFRib[8]; 
  Float_t StationTwoBRib[8]; 
  Float_t StationTwoFGas[12]; 
  Float_t StationTwoBGas[2]; 
  Float_t StationTwoAngles[96]; 
  Float_t StationTwoOffsets[48]; 
  Float_t StationThreeFrame[18]; 
  Float_t StationThreePanel[14]; 
  Float_t StationThreeAnode[10]; 
  Float_t StationThreeRib[8]; 
  Float_t StationThreeGas[12]; 
  Float_t StationThreeAngles[96]; 
  Float_t StationThreeOffsets[48]; 
  Int_t   FrameMedium[6]; 
  Float_t StationNominalZpos[8]; 
  Int_t   SpokeMedium; 
  
  Int_t mu_arm_medium;
  Float_t mt_chm_thick1[5];
  Float_t mt_chm_thick2[5];
  Int_t mt_frame_medium1[5];
  Int_t mt_frame_medium2[5];
  Float_t mt_frame_side_thick1[5];
  Float_t mt_frame_side_thick2[5];
  Float_t mt_frame_end_thick1[5];
  Float_t mt_frame_end_thick2[5];
  Int_t mt_planes_per_station1[5];
  Int_t mt_planes_per_station2[5];
  Int_t mt_station_medium1[5];
  Int_t mt_station_medium2[5];
  Float_t mt_plane_thickness1[5];
  Float_t mt_plane_thickness2[5];
  Float_t mt_plane_spacing11[10];
  Float_t mt_plane_spacing12[10];
  Float_t mt_plane_spacing13[10];
  Float_t mt_plane_spacing14[10];
  Float_t mt_plane_spacing15[10];
  Float_t mt_plane_spacing21[10];
  Float_t mt_plane_spacing22[10];
  Float_t mt_plane_spacing23[10];
  Float_t mt_plane_spacing24[10];
  Float_t mt_plane_spacing25[10];
  Float_t mt_station_z1[5];
  Float_t mt_station_z2[5];
  Float_t mt_station_inner_radius1[5];
  Float_t mt_station_inner_radius2[5];
  Float_t mt_station_outer_radius1[5];
  Float_t mt_station_outer_radius2[5];
  
  //! static interface
  static std::vector<MutPISAPara> _hits;

  public:
  
  //! constructor
  MutPISAPara() 
  {}  
  
  //! constructor
  MutPISAPara(const Int_t i[], const Float_t f[]);  

  //! destructor
  virtual ~MutPISAPara() 
  {}
  
  //!@name static interface
  //@{
  
  static Int_t GetMutParaCount() 
  {return _hits.size(); }
    
  static MutPISAPara *GetMutParaEvt() 
  {return _hits.empty() ? 0:&_hits[0]; }
   
  static void AddHit( const MutPISAPara& hit )
  { _hits.push_back( hit ); }
  
  static void MutParaClear()
  { _hits.clear(); }
  
  //@}

  //!@name accessors
  //@{
  
  Int_t GetMutParaFlag() 
  {return mumtrflg; }
  
  Int_t GetMutParaArms()
  {return mum_arms; }
  
  Int_t GetMutParaStations() 
  {return mum_stations; }
  
  Int_t GetMutParaPlanes(Int_t iarm, Int_t istation) 
  { return PlanesPerStation[iarm*3+istation]; }
  
  Float_t GetMutParaZpos(Int_t iarm, Int_t istation) 
  { return StationNominalZpos[iarm*4 + istation]; }
      
  Float_t GetMutParaOneFrame(Int_t iarray) 
  { return StationOneFrame[iarray]; }
  
  Float_t GetMutParaOnePanel(Int_t iarray) 
  { return StationOnePanel[iarray]; }
  
  Float_t GetMutParaOneAnode(Int_t iarray) 
  { return StationOneAnode[iarray]; }
  
  Float_t GetMutParaOneMount(Int_t iarray)
  { return StationOneMount[iarray]; }
  
  Float_t GetMutParaOneAngles(Int_t iarm, Int_t iquad, Int_t iarray) 
  { return StationOneAngles[iarm*4*6 + iquad*6 + iarray]; }
  
  Float_t GetMutParaOneOffsets(Int_t iarm, Int_t iquad, Int_t iarray)
  { return StationOneOffsets[iarm*4*3 + iquad*3 + iarray]; }
          
  Float_t GetMutParaTwoFFrame(Int_t iarm, Int_t iarray) 
  { return StationTwoFFrame[iarm*7 + iarray]; }
  
  Float_t GetMutParaTwoAnode(Int_t iarm) 
  { return StationTwoAnode[iarm]; }
  
  Float_t GetMutParaTwoFoilThickness() 
  { return StationTwoFoilThickness; }
  
  Float_t GetMutParaTwoAlFoilThickness() 
  { return StationTwoAlFoilThickness; }
  
  Float_t GetMutParaTwoAngles(Int_t iarm, Int_t ioctant, Int_t iarray) 
  { return StationTwoAngles[iarm*8*6 + ioctant*6 + iarray]; }
  
  Float_t GetMutParaTwoOffsets(Int_t iarm, Int_t ioctant, Int_t iarray) 
  { return StationTwoOffsets[iarm*8*3 + ioctant*3 + iarray]; }
                
  Float_t GetMutParaThreeFrame(Int_t iarm, Int_t iarray) 
  { return StationThreeFrame[iarm*9 + iarray]; }
  
  Float_t GetMutParaThreePanel(Int_t iarm, Int_t iarray)
  { return StationThreePanel[iarm*7 + iarray]; }
  
  Float_t GetMutParaThreeAnode(Int_t iarm, Int_t iarray) 
  { return StationThreeAnode[iarm*5 + iarray]; }
  Float_t GetMutParaThreeAngles(Int_t iarm, Int_t ioctant, Int_t iarray) 
  { return StationThreeAngles[iarm*8*6 + ioctant*6 + iarray]; }
  
  Float_t GetMutParaThreeOffsets(Int_t iarm, Int_t ioctant, Int_t iarray) 
  { return StationThreeOffsets[iarm*8*3 + ioctant*3 + iarray]; }

  //@}
               
  ClassDef(MutPISAPara,1)
};

#endif


