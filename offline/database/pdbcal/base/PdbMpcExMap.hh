#ifndef __PDBMPCEXMAP_HH__
#define __PDBMPCEXMAP_HH__

#include "PdbCalChan.hh"
#include <iostream>

class PdbMpcExMap : public PdbCalChan {
 public:

  //! number of arms
  enum { NumberOfArms = 2 };
  
  //! number of layers
  enum { NumberOfLayers = 8 };
  
  //! number of FEM channels
  enum { NumberOfFEMChannels = 4 };

  //! Chain type enumeration
  enum Chain {U1=0,U2};

  //! Chain type enumeration
  enum TopBot {Top=0,Bot};

  //

  PdbMpcExMap();
  virtual ~PdbMpcExMap(){}

  void set_PacketID(long new_id){PacketID = new_id;}
  void set_arm(float new_arm){arm = new_arm;}

  void set_layer(int iChan, char new_layer)
  {if( (iChan>=0 && iChan<NumberOfFEMChannels) && 
       (new_layer>=0 && new_layer<NumberOfLayers) ) layer[iChan] = new_layer;}

  void set_top_bot(int iChan, char new_top_bot)
  {if( (iChan>=0 && iChan<NumberOfFEMChannels) && 
       (new_top_bot>=0 && new_top_bot<=1) ) top_bot[iChan] = new_top_bot;}

  void set_chain(int iChan, char new_chain)
  {if( (iChan>=0 && iChan<NumberOfFEMChannels) && 
       (new_chain>=0 && new_chain<=1) ) chain[iChan] = new_chain;}

  long get_PacketID(){return PacketID;}
  int get_arm(){return arm;}

  char get_layer(int iChan)
  {if( iChan>=0 && iChan<NumberOfFEMChannels ) return layer[iChan]; else return -1;}

  char get_top_bot(int iChan)
  {if( iChan>=0 && iChan<NumberOfFEMChannels ) return top_bot[iChan]; else return -1;}

  char get_chain(int iChan)
  {if( iChan>=0 && iChan<NumberOfFEMChannels ) return chain[iChan]; else return -1;}

  virtual void Reset();
  virtual void print() const;
  
private:


  long PacketID; // uniquely a FEM/DCM connection
  int arm;       // the PHENIX Arm (0,1)

  char layer[NumberOfFEMChannels];   // number between 0-7, the  MPC-EX layer
  char top_bot[NumberOfFEMChannels]; // top (0) or bottom (1) carrier board 
  char chain[NumberOfFEMChannels];   // either the U1 chain (0) or the U2 chain (1) on the carrier board 

  ClassDef(PdbMpcExMap,1);

};

#endif /* __PDBMPCEXMAP_HH__ */
