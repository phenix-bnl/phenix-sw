#ifndef _PISAEVENTHEADER_
#define _PISAEVENTHEADER_

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// PISAEventHeader.h                                                    //
//                                                                      //
// Description of the PISA event header                                 //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TObject.h"
#include "TClonesArray.h"


class PISAEventHeader 
{

  public:
  
  //! constructor
  PISAEventHeader();
  
  //! destructor
  virtual ~PISAEventHeader();
  
  void   Set(int i[], float f[]);
  Int_t  GetIDRun() const 
  { return idrun; }
  
  Int_t  GetNptls() const 
  { return nptls; }
  
  Int_t  GetEvent() const 
  { return ntru_evt; }
  
  void   SetEvent(int jEvent)
  {ntru_evt = jEvent; }
  
  Int_t  GetIpopsub() const 
  { return ipopsub; }
  
  Int_t  GetNsubevent() const 
  { return nsubevent; }
  
  Int_t  GetNsub_evt() const 
  { return nsub_evt; }
  

  // Header information assumes one input file
  // May have to adjust for MERGE
  
  // using relic xyz integer array from PISA  (ZEBRA leftover unfortunately)
  Float_t GetXvertex() const 
  { return 0.0001*xyz1000[0]; }
  
  Float_t GetYvertex() const 
  { return 0.0001*xyz1000[1]; }
  
  Float_t GetZvertex() const 
  { return 0.0001*xyz1000[2]; }
  
  void SetXvertex(Float_t x) 
  { xyz1000[0] = int(10000*x); }
  
  void SetYvertex(Float_t y) 
  { xyz1000[1] = int(10000*y); }
  
  void SetZvertex(Float_t z) 
  { xyz1000[2] = int(10000*z); }
  
  Int_t GetT0femto() const { return t0femto; }
  Int_t GetIgdate() const {return igdate; }
  Int_t GetAtarg() const { return atarg; }
  Int_t GetZtarg() const { return ztarg; }
  Int_t GetAproj() const { return aproj; }
  Int_t GetZproj() const { return zproj; }
  Int_t GetIgtime() const { return igtime; }
  Int_t GetNrndm0() const { return nrndm[0]; }
  Int_t GetNrndm1() const { return nrndm[1]; }
  Int_t GetIsqstart() const { return isq_start; }
  Int_t GetIseconds() const { return itime_evt; }
  Int_t GetEventCode() const { return event_code; }
  
  Float_t GetImpactParameter() const { return 0.001*float(bimevt1000); }
  Float_t GetSqrtS() const { return 0.001*float(roots1000); }
  Float_t GetBmin() const { return 0.001*float(bmin1000); }
  Float_t GetBmax() const { return 0.001*float(bmax1000); }
  
  Int_t GetInputRunNumber() const { return eventInt[12]; } // event input identification
  Int_t GetOutputRunNumber() const { return eventInt[13]; } // PISA hits output identification
  Int_t GetProjectNumber() const { return eventInt[14]; } // identification of simulation project
  Int_t GetVersionNumber() const { return eventInt[15]; } // identification of version 
  Int_t GetBinaryCollisions() const {return eventInt[16]; } // binary collisions in HIJING1.37
  
  //
  // Event specific methods
  //
  Int_t GetNthrow() const { return nthrow; }
  Int_t GetNaccept() const { return naccept; }
  
  Int_t   GetEventInt(Int_t index) const {return eventInt [index]; }
  Float_t GetEventFloat(Int_t index) const {return eventFloat [index]; }
  
  Int_t   GetTreeMode() const { return eventInt[10]; } // PISA2000 (=1) or PISA99 (=0) tree structure
  Float_t GetEastShiftX() const { return eventFloat[71]; } // East Arm X shift
  Float_t GetEastShiftY() const { return eventFloat[72]; } // East Arm Y shift
  Float_t GetEastShiftZ() const { return eventFloat[73]; } // East Arm Z shift
  Float_t GetWestShiftX() const { return eventFloat[74]; } // West Arm X shift
  Float_t GetWestShiftY() const { return eventFloat[75]; } // West Arm Y shift
  Float_t GetWestShiftZ() const { return eventFloat[76]; } // West Arm Z shift
  
  Float_t GetEastRotateX() const { return eventFloat[77]; } // East Arm X rotate
  Float_t GetEastRotateY() const { return eventFloat[78]; } // East Arm Y rotate
  Float_t GetEastRotateZ() const { return eventFloat[79]; } // East Arm Z rotate
  Float_t GetWestRotateX() const { return eventFloat[80]; } // West Arm X rotate
  Float_t GetWestRotateY() const { return eventFloat[81]; } // West Arm Y rotate
  Float_t GetWestRotateZ() const { return eventFloat[82]; } // West Arm Z rotate
  
  //
  // Parameters on MAGF control line of pisa.kumac file
  //
  Int_t   GetMapFileFlag() const { return eventInt[11]; } // field map choice
  Float_t GetMapFScale() const { return eventFloat[90]; } // map scale factor
  Float_t GetRCutOff() const { return eventFloat[91]; } // tracking cut off in R
  Float_t GetZCutOff() const { return eventFloat[92]; } // tracking cut off in Z
  Float_t GetPCutOff() const { return eventFloat[93]; } // momentum cut off factor
  
  //
  // Reaction plane angle
  //
  Float_t GetReactionPlaneAngle() const { return eventFloat[94]; } // degrees, 0 to 360
  
  //!@name static interface
  //@{
  
  static PISAEventHeader* GetEventHeader() 
  { return &EventHeader; }
  
  static void SetEventHeader( const PISAEventHeader &header) 
  { EventHeader = header; }
  
  //@}

  private:
  Int_t   idrun;
  Int_t   ntru_evt;
  Int_t   nsub_evt;  /* which subevent in this event */
  Int_t   readout;
  Int_t   idevt;
  Int_t   nptls;     /* total number of particles in full event */
  Int_t   ipopsub;   /* total number of particles stored per subevent */
  Int_t   nsubevent; /* total number of subevents in this event */
  Int_t   bimevt1000;
  Int_t   atarg;
  Int_t   ztarg;
  Int_t   aproj;
  Int_t   zproj;
  Int_t   roots1000;
  Int_t   bmin1000;
  Int_t   bmax1000;
  Int_t   t0femto;  // header table start time in femtoseconds 
  Int_t   xyz1000[3]; // stupid integer relic in PISA
  Int_t   igdate;
  Int_t   igtime;
  Int_t   nrndm[2];
  Int_t   isq_start;
  Int_t   itime_evt;
  Int_t   event_code;
  //
  // Need event generator specific here
  //  
  Int_t   nthrow;
  Int_t   naccept;
  
  enum { ARRAY_SIZE = 100 };
  
  // Room for 100 data values for event specific information
  Int_t       eventInt[ARRAY_SIZE];              // Integer event specific info
  Float_t     eventFloat[ARRAY_SIZE];            // Floating event specific info
   
  //! static event header 
  static PISAEventHeader EventHeader;
      
  ClassDef(PISAEventHeader,1)  // PISA Event Header
};

#endif
