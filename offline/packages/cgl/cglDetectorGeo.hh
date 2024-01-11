// Created by: Jeffery T. Mitchell
// Description: Central arm global geometry generator
// Details: This class will generate the base detector geometries
//          for those detectors used by global tracking
//
// reworked by Julia Velkovska
// ----------------------------------------------
#ifndef __CGLDETECTORGEO_HH__
#define __CGLDETECTORGEO_HH__

#include <phool.h>
#include <PHCompositeNode.h>

#include <PHFrame.h>
#include <PHPanel.h>
#include <PHCylinderSection.h>
#include <PHAngle.h>

// These headers define the following constants.
#include <svxDetectorGeo.hh>
#include <Tof.hh>
#include <mEmcGeometryModule.h>
#include <TecGeometryObject.hh>
#include <Acc.h>
#include <AccGeometry.h>
#include <TofwPar.h>
#include <TofwGeometry.h>
#include <Hbd.h>
#include <hbdDetectorGeo.hh>
#include <padDetectorGeo.hh>


static const int ACCPANELSNUMBER = ACC::ACC_NBOX;
static const int TOFPANELSNUMBER = TOF_NPANEL_ALL; 
static const int CHAMBERSPERSECT = 2;
static const int TECPANELSNUMBER = TECMAXSECT * TECMAXSIDE;
static const int TOFWPANELSNUMBER = TOFW_NBOX;
static const int HBDPANELSNUMBER = HBD::HBD_NSECT;
static const int HBDPANELSPERARM = HBD::HBD_NSECTARM;

class PHsvxDetectorGeo;
class mEmcGeometryModule;
class TofwGeometry;
class hbdDetectorGeo;

class cglDetectorGeo { 

public:
  cglDetectorGeo();   
  virtual ~cglDetectorGeo() {}
  
public:
  void set_Verbose(const short verb) {Verbose = verb;} // Set Verbose
  short get_Verbose() const {return Verbose;}          // Get Verbose
  void set_pc1Active(const short, const short *);  // Set the PC1 active list for an arm
  void get_pc1Active(const short, short *) const;  // Get the PC1 active list for an arm
  void set_pc1Radius(const double pc1rad) {pc1Radius = pc1rad;} // Set the PC1 radius
  double get_pc1Radius() const {return pc1Radius;}           // Get the PC1 radius
  void set_pc2Active(const short, const short *);  // Set the PC2 active list for an arm
  void get_pc2Active(const short, short *) const;  // Get the PC2 active list for an arm
  void set_pc2Radius(const double pc2rad) {pc2Radius = pc2rad;}  // Set the PC2 radius
  double get_pc2Radius() const {return pc2Radius;}               // Get the PC2 radius
  void set_pc3Active(const short, const short *);  // Set the PC3 active list for an arm
  void get_pc3Active(const short, short *) const;  // Get the PC3 active list for an arm
  void set_pc3Radius(const double pc3rad) {pc3Radius = pc3rad;} // Set the PC3 radius
  double get_pc3Radius() const {return pc3Radius;}              // Get the PC3 radius
  void set_dchActive(const short arm, const short dchin) {dchActive[arm] = dchin;}    // Set the DCH active flag for an arm
  short get_dchActive(const short arm) const {return dchActive[arm];}    // Get the DCH active flag for an arm
  void set_dchRadius(const double dchrad) {dchRadius = dchrad;}  // Set the DCH radius
  double get_dchRadius() const {return dchRadius;}               // Get the DCH radius
  void set_crkActive(const short arm, const short crkin) {crkActive[arm] = crkin;}    // Set the CRK active flag for an arm
  short get_crkActive(const short arm) const {return crkActive[arm];}    // Get the CRK active flag for an arm
  void set_crkRadius(const double crkrad) {crkRadius = crkrad;} // Set the CRK radius
  double get_crkRadius() const {return crkRadius;}           // Get the CRK radius
  void set_tecActive(const short, const short *);  // Set the TEC active list for an arm
  void get_tecActive(const short, short *) const;  // Get the TEC active list for an arm
  void set_tecRadius(const double tecrad) {tecRadius = tecrad;} // Set the TEC radius
  double get_tecRadius() const {return tecRadius;}              // Get the TEC radius
  void set_tofActive(const short, const short *);  // Set the TOF active list for an arm
  void get_tofActive(const short, short *) const;  // Get the TOF active list for an arm
  void set_tofRadius(const double tofrad) {tofRadius = tofrad;}  // Set the TOF radius
  double get_tofRadius() const {return tofRadius;}               // Get the TOF radius
  void get_pbscActive(const short, short *) const;  // Get the PbSc active list for an arm
  void set_pbscActive(const short, const short *);  // Set the PbSc active list for an arm
  void set_pbscRadius(const double pbscrad) {pbscRadius = pbscrad;} // Set the PbSc radius
  double get_pbscRadius() const {return pbscRadius;}                // Get the PbSc radius
  void get_pbglActive(const short, short *) const;  // Get the PbGl active list for an arm
  void set_pbglActive(const short, const short *);  // Set the PbGl active list for an arm
  void set_pbglRadius(const double pbglrad) {pbglRadius = pbglrad;} // Set the PbGl radius
  double get_pbglRadius() const {return pbglRadius;}                // Get the PbGl radius
  void set_accActive(const short accin) {accActive = accin;}  // Set the acc active
  short get_accActive() const {return accActive;}             // Get the acc active
  void set_accRadius(const double accrad) {accRadius = accrad;}  // Set the ACC radius
  double get_accRadius() const {return accRadius;}               // Get the ACC radius
  void set_tofwActive(const short, const short *);  // Set the TOFW active list for an arm
  void get_tofwActive(const short, short *) const;  // Get the TOFW active list for an arm
  void set_tofwRadius(const double tofwrad) {tofwRadius = tofwrad;}  // Set the TOFW radius
  double get_tofwRadius() const {return tofwRadius;}               // Get the TOFW radius
  void set_svxActive(const short, const short, const short *);  // Set the SVX active list for an layer
  void set_SvxActive(const short);  // Set the ALL SVX active 
  void get_svxActive(const short, const short, short *) const;  // Get the SVX active list for an layer
  short int get_SvxActive() const;  // Get ALL SVX active
  void set_svxRadius(const int i, const double svxrad) {svxRadius[i] = svxrad;} // Set the SVX radius
  double get_svxRadius(const int i) const {return svxRadius[i];}              // Get the SVX radius
  void set_hbdActive(const short, const short *);  // Set the HBD active list for an arm
  void get_hbdActive(const short, short *) const;  // Get the HBD active list for an arm
  void set_hbdRadius(const double hbdrad) {hbdRadius = hbdrad;}  // Set the HBD radius
  double get_hbdRadius() const {return hbdRadius;}               // Get the HBD radius


  short get_pc1Geo(const short, PHPanel *) const;   // get PC1 geometry for an arm
  short get_pc2Geo(const short, PHPanel *) const;   // get PC2 geometry for an arm
  short get_pc3Geo(const short, PHPanel *) const;   // get PC3 geometry for an arm

  // Fetch DCH geometry for an arm
  short get_dchGeo(short, PHCylinderSection *);  

  // Fetch CRK, TEC or TEC OUT geometry for an arm
  short get_crkGeo(short, PHCylinderSection *);  
  short get_tecGeo(short, PHPanel *); 
  short get_tecGeoOut(short, PHPanel *);

  // The TOF has sector panels in cgl or panels within the sector
  // which come from the TofGeometryObject in order to keep the
  // interface to track models the same the get_tofGeo method will
  // decide which panels to return based on a flag tofGeoPanels which
  // is set true if the fetch_tofGeo method is called
  short get_tofGeo (const short, PHPanel *) const;   // Fetch TOF geometry for an arm
  short get_pbscGeo(const short, PHPanel *) const;   // Fetch PbGl geometry for an arm
  short get_pbglGeo(const short, PHPanel *) const;   // Fetch PbSc geometry for an arm
  short get_accGeo (const short, PHPanel *) const;   // Fetch ACC geometry 
  short get_tofwGeo (const short, PHPanel *) const;  // Fetch TOFW geometry for an arm
  short get_svxGeo(const short, const short, PHPanel *) const;   // get SVX geometry for an layer
  short get_hbdGeo (const short, PHPanel *) const;  // Fetch hbd geometry for an arm

  // Fetching geometry from individual DGO's use the fetch methods
  // instead of BuildxxxGeo, if geometry from DGOs is required
  void fetch_pc1Geo(PHCompositeNode *topNode,short);   
  void fetch_pc2Geo(PHCompositeNode *topNode,short);   
  void fetch_pc3Geo(PHCompositeNode *topNode,short);   
  void fetch_tecGeo(PHCompositeNode *topNode,short);   
  void fetch_tofGeo(PHCompositeNode *topNode,short);   
  void fetch_emcGeo(PHCompositeNode *topNode,short);   
  void fetch_accGeo(PHCompositeNode *topNode);
  void fetch_tofwGeo(PHCompositeNode *topNode,short);
  void fetch_svxGeo(const short);
  void fetch_svxGeo(PHCompositeNode *topNode){}
  void fetch_hbdGeo(PHCompositeNode *topNode,short);
  void fetchAllGeo(PHCompositeNode *topNode);

  // Print info for a detector, arm
  void PrintGeo(const short, const short) const;
  // get and set frames ... should be moved to transformDST ... because that's
  // the only place where it it still used, but let's keep it here so that we
  // don't need to change preco 
  PHFrame get_phenixFrame() const {return phenixFrame;}
  PHFrame get_eastFrame() const  {return eastFrame;}
  PHFrame get_westFrame() const   {return westFrame;}
  void set_phenixFrame(PHFrame &f){ phenixFrame = f;}
  void set_eastFrame  (PHFrame &f){ eastFrame   = f;}
  void set_westFrame  (PHFrame &f){ westFrame   = f;}

  void set_SvxUseAsciiFile(bool a) {SvxUseAsciiFile = a;}

private:

  PHPanel pc1Sectors[2][padDetectorGeo::PC1MAXSECTPERARM];

  // *2 chambers per sector
  PHPanel pc2Sectors[2][padDetectorGeo::CGLMAXSECTPERARM*CHAMBERSPERSECT];
  PHPanel pc3Sectors[2][padDetectorGeo::CGLMAXSECTPERARM*CHAMBERSPERSECT];

  PHPanel tecPanels[TECMAXSIDE][TECMAXSECT];
  PHPanel tecPanelsOut[TECMAXSIDE][TECMAXSECT];
  PHPanel tofPanels[TOFPANELSNUMBER];
  PHPanel emcSectors[2][padDetectorGeo::CGLMAXSECTPERARM];
  PHPanel accPanels[ACCPANELSNUMBER];
  PHPanel tofwPanels[TOFWPANELSNUMBER];
  PHPanel svxLadders[2][SVXLAYERNUMBER][SVXLADDERNUMBER];
  PHPanel hbdPanels[2][HBDPANELSPERARM];
  PHCylinderSection dchArm[2];
  PHCylinderSection crkArm[2];


  // TRUE - tof panels from TofGeometryObject
  // FALSE - tof panels from cgl
  PHBoolean tofGeoPanels; 

  // Number of active Tec panels from TecGeometryObject 0 (False) if
  // fetch_tecGeo method was not used and cgl tec geometry (not
  // exactly correct) is used
  PHBoolean tecGeoPanels; 

  // Number of active OUT Tec panels from TecGeometryObject 0 (False)
  // if fetch_tecGeo method was not used and cgl tec geometry (not
  // exactly correct) is used
  PHBoolean tecGeoPanelsOut; 

  // TRUE - tofw panels from TofwGeometry
  // FALSE - tofw panels from cgl
  PHBoolean tofwGeoPanels; 


  short Verbose;                         // Verbosity level
  short dchActive[2];                    // Flag active DCH arms
  short crkActive[2];                    // Flag active CRK arms
  short pc1Active[2][padDetectorGeo::PC1MAXSECTPERARM];  // Flag active PC1 sectors
  short pc2Active[2][padDetectorGeo::CGLMAXSECTPERARM*CHAMBERSPERSECT];  // Flag active PC2 sectors
  short pc3Active[2][padDetectorGeo::CGLMAXSECTPERARM*CHAMBERSPERSECT];  // Flag active PC3 sectors
  short svxActive[2][SVXLAYERNUMBER][SVXLADDERNUMBER];  // Flag active SVX sectors
  short SvxActive;  // Flag active ALL SVX sectors
  short tecActive[2][padDetectorGeo::CGLMAXSECTPERARM];  // Flag active TEC sectors
  short tofActive[2][padDetectorGeo::CGLMAXSECTPERARM];  // Flag active TOF sectors
  short pbscActive[2][padDetectorGeo::CGLMAXSECTPERARM]; // Flag active PbGl sectors
  short pbglActive[2][padDetectorGeo::CGLMAXSECTPERARM]; // Flag active PbSc sectors
  short accActive;
  short tofwActive[2][padDetectorGeo::CGLMAXSECTPERARM];  // Flag active TOFW sectors
  short hbdActive[2][HBDPANELSPERARM];


  // these radii are ideal geometry but are used in PHDchTrack to decide
  // where to start and end the track segment for projection calculation 
  double pc1Radius;    // The inner inscribed radius of PC1
  double pc2Radius;    // The inner inscribed radius of PC2
  double pc3Radius;    // The inner inscribed radius of PC3
  double dchRadius;    // The reference radius for DCH
  double crkRadius;    // The reference radius for CRK
  double tecRadius;    // The inner inscribed radius of TEC
  double tofRadius;    // The inner inscribed radius of TOF
  double pbscRadius;   // The inner inscribed radius of PbSc
  double pbglRadius;   // The inner inscribed radius of PbGl
  double accRadius;    // The inner inscribed radius of ACC
  double tofwRadius;   // The inner inscribed radius of TOFW 
  double svxRadius[SVXLAYERNUMBER];    // The inner inscribed radius of SVX
  double hbdRadius;    // The inner inscribed radius of HBD

  mEmcGeometryModule*  EmcDetGeo;
  AccGeometry* 	       AccDetGeo;
  TofwGeometry*        TofwDetGeo;
  hbdDetectorGeo*      HbdDetGeo;

  // these are still needed for transformDST
  PHFrame eastFrame;  
  PHFrame westFrame;  
  PHFrame phenixFrame;  

  bool SvxUseAsciiFile;
}; 

#endif /* __CGLDETECTORGEO_HH__ */







