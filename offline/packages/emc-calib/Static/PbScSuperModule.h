#ifndef __PBSCSUPERMODULE_H__
#define __PBSCSUPERMODULE_H__

/** 
Author:  E.Kistenev

Created: 03/01/99
*/

//  ROOT Classes

#include <Rtypes.h>

#ifndef __EMCSUPERMODULE_H__
#include "EmcSuperModule.h"
#endif

///  Short version of PMT-DB
#define  PMTWordsPerTower     36            

/// db902 structure
struct DB902{
	///
	 int       PMTBarCode;
	///
	 int       Replaced;
	///
	 Float_t   GainModFactor;
	///
	 Float_t   GQE1kV;
	///
	 Float_t   Slope;
	///
	 Float_t   Data[PMTWordsPerTower];
 };
/** PbSc implementation of EmcSuperModule.

The calibration data base is loaded from files written in 902 during calibration.
Data from 902 Data BAse is loaded only for interactive use from PbSc EMCal Hardware Control Window. Even in that case - only shortened version of the data decribed below is available to user, namely - parameters and bits of history for the PMT and results of before-the-installation on-the-sector testing with laser and muons. When those results are unavailable - data will be missing \\
What is copied into "NewData" are 12 words from PMT DB calibration data per Energy Range (see below). If data looks bad - there will be three words from earlier measurements made on sector (photocell and laser) which are copied without even bothering to check if they are meaninful. This resulted in a bit of screw-up (data from 5 GeV Laser test mixed with data from 2 geV recalibration?).\\

			INDY  Data - Base Structure\\
    1  - Bar code\\
    2  - MELZ number\\
    3  - MELZ Quantum Efficiency\\
    4  - MELZ HV\\
    5  - MELZ Dark Current\\
    6  - MELZ Uniformity\\
    7  - MELZ Slope\\
    8  - MELZ Scale\\
    9  - Flag to indicate manual modifications to the gain (Ratio:New/Old)\\
   10  - Reject Code (copied from Usachev) Used to indicate if 
         PMT is alive (code 0) and  well or dead or lost (nonzero code)\\
   11  - Where I am \\
         1   - Loaded into data - base from Fox-Pro DB\\
         2   - on shelf (means measurements are available)\\
         3   - matched to EMCal (means - results of matching are available)\\
   12  - Fit (measurement) Status Code\\
         0   - Status unknown\\
         1   - Fit results accepted\\
         2   - fit results available but unacceptable. Needs remeasurements or 
               fit to be repeated using restricted set of measurements\\
         3   - measurements available but all rejected. Fit failed\\
        11   - Data looked fine but PMT was replaced after box test \\
               - needs new measurement\\
   13  - Total number of measurements available to fitting procedure\\
   14  - Number of measurements used in the fit (last iteration)\\
   15  - Location for the principal run used in fitting\\
   16  - Slope\\
   17  - Gain\\
   18  - LY correction factor used in gain computation\\
   19  - PMT test stand run used as a principal measurement\\
   20  - Measurement story = \\
	        Number of independent meas. series + 100 * Number of meas. serie retained\\
   21  - Supermodule number\\
   22  - Tower\\
   23  - Tower light yield (photons per GeV) (AGAMGEV) !!!!!!!!!!!!!!!!!!!!!!!!!!\\
   24  - Muon Peak Position\\
   25  - Photocell peak position\\
   26  - Photocell test pulse peak position\\
   27  - Laser peak position\\
   28  - Computed equivalent in photons for Laser signal\\
   29  - Gain Correction factor used for matching \\
   30  - HV ( 5 GeV)\\
   31  - HV (16 GeV)\\
   32  - HV (80 GeV)\\

   33  - Predicted System Gain for 16 GeV dynamic range (pc/GeV)\\
  The next few values are computed assuming all amplitudes are  measured in units of 0.043 pc (WA98 board)\\
   34  - Number of photoelectrons from laser during calibration (from raw width)\\
   35  - Number of photoelectrons from laser during calibration (from norm. width)\\
   36  - \\
   37  - \\
 ****************************************************************************\\
   38  - Flag to indicate that this PMT replaced earlier allocation 
         in the same channel ("old" PMT barcode)\\
 ****************************************************************************\\
   Local variables which will later go into the final DB\\
   39  - amplitude measured on the test box prior to installation (at HV5)\\
   40  - used to store LGAMGEV from calibration in 902   !!!!!!!!!!!!!!!!!!!!!!!!!!!!\\
 *****************************************************************************\\
   Results of on-sectors testing with laser\\
               5 GeV Gain settings\\
   41  - Hamamatsu peak position\\ 
   42  - Photocell peak position \\
   43  - Photocell test pulse peak position\\
   44  - Average Laser amplitude\\
              16 GeV Gain settings\\
   45  - Hamamatsu peak position \\
   46  - Photocell peak position \\
   47  - Photocell test pulse peak position\\
   48  - Average Laser amplitude\\
              80 GeV Gain settings\\
   49  - Hamamatsu peak position \\
   50  - Photocell peak position \\
   51  - Photocell test pulse peak position\\
   52  - Average Laser amplitude\\
 *****************************************************************************\\
   Results of the on-sectors lateral muon calibration (when available)\\
	 If PMT Data Base is loaded (PXEMC) - all 12 words are copied into NewData area\\
   pop,l_cur,l_rms,laser0,p2h,p3h,p2h0,p3h0,phigh,rmshigh\\
               2 GeV Gain settings\\
   56  - Confidence Flag (Laser data)\\
   57  - Photocell peak position (N2)\\
   58  - Photocell peak position (YAG)\\
   59  - Photocell test pulse peak position\\
   60  - Average Laser amplitude (N2)\\
   61  - RMS of the laser amplitude (N2)\\
   62  - Average Laser amplitude (YAG)\\
   63  - RMS of the laser amplitude (YAG)\\
   64  - Confidence Flag (Muon data)\\
   65  - Muon population\\
   66  - Muon peak position   - final\\
   67  - RMS of the muon peak - final\\
               5 GeV Gain settings\\
   71  - Confidence Flag (Laser data)\\
   72  - Photocell peak position (N2)\\
   73  - Photocell peak position (YAG)\\
   74  - Photocell test pulse peak position\\
   75  - Average Laser amplitude (N2)\\
   76  - RMS of the laser amplitude (N2)\\
   77  - Average Laser amplitude (YAG)\\
   78  - RMS of the laser amplitude (YAG)\\
   79  - Confidence Flag (Muon data)\\
   80  - Muon population\\
   81  - Muon peak position   - final\\
   82  - RMS of the muon peak - final\\
              16 GeV Gain settings\\
   86  - Confidence Flag (Laser data)\\
   87  - Photocell peak position (N2)\\
   88  - Photocell peak position (YAG)\\
   89  - Photocell test pulse peak position\\
   90  - Average Laser amplitude (N2)\\
   91  - RMS of the laser amplitude (N2)\\
   92  - Average Laser amplitude (YAG)\\
   93  - RMS of the laser amplitude (YAG)\\
   94  - Confidence Flag (Muon data)\\
   95  - Muon population.\\
   96  - Muon peak position   - final.\\
   97  - RMS of the muon peak - final\\
*/
class PbScSuperModule: public EmcSuperModule
{
 private:
  int SMProductionId;
  struct {
    struct {
      Float_t Hamamatsu;
      Float_t RmsHamamatsu;
      Float_t HamamatsuPed;
      Float_t RmsHamamatsuPed;
      Float_t ExtSPD;
      Float_t RmsExtSPD;
      Float_t ExtSPDPed;
      Float_t RmsExtSPDPed;
      Float_t ExtSPDTP;
      Float_t RmsExtSPDTP;
      Float_t IntSPD;
      Float_t RmsIntSPD;
      Float_t IntSPDPed;
      Float_t RmsIntSPDPed;
      Float_t IntSPDTP;
      Float_t RmsIntSPDTP;
	    } Ref902;
    struct {
      int TowerId;
      int PMT902;
      Float_t        hv902;
      Float_t        PredictedGainQE;
      Float_t        MuPeak;
      Float_t        LaserRaw;
      Float_t        LaserPhelRaw;
      Float_t        LaserPhelNorm;
      Float_t        MeasuredGain;
      Float_t        ScrLightYield;
      Float_t        ScrPhelYield;
      Float_t        EstLightYield;
      Float_t        EstPhelYield;      
      Float_t        VGA;  
    } Tower[144];
  } Data902;

  DB902 * NewData; 

 public:

  PbScSuperModule(int &);
  ~PbScSuperModule();

	///
	bool            LoadSMData();
	///
	inline  int     getProductionId(){return SMProductionId;}
	///
  inline  Float_t getScrLightYield(int & Twr) {return Data902.Tower[Twr].ScrLightYield;}
	///
	inline  Float_t getMuPeak(int & Twr) {return Data902.Tower[Twr].MuPeak;}
	///
	inline  Float_t getLaserRaw(int & Twr) {return Data902.Tower[Twr].LaserRaw;}
	///
	inline  Float_t getIntSPD() {return Data902.Ref902.IntSPD;}
	///
	inline  Float_t getIntSPDTP() {return Data902.Ref902.IntSPDTP;}
	///
	inline  void    setVGA(int Twr, Float_t Value){Data902.Tower[Twr].VGA=Value;}
	///
	inline  Float_t getVGA(int Twr){return Data902.Tower[Twr].VGA;}
	///
	Float_t         getNewHV(int HVGroup, Float_t ERange, Float_t & HVIncrement, Float_t QRange=640., Float_t VGACentral=1.7, Float_t VGAScale=1.);
	///
	virtual void  getNewVGA(int , Float_t , Float_t *, Float_t *);
	///
	virtual void  buildDataBase902();
	///
	virtual void  LoadDataBase902(int , Float_t *);
	///
	virtual Float_t getNewData(int iTwr, int iWord);
	///
	virtual inline int   getPMTBarCode(int Twr){return ((NewData)? NewData[Twr].PMTBarCode : 0);}
	///
	virtual inline int   getReplacedPMTBarCode(int Twr){return ((NewData)? NewData[Twr].Replaced : 0);}
	///
	virtual inline Float_t getGainModFactor(int Twr){return ((NewData)? NewData[Twr].GainModFactor : 0);}
	///
	virtual inline Float_t getPMTSlope(int Twr){return ((NewData)? NewData[Twr].Slope : 0);}
	///
	virtual inline Float_t getPMTGQE1kV(int Twr){return ((NewData)? NewData[Twr].GQE1kV : 0);}
	///
  Float_t         getU0(int & ) {return 0.;}
	///
  Float_t         getUT(int & ) {return 0.;}
        ///
   int   getLgcNumber(int & ) {return 0;} 
       ///
   float getAY(int & ) {return 0.;} 
       ///
   float getVY(int & ) {return 0.;} 
       ///
   float getBL(int & ) {return 0.;} 
       ///
   float getRS(int & ) {return 0.;} 
       ///
   float getAYPeak(int & ) {return 0.;} 
       ///
   float getAYRef(int & ) {return 0.;} 
       ///
   float getTestPeak(int & ) {return 0.;} 
       ///
   float getTestRef(int & ) {return 0.;} 
       ///
   float getGC(int & ) {return 0.;} 
       ///
   float getC0(int & ) {return 0.;} 
       ///
   float getG0(int & ) {return 0.;} 
       ///
   float getCF(int & ) {return 0.;} 
};

#endif













