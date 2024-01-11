#include "mTecSlowSimModule.h"
#include "TecGeometryObject.hh"

#include "dTecFemDataWrapper.h"
#include "dTecGhitRawWrapper.h"
#include "fkinWrapper.h"
#include "tecghitWrapper.h"
#include "TecOutV1.hh"
#include "TecTrack.hh"
#include "TecHit.hh"
#include "TecGHitList.hh"

#include "PHNode.h"
#include "PHIODataNode.h"
#include "PHTypedNodeIterator.h"

#include "TF1.h"
#include "TSystem.h"

#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <vector>

#include "gsl/gsl_rng.h"
#include "gsl/gsl_randist.h"

static const gsl_rng_type *T = gsl_rng_default;
static gsl_rng *rng = 0;

using namespace std;

TSystem* gSystem;

typedef PHIODataNode<fkinWrapper> fkinNode_t;
typedef PHIODataNode<tecghitWrapper> tecghitNode_t;
typedef PHIODataNode<dTecGhitRawWrapper> dTecGhitRawNode_t;
typedef PHIODataNode<dTecFemDataWrapper> dTecFemDataNode_t;
typedef PHIODataNode<TecOutV1> TecOutNode_t;
typedef PHIODataNode<TecGHitList> TecGHitListNode_t;
typedef PHIODataNode < TObject > ObjectNode_t;
typedef PHIODataNode < PHObject > PHObjectNode_t;


const int MAXTECWIRES = 470;
const int MAXTECTIMES = 80;

static int Ntrd = 0;
static int Ntrdabsorbed = 0;
static float Etrd = 0.;
static float Etrdabsorbed = 0.;

//===========================================================================
static
float
tecPolya(const gsl_rng *r)
{
  static short firstcall = 0;
  static float integral[1000];

  int check, i;
  float par, f, buff[1000], lambda, step, shift, pran;

  /* EXECUTABLE STATEMENTS */

  step = 0.01;
  shift = 0.005;
  par = 1.5;

  if (firstcall == 0) /* initialize integral array */
    {
      for (i = 0; i < 1000; i++)
        {
          lambda = i * step + shift;
          buff[i] = par / 0.8862 * sqrt(par * lambda) * exp(-par * lambda);
          /* 0.8862 = Gamma(1.5) */
          if (i > 0)
            {
              integral[i] = integral[i - 1] + buff[i];
            }
          else
            {
              integral[i] = buff[i];
            }
        }
      firstcall = 1;
    }

  check = 0;
  pran = 1.0;
  while (check == 0)
    {
      f = gsl_rng_uniform(r);
      if (f > 0.0001 && f < 0.9999)
        check = 1;
    }

  i = 0;
  while (i < 1000)
    {
      if (f < integral[i] / integral[999])
        {
          pran = i * step + shift;
          break;
        }
      i++;
    }

  return pran;   /* return random number */
}

static
float
get_mass(int particle)
{

  // Particle masses in MeV
  float Mass[50];
  Mass[1] = 0.0;
  Mass[2] = 0.511;
  Mass[3] = 0.511;
  Mass[4] = 0.0;
  Mass[5] = 105.66;
  Mass[6] = 106.66;
  Mass[7] = 134.98;
  Mass[8] = 139.57;
  Mass[9] = 139.57;
  Mass[10] = 497.67;
  Mass[11] = 493.68;
  Mass[12] = 493.68;
  Mass[13] = 939.57;
  Mass[14] = 938.27;
  Mass[15] = 938.27;
  Mass[16] = 497.67;
  for (int i = 17; i < 50; i++)
    {
      Mass[i] = 139.57;
    }
  Mass[25] = 939.57;
  Mass[45] = 1875.613;
  Mass[46] = 2809.25;
  Mass[47] = 3727.417;
  Mass[49] = 2809.23;

  float mass;
  if ((particle < 2) || (particle > 49))
    {
      mass = 139.57;
    }
  else
    {
      mass = Mass[particle];
    }

  return mass;
}

//=============================================================================
// Calculate Lorentz-factor gamma in thousands
static
float
get_gamma(int particle, float momentum)
{
  float Mass2 = get_mass(particle) * get_mass(particle);
  float Momentum2 = momentum * momentum;
  float gamma = sqrt(Momentum2 / Mass2 + 1.) / 1000.;

  return gamma;
}

//=============================================================================
// Calculate number of TR X-rays
// Lorentz-factor gamma is in thousands
// Number of TR X-rays is assumed to have Poisson distribution
// This fit is valid only up to gamma = 25000 (electron momentum 12.5 GeV/c)
// 10-14-2002 SL
static
int
get_NTR(float gamma, long* randomseed)
{
  if (gamma < 0.5)
    return 0;

  // mean number of TR X-rays
  float meanntr = 0.;
  if (gamma > 25.0)
    {
      meanntr = 4.565;
    }
  else
    {
      meanntr = -2.27348e-01 + 6.45754e-01 * gamma +
                -4.16668e-02 * gamma * gamma +
                1.41580e-03 * gamma * gamma * gamma +
                -1.90263e-05 * gamma * gamma * gamma * gamma;
    }
  int ntr = 0;
  ntr = gsl_ran_poisson(rng, meanntr);

  return ntr;
}

//=============================================================================
// Calculate random TR X-ray energy in keV
// Lorentz-factor gamma is in thousands
// This is only approximate fit to the simulation results
// should be changed to more exact formula 
// 10-14-2002 SL
static
float
get_omega(float gamma)
{
  if (gamma < 0.49)
    return 0.;
  float a = 2.05 - gamma * 0.04;
  float b = -0.1 - 1.0 / (gamma + 2.5);
  TF1 omega = TF1("omega", "1.0*pow(x,[0])*exp([1]*x)", 0.5, 100.);
  omega.SetParameters(a, b);
  float e = omega.GetRandom();

  return e;
}

//=============================================================================
// Calculate distance at which TR X-ray  will be absorbed.
// Input TR X-ray energy is in keV, output path length in cm.
// Absorption data are taken from:
//    http://physics.nist.gov/PhysRefData/Xcom/Text/XCOM.html
// 10-14-2002 SL

static
float
calc_absorption(float omega, int gastype)
{
  float lambda = 0.;
  float par[3];
  float rho;

  switch (gastype)
    {
    case 1:      // P10 -- not ready, no absorption
      lambda = 1.0e-9;
      break;
    case 3:      // Ar  -- not ready, no absorption
      lambda = 1.0e-9;
      break;
    case 4:  	// 0.45 Xe + 0.45 He + 0.1 CH4 mixture
      rho = 2.80e-3;
      if (omega < 5.)
        {
          par[0] = 6.56356e+07;
          par[1] = -9.61194e+00;
          par[2] = 2.10738e-01;
          lambda = par[0] * exp(par[1] * pow(omega, par[2]));      // in cm^2/g
          lambda = lambda * rho;                                // in cm^-1
        }
      else
        {
          par[0] = 5.48214e+11;
          par[1] = -1.73652e+01;
          par[2] = 1.16323e-01;
          lambda = par[0] * exp(par[1] * pow(omega, par[2]));      // in cm^2/g
          lambda = lambda * rho;                                // in cm^-1
        }
      break;
    case 2: 	// 0.9 Xe + 0.1 CH4
      rho = 5.36e-3;
      if (omega < 5.)
        {
          par[0] = 2.95517e+08;
          par[1] = -1.04372e+01;
          par[2] = 1.95691e-01;
          lambda = par[0] * exp(par[1] * pow(omega, par[2]));      // in cm^2/g
          lambda = lambda * rho;                                // in cm^-1
        }
      else
        {
          par[0] = 7.13348e+11;
          par[1] = -1.69418e+01;
          par[2] = 1.18797e-01;
          lambda = par[0] * exp(par[1] * pow(omega, par[2]));      // in cm^2/g
          lambda = lambda * rho;                                // in cm^-1
        }
      break;
    default:
      cout << PHWHERE << "ERROR: unknown medium " << gastype << endl;
      break;
    }

  // now calculate random path length in cm

  TF1 eksp = TF1("eksp", "exp([0]*x)", 0., 100.);
  eksp.SetParameter(0, -1.0*lambda);
  float pathl = eksp.GetRandom();

  return pathl;
}

//=============================================================================
/** Detailed TEC detector response simulation. 
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/tec/slowsim.html}
*/

mTecSlowSimModule::mTecSlowSimModule()
{
  Verbose = 0;
  RandSeed = -923406781;
  //  GasType = 1; 		// P10
  //  GasType = 3; 		// Ar
  GasType = 4; 		// 0.45 Xe + 0.45 He + 0.1 CH4 mixture
  //  GasType = 2; 		// 0.9 Xe + 0.1 CH4 mixture
  GasGain = 3000.0;
  for (int i = 0; i < TECMAXINDEX; i++)
    {
      PlaneGasGain[i] = GasGain;
    }
  cout << "********* GAS GAIN: 3000 **************" << endl;
  //GasGain = 2500.0;
  //cout << "********* VARIABLE GAIN: 2500 + 500*iplane **************" << endl;
  //  cout << "********* VARIABLE GAIN: 5000 - 500*iplane **************" << endl;
  //  GasGain = 5000.0;
  //  for(int i=0; i<TECMAXSECT; i++) {
  //    for(int j=0; j<TECMAXSIDE; j++) {
  //      for(int k=0; k<TECMAXPLANE; k++) {
  //	int ind = i*TECMAXPLANE*TECMAXSIDE + k*TECMAXSIDE + j;
  //        //PlaneGasGain[ind] = GasGain + 500.*k;
  //        PlaneGasGain[ind] = GasGain - 500.*k;
  //      }
  //    }
  //  }
  TimeSampling = 26.5;
  DriftVelocity = 0.0025;
  FillTecGhitRaw = 1;
  FillTecGHitList = 0;
  ChangePar = 0;
  Noise = 0;
  TrdON = 1;
  if (rng == 0)
    {
      rng = gsl_rng_alloc (T);
    }
}

//=============================================================================
PHBoolean mTecSlowSimModule::event(PHCompositeNode *root)
{
  cerr << "mTecSlowSimModule::event(PHCompositeNode*) method is obsolete." << endl;
  cerr << "Use event(PHCompositeNode*, TecAddressObject*) method." << endl;

  return False;
}

//=============================================================================
PHBoolean 
mTecSlowSimModule::event(PHCompositeNode *root, TecAddressObject* TAO)
{
  
  PHNodeIterator lll(root);

  long i, j, irndm;
  int MinWire, MaxWire, MiddleWire, iTotalAmp, bin0;
  int itecgr, ighit, iplane, iraw, iarm, isect, izsign, iwire, ibin;
  int ghitarm, ghitsect, ghitplane, ghitzsign, trktec;
  int Particle, counter, debugTecSlowSim, iNumBins;
  int slsl, slsl1, slsl2, nnn, TIME_IC;
  int ghitid0, ghitid1, start, stop, iBin, NumBinsToFill;
  int fillTecGhitRaw, gastype, NSEC = 0, ipri;
  int stat, NumSecondary, NumTotal, NumPrimary;
  int icrate, islot, ipsadd, ichan, itmp;
  int ins, noisebin, noisewire;
  static int firsttime = 0;
  vector<float> trde;
  vector<float> trdpl;
  vector<float> trdused;
  vector<int> ListOfWires;

  float my = 0.;
  float myp = 0.;
  float mys = 0.;
  float myt = 0.;
  int myn = 0;
  int myna = 0;
  float mye = 0.;
  float myea = 0.;

  float TotalAmp;
  float Momentum, BinLength, rNumPrimary, NumBins;
  float TrackLength, FracThisCell, CellTrackLength, CellTrackLengthX;
  float CellTrackLengthY, chTotalInBin, TotalInBin, Fraction[3], tof0;
  float XYZin[3], XYZout[3], XYZentry[3], XYZexit[3];
  float xcellentry = 0., xcellexit = 0., xcellwire = 0., xmin, xmax, mean, sigma, amp;
  float Axy, Bxy, Ayz, Byz;
  float ELoss;
  float XYZ_IC[2], DIST_IC, ADC_IC, TECwirey, iii;
  float rtime, ramp, noiseAMP;
  float xgauss, xpolya, gasGain;
  // digitization parameters
  float SecondRange = 20.0; // fC
  //  float ThirdRange = 96.0;  // fC <-- this is wrong
  float ThirdRange = 92.0;  // fC <-- corrected March 2003
  float Charge2Channels = 2.0; // fC/channel
  float LowestNoise = 6.0;  // fC (corresponds to ADC channel 3)
  static float arraytime[100];
  int FadcCut = 2;

  float gamma = 1.;

  float charge, xyz[3];
  int trackid;
  charge = 0.;
  xyz[0] = 0.;
  xyz[1] = 0.;
  xyz[2] = 0.;
  trackid = -1;

  //------ Get Tables ---------------------------------------------

  fkinNode_t* fkN = static_cast<fkinNode_t*>(lll.findFirst("PHIODataNode", "fkin"));
  if (!fkN)
    {
      cerr << "mTecSlowSimModule -> ERROR: fkin table not found." << endl;
      return False;
    }
  fkinWrapper* fkin = fkN->getData();

  tecghitNode_t* tgN = static_cast<tecghitNode_t*>(lll.findFirst("PHIODataNode", "tecghit"));
  if (!tgN)
    {
      cerr << "mTecSlowSimModule -> ERROR: tecghit table not found." << endl;
      return False;
    }
  tecghitWrapper* tecghit = tgN->getData();

  dTecGhitRawNode_t* GRN = static_cast<dTecGhitRawNode_t*>(lll.findFirst("PHIODataNode", "dTecGhitRaw"));
  if (!GRN)
    {
      cerr << "mTecSlowSimModule -> ERROR: dTecGhitRaw table not found." << endl;
      return False;
    }
  dTecGhitRawWrapper* dTecGhitRaw = GRN->getData();

  dTecFemDataNode_t* FDN = static_cast<dTecFemDataNode_t*>(lll.findFirst("PHIODataNode", "dTecFemData"));
  if (!FDN)
    {
      cerr << "mTecSlowSimModule -> ERROR: dTecFemData table not found." << endl;
      return False;
    }
  dTecFemDataWrapper* dTecFemData = FDN->getData();

  //--------------------------------------------------------------------------
  // Find TecGeometryObject

  TecGeometryObject *TecGeometry;
  ObjectNode_t *TGONode;
  PHNodeIterator nodeIter (root);
  PHCompositeNode *parNode;
  parNode =
    static_cast <
    PHCompositeNode * > (nodeIter.findFirst ("PHCompositeNode", "PAR"));
  if (!parNode)
    {
      PHMessage ("mTecSlowSimModule: ", PHError,
                 "PAR node does not exist.");
      return False;
    }

  PHNodeIterator parNodeIter(parNode);

  TGONode =
    static_cast <
    ObjectNode_t * > (parNodeIter.findFirst ("PHIODataNode", "TecGeometry"));
  if (!TGONode)
    {
      PHMessage ("mTecSlowSimModule: ", PHError,
                 "TecGeometryObject not found.\n");
      return False;
    }
  else
    {
      TecGeometry = static_cast < TecGeometryObject * >(TGONode->getData ());
    }

  //---------------------------------------------------------------------------
  // Try to find TecOut in the node tree

  PHTypedNodeIterator<TecOutV1> teciter(root);
  TecOutNode_t *TecOutNode = teciter.find("TecOutV1");
  TecOutV1* tecout;
  PHCompositeNode* tecNode;

  if (TecOutNode)
    {
      tecout = (TecOutV1*)TecOutNode->getData();
      if (Verbose > 0)
        cout << "mTecSlowSimModule: Found TecOut." << endl;
    }
  else
    {        // TecOut does not exist. Add it to node tree.
      if (Verbose > 0)
        cout << "mTecSlowSimModule: Can not find TecOut. "
        << "Adding TecOut to the node tree... (tecNode)" << endl;

      tecNode = static_cast<PHCompositeNode*>(lll.findFirst("PHCompositeNode", "TEC"));
      if (!tecNode)
        {
          cerr << "mTecSlowSimModule ERROR: TEC node does not exist." << endl;
          return False;
        }
      else
        {
          if (Verbose > 0)
            cout << "mTecSlowSimModule: tecNode FOUND." << endl;
        }

      // Add TecOut to tecNode

      tecout = new TecOutV1();
      PHIODataNode<PHObject>* TecOutNodeNew = 
	new PHIODataNode<PHObject>(tecout, "TecOutV1", "PHObject");
      tecNode->addNode(TecOutNodeNew);
      if (Verbose > 0)
        cout << "mTecSlowSimModule: TecOut added to tecNode." << endl;

    }

  //---------------------------------------------------------------------------
  // Try to find TecGhitList in the node tree

  PHTypedNodeIterator<TecGHitList> giter(root);
  TecGHitListNode_t *TecGHitListNode = giter.find("TecGHitList");
  TecGHitList* tecghitlist;

  if (TecGHitListNode)
    {
      tecghitlist = (TecGHitList*)TecGHitListNode->getData();
      if (Verbose > 0)
        cout << "mTecSlowSimModule: Found TecGHitList." << endl;
    }
  else
    {        // TecGHitList does not exist. Add it to node tree.
      if (Verbose > 0)
        cout << "mTecSlowSimModule: Can not find TecGHitList. "
        << "Adding TecGHitList to the node tree... (tecNode)" << endl;

      tecNode = static_cast<PHCompositeNode*>(lll.findFirst("PHCompositeNode",
							    "TEC"));
      if (!tecNode)
        {
          cerr << "mTecSlowSimModule ERROR: TEC node does not exist." << endl;
          return False;
        }
      else
        {
          if (Verbose > 0)
            cout << "mTecSlowSimModule: tecNode FOUND." << endl;
        }

      // Add TecGHitList to tecNode
      tecghitlist = new TecGHitList();
      PHIODataNode<PHObject>* TecGHitListNodeNew = new
          PHIODataNode<PHObject>(tecghitlist, "TecGHitList", "PHObject");
      tecNode->addNode(TecGHitListNodeNew);
      if (Verbose > 0)
        cout << "mTecSlowSimModule: TecGHitList added to tecNode." << endl;
    }

  debugTecSlowSim = Verbose;
  fillTecGhitRaw = FillTecGhitRaw;
  iraw = 0;
  itecgr = 0;
  irndm = RandSeed;

  for (int k = 0; k < 48; k++)
    {
      TecGeometry->setActivePlane(k, 1);
    }

  if (debugTecSlowSim > 0)
    {
      cout << "GasType         = " << GasType << endl;
      cout << "GasGain         = " << GasGain << endl;
      cout << "TimeSampling    = " << TimeSampling << endl;
      cout << "DriftVelocity   = " << DriftVelocity << endl;
      cout << "TrdON           = " << TrdON << endl;
    }
  // local y coordinate of wires
  TECwirey = TecGeometryObject::get_ywidth() / 2.0 - 0.2;  

  counter = 0;

  // if ChangePar is not 0, decode various
  // simulation parameters. Otherwise use default values.

  if (ChangePar > 0)
    {
      slsl = ChangePar;
      slsl1 = slsl % 10;
      slsl2 = slsl / 10;
      Particle = slsl1;
      if (Particle < 2 || (Particle > 16 && Particle != 25))
        Particle = 3;
      Momentum = slsl2 * 100.0;
      if (Momentum < 100.0)
        Momentum = 100.0;
      printf("mTecSlowSim -> Change of parameters was requested. \n");
      printf("mTecSlowSim -> All particle ID will be %d \n", Particle);
      printf("mTecSlowSim -> Momentum will be %f MeV/c \n", Momentum);
    }

  gastype = GasType;

  if (debugTecSlowSim != 0)
    printf("mTecSlowSim -> Start loop over chambers.\n");

  // Loop over the two arms
  for (iarm = 0; iarm < 2; iarm++)
    {
      // Loop over the TEC sectors in each arm
      for (isect = 0; isect < TECMAXSECT; isect++)
        {

          // Loop over the TEC planes in each sector
          for (iplane = 0; iplane < TECMAXPLANE; iplane++)
            {

              // Loop over the positive and negative z signs in each sector
              for (izsign = 0; izsign < TECMAXSIDE; izsign++)
                {
                  if (TecGeometry->isActivePlane(isect*TECMAXPLANE*TECMAXSIDE +
                                                 iplane*TECMAXSIDE +
                                                 izsign))
                    {

                      // Plane-by-plane temporary storage of Flash ADC values
                      float FlashADC[MAXTECWIRES][MAXTECTIMES][3] = {{{0}}};
                      // Allow for a maximum of 3 GEANT hits associated to each wire.
                      short int GhitIDStore[MAXTECWIRES][MAXTECTIMES][3] = {{{0}}};

                      // Middle Wire is the wire number of the wire in
                      // the middle of the sector. This provides the
                      // offset for the other calculations.
		      
                      if (TecGeometryObject::get_NumWires(iplane) % 2 == 1)
                        {
                          MiddleWire = ((int)(TecGeometryObject::get_NumWires(iplane) / 2)) + 1;
                        }
                      else
                        {
                          MiddleWire = ((int)(TecGeometryObject::get_NumWires(iplane) / 2));
                        }   // NumWires[iplane]%2==1

                      for (ighit = 0; ighit < (int)tecghit->RowCount(); ighit++)
                        {
                          // current arm, sector, plane, and zsign?
                          ghitarm = tecghit->get_arm(ighit);
                          ghitsect = tecghit->get_sector(ighit) + 1;
                          // GEANT hit sector assignment incorrect for
                          // arm 2. Fix that here.
                          if (ghitarm == 0)
                            ghitsect = TecGeometryObject::get_sectperarm() * 2 - ghitsect;
                          if (ghitarm == 1)
                            ghitsect = TecGeometryObject::get_sectperarm() - ghitsect;
                          ghitplane = tecghit->get_plane(ighit);
                          ghitzsign = 1;
                          if (tecghit->get_xyzinglo(2, ighit) < 0)
                            ghitzsign = 0;

                          // From May 23, 2000 East Arm is arm 0
                          if (ghitarm == 0)
                            {
                              ghitarm = 1;
                            }
                          else
                            {
                              ghitarm = 0;
                            }

                          if (ghitarm == iarm && ghitsect == isect &&
                              ghitplane == iplane && ghitzsign == izsign)
                            {

                              // Find out particle ID if it was not
                              // set by ChangePar

                              if (ChangePar == 0)
                                {
                                  trktec = tecghit->get_mctrack(ighit);
                                  for (i = 0; i < (int)fkin->RowCount(); i++)
                                    {
                                      if (trktec == fkin->get_true_track(i))
                                        {
                                          Particle = fkin->get_idpart(i);
                                          if (Particle == 51)
                                            Particle = 11;
                                          if (Particle == 52)
                                            Particle = 12;
                                          Momentum = fkin->get_ptot(i) * 1000.0;
                                          gamma = get_gamma(Particle, Momentum); // in thousands
                                          break;
                                        }
                                    }
                                }

                              // Check if this particle will generate
                              // transition radiation
                              int NTRD = 0;
                              if (gamma > 0.5)
                                {
                                  NTRD = get_NTR(gamma, &irndm);
                                  for (int ntr = 0; ntr < NTRD; ntr++)
                                    {
                                      float e = get_omega(gamma);
                                      trde.push_back(e);
                                      mye += e;
                                      float pl = calc_absorption(e, gastype);
                                      trdpl.push_back(pl);
                                    }
                                }
                              for (int itrd = 0; itrd < NTRD; itrd++)
                                {
                                  trdused.push_back(0);
                                }
                              myn += NTRD;

                              // Grab the endpoints of the track in
                              // the TEC. Calculate the total track
                              // length. X, Y and Z local coordinates
                              // are rotated relative to global
                              // coordinates differently for East and
                              // West arms.

                              // From May 23, 2000 East Arm is arm 0
                              //		if(iarm==0)
                              if (iarm == 1)
                                {
                                  XYZin[0] = -tecghit->get_xyzinloc(0, ighit);
                                }
                              else
                                {
                                  XYZin[0] = tecghit->get_xyzinloc(0, ighit);
                                }
                              XYZin[1] = tecghit->get_xyzinloc(2, ighit);
                              XYZin[2] = -tecghit->get_xyzinloc(1, ighit);
                              //                if(iarm==0)
                              if (iarm == 1)
                                {
                                  XYZout[0] = -tecghit->get_xyzoutloc(0, ighit);
                                }
                              else
                                {
                                  XYZout[0] = tecghit->get_xyzoutloc(0, ighit);
                                }
                              XYZout[1] = tecghit->get_xyzoutloc(2, ighit);
                              XYZout[2] = -tecghit->get_xyzoutloc(1, ighit);
                              TrackLength = sqrt((XYZout[0] - XYZin[0]) *
                                                 (XYZout[0] - XYZin[0]) +
                                                 (XYZout[1] - XYZin[1]) *
                                                 (XYZout[1] - XYZin[1]) +
                                                 (XYZout[2] - XYZin[2]) *
                                                 (XYZout[2] - XYZin[2]));

                              if (debugTecSlowSim > 2)
                                {
                                  counter = counter + 1;
                                  printf(" *********************** %d ********************** \n", counter);
                                  printf(" *** arm,sector,plane,zsign : %d %d %d %d \n",
                                         ghitarm, ghitsect, ghitplane, ghitzsign);
                                  printf(" *** XYZin = %f %f %f \n", XYZin[0], XYZin[1], XYZin[2]);
                                  printf(" *** XYZout = %f %f %f \n", XYZout[0], XYZout[1], XYZout[2]);
                                  printf(" *** TrackLength = %f\n", TrackLength);
                                }

                              // calculate time bin offset due to TOF
                              tof0 = tecghit->get_tof(ighit);
                              bin0 = (int)(tof0 / TimeSampling);

                              // order the entry and exit x coordinates.
                              xmin = XYZin[0];
                              xmax = XYZout[0];
                              if (xmax < xmin)
                                {
                                  xmin = XYZout[0];
                                  xmax = XYZin[0];
                                }

                              // Make a line in the x-y plane, and a
                              // line in the y-z plane connecting
                              // these endpoints.  The x-y line will
                              // be defined by y = Axy*x + Bxy.  The
                              // y-z line will be defined by z = Ayz*y
                              // + Byz.

                              Axy = 0.0;
                              Bxy = 0.0;
                              if (XYZin[0] != XYZout[0])
                                {
                                  Axy = (XYZout[1] - XYZin[1]) /
                                        (XYZout[0] - XYZin[0]);
                                  Bxy = (XYZin[1] - Axy * XYZin[0]);
                                }   // Xin != Xout
                              Ayz = 0.0;
                              Byz = 0.0;
                              if (XYZin[1] != XYZout[1])
                                {
                                  Ayz = (XYZout[2] - XYZin[2]) /
                                        (XYZout[1] - XYZin[1]);
                                  Byz = (XYZin[2] - Ayz * XYZin[1]);
                                }   // Yin != Yout


                              // Calculate the range of wires that are
                              // traversed by the GEANT track.

                              MinWire = -1;
                              MaxWire = -1;
                              if (TecGeometryObject::get_NumWires(iplane) % 2 == 1)
                                {
                                  // 11/26/2000
                                  MinWire = (int)((xmin / TecGeometryObject::get_WireSpacing(iplane)) +
                                                  MiddleWire + 0.5);
                                  MaxWire = (int)((xmax / TecGeometryObject::get_WireSpacing(iplane)) +
                                                  MiddleWire + 0.5);
                                }
                              if (TecGeometryObject::get_NumWires(iplane) % 2 == 0)
                                {
                                  // 11/26/2000
                                  MinWire = (int)((xmin / TecGeometryObject::get_WireSpacing(iplane)) +
                                                  MiddleWire + 1.0);
                                  MaxWire = (int)((xmax / TecGeometryObject::get_WireSpacing(iplane)) +
                                                  MiddleWire + 1.0);
                                }
                              if (MinWire < 0)
                                MinWire = 0;
                              if (MaxWire >= TecGeometryObject::get_NumWires(iplane))
                                MaxWire = TecGeometryObject::get_NumWires(iplane);

                              if (debugTecSlowSim > 2)
                                printf(" *** MinWire, MaxWire = %d %d \n", MinWire, MaxWire);

                              // Make a list of fired wires and
                              // arrange it in correct order
                              if (XYZin[0] < XYZout[0])
                                {
                                  for (iwire = MinWire; iwire <= MaxWire; iwire++)
                                    {
                                      ListOfWires.push_back(iwire);
                                    }
                                }
                              else
                                {
                                  for (iwire = MaxWire; iwire >= MinWire; iwire--)
                                    {
                                      ListOfWires.push_back(iwire);
                                    }

                                }
                              float TotTrdLength = 0.;
                              for (int jwire = 0; jwire < (int)ListOfWires.size(); jwire++)
                                {
				  
                                  iwire = ListOfWires[jwire];
                                  vector<int> trdn, trdbin;

                                  // Calculate the x coordinate of the
                                  // wire cell entry and exit Also
                                  // calculate the x coordinate of the
                                  // wire itself.

                                  if (TecGeometryObject::get_NumWires(iplane) % 2 == 1)
                                    {
                                      xcellentry =
                                        ((float)iwire - (float)MiddleWire - 0.5) *
                                        TecGeometryObject::get_WireSpacing(iplane);
                                      xcellexit = xcellentry +
                                                  TecGeometryObject::get_WireSpacing(iplane);
                                      xcellwire =
                                        ((float)iwire - (float)MiddleWire - 0.0) *
                                        TecGeometryObject::get_WireSpacing(iplane);
                                    }
                                  if (TecGeometryObject::get_NumWires(iplane) % 2 == 0)
                                    {
                                      xcellentry =
                                        ((float)iwire - (float)MiddleWire - 1.0) *
                                        TecGeometryObject::get_WireSpacing(iplane);
                                      xcellexit = xcellentry +
                                                  TecGeometryObject::get_WireSpacing(iplane);
                                      xcellwire =
                                        ((float)iwire - (float)MiddleWire - 0.5) *
                                        TecGeometryObject::get_WireSpacing(iplane);
                                    }

                                  if (debugTecSlowSim > 2)
                                    printf(" * Xcell(entry,wire,exit) = %f %f %f \n",
                                           xcellentry, xcellwire, xcellexit);

                                  // Calculate the coordinates of the
                                  // track as it enters and exits the
                                  // cell corresponding to
                                  // iwire. These coordinates will be
                                  // XYZentry and XYZexit. The
                                  // coordinates at the wire will be
                                  // XYZwire.  Assuming straight line
                                  // particle trajectories.

                                  for (i = 0; i < 3; i++)
                                    {
                                      XYZentry[i] = 0.0;
                                      XYZexit[i] = 0.0;
                                    }
                                  if (iwire == MinWire)
                                    {
                                      for (i = 0; i < 3; i++)
                                        {
                                          XYZentry[i] = XYZin[i];
                                          if (XYZin[0] > XYZout[0])
                                            XYZentry[i] = XYZout[i];
                                        }
                                    }
                                  else
                                    {
                                      XYZentry[0] = xcellentry;
                                      XYZentry[1] = Axy * xcellentry + Bxy;
                                      XYZentry[2] = Ayz * XYZentry[1] + Byz;
                                    }   // iwire == MinWire

                                  if (iwire == MaxWire)
                                    {
                                      for (i = 0; i < 3; i++)
                                        {
                                          XYZexit[i] = XYZout[i];
                                          if (XYZin[0] > XYZout[0])
                                            XYZexit[i] = XYZin[i];
                                        }
                                    }
                                  else
                                    {
                                      XYZexit[0] = xcellexit;
                                      XYZexit[1] = Axy * xcellexit + Bxy;
                                      XYZexit[2] = Ayz * XYZexit[1] + Byz;
                                    }   // iwire == MaxWire

                                  if (XYZexit[1] != XYZentry[1]) //*** get rid of strange hits ***
                                    {

                                      if (debugTecSlowSim > 2)
                                        {
                                          printf(" * XYZentry = %f %f %f \n", XYZentry[0], XYZentry[1], XYZentry[2]);
                                          printf(" * XYZexit = %f %f %f \n", XYZexit[0], XYZexit[1], XYZexit[2]);
                                        }

                                      // calculate track length in
                                      // this wire cell and a fraction
                                      // of total track length which
                                      // is inside this cell

                                      CellTrackLength = sqrt((XYZentry[0] - XYZexit[0]) *
                                                             (XYZentry[0] - XYZexit[0]) +
                                                             (XYZentry[1] - XYZexit[1]) *
                                                             (XYZentry[1] - XYZexit[1]) +
                                                             (XYZentry[2] - XYZexit[2]) *
                                                             (XYZentry[2] - XYZexit[2]));
                                      CellTrackLengthX = XYZexit[0] - XYZentry[0];
                                      CellTrackLengthY = XYZexit[1] - XYZentry[1];
                                      FracThisCell = CellTrackLength / TrackLength;

                                      if (debugTecSlowSim > 2)
                                        printf(" * CellTrackLength = %f %f %f %f \n",
                                               CellTrackLength, CellTrackLengthX, CellTrackLengthY, FracThisCell);

                                      //**** calculate the number of primary collisions in this wire cell ****

                                      // ELoss = average number of primary collisions per 1 cm
                                      ELoss = PriIon(&gastype, &Particle, &Momentum);

                                      BinLength = DriftVelocity * TimeSampling;
                                      NumBins = CellTrackLength / BinLength;
                                      iNumBins = (int)(NumBins) + 1;
                                      if (iNumBins > 0)
                                        {
                                          BinLength = CellTrackLength / iNumBins;
                                          rNumPrimary = ELoss * CellTrackLength / iNumBins;
                                        }
                                      else
                                        {
                                          rNumPrimary = 0.0;
                                        }
                                      // rNumPrimary = average # of
                                      // primary collisions in one bin

                                      if (debugTecSlowSim > 2)
                                        printf(" * BinL, Npri/cm, Npri, Nbins(f,i) %f %f %f %f %d \n",
                                               BinLength, ELoss, rNumPrimary, NumBins, iNumBins);

                                      //**** Check if any of the TR
                                      //**** X-rays was absorbed in
                                      //**** this wire cell ****

                                      TotTrdLength += CellTrackLength;

                                      for (int itrd = 0; itrd < NTRD; itrd++)
                                        {
                                          if (trdpl[itrd] < TotTrdLength && !trdused[itrd])
                                            { // TR X-ray absorbed
                                              int itrdn = (TrdIon(gastype, trde[itrd]));
                                              trdused[itrd] = 1;
                                              trdn.push_back(itrdn);
                                              myna += 1;
                                              myea += trde[itrd];
                                              // calculate time bin where TR X-ray was absorbed
                                              float l = trdpl[itrd] - (TotTrdLength - CellTrackLength);
                                              int itrdbin = ((int)(((float)iNumBins) * l / CellTrackLength)) + 1;
                                              trdbin.push_back(itrdbin);
                                            }
                                          else
                                            { // no absorption, no signal
                                              trdn.push_back(0);
                                              trdbin.push_back( -1);
                                            }
                                        }

                                      //*********** Start loop over time bins *************

                                      for (i = 1; i < iNumBins + 1; i++)
                                        {

                                          // Randomize number of
                                          // primary collisions in
                                          // this time bin.  Primary
                                          // ionization is well
                                          // described by Poisson
                                          // statistics.

                                          mean = rNumPrimary;
                                          nnn = gsl_ran_poisson(rng, mean);
                                          NumPrimary = nnn;

                                          NumSecondary = 0;
                                          for (ipri = 0; ipri < NumPrimary; ipri++)
                                            {
                                              NSEC = SecIon(&irndm, &gastype, &Particle, &Momentum);
                                              NumSecondary += NSEC;
                                            }

                                          NumTotal = NumPrimary + NumSecondary;

                                          // check if TR X-ray was
                                          // absorbed in this time bin
                                          // and add it to NumTotal
                                          int NumTrd = 0;
                                          for (int itrd = 0; itrd < NTRD; itrd++)
                                            {
                                              if (trdbin[itrd] == i)
                                                {
                                                  NumTrd += trdn[itrd];
                                                }
                                            }

                                          if (TrdON)
                                            NumTotal += NumTrd;

                                          my += NumTotal;
                                          myp += NumPrimary;
                                          mys += NumSecondary;
                                          myt += NumTrd;

                                          //  Calculate distance to
                                          //  the wire and time bin -
                                          //  assume straight line
                                          //  trajectories

                                          iii = (i - 0.5) / iNumBins;
                                          XYZ_IC[0] = iii * CellTrackLengthX + XYZentry[0];
                                          XYZ_IC[1] = iii * CellTrackLengthY + XYZentry[1];
                                          DIST_IC = sqrt((XYZ_IC[0] - xcellwire) *
                                                         (XYZ_IC[0] - xcellwire) +
                                                         (XYZ_IC[1] - TECwirey) *
                                                         (XYZ_IC[1] - TECwirey));
                                          TIME_IC = bin0 + 5 + (int)(DIST_IC / DriftVelocity / TimeSampling + 0.5);
                                          if (TIME_IC > MAXTECTIMES - 1)
                                            TIME_IC = MAXTECTIMES - 1;

                                          // introduce gas gain
                                          // fluctuations according to
                                          // Polya distribution with
                                          // Polya parameter 1.5
                                          // (W.G.Gong - gong@bnl.gov;
                                          // Valeri Chernyatin
                                          // (cherniat@bnlarm.bnl.gov)
                                          // recomends 1.38)

                                          xpolya = tecPolya(rng);
                                          int tmpindex = isect * TECMAXPLANE * TECMAXSIDE + iplane * TECMAXSIDE + izsign;
                                          gasGain = PlaneGasGain[tmpindex] * xpolya;
                                          if (debugTecSlowSim > 10)
                                            printf(" Gas Gain = %f %f \n", xpolya, gasGain);

                                          ADC_IC = NumTotal * gasGain * 1.602e-4;

                                          if (debugTecSlowSim > 10)
                                            printf("   TIME_IC,ADC_IC,Num(Tot,Pri),xpolya = %d %f %d %d %f \n",
                                                   TIME_IC, ADC_IC, NumTotal, NumPrimary, xpolya);

                                          // Fill time bins using TEC
                                          // time responce by Valeri
                                          // Cherniatin

                                          if (ADC_IC > 0.0)
                                            {

                                              NumBinsToFill = ((int)(15 * 26.5 / TimeSampling));
                                              start = TIME_IC;
                                              if (start < 0)
                                                start = 0;
                                              stop = TIME_IC + NumBinsToFill;
                                              if (stop > MAXTECTIMES)
                                                stop = MAXTECTIMES;

                                              if (debugTecSlowSim > 10)
                                                printf(" start,stop = %d %d \n", start, stop);

                                              if (firsttime == 0)
                                                for (itmp = 0; itmp < NumBinsToFill; itmp++)
                                                  {
                                                    rtime = (0.75 + itmp) * TimeSampling;
                                                    stat = TecTimeResponce(&rtime, &ramp);
                                                    if (stat != 0)
                                                      printf(" mTecSlowSim -> ERROR in TecTimeResponce \n");
                                                    arraytime[itmp] = ramp * TimeSampling / 26.5;
                                                    firsttime = 1;
                                                  }
                                              for (iBin = start; iBin < stop; iBin++)
                                                {
                                                  ramp = arraytime[iBin - start];
                                                  amp = ADC_IC * ramp;

                                                  ghitid0 =
                                                    GhitIDStore[iwire][iBin][0];
                                                  ghitid1 =
                                                    GhitIDStore[iwire][iBin][1];
                                                  if (ghitid0 == 0 || ghitid0 == tecghit->get_mctrack(ighit))
                                                    {
                                                      GhitIDStore[iwire][iBin][0] = tecghit->get_mctrack(ighit);
                                                      FlashADC[iwire][iBin][0] += amp;
                                                    }
                                                  else
                                                    {
                                                      if (ghitid1 == 0 || ghitid1 == tecghit->get_mctrack(ighit))
                                                        {
                                                          GhitIDStore[iwire][iBin][1] = tecghit->get_mctrack(ighit);
                                                          FlashADC[iwire][iBin][1] += amp;
                                                        }
                                                      else // Overflows go into the 3rd slot
                                                        {
                                                          GhitIDStore[iwire][iBin][2] = tecghit->get_mctrack(ighit);
                                                          FlashADC[iwire][iBin][2] += amp;
                                                        }
                                                    }     // ghitid0 if

                                                } // iBin loop
                                            } // ADC_IC > 0

                                        } //***** end loop over time bins **************


                                    }
                                  else
                                  {} // end skip strange hits with the same entry and exit Y

                                  trdn.clear();
                                  trdbin.clear();

                                }   // iwire=MinWire,MaxWire


                              ListOfWires.clear();

                            }   // tecghit[ighit].plane==iplane

                          trde.clear();
                          trdpl.clear();
                          trdused.clear();

                        }   // ighit=0,tecghit_h->nok  -- end loop over Geant hits

                      //******************* Introduce noise **********************

                      // Simplified noise

                      for (ins = 0; ins < Noise; ins++)
                        {
                          noisebin = gsl_rng_uniform_int(rng, MAXTECTIMES);
                          noisewire = gsl_rng_uniform_int(rng, TecGeometryObject::get_NumWires(iplane));
                          mean = 0.0;
                          sigma = 2.0;
                          xgauss = gsl_ran_gaussian(rng, sigma) + mean;
                          noiseAMP = LowestNoise + fabs(xgauss);
                          FlashADC[noisewire][noisebin][0] += noiseAMP;
                        }

                      // For this plane, fill the dTecFemData and dTecGhitRaw tables

                      for (iwire = 0; iwire < MAXTECWIRES; iwire++)
                        {
                          iTotalAmp = 0;
                          TotalAmp = 0.0;
                          for (ibin = 0; ibin < MAXTECTIMES; ibin++)
                            {
                              for (j = 0; j < 3; j++)
                                {
                                  TotalAmp += FlashADC[iwire][ibin][j];
                                }
                              iTotalAmp += (int)(FlashADC[iwire][ibin][0] +
                                                 FlashADC[iwire][ibin][1] +
                                                 FlashADC[iwire][ibin][2]);
                            }

                          // Fill dTecFemData table once for each fired wire

                          if (iTotalAmp > 0.0)
                            {

                              TAO->setSoft(iarm, isect, izsign, iplane, iwire);
                              int iindex = isect * TECMAXPLANE * TECMAXSIDE + iplane * TECMAXSIDE + izsign;
                              icrate = TAO->getCrate();
                              islot = TAO->getSlot();
                              ipsadd = TAO->getPsadd();
                              ichan = TAO->getChannel();
                              dTecFemData->SetRowCount(iraw + 1);
                              dTecFemData->set_id(iraw, iraw);
                              dTecFemData->set_crate(iraw, icrate);
                              dTecFemData->set_slot(iraw, islot);
                              dTecFemData->set_psadd(iraw, ipsadd);
                              dTecFemData->set_ichan(iraw, ichan);

                              for (ibin = 0; ibin < MAXTECTIMES; ibin++)
                                {

                                  TotalInBin = 0.0;
                                  for (j = 0; j < 3; j++)
                                    {
                                      TotalInBin += FlashADC[iwire][ibin][j];
                                      Fraction[j] = FlashADC[iwire][ibin][j];
                                    }

                                  // Digitization: Charge --> Channels

                                  if (TotalInBin < SecondRange)
                                    {
                                      chTotalInBin = TotalInBin / Charge2Channels;
                                    }
                                  else
                                    {
                                      if (TotalInBin < ThirdRange)
                                        {
                                          chTotalInBin = 10.0 +
                                                         (TotalInBin - SecondRange) / Charge2Channels / 2.0;
                                        }
                                      else
                                        {
                                          chTotalInBin = 28.0 +
                                                         (TotalInBin - ThirdRange) / Charge2Channels / 50.0;
                                        }
                                    }

                                  if (chTotalInBin > 31.0)
                                    chTotalInBin = 31.0;

                                  dTecFemData->set_amplitude(ibin, iraw, ((int)(chTotalInBin)));

                                  //---------- Fill TecOut
                                  if (((int)(chTotalInBin)) > FadcCut)
                                    {
                                      tecout->AddTecHit(iindex, iwire, ibin,
                                                        ((int)(chTotalInBin)), charge, xyz, trackid);
                                    }

                                  // fill dTecGhitRaw table for each
                                  // time bin of each fired wire

                                  if (fillTecGhitRaw > 0)
                                    {

                                      for (j = 0; j < 3; j++)
                                        {

                                          if (GhitIDStore[iwire][ibin][j] != 0 &&
                                              (int)(chTotalInBin) > 0)
                                            {

                                              dTecGhitRaw->SetRowCount(itecgr + 1);
                                              dTecGhitRaw->set_ghitid(itecgr, GhitIDStore[iwire][ibin][j]);
                                              dTecGhitRaw->set_rawid(itecgr, tecout->getNHits() - 1);
                                              dTecGhitRaw->set_binnum(itecgr, ibin);
                                              dTecGhitRaw->set_fraction(itecgr, Fraction[j] / TotalInBin);
                                              itecgr++;

                                              if (FillTecGHitList != 0)
                                                {
                                                  //Fill TecGHitList

                                                  if (j == 0)
                                                    {
                                                      tecghitlist->AddTecGHit(tecghitlist->getNHits(),
                                                                                        GhitIDStore[iwire][ibin][j],
                                                                                        -1);
                                                    }
                                                  else
                                                    {
                                                      tecghitlist->set_tecghitid(tecghitlist->getNHits() - 1,
                                                                                 j,
                                                                                 GhitIDStore[iwire][ibin][j]);
                                                      tecghitlist->set_fraction (tecghitlist->getNHits() - 1,
                                                                                 0,
                                                                                 Fraction[0] / TotalInBin);
                                                      tecghitlist->set_fraction (tecghitlist->getNHits() - 1,
                                                                                 j,
                                                                                 Fraction[j] / TotalInBin);
                                                    }
                                                }
                                            }   			// GhitIDStore != 0
                                        }   			// j=0,2
				    } 				// fillTecGhitRaw
                                }   				// ibin=0,MAXTECTIMES
                              iraw++;
                            }   // TotalAmp > 0.0
                        }   // iwire=0,MAXTECWIRES
                    }   // active plane
                }   // izsign=0,1
            }   // iplane=0,3
        }   // isect=0,3
    }   // iarm=0,1


  // Pass the random number seed back for the next event
  RandSeed = irndm;

  if (Verbose > 0)
    {
      cout << "mTecSlowSim Finished. " << endl;
      cout << "dTecFemData->RowCount() = " << iraw << endl;
      cout << "dTecGhitRaw->RowCount() = " << itecgr << endl;
      cout << "TecOut = " << tecout->getNHits() << endl;
      cout << "TecGHitList = " << tecghitlist->getNHits() << endl;
    }

  Ntrd += myn;
  Ntrdabsorbed += myna;
  Etrd += mye;
  Etrdabsorbed += myea;
  if (Verbose > 0)
    {
      cout << "dE/dX: " << my << " " << myp << " " << mys << " " << myt << endl;
      cout << "Trd  : " << myn << " " << myna << " " << mye << " " << myea << endl;
      cout << "TrdTot : " << Ntrd << " " << Ntrdabsorbed << " "
      << Etrd << " " << Etrdabsorbed << endl;
    }

  return True;

}  // end mTecSlowSim module

//=============================================================================
int
mTecSlowSimModule::TecTimeResponce(float *time, float *ampl)
{
  // Taken from Valeri Cherniatin's (CHERNIAT@bnlarm.bnl.gov) TEC code
  // input time in nsec

  float t, tet0, aa, bb, anorm, x, etau, a, b, V1, V2;

  etau = 14.6; // nsec
  tet0 = 1.9;  // nsec
  aa = 1.95;
  bb = 19.5;
  anorm = 4.0e-4; // normalisation for etau = 14.6 nsec
  t = *time;
  x = t / etau;
  if (x == 0)
    {
      *ampl = 0.0;
      return 1;
    }

  if (x > 10000.0)
    {
      *ampl = 0.0;
    }
  else
    {
      a = etau / (aa * tet0) - 1;
      b = etau / (bb * tet0) - 1;

      V1 = 0.7 / a * (exp( -x) *
                      (x * x * x * x * x * x - 6.0 / a * (x * x * x * x * x) + 30.0 / (a * a) * (x * x * x * x) -
                       120.0 / (a * a * a) * (x * x * x) + 360.0 / (a * a * a * a) * (x * x) - 720. / (a * a * a * a * a) * x +
                       720.0 / (a * a * a * a * a * a)) - 720.0 / (a * a * a * a * a * a) * exp( -(a + 1) * x));

      V2 = 0.11 / b * (exp( -x) *
                       (x * x * x * x * x * x - 6.0 / b * (x * x * x * x * x) + 30.0 / (b * b) * (x * x * x * x) -
                        120.0 / (b * b * b) * (x * x * x) + 360.0 / (b * b * b * b) * (x * x) - 720. / (b * b * b * b * b) * x +
                        720.0 / (b * b * b * b * b * b)) - 720.0 / (b * b * b * b * b * b) * exp( -(b + 1) * x));

      *ampl = 4.0 * anorm * (V1 + V2) * etau;
    }

  return 0;
}

//=============================================================================
int mTecSlowSimModule::TrdIon(int gastype, float ee)
{
  // Calculates ionization from electron produced by TR X-ray
  // param = 2.20e-5 (Xe) 2.64e-5 (Ar) 2.80e-5 (methane) 2.66e-5 (P10)
  // 4.10e-5 (He)

  float param = 0.;

  switch (gastype)
    {
    case 1:              // P10
      param = 2.66e-5;
      break;
    case 2:              // 0.9 Xe + 0.1 CH4
      param = 2.26e-5;
      break;
    case 3:
      param = 2.64e-5;  // Ar
      break;
    case 4:
      param = 3.48e-5;  // 0.45 Xe + 0.45 He + 0.1 CH4
      break;
    default:
      cerr << PHWHERE << "ERROR: unknown gas type " << gastype << endl;
      break;
    }

  ee = ee / 1000.; // convert from keV to MeV

  int nSec = (int)(ee / param);

  return nSec;
}

//=============================================================================
int
mTecSlowSimModule::SecIon(long *lseed,
                          int *lgastype,
                          int *lparticle,
                          float *lmomentum)
// Generates delta-electron energy in the range from 1 eV to EM
// and calculates secondary ionization. param =
// 2.20e-5 (Xe) 2.64e-5 (Ar) 2.80e-5 (methane) 2.66e-5 (P10)
// 4.10e-5 (He)
{

  float param = 0., b, g, p, pp, C, random, E, E0, EM, Mass2, Mass;
  int particle, nSec, gastype;
  long iseed;


  iseed = *lseed;
  gastype = *lgastype;

  switch (gastype)
    {
    case 1:  		// P10
      param = 2.66e-5;
      break;
    case 2:  		// 0.9 Xe + 0.1 CH4
      param = 2.26e-5;
      break;
    case 3:
      param = 2.64e-5;  // Ar
      break;
    case 4:
      param = 3.48e-5;  // 0.45 Xe + 0.45 He + 0.1 CH4
      break;
    default:
      cerr << PHWHERE << "ERROR: unknown gas type " << gastype << endl;
      break;
    }

  particle = *lparticle;
  Mass = get_mass(particle);
  Mass2 = Mass * Mass;
  p = *lmomentum;
  pp = p * p / Mass2;

  b = pp / (pp + 1);	// beta^2
  g = pp + 1;	// gamma^2
  E0 = 1.0e-5;
  if (particle == 2 || particle == 3)
    {
      EM = p;
    }
  else
    {
      EM = 2 * 0.511 * b * g;
    }
  C = EM / (EM - E0);

  random = gsl_rng_uniform(rng);

  if ((1 - random / C) == 0)
    {
      nSec = 0;
    }
  else
    {
      E = E0 / (1 - random / C);
      nSec = (int)(E / param);
    }

  //cout << "SEC: " << p << " " << EM << " " << random << " " << E << " " << nSec << endl;

  *lseed = iseed; // return the modified random seed

  return nSec;
}

//=============================================================================
float
mTecSlowSimModule::PriIon(int *lgastype,
                          int *lparticle,
                          float *lmomentum)

// Calculates number of primary ionization collisions
{
  int gastype, Particle;
  float Momentum, Mass2, ELoss, MaxEr;
  float Xer01, Xer02, ErA1, ErA2, t2, tt, const2, Xer1, Xer2;

  Particle = *lparticle;
  if ((Particle < 2) || (Particle > 16 && Particle != 25))
    {
      if ((Particle != 45) && (Particle != 46) &&
          (Particle != 47) && (Particle != 49))
        {
          cerr << PHWHERE << "WARNING: Unknown particle type: " << Particle << endl;
          Particle = 9;
        }
    }
  gastype = *lgastype;
  Momentum = *lmomentum;

  Mass2 = get_mass(Particle) * get_mass(Particle);
  float Charge = 1.;
  if (Particle == 47 || Particle == 49)
    Charge = 2.;

  //---------------- Set gas dependent parameters. ------------------------
  switch (gastype)
    {
    case 1:  		// 10% CH4 + 90% Ar  (P10 mixture)
      Xer01 = 41.0;
      Xer02 = 150.0;
      ErA1 = 1.7;
      ErA2 = 11.5;
      MaxEr = 34.7;
      break;
    case 2:  		// 0.9 Xe + 0.1 CH4
      Xer01 = 40.6;
      Xer02 = 200.0;
      ErA1 = 3.554;
      ErA2 = 11.31;
      MaxEr = 69.6;
      break;
    case 3:  		// Ar
      Xer01 = 41.6;
      Xer02 = 155.0;
      ErA1 = 1.828;
      ErA2 = 11.45;
      MaxEr = 36.5;
      break;
    case 4:  		// 0.45 Xe + 0.45 He + 0.1 CH4
      Xer01 = 44.6;
      Xer02 = 200.0;
      ErA1 = 1.899;
      ErA2 = 11.47;
      MaxEr = 37.4;
      break;
    default:
      cerr << PHWHERE << "ERROR: Illegal Gas Type: " << gastype << endl;
      return 0.;
    }

  if ((Particle == 3) || (Particle == 2)) // energy loss by e+-; correct
    // only for E > 80 MeV
    {
      ELoss = MaxEr;
    }
  else
    {
      Xer1 = Xer01 * sqrt(Mass2);
      Xer2 = Xer02 * sqrt(Mass2) * 1.1;
      t2 = Xer2 * Xer2 / Mass2;
      tt = Momentum * Momentum / Mass2;
      const2 = ErA1 * (1.0 + t2) / t2 * (ErA2 + log(t2) - t2 / (1.0 + t2));
      if (tt < Xer1)
        {
          ELoss = ErA1 * (1.0 + tt) / tt * (ErA2 + log(tt) - tt / (1.0 + tt));
        }
      else
        {
          if (tt > Xer2)
            {
              ELoss = MaxEr;
            }
          else
            {
              ELoss = ErA1 * (1.0 + tt) / tt * (ErA2 + log(tt) - tt / (1.0 + tt)) -
                      ((tt - Xer1) / (Xer2 - Xer1)) * ((tt - Xer1) / (Xer2 - Xer1)) * (const2 - MaxEr);
            }
        }
    }

  // ELoss = average number of primary collisions per 1 cm
  ELoss = Charge * Charge * ELoss;

  return ELoss;
}

