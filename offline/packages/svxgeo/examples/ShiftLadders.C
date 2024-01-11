////////////////////////////////////////////////////////////////////////////////
//
// Shift VTX ladders randomly in:
// z
// r
// s=r*phi
//
////////////////////////////////////////////////////////////////////////////////
//
// T. Rinn
// 1 Feb 2016
//
////////////////////////////////////////////////////////////////////////////////
//
// Can be run compiled:
// $ root ShiftLadders.C++
//
////////////////////////////////////////////////////////////////////////////////


#include <TRandom3.h>
#include <TGeoManager.h>
#include <TGeoVolume.h>
#include <TCanvas.h>

#include <iostream>

using namespace std;

void ShiftLadders(const char *pfa = "parfiles/svxPISA.par",      // input geo file
                  const char *pfb = "modifiedPISA.par", // output geo file
                  const int verbosity = 0)
{

  //---------------------------------------------------//
  // SET PARAMETERS
  //---------------------------------------------------//

  // Select which shifts to apply
  bool appltShiftZ = false;
  bool appltShiftR = false;
  bool appltShiftS = true; //s=r*phi

  // Set width's for random shifts [cm]
  float sigZ = 50e-4;
  float sigR = 50e-4;
  float sigS = 50e-4;

  // Set the distribution to be sampled from
  // true  : Sample from a uniform distribution between [-sig, sig]
  // false : Sample from a Gaussian distribution with width sig
  bool sampleUniform = false;

  // Set layers & ladders
  const int NLAYERS = 4;
  const int NLADDERS[] = {10, 20, 16, 24};

  //---------------------------------------------------//
  // SHIFT GEOMETRY
  //---------------------------------------------------//

  // Load the geometry into the TGeo class and add
  // sensors to the default hall volume (100x100x100 cm)
  SvxTGeo *geo = new SvxTGeo;
  geo->ReadParFile(pfa);
  geo->MakeTopVolume(100, 100, 100);
  geo->AddSensors();

  TGeoManager *mgr = geo->GeoManager();
  // Done building model.
  // Close geometry to check for problems (overlapping boundaries)
  mgr->CloseGeometry();

  // Create a random number generator
  // uses time to select a seed, or can specify your own
  TRandom3 *shiftFunc = new TRandom3();
  shiftFunc->SetSeed(time(NULL));  


  // loops over layers you want to apply shifts on
  for (int lyr = 0; lyr < NLAYERS; lyr++) 
  {
    // loops over ladders in each layer to apply shift
    for (int ldr = 0; ldr < NLADDERS[lyr]; ldr++) 
    {
      if (appltShiftZ)
      {
        float shiftz = 0;
        if (sampleUniform)
          shiftz = shiftFunc->Uniform(-1 * sigZ, sigZ);
        else
          shiftz = shiftFunc->Gaus() * sigZ;
        //of form (layer number, ladder number, xshift, yshift, zshift)
        geo->TranslateLadder(lyr, ldr, 0, 0, shiftz);
        if (verbosity > 0)
        {
          cout << "layer, ladder " << lyr << ", " << ldr
               << " shifted in z by " << shiftz
               << endl;
        }
      } // appltShiftZ

      if (appltShiftR)
      {
        float shiftr = 0;
        if (sampleUniform)
          shiftr = shiftFunc->Uniform(-1 * sigR, sigR);
        else
          shiftr = shiftFunc->Gaus() * sigR;
        //of form (layer number, ladder number, shift in r direction)
        geo->MoveLadderRadially(lyr, ldr, shiftr);
        if (verbosity > 0)
        {
          cout << "layer, ladder " << lyr << ", " << ldr
               << " shifted in r by " << shiftr
               << endl;
        }
      } // appltShiftR

      if (appltShiftS)
      {
        float shifts = 0;
        if (sampleUniform)
          shifts = shiftFunc->Uniform(-1 * sigS, sigS);
        else
          shifts = shiftFunc->Gaus() * sigS;
        //of form (layer number, ladder number, shift along rphi )
        geo->RotateLadderRPhi(lyr, ldr, shifts);      
        if (verbosity > 0)
        {
          cout << "layer, ladder " << lyr << ", " << ldr
               << " shifted in s (rphi) by " << shifts
               << endl;
        }
      } // appltShiftS
    } // ldr
  } // lyr

  // Write new geometry file
  geo->WriteParFile(pfb);



  /*
  // Press j,k to zoom; u,i to look up/down; h,l to look left, right.
  // uncomment the following lines out, to generate a 3d drawing of the geometry
  TGeoVolume *top = mgr->GetTopVolume();

  TCanvas *c = new TCanvas("c", "svx model",500, 500);
  c->SetFillColor(kBlack);
  top->Draw();
  */


}








