{
//////////////////////////////////////////////////////////
//   This file has been automatically generated
//     (Wed Apr 14 15:53:03 2004 by ROOT version3.01/05)
//   from TTree align_vars2/align_vars2
//   found on file: zerofield-mutoo_ana_83663_align_vars2.root
//////////////////////////////////////////////////////////
//
// Alignment code by PLM 
// Analyzes ntuple output of mutoo simple straight line track finder
//  for field off data (p-p), ntuple file align.root
// Alignment constants are ds = r dphi, dr, dz, roll angle, pitch angle
//  and yaw angle
// extracts ds = r dphi (precision coordinate) and dr (radius) directly
//  from histograms for each half octant
// extracts dz from studies of dphi vs phi, dphi vs phislope, dr vs phi
// extracts roll angle from dphi vs r
// pitch and yaw not yet determined, could study by determining dz
//  with a cut on rcenter (pitch) or phicenter (yaw)

//Reset ROOT and connect tree file
   gROOT->Reset();
   #include <iostream.h>
   #include <stdio.h>
   #include <math.h>
   TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("align.root");
   if (!f) {
      f = new TFile("align.root");
   }
   TTree *align_vars2 = (TTree*)gDirectory->Get("align_vars2");
   c1 = new TCanvas("c1","canvas");
   c1->SetGrid();
   gStyle->SetOptFit();

//Declaration of leaves types, contents are event number, arm number, octant number, half octant number,
// xyz of track at 1,2,3 tracking station, xyr residual of line fit at Station 2, theta at station 1,2,3,
// phi at 1,2,3, r (radius) at 1,2,3,  etc.

   Float_t         x1;
   Float_t         y1;
   Float_t         z1;
   Float_t         x2;
   Float_t         y2;
   Float_t         z2;
   Float_t         x3;
   Float_t         y3;
   Float_t         z3;
   Float_t         theta1;
   Float_t         theta2;
   Float_t         theta3;
   Float_t         phi1;
   Float_t         phi2;
   Float_t         phi3;
   Float_t         r1;
   Float_t         r2;
   Float_t         r3;
   Float_t         rPhi1;
   Float_t         rPhi2;
   Float_t         rPhi3;
   Float_t         x1Proj;
   Float_t         y1Proj;
   Float_t         r1Proj;
   Float_t         phi1Proj;
   Float_t         x2Proj;
   Float_t         y2Proj;
   Float_t         r2Proj;
   Float_t         phi2Proj;
   Float_t         x3Proj;
   Float_t         y3Proj;
   Float_t         r3Proj;
   Float_t         phi3Proj;
   Float_t         x1Proj1;
   Float_t         x1Proj2;
   Float_t         x1Proj3;
   Float_t         y1Proj1;
   Float_t         y1Proj2;
   Float_t         y1Proj3;
   Float_t         r1Proj1;
   Float_t         r1Proj2;
   Float_t         r1Proj3;
   Float_t         x2Proj1;
   Float_t         x2Proj2;
   Float_t         x2Proj3;
   Float_t         y2Proj1;
   Float_t         y2Proj2;
   Float_t         y2Proj3;
   Float_t         r2Proj1;
   Float_t         r2Proj2;
   Float_t         r2Proj3;
   Float_t         x3Proj1;
   Float_t         x3Proj2;
   Float_t         x3Proj3;
   Float_t         y3Proj1;
   Float_t         y3Proj2;
   Float_t         y3Proj3;
   Float_t         r3Proj1;
   Float_t         r3Proj2;
   Float_t         r3Proj3;
   Float_t         phi2Proj1;
   Float_t         phi2Proj2;
   Float_t         phi2Proj3;
   Float_t         x1Gap1;
   Float_t         x1Gap2;
   Float_t         x1Gap3;
   Float_t         x2Gap1;
   Float_t         x2Gap2;
   Float_t         x2Gap3;
   Float_t         x3Gap1;
   Float_t         x3Gap2;
   Float_t         x3Gap3;
   Float_t         y1Gap1;
   Float_t         y1Gap2;
   Float_t         y1Gap3;
   Float_t         y2Gap1;
   Float_t         y2Gap2;
   Float_t         y2Gap3;
   Float_t         y3Gap1;
   Float_t         y3Gap2;
   Float_t         y3Gap3;
   Float_t         z1Gap1;
   Float_t         z1Gap2;
   Float_t         z1Gap3;
   Float_t         z2Gap1;
   Float_t         z2Gap2;
   Float_t         z2Gap3;
   Float_t         z3Gap1;
   Float_t         z3Gap2;
   Float_t         z3Gap3;
   Float_t         r1Gap1;
   Float_t         r1Gap2;
   Float_t         r1Gap3;
   Float_t         r2Gap1;
   Float_t         r2Gap2;
   Float_t         r2Gap3;
   Float_t         r3Gap1;
   Float_t         r3Gap2;
   Float_t         r3Gap3;
   Float_t         phi2Gap1;
   Float_t         phi2Gap2;
   Float_t         phi2Gap3;
   Float_t         oct1;
   Float_t         oct2;
   Float_t         oct3;
   Float_t         half1;
   Float_t         half2;
   Float_t         half3;
   Float_t         trk_arm;

//Set branch addresses
   align_vars2->SetBranchAddress("x1",&x1);
   align_vars2->SetBranchAddress("y1",&y1);
   align_vars2->SetBranchAddress("z1",&z1);
   align_vars2->SetBranchAddress("x2",&x2);
   align_vars2->SetBranchAddress("y2",&y2);
   align_vars2->SetBranchAddress("z2",&z2);
   align_vars2->SetBranchAddress("x3",&x3);
   align_vars2->SetBranchAddress("y3",&y3);
   align_vars2->SetBranchAddress("z3",&z3);
   align_vars2->SetBranchAddress("theta1",&theta1);
   align_vars2->SetBranchAddress("theta2",&theta2);
   align_vars2->SetBranchAddress("theta3",&theta3);
   align_vars2->SetBranchAddress("phi1",&phi1);
   align_vars2->SetBranchAddress("phi2",&phi2);
   align_vars2->SetBranchAddress("phi3",&phi3);
   align_vars2->SetBranchAddress("r1",&r1);
   align_vars2->SetBranchAddress("r2",&r2);
   align_vars2->SetBranchAddress("r3",&r3);
   align_vars2->SetBranchAddress("rPhi1",&rPhi1);
   align_vars2->SetBranchAddress("rPhi2",&rPhi2);
   align_vars2->SetBranchAddress("rPhi3",&rPhi3);
   align_vars2->SetBranchAddress("x1Proj",&x1Proj);
   align_vars2->SetBranchAddress("y1Proj",&y1Proj);
   align_vars2->SetBranchAddress("r1Proj",&r1Proj);
   align_vars2->SetBranchAddress("phi1Proj",&phi1Proj);
   align_vars2->SetBranchAddress("x2Proj",&x2Proj);
   align_vars2->SetBranchAddress("y2Proj",&y2Proj);
   align_vars2->SetBranchAddress("r2Proj",&r2Proj);
   align_vars2->SetBranchAddress("phi2Proj",&phi2Proj);
   align_vars2->SetBranchAddress("x3Proj",&x3Proj);
   align_vars2->SetBranchAddress("y3Proj",&y3Proj);
   align_vars2->SetBranchAddress("r3Proj",&r3Proj);
   align_vars2->SetBranchAddress("phi3Proj",&phi3Proj);
   align_vars2->SetBranchAddress("x1Proj1",&x1Proj1);
   align_vars2->SetBranchAddress("x1Proj2",&x1Proj2);
   align_vars2->SetBranchAddress("x1Proj3",&x1Proj3);
   align_vars2->SetBranchAddress("y1Proj1",&y1Proj1);
   align_vars2->SetBranchAddress("y1Proj2",&y1Proj2);
   align_vars2->SetBranchAddress("y1Proj3",&y1Proj3);
   align_vars2->SetBranchAddress("r1Proj1",&r1Proj1);
   align_vars2->SetBranchAddress("r1Proj2",&r1Proj2);
   align_vars2->SetBranchAddress("r1Proj3",&r1Proj3);
   align_vars2->SetBranchAddress("x2Proj1",&x2Proj1);
   align_vars2->SetBranchAddress("x2Proj2",&x2Proj2);
   align_vars2->SetBranchAddress("x2Proj3",&x2Proj3);
   align_vars2->SetBranchAddress("y2Proj1",&y2Proj1);
   align_vars2->SetBranchAddress("y2Proj2",&y2Proj2);
   align_vars2->SetBranchAddress("y2Proj3",&y2Proj3);
   align_vars2->SetBranchAddress("r2Proj1",&r2Proj1);
   align_vars2->SetBranchAddress("r2Proj2",&r2Proj2);
   align_vars2->SetBranchAddress("r2Proj3",&r2Proj3);
   align_vars2->SetBranchAddress("x3Proj1",&x3Proj1);
   align_vars2->SetBranchAddress("x3Proj2",&x3Proj2);
   align_vars2->SetBranchAddress("x3Proj3",&x3Proj3);
   align_vars2->SetBranchAddress("y3Proj1",&y3Proj1);
   align_vars2->SetBranchAddress("y3Proj2",&y3Proj2);
   align_vars2->SetBranchAddress("y3Proj3",&y3Proj3);
   align_vars2->SetBranchAddress("r3Proj1",&r3Proj1);
   align_vars2->SetBranchAddress("r3Proj2",&r3Proj2);
   align_vars2->SetBranchAddress("r3Proj3",&r3Proj3);
   align_vars2->SetBranchAddress("phi2Proj1",&phi2Proj1);
   align_vars2->SetBranchAddress("phi2Proj2",&phi2Proj2);
   align_vars2->SetBranchAddress("phi2Proj3",&phi2Proj3);
   align_vars2->SetBranchAddress("x1Gap1",&x1Gap1);
   align_vars2->SetBranchAddress("x1Gap2",&x1Gap2);
   align_vars2->SetBranchAddress("x1Gap3",&x1Gap3);
   align_vars2->SetBranchAddress("x2Gap1",&x2Gap1);
   align_vars2->SetBranchAddress("x2Gap2",&x2Gap2);
   align_vars2->SetBranchAddress("x2Gap3",&x2Gap3);
   align_vars2->SetBranchAddress("x3Gap1",&x3Gap1);
   align_vars2->SetBranchAddress("x3Gap2",&x3Gap2);
   align_vars2->SetBranchAddress("x3Gap3",&x3Gap3);
   align_vars2->SetBranchAddress("y1Gap1",&y1Gap1);
   align_vars2->SetBranchAddress("y1Gap2",&y1Gap2);
   align_vars2->SetBranchAddress("y1Gap3",&y1Gap3);
   align_vars2->SetBranchAddress("y2Gap1",&y2Gap1);
   align_vars2->SetBranchAddress("y2Gap2",&y2Gap2);
   align_vars2->SetBranchAddress("y2Gap3",&y2Gap3);
   align_vars2->SetBranchAddress("y3Gap1",&y3Gap1);
   align_vars2->SetBranchAddress("y3Gap2",&y3Gap2);
   align_vars2->SetBranchAddress("y3Gap3",&y3Gap3);
   align_vars2->SetBranchAddress("z1Gap1",&z1Gap1);
   align_vars2->SetBranchAddress("z1Gap2",&z1Gap2);
   align_vars2->SetBranchAddress("z1Gap3",&z1Gap3);
   align_vars2->SetBranchAddress("z2Gap1",&z2Gap1);
   align_vars2->SetBranchAddress("z2Gap2",&z2Gap2);
   align_vars2->SetBranchAddress("z2Gap3",&z2Gap3);
   align_vars2->SetBranchAddress("z3Gap1",&z3Gap1);
   align_vars2->SetBranchAddress("z3Gap2",&z3Gap2);
   align_vars2->SetBranchAddress("z3Gap3",&z3Gap3);
   align_vars2->SetBranchAddress("r1Gap1",&r1Gap1);
   align_vars2->SetBranchAddress("r1Gap2",&r1Gap2);
   align_vars2->SetBranchAddress("r1Gap3",&r1Gap3);
   align_vars2->SetBranchAddress("r2Gap1",&r2Gap1);
   align_vars2->SetBranchAddress("r2Gap2",&r2Gap2);
   align_vars2->SetBranchAddress("r2Gap3",&r2Gap3);
   align_vars2->SetBranchAddress("r3Gap1",&r3Gap1);
   align_vars2->SetBranchAddress("r3Gap2",&r3Gap2);
   align_vars2->SetBranchAddress("r3Gap3",&r3Gap3);
   align_vars2->SetBranchAddress("phi2Gap1",&phi2Gap1);
   align_vars2->SetBranchAddress("phi2Gap2",&phi2Gap2);
   align_vars2->SetBranchAddress("phi2Gap3",&phi2Gap3);
   align_vars2->SetBranchAddress("oct1",&oct1);
   align_vars2->SetBranchAddress("oct2",&oct2);
   align_vars2->SetBranchAddress("oct3",&oct3);
   align_vars2->SetBranchAddress("half1",&half1);
   align_vars2->SetBranchAddress("half2",&half2);
   align_vars2->SetBranchAddress("half3",&half3);
   align_vars2->SetBranchAddress("trk_arm",&trk_arm);

//     This is the loop skeleton
//       To read only selected branches, Insert statements like:
// align_vars2->SetBranchStatus("*",0);  // disable all branches
// TTreePlayer->SetBranchStatus("branchname",1);  // activate branchname

//define histograms
// g1-9 are radius, theta and phi of tracks at stations 1-3 (debugging)
// g10-17 are correlations of track positions at different stations (debugging)
// g18-25 are correlations of track positions for half octants (debugging)
// g26-31 more correlations checking station 1-2 projections versus station 2-3 (debugging)
// g32-37 are stub slopes in x,y at each of 3 stations, (stubs are line fits to
//  cathode clusters in one station) (debugging)
// g38-45 are correlations of stub slopes at different stations (debugging)
// g46-53 are residuals from projections of stub slopes from one station to another,
//  these help determine which station is out of alignment
// g54-63 are as above, octant by octant (2Dim)
// g64-77 mix of residuals from stub projection (titled ...proj) or fits (2Dim)
// g78-105 mostly profile histograms studying correlations of residuals, with octants
//  half octants, postion in octants, etc. These are the most useful histograms.
//  some are 2D used to set examine cuts for the profile histograms

/*   TH1F *g1 = new TH1F("g1","R1",50,0.,200.);
   TH1F *g2 = new TH1F("g2","R2",50,0.,300.);
   TH1F *g3 = new TH1F("g3","R3",50,0.,400.);
   TH1F *g4 = new TH1F("g4","THETA1",50,10.,35.);
   TH1F *g5 = new TH1F("g5","THETA2",50,10.,35.);
   TH1F *g6 = new TH1F("g6","THETA3",50,10.,35.);
   TH1F *g7 = new TH1F("g7","PHI1",100,-180.,180.);
   TH1F *g8 = new TH1F("g8","PHI2",100,-180.,180.);
   TH1F *g9 = new TH1F("g9","PHI3",100,-180.,180.);
   TH2F *g10 = new TH2F("g10","THETA2 v THETA1",50,10.,35.,50,10.,35.);
   TH2F *g11 = new TH2F("g11","THETA3 v THETA2",50,10.,35.,50,10.,35.);
   TH2F *g12 = new TH2F("g12","PHI2 v PHI1",50,-180.,180.,50,-180.,180.);
   TH2F *g13 = new TH2F("g13","PHI3 v PHI2",50,-180.,180.,50,-180.,180.);
   TH1F *g14 = new TH1F("g14","THETA2-THETA1",50,-10.,10.);
   TH1F *g15 = new TH1F("g15","THETA3-THETA2",50,-10.,10.);
   TH1F *g16 = new TH1F("g16","PHI2-PHI1",50,-10.,10.);
   TH1F *g17 = new TH1F("g17","PHI3-PHI2",50,-10.,10.); */
/*   TH1F *g18 = new TH1F("g18","PHI2-PHI1 HALF 0",50,-10.,10.);
   TH1F *g19 = new TH1F("g19","PHI2-PHI1 HALF 1",50,-10.,10.);
   TH1F *g20 = new TH1F("g20","PHI3-PHI2 HALF 0",50,-10.,10.);
   TH1F *g21 = new TH1F("g21","PHI3-PHI2 HALF 1",50,-10.,10.);
   TH1F *g22 = new TH1F("g22","THETA2-THETA1 HALF 0",50,-10.,10.);
   TH1F *g23 = new TH1F("g23","THETA2-THETA1 HALF 1",50,-10.,10.);
   TH1F *g24 = new TH1F("g24","THETA3-THETA2 HALF 0",50,-10.,10.);
   TH1F *g25 = new TH1F("g25","THETA3-THETA2 HALF 1",50,-10.,10.); */
   TH1F *g26 = new TH1F("g26","Z VERTEX 1-2",50,-200.,100.);
   TH1F *g27 = new TH1F("g27","Z VERTEX 2-3",50,-200.,100.);
   TH1F *g28 = new TH1F("g28","RSLOPE 1-2",50,-1.,1.);
   TH1F *g29 = new TH1F("g29","RSLOPE 2-3",50,-1.,1.);
   TH2F *g30 = new TH2F("g30","SLOPE12 v SLOPE23",50,-1.,0.,50,-1.,0.);
   TH1F *g31 = new TH1F("g31","RSLOPE DIFF",50,-0.2,0.2);
/*   TH1F *g32 = new TH1F("g32","DXDZ1",50,-2.,2.);
   TH1F *g33 = new TH1F("g33","DYDZ1",50,-2.,2.);
   TH1F *g34 = new TH1F("g34","DXDZ2",50,-2.,2.);
   TH1F *g35 = new TH1F("g35","DYDZ2",50,-2.,2.);
   TH1F *g36 = new TH1F("g36","DXDZ3",50,-2.,2.);
   TH1F *g37 = new TH1F("g37","DYDZ3",50,-2.,2.); */
/*   TH2F *g38 = new TH2F("g38","DXDZ1 v DXDZ2",50,-2.,2.,50,-2.,2.);
   TH2F *g39 = new TH2F("g39","DYDZ1 v DYDZ2",50,-2.,2.,50,-2.,2.);
   TH2F *g40 = new TH2F("g40","DXDZ2 v DXDZ3",50,-2.,2.,50,-2.,2.);
   TH2F *g41 = new TH2F("g41","DYDZ3 v DYDZ3",50,-2.,2.,50,-2.,2.);
   TH1F *g42 = new TH1F("g42","DXDZ1 - DXDZ2",50,-2.,2.);
   TH1F *g43 = new TH1F("g43","DYDZ1 - DYDZ2",50,-2.,2.);
   TH1F *g44 = new TH1F("g44","DXDZ2 - DXDZ3",50,-2.,2.);
   TH1F *g45 = new TH1F("g45","DYDZ2 - DYDZ3",50,-2.,2.); */
/*   TH1F *g46 = new TH1F("g46","DXDZ1 PROJ STA2",50,-10.,10.);
   TH1F *g47 = new TH1F("g47","DYDZ1 PROJ STA2",50,-10.,10.);
   TH1F *g48 = new TH1F("g48","DXDZ2 PROJ STA3",50,-10.,10.);
   TH1F *g49 = new TH1F("g49","DYDZ2 PROJ STA3",50,-10.,10.);
   TH1F *g50 = new TH1F("g50","DXDZ2 PROJ STA1",50,-10.,10.);
   TH1F *g51 = new TH1F("g51","DYDZ2 PROJ STA1",50,-10.,10.);
   TH1F *g52 = new TH1F("g52","DXDZ3 PROJ STA2",50,-10.,10.);
   TH1F *g53 = new TH1F("g53","DYDZ3 PROJ STA2",50,-10.,10.); */
/*   TH2F *g54 = new TH2F("g54","DXPROJ STA2 OCTANT",8,0.,8.,50,-20.,20.);
   TH2F *g55 = new TH2F("g55","DYPROJ STA2 OCTANT",8,0.,8.,50,-20.,20.);
   TH2F *g56 = new TH2F("g56","DXPROJ STA1 OCTANT",8,0.,8.,50,-20.,20.);
   TH2F *g57 = new TH2F("g57","DYPROJ STA1 OCTANT",8,0.,8.,50,-20.,20.);
   TH2F *g58 = new TH2F("g58","DRPROJ STA2 OCTANT",8,0.,8.,50,-20.,20.);
   TH2F *g59 = new TH2F("g59","DPHIPROJ STA2 OCTANT",8,0.,8.,50,-0.1,0.1);
   TH2F *g60 = new TH2F("g60","DSPROJ STA2 OCTANT",8,0.,8.,50,-10.,10.);*/
/*   TH2F *g61 = new TH2F("g61","DRPROJ STA1 OCTANT",8,0.,8.,50,-50.,50.);
   TH2F *g62 = new TH2F("g62","DPHIPROJ STA1 OCTANT",8,0.,8.,50,-0.1,0.1);
   TH2F *g63 = new TH2F("g63","DSPROJ STA1 OCTANT",8,0.,8.,50,-10.,10.); */
   TH2F *g64 = new TH2F("g64","DSLINE STA2 OCTANT",8,0.,8.,50,-5.,5.);
/*   TH2F *g65 = new TH2F("g65","DSPROJ STA1 OCTANT HALF=0",8,0.,8.,50,-10.,10.);
     TH2F *g66 = new TH2F("g66","DSPROJ STA1 OCTANT HALF=1",8,0.,8.,50,-10.,10.); */
   TH2F *g67 = new TH2F("g67","DSLINE STA3 OCTANT HALF=0",8,0.,8.,50,-20.,20.);
   TH2F *g68 = new TH2F("g68","DSLINE STA3 OCTANT HALF=1",8,0.,8.,50,-20.,20.);
   TH2F *g69 = new TH2F("g69","DPHI STA3 OCTANT HALF=0",8,0.,8.,50,-0.5,0.5);
   TH2F *g70 = new TH2F("g70","DPHI STA3 OCTANT HALF=1",8,0.,8.,50,-0.5,0.5);
   TH2F *g71 = new TH2F("g71","DSLINE STA2 OCTANT HALF=0",8,0.,8.,50,-1.,1.);
   TH2F *g72 = new TH2F("g72","DSLINE STA2 OCTANT HALF=1",8,0.,8.,50,-1.,1.);
//   TH2F *g73 = new TH2F("g73","DRPROJ STA3 OCTANT",8,0.,8.,50,-50.,50.);
   TH2F *g74 = new TH2F("g74","DSLINE STA2 VS DPHI",50,-0.6,0.6,50,-1.,1.);
   TH2F *g75 = new TH2F("g75","DPHI STA2 VS DPHI13",50,-0.6,0.6,50,-0.01,0.01);
   TH2F *g76 = new TH2F("g76","DR STA2 VS R2",50,50.,250.,50,-20.,20.);
   TH2F *g77 = new TH2F("g77","DPHI STA2 VS PHI2",160,-3.141,3.141,50,-0.01,0.01);
   TProfile *g78 = new TProfile("g78","DPHI STA2 VS DPHI13",50,-0.6,0.6);
   TProfile *g79 = new TProfile("g79","DR STA2 VS R2",50,50.,250.);
   TProfile *g80 = new TProfile("g80","DPHI STA2 VS PHI2",160,-3.141,3.141);
   TProfile *g81 = new TProfile("g81","DPHI STA2 VS PHI2",50,-0.4,0.4);
   TProfile *g82 = new TProfile("g82","DR STA2 VS RSLOPE",100,-1.,1.);
   TH2F *g83 = new TH2F("g83","DPHI13 STA2 VS RSLOPE",50,-1.,0.141,50,-0.6,0.6);
   TProfile *g84 = new TProfile("g84","DPHI STA2 VS DPHI13 HALF=0",50,-0.6,0.6);
   TProfile *g85 = new TProfile("g85","DPHI STA2 VS DPHI13 HALF=1",50,-0.6,0.6);
   TProfile *g86 = new TProfile("g86","DR STA2 VS PHI2",50,-0.4,0.4);
   TProfile *g87 = new TProfile("g87","DPHI STA2 VS R2",50,50.,250.);
   TH2F *g88 = new TH2F("g88","DSLINE STA2 VS PHI2-1",50,-0.3,0.3,50,-5.,5.);
//   TH2F *g89 = new TH2F("g89","DSLINE STA2 VS PHI2-1",50,-0.3,0.3,50,-10.,10.);
   TProfile *g89 = new TProfile("g89","DSLINE STA2 VS PHI3-1",50,-0.6,0.6);
   TH2F *g90 = new TH2F("g90","RSLOPE STA2 VS R2",50,50.,250.,100,-1.,1.);
   TProfile *g91 = new TProfile("g91","DPHI STA2 VS DPHI13 SAME HALF",50,-0.6,0.6);
   TH2F *g92 = new TH2F("g92","RSLOPE RESID STA2 VS R2",50,50.,250.,50,-0.4,0.4);
   TProfile *g93 = new TProfile("g93","DR STA2 VS R1",50,30.,140.);
   TProfile *g94 = new TProfile("g94","DR STA2 VS R3",50,100.,325.);
   TProfile *g95 = new TProfile("g95","DSLINE STA2 VS PHI2",50,-0.4,0.4);
   TProfile *g96 = new TProfile("g96","DSLINE STA3 VS PHI2",50,-0.4,0.4);
   TProfile *g97 = new TProfile("g97","DSLINE STA3 VS PHI3-1",50,-0.6,0.6);
/*   TProfile *g98 = new TProfile("g98","DRCR1 STA2 VS R2",50,50.,250.);
   TProfile *g99 = new TProfile("g99","DRCR2 STA2 VS R2",50,50.,250.);
   TProfile *g100 = new TProfile("g100","DRCR3 STA2 VS R2",50,50.,250.); */
   TH2F *g101 = new TH2F("g101","DR STA2 OCTANT HALF 0",8,0.,8.,50,-5.,5.);
   TH2F *g102 = new TH2F("g102","DR STA2 OCTANT HALF 1",8,0.,8.,50,-5.,5.);
   TH2F *g103 = new TH2F("g103","DS STA3 OCTANT HALF 0",8,0.,8.,50,-1.,1.);
   TH2F *g104 = new TH2F("g104","DS STA3 OCTANT HALF 1",8,0.,8.,50,-1.,1.);
   TH2F *g105 = new TH2F("g105","r2calc-r2Proj vs r2",50,50.,250.,50,-1.,4.);

   Int_t nentries = align_vars2->GetEntries();

   Float_t         phi2Octant;
   Float_t         dxdz1;
   Float_t         dydz1;
   Float_t         dxdz2;
   Float_t         dydz2;
   Float_t         dxdz3;
   Float_t         dydz3;
   Float_t         muidlpl;

   Float_t rslope12;
   Float_t rintercept12;
   Float_t rvtx12;
   Float_t rslope23;
   Float_t rslope13;
   Float_t rintercept23;
   Float_t rvtx23;
   Float_t x12proj;
   Float_t x13proj;
   Float_t y12proj;
   Float_t y13proj;
   Float_t x23proj;
   Float_t y23proj;
   Float_t x21proj;
   Float_t y21proj;
   Float_t x32proj;
   Float_t y32proj;
   Float_t r12proj;
   Float_t r13proj;
   Float_t phi23proj;
   Float_t phi12proj;
   Float_t r21proj;
   Float_t phi3proj;
   Float_t phi21proj;
   Float_t phi2line;
   Float_t rRes2Cross1;
   Float_t rRes2Cross2;
   Float_t rRes2Cross3;
   Float_t r2calc;
   Float_t octant;
   Float_t half;
   Int_t nbytes = 0;

// fit with gaussian plus flat background
   TF1 *myfit = new TF1("myfit","[0] + gaus(1)", 0, 10000);
   myfit->SetParName(0,"pedestal");
   myfit->SetParName(1,"amplitude");
   myfit->SetParName(2,"centroid");
   myfit->SetParName(3,"width");
   myfit->SetParameter(0,200.);
   myfit->SetParameter(1,1200.);
   myfit->SetParameter(2,0.);
   myfit->SetParameter(3,0.5);

// select arm
   Int_t iarm = 0;
   printf("enter arm number 0, or 1 ");
   scanf("%d",&iarm);

// loop over candidate tracks
   for (Int_t i=0; i<nentries;i++) {
      nbytes += align_vars2->GetEntry(i);
      octant = oct2;
      half = half2;
      if (trk_arm==iarm) {
// fill raw track histograms
/*      g1->Fill(r1);
      g2->Fill(r2);
      g3->Fill(r3);
      g4->Fill(-57.3*theta1);
      g5->Fill(-57.3*theta2);
      g6->Fill(-57.3*theta3); */

// compute phi at each station, use atan2!
      phi1 = atan2(x1,y1);
      phi2 = atan2(x2,y2);
      phi3 = atan2(x3,y3);
/*      g7->Fill(57.3*phi1);
      g8->Fill(57.3*phi2);
      g9->Fill(57.3*phi3);
      g10->Fill(-57.3*theta1,-57.3*theta2);
      g11->Fill(-57.3*theta2,-57.3*theta3);
      g12->Fill(57.3*phi1,57.3*phi2);
      g13->Fill(57.3*phi2,57.3*phi3);
      g14->Fill(57.3*theta1-57.3*theta2);
      g15->Fill(57.3*theta2-57.3*theta3);
      g16->Fill(-57.3*phi1+57.3*phi2);
      g17->Fill(-57.3*phi2+57.3*phi3); */

// compute radial slopes and intercepts for tracks using station 1-2 or 2-3
      rslope12 = (r2-r1)/(z2-z1);
      rintercept12 = r1 - (rslope12*z1);
      rvtx12 = 0.;
      if (rslope12!=0.) rvtx12 = -rintercept12/rslope12;
      g28->Fill(rslope12);
      g26->Fill(rvtx12);
      rslope23 = (r3-r2)/(z3-z2);
      rintercept23 = r2 - (rslope23*z2);
      rvtx23 = 0.;
      if (rslope23!=0.) rvtx23 = -rintercept23/rslope23;
      rslope13 = (r3-r1)/(z3-z1);
      g29->Fill(rslope23);
      g27->Fill(rvtx23);
    if (fabs(rvtx23)<2000.) {
      g30->Fill(rslope12,rslope23); 
      g31->Fill(rslope23-rslope12);

// fill stub slope histos
      /*      g32->Fill(dxdz1);
      g33->Fill(dydz1);
      g34->Fill(dxdz2);
      g35->Fill(dydz2);
      g36->Fill(dxdz3);
      g37->Fill(dydz3); */
      /*      g38->Fill(dxdz1,dxdz2);
      g39->Fill(dydz1,dydz2);
      g40->Fill(dxdz2,dxdz3);
      g41->Fill(dydz2,dydz3);
      g42->Fill(dxdz1-dxdz2);
      g43->Fill(dydz1-dydz2);
      g44->Fill(dxdz2-dxdz3);
      g45->Fill(dydz2-dydz3); */

// COMPUTE STUB PROJECTIONS in x,y from stub slopes
// e.g. x12proj is station 1 stub projected in x to station 2
      /*      x12proj = x1 + dxdz1*(z2-z1);
      y12proj = y1 + dydz1*(z2-z1);
      x13proj = x1 + dxdz1*(z3-z1);
      y13proj = y1 + dydz1*(z3-z1);
      x23proj = x2 + dxdz2*(z3-z2);
      y23proj = y2 + dydz2*(z3-z2);
      x21proj = x2 - dxdz2*(z2-z1);
      y21proj = y2 - dydz2*(z2-z1);
      x32proj = x3 - dxdz3*(z3-z2);
      y32proj = y3 - dydz3*(z3-z2);
      r12proj = sqrt(x12proj**2+y12proj**2)-0.3;
      r13proj = sqrt(x13proj**2+y13proj**2)-0.3;
      phi23proj = atan2(x23proj,y23proj);
      phi12proj = atan2(x12proj,y12proj);
      r21proj = sqrt(x23proj**2+y23proj**2);
      phi21proj = atan2(x21proj,y21proj);
      // phi3proj = phi2 + ((phi2-phi1)/(z2-z1))*(z3-z2);
      x23proj = x2 + ((x2-x1)/(z2-z1))*(z3-z2);
      y23proj = y2 + ((y2-y1)/(z2-z1))*(z3-z2);
      phi3proj = atan2(x23proj,y23proj); */

// fill stub projection histograms      
      if (fabs(dxdz1-dxdz2)<0.9 && fabs(dxdz1-dxdz2)<0.9) {
	/*        g46->Fill(x2-x12proj);
        g47->Fill(y2-y12proj);
        g50->Fill(x1-x21proj);
        g51->Fill(y1-y21proj); */
/*        g54->Fill(octant,x2-x12proj);
        g55->Fill(octant,y2-y12proj);
        g56->Fill(octant,x1-x21proj);
        g57->Fill(octant,y1-y21proj);
        g58->Fill(octant,r2-r12proj);
        g59->Fill(octant,phi2-phi12proj);
        g60->Fill(octant,r2*(phi2-phi12proj)); */
	g88->Fill(phi2-phi1,r2*(phi2-phi12proj));
	/*        g61->Fill(octant,r3-r21proj);
        g62->Fill(octant,phi1-phi21proj);
        g63->Fill(octant,r1*(phi1-phi21proj));
//        g73->Fill(octant,r3-r13proj); */

      }
      if (fabs(dxdz2-dxdz3)<0.9 && fabs(dxdz2-dxdz3)<0.9) {
	/*        g48->Fill(x3-x23proj);
        g49->Fill(y3-y23proj);
        g52->Fill(x2-x32proj);
        g53->Fill(y2-y32proj); */
      }

// now fill histograms from fitted track resduals, (not stub projections)
     phi2Proj = atan2(x2Proj,y2Proj);
     g64->Fill(octant,r2*(phi2-phi2Proj));
     g74->Fill(phi3-phi1,r2*(phi2-phi2Proj));
     g83->Fill(rslope13,phi3-phi1);

// makes some cuts to clean up tracks
     if (fabs(phi2-phi2Proj)<0.0025) {
// trial alignment fixes
       //       phi2Proj = phi2Proj - (((phi3-phi1)/(z3-z1))*-0.1);
       //       if (half==1) phi2Proj = phi2Proj + 0.00033;
       //       if (half==0) phi2Proj = phi2Proj - 0.00020;
       //phi12proj = phi12proj - (((phi3-phi1)/(z3-z1))*-0.5);
       //phi23proj = phi23proj - (((phi3-phi1)/(z3-z1))*0.5);

       if (fabs(r2*(phi2-phi12proj))<3.) g89->Fill(phi3-phi1,r2*(phi2-phi12proj));
       if (fabs(r3*(phi3-phi23proj))<3.) g97->Fill(phi3-phi1,r3*(phi3-phi23proj));
       g77->Fill(phi2,phi2-phi2Proj);
       g78->Fill(phi3-phi1,r2*(phi2-phi2Proj));
       if (half==0) g84->Fill(phi3-phi1,phi2-phi2Proj);
       if (half==1) g85->Fill(phi3-phi1,phi2-phi2Proj);
       g75->Fill(phi3-phi1,phi2-phi2Proj);
       if (half==0) g103->Fill(octant,r3*(phi3-phi3proj));
       if (half==1) g104->Fill(octant,r3*(phi3-phi3proj));
     }

     if (fabs(r2-r2Proj)<5.) {
        g76->Fill(r2,r2-r2Proj);
        r2calc = r1 + (r3-r1)*(z2-z1)/(z3-z1);
	//        r2calc = r1 + sqrt( ((x3-x1)*(z2-z1)/(z3-z1))**2 + ((y3-y1)*(z2-z1)/(z3-z1))**2 ); 
        g105->Fill(r2,r2calc-r2Proj);
// trial alignment fixes
	//r2Proj=r2Proj-(rslope13*-0.9)-0.3;
	//r2Proj=r2Proj-(rslope13*+0.1)-0.3;
        //r2Proj = r2Proj 
        //r2Proj= r1+((r3-r1)*(z2-z1)/(z3-z1))-(rslope13*-0.)-0.6; 
        //r2Proj= r1+(((r3-1.3)-r1)*(z2-z1)/(z3-z1))-(rslope13*-0.);
        //r2Proj = r2Proj - 0.1;
        //if (octant==0 && half==0) r2Proj = r2Proj - 0.7;
	//if (octant==1 && half==1) r2Proj = r2Proj - 0.2;

        g79->Fill(r2,r2-r2Proj);
        if (half==0) g101->Fill(octant,r2-r2Proj);
        if (half==1) g102->Fill(octant,r2-r2Proj);

	//        rRes2Cross1 = sqrt(xRes2Cross1**2+yRes2Cross1**2);
	//        rRes2Cross2 = sqrt(xRes2Cross2**2+yRes2Cross2**2);
	//        rRes2Cross3 = sqrt(xRes2Cross3**2+yRes2Cross3**2);
	//        if (fabs(xRes2Cross1)<5.) g98->Fill(r2,xRes2Cross1);
	//        if (fabs(xRes2Cross2)<5.) g99->Fill(r2,xRes2Cross2);
	//        if (fabs(xRes2Cross3)<5.) g100->Fill(r2,xRes2Cross3);
        g93->Fill(r1,r2-r2Proj);
        g94->Fill(r3,r2-r2Proj);
        g82->Fill(rslope13,r2-r2Proj);        
     }

     if (fabs(phi2-phi2Proj)<0.0025) {
       g80->Fill(phi2,phi2-phi2Proj);
// compute phi angle inside an octant for the best octants
       if (fabs(phi2)<=0.393) phi2Octant=phi2;
       if (phi2>0.393&&phi2<=1.179) phi2Octant=phi2-0.864;
       if (phi2>1.179&&phi2<=1.965) phi2Octant=phi2-1.572;
       if (phi2>2.751) phi2Octant=phi2-3.142;
       if (phi2<-2.751) phi2Octant=3.142-phi2;
       g81->Fill(phi2Octant,r2*(phi2-phi2Proj));
       if (fabs(r2*(phi2-phi12proj))<3.) g95->Fill(phi2Octant,r2*(phi2-phi12proj));
       if (fabs(r3*(phi3-phi23proj))<3.) g96->Fill(phi2Octant,r3*(phi3-phi23proj));
       if (fabs(phi2Octant)>.2) g91->Fill(phi3-phi1,phi2-phi2Proj);
       if (fabs(r2-r2Proj)<5.) g86->Fill(phi2Octant,r2-r2Proj);
       g87->Fill(r2,r2*(phi2-phi2Proj)); 
       g90->Fill(r2,rslope13); 
       g92->Fill(r2,(-0.25-.0055*(r2-80.))-rslope13);        
     }

// half octant histograms
     if (half==0) {
       /*        g18->Fill(-57.3*phi1+57.3*phi2);
        g20->Fill(-57.3*phi2+57.3*phi3);
        g22->Fill(57.3*theta1-57.3*theta2);
        g24->Fill(57.3*theta2-57.3*theta3); */
       //        g65->Fill(octant,r1*(phi1-phi21proj));
        g67->Fill(octant,r3*(phi3-phi3proj));
        g69->Fill(octant,(phi3-phi3proj));
        g71->Fill(octant,r2*(phi2-phi2Proj));
     }
      if (half==1) {
	/*        g19->Fill(-57.3*phi1+57.3*phi2);
        g21->Fill(-57.3*phi2+57.3*phi3);
        g23->Fill(57.3*theta1-57.3*theta2);
        g25->Fill(57.3*theta2-57.3*theta3); */
	//        g66->Fill(octant,r1*(phi1-phi21proj));
        g68->Fill(octant,r3*(phi3-phi3proj));
        g70->Fill(octant,(phi3-phi3proj));
        g72->Fill(octant,r2*(phi2-phi2Proj));
      }
      }
      }
   }

// book projections + perform gaussian fits as needed
//g58->ProjectionY("g58_y"); 
//g61->ProjectionY("g61_y"); 
//g73->ProjectionY("g73_y"); 
//g65->ProjectionY("g65_y",1,8,"");
//g66->ProjectionY("g66_y",1,8,"");
/* g60->ProjectionY("g60_1",1,1,"");
g60->ProjectionY("g60_2",2,2,"");
g60->ProjectionY("g60_3",3,3,"");
g60->ProjectionY("g60_4",4,4,"");
g60->ProjectionY("g60_5",5,5,"");
g60->ProjectionY("g60_6",6,6,"");
g60->ProjectionY("g60_7",7,7,"");
g60->ProjectionY("g60_8",8,8,"");
g63->ProjectionY("g63_1",1,1,"");
g63->ProjectionY("g63_2",2,2,"");
g63->ProjectionY("g63_3",3,3,"");
g63->ProjectionY("g63_4",4,4,"");
g63->ProjectionY("g63_5",5,5,"");
g63->ProjectionY("g63_6",6,6,"");
g63->ProjectionY("g63_7",7,7,"");
g63->ProjectionY("g63_8",8,8,"");
g64->ProjectionY("g64_1",1,1,"");
g64->ProjectionY("g64_2",2,2,"");
g64->ProjectionY("g64_3",3,3,"");
g64->ProjectionY("g64_4",4,4,"");
g64->ProjectionY("g64_5",5,5,"");
g64->ProjectionY("g64_6",6,6,"");
g64->ProjectionY("g64_7",7,7,"");
g64->ProjectionY("g64_8",8,8,""); */
g71->ProjectionY("g71_1",1,1,"");
g71->ProjectionY("g71_2",2,2,"");
g71->ProjectionY("g71_3",3,3,"");
g71->ProjectionY("g71_4",4,4,"");
g71->ProjectionY("g71_5",5,5,"");
g71->ProjectionY("g71_6",6,6,"");
g71->ProjectionY("g71_7",7,7,"");
g71->ProjectionY("g71_8",8,8,"");
g72->ProjectionY("g72_1",1,1,"");
g72->ProjectionY("g72_2",2,2,"");
g72->ProjectionY("g72_3",3,3,"");
g72->ProjectionY("g72_4",4,4,"");
g72->ProjectionY("g72_5",5,5,"");
g72->ProjectionY("g72_6",6,6,"");
g72->ProjectionY("g72_7",7,7,"");
g72->ProjectionY("g72_8",8,8,"");
g71_1->Fit("myfit");
g71_2->Fit("myfit");
g71_3->Fit("myfit");
g71_4->Fit("myfit");
g71_5->Fit("myfit");
g71_6->Fit("myfit");
g71_7->Fit("myfit");
g71_8->Fit("myfit");
g72_1->Fit("myfit");
g72_2->Fit("myfit");
g72_3->Fit("myfit");
g72_4->Fit("myfit");
g72_5->Fit("myfit");
g72_6->Fit("myfit");
g72_7->Fit("myfit");
g72_8->Fit("myfit");
/* g67->ProjectionY("g67_1",1,1,"");
g67->ProjectionY("g67_2",2,2,"");
g67->ProjectionY("g67_3",3,3,"");
g67->ProjectionY("g67_4",4,4,"");
g67->ProjectionY("g67_5",5,5,"");
g67->ProjectionY("g67_6",6,6,"");
g67->ProjectionY("g67_7",7,7,"");
g67->ProjectionY("g67_8",8,8,"");
g68->ProjectionY("g68_1",1,1,"");
g68->ProjectionY("g68_2",2,2,"");
g68->ProjectionY("g68_3",3,3,"");
g68->ProjectionY("g68_4",4,4,"");
g68->ProjectionY("g68_5",5,5,"");
g68->ProjectionY("g68_6",6,6,"");
g68->ProjectionY("g68_7",7,7,"");
g68->ProjectionY("g68_8",8,8,"");
g69->ProjectionY("g69_1",1,1,"");
g69->ProjectionY("g69_2",2,2,"");
g69->ProjectionY("g69_3",3,3,"");
g69->ProjectionY("g69_4",4,4,"");
g69->ProjectionY("g69_5",5,5,"");
g69->ProjectionY("g69_6",6,6,"");
g69->ProjectionY("g69_7",7,7,"");
g69->ProjectionY("g69_8",8,8,"");
g70->ProjectionY("g70_1",1,1,"");
g70->ProjectionY("g70_2",2,2,"");
g70->ProjectionY("g70_3",3,3,"");
g70->ProjectionY("g70_4",4,4,"");
g70->ProjectionY("g70_5",5,5,"");
g70->ProjectionY("g70_6",6,6,"");
g70->ProjectionY("g70_7",7,7,"");
g70->ProjectionY("g70_8",8,8,"");
g60_1->Fit("gaus","","",-5.,5.);
g60_2->Fit("gaus","","",-5.,5.);
g60_3->Fit("gaus","","",-5.,5.);
g60_4->Fit("gaus","","",-5.,5.);
g60_5->Fit("gaus","","",-5.,5.);
g60_6->Fit("gaus","","",-5.,5.);
g60_7->Fit("gaus","","",-5.,5.);
g60_8->Fit("gaus","","",-5.,5.);
g63_1->Fit("gaus","","",-10.,10.);
g63_2->Fit("gaus","","",-10.,10.);
g63_3->Fit("gaus","","",-10.,10.);
g63_4->Fit("gaus","","",-10.,10.);
g63_5->Fit("gaus","","",-10.,10.);
g63_6->Fit("gaus","","",-10.,10.);
g63_7->Fit("gaus","","",-10.,10.);
g63_8->Fit("gaus","","",-10.,10.);
g64_1->Fit("gaus","","",-5.,5.);
g64_2->Fit("gaus","","",-5.,5.);
g64_3->Fit("gaus","","",-5.,5.);
g64_4->Fit("gaus","","",-5.,5.);
g64_5->Fit("gaus","","",-5.,5.);
g64_6->Fit("gaus","","",-5.,5.);
g64_7->Fit("gaus","","",-5.,5.);
g64_8->Fit("gaus","","",-5.,5.); */

g101->ProjectionY("g101_1",1,1,"");
g101->ProjectionY("g101_2",2,2,"");
g101->ProjectionY("g101_3",3,3,"");
g101->ProjectionY("g101_4",4,4,"");
g101->ProjectionY("g101_5",5,5,"");
g101->ProjectionY("g101_6",6,6,"");
g101->ProjectionY("g101_7",7,7,"");
g101->ProjectionY("g101_8",8,8,"");
g102->ProjectionY("g102_1",1,1,"");
g102->ProjectionY("g102_2",2,2,"");
g102->ProjectionY("g102_3",3,3,"");
g102->ProjectionY("g102_4",4,4,"");
g102->ProjectionY("g102_5",5,5,"");
g102->ProjectionY("g102_6",6,6,"");
g102->ProjectionY("g102_7",7,7,"");
g102->ProjectionY("g102_8",8,8,"");
g101_1->Fit("myfit");
g101_2->Fit("myfit");
g101_3->Fit("myfit");
g101_4->Fit("myfit");
g101_5->Fit("myfit");
g101_6->Fit("myfit");
g101_7->Fit("myfit");
g101_8->Fit("myfit");
g102_1->Fit("myfit");
g102_2->Fit("myfit");
g102_3->Fit("myfit");
g102_4->Fit("myfit");
g102_5->Fit("myfit");
g102_6->Fit("myfit");
g102_7->Fit("myfit");
g102_8->Fit("myfit");

/*g103->ProjectionY("g103_1",1,1,"");
g103->ProjectionY("g103_2",2,2,"");
g103->ProjectionY("g103_3",3,3,"");
g103->ProjectionY("g103_4",4,4,"");
g103->ProjectionY("g103_5",5,5,"");
g103->ProjectionY("g103_6",6,6,"");
g103->ProjectionY("g103_7",7,7,"");
g103->ProjectionY("g103_8",8,8,"");
g104->ProjectionY("g104_1",1,1,"");
g104->ProjectionY("g104_2",2,2,"");
g104->ProjectionY("g104_3",3,3,"");
g104->ProjectionY("g104_4",4,4,"");
g104->ProjectionY("g104_5",5,5,"");
g104->ProjectionY("g104_6",6,6,"");
g104->ProjectionY("g104_7",7,7,"");
g104->ProjectionY("g104_8",8,8,"");*/
}




