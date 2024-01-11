#include "PHDchHistogrammer.hh"
#include "DchTrack.h"
#include "DchTrackCandidate.hh"
#include "DchMcRecoTrack.hh"
#include "DchRawTable.hh"
#include "DchHitLineTable.hh"
#include "DchRawInfo.hh"
#include "DchTrackInfo.hh"

#include <PHGeometry.h>
#include <dBbcOutWrapper.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHObject.h>
#include <PHTypedNodeIterator.h>

#include <TCanvas.h>
#include <TH1.h>
#include <TH2.h>
#include <TFile.h>
#include <TNtuple.h>

#include <iostream>
#include <sstream>

using namespace std;
using namespace PHGeometry;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<DchTrack> DchTrackNode_t;

template <class T> T
sqr(const T& x)
{
  return x * x;
}

PHDchHistogrammer::PHDchHistogrammer():
  flagInit(0),
  trackCanvas(NULL),
  EventRunNumber(0),
  globalEvent(0)
{}

int PHDchHistogrammer::initializeFile(int EventRunNumber)
{
  if (!flagInit)
    {
      ostringstream fname;
      ostringstream title;
      for (int i = 0;i < 100;i++)
        {
	  fname.str("");
	  title.str("");
          if (EventRunNumber > 0)
            {
	      fname << "hemmickE" << i << EventRunNumber << ".root";
	      title << getCVSVersion() << fname.str();
            }

          Datafile = new TFile(fname.str().c_str(), "NEW", title.str().c_str());
          if (Datafile->IsOpen())
            goto Exit;
          delete Datafile;
        }
    Exit:
      initializeDefault();
      flagInit = 1;
    }
  return 0;
}

int PHDchHistogrammer::saveToFile(int option)
{
  Datafile->Write();
  return 0;
}

int PHDchHistogrammer::initializeDefault()
{

  //now only three set of Histograms...
  NumberOfTimeBins = 768;
  TimeBinSize = 0.822;
  TimeBinSize = 0.78; //for the time beeing as we have different clock when taking cosmics

  float ResidualDisplayMin = 0;
  float ResidualDisplayMax = 10;

  TimeDistribX1 = new TH1F("TimeDistribX1", "dN/dt in X1 planes", NumberOfTimeBins, -0.5, 767.5);
  TimeDistribX1->SetFillColor(14);
  TimeDistribX1->SetXTitle("Drift time in bins");
  TimeDistribX1->SetYTitle("Number of entries");
  TimeDistribX1->GetXaxis()->SetTitleOffset(1.1);
  TimeDistribX1->GetYaxis()->SetTitleOffset(1.1);
  TimeDistribX1->GetXaxis()->SetLabelSize(0.035);
  TimeDistribX1->GetYaxis()->SetLabelSize(0.035);

  TimeDistribX2 = new TH1F("TimeDistribX2", "dN/dt in X2 planes", NumberOfTimeBins, -0.5, 767.5);
  TimeDistribX2->SetFillColor(42);
  TimeDistribX2->SetXTitle("Drift time in bins");
  TimeDistribX2->SetYTitle("Number of entries");
  TimeDistribX2->GetXaxis()->SetTitleOffset(1.1);
  TimeDistribX2->GetYaxis()->SetTitleOffset(1.1);
  TimeDistribX2->GetXaxis()->SetLabelSize(0.035);
  TimeDistribX2->GetYaxis()->SetLabelSize(0.035);

  TimeDistribU1 = new TH1F("TimeDistribU1", "dN/dt in U1 planes", NumberOfTimeBins, -0.5, 767.5);
  TimeDistribU1->SetFillColor(14);
  TimeDistribU1->SetXTitle("Drift time in bins");
  TimeDistribU1->SetYTitle("Number of entries");
  TimeDistribU1->GetXaxis()->SetTitleOffset(1.1);
  TimeDistribU1->GetYaxis()->SetTitleOffset(1.1);
  TimeDistribU1->GetXaxis()->SetLabelSize(0.035);
  TimeDistribU1->GetYaxis()->SetLabelSize(0.035);

  TimeDistribU2 = new TH1F("TimeDistribU2", "dN/dt in U2 planes", NumberOfTimeBins, -0.5, 767.5);
  TimeDistribU2->SetFillColor(42);
  TimeDistribU2->SetXTitle("Drift time in bins");
  TimeDistribU2->SetYTitle("Number of entries");
  TimeDistribU2->GetXaxis()->SetTitleOffset(1.1);
  TimeDistribU2->GetYaxis()->SetTitleOffset(1.1);
  TimeDistribU2->GetXaxis()->SetLabelSize(0.035);
  TimeDistribU2->GetYaxis()->SetLabelSize(0.035);

  TimeDistribV1 = new TH1F("TimeDistribV1", "dN/dt in V1 planes", NumberOfTimeBins, -0.5, 767.5);
  TimeDistribV1->SetFillColor(14);
  TimeDistribV1->SetXTitle("Drift time in bins");
  TimeDistribV1->SetYTitle("Number of entries");
  TimeDistribV1->GetXaxis()->SetTitleOffset(1.1);
  TimeDistribV1->GetYaxis()->SetTitleOffset(1.1);
  TimeDistribV1->GetXaxis()->SetLabelSize(0.035);
  TimeDistribV1->GetYaxis()->SetLabelSize(0.035);

  TimeDistribV2 = new TH1F("TimeDistribV2", "dN/dt in V2 planes", NumberOfTimeBins, -0.5, 767.5);
  TimeDistribV2->SetFillColor(42);
  TimeDistribV2->SetXTitle("Drift time in bins");
  TimeDistribV2->SetYTitle("Number of entries");
  TimeDistribV2->GetXaxis()->SetTitleOffset(1.1);
  TimeDistribV2->GetYaxis()->SetTitleOffset(1.1);
  TimeDistribV2->GetXaxis()->SetLabelSize(0.035);
  TimeDistribV2->GetYaxis()->SetLabelSize(0.035);
  TimeDistribX1AUX = new TH1F("TimeDistribX1AUX", "Auxiliary dN/dt in X1", NumberOfTimeBins, -0.5, 767.5);
  TimeDistribX2AUX = new TH1F("TimeDistribX2AUX", "Auxiliary dN/dt in X2", NumberOfTimeBins, -0.5, 767.5);
  TimeDistribU1AUX = new TH1F("TimeDistribU1AUX", "Auxiliary dN/dt in U1", NumberOfTimeBins, -0.5, 767.5);
  TimeDistribU2AUX = new TH1F("TimeDistribU2AUX", "Auxiliary dN/dt in U2", NumberOfTimeBins, -0.5, 767.5);
  TimeDistribV1AUX = new TH1F("TimeDistribV1AUX", "Auxiliary dN/dt in V1", NumberOfTimeBins, -0.5, 767.5);
  TimeDistribV2AUX = new TH1F("TimeDistribV2AUX", "Auxiliary dN/dt in V2", NumberOfTimeBins, -0.5, 767.5);

  int NResidualDisplayBins = 200;
  residualX1 = new TH1F("residualX1", "Residuals of X1 wires", NResidualDisplayBins, ResidualDisplayMin, ResidualDisplayMax);
  residualX2 = new TH1F("residualX2", "Residuals of X2 wires", NResidualDisplayBins, ResidualDisplayMin, ResidualDisplayMax);
  residualU1 = new TH1F("residualU1", "Residuals of U1 wires", NResidualDisplayBins, ResidualDisplayMin, ResidualDisplayMax);
  residualU2 = new TH1F("residualU2", "Residuals of U2 wires", NResidualDisplayBins, ResidualDisplayMin, ResidualDisplayMax);
  residualV1 = new TH1F("residualV1", "Residuals of V1 wires", NResidualDisplayBins, ResidualDisplayMin, ResidualDisplayMax);
  residualV2 = new TH1F("residualV2", "Residuals of V2 wires", NResidualDisplayBins, ResidualDisplayMin, ResidualDisplayMax);

  houghArray = new TH2F("houghArray", "houghArray", 6000 / 20, 0, 6000 / 20, 300, 0, 300);

  referenceTime = new TH1F("referenceTime", "ReferenceTime Distribution", 400, 299.5, 699.5);

  raw = new TNtuple("raw", "raw", "evt:nraw:global:arm:side:plane:cell:edge:time");
  hit = new TNtuple("hit", "hit", "evt:nhit:arm:side:plane:cell:distance:width:x:y:z:vx:vy:vz:time1:time2");
  track = new TNtuple("track", "track", "evt:ntr:arm:side:x:y:z:err:vx:vy:vz:alpha:phi:zed:beta:momentum:phi0:theta0:quality");

  perfectTrack = new TNtuple("perfectTrack", "perfectTrack", "evt:arm:side:x:y:z:err:vx:vy:vz");
  noise = new TNtuple("noise", "noise", "arm:side:plane:cell:ne0:ne1");
  timeResidual = new TNtuple("timeResidual", "timeResidual", "dt:p1:t1:p2:t2:p3:t3");
  trackInfo = new TNtuple("trackInfo", "trackInfo",
                          "evt:ntr:tid:arm:side:tx:ty:tz:tvx:tvy:tvz:alpha:beta:phi:zed:nhit:hid:plane:cell:dist:width:hx:hy:hz:hvx:hvy:hvz:res:blind:out:rid:edge:time");

  effi = new TNtuple("effi", "effi",
                     "ex1:eu1:ev1:ex2:eu2:ev2:tex1:teu1:tev1:tex2:teu2:tev2:bx1:bu1:bv1:bx2:bu2:bv2:tbx1:tbu1:tbv1:tbx2:tbu2:tbv2");
  hitInfo = new TNtuple("hitInfo", "hitInfo",
                        "evt:nhit:hid:arm:side:plane:cell:dist:width:hx:hy:hz:hvx:hvy:hvz:rid:edge:time:trid:tx:ty:tz:tvx:tvy:tvz:sx:sy:sz:nx:ny:nz");

  trackCandidate = new TNtuple("trackCandidate", "trackCandidate", "evt:tx1:tx2:tuv1:tuv2:cx1:cx2:cuv1:cuv2:talpha:tphi:tbeta:tzed:txh:tuvh:calpha:cphi:cbeta:czed:cxh:cuvh");
  evaluate = new TNtuple("evaluate", "evaluate", "gid:rid:dalpha:dbeta:dphi:dzed:e:g:f1:f2");

  evaluation = new TNtuple("evaluation", "evaluation", "eventID:eventxvtx:eventyvtx:eventzvtx:perfID:perfQual:momentumG:theta:phi:theta0G:phi0G:alphaG:betaG:zedG:generation:particleID:parentID:primaryID:rvtx:zvtx:recoID:recoQual:momentumR:theta0R:phi0R:xhits:uvhits:mulcontrib:xmulcontrib:uvmulcontrib:mainID:xmainID:uvmainID:ambig:xambig:uvambig:purity:xpurity:uvpurity:dalpha:dbeta:dphi:dzed:ddist:sumfound:solution:perfDvertex:recoDvertex:chi2:numHitsFit:dalphaMin:dphiMin:idGeantTrack:avDist");

  completeEvaluation = new TNtuple("completeEvaluation", "completeEvaluation", "eventID:eventxvtx:eventyvtx:eventzvtx:perfID:perfQual:momentumG:theta:phi:theta0G:phi0G:alphaG:betaG:zedG:generation:particleID:parentID:primaryID:rvtx:zvtx:phivtx:recoID:recoQual:momentumR:theta0R:phi0R:xhits:uvhits:mulcontrib:xmulcontrib:uvmulcontrib:mainID:xmainID:uvmainID:ambig:xambig:uvambig:purity:xpurity:uvpurity:dalpha:dbeta:dphi:dzed:ddist:sumfound:solution:perfDvertex:recoDvertex:chi2:numHitsFit:dalphaMin:dphiMin:avDist:gid:pc1recoid:pc2recoid:pc3recoid:pc1mcid:pc2mcid:pc3mcid:xpc1r:ypc1r:zpc1r:xpc1m:ypc1m:zpc1m:xpc2r:ypc2r:zpc2r:xpc2m:ypc2m:zpc2m:xpc3r:ypc3r:zpc3r:xpc3m:ypc3m:zpc3m:xpc1pro:ypc1pro:zpc1pro:xpc2pro:ypc2pro:zpc2pro:xpc3pro:ypc3pro:zpc3pro:pc13vtxm:pc13vtxr:bbcvtx:bbct0:tofrecoid:tofmcid:xtofr:ytofr:ztofr:xtofm:ytofm:ztofm:xtofpro:ytofpro:ztofpro:tofpath:toftofr:toftofm:elosstofr:elosstofm:pidtofr:pidtofm:emcrecoid:xemcreco:yemcreco:zemcreco:emcswkey:emcmease:emcecore:emcecorr:emcecent:emctof:emctofcorr:emctofmin:emcprobphot:twrhit:emcchi2:emcpartesum0:emcpartesum1:emcpartesum2:emcpartesum3:emcanctrkno0:emcanctrkno1:emcanctrkno2:emcanctwrhit0:emcanctwrhit1:emcanctwrhit2:emcancpid0:emcancpid1:emcancpid2:emcancedep0:emcancedep1:emcancedep2:emcancptot0:emcancptot1:emcancptot2:xemcproj:yemcproj:zemcproj:pathlemc:emcmcid:xemcmc:yemcmc:zemcmc:emcmcefrac:emcmcecore:emcmcmease:emcmctof:crkacc:crknpmt0:crknpmt1:crknpmt3:crknpe0:crknpe1:crknpe3:crkchi2:crkdisp:crkpath:ntrkG:ntrkR:sigpc1:sigpc1p:sigpc1z:delpc1p:delpc1z:sigpc2:sigpc2p:sigpc2z:delpc2p:delpc2z:sigpc3:sigpc3p:sigpc3z:delpc3p:delpc3z:sigtof:sigtofp:sigtofz:deltofp:deltofz:sigemc:sigemcp:sigemcz:delemcp:delemcz");

  aligment = new TNtuple("aligment", "aligment", "zvtx:multi:id:sector:rpc1:ppc1:zpc1:arm:side:nx1:nx2:gphi1:gphi2:ga1:ga2:oe1:oe2:md1:md2:i1:i2:s1:s2:t1_0:c1_0:wd1_0:d1_0:p1_0:t1_1:c1_1:wd1_1:d1_1:p1_1:t1_2:c1_2:wd1_2:d1_2:p1_2:t1_3:c1_3:wd1_3:d1_3:p1_3:t1_4:c1_4:wd1_4:d1_4:p1_4:t1_5:c1_5:wd1_5:d1_5:p1_5:t1_6:c1_6:wd1_6:d1_6:p1_6:t1_7:c1_7:wd1_7:d1_7:p1_7:t1_8:c1_8:wd1_8:d1_8:p1_8:t1_9:c1_9:wd1_9:d1_9:p1_9:t1_10:c1_10:wd1_10:d1_10:p1_10:t1_11:c1_11:wd1_11:d1_11:p1_11:t2_0:c2_0:wd2_0:d2_0:p2_0:t2_1:c2_1:wd2_1:d2_1:p2_1:t2_2:c2_2:wd2_2:d2_2:p2_2:t2_3:c2_3:wd2_3:d2_3:p2_3:t2_4:c2_4:wd2_4:d2_4:p2_4:t2_5:c2_5:wd2_5:d2_5:p2_5:t2_6:c2_6:wd2_6:d2_6:p2_6:t2_7:c2_7:wd2_7:d2_7:p2_7:t2_8:c2_8:wd2_8:d2_8:p2_8:t2_9:c2_9:wd2_9:d2_9:p2_9:t2_10:c2_10:wd2_10:d2_10:p2_10:t2_11:c2_11:wd2_11:d2_11:p2_11:t0:vd:k1:k2");

  embedevaluation = new TNtuple("embedevaluation", "embedevaluation", "eventId:type:xhits:uvhits:mulcollector:xmulcollector:uvcollector:purity:xpurity:uvpurity:dalpha:dbeta:dphi:dzed:ddist");

  residual = new TNtuple("residual", "residual", "tid:arm:res:twdist:plane:cc:cw:pcc:pcw:t0:vd:discontinuity:alpha:phi");

  vtxBbcCorrelation = new TNtuple("vtxbbc", "vtxbbc", "bbct0:bbcvtx:zdcvtx:vtxz");
  t0s = new TNtuple("t0s", "t0s", "bbct0:zdct0:zdct1:zdct2");

  stereoWires = new TNtuple("stereoWires", "stereoWires",
                            "ncX:ncUV:nX:nUV:nx1:nx2:nuv1:nuv2:npc1:alpha:phi:beta:zed:u11ID:u11IDm:u11Cell:u11Plane:u11Arm:u11Side:u11Dist:u11Width:u11Time1:u11R:u11PHI:u12ID:u12IDm:u12Cell:u12Plane:u12Arm:u12Side:u12Dist:u12Width:u12Time1:u12R:u12PHI:u13ID:u13IDm:u13Cell:u13Plane:u13Arm:u13Side:u13Dist:u13Width:u13Time1:u13R:u13PHI:u14ID:u14IDm:u14Cell:u14Plane:u14Arm:u14Side:u14Dist:u14Width:u14Time1:u14R:u14PHI:v11ID:v11IDm:v11Cell:v11Plane:v11Arm:v11Side:v11Dist:v11Width:v11Time1:v11R:v11PHI:v12ID:v12IDm:v12Cell:v12Plane:v12Arm:v12Side:v12Dist:v12Width:v12Time1:v12R:v12PHI:v13ID:v13IDm:v13Cell:v13Plane:v13Arm:v13Side:v13Dist:v13Width:v13Time1:v13R:v13PHI:v14ID:v14IDm:v14Cell:v14Plane:v14Arm:v14Side:v14Dist:v14Width:v14Time1:v14R:v14PHI:u21ID:u21IDm:u21Cell:u21Plane:u21Arm:u21Side:u21Dist:u21Width:u21Time1:u21R:u21PHI:u22ID:u22IDm:u22Cell:u22Plane:u22Arm:u22Side:u22Dist:u22Width:u22Time1:u22R:u22PHI:u23ID:u23IDm:u23Cell:u23Plane:u23Arm:u23Side:u23Dist:u23Width:u23Time1:u23R:u23PHI:u24ID:u24IDm:u24Cell:u24Plane:u24Arm:u24Side:u24Dist:u24Width:u24Time1:u24R:u24PHI:v21ID:v21IDm:v21Cell:v21Plane:v21Arm:v21Side:v21Dist:v21Width:v21Time1:v21R:v21PHI:v22ID:v22IDm:v22Cell:v22Plane:v22Arm:v22Side:v22Dist:v22Width:v22Time1:v22R:v22PHI:v23ID:v23IDm:v23Cell:v23Plane:v23Arm:v23Side:v23Dist:v23Width:v23Time1:v23R:v23PHI:v24ID:v24IDm:v24Cell:v24Plane:v24Arm:v24Side:v24Dist:v24Width:v24Time1:v24R:v24PHI");

  stereoZed = new TNtuple("stereoZed", "stereoZed",
                          "arm:side:alpha:phi:beta:zed:vix:viy:vz:pix:piy:pz:u11IDm:u11Dist:u11Width:u11Time1:u11ix:u11iy:u11dx:u12IDm:u12Dist:u12Width:u12Time1:u12ix:u12iy:u12dx:u13IDm:u13Dist:u13Width:u13Time1:u13ix:u13iy:u13dx:u14IDm:u14Dist:u14Width:u14Time1:u14ix:u14iy:u14dx:v11IDm:v11Dist:v11Width:v11Time1:v11ix:v11iy:v11dx:v12IDm:v12Dist:v12Width:v12Time1:v12ix:v12iy:v12dx:v13IDm:v13Dist:v13Width:v13Time1:v13ix:v13iy:v13dx:v14IDm:v14Dist:v14Width:v14Time1:v14ix:v14iy:v14dx:u21IDm:u21Dist:u21Width:u21Time1:u21ix:u21iy:u21dx:u22IDm:u22Dist:u22Width:u22Time1:u22ix:u22iy:u22dx:u23IDm:u23Dist:u23Width:u23Time1:u23ix:u23iy:u23dx:u24IDm:u24Dist:u24Width:u24Time1:u24ix:u24iy:u24dx:v21IDm:v21Dist:v21Width:v21Time1:v21ix:v21iy:v21dx:v22IDm:v22Dist:v22Width:v22Time1:v22ix:v22iy:v22dx:v23IDm:v23Dist:v23Width:v23Time1:v23ix:v23iy:v23dx:v24IDm:v24Dist:v24Width:v24Time1:v24ix:v24iy:v24dx");

  UValignment = new TNtuple("UValignment", "UValignment",
                            "arm:side:alpha:phi:beta:zed:zvtx:rpc1:ppc1:zpc1:u11cell:u11IDm:u11Dist:u11Width:u11Time1:u11phi1:u11phi2:u11FitPhi:u11FitZ:u12cell:u12IDm:u12Dist:u12Width:u12Time1:u12phi1:u12phi2:u12FitPhi:u12FitZ:u13cell:u13IDm:u13Dist:u13Width:u13Time1:u13phi1:u13phi2:u13FitPhi:u13FitZ:u14cell:u14IDm:u14Dist:u14Width:u14Time1:u14phi1:u14phi2:u14FitPhi:u14FitZ:v11cell:v11IDm:v11Dist:v11Width:v11Time1:v11phi1:v11phi2:v11FitPhi:v11FitZ:v12cell:v12IDm:v12Dist:v12Width:v12Time1:v12phi1:v12phi2:v12FitPhi:v12FitZ:v13cell:v13IDm:v13Dist:v13Width:v13Time1:v13phi1:v13phi2:v13FitPhi:v13FitZ:v14cell:v14IDm:v14Dist:v14Width:v14Time1:v14phi1:v14phi2:v14FitPhi:v14FitZ:u21cell:u21IDm:u21Dist:u21Width:u21Time1:u21phi1:u21phi2:u21FitPhi:u21FitZ:u22cell:u22IDm:u22Dist:u22Width:u22Time1:u22phi1:u22phi2:u22FitPhi:u22FitZ:u23cell:u23IDm:u23Dist:u23Width:u23Time1:u23phi1:u23phi2:u23FitPhi:u23FitZ:u24cell:u24IDm:u24Dist:u24Width:u24Time1:u24phi1:u24phi2:u24FitPhi:u24FitZ:v21cell:v21IDm:v21Dist:v21Width:v21Time1:v21phi1:v21phi2:v21FitPhi:v21FitZ:v22cell:v22IDm:v22Dist:v22Width:v22Time1:v22phi1:v22phi2:v22FitPhi:v22FitZ:v23cell:v23IDm:v23Dist:v23Width:v23Time1:v23phi1:v23phi2:v23FitPhi:v23FitZ:v24cell:v24IDm:v24Dist:v24Width:v24Time1:v24phi1:v24phi2:v24FitPhi:v24FitZ");


  t0bbc = new TH1F("t0bbc", "t0bbc", 100, -49.5, 50.5);
  t0zdc = new TH1F("t0zdc", "t0zdc", 100, -49.5, 50.5);

  // for the DriftVelocity calibration

  timeDistributionTrack[0] = new TH1F("timeDistributionTrackEAST", "timeDistributionTrackEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionTrackScaled[0] = new TH1F("timeDistributionTrackScaledEAST", "timeDistributionTrackScaledEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionTrack[1] = new TH1F("timeDistributionTrackWEST", "timeDistributionTrackWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionTrackScaled[1] = new TH1F("timeDistributionTrackScaledWEST", "timeDistributionTrackScaledWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);

  // full arm
  timeDistribution[0][0] = new TH1F("timeDistributionEAST", "timeDistributionEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionScaled[0][0] = new TH1F("timeDistributionScaledEAST", "timeDistributionScaledEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistribution[0][1] = new TH1F("timeDistributionWEST", "timeDistributionWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionScaled[0][1] = new TH1F("timeDistributionScaledWEST", "timeDistributionScaledWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);

  // X1 arm
  timeDistribution[1][0] = new TH1F("timeDistributionEASTx1", "timeDistributionEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionScaled[1][0] = new TH1F("timeDistributionScaledEASTx1", "timeDistributionScaledEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistribution[1][1] = new TH1F("timeDistributionWESTx1", "timeDistributionWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionScaled[1][1] = new TH1F("timeDistributionScaledWESTx1", "timeDistributionScaledWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  // U1 arm
  timeDistribution[2][0] = new TH1F("timeDistributionEASTu1", "timeDistributionEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionScaled[2][0] = new TH1F("timeDistributionScaledEASTu1", "timeDistributionScaledEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistribution[2][1] = new TH1F("timeDistributionWESTu1", "timeDistributionWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionScaled[2][1] = new TH1F("timeDistributionScaledWESTu1", "timeDistributionScaledWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  // V1 arm
  timeDistribution[3][0] = new TH1F("timeDistributionEASTv1", "timeDistributionEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionScaled[3][0] = new TH1F("timeDistributionScaledEASTv1", "timeDistributionScaledEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistribution[3][1] = new TH1F("timeDistributionWESTv1", "timeDistributionWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionScaled[3][1] = new TH1F("timeDistributionScaledWESTv1", "timeDistributionScaledWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);

  // X2 arm
  timeDistribution[4][0] = new TH1F("timeDistributionEASTx2", "timeDistributionEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionScaled[4][0] = new TH1F("timeDistributionScaledEASTx2", "timeDistributionScaledEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistribution[4][1] = new TH1F("timeDistributionWESTx2", "timeDistributionWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionScaled[4][1] = new TH1F("timeDistributionScaledWESTx2", "timeDistributionScaledWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  // U2 arm
  timeDistribution[5][0] = new TH1F("timeDistributionEASTu2", "timeDistributionEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionScaled[5][0] = new TH1F("timeDistributionScaledEASTu2", "timeDistributionScaledEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistribution[5][1] = new TH1F("timeDistributionWESTu2", "timeDistributionWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionScaled[5][1] = new TH1F("timeDistributionScaledWESTu2", "timeDistributionScaledWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  // V2 arm
  timeDistribution[6][0] = new TH1F("timeDistributionEASTv2", "timeDistributionEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionScaled[6][0] = new TH1F("timeDistributionScaledEASTv2", "timeDistributionScaledEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistribution[6][1] = new TH1F("timeDistributionWESTv2", "timeDistributionWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionScaled[6][1] = new TH1F("timeDistributionScaledWESTv2", "timeDistributionScaledWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);

  distDistributionScaled = new TH1F("distDistributionScaled", "distDistributionScaled", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);

  timeDistance[0] = new TH1F("timeDistanceEAST", "timeDistanceEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistanceTrack[0] = new TH1F("timeDistanceTrackEAST", "timeDistanceTrackEAST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);

  timeDistance[1] = new TH1F("timeDistanceWEST", "timeDistanceWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistanceTrack[1] = new TH1F("timeDistanceTrackWEST", "timeDistanceTrackWEST", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);

  // for RAW quality
  timeDistributionX1 = new TH1F("timeDistributionX1", "timeDistributionX1", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionX2 = new TH1F("timeDistributionX2", "timeDistributionX2", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionU1 = new TH1F("timeDistributionU1", "timeDistributionU1", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionU2 = new TH1F("timeDistributionU2", "timeDistributionU2", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionV1 = new TH1F("timeDistributionV1", "timeDistributionV1", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  timeDistributionV2 = new TH1F("timeDistributionV2", "timeDistributionV2", numberOfTimeBins, -0.5, (float)(numberOfTimeBins) - 0.5);
  char buff[256];
  for (int a = 0; a < 2; a++)
    {
      for (int s = 0;s < 2;s++)
        {
          sprintf(buff, "onoff%d%d", a, s);
          onoff[a][s] = new TH2F(buff, buff, numberOfCells, 0, (float)(numberOfCells), numberOfPlanes, 0, (float)(numberOfPlanes));
          for (int p = 0;p < 40;p++)
            {
              sprintf(buff, "arm_%d_side_%d_plane_%d", a, s, p);
              cellDistribution[a][s][p] = new TH1F(buff, buff, numberOfCells, -0.5, (float)(numberOfCells) - 0.5);
            }
        }
    }

  //easy efficiency vs. drift time
  supposedEasyEffiTime = new TH1F("supposedEasyEffiTime", "supposed efficiency drift time", 50, 0, 1000);
  trueEasyEffiTime = new TH1F("trueEasyEffiTime", "true efficiency drift time", 50, 0, 1000);
  easyTimeResidual = new TH1F("easyTimeResidual", "easy time residual", 50, 0. , 10.);

  //efficiency vs. drift distance
  supposedEffiAll = new TH1F("supposedEffiAll", "supposed efficiency drift distance for all wires", 20, 0, 2.5);
  supposedEffiX1 = new TH1F("supposedEffiX1", "supposed efficiency drift distance for X1 wires", 20, 0, 2.5);
  supposedEffiU1 = new TH1F("supposedEffiU1", "supposed efficiency drift distance for u1 wires", 20, 0, 2.5);
  supposedEffiV1 = new TH1F("supposedEffiV1", "supposed efficiency drift distance for V1  wires", 20, 0, 2.5);
  supposedEffiX2 = new TH1F("supposedEffiX2", "supposed efficiency drift distance for X2  wires", 20, 0, 2.5);
  supposedEffiU2 = new TH1F("supposedEffiU2", "supposed efficiency drift distance for U2 wires", 20, 0, 2.5);
  supposedEffiV2 = new TH1F("supposedEffiV2", "supposed efficiency drift distance for V2 wires", 20, 0, 2.5);

  trueEffiAll = new TH1F("trueEffiAll", "true efficiency drift distance for all wires", 20, 0, 2.5);
  trueEffiX1 = new TH1F("trueEffiX1", "true efficiency drift distance for X1 wires", 20, 0, 2.5);
  trueEffiU1 = new TH1F("trueEffiU1", "true efficiency drift distance for U1 wires", 20, 0, 2.5);
  trueEffiV1 = new TH1F("trueEffiV1", "true efficiency drift distance for V1 wires", 20, 0, 2.5);
  trueEffiX2 = new TH1F("trueEffiX2", "true efficiency drift distance for X2 wires", 20, 0, 2.5);
  trueEffiU2 = new TH1F("trueEffiU2", "true efficiency drift distance for U2 wires", 20, 0, 2.5);
  trueEffiV2 = new TH1F("trueEffiV2", "true efficiency drift distance for V2 wires", 20, 0, 2.5);

  residualX = new TH1F("residualX", "residual for X wires", 100, -0.1, 0.1);
  residualUV = new TH1F("residualUV", "residual for UV wires", 100, -0.1, 0.1);

  //efficiency vs. plane
  supposedEffiPlane = new TH1F("supposedEffi", "supposed efficiency vs. plane", 20, 0, 40);
  trueEffiPlane = new TH1F("trueEffi", "true efficiency vs. plane", 20, 0, 40);
  supposedEffiType = new TH1F("supposedEffiType", "supposed efficiency vs wire type", 6, 0., 6.);
  trueEffiType = new TH1F("trueEffiType", "true efficiency vs wire type", 6, 0., 6.);

  AllData = new TNtuple("AllData", "AllData", "arm:side:plane:keystone:board:bChan:edge:time:width:eventnumber:current_reference_time");
  DisplayData = new TNtuple("DisplayData", "DisplayData", "distance:supposeddistance:board:bChan:eventnumber:xhitbase:yhitbase:xtrackbase:ytrackbase:ztrackbase:xtrackdir:ytrackdir:ztrackdir");
  AllHits = new TNtuple("AllHits", "AllHits", "distance:board:bChan:width:EventCounter:xbase:ybase:xtrackbase:ytrackbase:ztrackbase:xtrackdir:ytrackdir:ztrackdir:residual:ActivationState");
  TrueEfficiencies = new TNtuple("TrueEfficiencies", "TrueEfficiencies", "bChan:intersectcell:ActivationState:Seeingflag:mindistance:is:TrackToWireDistance:xtrackbase:ytrackbase:ztrackbase:xtrackdir:ytrackdir:ztrackdir");
  AllTracks = new TNtuple("AllTracks", "AllTracks", "arm:side:id:xbase:ybase:zbase:xdir:ydir:zdir:TrackCounter,eventnumber");
  AllResiduals = new TNtuple("AllResiduals", "AllResiudals", "distance:plane:residual");
  AllEfficiencies = new TNtuple("AllEfficiencies", "AllEfficiencies", "NEfficientWiresX1:NEfficientWiresX2:NEfficientWiresU1:NEfficientWiresU2:NEfficientWiresV1:NEfficientWiresV2:NBackEfficientWiresX1:NBackEfficientWiresX2:NBackEfficientWiresU1:NBackEfficientWiresU2:NBackEfficientWiresV1:NBackEfficientWiresV2:Eventnumber");

  initializeVertexFit();

  return 0;
}

int PHDchHistogrammer::initializeVertexFit()
{

  nHist1 = new TH1F("nHist1", "number of tracks", 200, 0.0, 1000.0);
  xHist1 = new TH1F("xHist1", "x coordinate", 500, 0.0, 500.0);
  yHist1 = new TH1F("yHist1", "y coordinate", 500, 0.0, 500.0);
  zHist1 = new TH1F("zHist1", "z coordinate", 500, 0.0, 500.0);
  rHist1 = new TH1F("rHist1", "radius", 500, 0.0, 500.0);
  hHist1 = new TH1F("hHist1", "hits per track", 40, 0.0, 40.0);
  h1Hist1 = new TH1F("h1Hist1", "hits per track in geom. acceptance",
                     40, 0.0, 40.0);
  gdistHist1 = new TH1F("gdistHist1", "track - vtx", 100, 0.0, 500.0);
  ghHist1 = new TH1F("ghHist1", "hits per track", 40, 0.0, 40.0);
  bdistHist1 = new TH1F("bdistHist1", "track - vtx", 100, 0.0, 500.0);
  bhHist1 = new TH1F("bhHist1", "hits per track", 40, 0.0, 40.0);
  distrHist1 = new TH1F("distrHist1", "track - prim vtx radial", 100, 0.0, 150.0);
  distxHist1 = new TH1F("distxHist1", "track - prim vtx x", 201, -25.5, 25.5);
  distyHist1 = new TH1F("distyHist1", "track - prim vtx y", 201, -25.5, 25.5);
  distzHist1 = new TH1F("distzHist1", "track - prim vtx z", 401, -200.5, 200.5);
  accdistrHist1 = new TH1F("accdistrHist1", "track - vtx radial", 100, 0.0, 10.0);
  accdistxHist1 = new TH1F("accdistxHist1", "track - vtx x ", 101, -10.0, 10.0);
  accdistyHist1 = new TH1F("accdistyHist1", "track - vtx y ", 101, -10.0, 10.0);
  accdistzHist1 = new TH1F("accdistzHist1", "track - vtx z ", 401, -200.5, 200.5);
  accdistxyHist2 = new TH2F("accdistxyHist2", "track - vtx xy ",
                            101, -10.0, 10.0, 101, -10.0, 10.0);
  distxyHist2 = new TH2F("distxyHist2", "track - prim. vtx xy ",
                         201, -50.0, 50.0, 201, -50.0, 50.0);

  xvtxHist1 = new TH1F("xvtxHist1", "x_vtx", 210, -20.5, 20.5);
  yvtxHist1 = new TH1F("yvtxHist1", "y_vtx", 210, -20.5, 20.5);
  zvtxHist1 = new TH1F("zvtxHist1", "z_vtx", 210, -20.5, 20.5);
  dvtxHist1 = new TH1F("dvtxHist1", "svtx - vtx", 100, 40.0, 65.0);
  rvtxHist1 = new TH1F("rvtxHist1", "r_vtx", 100, 40.0, 50.0);
  xyvtxHist2 = new TH2F("xyvtxHist2", "xy_vtx", 100, -60.0, -20.0, 100, -20.0, 20.0);
  ngvtxHist1 = new TH1F("ngvtxHist1", "nb. of accepted tracks", 100, -0.0, 500.0);
  nbvtxHist1 = new TH1F("nbvtxHist1", "nb. of rejected tracks", 100, -0.0, 500.0);
  ngeogvtxHist1 = new TH1F("ngeogvtxHist1", "nb. of geo accepted tracks",
                           100, -0.0, 1000.0);
  ngeobvtxHist1 = new TH1F("ngeobvtxHist1", "nb. of geo rejected tracks",
                           100, -0.0, 1000.0);
  ntvtxHist1 = new TH1F("ntvtxHist1", "total nb. of tracks", 100, -0.0, 1000.0);
  nfinalHist1 = new TH1F("nfinalHist1", "nb. of tracks on rec. vertex",
                         100, -0.0, 200.0);

  dxHist1 = new TH1F("dxHist1", "delta x", 100, -5.0, 5.0);
  dyHist1 = new TH1F("dyHist1", "delta y", 100, -5.0, 5.0);
  dzHist1 = new TH1F("dzHist1", "delta z", 100, -5.0, 5.0);
  dxyHist2 = new TH2F("dxyHist2", "delta xy", 100, -5.0, 5.0, 100, -5.0, 5.0);

  return 0;
}

int PHDchHistogrammer::initCanvas()
{
  distributionX = new TCanvas("X_Wire_Drift_Times", "X_Wire_Drift_Times", 20, 20, 1200, 800);
  distributionU = new TCanvas("U_Wire_Drift_Times", "U_Wire_Drift_Times", 20, 20, 1200, 800);
  distributionV = new TCanvas("V_Wire_Drift_Times", "V_Wire_Drift_Times", 20, 20, 1200, 800);

  distributionX->Divide(2, 1);
  distributionU->Divide(2, 1);
  distributionV->Divide(2, 1);

  return 1;
}

int PHDchHistogrammer::drawXTHist(int flag)
{
  distributionX->cd(1);
  TimeDistribX1->Draw();
  distributionX->cd(2);
  TimeDistribX1->Draw();
  distributionX->Update();
  distributionX->Modified();

  distributionU->cd(1);
  TimeDistribU1->Draw();
  distributionU->cd(2);
  TimeDistribU2->Draw();
  distributionU->Update();
  distributionU->Modified();

  distributionV->cd(1);
  TimeDistribV1->Draw();
  distributionV->cd(2);
  TimeDistribV2->Draw();
  distributionV->Update();
  distributionV->Modified();

  return 0;
}

PHBoolean PHDchHistogrammer::fillEvaluate(float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8, float a9, float a10)
{
  evaluate->Fill(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
  return True;
}

PHBoolean PHDchHistogrammer::fillEvaluation(float ea[54])
{
  evaluation->Fill(ea);
  return True;
}

PHBoolean PHDchHistogrammer::fillCompleteEvaluation(DchMcRecoTrack* track)
{
  float array[200];
  int m = 0;
  array[m++] = track->get_eventID();
  array[m++] = track->get_eventxvtx();
  array[m++] = track->get_eventyvtx();
  array[m++] = track->get_eventzvtx();
  array[m++] = track->get_perfID();
  array[m++] = track->get_perfQual();
  array[m++] = track->get_momentumG();
  array[m++] = track->get_theta();
  array[m++] = track->get_phi();
  array[m++] = track->get_theta0G();
  array[m++] = track->get_phi0G();
  array[m++] = track->get_alphaG();
  array[m++] = track->get_betaG();
  array[m++] = track->get_zedG();
  array[m++] = track->get_generation();
  array[m++] = track->get_particleID();
  array[m++] = track->get_parentID();
  array[m++] = track->get_primaryID();
  array[m++] = track->get_rvtx();
  array[m++] = track->get_zvtx();
  array[m++] = track->get_phivtx();
  array[m++] = track->get_recoID();
  array[m++] = track->get_recoQual();
  array[m++] = track->get_momentumR();
  array[m++] = track->get_theta0R();
  array[m++] = track->get_phi0R();
  array[m++] = track->get_xhits();
  array[m++] = track->get_uvhits();
  array[m++] = track->get_mulcontrib();
  array[m++] = track->get_xmulcontrib();
  array[m++] = track->get_uvmulcontrib();
  array[m++] = track->get_mainID();
  array[m++] = track->get_xmainID();
  array[m++] = track->get_uvmainID();
  array[m++] = track->get_ambig();
  array[m++] = track->get_xambig();
  array[m++] = track->get_uvambig();
  array[m++] = track->get_purity();
  array[m++] = track->get_xpurity();
  array[m++] = track->get_uvpurity();
  array[m++] = track->get_dalpha();
  array[m++] = track->get_dbeta();
  array[m++] = track->get_dphi();
  array[m++] = track->get_dzed();
  array[m++] = track->get_ddist();
  array[m++] = track->get_sumfound();
  array[m++] = track->get_solution();
  array[m++] = track->get_perfDvertex();
  array[m++] = track->get_recoDvertex();
  array[m++] = track->get_chi2();
  array[m++] = track->get_numHitsFit();
  array[m++] = track->get_dalphaMin();
  array[m++] = track->get_dphiMin();
  array[m++] = track->get_avDist();
  array[m++] = track->get_idGeantTrack();
  array[m++] = track->get_pc1RecoId();
  array[m++] = track->get_pc2RecoId();
  array[m++] = track->get_pc3RecoId();
  array[m++] = track->get_pc1McId();
  array[m++] = track->get_pc2McId();
  array[m++] = track->get_pc3McId();
  array[m++] = track->get_xPc1Reco();
  array[m++] = track->get_yPc1Reco();
  array[m++] = track->get_zPc1Reco();
  array[m++] = track->get_xPc1Mc();
  array[m++] = track->get_yPc1Mc();
  array[m++] = track->get_zPc1Mc();
  array[m++] = track->get_xPc2Reco();
  array[m++] = track->get_yPc2Reco();
  array[m++] = track->get_zPc2Reco();
  array[m++] = track->get_xPc2Mc();
  array[m++] = track->get_yPc2Mc();
  array[m++] = track->get_zPc2Mc();
  array[m++] = track->get_xPc3Reco();
  array[m++] = track->get_yPc3Reco();
  array[m++] = track->get_zPc3Reco();
  array[m++] = track->get_xPc3Mc();
  array[m++] = track->get_yPc3Mc();
  array[m++] = track->get_zPc3Mc();
  array[m++] = track->get_xPc1Proj();
  array[m++] = track->get_yPc1Proj();
  array[m++] = track->get_zPc1Proj();
  array[m++] = track->get_xPc2Proj();
  array[m++] = track->get_yPc2Proj();
  array[m++] = track->get_zPc2Proj();
  array[m++] = track->get_xPc3Proj();
  array[m++] = track->get_yPc3Proj();
  array[m++] = track->get_zPc3Proj();
  array[m++] = track->get_pc13vtxm();
  array[m++] = track->get_pc13vtxr();
  array[m++] = track->get_bbcvtx();
  array[m++] = track->get_bbct0();
  array[m++] = track->get_tofRecoId();
  array[m++] = track->get_tofMcId();
  array[m++] = track->get_xTofReco();
  array[m++] = track->get_yTofReco();
  array[m++] = track->get_zTofReco();
  array[m++] = track->get_xTofMc();
  array[m++] = track->get_yTofMc();
  array[m++] = track->get_zTofMc();
  array[m++] = track->get_xTofProj();
  array[m++] = track->get_yTofProj();
  array[m++] = track->get_zTofProj();
  array[m++] = track->get_pathlTof();
  array[m++] = track->get_tofTofReco();
  array[m++] = track->get_tofTofMc();
  array[m++] = track->get_elossTofReco();
  array[m++] = track->get_elossTofMc();
  array[m++] = track->get_pidTofReco();
  array[m++] = track->get_pidTofMc();
  array[m++] = track->get_emcRecoId();
  array[m++] = track->get_xEmcReco();
  array[m++] = track->get_yEmcReco();
  array[m++] = track->get_zEmcReco();
  array[m++] = track->get_emcswkey();
  array[m++] = track->get_emcmease();
  array[m++] = track->get_emcecore();
  array[m++] = track->get_emcecorr();
  array[m++] = track->get_emcecent();
  array[m++] = track->get_emctof();
  array[m++] = track->get_emctofcorr();
  array[m++] = track->get_emctofmin();
  array[m++] = track->get_emcprobphot();
  array[m++] = track->get_twrhit();
  array[m++] = track->get_emcchi2();
  array[m++] = track->get_emcpartesum0();
  array[m++] = track->get_emcpartesum1();
  array[m++] = track->get_emcpartesum2();
  array[m++] = track->get_emcpartesum3();
  array[m++] = track->get_emcAnctrkno0();
  array[m++] = track->get_emcAnctrkno1();
  array[m++] = track->get_emcAnctrkno2();
  array[m++] = track->get_emcAnctwrhit0();
  array[m++] = track->get_emcAnctwrhit1();
  array[m++] = track->get_emcAnctwrhit2();
  array[m++] = track->get_emcAncpid0();
  array[m++] = track->get_emcAncpid1();
  array[m++] = track->get_emcAncpid2();
  array[m++] = track->get_emcAncedep0();
  array[m++] = track->get_emcAncedep1();
  array[m++] = track->get_emcAncedep2();
  array[m++] = track->get_emcAncptot0();
  array[m++] = track->get_emcAncptot1();
  array[m++] = track->get_emcAncptot2();
  array[m++] = track->get_xEmcProj();
  array[m++] = track->get_yEmcProj();
  array[m++] = track->get_zEmcProj();
  array[m++] = track->get_pathlEmc();
  array[m++] = track->get_emcMcId();
  array[m++] = track->get_xEmcMc();
  array[m++] = track->get_yEmcMc();
  array[m++] = track->get_zEmcMc();
  array[m++] = track->get_emcMcefrac();
  array[m++] = track->get_emcMcecore();
  array[m++] = track->get_emcMcmease();
  array[m++] = track->get_emcMctof();
  array[m++] = track->get_crkacc();
  array[m++] = track->get_crknpmt0();
  array[m++] = track->get_crknpmt1();
  array[m++] = track->get_crknpmt3();
  array[m++] = track->get_crknpe0();
  array[m++] = track->get_crknpe1();
  array[m++] = track->get_crknpe3();
  array[m++] = track->get_crkchi2();
  array[m++] = track->get_crkdisp();
  array[m++] = track->get_crkpath();
  array[m++] = track->ntrkG;
  array[m++] = track->ntrkR;
  array[m++] = track->sigpc1;
  array[m++] = track->sigpc1p;
  array[m++] = track->sigpc1z;
  array[m++] = track->delpc1p;
  array[m++] = track->delpc1z;
  array[m++] = track->sigpc2;
  array[m++] = track->sigpc2p;
  array[m++] = track->sigpc2z;
  array[m++] = track->delpc2p;
  array[m++] = track->delpc2z;
  array[m++] = track->sigpc3;
  array[m++] = track->sigpc3p;
  array[m++] = track->sigpc3z;
  array[m++] = track->delpc3p;
  array[m++] = track->delpc3z;
  array[m++] = track->sigtof;
  array[m++] = track->sigtofp;
  array[m++] = track->sigtofz;
  array[m++] = track->deltofp;
  array[m++] = track->deltofz;
  array[m++] = track->sigemc;
  array[m++] = track->sigemcp;
  array[m++] = track->sigemcz;
  array[m++] = track->delemcp;
  array[m++] = track->delemcz;
  completeEvaluation->Fill(array);
  return True;
}

PHBoolean PHDchHistogrammer::fillEmbedEvaluation(float ea[15])
{
  embedevaluation->Fill(ea);
  return True;
}

int PHDchHistogrammer::drawHoughArray()
{
  houghCanvas = new TCanvas("HoughDistribution", "HoughDistribution", 20, 20, 1200, 800);
  houghCanvas->cd();
  houghArray->Draw("CONT1");
  houghCanvas->Modified();
  houghCanvas->Update();
  return 1;
}
int PHDchHistogrammer::refresh()
{
  distributionX->Update();
  distributionX->Modified();
  distributionU->Update();
  distributionU->Modified();
  distributionV->Update();
  distributionV->Modified();
  houghCanvas->Modified();
  houghCanvas->Update();

  return 1;
}

PHBoolean PHDchHistogrammer::fillTimeDistanceCorrelation(float tdRaw0[numberOfTimeBins], float tdTrack0[numberOfTimeBins], float tdRaw1[numberOfTimeBins], float tdTrack1[numberOfTimeBins])

{
  for (int i = 0; i < numberOfTimeBins; i++)
    {
      timeDistance[0]->Fill(i, tdRaw0[i]);
      timeDistanceTrack[0]->Fill(i, tdTrack0[i]);
      timeDistance[1]->Fill(i, tdRaw1[i]);
      timeDistanceTrack[1]->Fill(i, tdTrack1[i]);
    }
  return True;

}

PHBoolean
PHDchHistogrammer::fillVertexBbcCorrelation(float bbct0, float bbcvtx, float zdcvtx, float vtx)
{
  vtxBbcCorrelation->Fill(bbct0, bbcvtx, zdcvtx, vtx);
  return True;
}

PHBoolean
PHDchHistogrammer::fillT0s(float bbcT0, float zdcT0, float zdcT1, float zdcT2)
{

  t0s->Fill(bbcT0, zdcT0, zdcT1, zdcT2);
  t0bbc->Fill(bbcT0);
  t0zdc->Fill(zdcT0);

  return True;
}

PHBoolean
PHDchHistogrammer::fillTracksAndCandidates(PHCompositeNode *TopNode)
{
  float array[21];
  PHNodeIterator nodeIter(TopNode);

  PHTypedNodeIterator <DchTrack> dchtrackiter(TopNode);
  DchTrackNode_t *DchTrackNode = dchtrackiter.find("DchTrack");
  if (DchTrackNode)
    {
      trackTable = DchTrackNode->getData();
    }
  else
    {
      cout << PHWHERE << "DchTrack Node not found" << endl;
      return False;
    }

  PHDataNode<PHPointerList<DchTrackCandidate> > *candiNode = (PHIODataNode<PHPointerList<DchTrackCandidate> >*)nodeIter.findFirst("PHDataNode", "DchTrackCandidate");
  PHPointerList<DchTrackCandidate>* candidates;
  candidates = candiNode->getData();

  PHDataNode<int> *eventNode = (PHDataNode<int>*)nodeIter.findFirst("PHDataNode", "DchEvent");
  int* event = eventNode->getData();
  globalEvent = *(event);
  int evt = *(event);

  int numberOfTracks = trackTable->get_DchNTrack();
  int numberOfCandidates = candidates->length();
  DchTrackCandidate* candi;
  for (int c = 0; c < numberOfCandidates; c++)
    {
      candi = (*candidates)[c];
      float alphac = candi->getLocalAlpha();
      float betac = candi->getLocalBeta();
      float zedc = candi->getLocalZed();
      float phic = candi->getLocalPhi();
      int cx1 = candi->x1->length();
      int cx2 = candi->x2->length();
      int cuv1 = candi->uv1->length();
      int cuv2 = candi->uv2->length();
      int cxh = candi->getXHough();
      int cuvh = candi->getUVHough();
      for (int i = 0; i < numberOfTracks; i++)
        {

          float alphat = trackTable->get_alpha(i);
          float betat = trackTable->get_beta(i);
          float zedt = trackTable->get_zed(i);
          float phit = trackTable->get_phi(i);
          int tx1 = 0;
          int tx2 = 0;
          int tuv1 = 0;
          int tuv2 = 0;
          for (int k = 0; k < numberOfPlanes; k++)
            {
              int id = trackTable->get_hits(i, k);
              if (id >= 0)
                {
                  if (k < 12)
                    {
                      tx1++;
                    }
                  else if (k < 20)
                    {
                      tuv1++;
                    }
                  else if (k < 32)
                    {
                      tx2++;
                    }
                  else
                    {
                      tuv2++;
                    }
                }
            }
          if (fabs(phic - phit) < 10 / ToDegree)
            {
              int m = 0;
              array[m++] = evt;
              array[m++] = tx1;
              array[m++] = tx2;
              array[m++] = tuv1;
              array[m++] = tuv2;
              array[m++] = cx1;
              array[m++] = cx2;
              array[m++] = cuv1;
              array[m++] = cuv2;
              array[m++] = alphat;
              array[m++] = phit;
              array[m++] = betat;
              array[m++] = zedt;
              array[m++] = 0;
              array[m++] = 0;
              array[m++] = alphac;
              array[m++] = phic;
              array[m++] = betac;
              array[m++] = zedc;
              array[m++] = cxh;
              array[m++] = cuvh;

              trackCandidate->Fill(array);
            }
        }
    }

  return True;
}

PHBoolean PHDchHistogrammer::fillStereoWires(PHCompositeNode *TopNode)
{
  float array[189];
  PHNodeIterator nodeIter(TopNode);

  PHDataNode<PHPointerList<DchTrackCandidate> > *candiNode = (PHIODataNode<PHPointerList<DchTrackCandidate> >*)nodeIter.findFirst("PHDataNode", "DchTrackCandidate");
  PHPointerList<DchTrackCandidate>* candidates;
  candidates = candiNode->getData();

  PHDataNode<int> *eventNode = (PHDataNode<int>*)nodeIter.findFirst("PHDataNode", "DchEvent");
  int* event = eventNode->getData();
  globalEvent = *(event);
  int numberOfCandidates = candidates->length();
  DchTrackCandidate* candi;
  for (int c = 0; c < numberOfCandidates; c++)
    {
      candi = (*candidates)[c];

      float alphac = candi->getLocalAlpha();
      float betac = candi->getLocalBeta();
      float zedc = candi->getLocalZed();
      float phic = candi->getLocalPhi();

      int ncX = candi->closestX->length();
      int ncUV = candi->closestUV->length();
      int nX = candi->X->length();
      int nUV = candi->UV->length();
      int nx1 = candi->x1->length();
      int nx2 = candi->x2->length();
      int nuv1 = candi->uv1->length();
      int nuv2 = candi->uv2->length();
      int npc1 = candi->pc1->length();
      int m = 0;
      array[m++] = ncX;
      array[m++] = ncUV;
      array[m++] = nX;
      array[m++] = nUV;
      array[m++] = nx1;
      array[m++] = nx2;
      array[m++] = nuv1;
      array[m++] = nuv2;
      array[m++] = npc1;
      array[m++] = alphac;
      array[m++] = phic;
      array[m++] = betac;
      array[m++] = zedc;

      //"Golden" tracks are those for which a number of conditions are true:
      // There are exactly 12 chosen X hits.
      // There are exactly 4 closest uv1 hits.
      // If and only if I find that the given track is golden
      // I will make a record in the NTUPLE.
      // TKH -- 10-23-2001
      if (nX > 10 && npc1 == 1)
        {
          DchHitLine * uv[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
          DchHitLine * line;
          for (int j = 0; j < ncUV; j++)
            {
              line = (*(candi->closestUV))[j];
              int plane = line->getPlane();
              if ((plane > 11) && (plane < 20))
                {  //Now doing UV1!!!
                  uv[plane - 12] = (*(candi->closestUV))[j];
                }
              if ((plane > 31) && (plane < 40))
                {  //Now doing UV2!!!
                  uv[plane - 24] = (*(candi->closestUV))[j];
                }
            }
          for (int j = 0; j < 16; j++)
            {
              if (uv[j])
                {
                  array[m++] = uv[j]->getId();
                  array[m++] = uv[j]->getIdmirror();
                  array[m++] = uv[j]->getCell();
                  array[m++] = uv[j]->getPlane();
                  array[m++] = uv[j]->getArm();
                  array[m++] = uv[j]->getSide();
                  array[m++] = uv[j]->getDistance();
                  array[m++] = uv[j]->getWidth();
                  array[m++] = uv[j]->getTime();
                  array[m++] = uv[j]->getLocalRadius();
                  array[m++] = uv[j]->getLocalPhi();
                }
              else
                {
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                }
            }
          stereoWires->Fill(array);
        }
    }
  return True;
}

PHBoolean
PHDchHistogrammer::fillStereoZed(PHCompositeNode *TopNode)
{
  float array[124];
  PHNodeIterator nodeIter(TopNode);
  float u11r = 214.85; //This is the ideal radius of the u11 wire.

  //get the candidates pointer list...
  PHDataNode<PHPointerList<DchTrackCandidate> > *candiNode =
    (PHIODataNode<PHPointerList<DchTrackCandidate> >*)nodeIter.findFirst("PHDataNode", "DchTrackCandidate");
  PHPointerList<DchTrackCandidate>* candidates;
  candidates = candiNode->getData();

  //get the BBC vertex...
  PHIODataNode<TObject> *BBCNode = (PHIODataNode<TObject>*)nodeIter.findFirst("PHIODataNode", "dBbcOut");
  if (!BBCNode)
    {
      PHMessage("PHDchHistogrammer::fillStereoZed", PHWarning, "dBbcOut table not found, can't fill stereoZed ");
      return False;
    }
  dBbcOutWrapper* bbcWrapper = (dBbcOutWrapper*)(BBCNode->getData());
  PHPoint vertex(0, 0, bbcWrapper->get_VertexPoint(0) );

  PHDataNode<int> *eventNode = (PHDataNode<int>*)nodeIter.findFirst("PHDataNode", "DchEvent");
  int* event = eventNode->getData();
  globalEvent = *(event);

  PHPoint globalVertexIntersection(0, 0, 0), vertexIntersection(0, 0, 0);
  int numberOfCandidates = candidates->length();
  DchTrackCandidate* candi;
  for (int c = 0; c < numberOfCandidates; c++)
    {
      candi = (*candidates)[c];

      int iarm = candi->getArm();
      int iside = candi->getSide();
      float alpha = candi->getLocalAlpha();
      float beta = candi->getLocalBeta();
      float zed = candi->getLocalZed();
      float phi = candi->getLocalPhi();

      int m = 0;
      array[m++] = iarm;
      array[m++] = iside;
      array[m++] = alpha;
      array[m++] = phi;
      array[m++] = beta;
      array[m++] = zed;

      PHPlane planeUV = candi->getGlobalPlane(UV1Wire);
      PHLine tmp(vertex, planeUV.getNormal());
      intersectionLinePlane(tmp, planeUV, globalVertexIntersection);
      vertexIntersection = candi->rotateAndTranslate(globalVertexIntersection);

      array[m++] = vertexIntersection.getX();
      array[m++] = vertexIntersection.getY();
      array[m++] = vertex.getZ();

      float vz = vertex.getZ();

      //"Golden" tracks are those for which a number of conditions are true:
      // There are exactly 12 chosen X hits.
      // There are exactly 4 closest uv1 hits.
      // If and only if I find that the given track is golden
      // I will make a record in the NTUPLE.
      // TKH -- 10-23-2001
      int nX = candi->X->length();
      int npc1 = candi->pc1->length();
      if (nX > 10 && npc1 == 1)
        {

          DchPc1Hit *phit = (*candi->pc1)[0];
          PHPoint inter = candi->rotateAndTranslate(*phit);
          array[m++] = inter.getX();
          array[m++] = inter.getY();
          array[m++] = phit->getZ();

          float pix = inter.getX();
          float pz = phit->getZ();
          PHCylPoint PadCyl(*phit);
          float pr = PadCyl.getR();

          DchHitLine * uv[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
          DchHitLine * line;
          for (unsigned int j = 0; j < candi->closestUV->length(); j++)
            {
              line = (*(candi->closestUV))[j];
              int plane = line->getPlane();
              if ((plane > 11) && (plane < 20))
                {  //Now doing UV1!!!
                  uv[plane - 12] = (*(candi->closestUV))[j];
                }
              if ((plane > 31) && (plane < 40))
                {  //Now doing UV2!!!
                  uv[plane - 24] = (*(candi->closestUV))[j];
                }
            }
          for (int j = 0; j < 16; j++)
            {
              if (uv[j])
                {
                  array[m++] = uv[j]->getIdmirror();
                  array[m++] = uv[j]->getDistance();
                  array[m++] = uv[j]->getWidth();
                  array[m++] = uv[j]->getTime();
                  array[m++] = uv[j]->getLocalIntersectionPoint().getX();
                  array[m++] = uv[j]->getLocalIntersectionPoint().getY();
                  float hix = uv[j]->getLocalIntersectionPoint().getX();
                  float hr = uv[j]->getLocalRadius();
                  float prediction = (pix - vz) / pr * hr + vz;   // The GOOD one...
                  // Parameterized error due to residual bend: TKH
                  // There is no a fundamental justification for the form.
                  prediction = prediction + (0.25 * pz * alpha * alpha * (pr - hr) / (pr - u11r));
                  array[m++] = hix - prediction;
                }
              else
                {
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                }
            }
          stereoZed->Fill(array);
        }
    }
  return True;
}

PHBoolean
PHDchHistogrammer::fillUValignment(PHCompositeNode *TopNode)
{
  int nX, npc1;
  float pr, pp, pz;
  unsigned int j;
  int plane;
  int iarm;
  int iside;
  float alpha;
  float beta;
  float zed;
  float phi;
  int m;
  int c;
  double t;
  double R;
  double z;
  double L;
  double f;
  double r;
  float array[154];
  PHNodeIterator nodeIter(TopNode);

  //get the candidates pointer list...
  PHDataNode<PHPointerList<DchTrackCandidate> > *candiNode =
    (PHIODataNode<PHPointerList<DchTrackCandidate> >*)nodeIter.findFirst("PHDataNode", "DchTrackCandidate");
  PHPointerList<DchTrackCandidate>* candidates;
  candidates = candiNode->getData();

  //get the BBC vertex...
  PHIODataNode<TObject> *BBCNode = (PHIODataNode<TObject>*)nodeIter.findFirst("PHIODataNode", "dBbcOut");
  if (!BBCNode)
    {
      PHMessage("PHDchHistogrammer::fillStereoZed", PHWarning, "dBbcOut table not found, can't fill stereoZed ");
      return False;
    }
  dBbcOutWrapper* bbcWrapper = (dBbcOutWrapper*)(BBCNode->getData());
  PHPoint vertex(0, 0, bbcWrapper->get_VertexPoint(0) );

  PHDataNode<int> *eventNode = (PHDataNode<int>*)nodeIter.findFirst("PHDataNode", "DchEvent");
  int* event = eventNode->getData();
  globalEvent = *(event);

  int numberOfCandidates = candidates->length();
  DchTrackCandidate* candi;
  for (c = 0; c < numberOfCandidates; c++)
    {
      candi = (*candidates)[c];
      PHLine theTrackLine(candi->getGlobalPoint(), candi->getGlobalVector());

      iarm = candi->getArm();
      iside = candi->getSide();
      alpha = candi->getGlobalAlpha();
      beta = candi->getGlobalBeta();
      zed = candi->getGlobalZed();
      phi = candi->getGlobalPhi();

      m = 0;
      array[m++] = iarm;
      array[m++] = iside;
      array[m++] = alpha;
      array[m++] = phi;
      array[m++] = beta;
      array[m++] = zed;
      array[m++] = vertex.getZ();

      // "Golden" tracks are those for which a number of conditions
      // are true: There are more than 9 chosen X hits.  There are
      // exactly 4 closest uv1 hits.  If and only if I find that the
      // given track is golden I will make a record in the NTUPLE.
      // TKH -- 10-28-2002
      nX = candi->X->length();
      npc1 = candi->pc1->length();
      if (nX > 10 && npc1 == 1)
        {
          DchPc1Hit *phit = (*candi->pc1)[0];
          PHCylPoint PadCyl(*phit);

          pr = PadCyl.getR();
          pp = PadCyl.getPhi();
          pz = PadCyl.getZ();
          array[m++] = pr;
          array[m++] = pp;
          array[m++] = pz;

          //  Assign an array of pointers to the hits on every plane.
          //  NULL pointers for planes that have no hits.
          DchHitLine * uv[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
          DchHitLine * line;
          for (j = 0; j < candi->closestUV->length(); j++)
            {
              line = (*(candi->closestUV))[j];
              plane = line->getPlane();
              if (plane > 11 && plane < 20)
                {  //Now doing UV1!!!
                  uv[plane - 12] = (*(candi->closestUV))[j];
                }
              if (plane > 31 && plane < 40)
                {  //Now doing UV2!!!
                  uv[plane - 24] = (*(candi->closestUV))[j];
                }
            }

          // Ummm...OK.  Now we need to loop through these hits to
          // fill the NTUPLE.  There are two "deduced" values: phi1
          // and phi2.  These are the phi coordinates of the hit at
          // its "endpoints".  Since the hit structure does not
          // actually define endoints, we will instead use the
          // intersection points of the hit at planes located at
          // Z=+100 and Z=-100 cm.  TKH 10-28-2002

          PHVector origin(0, 0, 0);
          PHVector origin1(0, 0, -100);
          PHVector origin2(0, 0, 100);
          PHVector zAxis(0, 0, 1);
          PHVector cylinderAxis(0, 0, 150);
          PHPlane plane1(origin1, zAxis);
          PHPlane plane2(origin2, zAxis);
          PHPoint point1, point2, predPoint1, predPoint2;
          PHCylPoint cylPoint1, cylPoint2, cylPredPoint, cylPredPoint1, cylPredPoint2;
          for (j = 0; j < 16; j++)
            {
              if (uv[j])
                {
                  array[m++] = uv[j]->getCell();
                  array[m++] = uv[j]->getIdmirror();
                  array[m++] = uv[j]->getDistance();
                  array[m++] = uv[j]->getWidth();
                  array[m++] = uv[j]->getTime();

                  //Now get the two intersection points of the hits at the reference plane:
                  intersectionLinePlane( *uv[j], plane1, point1);
                  intersectionLinePlane( *uv[j], plane2, point2);
                  cylPoint1 = point1;
                  cylPoint2 = point2;
                  array[m++] = cylPoint1.getPhi();
                  array[m++] = cylPoint2.getPhi();

                  // With a little bit of trigonometry we can
                  // determine the small correction to the radius at
                  // which a UV wire intersects a given z-plane.  It's
                  // only a matter of a few mm at most.
                  t = (cylPoint1.getPhi() - cylPoint2.getPhi()) / 2.0;
                  R = (cylPoint1.getR() + cylPoint2.getR()) / 2.0;
                  z = phit->getZ() - origin1.getZ();
                  L = origin2.getZ() - origin1.getZ();
                  f = z / L;
                  r = R * sqrt(1.0 + f * (f - 1.0) * 4 * sqr(sin(t)));

                  // Now get the prediction from the final line.
                  // Hmmm...probably a dumb way to do it, but I'm
                  // gonna make a cylinder with which I will intersect
                  // the track's line.
                  PHCylinder theCylinder(origin, r, cylinderAxis);
                  intersectionLineCylinder(theTrackLine, theCylinder, predPoint1, predPoint2);

                  // There should be two points of intersection; pick
                  // the one which has the lesser phi difference with
                  // the UV wire.  The other intersection point ought
                  // to be about M_PI away.  This is a big enough
                  // difference that it doesn't matter whether we
                  // choose to compare to cylPoint1 or cylPoint2 or
                  // their average.
                  cylPredPoint1 = predPoint1;
                  cylPredPoint2 = predPoint2;
                  if (abs(cylPredPoint1.getPhi() - cylPoint1.getPhi()) <
                      abs(cylPredPoint2.getPhi() - cylPoint1.getPhi()))
                    {
                      cylPredPoint = predPoint1;
                    }
                  else
                    {
                      cylPredPoint = predPoint2;
                    }
                  array[m++] = cylPredPoint.getPhi();
                  array[m++] = cylPredPoint.getZ();
                }
              else
                {
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                }
            }
          UValignment->Fill(array);
        }
    }
  return True;
}

PHBoolean
PHDchHistogrammer::fillRawDistributions(PHCompositeNode *TopNode)
{
  //---------------------------------------------
  // fill the time distribution for the edge = 0 and edge = 1
  //---------------------------------------------

  PHNodeIterator nodeIter(TopNode);
  PHObjectNode_t *phob;

  phob = static_cast < PHObjectNode_t * >(nodeIter.findFirst ("PHIODataNode", "DchRawTablev1"));
  rawTable = dynamic_cast < DchRawTable * >(phob->getData ());
  if (!rawTable)
    {
      cout << "DchRawTablev1 not found " << endl;
      return False;
    }

  int totalEntries = rawTable->Entries();
  short arm, side, plane, cell;
  float time;
  for (int i = 0; i < totalEntries; i++)
    {
      arm = rawTable->getArm(i);
      side = rawTable->getSide(i);
      cell = rawTable->getCell(i);
      plane = rawTable->getPlane(i);
      time = (float)rawTable->getTime(i);
      cellDistribution[arm][side][plane]->Fill(cell);
      if (plane < 12)
        {
          timeDistributionX1->Fill(time);
        }
      else if (plane < 16)
        {
          timeDistributionU1->Fill(time);
        }
      else if (plane < 20)
        {
          timeDistributionV1->Fill(time);
        }
      else if (plane < 32)
        {
          timeDistributionX2->Fill(time);
        }
      else if (plane < 36)
        {
          timeDistributionU2->Fill(time);
        }
      else
        {
          timeDistributionV2->Fill(time);
        }
    }
  return True;
}

PHBoolean PHDchHistogrammer::fillRawData(PHCompositeNode *TopNode)
{
  PHNodeIterator nodeIter(TopNode);
  PHObjectNode_t *phob;

  phob = static_cast < PHObjectNode_t * >(nodeIter.findFirst ("PHIODataNode", "DchRawTablev1"));
  rawTable = dynamic_cast < DchRawTable * >(phob->getData ());
  if (!rawTable)
    {
      cout << "DchRawTablev1 not found " << endl;
      return False;
    }

  PHDataNode<int> *eventNode = (PHDataNode<int>*)nodeIter.findFirst("PHDataNode", "DchEvent");
  int* event = eventNode->getData();
  globalEvent = *(event);
  int evt = *(event);
  int totalEntries = rawTable->Entries();

  int time, global;
  short arm, side, plane, cell, edge;

  int nraws = totalEntries;
  for (int i = 0; i < totalEntries; i++)
    {
      global = rawTable->getGlobal(i);
      plane = rawTable->getPlane(i);
      side = rawTable->getSide(i);
      arm = rawTable->getArm(i);
      cell = rawTable->getCell(i);
      edge = rawTable->getEdge(i);
      time = rawTable->getTime(i);

      raw->Fill(evt, nraws, global, arm, side, plane, cell, edge, time);

    }
  return True;
}

PHBoolean PHDchHistogrammer::fillHits(PHCompositeNode* TopNode)
{
  PHObjectNode_t *phob;
  PHNodeIterator nodeIter(TopNode);

  phob = static_cast < PHObjectNode_t * >(nodeIter.findFirst ("PHIODataNode", "DchHitLineTablev1"));
  hitLineTable = dynamic_cast < DchHitLineTable * >(phob->getData ());
  if (!hitLineTable)
    {
      cout << "DchHitLineTablev1 not found " << endl;
      return False;
    }

  PHDataNode<int> *eventNode = (PHDataNode<int>*)nodeIter.findFirst("PHDataNode", "DchEvent");
  int* event = eventNode->getData();
  int evt = *(event);
  globalEvent = *(event);

  int totalEntries = hitLineTable->Entries();

  short plane, side, arm, cell;
  float distance, width;
  float x, y, z;
  float vx, vy, vz;
  float time1, time2;

  float array[16];
  int nhits = totalEntries;
  for (int i = 0; i < totalEntries; i++)
    {
      plane = hitLineTable->getPlane(i);
      side = hitLineTable->getSide(i);
      arm = hitLineTable->getArm(i);
      cell = hitLineTable->getCell(i);
      width = hitLineTable->getWidth(i);
      x = hitLineTable->getXYZ(i).getX();
      y = hitLineTable->getXYZ(i).getY();
      z = hitLineTable->getXYZ(i).getZ();
      vx = hitLineTable->getVXYZ(i).getX();
      vy = hitLineTable->getVXYZ(i).getY();
      vz = hitLineTable->getVXYZ(i).getZ();
      distance = hitLineTable->getDistance(i);
      time1 = hitLineTable->getTime1(i);
      time2 = hitLineTable->getTime2(i);

      // fill on off distributions
      onoff[arm][side]->Fill(Axis_t(cell + 0.5), Axis_t(plane + 0.5));

      int m = 0;
      array[m++] = evt;
      array[m++] = nhits;
      array[m++] = arm;
      array[m++] = side;
      array[m++] = plane;
      array[m++] = cell;
      array[m++] = distance;
      array[m++] = width;
      array[m++] = x;
      array[m++] = y;
      array[m++] = z;
      array[m++] = vx;
      array[m++] = vy;
      array[m++] = vz;
      array[m++] = time1;
      array[m++] = time2;

      hit->Fill(array);
    }

  return True;
}

PHBoolean
PHDchHistogrammer::fillTracks(PHCompositeNode* TopNode)
{
  PHTypedNodeIterator <DchTrack> dchtrackiter(TopNode);
  DchTrackNode_t *DchTrackNode = dchtrackiter.find("DchTrack");
  if (DchTrackNode)
    {
      trackTable = DchTrackNode->getData();
    }
  else
    {
      cout << PHWHERE << "DchTrack Node not found" << endl;
      return False;
    }

  PHNodeIterator nodeIter(TopNode);
  PHDataNode<int> *eventNode = (PHDataNode<int>*)nodeIter.findFirst("PHDataNode", "DchEvent");
  int* event = eventNode->getData();
  int evt = *(event);
  globalEvent = *(event);

  short arm, side;
  float x, y, z, vx, vy, vz;
  float xerr, yerr, zerr, err;
  float alpha, phi, phi0, momentum, quality;
  float beta, zed, theta0;
  float aa[19];

  int totalEntries = trackTable->get_DchNTrack();
  int ntracks = totalEntries;
  PHPoint errPoint;

  for (int i = 0; i < totalEntries; i++)
    {
      x = trackTable->get_point(i).getX();
      y = trackTable->get_point(i).getY();
      z = trackTable->get_point(i).getZ();
      xerr = errPoint.getX();
      yerr = errPoint.getY();
      zerr = errPoint.getZ();
      err = sqrt(xerr * xerr + yerr * yerr + zerr * zerr);
      vx = trackTable->get_direction(i).getX();
      vy = trackTable->get_direction(i).getY();
      vz = trackTable->get_direction(i).getZ();
      arm = trackTable->get_arm(i);
      side = trackTable->get_side(i);
      alpha = trackTable->get_alpha(i);
      phi = trackTable->get_phi(i);
      phi0 = trackTable->get_phi0(i);
      beta = trackTable->get_beta(i);
      zed = trackTable->get_zed(i);
      theta0 = trackTable->get_theta0(i);
      momentum = trackTable->get_momentum(i);
      quality = trackTable->get_quality(i);
      int m = 0;
      aa[m++] = evt;
      aa[m++] = ntracks;
      aa[m++] = arm;
      aa[m++] = side;
      aa[m++] = x;
      aa[m++] = y;
      aa[m++] = z;
      aa[m++] = err;
      aa[m++] = vx;
      aa[m++] = vy;
      aa[m++] = vz;
      aa[m++] = alpha;
      aa[m++] = phi;
      aa[m++] = zed;
      aa[m++] = beta;
      aa[m++] = momentum;
      aa[m++] = phi0;
      aa[m++] = theta0;
      aa[m++] = quality;
      track->Fill(aa);
    }

  cout << "Using new table structure " << trackTable->get_DchNTrack() << endl;
  return True;
}

PHBoolean PHDchHistogrammer::fillTrackInfo(const PHPointerList<DchTrackInfo>* trackInfoList)
{
  float array[33];
  float time;
  float tid, rid, hid, nhit;
  float arm, side, cell, plane, edge;
  float tx, ty, tz, hx, hy, hz;
  float tvx, tvy, tvz, hvx, hvy, hvz;
  float dist, width;
  float residual, blind;
  float ntracks, out;
  float evt;

  float alpha, beta, phi, zed;

  DchRawInfo* inforaw;
  DchHitInfo* infohit;
  DchTrackInfo* infotrack;

  int length = trackInfoList->length();

  ntracks = (float)length;

  for (int i = 0; i < length; i++)
    {
      infotrack = (*trackInfoList)[i];
      if (!infotrack)
        break;
      evt = globalEvent;
      tid = (float)infotrack->getId();
      arm = (float)infotrack->getArm();
      side = (float)infotrack->getSide();
      tx = infotrack->getBasepoint().getX();
      ty = infotrack->getBasepoint().getY();
      tz = infotrack->getBasepoint().getZ();
      tvx = infotrack->getDirection().getX();
      tvy = infotrack->getDirection().getY();
      tvz = infotrack->getDirection().getZ();

      alpha = infotrack->getAlpha();
      beta = infotrack->getBeta();
      phi = infotrack->getPhi();
      zed = infotrack->getZed();

      int numberOfHits = (infotrack->getHitInfoList()).length();
      nhit = (float)numberOfHits;

      for (int k = 0; k < numberOfHits; k++)
        {
          infohit = infotrack->getHitInfo(k);
          if (!infohit)
            continue;
          hid = (float)infohit->getId();
          plane = (float)infohit->getPlane();
          cell = (float)infohit->getCell();
          dist = infohit->getDistance();
          width = infohit->getWidth();
          hx = infohit->getBasepoint().getX();
          hy = infohit->getBasepoint().getY();
          hz = infohit->getBasepoint().getZ();
          hvx = infohit->getDirection().getX();
          hvy = infohit->getDirection().getY();
          hvz = infohit->getDirection().getZ();
          blind = infohit->getBlind();
          out = infohit->getOut();
          residual = infohit->getResidual();
          inforaw = infohit->getRaw1();
          if (inforaw)
            {
              rid = (float)inforaw->getId();
              edge = (float)inforaw->getEdge();
              time = (float)inforaw->getTime();
            }
          else
            {
              rid = -1;
              edge = -1;
              time = -1;
            }

          int m = 0;
          array[m++] = evt;
          array[m++] = ntracks;
          array[m++] = tid;
          array[m++] = arm;
          array[m++] = side;
          array[m++] = tx;
          array[m++] = ty;
          array[m++] = tz;
          array[m++] = tvx;
          array[m++] = tvy;
          array[m++] = tvz;
          array[m++] = alpha;
          array[m++] = beta;
          array[m++] = phi;
          array[m++] = zed;
          array[m++] = nhit;
          array[m++] = hid;
          array[m++] = plane;
          array[m++] = cell;
          array[m++] = dist;
          array[m++] = width;
          array[m++] = hx;
          array[m++] = hy;
          array[m++] = hz;
          array[m++] = hvx;
          array[m++] = hvy;
          array[m++] = hvz;
          array[m++] = residual;
          array[m++] = blind;
          array[m++] = out;
          array[m++] = rid;
          array[m++] = edge;
          array[m++] = time;

          trackInfo->Fill(array);

        } //  loop on hits
    } // loop on tracks
  return True;
}

PHBoolean PHDchHistogrammer::fillTimeResidual(float dt, int p1, int t1, int p2, int t2, int p3, int t3)
{
  float array[7];
  int m = 0;
  array[m++] = dt;
  array[m++] = p1;
  array[m++] = t1;
  array[m++] = p2;
  array[m++] = t2;
  array[m++] = p3;
  array[m++] = t3;

  timeResidual->Fill(array);

  return True;
}

PHBoolean PHDchHistogrammer::fillHitInfo(const PHPointerList<DchHitInfo>* hitInfoList)
{
  float array[31];

  float evt, nhit;
  float trid, rid, hid;
  float arm, side, cell, plane;
  float tx, ty, tz, hx, hy, hz;
  float tvx, tvy, tvz, hvx, hvy, hvz;
  float dist, width, edge, time;
  float sx, sy, sz, nx, ny, nz;

  DchRawInfo* inforaw;
  DchHitInfo* infohit;

  int length = hitInfoList->length();
  nhit = (float)length;
  evt = globalEvent;

  for (int i = 0; i < length; i++)
    {
      infohit = (*hitInfoList)[i];
      if (!infohit)
        continue;
      hid = (float)infohit->getId();
      arm = (float)infohit->getArm();
      side = (float)infohit->getSide();
      plane = (float)infohit->getPlane();
      cell = (float)infohit->getCell();
      dist = infohit->getDistance();
      width = infohit->getWidth();
      hx = infohit->getBasepoint().getX();
      hy = infohit->getBasepoint().getY();
      hz = infohit->getBasepoint().getZ();
      hvx = infohit->getDirection().getX();
      hvy = infohit->getDirection().getY();
      hvz = infohit->getDirection().getZ();
      inforaw = infohit->getRaw1();
      if (inforaw)
        {
          rid = (float)inforaw->getId();
          edge = (float)inforaw->getEdge();
          time = (float)inforaw->getTime();
        }
      else
        {
          rid = -1;
          edge = -1;
          time = -1;
        }
      trid = infohit->getTrackId();
      tx = infohit->getTrackBasepoint().getX();
      ty = infohit->getTrackBasepoint().getY();
      tz = infohit->getTrackBasepoint().getZ();
      tvx = infohit->getTrackDirection().getX();
      tvy = infohit->getTrackDirection().getY();
      tvz = infohit->getTrackDirection().getZ();
      sx = infohit->getSouth().getX();
      sy = infohit->getSouth().getY();
      sz = infohit->getSouth().getZ();
      nx = infohit->getNorth().getX();
      ny = infohit->getNorth().getY();
      nz = infohit->getNorth().getZ();

      int m = 0;
      array[m++] = evt;
      array[m++] = nhit;
      array[m++] = hid;
      array[m++] = arm;
      array[m++] = side;
      array[m++] = plane;
      array[m++] = cell;
      array[m++] = dist;
      array[m++] = width;
      array[m++] = hx;
      array[m++] = hy;
      array[m++] = hz;
      array[m++] = hvx;
      array[m++] = hvy;
      array[m++] = hvz;
      array[m++] = rid;
      array[m++] = edge;
      array[m++] = time;
      array[m++] = trid;
      array[m++] = tx;
      array[m++] = ty;
      array[m++] = tz;
      array[m++] = tvx;
      array[m++] = tvy;
      array[m++] = tvz;
      array[m++] = sx;
      array[m++] = sy;
      array[m++] = sz;
      array[m++] = nx;
      array[m++] = ny;
      array[m++] = nz;

      hitInfo->Fill(array);
    } //  loop on hits

  return True;
}

void PHDchHistogrammer::drawTrackInfo()
{
  if (! trackCanvas)
    {
      trackCanvas = new TCanvas("trackCanvas", "trackCanvas", 100, 100, 800, 800);
    }
  trackCanvas->cd();
  hit->Draw("evt");
  trackCanvas->Modified();
  trackCanvas->Update();

}

PHBoolean PHDchHistogrammer::fillEfficiency(int* eYes, int* eAll, int* bYes, int* bAll)
{
  float array[24];
  for (int ii = 0; ii < 6; ii++)
    {
      array[ii] = (float)eYes[ii];
      array[ii + 6] = (float)eAll[ii];
      array[ii + 12] = (float)bYes[ii];
      array[ii + 18] = (float)bAll[ii];
    }

  effi->Fill(array);
  return True;
}

PHBoolean PHDchHistogrammer::fillNoise(int a, int s, int p, int c, float e0, int e1)
{
  float array[6];
  array[0] = a;
  array[1] = s;
  array[2] = p;
  array[3] = c;
  array[4] = e0;
  array[5] = e1;
  noise->Fill(array);

  return True;
}

PHBoolean PHDchHistogrammer::fillReferenceTime(int time)
{
  referenceTime->Fill(time);
  return True;
}

void PHDchHistogrammer::fillSupposedEffiPlane(int plane)
{
  supposedEffiPlane->Fill(float(plane));

  if (plane < 12 )
    supposedEffiType->Fill(0);
  else if (plane < 16)
    supposedEffiType->Fill(1);
  else if (plane < 20)
    supposedEffiType->Fill(2);
  else if (plane < 32)
    supposedEffiType->Fill(3);
  else if (plane < 36)
    supposedEffiType->Fill(4);
  else
    supposedEffiType->Fill(5);
}

void PHDchHistogrammer::fillTrueEffiPlane(int plane)
{
  trueEffiPlane->Fill(float(plane));

  if (plane < 12)
    trueEffiType->Fill(0);
  else if (plane < 16)
    trueEffiType->Fill(1);
  else if (plane < 20)
    trueEffiType->Fill(2);
  else if (plane < 32)
    trueEffiType->Fill(3);
  else if (plane < 36)
    trueEffiType->Fill(4);
  else
    trueEffiType->Fill(5);

}

int PHDchHistogrammer::fillHoughArray()
{
  int x, y;

  for (x = 0;x < 6000 / 20;x++)
    {
      for (y = 0;y < 300;y++)
        {
          houghArray->SetCellContent(x, y, x*y);
        }
    }
  return 1;
}

int PHDchHistogrammer::initCellCanvas()
{
  if (CellCanvas == NULL)
    CellCanvas = new TCanvas("CellCanvas", "CellCanvas", 100, 100, 800, 800);
  CellCanvas->cd();
  Side0Arm0 = new TH1F("Side0Arm0", "Side0Arm0", 80, 0, 80);
  Side1Arm0 = new TH1F("Side1Arm0", "Side1Arm0", 80, 0, 80);
  Side0Arm1 = new TH1F("Side0Arm1", "Side0Arm1", 80, 0, 80);
  Side1Arm1 = new TH1F("Side1Arm1", "Side1Arm1", 80, 0, 80);
  Side0Arm0->SetFillColor(14);
  Side1Arm0->SetFillColor(14);
  Side0Arm1->SetFillColor(14);
  Side1Arm1->SetFillColor(14);

  return 0;
}

int
PHDchHistogrammer::fillCellCanvas(int arm, int side, int plane, int cell)
{
  return 1;
}

PHBoolean
PHDchHistogrammer::fillResiduals(float array[14])
{
  residual->Fill(array);
  return True;
}
