
#include "SaveCanvas.C"
#include "SetOKStyle.C"


void CheckGeometry(void)
{

  gSystem->Load("libfvtxgeom.so");

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("RUNNUMBER", 431739);
  FvtxGeom::create_arms();

//   FvtxColumn * col = FvtxGeom::get_arm(arm)->get_cage(cage)->get_station(station)->get_sector(sector)->get_column(0);

  TH2F * hcover = new TH2F("hcover","FVTX Coverage in station count; z (cm); #eta",200,-60,60,200,1,3);


  for (int station = 0; station<4; station++)
  {
    const FvtxColumn * col = FvtxGeom::get_arm(1)->get_cage(1)->get_station(station)->get_sector(0)->get_column(0);
    assert(col);
    const double station_z = col->get_z();
    assert(station_z>0);

    const double inner_r = col->get_inner_radius();
    const double outer_r = col->get_outer_radius();

    cout <<"inner_r = "<<inner_r<<", outer_r = "<<outer_r<<endl;

    assert(inner_r>0);
    assert(outer_r>inner_r);

    for (int zbin = 1; zbin<=hcover->GetNbinsX(); zbin++)
    {
      const double z = hcover->GetXaxis()->GetBinCenter(zbin);

      for (int etabin = 1; etabin<=hcover->GetNbinsY(); etabin++)
      {
        const double eta = hcover->GetYaxis()->GetBinCenter(etabin);
        const double angle = atan(exp(-eta))*2;
        const double tan_angle = tan(angle);

        const double r = (col->get_z() - z) * tan(angle);

        if (r >= inner_r && r<= outer_r )
        {
          hcover->SetBinContent(zbin, etabin,
                                hcover->GetBinContent(zbin, etabin) + 1
                               );
        }
      }
    }
  }


  SetOKStyle();
  gStyle->SetOptStat(0);


  TCanvas * c1 = new TCanvas("CheckGeometry","CheckGeometry",1000,550);
  hcover -> Draw("COLZ");
//  SaveCanvas(c1,c1->GetName(),kFALSE);
}
