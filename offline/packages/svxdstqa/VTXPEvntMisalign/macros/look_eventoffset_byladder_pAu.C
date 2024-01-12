#include <TFile.h>
#include <TTree.h>
#include <TSystem.h>

#include <string>
#include <utility>
#include <map>

void look_eventoffset_byladder_pAu(
    //char* infile  = "data/428166/misalignTree_428166.root",
    //char* infile  = "/phenix/scratch/kurthill/misalignTrees/434052_reverse/misalignTree_434052.root",
    char* infile  = "/phenix/scratch/kurthill/misalignTrees/434052_newascii_noskip/misalignTree_434052.root",
    char* outfile  = "434052_test_out.root",
    int run = 434052,
    int ladder = 2)
{

  bool print_images = true;

  TFile* f = new TFile(infile);
  TTree* t_in = (TTree*)f->Get("tree");

  if(print_images)
    gSystem->Exec(Form("mkdir -p plots_new/run15pAu/newascii_noskip/%d",run));
    //gSystem->Exec(Form("mkdir -p plots_new/run15pAu/%d",run));

  // max must coinside with the _misalign_max variable defined in VTXPEvntMisalign.h
  const int max = 1000;
  int chip(0), n_misalign(0);
  int misalignments[max][3] = {0};
  bool trash(false), died(false);

  TFile* g = new TFile(outfile,"RECREATE");
  TTree* t_out = new TTree("misalignments","Tree of event misalignment data");
  t_out->Branch("run",&run,"run/I");
  t_out->Branch("ladder",&ladder,"ladder/I");
  t_out->Branch("chip",&chip,"chip/I");
  t_out->Branch("n_misalign",&n_misalign,"n_misalign/I");
  t_out->Branch("misalignments",misalignments,"misalignments[n_misalign][3]/I");
  t_out->Branch("died",&died,"died/O");
  t_out->Branch("trash",&trash,"trash/O");

  gSystem->Load("libVTXPEvntMisalign.so");

  for(int ichip = 4; ichip < 6; ++ichip)
  {
    //VTXPEvntMisalign* evntmis = new VTXPEvntMisalign(t_in, ladder, ichip, "AuAu");
    VTXPEvntMisalign* evntmis = new VTXPEvntMisalign(t_in, ladder, ichip, "pAu");
    //VTXPEvntMisalign* evntmis = new VTXPEvntMisalign(t_in, ladder, ichip, "pp");
    evntmis->add_shift(1);
    evntmis->add_shift(2);
    evntmis->add_shift(3);
    //evntmis->add_shift(4);
    //evntmis->add_shift(5);
    //evntmis->add_shift(6);

    // Good for AuAu
    //evntmis->set_nbincorr(20);
    //evntmis->set_nrebin(50);
    //evntmis->set_sigma(10);
    //evntmis->set_threshold_multiplier(7);
    //evntmis->set_died_threshold(0.2);
    //evntmis->make_corr_histos("BBC");
    
    // Good for pAu
    evntmis->set_nbincorr(70);
    evntmis->set_nrebin(2000);
    evntmis->set_sigma(6);
    evntmis->set_threshold_multiplier(5);
    evntmis->set_died_threshold(0.06);
    evntmis->make_corr_histos("BBC");
    
    // Good for pp
    //evntmis->set_nbincorr(100);
    //evntmis->set_nrebin(20000);
    //evntmis->set_sigma(2);
    //evntmis->set_threshold_multiplier(4);
    //evntmis->set_died_threshold(0.05);
    //evntmis->make_corr_histos("ARM");
    
    //evntmis->set_spike_threshold(0.05);
    evntmis->run_search();


    chip       = ichip;
    n_misalign = evntmis->get_n_misalignments();
    died = evntmis->get_did_die();
    if(n_misalign < max)
      evntmis->get_misalignment_array(misalignments);
    trash = evntmis->get_did_misalign_max();

    t_out->Fill();

    int lay = ladder < 10 ? 0 : 1;
    int lad = ladder < 10 ? ladder : ladder - 10;
    int sen = ichip / 4;
    int chi = ichip % 4;

    if(print_images)
    {
      evntmis->print_image(
          Form("plots_new/run15pAu/newascii_noskip/%d/%d_B%dL%dS%dC%d.png",run,run,lay,lad,sen,chi),false);
      evntmis->print_image(
          Form("plots_new/run15pAu/newascii_noskip/%d/%d_B%dL%dS%dC%d_DoG.png",run,run,lay,lad,sen,chi),true);
    }
  
    delete evntmis;
  }
  
  g->cd();
  t_out->Write();
}


