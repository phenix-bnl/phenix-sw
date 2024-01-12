#include <TFile.h>
#include <TTree.h>
#include <TSystem.h>

#include <string>
#include <utility>
#include <map>

void look_eventoffset(
    char* infile  = "/phenix/scratch/kurthill/misalignTrees/421989/misalignTree_421989.root",
    char* outfile  = "421989_test.root",
    char* imageoutdir = "test",
    int run = 421989,
    int iladder = 0)
{

    bool print_images = true;
    bool save_histos = true;

    TFile* f = new TFile(infile);
    TTree* t_in = (TTree*)f->Get("tree");

    if(print_images || save_histos)
	gSystem->Exec(Form("mkdir -p %s",imageoutdir));

    // max must coinside with the _misalign_max variable defined in VTXPEvntMisalign.h
    const int max = 1000;
    int chip(0), n_misalign(0), ladder(0);
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
    // gSystem->Load("/direct/phenix+u/zrowan/offline/packages/svxdstqa/VTXPEvntMisalign/install/lib/libVTXPEvntMisalign.so");

    for(int ichip = 0; ichip < 16; ++ichip)
    {
	//VTXPEvntMisalign* evntmis = new VTXPEvntMisalign(t_in, ladder, ichip, "AuAu");
	//VTXPEvntMisalign* evntmis = new VTXPEvntMisalign(t_in, ladder, ichip, "pAu");
	VTXPEvntMisalign* evntmis = new VTXPEvntMisalign(t_in, iladder, ichip, "pp");
	evntmis->add_shift(1);
	evntmis->add_shift(2);
	evntmis->add_shift(3);

	// Good for AuAu
	//evntmis->set_nbincorr(20);
	//evntmis->set_nrebin(50);
	//evntmis->set_sigma(10);
	//evntmis->set_threshold_multiplier(7);
	//evntmis->set_died_threshold(0.2);
	//evntmis->make_corr_histos("BBC");
      
	// Good for pAu
	//evntmis->set_nbincorr(70);
	//evntmis->set_nrebin(2000);
	//evntmis->set_sigma(6);
	//evntmis->set_threshold_multiplier(5);
	//evntmis->make_corr_histos("BBC");
      
	// Good for pp
	evntmis->set_nbincorr(100);
	evntmis->set_nrebin(20000);
	evntmis->set_sigma(2);
	evntmis->set_threshold_multiplier(4);
	evntmis->set_died_threshold(0.05);
	evntmis->make_corr_histos("ARM");
      
	int lay = iladder < 10 ? 0 : 1;
	int lad = iladder < 10 ? iladder : iladder - 10;
	int sen = ichip / 4;
	int chi = ichip % 4;

	if(save_histos)
	{
	    evntmis->save_histos(
		Form("%s/%d_B%dL%dS%dC%d.root",imageoutdir,run,lay,lad,sen,chi),false);
	    // evntmis->save_histos(
	    //     Form("%s/%d/%d_B%dL%dS%dC%d.root",imageoutdir,run,run,lay,lad,sen,chi),true);
	}

	//evntmis->set_spike_threshold(0.05);
	evntmis->run_search();

	ladder     = iladder;
	chip       = ichip;
	n_misalign = evntmis->get_n_misalignments();
	died = evntmis->get_did_die();
	if(n_misalign < max)
	    evntmis->get_misalignment_array(misalignments);
	trash = evntmis->get_did_misalign_max();

	t_out->Fill();

	if(print_images)
	{
	    evntmis->print_image(
		Form("%s/%d_B%dL%dS%dC%d.png",imageoutdir,run,lay,lad,sen,chi),false);
	    //evntmis->print_image(
	    //    Form("%s/%d/%d_B%dL%dS%dC%d_DoG.png",imageoutdir,run,run,lay,lad,sen,chi),true);
	}
    
	delete evntmis;
    }

  
    g->cd();
    t_out->Write();
}


