//-----------------------------------------------------------------------------------
// This program draws comparing plot with several different tube efficiency files   |    
// Comparison will be done HV chain by chain                                        |
// This program is maintained by MinJung Kweon, May 19, 2005  mjkwn@bnl.gov         |
// ----------------------------------------------------------------------------------


void plot_compare(char *south_outfile="south.gif", char *north_outfile="north.gif"){



        const int Ninput=2;

        int arm=0, gap=0, pannel=0, ort=0, hvgroup=0;
        float effic=0, effic_err=0;
        float eff[Ninput][2][5][6][2][3]={0};
        float eff_err[Ninput][2][5][6][2][3]={0};
        float x[3]={0.5,1.5,2.5};
        float ex[3]={0};



	ifstream fin1("/phenix/data22/mjkwn/run4AuAu/minbias/muidEff/tool_macro/testplot_input_RDana.txt"); //test HVmasked output
	ifstream fin2("/phenix/data22/mjkwn/run4AuAu/minbias/muidEff/tool_macro/testplot_input.txt"); //test RD output (from AuAu 200GeV)

	while(fin1 >> arm >> gap >> pannel >> ort >> hvgroup >> effic >> effic_err){
		eff[0][arm][gap][pannel][ort][hvgroup]=effic*100;
		eff_err[0][arm][gap][pannel][ort][hvgroup]=effic_err*100;
	}
	while(fin2 >> arm >> gap >> pannel >> ort >> hvgroup >> effic >> effic_err){
		eff[1][arm][gap][pannel][ort][hvgroup]=effic*100;
		eff_err[1][arm][gap][pannel][ort][hvgroup]=effic_err*100;
	}



	c1 = new TCanvas("c1","south tube efficiency",10,10,1200,800);
	c2 = new TCanvas("c2","north tube efficiency",10,10,1200,800);
	c1->Divide(12,5);
	c2->Divide(12,5);

        TH2F *hr = new TH2F("a","a",30,0,3,500,0,100);

	int nth=0;
	TGraph* gr[Ninput][120];
	for(int iarm=0; iarm<2; iarm++){
		for(int igap=0; igap<5; igap++){
                        for(int iort=0; iort<2; iort++){
			        for(int ipannel=0; ipannel<6; ipannel++){
			            nth=iarm*6*2*5+igap*6*2+iort*6+ipannel;

                                    for(int in=0; in<Ninput; in++){
                                        if((ipannel==1 || ipannel==4) && iort==0 ) {
                                           gr[in][nth] = new TGraphErrors(2,x,eff[in][iarm][igap][ipannel][iort],ex,eff_err[in][iarm][igap][ipannel][iort]);
                                        }else if((ipannel==1 || ipannel==4) && iort==1 ) {
                                           gr[in][nth] = new TGraphErrors(1,x,eff[in][iarm][igap][ipannel][iort],ex,eff_err[in][iarm][igap][ipannel][iort]);
                                        }else{
                                           gr[in][nth] = new TGraphErrors(3,x,eff[in][iarm][igap][ipannel][iort],ex,eff_err[in][iarm][igap][ipannel][iort]);
                                        }
                                    }

	                       	    gr[0][nth]->SetTitle("MuID Tube Efficiency : HV mask");
	                            gr[0][nth]->GetHistogram()->SetXTitle("hvchain");
                                    gr[0][nth]->GetHistogram()->SetYTitle("tube efficiency(%)");
				    gStyle->SetEndErrorSize(2);

	                            if (nth<60){
	                                	gStyle->SetOptStat(0);
				        	c1->cd(nth+1);
						hr->Draw();
                                                if(iort==0) gPad->SetFillColor(42);
                                                if(iort==1) gPad->SetFillColor(40);

                                                for(int in=0; in<Ninput; in++){
                                        		gr[in][nth]->SetMarkerColor(6-2*in);
                                        		gr[in][nth]->SetLineColor(6-2*in);
                                        		gr[in][nth]->SetMarkerStyle(29-4*in);
                                        		gr[in][nth]->SetMarkerSize(0.9);
	                                		gr[in][nth]->Draw("LP");
                                                }

			 	    }else if (nth>=60){
	                                	gStyle->SetOptStat(0);
						c2->cd(nth+1-60);
						hr->Draw();
                                                if(iort==0) gPad->SetFillColor(42);
                                                if(iort==1) gPad->SetFillColor(40);

                                                for(int in=0; in<Ninput; in++){
                                        		gr[in][nth]->SetMarkerColor(2+2*in);
                                        		gr[in][nth]->SetLineColor(2+2*in);
                                        		gr[in][nth]->SetMarkerStyle(29-4*in);
                                        		gr[in][nth]->SetMarkerSize(0.9);
	                                		gr[in][nth]->Draw("LP");
                                                }
				    }
				}
			}
		}
	}
	c1->SaveAs(south_outfile);
	c2->SaveAs(north_outfile);
}
