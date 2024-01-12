{

bool status_bit = 0;
textFile << " ----------------------------------------------------" << endl;
textFile << " -- EWG QA Summary --" << endl;
textFile << " ----------------------------------------------------" << endl;

textFile << "BBCLL1 triggered live  events = " << elcut->GetBinContent(1) << endl;
textFile << "Min. Bias events = " << elcut->GetBinContent(2) << endl;
textFile << "ERT electron events = " << elcut->GetBinContent(3) << endl << endl;
textFile << "Electron cut summary (using ERT_electron events)" << endl;
textFile << "-------- --- ------- " << endl;
textFile << "total tracks = " << elcut->GetBinContent(4) << endl;
textFile << "DCH nx1>2 && nx2>2 = " << elcut->GetBinContent(5) << endl;
textFile << "&& |emcsdphi|<5 && |emcsdz|<5 = " << elcut->GetBinContent(6) << endl;
textFile << "&& |pc3sdphi|<5 && |pc3sdz|<5 = " << elcut->GetBinContent(7) << endl;
textFile << "&& 0.2<p<5 = " << elcut->GetBinContent(8) << endl;
textFile << "&& EMCal Q<2 = " << elcut->GetBinContent(9) << endl;
textFile << "&& RICH n0>2 = " << elcut->GetBinContent(10) << endl;
textFile << "&& chi2/npe<20 = " << elcut->GetBinContent(11) << endl;
textFile << "&& RICH n3>0 = " << elcut->GetBinContent(12) << endl;
textFile << "&& dep>-2 = " << elcut->GetBinContent(13) << endl;
textFile << "electrons = " << elmom->Integral() << endl << endl;
textFile << "electrons (BBCLL1>=1) = " << elbbcll1->Integral() << endl;

textFile << "Averages " << endl;
textFile << "-------- " << endl;
textFile << "<EMCal Q> = " << elemcq->GetMean() << "  sigma = " << elemcq->GetRMS() << endl;
textFile << "<Electron_dep> = " << eldep->GetMean() << "  sigma = " << eldep->GetRMS() << endl;
textFile << "<RICH n0> (dep>-2) = " << eln0->GetMean() << endl;
textFile << "<RICH n3> = " << eln3->GetMean() << endl;

textFile << "DCH tracks/event (ERT_Electron event)= " << float(elcut->GetBinContent(2))/elcut->GetBinContent(3) << endl;
textFile << "electrons / bbcll1 triggered event = " << float(elmom->Integral())/elcut->GetBinContent(2) << endl;
textFile << "electrons/event (ERT_Electron event)= " << float(elmom->Integral())/elcut->GetBinContent(3) << endl;
tectFile << "ERT_electrons / BBCLL1_electrons ' " << float(elmom->Integral())/elbbcll1->Integral() << endl;

// Status check
status_bit = elcut->GetBinContent(4)/elcut->GetBinContent(3) < 8.0; // <nTracks/ nEvents>
status_bit |= elmom->GetMean()<0.5 && elmom->GetMean() > 0.85; // 0.5 < <P> < 0.85
status_bit |= elemcq->GetMean()<0.1 && elemcq->GetMean()>1.4; // <EMCal matching> < 1.4 sigmas
status_bit |= eln0->GetMean()<3;  // RICH <n0> > 3
status_bit |= elmom->Integral()/elcut->GetBinContent(4) < 0.004; // <nElectrons/nTracks> > 0.004
status_bit |= elmom->Integral()/elbbcll1->Integral() <0.1; // ERT efficiency > 10%

textFile << "EWG Status = " << status_bit << endl;

statusFile << status_bit << "  "; // to be checked
}
