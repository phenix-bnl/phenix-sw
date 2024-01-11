//This is an example use of the MpcWarnMap class.  I use one run and
//multiple fit options to test how sigma (=
//sqrt(ChiSqPerDof)/correction) depends on the fit parameter used
//(ROB=0.x). Also, I test the effect of the background correction and
//the results are output to stdout


void macro_textfile_MpcWarnMap( std::string inputfile, const char* fn = "-graphs.png")
{
  
gSystem->Load("libmpc.so");

MpcWarnMap* warn = new MpcWarnMap(inputfile);
warn->SetSigmaCut(5.0);


 warn->CreateGraphs();
 warn->SetFitArg(0.7);
 warn->FitGraphs();
 for(int impc=0;impc<2;impc++)
    {
      TString ts_mpc = impc?"_N_":"_S_";
      TString FileName = warn->GetFitOpt(); FileName+=ts_mpc; FileName+= fn; 
      warn->DrawGraphsBounds(FileName,impc);
    }
 warn->Summarize();
 warn->FilePrintLineDiffRatioCorrected("temp.txt");
    
} // end block
