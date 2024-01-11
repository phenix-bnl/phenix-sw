static const int NCROSS=120;
class SpinDBContent;

void MakeRellumiFile(char *outdir,int run1,int run2){
  gSystem->Load("libuspin.so");

  SpinDBOutput spin_out("phnxrc");
  spin_out.StoreDBContent(run1,run2);

  char filename[1024];
  sprintf(filename,"%s/polarization.dat",outdir);
  remove(filename);
  FILE *fp_sum=fopen(filename,"w");
  fprintf(fp_sum,"#run fill bpol bpol_err ypol ypol_err shift\n");

  sprintf(filename,"%s/fill_vs_run.dat",outdir);
  remove(filename);
  FILE *fp_fill=fopen(filename,"w");
  fprintf(fp_fill,"#fill run\n");

  sprintf(filename,"%s/cross_shift.dat",outdir);
  remove(filename);
  FILE *fp_shift=fopen(filename,"w");
  fprintf(fp_shift,"#run shift\n");

  sprintf(filename,"%s/fill_vs_pol.dat",outdir);
  remove(filename);
  FILE *fp_pol=fopen(filename,"w");
  fprintf(fp_pol,"#fill bpol bpol_err ypol ypol_err\n");

  sprintf(filename,"%s/bad_bunch.dat",outdir);
  remove(filename);
  FILE *fp_bad_bunch=fopen(filename,"w");
  fprintf(fp_bad_bunch,"#fill bad_bunch\n");

  int fill_old=-1;
  for(int run=run1; run<run2+1; run++){
    SpinDBContent spin_cont;
    if(spin_out.CheckRunRow(run)==0){continue;}
    if(spin_out.GetDBContent(spin_cont,run)==0){continue;}

    MakeRellumiFile(outdir,spin_cont);
    MakePolFile(fp_sum,spin_cont);
    fprintf(fp_fill,"%d %6d\n",
	    spin_cont.GetFillNumber(),spin_cont.GetRunNumber());
    fprintf(fp_shift,"%6d %3d\n",
	    spin_cont.GetRunNumber(),spin_cont.GetCrossingShift());
    MakeSpinPattern(outdir,spin_cont);

    if(fill_old!=spin_cont.GetFillNumber()){
      MakeFillVsPol(fp_pol,spin_cont);
      MakeBadBunch(fp_bad_bunch,spin_cont);
    }

    fill_old=spin_cont.GetFillNumber();
    printf("Processed run %d.\n",run);
  }

  fclose(fp_sum);
  fclose(fp_fill);
  fclose(fp_shift);
  fclose(fp_pol);
  fclose(fp_bad_bunch);

  return;
}

///////////////////////////////////////////////////////////////

void MakeRellumiFile(char *outdir,SpinDBContent spin_cont){
  char filename[1024];
  sprintf(filename,"%s/gl1p%6.6d.dat",outdir,spin_cont.GetRunNumber());

  FILE *fpout=fopen(filename,"w");
  fprintf(fpout,"run %d : run_gl1p %d : fill %d\n",
	  spin_cont.GetRunNumber(),-9999,spin_cont.GetFillNumber());
  fprintf(fpout,"\n\n");
  for(int icross=0; icross<NCROSS; icross++){
    int bpat=spin_cont.GetSpinPatternBlue(icross);
    int ypat=spin_cont.GetSpinPatternYellow(icross);
    if(spin_cont.GetBadBunchFlag(icross)==1){bpat=20; ypat=20;}

    fprintf(fpout,"%3d: %11d %11d %11d %11d %6.1f %6.1f\n",icross,
	    (int)spin_cont.GetScalerBbcVertexCut(icross),
	    (int)spin_cont.GetScalerBbcNoCut(icross),
	    (int)spin_cont.GetScalerZdcWide(icross),
	    (int)spin_cont.GetScalerZdcNarrow(icross),
	    (float)bpat,(float)ypat);
  }

  fclose(fpout);

  return;
}

/////////////////////////////////////////////////////////////////

void MakePolFile(FILE *fp,SpinDBContent spin_cont){
  double bpol,bpol_err,ypol,ypol_err;
  spin_cont.GetPolarizationBlue(0,bpol,bpol_err);
  spin_cont.GetPolarizationYellow(0,ypol,ypol_err);

  fprintf(fp,"%6d %5d %10.4f %10.4f %10.4f %10.4f %5d\n",
	  spin_cont.GetRunNumber(),spin_cont.GetFillNumber(),
	  bpol,bpol_err,ypol,ypol_err,spin_cont.GetCrossingShift());

  return;
}

/////////////////////////////////////////////////////////////////

void MakeSpinPattern(char *outdir,SpinDBContent spin_cont){
  char filename[1024];
  sprintf(filename,"%s/run%6.6d.dat",outdir,spin_cont.GetRunNumber());

  FILE *fp=fopen(filename,"w");

  fprintf(fp,"run %d\n",spin_cont.GetRunNumber());
  fprintf(fp,"fill %d\n\n",spin_cont.GetFillNumber());
  for(int icross=0; icross<NCROSS; icross++){
    fprintf(fp,"%3d %3d %3d\n",icross,spin_cont.GetSpinPatternBlue(icross),
	    spin_cont.GetSpinPatternYellow(icross));
  }

  fclose(fp);

  return;
}

////////////////////////////////////////////////////////////////

void MakeFillVsPol(FILE *fp,SpinDBContent spin_cont){
  double bpol,bpol_err;
  double ypol,ypol_err;
  spin_cont.GetPolarizationBlue(0,bpol,bpol_err);
  spin_cont.GetPolarizationYellow(0,ypol,ypol_err);

  fprintf(fp,"%d %10.4f %10.4f %10.4f %10.4f\n",
	  spin_cont.GetFillNumber(),bpol,bpol_err,ypol,ypol_err);

  return;
}

/////////////////////////////////////////////////////////////////

void MakeBadBunch(FILE *fp,SpinDBContent spin_cont){
  for(int icross=0; icross<NCROSS; icross++){
    if(!spin_cont.GetBadBunchFlag(icross)){continue;}
    fprintf(fp,"%d %3d\n",spin_cont.GetFillNumber(),icross);
  }

  return;
}

/////////////////////////////////////////////////////////////////
