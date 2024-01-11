#include <iomanip>
#include <iostream>


const int nrun=2;
const int run[nrun]={};

const int nch=40;

void avg_diff(int run)
{
  double mean[nch][nrun];
  double rms [nch][nrun];

  /** read mean, rms from text files **/
  for(int irun=0; irun<nrun; irun++)
    {
      char fname[100];
      sprintf(fname,"ZdcCalib.ledmean_run%d",run[irun]);
      ifstream ifs(fname);
      for(int ich=0;ich<40;ich++)
	{	  
	  char line[100];
	  ifs.getline(line,100);
	  sscanf(line,"%ld %ld", &mean[ich][irun], &rms[ich][irun]);
	}
    }

  /** calc **/
  FILE* fp=NULL;

  /** for pedestal average **/
  char fname[100];
  sprintf(fname,"ZdcCalib.pedestal_average_run%d_run%d",run[0],run[nrun-1]);
  fp= fopen(fname); 

  for(int ich=0;ich<40;ich++)
    {
      double avg_mean, avg_sig;
      avg(mean, rms, avg_mean, avg_sig);
      fprintf(fp,"%.1ld\t%.1ld\t0\n", avg_mean, avg_sig);
      cout<<"ch"<<ich<<"\taverage = "<<avg_mean<<" +- "<<avg_sig<<endl;
    }
  fclose(fp);

  /** difference for gain matching **/

  sprintf(fname,"ZdcCalib.gain_diff_run%d_run%d",run[0],run[nrun-1]);
  fp= fopen(fname); 

  for(int ich=0;ich<40;ich++)
    {
      double diff_val, diff_err;
      diff(mean, rms, diff_val, diff_err);
      fprintf(fp,"%.1ld\t%.1ld\n", diff_val, diff_err);
      cout<<"ch"<<ich<<"\tdiff = "<<diff_val<<" +- "<<diff_err<<endl;
    }
  fclose(fp);

}

void avg(const double x[], const double dx[], double& mean_avg, double& sig_avg)
{
  const int n=( sizeof(x)/sizeof(x[0]) );
  
  mean_avg=0;
  sig_avg=0;
  
  for(int i=0; i<n; i++)
    {
      mean_avg+=x[i];
      sig_avg+=pow(dx[i],2);
    }

  mean_avg = mean_avg/(double)n;
  sig_avg  = sqrt(sig_avg)/(double)n;
}

void diff(const double x[], const double dx[], double& mean_diff, double& sig_diff)
{  
  mean_diff = fabs(x[1]-x[0]);
  sig_diff  = sqrt( pow(dx[0],2) + pow(dx[1],2) );
 }
