// This macro outputs the following functional forms via:
//
// .L fit_GlauberVar_vs_b()
// TF1*f=(TF1*)fit_GlauberVar_vs_b()
// emcAnalyzerUtils::dump_function(f)
/*
  TF1 *fAT_vs_b= new TF1("fAT_vs_b","pol6/([7]+exp((x-[8])/[9]))",0,20);
  fAT_vs_b->SetParameters(3755.7,-22.6065,-6.78784,-11.8696,2.1149,-0.141457,0.00344969,25.0349,14.0565,0.606894)
  
  TF1 *fNpart_vs_b= new TF1("fNpart_vs_b","pol6/([7]+exp((x-[8])/[9]))",0.,20.); 
  fNpart_vs_b->SetParameters(1142.49,-17.4869,-6.84553,-2.34426,0.414572,-0.0231652,0.000458723,2.98342,11.9418,1.22045)
  
  TF1 *fNcoll_vs_b= new TF1("fNcoll_vs_b","pol6/([7]+exp((x-[8])/[9]))",0.,20.); 
  fNcoll_vs_b->SetParameters(9449.06,-187.61,-143.892,-6.29555,2.96372,-0.196842,0.00408522,7.73342,10.9962,1.07787)

  TF1 *fcc_percentile_vs_b= new TF1("fcc_percentile_vs_b","pol6",0,20);
  fcc_percentile_vs_b->SetParameters(0.0378117,-0.140785,0.618647,-0.0519543,0.00583817,-0.000186341,-1.87372e-06)

  TF1 *b_vs_fcc_percentile= new TF1("b_vs_fcc_percentile","pol9",0,100);
  b_vs_fcc_percentile->SetParameters(0.319777,1.02717,-0.11322,0.00840246,-0.000368604,9.77327e-06,-1.58187e-07,1.52723e-09,-8.07674e-12,1.80031e-14);
  

*/

TF1* fit_GlauberVar_vs_b( const TString opt = "b_vs_cc", double bfitmin = 0., double bfitmax = 100. )
{

  cout << "<I> OK. I'll compute the fit function of " << opt.Data() 
       << " versus impact parameter (b) ..." << endl 
       << "    Other options are: \"Ncoll\", \"AT\", \"CC\", and \"b_vs_CC\" " << endl;

  const int Ncent = 37;

  // 0-0%,0-1%,1-2%,2-4%, | 0.-1.5,1.5-2.5,2.5-3.5,3.5-4.5,4.5-5.,5-5.5% |
  // 0-0.1%,0.1-0.2%,0.2-0.4%
  // 0-5%,5-10%, ... 90-95%,95-100% | 96-97%,97-98%,98-99%,99-100%:

  double cc_percentile[Ncent]={0.,0.5,1.5,3.,
			       0.75,2.,3.,4.,4.75,5.25,
			       0.05,0.15,0.3,
			       2.5,7.5,12.5,17.5,22.5,27.5,32.5,37.5,42.5,47.5,52.5,57.5,
			       62.5,67.5,72.5,77.5,82.5,87.5,92.5,97.5,
			       96.5,97.5,98.5,99.5};

  double b[Ncent]={0.,1.0,1.8,2.2,
		   1.2,2.1,2.6,2.9,3.2,3.4,
		   0.30,0.57,0.84,
		   2.5,4.0,5.2,6.2,7.0,7.7,8.4,9.0,9.6,10.2,10.7,11.2,
		   11.7,12.1,12.6,13.0,13.5,13.9,14.5,15.7,
		   15.2,15.5,16.2,16.8};

  // fixme: A = 179 not 192 (?!)

  double Npart[Ncent]={2*192,374.4,362.8,345.8,
		       371.6,357.0,345.8,334.5,327.3,321.2,
		       379.4,378.6,376.1,
		       351.6,298.9,253.5,214.7,181.2,151.2,125.1,102.6,82.9,
		       66.0,51.8,40.0,30.2,22.0,15.9,11.2,7.9,5.7,4.1,2.7,
		       3.2,2.8,2.6,2.5};

  double Ncoll[Ncent]={1220,1180.6,1119.1,1041.1,
		       1165.3,1091.1,1040.8,991.2,957.6,933.9,
		       1209.2,1204.9,1196.0,
		       1069.9,845.4,671.1,532.2,419.1,325.3,249.9,188.3,139.3,
		       101.4,72.4,50.9,35.0,23.0,15.1,9.9,6.3,4.3,2.8,1.8,
		       2.0,1.8,1.7,1.5};

 double AT[Ncent]={27.8,27.583,27.043,26.167,
	           27.449,26.764,26.170,25.575,25.243,24.887,
		   27.753,27.682,27.484,
		   26.460,23.619,21.151,18.980,17.136,15.449,13.863,12.541,11.329,
		   10.218,9.241,8.364,7.598,6.888,6.238,5.699,5.267,4.876,4.732,4.84,
		   4.86,4.655,4.704,4.712};

  double Amax = 150.; // fm^2 normalized area
  double ATnorm[Ncent];

  for (int i=0;i<Ncent;i++)
    {
      ATnorm[i]=AT[i]/AT[0]*Amax;
    }

  double errb[Ncent];
  double errNpart[Ncent];
  double errNcoll[Ncent];
  double errAT[Ncent];
  double errcc_percentile[Ncent];

  // Trick-1: play with the (x,y) errors until you get a decent fit ...
  // Trick-2: play with the fit limits until you get a decent fit ...
  for (int i=0;i<Ncent;i++)
    {
      errb[i]=b[i]*0.03;
      errNpart[i]=Npart[i]*0.01;
      errNcoll[i]=Ncoll[i]*0.01;
      errAT[i]=AT[i]*0.03;
      errcc_percentile[i]=cc_percentile[i]*0.02;
    }


  TGraphErrors *GlauberVar_vs_b = 0;
  TF1 *fGlauberVar_vs_b = 0;
  TCanvas *c = 0;

  char title[200];

  if (opt.Contains("Npart"))
    {
      GlauberVar_vs_b = new TGraphErrors(Ncent,b,Npart,errb,errNpart);
      //TF1 *pol3= new TF1("pol3","pol3",0,8.09466);
      //pol3->SetParameters(381.692,-1.16793,-5.95361,0.288317)
      //fGlauberVar_vs_b = new TF1("fGlauberVar_vs_b","pol1/([2]+exp(((x-[3])/[4])))*pol1/([7]+exp(((x-[8])/[9])))",bfitmin,bfitmax);
      fGlauberVar_vs_b = new TF1("fGlauberVar_vs_b","pol6/([7]+exp(((x-[8])/[9])))",bfitmin,bfitmax);
      //fGlauberVar_vs_b = new TF1("fGlauberVar_vs_b","pol5/([6]+exp((x-[7])/[8]))",bfitmin,bfitmax);
      //fGlauberVar_vs_b = new TF1("fGlauberVar_vs_b","pol3*(x<8)+pol3/([9]+exp(((x-[10])/[11])))*(x>8)",bfitmin,bfitmax);
      //fGlauberVar_vs_b = new TF1("fGlauberVar_vs_b","pol1/([2]+exp(((x-[3])/[4])))+pol1/([7]+exp(((x-[8])/[9])))",bfitmin,bfitmax);
      sprintf(title,"fNpart_vs_b");
    }
  else if (opt.Contains("Ncoll"))
    {
      GlauberVar_vs_b = new TGraphErrors(Ncent,b,Ncoll,errb,errNcoll);
      fGlauberVar_vs_b = new TF1("fGlauberVar_vs_b","pol6/([7]+exp(((x-[8])/[9])))",bfitmin,bfitmax);
      sprintf(title,"fNcoll_vs_b");
    }
  else if (opt.Contains("AT"))
    {
      GlauberVar_vs_b = new TGraphErrors(Ncent,b,ATnorm,errb,errAT);
      fGlauberVar_vs_b = new TF1("fGlauberVar_vs_b","pol6/([7]+exp(((x-[8])/[9])))",bfitmin,bfitmax);
      sprintf(title,"fAT_vs_b");
    }
  else if (opt.Contains("b_vs_cc"))
    {
      GlauberVar_vs_b = new TGraphErrors(Ncent,cc_percentile,b,errcc_percentile,errb);
      GlauberVar_vs_b->SetMinimum(0.001);
      fGlauberVar_vs_b = new TF1("fGlauberVar_vs_b","pol9",bfitmin,bfitmax);
      sprintf(title,"b_vs_fcc_percentile");
    }
  else if (opt.Contains("cc"))
    {
      GlauberVar_vs_b = new TGraphErrors(Ncent,b,cc_percentile,errb,errcc_percentile);
      GlauberVar_vs_b->SetMinimum(0.001);
      fGlauberVar_vs_b = new TF1("fGlauberVar_vs_b","pol6",bfitmin,bfitmax);
      sprintf(title,"fcc_percentile_vs_b");

      c = new TCanvas(title,title,650,600);
      GlauberVar_vs_b->SetMarkerStyle(20);
      GlauberVar_vs_b->SetMarkerColor(2);
      GlauberVar_vs_b->Draw("AP");
      GlauberVar_vs_b->Fit("fGlauberVar_vs_b","R");
      fGlauberVar_vs_b->SetName(title);
      
      return fGlauberVar_vs_b;
    }
  else
    {
      cout << "<E> Don't know this option: " << opt << endl;
      return;
    }

  c = new TCanvas(title,title,650,600);
  GlauberVar_vs_b->SetMarkerStyle(20);
  GlauberVar_vs_b->SetMarkerColor(2);
  GlauberVar_vs_b->Draw("AP");

  const int N = 10; 
  if (N!=fGlauberVar_vs_b->GetNpar())
    {
      cout << endl << "<E> Npars fit != Npars func. = " << fGlauberVar_vs_b->GetNpar() << endl;
      return;
    }

  //double Npars[N]={1000,-20,-7,-2.,0.5,0,0.,3,10,1.}; // Npart
  //double Npars[N]={10000,-190,-100,-10,3,-0.1,0.0,10,10.,1.}; // Ncoll
  double Npars[N]={2500,-30,2,-1,2.,-1.,0.,10.,10.,1.};
  //double Npars[N]={3000,-30,7,-10,2.,-1.,0.,20.,10.,0.5};
  fGlauberVar_vs_b->SetParameters(2801.61,-30.6665,6.61134,-12.574,2.11156,-0.140449,0.00342081,18.6532,14.2221,0.560879);
  fGlauberVar_vs_b->SetParameters(-10,5,-0.3,0.,-0.0,0,-0,0,-0.,0.);

  //fGlauberVar_vs_b->SetParameters(Npars);
  GlauberVar_vs_b->Fit("fGlauberVar_vs_b","R");

  fGlauberVar_vs_b->SetName(title);

  return fGlauberVar_vs_b;
}


//   0-  5%:  351.6    0.2   18.4  1069.9    0.9  100.0     2.2    0.0    0.8   0.029  0.001  0.065  26.460  0.013  1.571  
//   5- 10%:  298.9    0.2   18.9   845.4    0.8   88.9     4.0    0.0    0.4   0.088  0.001  0.080  23.619  0.015  1.720  
//  10- 15%:  253.5    0.2   18.8   671.1    0.7   82.4     5.2    0.0    0.3   0.143  0.001  0.093  21.151  0.015  1.789  
//  15- 20%:  214.7    0.2   18.1   532.2    0.6   72.7     6.2    0.0    0.3   0.188  0.001  0.106  18.980  0.016  1.856  
//  20- 25%:  181.2    0.2   18.1   419.1    0.6   66.1     7.0    0.0    0.2   0.227  0.001  0.118  17.136  0.017  1.935  
//  25- 30%:  151.2    0.1   16.9   325.3    0.5   57.6     7.7    0.0    0.2   0.263  0.001  0.131  15.449  0.017  1.994  
//  30- 35%:  125.1    0.1   16.6   249.9    0.4   50.9     8.4    0.0    0.2   0.296  0.001  0.148  13.863  0.018  2.072  
//  35- 40%:  102.6    0.1   15.5   188.3    0.4   43.2     9.0    0.0    0.2   0.321  0.001  0.165  12.541  0.019  2.168  
//  40- 45%:   82.9    0.1   14.6   139.3    0.3   36.0     9.6    0.0    0.2   0.345  0.002  0.184  11.329  0.019  2.248  
//  45- 50%:   66.0    0.1   13.3   101.4    0.3   29.9    10.2    0.0    0.2   0.365  0.002  0.204  10.218  0.020  2.332  
//  50- 55%:   51.8    0.1   12.3    72.4    0.2   24.3    10.7    0.0    0.1   0.382  0.002  0.226   9.241  0.021  2.452  
//  55- 60%:   40.0    0.1   11.0    50.9    0.2   19.4    11.2    0.0    0.1   0.393  0.002  0.257   8.364  0.023  2.628  
//  60- 65%:   30.2    0.1    9.8    35.0    0.1   15.2    11.7    0.0    0.1   0.397  0.003  0.292   7.598  0.025  2.872  
//  65- 70%:   22.0    0.1    8.6    23.0    0.1   11.8    12.1    0.0    0.1   0.393  0.003  0.340   6.888  0.027  3.108  
//  70- 75%:   15.9    0.1    7.3    15.1    0.1    8.8    12.6    0.0    0.1   0.392  0.003  0.373   6.238  0.029  3.366  
//  75- 80%:   11.2    0.1    6.0     9.9    0.1    6.6    13.0    0.0    0.1   0.369  0.004  0.430   5.699  0.033  3.795  
//  80- 85%:    7.9    0.0    4.7     6.3    0.0    4.7    13.5    0.0    0.1   0.357  0.004  0.469   5.267  0.034  4.005  
//  85- 90%:    5.7    0.0    3.7     4.3    0.0    3.5    13.9    0.0    0.1   0.313  0.004  0.519   4.876  0.037  4.281  
//  90- 95%:    4.1    0.0    2.5     2.8    0.0    2.3    14.5    0.0    0.2   0.274  0.005  0.557   4.732  0.042  4.880  
//  95-100%:    2.9    0.0    1.6     1.8    0.0    1.4    15.7    0.0    0.7   0.172  0.005  0.610   4.944  0.047  5.479  

//  0.0-1.5%:  371.6    0.1    7.7  1165.3    1.1   66.9     1.2     0.0    0.4   0.007  0.001  0.054  27.449  0.020  1.254  
//  1.5-2.5%:  357.0    0.2    8.7  1091.1    1.2   65.0     2.09    0.0    0.1   0.022  0.001  0.062  26.764  0.026  1.330  
//  2.5-3.5%:  345.8    0.2    9.9  1040.8    1.3   67.0     2.56    0.0    0.1   0.036  0.001  0.064  26.170  0.027  1.386  
//  3.5-4.5%:  334.5    0.2   10.9   991.2    1.3   68.1     2.95    0.0    0.1   0.048  0.001  0.068  25.575  0.027  1.422  
//  4.5-5.0%:  327.3    0.3   10.9   957.6    1.9   70.7     3.22    0.0    0.0   0.056  0.002  0.071  25.243  0.040  1.485  
//  5.0-5.5%:  321.2    0.3   11.7   933.9    2.0   72.8     3.39    0.0    0.0   0.063  0.002  0.069  24.887  0.039  1.442  

//   0-  1%:  374.4    0.1    6.3  1180.6    1.2   64.0     1.0    0.0    0.3   0.006  0.001  0.053  27.583  0.024  1.232  
//   1-  2%:  362.8    0.2    8.1  1119.1    1.3   65.2     1.8    0.0    0.2   0.015  0.001  0.058  27.043  0.025  1.292  
//   2-  4%:  345.8    0.2   11.4  1041.1    1.0   71.1     2.5    0.0    0.2   0.035  0.001  0.065  26.167  0.019  1.418  

//  96- 97%:    3.2    0.0    1.7     2.0    0.0    1.6    15.2    0.0    0.1   0.214  0.011  0.594   4.996  0.105  5.473  
//  97- 98%:    2.8    0.0    1.5     1.8    0.0    1.3    15.5    0.0    0.1   0.192  0.012  0.612   4.655  0.101  5.270  
//  98- 99%:    2.7    0.0    1.3     1.7    0.0    1.2    15.9    0.0    0.2   0.156  0.012  0.613   4.704  0.097  5.091  
//  99-100%:    2.5    0.0    1.2     1.5    0.0    1.1    16.8    0.0    0.6   0.065  0.012  0.634   5.412  0.116  6.043  

// 0-0.1%  :  379.4    0.3    4.2  1209.2    3.7   59.9     0.30    0.0    0.1   0.007  0.003  0.048  27.753  0.072  1.171  
// 0.1-0.2%:  378.6    0.3    4.6  1204.9    3.4   56.7     0.57    0.0    0.1   0.000  0.003  0.053  27.682  0.068  1.133  
// 0.2-0.4%:  376.1    0.2    5.2  1196.3    2.5   57.2     0.84    0.0    0.1   0.005  0.002  0.055  27.484  0.053  1.226  
// 0.4-1.5%:  369.4    0.1    7.4  1152.2    1.2   65.0    1.42    0.0    0.2   0.008  0.001  0.055  27.394  0.023  1.271  
