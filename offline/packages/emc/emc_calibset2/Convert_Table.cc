void Convert_Table(char* title = "trkass"){ 
   int is;
   char fname[128];
   char cline[128];
   int iarm,isect,itwr,iswkey,iqa;
   int iz,iy;
   float corr, corr_err;
   int read_num,irun,n;
   //
   //=============================================================== Full field run list
   cout<<" Open run2_fullfield_table.txt.... "<<endl;
   bool run_fullfield[50000];
   n = 50000;
   while(n--) run_fullfield[n] = false;
   ifstream frun("/phenix/u/htorii/local/photon/Calibset2/run2_fullfield_table.txt");
   read_num = 0;
   while( frun >> irun ){
     if( irun < 50000 && irun >= 0 ){
       run_fullfield[irun] = true;
       //cout<<irun<<"   "<<cline<<endl;
       read_num++;
     }
     frun.getline(cline,128);
   }
   cout<<"  ..... Read "<<read_num<<" lines "<<endl;
   frun.close();

   //=============================================================== QA bit
   cout<<" Open QA table.... "<<endl;
   bool qabit_miprun[8][50000];
   bool qabit_tofrun[8][50000];
   bool qabit_miptwr[8][4608];
   bool qabit_toftwr[8][4608];
   is = 8;
   while( is-- ){
     itwr = 2592;
     while(itwr--){
       qabit_miptwr[is][itwr] = false;
       qabit_toftwr[is][itwr] = false;
     }
     irun = 50000;
     while( irun-- ){
       qabit_miprun[is][irun] = false;
       qabit_tofrun[is][irun] = false;
     }
   }
   is = 8;
   while( is-- ){
     sprintf(fname,"%s_qatwr_sect%d.txt",title,is);
     cout<<" Open file : "<<fname<<endl;
     ifstream finqa_twr(fname);
     while( finqa_twr.getline(cline,128) ){
       if( cline[0] != '#' ){
	 sscanf(cline,"%d %d",&itwr,&iqa);
	 if( iqa & 0x00ff > 0  )
	   qabit_miptwr[is][itwr] = true;
	 if( iqa & 0x0f00 > 0 )
	   qabit_toftwr[is][itwr] = true;
       }
     }
     finqa_twr.close();
   }
   is = 8;
   while( is-- ){
     sprintf(fname,"%s_qarun_sect%d.txt",title,is);
     cout<<" Open file : "<<fname<<endl;
     ifstream finqa_run(fname);
     while( finqa_run.getline(cline,128) ){
       if( cline[0] != '#' ){
	 sscanf(cline,"%d %d",&irun,&iqa);
	 if( iqa & 0x00ff > 0  )
	   qabit_miprun[is][itwr] = true;
	 if( iqa & 0x0f00 > 0 )
	   qabit_tofrun[is][itwr] = true;
       }
     }
     finqa_run.close();
   }
   //=============================================================== MIP tower
   cout<<" MIP Tower table.... "<<endl;
   cout<<"         .... Writing to pbsc_ecorr_twr_Run2.txt "<<endl;
   ofstream fout_miptwr("pbsc_ecorr_twr_Run2.txt");
   fout_miptwr.precision(4);
   is = 6;
   while( is-- ){
     sprintf(fname,"%s_miptwr_sect%d.txt",title,is);
     cout<<" Open file : "<<fname<<endl;
     ifstream fin(fname);
     //
     while( fin.getline(cline,128) ){
       read_num = sscanf(cline,"%d %f %f",&itwr,&corr,&corr_err);
       if( cline[0] != '#' && read_num == 3 ){
	 if( is < 4 ) iarm = 0; else iarm = 1;
	 if( is < 4 ) isect = is; else isect = is - 2;
	 iz = itwr % 72;
	 iy = (int)(itwr / 72);
	 iswkey = iarm*100000 + isect*10000 + iy*100 + iz;
	 corr = corr / 0.270;
	 corr_err = corr_err / 0.270;
	 fout_miptwr<<iswkey<<"\t"<<corr<<"\t"<<corr_err<<"\t";
	 if(! qabit_miptwr[is][itwr] && corr < 1.3 && corr > 0.7 ) fout_miptwr<<" 1 "<<endl;
	 else fout_miptwr<<" 0 "<<endl;
       }
     }
     fin.close();
   }
   fout_miptwr.close();
   //=============================================================== MIP run
   cout<<" MIP Run table.... "<<endl;
   cout<<"         .... Writing to pbsc_ecorr_run_Run2.txt "<<endl;
   ofstream fout_miprun("pbsc_ecorr_run_Run2.txt");
   fout_miprun.precision(4);
   is = 6;
   while( is-- ){
     sprintf(fname,"%s_miprun_sect%d.txt",title,is);
     cout<<" Open file : "<<fname<<endl;
     ifstream fin(fname);
     //
     while( fin.getline(cline,128) ){
       read_num = sscanf(cline,"%d %f %f",&irun,&corr,&corr_err);
       if( cline[0] != '#' && read_num == 3 ){
	 if( run_fullfield[irun] ){
	   if( is < 4 ) iarm = 0; else iarm = 1;
	   if( is < 4 ) isect = is; else isect = is - 2;
	   corr = corr / 0.270;
	   corr_err = corr_err / 0.270;
	   fout_miprun<<irun<<"\t"<<iarm<<"\t"<<isect<<"\t"<<corr<<"\t"<<corr_err;
	   if(! qabit_miprun[is][itwr] && corr < 10 ) fout_miprun<<" 1 "<<endl;
	   else fout_miprun<<" 0 "<<endl;
	 } else {
	   cout<<" ... Skipp run "<<irun<<" because of half field run... "<<endl;
	 }
       }
     }
     fin.close();
   }
   fout_miprun.close();

   //=============================================================== TOF tower
   cout<<" TOF Tower table.... "<<endl;
   cout<<"         .... Writing to pbsc_tofcorr_twr_Run2.txt "<<endl;
   cout<<"         .... Writing to pbgl_tofcorr_twr_Run2.txt "<<endl;
   ofstream fout_toftwr_pbsc("pbsc_tofcorr_twr_Run2.txt");
   ofstream fout_toftwr_pbgl("pbgl_tofcorr_twr_Run2.txt");
   fout_toftwr_pbsc.precision(4);
   fout_toftwr_pbgl.precision(4);
   is = 8;
   while( is-- ){
     sprintf(fname,"%s_t0twr_sect%d.txt",title,is);
     cout<<" Open file : "<<fname<<endl;
     ifstream fin(fname);
     ofstream* ftmp; 
     if( is < 6 ) ftmp = &fout_toftwr_pbsc;
     else ftmp = &fout_toftwr_pbgl;
     ofstream& fout_toftwr = *ftmp;
     //
     while( fin.getline(cline,128) ){
       read_num = sscanf(cline,"%d %f %f",&itwr,&corr,&corr_err);
       if( cline[0] != '#' && read_num == 3 ){
	 if( is < 4 ) iarm = 0; else iarm = 1;
	 if( is < 4 ) isect = is;
	 else if( is < 6 ) isect = is - 2;
	 else isect = is - 6;
	 iz = itwr % 72;
	 iy = (int)(itwr / 72);
	 iswkey = iarm*100000 + isect*10000 + iy*100 + iz;
	 //	 cfile >> y >> z >> s >> a >> corr >> reso;
	 //	 cfile >> lc >> lc_err >> lc_flag >> flag;
	 fout_toftwr<<iy<<" "<<iz<<" "<<isect<<" "<<iarm<<" ";
	 fout_toftwr<<corr<<" "<<corr_err<<" ";
	 if( qabit_miptwr[is][itwr] ) fout_toftwr<<"0";
	 else fout_toftwr<<"1";
	 fout_toftwr<<" 0.0 0.0 0 1.0 0.0 0";
	 fout_toftwr<<endl;
       }
     }
     fin.close();
   }
   fout_toftwr_pbsc.close();
   fout_toftwr_pbgl.close();
   //=============================================================== TOF run
   cout<<" TOF Run table.... "<<endl;
   cout<<"         .... Writing to pbsc_tofcorr_run_Run2.txt "<<endl;
   cout<<"         .... Writing to pbgl_tofcorr_run_Run2.txt "<<endl;
   ofstream fout_tofrun_pbsc("pbsc_tofcorr_run_Run2.txt");
   ofstream fout_tofrun_pbgl("pbgl_tofcorr_run_Run2.txt");
   fout_tofrun_pbsc.precision(4);
   fout_tofrun_pbgl.precision(4);
   is = 8;
   while( is-- ){
     sprintf(fname,"%s_t0run_sect%d.txt",title,is);
     cout<<" Open file : "<<fname<<endl;
     ifstream fin(fname);
     ofstream* ftmp;
     if( is < 6 ) ftmp = &fout_tofrun_pbsc;
     else ftmp = &fout_tofrun_pbgl;
     ofstream& fout_tofrun = *ftmp;

     while( fin.getline(cline,128) ){
       read_num = sscanf(cline,"%d %f %f",&irun,&corr,&corr_err);
       if( cline[0] != '#' && read_num == 3 ){
	 if( is < 4 ) iarm = 0; else iarm = 1;
	 if( is < 4 ) isect = is;
	 else if( is < 6 ) isect = is - 2;
	 else isect = is - 6;
	 fout_tofrun<<irun<<"\t"<<iarm<<"\t"<<isect<<"\t"<<corr<<"\t"<<corr_err;
	 if( qabit_tofrun[is][itwr] ) fout_tofrun<<" 0 "<<endl;
	 else fout_tofrun<<" 1 "<<endl;
       }
     }
     fin.close();
   }
   fout_tofrun_pbsc.close();
   fout_tofrun_pbgl.close();
   //===============================================================


 }
