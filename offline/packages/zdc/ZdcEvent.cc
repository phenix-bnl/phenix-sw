#include <ZdcOut.h>
#include <SmdOut.h>
#include <ZdcRawv1.h>
#include <ZdcEvent.hh>

#include <PdbPmtFitPar.hh>
#include <ZdcMap.h>

#include <getClass.h>
#include <Event.h>

#include <gsl/gsl_const.h>

#include <cstdlib>
#include <iostream>

using namespace std;

static const float C = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9;


ZdcEvent::ZdcEvent()
{
  setEventNumber (0);
  zdcraw = 0;
  zdcout = 0;
  smdout = 0;
  zdcrawevt = 0;
  zdc_pmt_map = 0;
  memset(Charge,0,sizeof(Charge));
}

ZdcEvent::~ZdcEvent()
{
  delete zdcrawevt;
  delete [] zdc_pmt_map;
}

void ZdcEvent::Clear ()
{
  ///reset ZDC raw data  
  if (zdcout) 
    {
      zdcout->Clear();
    }
  if (smdout) 
    {
      smdout->Clear();
    }
}

PHBoolean ZdcEvent::setRawData ( PHCompositeNode *topNode )
{
  Event* event = findNode::getClass<Event>(topNode,"PRDF");
  if ( event==0 )
    {
      cout << PHWHERE << "Unable to get PRDF, is Node missing?" << endl;
      return False;
    }
  
  if ( !zdcraw )
    {
      zdcraw = findNode::getClass<ZdcRaw>(topNode,"ZdcRaw");
      if( !zdcraw )
	{
	  zdcraw = new ZdcRawv1();
          zdcrawevt = zdcraw; // if we make the ZdcRaw here, we need to delete it in dtor
	}
    }
return  setRawData(event);
}

PHBoolean ZdcEvent::setRawData ( Event *event )
{
  // this is for the monitoring which does not have a node tree
  if ( !zdcraw )
    {
	  zdcraw = new ZdcRawv1();
          zdcrawevt = zdcraw; // if we make the ZdcRaw here, we need to delete it in dtor
    }


  // Get the relevant packets from the Event object and transfer the
  // data to the subsystem-specific table.
  Packet *p;

  static const int id = 13001;
  static const int nch_per_board = 8;

  p = event->getPacket(id);

  if ( p != 0 )
    {
      // we have an extra board in there now whose data is appended so the assumed
      // order here hasn't changed; 
      //      unsigned int num_channels = nch_per_board * p->iValue(0,"NUM_BOARDS") 
      unsigned int num_channels = nch_per_board * 5;
      if (num_channels != zdc_pmt_map_size)
	{
	  cout << PHWHERE 
          << " size of calibration bank "
	       << zdc_pmt_map_size 
              << " does not match number of channels" 
	       << num_channels
          << endl;
	  exit(1);
	}
      for (unsigned int pmt_number=0; pmt_number<num_channels; pmt_number++)
        {
          zdcraw->AddZdcRawHit( p->iValue(zdc_pmt_map[pmt_number]),
                                p->iValue(zdc_pmt_map[pmt_number], "T1"),
                                p->iValue(zdc_pmt_map[pmt_number], "T2"),
                                pmt_number );
	  //	  cout << "loop variable: " << pmt_number << "\tmapvalue: " << zdc_pmt_map[pmt_number] << "\t" << endl;

        }
      


      delete p;
    }
 
  return True;
}

PHBoolean ZdcEvent::PutEndProduct(PHCompositeNode * root)
{
  // get ZdcRaw object if it exists
  if (!zdcraw)
    {
      zdcraw = findNode::getClass<ZdcRaw>(root,"ZdcRaw");
      if (!zdcraw)
        {
          cout << "ZdcEvent: Unable to get ZdcRaw, is Node missing?" << endl;
          return False;
        }
    }

  // get ZdcOut object if it exists
  if (!zdcout)
    {
      zdcout = findNode::getClass<ZdcOut>(root,"ZdcOut");
      if (!zdcout)
        {
          cout << "ZdcEvent: Unable to get ZdcOut, is Node missing?" << endl;
          return False;
        }
    }

  // get SmdOut object if it exists
  if ( !smdout )
    {
      smdout = findNode::getClass<SmdOut>(root,"SmdOut");
      if ( !smdout )
        {
          static int num_messages = 0;
          if ( num_messages==0 )
            {
              cout << "ZdcEvent: Unable to get SmdOut, is Node missing?" << endl;
              ++num_messages;
            }
        }
    }

  calculate();

  // copy zdc information to the ZdcOut object
  for (int ipmt = 0; ipmt<ZDC_N_PMT; ipmt++)
    {
      zdcout->AddZdcHit(Charge[ipmt],Time0[ipmt],Time1[ipmt],(short)ipmt);
    }
  zdcout->AddZdcNS(Energy[0],Timing[0],0);
  zdcout->AddZdcNS(Energy[1],Timing[1],1);
  zdcout->set_TimeVertex(TimeZero,TimeZeroError,Zvertex,ZvertexError);

  // copy smd information to the SmdOut object (but only if it exists)
  if ( smdout )
    {
      for (int ipmt=ZDC_N_PMT; ipmt<zdcraw->get_npmt(); ipmt++)
        {
          smdout->AddSmdHit(Charge[ipmt],Timing[0],Timing[1],(short)(ipmt-8));
        }
      if (zdcraw->get_npmt() > 8)
	{
          int arm = Zdc::South;
          smdout->AddSmdNS(smdposx[arm],smdposy[arm],smdenergy[arm],arm);
          arm = Zdc::North;
          smdout->AddSmdNS(smdposx[arm],smdposy[arm],smdenergy[arm],arm);
	}
    }

  return True;
}

int ZdcEvent::calculate ()
{
  //-*** get calibrated information (energy, time)
  for (short ipmt = 0; ipmt < ZDC_N_PMT; ipmt++)
    {
      Time0Err[ipmt] = calib.getTdcGain0()->getCalibPar(ipmt)->getPar4();
      Time1Err[ipmt] = calib.getTdcGain1()->getCalibPar(ipmt)->getPar4();

      Charge[ipmt] = calib.getCharge( ipmt, zdcraw->get_Adc(ipmt) );
      Time0[ipmt] = calib.getHitTime0( ipmt, zdcraw->get_Tdc0(ipmt),
                                             zdcraw->get_Adc(ipmt) );
      Time1[ipmt] = calib.getHitTime1( ipmt, zdcraw->get_Tdc1(ipmt),
                                             zdcraw->get_Adc(ipmt) );
    }

  bool hit_in_south = false;
  bool hit_in_north = false;
  if ( Time1[0] > -999. ) hit_in_south = true;
  if ( Time1[4] > -999. ) hit_in_north = true;

  int num_channels = zdcraw->get_npmt();
  if (num_channels>ZDC_N_PMT)
    {
      // south smd
      for (short ipmt=ZDC_N_PMT; ipmt<24; ipmt++)
        {
          if ( hit_in_south ) Charge[ipmt] = calib.getCharge( ipmt, zdcraw->get_Adc(ipmt) );
          else                Charge[ipmt] = 0.;
        }

      // north smd
      for (short ipmt=24; ipmt<40; ipmt++)
        {
          if ( hit_in_north ) Charge[ipmt] = calib.getCharge( ipmt, zdcraw->get_Adc(ipmt) );
          else                Charge[ipmt] = 0.;
        }
    }

  //-*** calculate the energy
  // run01: We use the digital sum since the analog is saturated in
  //        the fan-in/fan-out.
  // run02: New Maryland fanin/fanout. We start to use the analog sum
  //        for energy measurement.
  // run03: Analog sum for d side (North), linearity corrected
  //        digital sum on the Au side (South).
  // run04: Linearity corrected digital sum for both sides.

  Energy[Zdc::South] = 0.;
  Energy[Zdc::North] = 0.;

  if ( Time0[0] != INVALID_FLOAT )
    {
      if ( (calib.getAdcGain())->getCalibPar(0)->getPar4()==1 )
        {
          // use the digital sum
          for (int ipmt=1; ipmt<=3; ipmt++)
            {
              if ( Charge[ipmt] != INVALID_FLOAT )
                {
                  Energy[Zdc::South] += Charge[ipmt];
                }
            }
        }
      else
        {
          // use the analog sum
          Energy[Zdc::South] = Charge[0];
        }
    }
  if ( Time0[4] != INVALID_FLOAT )
    {
      if ( (calib.getAdcGain())->getCalibPar(4)->getPar4()==1 )
        {
          // use the digital sum
          for (int ipmt=5; ipmt<=7; ipmt++)
            {
              if ( Charge[ipmt] != INVALID_FLOAT )
                {
                  Energy[Zdc::North] += Charge[ipmt];
                }
            }
        }
      else
        {
          // use the analog sum
          Energy[Zdc::North] = Charge[4];
        }
    }

  //-*********************************************************************
  //-*** calculate the timing information (vertex, start time)
  //-*********************************************************************

  // Get Arm Hit Times
  CalcArmTime(Zdc::South,4,Timing[Zdc::South],TimingError[Zdc::South]);
  CalcArmTime(Zdc::North,4,Timing[Zdc::North],TimingError[Zdc::North]);

  if ( (Timing[Zdc::South] != INVALID_FLOAT) && (Timing[Zdc::North] != INVALID_FLOAT) )
    {
      // Zvertex is in centimeters, TimeZero is in seconds
      float t0_offset = calib.getTzero()->getCalibPar(0)->getPeakChannel();
      TimeZero = (Timing[Zdc::South] + Timing[Zdc::North]) / 2.0 - t0_offset;
      TimeZeroError = 0.5*sqrt(TimingError[Zdc::South]*TimingError[Zdc::South] + 
                               TimingError[Zdc::North]*TimingError[Zdc::North]);

      float zvtx_offset = calib.getZvtx()->getCalibPar(0)->getPeakChannel();
      Zvertex = C * (Timing[Zdc::South] - Timing[Zdc::North]) / 2.0 - zvtx_offset;
      ZvertexError = C * TimeZeroError;
    }
  else
    {
      Zvertex = INVALID_FLOAT;
      ZvertexError = INVALID_FLOAT;
      TimeZero = INVALID_FLOAT;
      TimeZeroError = INVALID_FLOAT;
    }

  // now calculate SMD positions
  if ( num_channels > 8 )
    {
      if ( hit_in_south )
        {
          smdposx[Zdc::South] = CalcSmdPosX(Zdc::South);
          smdposy[Zdc::South] = CalcSmdPosY(Zdc::South);
          smdenergy[Zdc::South] = CalcSmdEnergy(Zdc::South);
        }
      else
        {
          smdposx[Zdc::South] = INVALID_FLOAT;
          smdposy[Zdc::South] = INVALID_FLOAT;
          smdenergy[Zdc::South] = 0.;
        }

      if ( hit_in_north )
        {
          smdposx[Zdc::North] = CalcSmdPosX(Zdc::North);
          smdposy[Zdc::North] = CalcSmdPosY(Zdc::North);
          smdenergy[Zdc::North] = CalcSmdEnergy(Zdc::North);
        }
      else
        {
          smdposx[Zdc::North] = INVALID_FLOAT;
          smdposy[Zdc::North] = INVALID_FLOAT;
          smdenergy[Zdc::North] = 0.;
        }
    }

  return 0;
}

/// returns smd x centroid in cm (using vertical slats)
double ZdcEvent::CalcSmdPosX(const int arm)
{
  // Relative coordinate of the strip's center lines. The offsets have to 
  // be applied if needed.
  // scale factor to account for strips wrapping: 10.5 cm "ideal" width of 
  // 7 x-strips, 11.cm - measured one.
  double scl_str = 11./10.5 ;

  // X-strips width is 1.5 cm
  // "ideal"position relative ZDC's center.
  // this can be hard-coded and won't change
  double x_slat_rel[7] = { -4.5, -3.0, -1.5, 0.0, 1.5, 3.0, 4.5};

  // the following will eventually go in the database
  // these need to be updated with the survey numbers
  double x_slat_offset[2];

  // Offset of vert slats relative to ZDC_s center (cm)
  x_slat_offset[Zdc::South] = calib.getSmdOffset()->getCalibPar(0)->getPeakChannel();

  // Offset of vert slats relative to ZDC_n center (cm)
  x_slat_offset[Zdc::North] = calib.getSmdOffset()->getCalibPar(2)->getPeakChannel();

  //Positioning x-strips relative ZDC vertical center line: 
  double x_slat_pos[8] = {0.};
  for( int islat =0; islat< 7; islat++)
    {
      x_slat_pos[islat] = x_slat_rel[islat]*scl_str + x_slat_offset[arm];
    }

  int begin_ch = 0;
  int end_ch = 0;

  if ( arm == Zdc::South )	// south vertical slats
    {
      begin_ch = 16;
      end_ch = 22;
    }
  else				// north vertical slats
    {
      begin_ch = 32;
      end_ch = 38;
    }

  // calculate center of gravity
  double weighted_sum = 0.;
  double sum = 0.;
  double weighted_avg = 0.;
  for (int ich=begin_ch; ich<=end_ch; ich++)
    {
      int slat = ich - begin_ch;

      weighted_sum += Charge[ich]*x_slat_pos[slat];
      sum += Charge[ich];
    }

  if ( sum!=0. ) weighted_avg = weighted_sum/sum;
  else           weighted_avg = INVALID_FLOAT;

  // north arm has inverse phenix coordinates
  if ( arm == Zdc::North ) weighted_avg = -1.0*weighted_avg;

  return weighted_avg;
}

/// returns smd y centroid in cm (using horizontal slats)
double ZdcEvent::CalcSmdPosY(const int arm)
{
  double y_slat_rel[8] = { 1.0, 3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0};  
  double sin45 = sin(M_PI_4);     // sin(45 dgr)
  double plane_thickness = 0.65;  // Thickness of single scintillator plane(6mm) including coverage.
  double scl_str = 11./10.5 ;

  //-** There is additional vertical offset of SMD
  //-** due to steel right angle support (0.3 cm)
  double y_slat_offset[2];
  y_slat_offset[Zdc::South] = 0.3 + calib.getSmdOffset()->getCalibPar(1)->getPeakChannel();
  y_slat_offset[Zdc::North] = 0.3 + calib.getSmdOffset()->getCalibPar(3)->getPeakChannel();

  double beam_altitude[2];     // Beam altitude relative to the ZDC's bottom side
  beam_altitude[Zdc::South] = 5.5;
  beam_altitude[Zdc::North] = 5.64;

  double y_slat_pos[8] = {0.};
  for( int islat=0; islat < 8; islat++)
    {
      y_slat_pos[islat] = y_slat_rel[islat]*scl_str*sin45
        + 1.5*plane_thickness*sin45 + y_slat_offset[arm] - beam_altitude[arm];
    }

  int begin_ch = 0;
  int end_ch = 0;

  if ( arm == Zdc::South )	// south horizontal slats
    {
      begin_ch = 8;
      end_ch = 15;
    }
  else				// north horizontal slats
    {
      begin_ch = 24;
      end_ch = 31;
    }

  double weighted_sum = 0.;
  double sum = 0.;
  double weighted_avg = 0.;
  for (int ich=begin_ch; ich<=end_ch; ich++)
    {
      int slat = ich - begin_ch;

      weighted_sum += ( Charge[ich] * y_slat_pos[slat] );
      sum += Charge[ich];
    }

  if ( sum!=0. ) weighted_avg = weighted_sum/sum;
  else           weighted_avg = INVALID_FLOAT;

  return weighted_avg;
}

double ZdcEvent::CalcSmdEnergy(int arm)
{
  int begin_ch = 0;
  int end_ch = 0;
  if ( arm == Zdc::South )
    {
      begin_ch = 8;
      end_ch = 22;
    }
  else if ( arm == Zdc::North )
    {
      begin_ch = 24;
      end_ch = 38;
    }

  float energy = 0.;
  for (short ich=begin_ch; ich<=end_ch; ich++)
    {
      energy += Charge[ich];
    }

  return energy;
}

// This calculates time using 1st nch channels
int ZdcEvent::CalcArmTime(int arm, int nch, float &timing, float &timing_error)
{
  if (!(arm == Zdc::South || arm == Zdc::North))
    {
      timing = INVALID_FLOAT;
      return 0;
    }

  ZdcCalibPar<PdbPmtFitPar> *tdcgain0 = calib.getTdcGain0();
  ZdcCalibPar<PdbPmtFitPar> *tdcgain1 = calib.getTdcGain1();

  int hits = 0;		// number of valid pmt hits in one arm of zdc
  bool warnflag = false; // whether we have a warn PMT

  int startch;
  if (arm==Zdc::South) 
    {
      startch = 0;
    }
  else if (arm==Zdc::North) 
    {
      startch = 4;
    }
  else
    {
      cout << "Bad Arm: " << arm << endl;
      timing = INVALID_FLOAT;
      return 0;
    }

  //-** calc time = sum(time_i/sigma_i^2)/sum(1/sigma_i^2)
  //-** calc timing_error = sqrt(sum(sigma_i^2))
  double sum_timing = 0.;
  double sum_terr_weight = 0.;
  short int ich;
  for (ich=startch; ich<startch+nch; ich++)
    {
      // check for status bit
      // 0 == good, 1 == bad, 2 == warning
      if ( (tdcgain0->getCalibPar(ich))->getStatus() == 0 )
        {
          if ( Time0[ich] != INVALID_FLOAT )
            {
              double terrweight = 0.200;
              if (Time0Err[ich]!=0.) terrweight = 1.0/(Time0Err[ich]*Time0Err[ich]);
              else                   terrweight = 0.200;
              sum_timing += Time0[ich]*terrweight;
              sum_terr_weight += terrweight;
              hits++;
            }
        }
      else if ( (tdcgain0->getCalibPar(ich))->getStatus() == 2 )
        {
          warnflag = true;
        }

      if ( (tdcgain1->getCalibPar(ich))->getStatus() == 0 )
        {
          if ( Time1[ich] != INVALID_FLOAT )
            {
              double terrweight = 0.200;
              if (Time1Err[ich]!=0.) terrweight = 1.0/(Time1Err[ich]*Time1Err[ich]);
              else                   terrweight = 0.200;
              sum_timing += Time1[ich]*terrweight;
              sum_terr_weight += terrweight;
              hits++;
            }
        }
      else if ( (tdcgain1->getCalibPar(ich))->getStatus() == 2 )
        {
          warnflag = true;
        }
    }

  // if we didn't find any hits, and there were some warning
  // PMTs, we go back and use the warning PMTs
  if ( warnflag && (hits == 0) )
    {
      for (ich=startch; ich<startch+4; ich++)
        {
          if ( (tdcgain0->getCalibPar(ich))->getStatus() == 2 )
            {
              if ( Time0[ich] != INVALID_FLOAT )
                {
                  double terrweight = 0.200;
                  if (Time0Err[ich]!=0.) terrweight = 1.0/(Time0Err[ich]*Time0Err[ich]);
                  else                   terrweight = 0.200;
                  sum_timing += Time0[ich]*terrweight;
                  sum_terr_weight += terrweight;
                  hits++;
                }
            }

          if ( (tdcgain1->getCalibPar(ich))->getStatus() == 2 )
            {
              if ( Time1[ich] != INVALID_FLOAT )
                {
                  double terrweight = 0.200;
                  if (Time1Err[ich]!=0.) terrweight = 1.0/(Time1Err[ich]*Time1Err[ich]);
                  else                   terrweight = 0.200;
                  sum_timing += Time1[ich]*terrweight;
                  sum_terr_weight += terrweight;
                  hits++;
                }
            }
        }
    }
   
  if (hits != 0)
    {
      timing = sum_timing / sum_terr_weight;	// Avg Arm Time
      timing_error = sqrt( 1.0/sum_terr_weight );
    }
  else
    {
      timing = INVALID_FLOAT;
    }

  return 1;
}

void ZdcEvent::setCalibDataAll()
{
  calib.restore();
  
}

void ZdcEvent::setCalibDataAll (const PHTimeStamp & time)
{
  calib.restore(time);

  ZdcMap zdcmap;
  // zdc_pmt_map is initialized to 0, null pointers can be deleted
  // but on the second call this will contain the map from the previous call
  delete [] zdc_pmt_map;
  zdc_pmt_map = zdcmap.FillChannelMap(time,zdc_pmt_map_size);

}

void ZdcEvent::setCalibDataAll (ZdcCalib * ExistCalib, const PHTimeStamp &time)
{
  calib = *ExistCalib;
  ZdcMap zdcmap;
  // zdc_pmt_map is initialized to 0, null pointers can be deleted
  // but on the second call this will contain the map from the previous call
  delete [] zdc_pmt_map;
  zdc_pmt_map = zdcmap.FillChannelMap(time,zdc_pmt_map_size);
}

void ZdcEvent::setCalibDataAll (const ZdcCalib& ExistCalib, const PHTimeStamp &time)
{
  calib = ExistCalib;
  ZdcMap zdcmap;
  // zdc_pmt_map is initialized to 0, null pointers can be deleted
  // but on the second call this will contain the map from the previous call
  delete [] zdc_pmt_map;
  zdc_pmt_map = zdcmap.FillChannelMap(time,zdc_pmt_map_size);
}
