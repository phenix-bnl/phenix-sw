<?php

include('graph2d.class.php');

list ($tablename, $parname) = split("::",$_GET['par']);
$tag=$_GET['tag'];
$paramlow=$_GET['paramlow'];
$paramhigh=$_GET['paramhigh'];
$runmin=$_GET['runmin'];
$runmax=$_GET['runmax'];
$shouldTally=$_GET['shouldTally'];

$N_events=0;

//query db for parameter within the range
$psqlhost = "phnxdb2.phenix.bnl.gov";
$psqluser = "phbrowse";
$psqldb = "phnxdb2_calibrations";
$psqlpwd = "phbrowse";
$conn = pg_connect("host=$psqlhost user=$psqluser dbname=$psqldb password=$psqlpwd") or die("<br>Unable to connect to db.");
$sql = "select distinct on (runnumber) runnumber,inserttime,tag,parname,parameter,parerror from $tablename where parname='$parname'";

if ($_GET['tag']) $sql.=" and tag like '$tag'";
if ($_GET['paramhigh']) $sql.=" and (parameter+parerror)<$paramhigh";
if ($_GET['paramlow']) $sql.=" and (parameter-parerror)>$paramlow";
if ($_GET['runmin']) $sql.=" and runnumber>".$runmin;
if ($_GET['runmax']) $sql.=" and runnumber<".$runmax;
$sql.= " order by runnumber,inserttime desc";

$result=pg_exec($conn, $sql) or die("Pgsql Error: ".pg_result_error());
$numrows=pg_numrows($result);
//print("<br>".$sql);

$runnumber = array();
$parameter = array();
$parerror = array();
$par_min_err = array();
$par_plu_err = array();

for ($i=0;$i<$numrows;$i++) {
  $obj=pg_fetch_object($result);
  $runnumber[$i] = $obj->runnumber;
  if($shouldTally == "yes")
    {
      //select distinct will only return unique runnumbers
      //As this process requires a seperate connection into
      //the Phenix database for each runnumber, it is quite
      //slow.
      $N_events += get_num_events($runnumber[$i]);
    }
  $parameter[$i] = $obj->parameter;
  $parerror[$i] = $obj->parerror;
  $par_min_err[$i]=$obj->parameter - $obj->parerror;
  $par_plu_err[$i]=$obj->parameter + $obj->parerror;
  //echo "<br>parname=$obj->parname   parameter=$obj->parameter   parerror=$obj->parerror   runnumber=$obj->runnumber";
  //print("<br>par = $obj->parameter   err = $obj->parerror   min =".$par_min_err[$i]."  max = ".$par_plu_err[$i]);
  //print("<br>par =".$parameter[$i]." err = $obj->parerror   min =".$par_min_err[$i]."  max = ".$par_plu_err[$i]);
}

// Generate the plot
if ($numrows>0) 
{
  $minrun=min($runnumber);
  $maxrun=max($runnumber);
  $minparam=min($par_min_err);
  $maxparam=max($par_plu_err);

  // here I try to make a more convenient Y labels
  $YLegend = "Parameter";
  if (abs($maxparam)>1000 || abs($minparam)>1000)
    {
      $maxparam /= 1000;
      $minparam /= 1000;
      for ($i=0; $i<$numrows; $i++)
	{
	  $parameter[$i] /=1000; $parerror[$i] /=1000;
	}
      $YLegend .= "  ( x 10^3 )";
    }
  else
    {
      if (abs($maxparam)<0.9)
       {
          $maxparam *= 1000;
          $minparam *= 1000;
          for ($i=0; $i<$numrows; $i++) 
	    {
	      $parameter[$i] *=1000; $parerror[$i] *= 1000;
	    }
         $YLegend .="  ( x 10^-3 )";
       }
    }
  //------------------------------------------------
  $runstep= ($maxrun - $minrun)/5.0;
  $paramstep= ($maxparam - $minparam)/10.0;
  $param_plot = new graph2d ("jpeg", 800, 350, 120, 20, 30, 40);
  $param_plot->SetAxisScales ($minrun, $maxrun, $runstep, 1, $minparam, $maxparam, $paramstep, 1);
  $param_plot->SetGrid(TRUE, 0xA0, 0xA0, 0xA0);
  if($shouldTally == "yes")
    {
      $param_plot->SetTitle ($tablename." : ".$parname." - ".$N_events." BBCLL1 events");
    }
  else
    {
      $param_plot->SetTitle($tablename." : ".$parname);
    }
  $param_plot->SetXLegend ("Run Number");
  $param_plot->SetYLegend ($YLegend);
  $param_plot->GraphCoord();
  $param_plot->Draw($runnumber, $parameter, $parerror, 0xFF, 0, 0);
  $param_plot->ImageOut("");
  $param_plot->Close();
} else {
  header("Content-type: image/jpeg");
  echo `cat badrange.jpg`;
}

pg_close($conn);

?>

<?php
function get_num_events($runno)
{

  // Note: Accesses different database

  $host = "phnxdb2.phenix.bnl.gov";
  $dbname = "daq";
  $user = "phnxrc";
  $pswrd = "";

  $conn2 = pg_connect("host=$host dbname=$dbname user=$user password=$pswrd") or die ("Unable to Connect to Phenix Database"); 

  $res = pg_query($conn2,"select scalerupdatescaled from trigger where runnumber=$runno and and name='BBCLL1';");
  $line =  pg_fetch_array($res);
  pg_close($conn2);
  return $line["scalerupdatescaled"];
}

?>