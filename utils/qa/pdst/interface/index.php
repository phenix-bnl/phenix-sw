<?php
header("Cache-control: private");
?>

<html>
<title>QA Summary Webpage</title>
<LINK REL=stylesheet HREF="style.css" TYPE="text/css">

<SCRIPT>
<!--

// Function to capture only allowed chars 
function AllowChar(event) {
  //var strValidChars = "0123456789.\b\t";
  //var strChar;
  //var KeyTyped = String.fromCharCode(event.keyCode).toString();
  
  //var blnResult = true;

  //if (strValidChars.indexOf(KeyTyped)==-1) blnResult = false;

  //return blnResult;
  return true;
}

// Doesn't work well across different platforms, so I've disabled this.
function AllowLogicChars(event) {
 
 //Still disallows delete, and arrow

  //var strValidChars = "123456789()&|C\b\t\x00\x08";

  // Handles both Internet Explorer and Netscape
  //var keyCode = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;

  //var KeyTyped = String.fromCharCode(keyCode).toString();

  //var blnResult = true;

  //if (strValidChars.indexOf(KeyTyped)==-1) blnResult = false;

  //return blnResult;
  return true;
}

function showtab(tabname) {
document.getElementById('plottab').style.display='none';
document.getElementById('mparamtab').style.display='none';
document.getElementById('SQLtab').style.display='none';
document.getElementById(tabname).style.display='block';
}

// -->
</SCRIPT>

<?php
// If returning from one of tab submit buttons, return to that tab
if($_POST['plotTabPostExists'])
{
  echo "<body onLoad=\"showtab('plottab')\">";
}
else if ($_POST['mparTabPostExists'])
{
  echo "<body onLoad=\"showtab('mparamtab')\">";
}
else if ($_POST['sqlTabPostExists'])
{
  echo "<body onLoad=\"showtab('SQLtab')\">";
}

// If no tab post exists, go to the last one seen
else if ($_POST['tabname'])
{
  $tabname = $_POST['tabname'];
  if ($tabname == "plottab")
    {
      echo "<body onLoad=\"showtab('plottab')\">";
    } 
  else if ($tabname == "mparamtab")
    {
      echo "<body onLoad=\"showtab('mparamtab')\">";
    }
  else if($tabname == "SQLtab") 
    {
      echo "<body onLoad=\"showtab('SQLtab')\">";
    }

// If all else fails, go to the plot tab
} else {
  echo "<body onLoad=\"showtab('plottab')\">";
}
?>

<div id="topblock">

<div style="">
<table width="100%" cellpadding="0" cellspacing="0">
<tr>
        <td width="200px">
        <img src="phenix_qc_logo_1.png" border=0 alt="Phenix Quality Control">
        </td>
     	<td valign=bottom>
        <div id="navbar">
        <table>
	<tr>
		<td><a href="index.php" style="color: #1E90FF">Run Selector</a></td>
         <!--       <td><a href="dictionary.php">Dictionary</a></td> -->
        	<td><a href="update.php">Update Parameters</a></td>
        	<td><a href="forums.php">Forums</a></td>
                <td><a href="tutorial.html">Tutorial</a></td>
                <td><a href="about.php">About</a></td>
	</tr>
        </table>
        </div>
        </td>
</tr>
</table>
</div>


</div>


<?php

include("myfunc.php");
include("myforms.php");

// Connect to the database
$psqlhost = "phnxdb2.phenix.bnl.gov";
$psqluser = "phbrowse";
$psqldb = "phnxdb2_calibrations";
$passwd = "phbrowse";
$conn = pg_connect("host=$psqlhost user=$psqluser dbname=$psqldb password=$passwd") or die("<br>Unable to connect to db.");

//report psql connection 
if ($conn) print " <div id=\"connect\">User <b>$psqluser</b> connected to <b>$psqldb</b> (running on <b>$psqlhost</b>)</div><br>";
?>

<div align="center">

<div id="tabwrapper">


<table align="center">
<tr>
	<td><a href="#" onclick="document.form1.tabname.value='plottab';document.form1.submit();">Single Parameter Cut</a></td>
	<td><a href="#" onclick="document.form2.tabname.value='mparamtab';document.form2.submit();">Multiple Parameter Cut</a></td>
	<td><a href="#" onclick="document.form3.tabname.value='SQLtab';document.form3.submit();">SQL Statement Cut</a></td>
</tr>
</table>


<div id="mparamtab">
<?php

// Let user select the number of cuts--prints out that many cut forms
// This should be merged into the primary multi-parameter form...


get_param_select_form($conn);
?>
</div>


<div id="plottab">

<?php
// If connection to DB successful, show filtering options
//*****************************************************************
if ($conn) {

//Call up the form
get_param_plot_form($conn);

}
else echo "postgreSQL Connection could not be established!";
//*****************************************************************
?>
</div>


<div id="SQLtab">
<?php

get_SQL_form();
?>
</div>

</div>


<br>

<?php 

if ($_POST['makePlot']||$_POST['makeMParPlot'])
{
  // Make a plot from the plot tab window

  if ($_POST['makePlot']) {
    echo "<div id=\"plotwindow\">";
    echo "<table><tr><td>";	
    echo "<img src=\"plot.php?par=".urlencode($_POST['par'])."&tag=".urlencode($_POST['tag'])."&paramlow=".urlencode($_POST['paramlow'])."&paramhigh=".urlencode($_POST['paramhigh'])."&runmin=".urlencode($_POST['runmin'])."&runmax=".urlencode($_POST['runmax'])."&shouldTally=".urlencode($_POST['shouldTally'])."\">";
    echo "</td></tr></table>";
    echo "</div>";
  }

  // Make a plot from the multiparamter selector window

  for ($cut=1;$cut<=$numcuts;$cut++)
  {
    if ($_POST['makeMParPlot'][$cut])
    {
      //echo "2: ".$_POST['mpar'][$cut];
      echo "<div id=\"plotwindow\">";
      echo "<table><tr><td>";	
      echo "<img src=\"plot.php?par=".urlencode($_POST['mpar'][$cut])."&tag=".urlencode($_POST['tag'])."&paramlow=".urlencode($_POST['mparamlow'][$cut])."&paramhigh=".urlencode($_POST['mparamhigh'][$cut])."&runmin=".urlencode($_POST['runmin'])."&runmax=".urlencode($_POST['runmax'])."&shouldTally=".urlencode($_POST['shouldTally'])."\">";
      
      echo "</td></tr></table>";
      echo "</div>";
    }
  }
}

// Make a runlist of some type
else if ($_POST['makeList']||$_POST['makeMParList']||$_POST['makeSqlList'])
{
  echo "<div id=\"runlistwindow\">";

  echo "<form id=\"form4\" name=\"form4\" method=\"post\" action =\"list.php\"> ";
  echo "<center>";
  echo "<input type =\"submit\" name=\"makePrintable\" value=\"Show Printable List\">";
  echo "</center>";

  echo "</div>";
  echo "<div id=\"runlistwindow\">";

  $today=getdate();
  $date=$today[mon]."/".$today[mday]."/".$today[year];

  // From either parameter selector
  if ($_POST['makeList']||$_POST['makeMParList'])
    {

      // From the single paramter selector
      // Mimic multiple parameter form
      if ($_POST['makeList'])
	{
	  $numcuts=1;
	  $mpar[1]=$_POST['par'];
	  list($mtablename[1], $mparamname[1])=split("::", $mpar[1]);
	  $mparamlow[1]=$_POST['paramlow'];
	  $mparamhigh[1]=$_POST['paramhigh'];
	  $mparamlogicstring="(C1)";
	  $runmin=$_POST['runmin'];
	  $runmax=$_POST['runmax'];
	  $tag=$_POST['tag'];
	}
      // From the multiple parameter selector
      else if ($_POST['makeMParList'])
	{
	  $numcuts=$_POST['numcuts'];
	  $mpar=$_POST['mpar'];
	  for ($cut=1;$cut<=$numcuts;$cut++) list ($mtablename[$cut], $mparamname[$cut])=split("::", $mpar[$cut]);
	  $mparamlow=$_POST['mparamlow'];
	  $mparamhigh=$_POST['mparamhigh'];
	  $mparamlogicstring=$_POST['mparamlogicstring'];
	  $runmin=$_POST['runmin'];
	  $runmax=$_POST['runmax'];
	  $tag=$_POST['tag'];
	}

      if ($_POST['makeList'] == "Passing" || $_POST['makeMParList'] == "Passing")
	{

	  $sql="";

	  // C1,C2,C3 are too common and will appear in PC1,PC2,PC3 names... This is more unique
	  $mparamlogicstring = str_replace("C", "Cut!#", $mparamlogicstring);
	  $mparamlogicstring = str_replace("&", " INTERSECT ", $mparamlogicstring);
	  $mparamlogicstring = str_replace("|", " UNION ", $mparamlogicstring);

	  for ($cut=1;$cut<=$numcuts;$cut++) 
	    {
	      $mparamlogicstring=str_replace("Cut!#$cut", "(select distinct runnumber from (select distinct on (runnumber) runnumber,inserttime,tag,parname,parameter,parerror from $mtablename[$cut] where parname='$mparamname[$cut]' and tag like '$tag' and runnumber < $runmax and runnumber > $runmin and parameter < $mparamhigh[$cut] and parameter > $mparamlow[$cut] order by runnumber, inserttime desc) as cut$cut)", $mparamlogicstring);
	    }
	  $sql.=$mparamlogicstring;
	  $sql.=" order by runnumber";
	}
      else if ($_POST['makeList'] == "Failing" || $_POST['makeMParList'] == "Failing")
	{
	  $passingSql="";
	  $mparamlogicstring = str_replace("C", "Cut!#", $mparamlogicstring);
	  $mparamlogicstring = str_replace("&", " INTERSECT ", $mparamlogicstring);
	  $mparamlogicstring = str_replace("|", " UNION ", $mparamlogicstring);

	  for ($cut=1;$cut<=$numcuts;$cut++) 
	    {
	      $mparamlogicstring=str_replace("Cut!#$cut", "(select distinct runnumber from (select distinct on (runnumber) runnumber,inserttime,tag,parname,parameter,parerror from $mtablename[$cut] where parname='$mparamname[$cut]' and tag like '$tag' and runnumber < $runmax and runnumber > $runmin and parameter < $mparamhigh[$cut] and parameter > $mparamlow[$cut] order by runnumber, inserttime desc) as cut$cut)", $mparamlogicstring);
	    }
	  $passingSql.=$mparamlogicstring;
	  
	  // Look in the same parameters, tags, and runnumber range
	  $allChoices = "";

	  for ($cut=1;$cut<=$numcuts;$cut++)
	    {
	      if ($cut>1)
	      {
		$allChoices.=" UNION ";
	      }
	      $allChoices.="(select distinct runnumber from (select distinct on (runnumber) runnumber,inserttime,tag,parname,parameter,parerror from $mtablename[$cut] where parname='$mparamname[$cut]' and tag like '$tag' and runnumber < $runmax and runnumber > $runmin order by runnumber,inserttime desc) as choice$cut)";
	    }
	  // Report the runnumbers that didn't make it into
	  $sql ="";
	  $sql ="select distinct runnumber from ($allChoices) as allchoices where runnumber not in ($passingSql)";
	  $sql.=" order by runnumber";
	}
    }
  else if ($_POST['makeSqlList'])
    {
      $sql = $_POST['query'];
      $sql = stripslashes($sql);

      // Redundant security... phbrowse not allowed to, but kill
      // SQL commands containing UPDATE, DELETE, and INSERT anyway
      
      //if ( SQL contains these)
      //{
      //  $sql = " SQL statement may not alter database (ie. UPDATE, DELETE, or INSERT commands are not permitted).";
      //}

    }
  
  $result = pg_exec($conn,$sql) or die("Failed SQL Statement: ".$sql);
  $numrows=pg_numrows($result);

  $output = "";
  $output .= "\n#========================================================<br>";
  $output .= "\n# Quality Control WebPage Runlist. Produced on $date.<br>";
  $output .= "\n#========================================================<br>";
  //print("# SQL Statement:<br>");
  //print("#  ".$sql."<br>");
  //print("#----------------------------------------------------<br>");

  for ($i=0;$i<$numrows;$i++)
    {
      $obj=pg_fetch_object($result);
      $output .= "\n".$obj->runnumber."<br>";
    }
  if ($numrows==0)
    {
      $output.="\n No runs fit your required criteria"."<br>";
    }
  $output .= "\n#=========================END============================";

  print($output);

  echo "<input type=\"hidden\" name=\"runlist\" value=\"$output\">";

  echo "</form>";
  echo "</div>";

} else {
  // No make submit -> make an empty window
  echo "<div id=\"emptyplotwindow\">";
  echo "</div>";
}
?>
</div>

<br>
<div id="bottomblock">
<a href="mailto:mccumber@grad.physics.sunysb.edu" style="text-decoration: none">Email</a>
</div>

</body>
</html>

