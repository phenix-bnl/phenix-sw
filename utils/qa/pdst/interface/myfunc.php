<?php

//==========================================================================
// In an attempt to make page initialization, the rarely changing parameters
// are stored locally. The file access should be faster than searching the
// database. I have added an automatic update when the page is openned
// for the first time in 24 hours.
//==========================================================================

function get_parameter_list($conn)
{
  $output="";

  // If file does not exist, get parameters from
  // database and make the file.
  if(file_exists("parameters.txt"))
    {
      $FILE = fopen("parameters.txt","r") or die ("WARNING: parameters.txt present, but can not be accessed.");
      
      // If last database update older than 24 hours,
      // automatically update from database now.
      $lastUpdate = fgets($FILE,999);
      $now = time();
      if($now - $lastUpdate <= 86400)
	{
	  while($line=fgets($FILE,999))
	    {
	      $output=$output.$line;
	    }
	} else {
	  $output = update_parameter_list($conn);
	}
    } else {
      $output = update_parameter_list($conn);
    }
  fclose($FILE);
  return $output;
}

//============================================================================
// This function creates the selector for the parameters in the database
//  -Borrowed from Cesar and altered
//============================================================================

function update_parameter_list($conn)
{
  grant_access();

  $output="";
  $sql = "select relname from pg_stat_user_tables where relname like '%qa' order by relname";
  $res = pg_query($conn, $sql) or die ("Pqsql Result Error: ".pg_result_error());
  $firstEntry = "true";
  while ($line = pg_fetch_array($res))
    {
      $subsystable = $line["relname"];
      $subsysname = str_replace("qa","",$subsystable);
      $output=$output."\n<optgroup label=\"$subsysname\" id=\"tablename\" value=\"$subsystable\">";
      $res1 = pg_query($conn, "select distinct parname from $subsystable group by parname");
      while ($line1 = pg_fetch_array($res1))
	{
     
	  // Solves special char refresh problem.
	  $htmlparname = htmlentities($line1["parname"]);
	  $parname = $line1["parname"];

	  //$parname = htmlentities($line1["parname"]);
	  if ($firstEntry == "true")
	    {
	      $output=$output."\n<OPTION LABEL=\"$parnam\" VALUE=\"$subsystable::$parname\" SELECTED>$htmlparname</OPTION>";
	    }
	  else {
	    $output=$output."\n<OPTION LABEL=\"$parnam\" VALUE=\"$subsystable::$parname\">$htmlparname</OPTION>";
	  }
	  $firstEntry="false";
	}
      $output=$output."\n</optgroup>\n";
    }
  // Records Paramters to File
  $FILE=fopen("parameters.txt","w+");
  fwrite($FILE,time());
  fwrite($FILE,$output);
  fclose($FILE);
  
  // Also, returns paramters
  return $output;
}

//============================================================
// Again, trying to eliminate database calls on initialization
//============================================================

function get_tag_list($conn)
{
  $output="";
  if(file_exists("tags.txt"))
    {
      $FILE = fopen("tags.txt","r") or die ("WARNING: tags.txt present, but can not be accessed.");
      $lastUpdate = fgets($FILE,999);
      $now = time();
      if($now - $lastUpdate <= 86400)
	{
	  while($line=fgets($FILE,999))
	    {
	      $output=$output.$line;
	    }
	} else {
	  $output = update_tag_list($conn);
	}
    } else {
      $output = update_tag_list($conn);
    }
  fclose($FILE);
  return $output;
}

//==================================================================
// This function creates selector for the Tags found in the database
//  -Borrowed from Cesar and altered
//==================================================================

function update_tag_list($conn)
{
  // Now grants access to all qa tables before update
  grant_access();

  $firstEntry = "true";
  $output = "";

  if ($firstEntry == "true")
    {
      $output=$output."\n<option value=\"%\" SELECTED>latest Tags</option>";
    }
  else {
    $output=$output."\n<option value=\"%\">latest Tags</option>";
  }
  $firstEntry = "false";

  // Get all table names
  $sql = "select relname from pg_stat_user_tables where relname like '%qa' order by relname";
  $res = pg_query($conn, $sql) or die ("Pqsql Result Error: ".pg_result_error());

  // Construct an sql statement that will grab unique tags from all tables
  $sql = "";
  $firstCycle = "true";
  while ($line = pg_fetch_array($res))
    {
      $subsystable = $line["relname"];

      // solves fence-post issue
      if ($firstCycle == "true")
	{
	  $sql = $sql."select distinct tag from ".$subsystable;
	  $firstCycle = "false";
	}
      else
	{
	  $sql = $sql." UNION select distinct tag from ".$subsystable;
	}
    }
  $sql = "select distinct tag from (".$sql.") as alltags order by tag";
  $res = pg_query($conn,$sql) or die ("Pqsql Result Error: ".pg_result_error());

  // Create Unique Wild Tag Fields
  while($line = pg_fetch_array($res))
    {
      $tag = $line["tag"];
      
      $num = substr_count($tag,"_");
      
      if ($num == 4) // Format in which energy does not contain "_"
	{
	  $array = explode("_",$tag);
	  $field ="\n<OPTION VALUE=\"%".$array[0]."%".$array[2]."%\">latest ".$array[0]." ".$array[2]." Tags</OPTION>"; 
	  // if field does not already exists in output, add
	  if(substr_count($output,$field) == 0)
	    {
	      $output=$output.$field;
	    }
	}
      
      if ($num == 5) // Format in which energy does contain "_"
	{
	  $array = explode("_",$tag);
	  $field = "\n<OPTION VALUE=\"%".$array[0]."%".$array[2]."_".$array[3]."%\">latest ".$array[0]." ".$array[2].".".$array[3]." Tags</OPTION>";
	  if (substr_count($output,$field) == 0)
	    {
	      $output=$output.$field;
	    }
	}
	

    }

  // Go back to the top
  pg_result_seek($res,0);

  // Create Unique Tag Fields
  while($line = pg_fetch_array($res))
    {
      $tag = $line["tag"];
      $output=$output."\n<OPTION VALUE=\"".$tag."\">".$tag."</OPTION>";
    }

  // Records Tags to File
  $FILE=fopen("tags.txt","w+");
  fwrite($FILE,time());
  fwrite($FILE,$output);
  fclose($FILE);

  return $output;
}

function grant_access()
{

  // Connect to the database
  $psqlhost = "phnxdb2.phenix.bnl.gov";
  $psqluser = "phnxrc";
  $psqldb = "calibrations";
  $password = "";
  $conn2 = pg_connect("host=$psqlhost user=$psqluser dbname=$psqldb password=$passwd") or die("<br>Unable to connect to db.");
 
  // Get all table names
  $sql = "select relname from pg_stat_user_tables where relname like '%qa' order by relname";
  $res = pg_query($conn2, $sql) or die ("Pqsql Result Error: ".pg_result_error());
  
  // Cycle of QA table names and grant select permission
  while ($line = pg_fetch_array($res))
    {
      $sql = "";
      $subsystable = $line["relname"];

      //print($subsystable."\n");
      
      $sql = "grant select on $subsystable to phbrowse";
      
      $temp = pg_query($conn2,$sql) or die ("Pqsql Result Error: ".pg_result_error());
    }
  
  return 0;
}
?>

