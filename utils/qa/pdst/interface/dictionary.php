<?php header("Cache-control: private"); ?>

<html>
<LINK REL=stylesheet HREF="style.css" TYPE="text/css">

<SCRIPT>
<!--

// Function to capture only allowed chars 
function AllowChar(event) {
  //var strValidChars = "0123456789.";
  //var strChar;
  //var KeyTyped = String.fromCharCode(event.keyCode).toString();

  //var blnResult = true;

  //if (strValidChars.indexOf(KeyTyped)==-1) blnResult = false;

//for (i=0;i< strString.length && blnresult == true ; i++) {
//	strChar = strString.charAt(i);
//	if (strValidChars.indexOf(strChar) == -1) blnResult = false;
//	}

return blnResult;
}
// -->
</SCRIPT>

<body>

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
		<td><a href="index.php">Run Selector</a></td>
        <!--	<td><a href="dictionary.php" style="color: #1E90FF">Dictionary</a></td> -->
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

<div align="center">

<br><br>
<div id="textwindowsolid">
Largely based on Cesar Luiz da Silva's QA Summary webpage, this re-built version implements new features to increase function and ease of use.  Workable requests for new features are welcome.  The source for this page is stored in CVS under <a href="http://www.phenix.bnl.gov/viewcvs/utils/qa/pdst/interface/">/utils/qa/pdst/interface</a>.
</div>
<br>

<div id="textwindow">
03/04/2005 - A new feature and a change made:

<ul>
<li>Added option to view a printable list</li>
<li>Instead of searching over all tags which can produce multiple points per run, for each run the wild cards will pull only the most resent database entry.</li>
</ul>

</div>
<br>
<div id="textwindow">
03/02/2005 - Another fix made:

<ul>
<li>Improved the tag field generation. New tags should now be incorporated into the selector seemlessly without need for an update to the webpage.</li>
</ul>

</div>
<br>

<div id="textwindow">
02/25/2005 - More fixes made:

<ul>
<li>Moved number cut selection into primary form for better refresh action.</li>
<li>Fixed special character bug in SQL statement tab.</li>
<li>Small aesthetic changes.</li>
</ul>

</div>
<br>
<div id="textwindow">
02/23/2005 - New features added and fixes made:

<ul>
<li>Passing and failing run lists.</li>
<li>Plots from multiple parameter tab.</li>
<li>Lists from single parameter tab.</li>
<li>Fixed bug causing menu selections with special characters to not refresh properly.</li>
</ul>

</div>
<br>
<div id="textwindow">
02/16/2005 - New features added:

<ul>
<li>Multiple parameter cuts on runlist queries with arbitrary logic selection.</li>
<li>Direct SQL statement cuts.</li>
<li>Faster implementation.  Parameters are read from local files , updated automatically after 24 hours or manually upon request.</li>
<li>Wild cards in the tag selection menus.</li>
<li>Slow BBC event tally now optional.</li>
</ul>

</div>
<br><br>
</div>
<?php


?>

<div id="bottomblock">
<a href="mailto:mccumber@grad.physics.sunysb.edu" style="text-decoration: none">Email</a>
</div>
</body>
</html>

