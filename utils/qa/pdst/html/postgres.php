<?php
require_once('ado.php');
class postgres extends ado
{
  function postgres($str)
    {
      $this->bank = $str;
    }
  function db_connect()
    {
      $this->host = "phnxdb1.phenix.bnl.gov";
      $this->user = "phnxrc";
      $this->pswrd = "";
      if ($conn = pg_connect("host=$this->host dbname=$this->bank user=$this->user password=$this->pswrd"))
	{
	  return $conn;
	}
      else
	{
	  return false;
	}
    }
  function db_close($conn)
    {
      if (pg_close($conn))
	{
	  return true;
	}
      else
	{
	  return false;
	}
    }
  function query($sql)
    {
      $this->conn = $this->db_connect() or die ("Conection Refused");
      if ($res = pg_query($this->conn,$sql))
	{
	  pg_close($this->conn);
	  return $res;
	}
      else
	{
	  pg_close($this->conn);
	  return false;
	}
    }
  function to_array($res)
    {
      if ($linha = pg_fetch_array($res))
	{
	  return $linha;
	}
      else
	{
	  return false;
	}
    }
  function num_rows($res)
    {
      if ($num = pg_num_rows($res))
	{
	  return $num;
	}
      else
	{
	  return false;
	}
    }
  function num_fields($res)
    {
      if ($num = pg_num_fields($res))
	{
	  return $num;
	}
      else
	{
	  return false;
	}
    }
  function affected_rows($res)
    {
      if ($num = pg_affected_rows($res))
	{
	  return $num;
	}
      else
	{
	  return false;
	}
    }
  function read_object($iob)
    {
      $this->conn = $this->db_connect() or die ("Conection Refused");
      pg_exec ($this->conn, "begin");
      if($ifd = pg_loopen ($this->conn, $iob, "rb"))
	{
	  $retval = pg_loread ($ifd,2000);
	  pg_loclose($ifd);
	  return $retval;
	}
      pg_exec ($this->conn, "commit");
      pg_close($this->conn);
    }
}
?>