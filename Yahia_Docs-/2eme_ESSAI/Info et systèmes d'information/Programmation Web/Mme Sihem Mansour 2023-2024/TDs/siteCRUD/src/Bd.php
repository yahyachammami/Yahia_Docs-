<?php

class Bd {
    private static $con=null;
    public static function connexion() {  
    try{
      self::$con=new PDO('mysql:host=localhost;dbname=gest','root','');
      return self::$con;
    }
    catch(PDOException $e){
        echo 'Erreur '.$e->getMessage();
        die();
    }

}
    }
    ?>
 