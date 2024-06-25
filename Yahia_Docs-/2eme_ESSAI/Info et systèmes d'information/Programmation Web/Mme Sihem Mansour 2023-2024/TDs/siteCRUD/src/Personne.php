<?php
    class Personne{
        private $mat;
        private $nom;
/************************************************ */
        public function __construct($mat,$nom)
        {
            $this->mat=intval($mat);
            $this->nom=$nom;
        }
/************************************************************ */
        public static function verif($mat,$nom){
            $patmat='/^[0-9]{4}$/';
            $patNom='/^[a-z][a-z]*$/i';
            if(preg_match($patmat,$mat) && preg_match($patNom,$nom))return true;
            else return false;
        }

 /*************************************************************** */
 
        public function inserer($con){
            $req=$con->prepare('insert into per(mat,nom) values (?,?)');
            $req->execute(array($this->mat, $this->nom));
            $con=null;
        }
    }
?>