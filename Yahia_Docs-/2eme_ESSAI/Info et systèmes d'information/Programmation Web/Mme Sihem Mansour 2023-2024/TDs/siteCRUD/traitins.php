<?php
spl_autoload_register(function($name){
    require_once('src/'.$name.'.php');});
?>

<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Document</title>
</head>
<body>
    <?php
    if($_SERVER['REQUEST_METHOD']==='GET'){
        echo 'SVP, passer par le formulaire!!!';
        include('inscr.php');
    }
    else{
        $mat=$_POST['mat'];
        $nom=$_POST['nm'];
        if(Personne::verif($mat,$nom)){
            $con=Bd::connexion();
            $a=new Personne($mat,$nom);
            $a->inserer($con);
            $con=null;
            echo 'insertion réussie!!!';
            include('index.php');
        }
        else{
            echo 'SVP, Saisie convenablement les info!!!';
            include('inscr.php');

        }

    }
    ?>
</body>
</html>