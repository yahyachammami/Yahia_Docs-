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
        include('aut.php');
    }
    else{
        $lg=$_POST['lg'];
        $mp=$_POST['mp'];
        if(Personne::verif1($lg,$mp)){
            $con=Bd::connexion();
           if(Personne::consulter($con,$lg,$mp)){
                echo 'Vous êtes connectées ';
                include('mise.php');
            }
            else{
                echo 'SVP, login/ mot de passe erronnés !!!';
            include('aut.php');
            }
            
        }
        else{
            echo 'SVP, Saisie convenablement les info!!!';
            include('aut.php');

        }

    }
    ?>
</body>
</html>