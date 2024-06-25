<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Mettre à jour l'étudiant</title>
</head>
<body>
    <form action="traitinsMise.php" method="post">
        <p>
            <label for="t1">Introduire votre matricule</label>
            <input type="text" name="mat" id="t1">
        </p>
        <p>
            <label for="t2">Introduire le nouveau nom</label>
            <input type="text" name="nm" id="t2">
        </p>
        <p>
            <input type="submit" value="OK">
            <input type="reset" value="NO">
        </p>
    </form>
</body>
</html>