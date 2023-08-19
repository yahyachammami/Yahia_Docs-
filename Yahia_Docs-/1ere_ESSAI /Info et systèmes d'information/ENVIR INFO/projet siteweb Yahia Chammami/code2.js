function decision(){
    let resultat=document.querySelector("#obj");
    if(localStorage.getItem('name')&&localStorage.getItem('average')){
        if(localStorage.getItem('average')>2){
            resultat.innerHTML='  Nous sommes tres desole Mr  '+localStorage.getItem('name')+'   votre reservation    '+localStorage.getItem('average')+'  a depasse le nombre max de tables disponibles pour la reservation'
        resultat.style.color='red';
        resultat.style.fontSize="30px";
        resultat.style.fontWeight="bold";     
        
    }
        else if (localStorage.getItem('average')<=2){
            resultat.innerHTML='   Votre demande de reservation a ete Bien Enrigistrer Mr   '+localStorage.getItem('name');
            resultat.style.color='green';
        resultat.style.fontSize="30px";
        resultat.style.fontWeight="bold"; 
        }
    }
}

function valid(){
    let nom=document.querySelector('#nm');
    let table =document.querySelector('#tab');
    let error=document.querySelector('#err');
    let patternom=/[a-zA-Z]{3,20}/;
    let pattertable =/[0-9]{1,2}/;
    if(patternom.test(nom.value)==false){
        event.preventDefault();
        nom.value="";
        error.innerHTML='Nom invalide!!'
        error.style.color='red';
    }
    else if(pattertable.test(table.value)==false){
        event.preventDefault();
        nom.value="";
        table.value="";
        error.innerHTML='Nombre de tables est invalide !!'
        error.style.color='red';
    }
    else if (parseFloat(table.value)<0|| parseFloat(table.value)>20){
        event.preventDefault();
        nom.value="";
        table.value="";
        error.innerHTML='Nombre de tables est hors intervall !!';
        error.style.color='red';
    }
    else {
        localStorage.setItem('name',nom.value);
        localStorage.setItem('average',parseFloat(table.value));
    }
}