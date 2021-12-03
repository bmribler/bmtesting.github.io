function doClear(theText){
	if (theText.value == theText.defaultValue){
		theText.value = ""
	}
}

function changeid(id){

	var ID = document.getElementById(id); 
	
	if (ID.style.display == ""){
		ID.style.display = "none"; 
	}else{
		ID.style.display = ""; 
	}
}

function closeid(id){

var ID = document.getElementById(id); 

	if (ID.style.display == "none"){
		ID.style.display = "none"; 
	}else{
		ID.style.display = "none"; 
	}
}

function getURL(){
	var path=document.dynamicselector.dynamicselector2.value
	open(path);
	alert("To download this image: Right click on image and select save image/picture as:"); 
}

