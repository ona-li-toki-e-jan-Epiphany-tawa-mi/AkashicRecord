var butt = document.getElementById("cancer");
var onOn = 0;

function letsGetThisBread() {
   if (onOn == 0) 
      butt.innerHTML = "Umm... try that again";
   else if (onOn == 1)
      butt.innerHTML = "tRy It AgAiN";
   else {
      console.log(onOn);
      butt.innerHTML = "!dekrow ti ,teews hO";
   }
   
   onOn++;
}