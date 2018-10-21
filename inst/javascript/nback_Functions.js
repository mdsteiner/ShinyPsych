/* This script contains functions for the ShinyPsych
Bart Experiment. When running the experiment just
compile this code on http://closure-compiler.appspot.com
to make it less readable */

console.log("VALUABLE RESEARCH NEEDS VALUABLE DATA. PLEASE DON'T CHEAT! \nIf you do please answer in the questionnaire honestly that you did so\nthat we can make sure to not use your data.\nThank you!");

var pumps = 0;
var maxPumps;
var popMax;
var actionTimes = [];

//Receive the maximum amount of pumps for the whole game, and wether the boundary should be drawn
Shiny.addCustomMessageHandler("maxPopHandler",
  function(temp) {
    popMax = temp[0];
    drawBoundary = temp[1];

  }
);

//Receives the new max-pump values for this balloon
Shiny.addCustomMessageHandler("maxPumpHandler",
  function(newMaxPumps) {
    maxPumps = newMaxPumps;
  }
);


function jsPump(col){
 actionTimes.push(new Date().getTime() - t);
 pumps = pumps + 1;
 if (pumps <= maxPumps) {
    document.getElementById("pumpCounter").innerHTML = pumps;
    jsRedrawBalloon(col);
  } else {
    Shiny.onInputChange("popped", 1);
    Shiny.onInputChange("pumps", pumps);
    Shiny.onInputChange("actionTimes", actionTimes);
  }
}


function jsSaveBalloon(){
  actionTimes.push(new Date().getTime() - t);
  Shiny.onInputChange("actionTimes", actionTimes);
  Shiny.onInputChange("pumps", pumps);
}


function jsNewBalloon(){
  pumps = 0;
  Shiny.onInputChange("popped", 0);
  actionTimes = [];
}


function jsRedrawBalloon(balloonColor) {
  balloonSize = pumps/popMax * 95 + 5;
  ctx.clearRect(0, 0, jsCanvas.width, jsCanvas.height);
  ctx.beginPath();
  ctx.arc(250, 160, balloonSize, 0, 2 * Math.PI);
  ctx.stroke();
  ctx.fillStyle = balloonColor;
  ctx.fill();

  // Write the pop/save messages to the canvas:
  ctx.fillStyle = "black";
  ctx.font = "24px Arial";
  if (balloonColor == "red") {
      ctx.fillText("Popped at pump " + pumps, 160, 40);
      ctx.fillText("No points", 360, 170);
  } else if (balloonColor == "green") {
      ctx.fillText("Saved at pump " + pumps, 160, 40);
      ctx.fillText("+ " + pumps + " points", 360, 170);
  }

  // Draw the maxmum boundary if desired:
  if (drawBoundary) {
    ctx.beginPath();
    ctx.arc(250, 160, 100, 0, 2 * Math.PI);
    ctx.stroke();

  }
}


// Wait a second before showing the nextballoon button to avoid misclicks
function jsDelayButton() {
   document.getElementById("nextballoon").style.visibility = 'hidden';
   setTimeout(function(){
   document.getElementById("nextballoon").style.visibility = 'visible';
   }, 1000);
}
