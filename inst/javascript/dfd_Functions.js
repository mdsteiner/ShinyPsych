/* This script contains functions for the Shiny
DfD Experiment. When running the experiment just
compile this code on http://closure-compiler.appspot.com
to make it less readable */

console.log("VALUABLE RESEARCH NEEDS VALUABLE DATA. PLEASE DON'T CHEAT! \nIf you do please answer in the questionnaire honestly that you did so\nthat we can make sure to not use your data.\nThank you!");

function newDfdGame(gameNrIn){
	/* function to create arrays in which
the game data will be saved. */

	// time reference for the first trial
	t = [Date.now()];

	// array to store the response times 
	respTime = [];

	// array to control when a deck is clickable
	clickEnabled = [1];

	gameNr = [gameNrIn];

}

function onGambleClick(elmtG, deckNr, t, respTime, clickEnabled, gameNr){
	/* this function handles actions if input is given */

	if (clickEnabled[clickEnabled.length - 1] === 1){

		// get reaction Time
		var clickedRT = Date.now();

		// disable reaction to clicks
		clickEnabled.push(0);

		// first compute the response time
		respTime.push((clickedRT - t[t.length - 1]) / 1000);

		// select element to change parameters
		var element = document.getElementById(elmtG);

		// change background color of selected element
		element.style.backgroundColor = "#A9A9A9";
		
		setTimeout(function(){
			Shiny.onInputChange("selected", deckNr);
			Shiny.onInputChange("respTime", respTime);
			Shiny.onInputChange("gameNr", gameNr)
		}, 700);
	}
}