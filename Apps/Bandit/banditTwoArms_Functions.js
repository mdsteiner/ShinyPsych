/* This script contains functions for the Shiny
Bandit Experiment. When running the experiment just
compile this code on http://closure-compiler.appspot.com
to make it less readable */

console.log("VALUABLE RESEARCH NEEDS VALUABLE DATA. PLEASE DON'T CHEAT! \nIf you do please answer in the questionnaire honestly that you did so\nthat we can make sure to not use your data.\nThank you!");


Shiny.addCustomMessageHandler("envBanditTwoArms",
	function(message){
		/* function to communicate with the server. The
argument message takes am R list as input that is then
sent to the client in a json format. This function handles
all the server to client communication in this experiment */

		// define a local variable that contains the whole parsed json file
		var json = JSON.parse(JSON.stringify(message));

		// the following are all definitions of global variables
		// get array of values for environment 1
		enOne = json.enOne;

		// get array of values for environment 2
		enTwo = json.enTwo;

		// get number of trials
		nTrials = json.nTrials;

		// game number. create an array
		gameNr = [json.game];

		// number of digits to round to
		nDigits = [json.nDigits]

	})


function endGame(selection, outcome, outcomeCum, respTime, trial, gameNr){
	/* this function handles all the client to server communication in this
experiment. It returns arrays of game data to the server. These can then
be accessed in R Shiny through input$. */ 

	Shiny.onInputChange("selection", selection);
	Shiny.onInputChange("outcome", outcome);
	Shiny.onInputChange("outcomeCum", outcomeCum);
	Shiny.onInputChange("respTime", respTime);
	Shiny.onInputChange("trial", trial);
	Shiny.onInputChange("gameNr", gameNr);
}

function myRound(number, precision) {
    var factor = Math.pow(10, precision);
    var tempNumber = number * factor;
    var roundedTempNumber = Math.round(tempNumber);
    return roundedTempNumber / factor;
};


function newGame(){
	/* function to create arrays in which
the game data will be saved. */

	// index variable. its length determines the trial (e.g. which value to display)
	ind = [];

	// array to store which options were selected
	selection = [];

	// array to store the outcome values
	outcome = [];

	// array to store the cumulative outcome values (i.e. current total point value)
	outcomeCum = [];

	// time reference for the first trial
	t = [Date.now()];

	// array to store the response times 
	respTime = [];

	// array to store trial number
	trial = [];

	// this controls the reaction timeout of the buttons to ensure that people don't go through it too fast
	clickEnabled = [1];
}

function add(a, b) {
	/* I don't know why exactly this is needed but but this function later
	returns in combination of another function the 
	cumulative points. */
    return a + b;
}


function updateBanditTwoArms(elmtG, elmtGO1, elmtP, elmtC, dist, distd1, ind, sel, outcome, outcomeCum, selection,
	nTrials, gameNr, respTime, trial, t, clickEnabled, nDigits){
	/* this function is the heart of the game. It gets the value of the selected option and changes the 
	html tag to this value to display it. It then appends the relevant parameters to the different arrays. */

	// only do this if trial number is smaller than maximum number of trials. This ensures that the game stops at nTrials
	if (ind.length < nTrials){

		if (clickEnabled[clickEnabled.length - 1] === 1){

			// get reaction Time
			var clickedRT = Date.now()

			// disable reaction to clicks
			clickEnabled.push(0);

			// first compute the response time
			respTime.push((clickedRT - t[t.length - 1]) / 1000);
			t.push(clickedRT);

			// select the different html tags that are to be changed
			var element = document.getElementById(elmtG);
			var otherelement1 = document.getElementById(elmtGO1);
			var pointsP = document.getElementById(elmtP);
			var clicksP = document.getElementById(elmtC);

			// set the color of the displayed outcome value
			if (dist[ind.length] === 0) {
				var color = "#BEBEBE";
			} else if (dist[ind.length] < 0) {
				var color = "#FF6A6A";
				} else {
					var color = "#00CD00";
			}

			// select the outcome value and display it in the specified color
			element.innerHTML = dist[ind.length];
			element.style.color = color;

			// make sure in the other element nothing is displayed
			if (selection[selection.lenth] != sel){
				otherelement1.innerHTML = ' ';
			}
			
			// append selected option to selection array
			selection.push(sel);

			// append outcome to outcome array
			outcome.push(dist[ind.length]);

			// append trial number to trial array
			trial.push(ind.length + 1);

			// update the displayed clicks counter on the game page
			clicksP.innerHTML = nTrials - (ind.length + 1);

			// append cumulative outcome values to outcomeCum array
			outcomeCum.push(outcome.reduce(add, 0));

			// append one to index array
			ind.push(1);

			// append game number to gameNr array
			if (gameNr.length < nTrials){
				gameNr.push(gameNr[0]);
			}

			// change the displayed total point value to new value
			pointsP.innerHTML = myRound(outcomeCum[outcomeCum.length - 1], nDigits);

			// if this was the last trial send the gathered data back to the server
			if (ind.length === nTrials){
				endGame(selection, outcome, outcomeCum, respTime, trial, gameNr);
			}

			// wait 0.5 seconds and then enable click again
			setTimeout(function(){
				clickEnabled.push(1);
			}, 500);
		}
	}
}