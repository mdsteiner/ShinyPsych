/* This script contains functions for the Shiny
DfE Experiment. When running the experiment just
compile this code on http://closure-compiler.appspot.com
to make it less readable */

console.log("VALUABLE RESEARCH NEEDS VALUABLE DATA. PLEASE DON'T CHEAT! \nIf you do please answer in the questionnaire honestly that you did so\nthat we can make sure to not use your data.\nThank you!");


Shiny.addCustomMessageHandler("envDfeFourOpts",
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

		// get array of values for environment 3
		enThree = json.enThree;

		// get array of values for environment 4
		enFour = json.enFour;

		// game number. create an array
		gambleNr = [json.gambleNr];

		// control whether signal colors should be used
		signalColors = json.signalColors;

	})


function endDfeGame(selected, samples, outcome, respTime, trial, gambleNr, finalOutcome){
	/* this function handles all the client to server communication in this
experiment. It returns arrays of game data to the server. These can then
be accessed in R Shiny through input$. */ 

	Shiny.onInputChange("selected", selected);
	Shiny.onInputChange("samples", samples);
	Shiny.onInputChange("finalOutcome", finalOutcome);
	Shiny.onInputChange("outcome", outcome);
	Shiny.onInputChange("respTime", respTime);
	Shiny.onInputChange("trial", trial);
	Shiny.onInputChange("gambleNr", gambleNr);
}


function newDfeGame(){
	/* function to create arrays in which
the game data will be saved. */

	// index variable. its length determines the trial (e.g. which value to display)
	ind = [];

	// array to store which options was selected
	selected = [];

	// array to store the outcome that was drawn from the chosen distribution
	finalOutcome = [];

    // array to store which options were sampled
    samples = [];

	// array to store the outcome values
	outcome = [];

	// time reference for the first trial
	t = [Date.now()];

	// array to store the response times 
	respTime = [];

	// array to store trial number
	trial = [];

	// this controls the reaction timeout of the buttons to ensure that people don't go through it too fast
	clickEnabled = [1];

	// this controls when the selection should be made
	makeDecision = [false];
}

function add(a, b) {
	/* I don't know why exactly this is needed but but this function later
	returns in combination of another function the 
	cumulative points. */
    return a + b;
}

function enableDecisionFourOpt(makeDecision, elmt1, elmt2, elmt3, elmt4){
	// disable make decision button
	Shiny.onInputChange("toggleButton", 2);

	var element1 = document.getElementById(elmt1);
	var element2 = document.getElementById(elmt2);
	var element3 = document.getElementById(elmt3);
	var element4 = document.getElementById(elmt4);

	element1.innerHTML = ' ';
	element2.innerHTML = ' ';
	element3.innerHTML = ' ';
	element4.innerHTML = ' ';

	makeDecision.push(true);
}


function updateDfeFourOpts(elmtG, elmtGO1, elmtGO2, elmtGO3, dist, distd1, distd2, distd3, ind, sam, outcome, samples,
	gambleNr, respTime, trial, t, clickEnabled, signalColors, makeDecision, selected, finalOutcome){
	/* this function is the heart of the game. It gets the value of the selected option and changes the 
	html tag to this value to display it. It then appends the relevant parameters to the different arrays. */

	// if this was the last trial send the gathered data back to the server
	if (makeDecision[makeDecision.length -1] != true){

		if (clickEnabled[clickEnabled.length - 1] === 1){

			// get reaction Time
			var clickedRT = Date.now();

			// disable reaction to clicks
			clickEnabled.push(0);

			// first compute the response time
			respTime.push((clickedRT - t[t.length - 1]) / 1000);
			t.push(clickedRT);

			// select the different html tags that are to be changed
			var element = document.getElementById(elmtG);
			var otherelement1 = document.getElementById(elmtGO1);
			var otherelement2 = document.getElementById(elmtGO2);
			var otherelement3 = document.getElementById(elmtGO3);


			// set the color of the displayed outcome value
			if (signalColors === 1) {
				if (dist[ind.length] === 0) {
					var color = "#BEBEBE";
				} else if (dist[ind.length] < 0) {
					var color = "#FF6A6A";
				} else {
					var color = "#00CD00";
				}
			} else {
				var color = "#000000"
			}

			element.innerHTML = ' ';

			// select the outcome value and display it in the specified color
			setTimeout(function(){
				element.innerHTML = dist[ind.length];
				element.style.color = color;
			}, 100)


			// make sure in the other element nothing is displayed
			if (samples[samples.lenth] != sam){
				otherelement1.innerHTML = ' ';
				otherelement2.innerHTML = ' ';
				otherelement3.innerHTML = ' ';
			}
			
			// append selected option to samples array
			samples.push(sam);

			// append outcome to outcome array
			outcome.push(dist[ind.length]);

			// append trial number to trial array
			trial.push(ind.length + 1);

			// append one to index array
			ind.push(1);

			if (trial[trial.length - 1] > 1){
				// append game number to gameNr array
				gambleNr.push(gambleNr[0]);
			}

			// enable make decison button after first trial
			if (trial[trial.length - 1] === 1){
				Shiny.onInputChange("toggleButton", 1);
			}

			// wait 0.5 seconds and then enable click again
			setTimeout(function(){
				clickEnabled.push(1);
			}, 500);
		}
	} else {

   		if (clickEnabled[clickEnabled.length - 1] === 1){

			// disable reaction to clicks
			clickEnabled.push(0);
			
			// select the different html tags that are to be changed
			var element = document.getElementById(elmtG);
			var otherelement1 = document.getElementById(elmtGO1);
			var otherelement2 = document.getElementById(elmtGO2);
			var otherelement3 = document.getElementById(elmtGO3);

			// select the outcome value and display it in the specified color
			element.innerHTML = ' ';
			element.style.backgroundColor = "#A9A9A9"

			// make sure in the other element nothing is displayed
			if (samples[samples.lenth] != sam){
				otherelement1.innerHTML = ' ';
				otherelement2.innerHTML = ' ';
				otherelement3.innerHTML = ' ';
			}
			
			// append selected option to selected array
			selected.push(sam);

			finalOutcome.push(dist[ind.length]);


			// append one to index array
			ind.push(1);

			// wait 0.5 seconds and then enable click again
			setTimeout(function(){
				endDfeGame(selected, samples, outcome, respTime, trial, gambleNr, finalOutcome);
			}, 700);

			
		}
	}
}