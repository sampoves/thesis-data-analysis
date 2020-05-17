
// JavaScript for the analysis app of my thesis results

// "Parking of private cars and spatial accessibility in Helsinki Capital Region" 
// by Sampo Vesanen, 17.5.2020


// This JS function checks the state of the display parameter of inputted div "shrink_me"
// and edits the value accordingly. Also change the eye icon of the inputted "parent_div"
// parameter.
function show_hide(shrink_me, parent_div) {
	var elem_to_hide = document.getElementById(shrink_me);
	var parent_div = '#' + parent_div;

	if(elem_to_hide.style.display === 'none') {
		elem_to_hide.style.display = 'block';
		$(parent_div).find('i.icon.eye').toggleClass('eye eyeslash');
		$(parent_div).find('i.icon.eyeslash')[0].setAttribute('title', 'Hide element');
	} else {
		elem_to_hide.style.display = 'none';
		$(parent_div).find('i.icon.eyeslash').toggleClass('eyeslash eye');
		$(parent_div).find('i.icon.eye')[0].setAttribute('title', 'Show element');
	}
};

// Specified function to hide/show sidebar
function show_hide_sb() {
	var elem_to_hide = document.getElementById('sidebar');
	var showhidebutton = document.getElementById('showhidebutton');

	if(elem_to_hide.style.display === 'none') {
		elem_to_hide.style.display = 'block';
		$(showhidebutton).find('i.icon.eye').toggleClass('eye eyeslash');
		$(showhidebutton).find('i.icon.eyeslash')[0].setAttribute('title', 'Hide sidebar');
	} else {
		elem_to_hide.style.display = 'none';
		$(showhidebutton).find('i.icon.eyeslash').toggleClass('eyeslash eye');
		$(showhidebutton).find('i.icon.eye')[0].setAttribute('title', 'Show sidebar');
	}
};

// Insert sidebar hide/show button to col-sm-3, the div that contains the sidebar. Add
// the button after the actual sidebar div. The button appears on the top right corner
// of the sidebar.
$(function() {
	$('.well').after("<div class='hidesidebar'><button id='showhidebutton' onclick='show_hide_sb()'><i class='icon eyeslash' title='Hide sidebar'></i></button></div>");
	
	// Make ggiraph outputs untouchable
	$("#hist").addClass('noselect');
	$("#barplot_ord").addClass('noselect');
	$("#boxplot").addClass('noselect');
	$("#map").addClass('noselect');
	$("#interactive").addClass('noselect');
});

// Insert significance element right after table element so that overflow-x: auto;
// does not hide the text. Again with the hacky setTimeout() approach. Could not figure
// a better way in this timeframe.
$(document).one('shiny:idle', function(event) {
	var sig = "<p id='signif'>Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1</p>";
	
	setTimeout(function() {
		$('#levene table').after(sig);
		$('#anova table').after(sig);
	}, 8000);
});

// This jQuery function listens to anchor link clicking. With this function I aimed to
// make repeated clicking possible, which was not with :target selectors I used earlier.
// In short, this detects with the amount of clicks if we fire a delayed highlight or
// an immediate one. A bit hacky setTimeout() is employed to remove the delayed highlight.
// leave console.log()'s in to illustrate the behaviour of this function.
$(function() {
	
	var timesClicked = 0;
	
	$("a").on("click", function() {
		timesClicked++;
		var dest_id = $(this)[0].hash;
		var dest = dest_id.replace('#', '');
		element = document.getElementById(dest);
		
		if(timesClicked === 1) {
			console.log(`${dest_id} timesclicked: ${timesClicked}`);			
			$(dest_id).addClass('animate');
			
			ticktock = function() {
				timeout_val = setTimeout(function() {
					$(dest_id).removeClass('animate');
					// The next line removes the immediate animation from the entire
					// document. Without this fast clicking around the document ends
					// with the last div id having a lingering animation class.
					$('.animate-immediately').removeClass('animate-immediately');
					timesClicked = 0;
					console.log("end");
				}, 2000);
			}
			ticktock();
		}
		
		// Detect a click event while the class .animate is present in dest_id
		if (timesClicked > 1) {
			console.log(`timesclicked: ${timesClicked}, interrupt start`);
			clearTimeout(timeout_val);
			element.classList.remove('animate');
			element.classList.remove('animate-immediately');
			
			// Trigger a DOM reflow with .offsetWidth. A simple elem.remove().add() will 
			// not work to interrupt a CSS animation and run it again. See:
			// https://css-tricks.com/restart-css-animation/
			void element.offsetWidth;
			element.classList.add('animate-immediately');
			ticktock();
		}
    });
});